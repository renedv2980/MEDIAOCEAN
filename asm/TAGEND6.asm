*          DATA SET TAGEND6    AT LEVEL 035 AS OF 04/01/13                      
*PHASE T702D6A,*                                                                
         TITLE 'T702D6 - HOLD COPY/MOVE'                                        
T702D6   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D6,R7                                                      
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
         SPACE 3                                                                
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'  GET ADDRESS OF TRPACK                    
         GOTO1 CALLOV,DMCB                                                      
         MVC   ATRPACK,0(R1)                                                    
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         BAS   RE,SETSCRN          SET SCREEN FIELDS                            
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         IF VALIDATE KEY                              
         BNE   HLD40                                                            
         CLI   SCRSTAT,0           IF NO SCREEN CHANGE                          
         BNE   HLD10                                                            
         GOTO1 FLDVAL,DMCB,(X'40',AFRSTKEY),999 IF NO FIELDS CHGD               
         BNE   HLD10                                                            
         TM    STATUS,PFKPEND      IF PFKEY IS NOT PENDING                      
         BO    HLD20                                                            
HLD10    BAS   RE,VKEY             VALIDATE KEY                                 
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTKEY),999 MAKE ALL FIELDS VALID           
         B     PFKMSG                                                           
*                                                                               
HLD20    CLI   PFAID,13            TEST OVERRODE PFKEY                          
         BNE   PFKMSG                                                           
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     HLDX                                                             
         SPACE 1                                                                
HLD40    CLI   MODE,PRINTREP       IF PRINT REPORT                              
         BNE   HLDX                                                             
         LA    R1,HOOK             SET A(HEADHOOK)                              
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS          SET A(SPECS)                                 
         ST    R1,SPECS                                                         
         BAS   RE,PROCESS          PROCESS MOVE/COPY                            
         SPACE 1                                                                
HLDX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET SCREEN FIELDS DEPENDING ON ACTION                 
         SPACE                                                                  
SETSCRN  NTR1                                                                   
         CLI   ACTNUM,ACTCOPY      IF COPYING HOLD RECORDS                      
         BNE   SETSC10                                                          
         OI    SNXTOAYH+1,X'0C'    SET 'TO AGENCY'   ZERO INTENSITY             
         OI    SNXTAYH+1,X'0C'     SET AGENCY INPUT  ZERO INTENSITY             
         OI    SNXTAYH+1,X'20'                       AND PROTECTED              
         OI    SNXTAYNH+1,X'0C'    SET AGENCY NAME   ZERO INTENSITY             
         NI    SNXTTOH+1,X'F3'     SET TO MAKE 'TO:' NORMAL INTENSITY           
         B     SETSC20                                                          
*                                                                               
SETSC10  NI    SNXTOAYH+1,X'F3'     SET 'TO AGENCY'   NORMAL INTENSITY          
         NI    SNXTAYH+1,X'F3'      SET AGENCY INPUT  NORMAL INTENSITY          
         NI    SNXTAYH+1,X'DF'                        AND UNPROTECTED           
         NI    SNXTAYNH+1,X'F3'     SET AGENCY NAME   NORMAL INTENSITY          
         OI    SNXTTOH+1,X'0C'      SET TO MAKE 'TO:' ZERO INTENSITY            
*                                                                               
SETSC20  OI    SNXTOAYH+6,X'80'                                                 
         OI    SNXTAYH+6,X'80'                                                  
         OI    SNXTAYNH+6,X'80'                                                 
         OI    SNXTTOH+6,X'80'                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         SPACE 1                                                                
         BAS   RE,VKFRMKY          VALIDATE FROM KEY DETAILS                    
         MVI   TGTYPE,0                                                         
         GOTO1 RECVAL,DMCB,TLNXCDQ,(X'40',0)                                    
         BAS   RE,CHKKEY           CHECK KEY                                    
         SPACE 1                                                                
         BAS   RE,VKTOKY           VALIDATE TO KEY DETAILS                      
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE FROM DETAILS                                 
*        COPY - AGY,NID REQUIRED (MUST HAVE ONE HOLD RECORD)                    
*        MOVE (W/IN AGY)   - SAME AS COPY                                       
*        MOVE (ACROSS AGY) - AGY,NID OR AGY,CLI,PRD,DATE,UID REQUIRED           
*        LIMITING REQUEST AS PER DAVID H - TOO DANGEROUS OTHERWISE              
         SPACE                                                                  
VKFRMKY  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SNXFAYH),SNXFAYNH                     
         MVC   FRMAGY,TGAGY                                                     
         MVC   FRMAGYN,TGNAME                                                   
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         LA    R2,SNXFNIDH         R2=A(FROM NETWORK/SPOT COMML ID FLD)         
         XC    FRMADID,FRMADID                                                  
         XC    TGADID,TGADID                                                    
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   *+12                                                             
         CLI   ACTNUM,ACTMOVE      MOVE MIGHT NOT REQUIRE IT                    
         BE    VKFRM20                                                          
         GOTO1 ANY                                                              
         MVC   FRMADID,WORK                                                     
         MVC   TGADID,FRMADID                                                   
         MVC   TGADID,FRMADID                                                   
*                                                                               
VKFRM20  LA    R2,SNXFCLH          R2=A(CLIENT FIELD)                           
         BAS   RE,VALFLD           VALIDATE KEY FIELD                           
         MVC   FRMCLI,WORK         RETURNS INPUT IN WORK                        
         MVC   TGNCLI,FRMCLI                                                    
*                                                                               
         LA    R2,SNXFPRH          R2=A(FROM PRODUCT FIELD)                     
         BAS   RE,VALFLD           VALIDATE KEY FIELD                           
         MVC   FRMPRD,WORK         RETURNS INPUT IN WORK                        
         MVC   TGNPRD,FRMPRD                                                    
*                                                                               
         LA    R2,SNXFMEDH         R2=A(FROM MEDIA FIELD)                       
         BAS   RE,VALFLD           VALIDATE KEY FIELD                           
         XC    TGMENAME,TGMENAME                                                
         XC    FRMMED,FRMMED                                                    
         OC    WORK,WORK                                                        
         BZ    VKFRM25                                                          
         GOTO1 MEDVAL,DMCB,WORK    RETURNS INPUT IN WORK                        
         BNE   FLDINV                                                           
         MVC   FRMMED,TGMENAME                                                  
*                                                                               
VKFRM25  LA    R2,SNXFUSEH         R2=A(FROM USE FIELD)                         
         BAS   RE,VALFLD           VALIDATE KEY FIELD                           
         XC    FRMUSE,FRMUSE                                                    
         XC    TGUSCDE,TGUSCDE                                                  
         OC    WORK,WORK                                                        
         BZ    VKFRM30                                                          
         GOTO1 USEVAL,DMCB,(X'40',WORK)                                         
         BNE   FLDINV                                                           
         MVC   FRMUSE,TGUSCDE      RETURNS INPUT IN WORK                        
*                                                                               
VKFRM30  LA    R2,SNXFADDH         R2=A(FROM ADDED DATE FIELD)                  
         XC    FRMDATE,FRMDATE                                                  
         BAS   RE,VALFLD           VALIDATE KEY FIELD                           
         OC    WORK,WORK                                                        
         BZ    VKFRM40                                                          
         GOTO1 DTVAL,DMCB,WORK                                                  
         MVC   FRMDATE,WORK                                                     
         XC    FRMDATE,XFFS        COMPLEMENT THE DATE                          
VKFRM40  MVC   TGDATE,FRMDATE                                                   
*                                                                               
         LA    R2,SNXFUIDH         R2=A(FROM USER ID)                           
         XC    FRMUSRS,FRMUSRS     CLEAR ID AND CODE                            
         XC    TGUSER,TGUSER                                                    
         BAS   RE,VALFLD           VALIDATE KEY FIELD                           
         OC    WORK,WORK                                                        
         BZ    VKFRMX                                                           
         GOTO1 USERVAL,DMCB,(X'B0',WORK)                                        
         BNE   ERRNTFD2                                                         
         MVC   FRMUSR,TGUSER                                                    
         MVC   FRMUSRID,TGUSERID                                                
VKFRMX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE SCREEN INPUT                                 
*                                  NTRY - R2 = A(SCREEN HEADER)                 
*                                  XIT  - SCREEN INPUT IN WORK                  
         SPACE 2                                                                
VALFLD   NTR1                                                                   
         XC    WORK,WORK           PRE-CLEAR INPUT                              
         CLI   5(R2),0                                                          
         BNE   VALFLD10                                                         
         CLI   ACTNUM,ACTCOPY                                                   
         BE    VALFLDX                                                          
         OC    FRMADID,FRMADID     INPUT REQUIRED ON MOVE IF NO ID              
         BNZ   VALFLDX                                                          
VALFLD10 GOTO1 ANY                 SETS INPUT IN WORK                           
*                                                                               
VALFLDX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO READ HOLD RECORD IF NECESSARY                         
         SPACE                                                                  
CHKKEY   NTR1                                                                   
         XC    SNXFNDN,SNXFNDN     CLEAR NETWORK/SPOT COMML TITLE               
         XC    SNXFCLN,SNXFCLN     CLEAR CLIENT NAME                            
         XC    SNXFPRN,SNXFPRN     CLEAR PRODUCT NAME                           
         OC    FRMADID,FRMADID     IF FROM COMML ID SPECIFIED                   
         BZ    CHKKEYX                                                          
         BAS   RE,MYRDHLD          READ HOLD RECORD                             
*                                  AND DISPLAY KEY VALUES TO SCREEN             
         L     R4,AIO                                                           
         USING TLNXD,R4                                                         
         GOTO1 CHAROUT,DMCB,TAFNELQ,SNXFNDNH,TAFNTTTL                           
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SNXFCLNH,TAFNTCLI                           
         MVC   SNXFCL,TLNXNCLI                                                  
         OI    SNXFCLH+6,X'80'                                                  
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SNXFPRNH,TAFNTPRD                           
         MVC   SNXFPR,TLNXNPRD                                                  
         OI    SNXFPRH+6,X'80'                                                  
*                                                                               
         MVC   SNXFMED,TLNXMED                                                  
         OI    SNXFMEDH+6,X'80'                                                 
*                                                                               
         MVC   SNXFUSE,TLNXUSE                                                  
         OI    SNXFUSEH+6,X'80'                                                 
*                                                                               
         MVC   WORK(L'TLNXDATE),TLNXDATE                                        
         XC    WORK,XFFS                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(5,SNXFADD)                                 
         OI    SNXFADDH+6,X'80'                                                 
*                                                                               
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4                                                         
         MVC   SNXFUID,TANXUID                                                  
         OI    SNXFUIDH+6,X'80'                                                 
*                                                                               
         XC    TGADID,TGADID                                                    
         CLI   TANXLEN,TANXLN2Q                                                 
         BL    CHKKEYX                                                          
         MVC   TGADID,TANXADID                                                  
*                                                                               
CHKKEYX  OI    SNXFNDNH+6,X'80'    TRANSMIT NAME FIELDS                         
         OI    SNXFCLNH+6,X'80'                                                 
         OI    SNXFPRNH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
MYRDHLD  NTR1                                                                   
         GOTO1 HIGH                READ HIGH                                    
         LA    R4,KEY                                                           
         USING TLNXD,R4                                                         
         B     MYRDHLD5                                                         
*                                                                               
MYRDHLD2 GOTO1 SEQ                 READ NEXT                                    
*                                                                               
MYRDHLD5 CLC   KEY(TLNXNCLI-TLNXD),KEYSAVE                                      
         BNE   ERRNTFND            NO HOLD RECORD FOUND                         
         TM    KEY+TLDRSTAT-TLDRD,TLNXSDEL+TLNXSUSD+TLNXSMAT                    
         BZ    *+8                                                              
         B     MYRDHLD2            SKIP DELETED/USED/MATCHED                    
*                                                                               
         OC    FRMCLI,FRMCLI       IF FROM NETWORK/SPOT CLIENT                  
         BZ    *+14                                                             
         CLC   TLNXNCLI,FRMCLI     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    FRMPRD,FRMPRD       IF FROM NETWORK/SPOT PRODUCT                 
         BZ    *+14                                                             
         CLC   TLNXNPRD,FRMPRD     MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    FRMMED,FRMMED       IF FROM MEDIA                                
         BZ    *+14                                                             
         CLC   TLNXMED,FRMMED      MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    FRMUSE,FRMUSE       IF FROM USE CODE                             
         BZ    *+14                                                             
         CLC   TLNXUSE,FRMUSE      MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    FRMDATE,FRMDATE     IF FROM DATE ADDED                           
         BZ    *+14                                                             
         CLC   TLNXDATE,FRMDATE    MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         OC    FRMUSR,FRMUSR       IF FROM USER ID NUMBER                       
         BZ    *+14                                                             
         CLC   TLNXUID,FRMUSR      MATCH ON IT                                  
         BNE   MYRDHLD2                                                         
         GOTO1 GETREC                                                           
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE TO DETAILS                                   
*                                                                               
VKTOKY   NTR1                                                                   
         CLI   ACTNUM,ACTCOPY      IF COPY                                      
         BNE   VKTO5                                                            
         MVC   TOAGY,FRMAGY        ALWAYS TO SAME AGENCY                        
         MVC   TOAGYN,FRMAGYN                                                   
         B     VKTO10                                                           
*                                  ELSE VALIDATE FOR MOVE                       
VKTO5    LA    R2,SNXTAYH          R2=A(TO AGENCY FIELD)                        
         GOTO1 ANY                 FORCE AGENCY INPUT                           
         XC    TGAGY,TGAGY                                                      
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SNXTAYNH                        
         MVC   TOAGY,TGAGY                                                      
         MVC   TOAGYN,TGNAME                                                    
*                                                                               
         CLC   FRMAGY,TOAGY        IF SAME AGENCY ON MOVE                       
         BNE   VKTO10                                                           
         OC    FRMADID,FRMADID     THEN FROM NET/SPOT COMML ID REQUIRED         
         BNZ   VKTO10                                                           
         LA    R2,SNXFNIDH                                                      
         B     FLDMISS                                                          
*                                                                               
VKTO10   LA    R2,SNXTNIDH         R2=A(TO NET/SPOT COMML ID FIELD)             
         XC    TOADID,TOADID                                                    
         CLI   ACTNUM,ACTCOPY      IF COPYING HOLD RECORDS                      
         BNE   *+12                                                             
         BAS   RE,VKCTNID          VALIDATE TO COMML ID FOR COPY ACTION         
         B     *+8                                                              
         BAS   RE,VKMTNID          VALIDATE TO COMML ID FOR MOVE ACTION         
*                                                                               
         LA    R2,SNXTNDNH         R2=A(TO NWK/SPOT TITLE FIELD)                
         XC    TONIDN,TONIDN       CLEAR NETWORK/SPOT TITLE                     
         CLI   5(R2),0             IF INPUT                                     
         BE    VKTOX                                                            
         OC    TOADID,TOADID       THEN MUST NWK/SPOT COMML ID                  
         BZ    FLDINV                                                           
         GOTO1 ANY                                                              
         MVC   TONIDN,WORK                                                      
*                                                                               
VKTOX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE TO NETWORK/SPOT COMML ID ON MOVE             
         SPACE                                                                  
VKMTNID  NTR1                                                                   
         CLI   5(R2),0             IF NO TO NWK/SPOT COMML ID INPUT             
         BNE   VKMTNID5                                                         
         CLC   FRMAGY,TOAGY        REQUIRED IF SAME AGENCIES                    
         BE    FLDMISS                                                          
         B     VKMTNIDX                                                         
*                                                                               
VKMTNID5 GOTO1 ANY                 IF TO NWK/SPOT COMML ID INPUT                
         MVC   TOADID,WORK                                                      
*                                                                               
         OC    FRMADID,FRMADID     FROM ID MUST BE SPECIFIED                    
         BZ    FLDINV                                                           
         CLC   FRMAGY,TOAGY        FROM ID MUST BE DIFF                         
         BNE   *+14                IF SAME AGENCIES                             
         CLC   FRMADID,TOADID                                                   
         BE    FLDINV                                                           
*                                                                               
VKMTNIDX B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE TO NETWORK/SPOT COMML ID ON COPY             
         SPACE                                                                  
VKCTNID  NTR1                                                                   
         GOTO1 ANY                 ID ALWAYS REQUIRED INPUT                     
         MVC   TOADID,WORK                                                      
*                                                                               
         CLC   FRMADID,TOADID      SINCE ALWAYS SAME AGENCY,                    
         BE    FLDINV              IDS MUST BE DIFFERENT                        
         B     XIT                                                              
         EJECT                                                                  
*              CALL SYSIO AND PRINT REPORT                                      
         SPACE 2                                                                
PROCESS  NTR1                                                                   
         LH    RF,=Y(TIEND-TASYSIOD)                                            
         XCEFL TASYSIOD                                                         
         LA    R0,LRHOOK                                                        
         ST    R0,TIHOOK                                                        
         ZAP   COUNTER,=P'0'       NUMBER OF HOLD RECORDS PROCESSED             
         MVI   PRNTED,C'N'         NOTHING PRINTED                              
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLNXCDQ                                                   
         MVC   TIKHOOK,SETLSTK                                                  
         OI    TIQFLAGS,TIQFDIR    PASS DIRECTORY HOOKS                         
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIFAGY,FRMAGY              SET AGENCY                            
         MVC   TIQSTART(L'TLNXNID),FRMADID  SET NETWORK/SPOT COMML ID           
         CLI   FRMADID+8,C' '      IS COMML > 8 CHARACTERS?                     
         BNH   PROC30                                                           
         XC    DUB,DUB                                                          
         GOTO1 ATRPACK,DMCB,(C'P',FRMADID),DUB      PACK IT                     
         MVC   TIQSTART(L'TLNXNID),DUB    SET PACKED NETWORK COMML ID           
         OI    TIQFLAG3,TIQHLPCK    SET TO READ PACKED COMMLS FIRST             
PROC30   GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
         CLI   MODE,PRINTREP                                                    
         BNE   PROCX                                                            
         CP    COUNTER,=P'0'       IF PRINTED A COUNT                           
         BE    PROC40                                                           
         BAS   RE,PRINTIT          SKIP A LINE                                  
         EDIT  COUNTER,(5,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(12,R1),=C'HOLD RECORDS'                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
PROC40   CLI   PRNTED,C'Y'         IF ANYTHING PRINTED                          
         BNE   PROCX                                                            
         TM    WHEN,X'40'          AND IF SPOOLING NOW                          
         BZ    PROCX                                                            
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
PROCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS PASSED BY SYSIO                       
         SPACE                                                                  
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS DIRECTORY                            
         BNE   LRH30                                                            
         BAS   RE,FILTER           FILTER DIRECTORY RECORD                      
         BE    YES                                                              
         B     NO                                                               
*                                                                               
LRH30    CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
*                                                                               
         OC    FRMADID,FRMADID      IF FROM NID SPECIFIED                       
         BZ    *+14                                                             
         CLC   TICID,FRMADID        MATCH ON IT                                 
         BNE   NO                                                               
         BAS   RE,SETGVALS         GET GLOBAS VALUES                            
*                                                                               
         CLI   ACTNUM,ACTCOPY      IF HOLD/COPY TO COPY RECORDS                 
         BNE   *+12                                                             
         BAS   RE,SETCPY           SET TO COPY RECORD                           
         B     LRH40                                                            
*                                                                               
         BAS   RE,SETMVC           ELSE, SET NEW RECORD                         
*                                                                               
LRH40    CLI   NEWCTYPE,C'R'       IF HOLD RECORD PROCESSED                     
         BE    *+10                                                             
         AP    COUNTER,=P'1'       ADD TO COUNTER                               
         BAS   RE,PLINE            PRINT NEW HOLD RECORD INFO                   
*                                                                               
LRHX     XC    KEY,KEY             RE-READ SYSIO'S KEY                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER HOLD RECORDS                                   
         SPACE                                                                  
FILTER   NTR1                                                                   
         LA    R6,TIKEY            R6=A(SYSIO'S KEY)                            
         USING TLNXD,R6                                                         
*                                                                               
         CLI   TLNXSEQ,0           PROCESS BASE RECORDS ONLY                    
         BNE   NO                                                               
*                                                                               
         TM    TIKEYST,TLNXSDEL    SKIP 'DELETED' RECORDS                       
         BO    NO                                                               
         TM    TIKEYST,TLNXSUSD    SKIP 'USED' RECORDS                          
         BO    NO                                                               
         TM    TIKEYST,TLNXSMAT    SKIP 'MATCHED' RECORDS                       
         BO    NO                                                               
*                                                                               
         OC    FRMCLI,FRMCLI       IF FROM CLIENT SPECIFIED                     
         BZ    *+14                                                             
         CLC   TICLI(L'FRMCLI),FRMCLI  MATCH ON IT                              
         BNE   NO                                                               
*                                                                               
         OC    FRMPRD,FRMPRD       IF FROM PRODUCT SPECIFIED                    
         BZ    *+14                                                             
         CLC   TIPRD(L'FRMPRD),FRMPRD  MATCH ON IT                              
         BNE   NO                                                               
*                                                                               
         OC    FRMMED,FRMMED       IF FROM MEDIA                                
         BZ    *+14                                                             
         CLC   FRMMED,TIMED        CHECK MEDIA CODE                             
         BNE   NO                                                               
*                                                                               
         OC    FRMUSE,FRMUSE       IF FROM USE                                  
         BZ    *+14                                                             
         CLC   FRMUSE,TIUSE        CHECK USE CODE                               
         BNE   NO                                                               
*                                                                               
         OC    FRMDATE,FRMDATE     IF FROM DATE ADDED                           
         BZ    *+14                                                             
         CLC   FRMDATE,TLNXDATE    CHECK DATE ADDED                             
         BNE   NO                                                               
*                                                                               
         OC    FRMUSR,FRMUSR       IF FROM USER ID NUMBER SPECIFIED             
         BZ    *+14                                                             
         CLC   FRMUSR,TLNXUID      MATCH ON IT                                  
         BNE   NO                                                               
*                                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO SET GLOBAS VALUES                                     
         SPACE                                                                  
SETGVALS NTR1                                                                   
         MVC   TGAGY,TIAGY         SET GLOBAS AGENCY                            
         OC    TOAGY,TOAGY                                                      
         BZ    *+10                                                             
         MVC   TGAGY,TOAGY                                                      
*                                                                               
         MVC   TGNCLI,TICLI        SET GLOBAS CLIENT                            
         MVC   TGNPRD,TIPRD        SET GLOBAS PRODUCT                           
*                                                                               
         MVC   TGADID,TICID        SET NETWORK/SPOT COMML ID                    
         OC    TOADID,TOADID                                                    
         BZ    *+10                                                             
         MVC   TGADID,TOADID                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET RECORD FOR MOVE                                   
         SPACE                                                                  
SETMVC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING TLDRD,R6                                                         
         MVC   TLDRDA,TIDSKADD                                                  
         MVC   AIO,AIO1            SET IOAREA                                   
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
*                                                                               
         L     R6,AIO              R6=A(HOLD RECORD)                            
         USING TLNXD,R6                                                         
         BAS   RE,SETNKEY          SET NEW KEY DETAILS                          
*                                                                               
         BAS   RE,SETNREC          SET NEW RECORD                               
*                                                                               
         BAS   RE,GETMTCH          CHECK ID IS A MATCH ON NEW AGY               
         BNE   SETMVC30            RETURNS NEWCOM,NEWCTYPE                      
*                                                                               
         OI    TLNXSTAT,TLNXSMAT   SET RECORD MATCHED                           
         BAS   RE,SETTANX          SET TANXD ELEMENT                            
*                                                                               
SETMVC30 BAS   RE,CHKDUP           CHECK ALREADY ON FILE                        
         CLI   NEWCTYPE,C'R'       IF ERROR FOUND                               
         BE    SETMVCX             DON'T MOVE HOLD RECORD                       
*                                                                               
         XC    KEY,KEY             READ OLD RECORD FOR UPDATE                   
         MVC   KEY+TLDRDA-TLDRD(4),TIDSKADD                                     
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
         OI    TLNXSTAT,TLNXSDEL   DELETE                                       
*                                                                               
         GOTO1 PUTREC              WRITE BACK OLD RECORD                        
         MVI   BYTE,X'80'          CHANGE ACTIVE POINTER                        
         CLC   TIKEYST,TLNXSTAT    AND IF STATUS CHANGED                        
         BE    *+8                                                              
         OI    BYTE,X'20'          FORCE CHANGE ALL                             
         GOTO1 ADDPTRS,DMCB,(X'A0',PTRBLK)                                      
*                                                                               
         MVC   AIO,AIO1            ADD NEW RECORD                               
*                                                                               
         BAS   RE,SETTANX2         SET TANXD ELEMENT                            
*                                                                               
         GOTO1 ADDREC              ADD NEW RECORD                               
         LA    R2,PTRBLK                                                        
         XC    0(L'TLDRREC+1,R2),0(R2)                                          
         GOTO1 ADDPTRS,DMCB,(R2)                                                
*                                                                               
         BAS   RE,MVCREST          MOVE RECS(IF ANY) BELONGING TO BASE          
SETMVCX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO SET NEW AGENCY IN SUBSEQUENT RECORDS                  
*              THAT BELONG TO BASE JUST CHANGED                                 
         SPACE                                                                  
MVCREST  NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA                                   
         XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(L'TIKEY),TIKEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MVCRES40 GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXKEY),TIKEY                                       
         BNE   MVCRESX                                                          
         MVC   SVKEY,KEY           SAVE KEY                                     
         LA    R6,KEY                                                           
         USING TLDRD,R6                                                         
         MVC   NXDSKADD,TLDRDA     SAVE DISK ADDRESS                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              READ OLD RECORD TO AIO1 TO CHANGE            
         GOTO1 SAVPTRS,DMCB,PTRBLK   FOR NEW RECORD                             
         L     R6,AIO                                                           
         USING TLNXD,R6                                                         
         BAS   RE,SETNKEY          SET NEW KEY DETAILS                          
*                                                                               
         XC    KEY,KEY             READ OLD RECORD FOR UPDATE IN AIO2           
         MVC   KEY+TLDRDA-TLDRD(4),NXDSKADD                                     
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         OI    TLNXSTAT,TLNXSDEL   DELETE OLD RECORD                            
         GOTO1 PUTREC                                                           
*                                                                               
         MVI   BYTE,X'80'          CHANGE ACTIVE POINTER                        
         CLC   TIKEYST,TLNXSTAT    AND IF STATUS CHANGED                        
         BE    *+8                                                              
         OI    BYTE,X'20'          FORCE CHANGE ALL                             
         GOTO1 ADDPTRS,DMCB,(X'A0',PTRBLK)                                      
*                                                                               
         MVC   AIO,AIO3            ADD NEW RECORD                               
         GOTO1 ADDREC                                                           
         LA    R2,PTRBLK                                                        
         XC    0(L'TLDRREC+1,R2),0(R2)                                          
         GOTO1 ADDPTRS,DMCB,(R2)                                                
*                                                                               
         XC    KEY,KEY             RE-SET READ SEQUENCE                         
         MVC   KEY(L'SVKEY),SVKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(L'SVKEY),SVKEY                                               
         BE    MVCRES40                                                         
         DC    H'0'                                                             
         B     MVCRES40                                                         
*                                                                               
MVCRESX  MVC   AIO,AIO1            RESET IOAREA TO BASE RECORD                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO SET RECORD FOR COPY                                   
         SPACE                                                                  
SETCPY   NTR1                                                                   
         BAS   RE,CPYREC           COPY RECORD PASSED TO AIO1                   
         L     R6,AIO1             R6=A(NEW HOLD RECORD TO ADD)                 
         USING TLNXD,R6                                                         
         BAS   RE,SETNKEY          SET NEW KEY DETAILS                          
*                                                                               
         BAS   RE,SETNREC          SET NEW RECORD                               
*                                                                               
         BAS   RE,GETMTCH          CHECK ID IS A MATCH ON NEW AGY               
         BNE   SETCPY30            RETURNS NEWCOM,NEWCTYPE                      
*                                                                               
         OI    TLNXSTAT,TLNXSMAT   SET RECORD MATCHED                           
         BAS   RE,SETTANX          SET TANXD ELEMENT                            
*                                                                               
SETCPY30 BAS   RE,CHKDUP           CHECK ALREADY ON FILE                        
         CLI   NEWCTYPE,C'R'       IF HOLD RECORD ALREADY EXISTS                
         BE    SETCPYX             DON'T COPY HOLD RECORDS                      
*                                                                               
         BAS   RE,SETTANX2         SET TANXD ELEMENT                            
*                                                                               
         GOTO1 ADDREC              ADD NEW RECORD                               
         LA    R2,PTRBLK                                                        
         XC    0(L'TLDRREC+1,R2),0(R2)                                          
         GOTO1 ADDPTRS,DMCB,(R2)                                                
*                                                                               
         BAS   RE,CPYREST          COPY RECS(IF ANY) BELONGING TO BASE          
SETCPYX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COPY SUBSEQUENT RECORDS                               
*              THAT BELONG TO BASE JUST CHANGED                                 
         SPACE                                                                  
CPYREST  NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA                                   
         XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),TIKEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CPYRES40 GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXKEY),TIKEY                                       
         BNE   CPYRESX                                                          
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING TLNXD,R6                                                         
         BAS   RE,SETNKEY          SET NEW KEY                                  
         GOTO1 ADDREC              ADD NEW RECORD                               
         LA    R2,PTRBLK                                                        
         XC    0(L'TLDRREC+1,R2),0(R2)                                          
         GOTO1 ADDPTRS,DMCB,(R2)                                                
*                                                                               
         XC    KEY,KEY             RE-SET READ SEQUENCE                         
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SVKEY),SVKEY                                               
         BE    CPYRES40                                                         
         DC    H'0'                                                             
*                                                                               
CPYRESX  MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COPY SYSIO RECORD TO AIO1                             
         SPACE                                                                  
CPYREC   NTR1                                                                   
         L     R2,TIAREC           FROM                                         
         USING TLRCD,R2                                                         
         LH    R3,TLRCLEN                                                       
         L     RE,AIO1             TO                                           
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO SET KEY FOR HOLD COPY OR MOVE                         
         SPACE                                                                  
         USING TLNXD,R6            R6=A(HOLD RECORD)                            
SETNKEY  NTR1                                                                   
         NI    TLNXSTAT,X'FF'-TLNXPACK   TURN OFF PACKED STATUS                 
         MVC   TLNXAGY,TGAGY       SET NEW AGENCY                               
         MVC   TLNXNID,TGADID      SET NEW NETWORK/SPOT COMML ID                
         CLI   TGADID+8,C' '       IS COMML > 8 CHARACTERS?                     
         BNH   XIT                                                              
         XC    DUB,DUB                                                          
         GOTO1 ATRPACK,DMCB,(C'P',TGADID),DUB      PACK IT                      
         MVC   TLNXNID,DUB         SET PACKED NETWORK COMML ID                  
         OI    TLNXSTAT,TLNXPACK   SET AS PACKED COMMERCIAL                     
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
*              SET NEW HOLD RECORD                                              
         SPACE                                                                  
         USING TLNXD,R6                                                         
SETNREC  NTR1                                                                   
         NI    TLNXSTAT,X'FF'-TLNXSMAT   SET RECORD UNMATCHED                   
         BAS   RE,CLRTANX          CLEAR TANXD ELEMENT                          
*                                                                               
         GOTO1 SETTAFN,DMCB,TAFNTTTL,(L'TONIDN,TONIDN),TAFNELQ                  
*                                                                               
         BAS   RE,ADDTAON          ADD TAOND ELEMENT                            
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO DELETE AND ADD NEW TAFND ELEMENT                      
*                                  P1      - ELEMENT TYPE                       
*                                  P2(0)   - L'DESTINATION                      
*                                  P2(1-3) - A(DESTINATION)                     
*                                  P3      - ELEMENT CODE                       
         SPACE                                                                  
SETTAFN  NTR1                                                                   
         ZIC   R2,11(R1)           ELEMENT CODE                                 
         STC   R2,ELCODE                                                        
         MVC   BYTE,3(R1)          SEARCH TYPE FOR DELL                         
         L     R4,4(R1)            A(DESTINATION)                               
         ZIC   R3,4(R1)            LENGTH OF DESTINATION                        
         BCTR  R3,0                LESS ONE FOR EX                              
*                                                                               
         EX    R3,*+8              IF TO NAME SPECIFIED                         
         B     *+10                                                             
         OC    0(0,R4),0(R4)                                                    
         BZ    SETTAFNX                                                         
*                                                                               
         GOTO1 DELL,DMCB,(1,BYTE)  DELETE OLD NAME                              
*                                                                               
         XC    ELEMENT,ELEMENT     AND ADD NEW NAME                             
         LA    RE,ELEMENT                                                       
         USING TAFND,RE                                                         
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ                                                  
         MVC   TAFNTYPE,BYTE                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),0(R4)                                                
         LA    R3,1(R3)                                                         
         ZIC   R1,TAFNLEN                                                       
         AR    R1,R3                                                            
         STC   R1,TAFNLEN                                                       
         GOTO1 ADDELEM                                                          
SETTAFNX B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO CLEAR TANXD                                           
         SPACE                                                                  
CLRTANX  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TANXELQ      R4=A(NETWORK/SPOT TRANSFER DETAILS)          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4                                                         
         XC    TANXMDTE,TANXMDTE   CLEAR MATCHED                                
         XC    TANXCOM,TANXCOM     CLEAR INTERNAL COMMERCIAL #                  
         MVI   TANXTYPE,0          CLEAR COMMERCIAL TYPE                        
         MVI   TANXVERS,0          CLEAR VERSION NUMBER                         
         CLI   TANXLEN,TANXLN2Q    CLEAR ADID IF THERE IS ONE                   
         BL    XIT                                                              
         XC    TANXADID,TANXADID                                                
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO SET TANXD                                             
         SPACE                                                                  
SETTANX  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TANXELQ      R4=A(NETWORK/SPOT TRANSFER DETAILS)          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4                                                         
         MVC   TANXMDTE,TGTODAY1   SET MATCHED TODAY                            
         MVC   TANXCOM,NEWCOM      SET INTERNAL COMMERCIAL #                    
         MVC   TANXTYPE,NEWCTYPE   SET COMMERCIAL TYPE                          
         MVC   TANXVERS,NEWVER     SET VERSION NUMBER                           
         SPACE 1                                                                
***      CLC   TGADID+8(4),SPACES                                               
***      BE    XIT                                                              
*                                                                               
         CLI   TANXLEN,TANXLN2Q    DOES ELEMENT HAVE ROOM FOR ADID?             
         BL    STANX20                                                          
         MVC   TANXADID,TGADID     SAVE ADID                                    
         B     XIT                                                              
*                                                                               
STANX20  XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(TANXLNQ),0(R4)  SAVE ELEMENT                             
         MVI   ELCODE,TANXELQ                                                   
         GOTO1 REMELEM             REMOVE OLD ELEMENT                           
         DROP  R4                                                               
*                                                                               
         LA    R4,ELEMENT                                                       
         USING TANXD,R4                                                         
         MVI   TANXLEN,TANXLN2Q    ADD NEW LENGTH ELEMENT                       
         MVC   TANXADID,TGADID     SAVE ADID                                    
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO SET ONLY ADID IN TANXD                                
                                                                                
SETTANX2 NTR1                                                                   
****     CLC   TGADID+8(4),SPACES  IF MOVING/COPYING TO AN AD-ID                
****     BE    XIT                                                              
                                                                                
         USING TANXD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANXELQ      R4=A(NETWORK/SPOT TRANSFER DETAILS)          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TANXLEN,TANXLN2Q    DOES ELEMENT HAVE ROOM FOR ADID?             
         BL    STANX220                                                         
         MVC   TANXADID,TGADID     SAVE AD-ID IN TANX ELEMENT                   
         B     XIT                                                              
STANX220 XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(TANXLNQ),0(R4)  SAVE ELEMENT                             
         MVI   ELCODE,TANXELQ                                                   
         GOTO1 REMELEM             REMOVE OLD ELEMENT                           
         DROP  R4                                                               
*                                                                               
         LA    R4,ELEMENT                                                       
         USING TANXD,R4                                                         
         MVI   TANXLEN,TANXLN2Q    ADD NEW LENGTH ELEMENT                       
         MVC   TANXADID,TGADID     SAVE ADID IN TANX ELEMENT                    
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R4                                                               
                                                                                
*              ROUTINE TO ADD TAOND ELEMENT                                     
         SPACE                                                                  
ADDTAON  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          R4=A(OLD AGENCY/ID ELEMENT)                  
         USING TAOND,R4                                                         
         MVI   TAONEL,TAONELQ      ELEMENT CODE                                 
         MVI   TAONLEN,TAONLNQ     ELEMENT LENGTH                               
         MVC   TAONDTE,TGTODAY1    EFFECTIVE DATE COMPLEMENTED                  
         XC    TAONDTE,XFFS                                                     
         TIME  DEC                                                              
         STCM  R0,14,TAONTIM                                                    
         XC    TAONTIM,XFFS        EFFECTIVE TIME COMPLEMENTED                  
         MVC   TAONAGY,TIAGY       FROM AGENCY                                  
         MVC   TAONNID,TICID       FROM NETWORK/SPOT COMML ID                   
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK IF HOLD RECORD SHOULD BE MATCHED                
*                                  XIT - NEWCOM AND NEWCTYPE SET                
         SPACE                                                                  
GETMTCH  NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA                                   
*                                                                               
         BAS   RE,CHKNID           CHECK ID IS A MATCH                          
         MVC   AIO,AIO1            RESET IOAREA                                 
         BNE   NO                                                               
*                                                                               
         L     R4,AIO2             R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   TGCID,TACOCID       SET TALENT COMM'L ID                         
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
       ++INCLUDE TACHKNID                                                       
         EJECT                                                                  
*              ROUTINE TO CHECK HOLD RECORD ALREADY EXISTS BEFORE               
*              MOVE OR COPY PROCESSED                                           
*              NOTE - CHECKING DELETED SO DON'T DIE ON ADD - SHOULD             
*              BE ABLE TO HANDLE DELETED (TOM SAYS THIS OKAY FOR NOW)           
         SPACE                                                                  
CHKDUP   NTR1                                                                   
         L     R6,AIO              R6=(HOLD RECORD TO BE PROCESSED)             
         USING TLNXD,R6                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLNXKEY),TLNXKEY                                           
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLNXKEY),KEYSAVE                                           
         BNE   CHKDUPX                                                          
         MVI   NEWCTYPE,C'R'       ERROR HOLD RECORD ALREADY EXISTS             
*                                                                               
CHKDUPX  NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT NEW RECORD INFO                                 
         SPACE                                                                  
PLINE    NTR1                                                                   
         MVC   P,SPACES                                                         
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         L     R6,AIO              R6=A(HOLD RECORD FROM SYSIO)                 
         USING TLNXD,R6                                                         
*                                                                               
         MVC   LINNID(L'TLNXNID),TLNXNID    NETWORK/SPOT COMML ID               
         TM    TLNXSTAT,TLNXPACK            IF PACKED, UNPACK                   
         BZ    PLINE10                                                          
         XC    WORK,WORK                                                        
         GOTO1 ATRPACK,DMCB,(C'U',TLNXNID),WORK                                 
         MVC   LINNID,WORK                                                      
PLINE10  MVC   LINCLI,TLNXNCLI     NETWORK/SPOT CLIENT CODE                     
         MVC   LINPRD,TLNXNPRD     NETWORK/SPOT PRODUCT CODE                    
         MVC   LINMED,TLNXMED      NETWORK/SPOT MEDIA CODE                      
         MVC   LINUSE,TLNXUSE      NETWORK/SPOT USE CODE                        
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4            R4=A(NETWORK/SPOT XFER DTL EL)               
*                                                                               
         EDIT  TANXSEC,(3,LINLEN)  LENGTH IN SECONDS                            
*                                                                               
         MVC   WORK(L'TLNXDATE),TLNXDATE   DATE ADDED                           
         XC    WORK(L'TLNXDATE),XFFS                                            
         GOTO1 DATCON,DMCB,(1,WORK),(5,LINDATE)                                 
*                                                                               
         CLI   NEWCTYPE,C'R'       IF ERROR WHILE PROCESSING                    
         BNE   *+14                                                             
         MVC   LINCID(31),=CL31'SKIPPED - RECORD ALREADY EXISTS'                
         B     PLINEX                                                           
*                                                                               
         TM    TLNXSTAT,TLNXSMAT   IF HOLD RECORD MATCHED                       
         BZ    PLINEX                                                           
*                                                                               
         CLI   TANXTYPE,C'E'       IF MATCHED EXACTLY                           
         BNE   *+14                                                             
         MVC   LINCID,=CL12'(SAME)' THEN NWK/SPOT ID= TALENT ID                 
         B     *+10                                                             
         MVC   LINCID,TGCID        ELSE DISPLAY TALENT ID                       
*                                                                               
PLINE30  GOTO1 DATCON,DMCB,(1,TANXMDTE),(5,LINMTCH)                             
*                                                                               
PLINEX   BAS   RE,PRINTIT          PRINT THE LINE                               
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A LINE                                                     
         SPACE                                                                  
PRINTIT  NTR1                                                                   
         MVI   PRNTED,C'Y'         PRINTED A LINE OF INFO                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              HEADLINE ROUTINE                                                 
         SPACE                                                                  
HOOK     NTR1                                                                   
         LA    R2,HEAD3            SUB-TITLE                                    
         CLI   ACTNUM,ACTCOPY      IF COPY                                      
         BNE   *+14                                                             
         MVC   34(6,R2),=C'(COPY)'                                              
         B     *+10                                                             
         MVC   34(6,R2),=C'(MOVE)'                                              
*                                                                               
         BAS   RE,FRMHEADS         PRINT FROM HEADLINE DETAILS                  
*                                                                               
         BAS   RE,TOHEADS          PRINT TO HEADLINE DETAILS                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT FROM HEADLINE DETAILS                           
         SPACE                                                                  
FRMHEADS NTR1                                                                   
         LA    R2,HEAD4            HEADLINES FOR FROM DETAILS                   
         MVC   14(L'FRMAGY,R2),FRMAGY                                           
         MVC   27(17,R2),FRMAGYN                                                
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
         OC    FRMADID,FRMADID     FROM NETWORK/SPOT COMML ID                   
         BZ    FRMHK5                                                           
         MVC   6(6,R2),=C'TRF ID'                                               
         MVC   14(L'FRMADID,R2),FRMADID                                         
         OC    SNXFNDN,SNXFNDN                                                  
         BZ    *+10                                                             
         MVC   27(17,R2),SNXFNDN   FROM TITLE                                   
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
FRMHK5   OC    FRMCLI,FRMCLI       FROM CLIENT                                  
         BZ    FRMHK10                                                          
         MVC   6(6,R2),=C'CLIENT'                                               
         MVC   14(L'FRMCLI,R2),FRMCLI                                           
         OC    SNXFCLN,SNXFCLN                                                  
         BZ    *+10                                                             
         MVC   27(17,R2),SNXFCLN   FROM CLIENT NAME                             
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
FRMHK10  OC    FRMPRD,FRMPRD       FROM PRODUCT                                 
         BZ    FRMHK15                                                          
         MVC   6(7,R2),=C'PRODUCT'                                              
         MVC   14(L'FRMPRD,R2),FRMPRD                                           
         OC    SNXFPRN,SNXFPRN                                                  
         BZ    *+10                                                             
         MVC   27(17,R2),SNXFPRN   FROM PRODUCT NAME                            
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
FRMHK15  OC    FRMMED,FRMMED       FROM MEDIA                                   
         BZ    FRMHK18                                                          
         MVC   6(5,R2),=C'MEDIA'                                                
         MVC   14(L'FRMMED,R2),FRMMED                                           
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
FRMHK18  OC    FRMUSE,FRMUSE       FROM USE                                     
         BZ    FRMHK20                                                          
         MVC   6(3,R2),=C'USE'                                                  
         MVC   14(L'TGUSCDE,R2),FRMUSE                                          
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
FRMHK20  OC    FRMDATE,FRMDATE     FROM DATE ADDED                              
         BZ    FRMHK40                                                          
         MVC   6(5,R2),=C'ADDED'                                                
         MVC   WORK(L'FRMDATE),FRMDATE                                          
         XC    WORK(L'FRMDATE),XFFS                                             
         GOTO1 DATCON,DMCB,(1,WORK),(5,14(R2))                                  
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
FRMHK40  OC    FRMUSRID,FRMUSRID     FROM USERID                                
         BZ    FRMHKX                                                           
         MVC   6(6,R2),=C'USERID'                                               
         MVC   14(L'FRMUSRID,R2),FRMUSRID                                       
FRMHKX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT TO HEADLINE DETAILS                             
         SPACE                                                                  
TOHEADS  NTR1                                                                   
         LA    R2,HEAD4            HEADLINES FOR TO DETAILS                     
         MVC   55(L'TOAGY,R2),TOAGY                                             
         MVC   68(12,R2),TOAGYN                                                 
         LA    R2,L'HEAD1(R2)                                                   
*                                                                               
         OC    TOADID,TOADID       TO NETWORK/SPOT COMML ID                     
         BZ    TOHKX                                                            
         MVC   47(7,R2),=C'TRAF ID'                                             
         MVC   55(L'TOADID,R2),TOADID                                           
*                                                                               
         OC    TONIDN,TONIDN       IF TO TITLE                                  
         BZ    *+14                                                             
         MVC   68(12,R2),TONIDN    PRINT IT                                     
         B     TOHKX                                                            
*                                                                               
         OC    SNXFNDN,SNXFNDN     ELSE IF FROM TITLE                           
         BZ    *+10                                                             
         MVC   68(12,R2),SNXFNDN   PRINT IT                                     
*                                                                               
TOHKX    B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ERRNTFND LA    R2,SNXFAYH                                                       
ERRNTFD2 MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
ERRFRM   LA    R2,SNXFAYH          ALL FROM DETAILS MUST BE SPECIFIED           
         MVI   ERROR,ERNXFRM       FOR NOW REQUEST                              
         B     THEEND                                                           
         SPACE 1                                                                
PFKMSG   LA    R2,CONRECH                                                       
         OI    STATUS,PFKPEND      SET PFKEY 13 PENDING                         
         MVI   MYMSGNO1,35                                                      
         B     INFEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
XFFS     DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR REPORT                                                 
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,32,C'HOLD RECORDS'                                            
         SSPEC H2,32,12X'BF'                                                    
         SPACE 1                                                                
         SSPEC H4,2,C'FROM AGENCY'                                              
         SPACE 1                                                                
         SSPEC H4,45,C'TO AGENCY'                                               
         SPACE 1                                                                
         SSPEC H12,2,C'TRAF ID       LEN  CLI  PRD  M  USE  ADDED'              
         SSPEC H13,2,C'-------       ---  ---  ---  -  ---  -----'              
         SSPEC H12,44,C'TALENT CID    MATCHED'                                  
         SSPEC H13,44,C'----------    -------'                                  
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE                                                                  
LINED    DSECT                                                                  
         DS    CL1                                                              
*                                                                               
LINNID   DS    CL12                NETWORK/SPOT COMML ID                        
         DS    CL2                                                              
LINLEN   DS    CL3                 COMMERCIAL LENGTH                            
         DS    CL2                                                              
LINCLI   DS    CL3                 NETWORK/SPOT CLIENT                          
         DS    CL2                                                              
LINPRD   DS    CL3                 NETWORK/SPOT PRODUCT                         
         DS    CL2                                                              
LINMED   DS    CL1                 NETWORK/SPOT MEDIA                           
         DS    CL2                                                              
LINUSE   DS    CL3                 NETWORK/SPOT USE                             
         DS    CL2                                                              
LINDATE  DS    CL8                 DATE ADDED                                   
         DS    CL2                                                              
LINCID   DS    CL12                TALENT COMMERCIAL ID                         
         DS    CL2                                                              
LINMTCH  DS    CL8                 MATCHED DATE                                 
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD6D                                                       
*                                                                               
         ORG   SNXWORK                                                          
*                                                                               
ATRPACK  DS    A                   A(TRPACK)                                    
*                                                                               
FRMVALS  DS    0C                  FROM VALUES                                  
FRMAGY   DS    CL6                 FROM NETWORK/SPOT AGENCY                     
FRMAGYN  DS    CL36                FROM NETWORK/SPOT AGENCY NAME                
FRMCLI   DS    CL3                 FROM NETWORK/SPOT CLIENT                     
FRMPRD   DS    CL3                 FROM NETWORK/SPOT PRODUCT                    
FRMMED   DS    CL1                 FROM NETWORK/SPOT MEDIA                      
FRMUSE   DS    CL3                 FROM NETWORK/SPOT USE CODE                   
FRMADID  DS    CL12                FROM NETWORK/SPOT COMML ID                   
FRMDATE  DS    XL3                 FROM DATE ADDED (COMPLEMENTED)               
FRMUSRS  DS    0CL8                                                             
FRMUSR   DS    XL2                 FROM NETWORK/SPOT USERID NUMBER              
FRMUSRID DS    CL6                 FROM NETWORK/SPOT USER                       
FRMVALSL EQU   *-FRMVALS                                                        
*                                                                               
TOVALS   DS    0C                  TO VALUES                                    
TOAGY    DS    CL6                 TO TALENT AGENCY                             
TOAGYN   DS    CL36                TO TALENT AGENCY NAME                        
TOADID   DS    CL12                TO NETWORK/SPOT COMML ID                     
TONIDN   DS    CL36                TO NETWORK/SPOT COMML TITLE                  
TOVALSL  EQU   *-TOVALS                                                         
*                                                                               
STATUS   DS    XL1                 STATUS BYTE                                  
PFKPEND  EQU   X'80'               PFKEY CONFIRMATION PENDING                   
*                                                                               
PRNTED   DS    CL1                 Y= PRINTED SOMETHING                         
NEWCTYPE DS    CL1                 COMMERCIAL TYPE                              
NEWCOM   DS    XL4                 TALENT INTERNAL COMMERCIAL RECORD            
NEWVER   DS    CL1                 COMMERCIAL VERSION LETTER                    
COUNTER  DS    PL4                 COUNTER                                      
SVKEY    DS    CL(L'TLDRKEY)       SAVE KEY                                     
NXDSKADD DS    A                                                                
PTRBLK   DS    CL((3*L'TLDRREC)+1) ACTIVE + PASSIVE                             
         DS    0X                                                               
         SPACE 3                                                                
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSIOD   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         ORG   CONTAGH+2300                                                     
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
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035TAGEND6   04/01/13'                                      
         END                                                                    
