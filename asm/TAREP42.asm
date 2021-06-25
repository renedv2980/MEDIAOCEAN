*          DATA SET TAREP42    AT LEVEL 186 AS OF 03/09/17                      
*PHASE T70342B,*                                                                
*                                                                               
         TITLE 'T70342 - ADVICE GENERATE'                                       
T70342A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYAREAL,T70342,R5,RR=R2                                          
         LR    R3,RC                                                            
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF             R7=A(LOCAL W/S)                              
         LA    R7,8(R7)                                                         
         USING WORKD,R7                                                         
         ST    R3,AMKTTAB          A(MARKET TABLE)                              
         AHI   R3,MAXMKTL                                                       
         ST    R3,ASRTREC          A(SORT RECORD AREA)                          
         ST    R2,RELO             RELOCATION FACTOR                            
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)                                                        
         USING COMFACSD,R1                                                      
         MVC   MYGETRET,CGETRET                                                 
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'  GET ADDRESS OF TRPACK                    
         GOTO1 CALLOV,DMCB                                                      
         MVC   ATRPACK,0(R1)                                                    
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         IF MODE TO VALIDATE                          
         BNE   ADVG40                                                           
         MVI   ADIDFLG,0           INITIALIZE AD-ID FLAG                        
*                                                                               
         TM    WHEN,X'20'          IF SOON PROCESSING                           
         BZ    ADVG20                                                           
******** TM    WHEN,X'40'          IF NOW PROCESSING                            
******** BZ    ADVG20                                                           
         BRAS  RE,CHKFVAL          IF NO FIELDS CHANGED                         
         BNE   *+12                                                             
         TM    STATUS,PFKPEND      AND PFKEY IS NOT PENDING                     
         BO    ADVG10              CONTINUE                                     
         BRAS  RE,CLRNAMES         ELSE, CLEAR NAME FIELDS                      
         BRAS  RE,VKSOON           VALIDATE KEY FOR NOW REQUEST                 
         BRAS  RE,SETFVAL          SET ALL FIELDS VALID                         
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BE    ADVGX               CONTINUE                                     
         B     PFKMSG                                                           
*                                                                               
ADVG10   CLI   PFAID,13            INSURE PF13 HIT                              
         BNE   PFKMSG                                                           
         BRAS  RE,VKSOON                                                        
         B     ADVGX                                                            
*                                                                               
ADVG20   CLI   OFFLINE,C'Y'                                                     
         BNE   ERRPRNT             NOT ALLOWED TO REQUEST FOR OV                
         BRAS  RE,CLRNAMES         CLEAR NAME FIELDS                            
         BAS   RE,VKOV             VALIDATE KEY FOR OVERNIGHT                   
         B     ADVGX                                                            
         SPACE 2                                                                
ADVG40   CLI   MODE,PRINTREP       IF MODE TO PRINT REPORT                      
         BNE   ADVGX                                                            
*                                                                               
         L     R0,=A(BUFFELNQ)                                                  
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         ST    R1,ABUFFEL          A(BUFFEL BUFFER)                             
*                                                                               
         BAS   RE,INIT             INITIALIZE THE REPORT                        
         BAS   RE,PREP             PROCESS THE REPORT                           
*                                                                               
         TM    WHEN,X'20'                                                       
         BO    ADVGX                                                            
******** TM    WHEN,X'40'          IF NOW PROCESSING                            
******** BZ    ADVG60                                                           
******** XC    CONSERV,CONSERV                                                  
******** MVC   CONSERV(4),=C'$DQU'                                              
******** B     ADVGX                                                            
*                                                                               
ADVG60   GOTO1 SORTER,DMCB,=C'END'                                              
         OC    ABUFFEL,ABUFFEL                                                  
         BZ    ADVGX                                                            
         L     R0,=A(BUFFELNQ)                                                  
         L     R1,ABUFFEL                                                       
         FREEMAIN RC,A=(1),LV=(0)                                               
*                                                                               
ADVGX    B     XIT                                                              
*                                                                               
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY FOR OVERNIGHT PROCESSING                        
         SPACE                                                                  
VKOV     NTR1                                                                   
         LA    R2,SAGAGYH          R2=A(AGENCY FIELD)                           
         CLI   5(R2),0             IF INPUT AND                                 
         BE    ERRMISS                                                          
*                                                                               
         XC    PROCAGY,PROCAGY                                                  
         CLC   =C'ALL',8(R2)       IF SPECIFIC AGENCY                           
         BE    VKOV10                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SAGAGYH),SAGAGYNH                     
         MVC   PROCAGY,TGAGY       SET AGENCY CODE                              
*                                                                               
VKOV10   LA    R2,SAGUSEH          R2=A(USE FIELD)                              
         XC    TGUSCDE,TGUSCDE                                                  
         CLI   5(R2),0                                                          
         BE    VKOV20                                                           
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   INVERR                                                           
*                                                                               
VKOV20   BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE                                                                  
VALOPT   NTR1                                                                   
         LA    R2,SAGOPTH          R2=A(OPTIONS)                                
         CLI   5(R2),0                                                          
         BE    VALOPTX                                                          
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VALOPT25 CLC   =C'BOX',SCDATA1     BOX                                          
         BNE   VALOPT30                                                         
         CLI   SCDATA2,C'Y'                                                     
         BE    VALOPT40                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   INVERR                                                           
         OI    NOPTION,NOBOX       NO BOXES                                     
         B     VALOPT50                                                         
*                                                                               
VALOPT30 CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VALOPT40                                                         
         CLI   SCDATA2,C'N'                                                     
         BE    VALOPT50                                                         
         B     INVERR                                                           
VALOPT40 OI    NOPTION,NTRACE      SET TRACE ON                                 
*                                                                               
VALOPT50 LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VALOPT25         AND CONTINUE                                 
VALOPTX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO INITIALIZE                                            
         SPACE                                                                  
INIT     NTR1                                                                   
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK         SET A(HEADHOOK)                              
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS            SET A(SPECS)                                 
*                                                                               
         L     R2,TWADCONS         GET ADDRESS OF PRNTBL                        
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         MVC   NLOGOC,TLOGOC       GET A(LOGOC)                                 
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         MVC   NLOGO,MCVLOGO       GET A(LOGO)                                  
         MVI   RCSUBPRG,10                                                      
         TM    WHEN,X'20'                                                       
         BO    INITX                                                            
******** TM    WHEN,X'40'          IF NOW PROCESSING                            
******** BZ    INIT20                                                           
******** OI    NOPTION,NOBOX       CANNOT USE BOXES                             
******** MVI   RCSUBPRG,20         SET TO UNDERLINE TITLES                      
******** B     INITX                                                            
*                                                                               
INIT20   GOTO1 SORTER,DMCB,SORTCARD,RECCARD INTIALIZE SORTER                    
******** MVI   RCSUBPRG,10                                                      
*                                                                               
INITX    B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
*              PROCESS REPORT                                                   
PREP     NTR1                                                                   
         BRAS  RE,CLRMKTB          CLEAR MARKET TABLE                           
*                                                                               
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIFUSE,TGUSCDE                                                   
         LA    R1,IOHOOK           A(IOHOOK)                                    
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTORIZATION                                 
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLNXCDQ      READ HOLD RECORDS                            
         MVC   TIFAGY,PROCAGY      SET AGENCY (IF ANY)                          
         OI    TIQFLAGS,TIQFDIR    PASS DIRECTORY HOOKS                         
         TM    WHEN,X'20'          IF PROCESSING SOON                           
         BZ    PREP10                                                           
******** TM    WHEN,X'40'          IF PROCESSING NOW                            
******** BZ    PREP10                                                           
         MVC   TIQSTART(L'TGNID),TGNID SET NETWORK ID                           
         BRAS  RE,AGSYSIO                                                       
         BE    PREP20                                                           
         B     PREPX                                                            
PREP10   GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
PREP20   XC    SVSRTKEY,SVSRTKEY   CLEAR SAVED VALUES                           
         MVI   FRSTREC,C'Y'        SET FIRST RECORD                             
*                                                                               
         USING SRTRECD,R3                                                       
PREP30   BAS   RE,GETSORT          GET FROM SORT                                
         LTR   R3,R3               R3=A(SORT RECORD)                            
         BZ    PREP110                                                          
*                                                                               
         GOTO1 =A(WRTHOLD),DMCB,(RC),RR=RELO   MARK HOLD RECORD USED            
*                                                                               
         CLC   SRTAGY,SVAGY        IF NOT SAME AGENCY                           
         BE    PREP50                                                           
         CLI   FRSTREC,C'Y'                                                     
         BE    PREP40                                                           
         BAS   RE,ADDLAST          ADD LAST ADVICE RECORD                       
         BRAS  RE,PRTELOGO         PRINT END LOGO - IF NOT FIRST                
*                                                                               
PREP40   BRAS  RE,SETAGY           SET AGENCY INFO FOR LOGO PAGES               
         BRAS  RE,PRTSLOGO         PRINT START LOGO                             
         B     PREP70                                                           
*                                                                               
PREP50   CLC   SRTTCID,SVTCID      IF NOT SAME TALENT CID                       
         BNE   PREP60                                                           
         CLC   SRTVERS,SVVERS      OR VERSION                                   
         BNE   PREP60                                                           
         CLC   SRTMED,SVMED        OR SAME MEDIA                                
         BE    PREP80                                                           
PREP60   CLI   FRSTREC,C'Y'        AND NOT FIRST RECORD                         
         BE    PREP70                                                           
         BAS   RE,ADDLAST          ADD/PRINT LAST ADVICE RECORD                 
*                                                                               
PREP70   MVI   FRSTREC,C'N'        SET NOT FIRST RECORD                         
         BRAS  RE,CLRBUFF          CLEAR BUFFER OF ELEMENTS                     
         BRAS  RE,SETVALS          SET SAVED VALUES                             
         BAS   RE,SETAREC          SET ADVICE RECORD (AIO2)                     
*                                                                               
PREP80   BRAS  RE,ADDTANX          ADD TANXD ELEM FROM HOLD RECORDS             
                                                                                
         LA    RE,SRTKLNQ(R3)                                                   
PREP90   CLI   0(RE),0             IF THERE ARE NO TANPD ELEMENTS               
         BE    PREP30              IN THE SORT RECORD, GET THE NEXT ONE         
         CLI   0(RE),TANPELQ                                                    
         BE    PREP100                                                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     PREP90                                                           
*                                  BUFFER CLASS A/NETWORK DETAILS               
PREP100  GOTO1 =A(BUFTANP),DMCB,(RC),(R3),RR=RELO                               
         BRAS  RE,ADDVERS                                                       
         B     PREP30                                                           
*                                                                               
PREP110  CLI   FRSTREC,C'Y'        IF NOT FIRST RECORD                          
         BE    PREPX                                                            
         BAS   RE,ADDLAST          ADD/PRINT LAST ADVICE RECORD                 
PREPX    B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO ADD/PRINT LAST ADVICE RECORD                          
         SPACE                                                                  
ADDLAST  NTR1                                                                   
         BAS   RE,SETANP           READ CLA USAGE REC/ADD TANPD ELE'S           
                                                                                
         BRAS  RE,PROCEDE          IF NOTHING ON THE ADVICE                     
         BNE   XIT                 DO NOT ADD IT                                
                                                                                
         BAS   RE,SETWARN          READ HLD/BSS USAGE RECORD                    
         GOTO1 =A(ADDTAXC),DMCB,(RC),RR=RELO  ADD COMMENT ELEMENT               
         BRAS  RE,OK2ADD                                                        
         BNE   ALAST10                                                          
         BRAS  RE,ADDIT            ADD THE ADVICE RECORD                        
         BAS   RE,PLINE            PRINT THE RECORD INFORMATION                 
ALAST10  BRAS  RE,WHAT2ADD                                                      
         MVC   GNPAXCLA,DMCB                                                    
         BAS   RE,ADDANOT          ADD ANOTHER ADVICE?                          
         XC    LNSTATUS,LNSTATUS                                                
ALAST20  GOTOR ADDLNITE            ADD LATE NIGHT ADVICES?                      
         OC    LNSTATUS,LNSTATUS                                                
         BZ    XIT                                                              
         BAS   RE,PLINE                                                         
         GOTOR RESETCLA                                                         
         B     ALAST20                                                          
         EJECT                                                                  
*              FAKE SYSIO FOR NOW REQUESTS                                      
         SPACE 1                                                                
AGSYSIO  NTR1                                                                   
         MVI   TIMODE,PROCDIR      FIRST SET TO PROCESS KEY                     
         SPACE 1                                                                
         USING TLNXD,RE                                                         
         LA    RE,KEY                                                           
         XC    KEY,KEY             SET KEY WITH                                 
         MVC   TLNXCD,TIREAD       RECORD CODE                                  
         MVC   TLNXAGY,TIFAGY      AGENCY                                       
         MVC   TLNXNID,TIQSTART    AND TRAFFIC COMMERCIAL ID                    
******** MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLNXNCLI-TLNXD),KEYSAVE                                      
         BE    ASYSIO30            IF NO MATCH, TRY ADID                        
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLNXCD,TIREAD       RECORD CODE                                  
         MVC   TLNXAGY,TIFAGY      AGENCY                                       
         MVC   TLNXNID,TGADID      AND ADID (1ST 8 CHARS)                       
******** MVI   RDUPDATE,C'Y'                                                    
         MVI   ADIDFLG,C'Y'                                                     
         GOTO1 HIGH                                                             
         B     ASYSIO20                                                         
ASYSIO10 GOTO1 SEQ                                                              
ASYSIO20 CLC   KEY(TLNXNCLI-TLNXD),KEYSAVE                                      
         BNE   NO                                                               
         DROP  RE                                                               
         SPACE 1                                                                
         USING TLDRD,RE                                                         
ASYSIO30 LA    RE,KEY              IF A KEY IS FOUND                            
         MVC   TIKEY,KEY           SAVE KEY                                     
         MVC   TIKEYST,TLDRSTAT    KEY STATUS                                   
         MVC   TIDSKADD,TLDRDA     AND DISK ADDRESS                             
         DROP  RE                                                               
         SPACE 1                                                                
         USING TLNXD,RE                                                         
         MVC   TICID,SPACES        ALSO SAVE TRAFFIC COMMERCIAL ID              
         MVC   TICID(L'TLNXNID),TLNXNID                                         
         MVC   TICLI,TLNXNCLI      CLIENT                                       
         MVC   TIPRD,TLNXNPRD      PRODUCT                                      
         MVC   TIMED,TLNXMED       MEDIA                                        
         MVC   TIUSE,TLNXUSE       AND USE                                      
         DROP  RE                                                               
         SPACE 1                                                                
******** MVI   RDUPDATE,C'Y'                                                    
         LA    R1,IO                                                            
         ST    R1,TIAREC                                                        
         MVC   AIO,TIAREC                                                       
         GOTO1 GETREC              GET HOLD RECORD INTO TIAREC                  
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         USING TANXD,R4                                                         
         CLI   ADIDFLG,C'Y'        IF TRAFFIC COMMERCIAL ID IS SAVED            
         BNE   ASYSIO40            AS AD-ID                                     
         L     R4,TIAREC                                                        
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ASYSIO40                                                         
         CLI   TANXLEN,TANXLN2Q                                                 
         BL    ASYSIO40                                                         
         MVC   TICID,TANXADID      SAVE TRAFFIC COMMERCIAL ID                   
         DROP  R4                                                               
         SPACE 1                                                                
ASYSIO40 BAS   RE,IOHOOK           GO SEE IF KEY MATCHES ALL                    
         BNE   ASYSIO10            CRITERIA                                     
         SPACE 1                                                                
         MVI   TIMODE,PROCREC      IF IT DOES, SET TO PROCESS RECORD            
         SPACE 1                                                                
         BAS   RE,IOHOOK           AND GO TO IO HOOK                            
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO PUT HOLD INFO IN TABLE, MARK RECORDS USED             
         SPACE                                                                  
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS DIRECTORY                            
         BNE   IOHK30                                                           
         TM    NOPTION,NTRACE      IF TRACING                                   
         BZ    IOHK10                                                           
         GOTO1 HEXOUT,DMCB,TIKEY,P,32,=C'TOG'                                   
         BAS   RE,PRNTIT                                                        
IOHK10   GOTO1 =A(FILTER),DMCB,(RC),RR=RELO   FILTER THE KEY                    
         BE    YES                                                              
         B     NO                                                               
*                                                                               
IOHK30   CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   IOHKX                                                            
*                                                                               
         USING TLNXD,R6                                                         
         L     R6,TIAREC           R6=A(HOLD RECORD)                            
         ST    R6,AIO                                                           
         BAS   RE,SETSORT          SET SORT RECORD                              
         MVC   AIO,AIO1            RESET THE IO AREA                            
*                                                                               
         XC    KEY,KEY             FIX READ SEQUENCE FOR SYSIO                  
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
IOHKX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CREATE AND ADD SORT RECORD                            
         USING TLNXD,R6            R6=A(HOLD RECORD)                            
         SPACE                                                                  
SETSORT  NTR1                                                                   
******** MVI   NOWERR,C'N'                                                      
         XC    ANXTEL,ANXTEL       CLEAR A(NEXT ELEMENT IN SRTREC)              
         BRAS  RE,CLRSREC          CLEAR SORT RECORD                            
         L     R3,ASRTREC          R3=A(SORT RECORD)                            
         USING SRTRECD,R3                                                       
*                                                                               
         MVC   SRTAGY,TLNXAGY      SET TALENT AGENCY                            
         MVC   SRTMED,TLNXMED      SET MEDIA                                    
         MVC   SRTUSE,TLNXUSE      SET USE                                      
         MVC   SRTHKEY,TLNXKEY     SAVE KEY                                     
         BAS   RE,SETSINFO         SET TALENT INFO FOR SORT                     
         BNE   SETSRTX             BAD INT COMM # - SKIP HOLD RECORD            
         MVC   SVSRTKEY,SRTKEY     SAVE FOR CONT'D REC (IF NEC)                 
*                                                                               
         TM    WHEN,X'20'          IF PROCESSING SOON                           
         BZ    SETSRT10                                                         
******** TM    WHEN,X'40'          IF PROCESSING NOW                            
******** BZ    SETSRT10                                                         
         XC    KEY,KEY             HAVE TO RESET THE HOLD RECORD                
         MVC   KEY(L'TIKEY),TIKEY  INTO TIAREC                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),TIKEY                                               
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
*                                                                               
SETSRT10 MVC   SRTHKEY,TLNXKEY     SAVE KEY OF HOLD RECORD                      
         MVC   TIKEY,TLNXKEY                                                    
         MVC   SVXTYPE,SRTXTYPE                                                 
         LA    R3,SRTKLNQ(R3)      R3=A(DATA SORTREC AREA)                      
         BRAS  RE,SETSRTNX         SET TANXD ELEMENT IN SORT                    
*                                                                               
SETSRT20 BAS   RE,SETSRTNP         SET TANPD ELEMENTS IN SORT                   
*                                                                               
         ST    R3,ANXTEL           SAVE A(NEXT TANPD ELEMENT)                   
         MVI   0(R3),0             SET END OF RECORD                            
         LA    R3,1(R3)                                                         
         L     RE,ASRTREC                                                       
         SR    R3,RE                                                            
         STH   R3,0(RE)            SET SORT RECORD LENGTH                       
*                                                                               
         BRAS  RE,PUTSORT          PUT RECORD TO SORT                           
*                                                                               
SETSRT30 CLI   TLNXSEQ,0           IF BASE RECORD                               
         BNE   SETSRT40                                                         
*                                                                               
         XC    KEY,KEY             RE-READ SYSIO'S KEY                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    SETSRT40                                                         
         DC    H'0'                MUST HAVE IT                                 
*                                                                               
SETSRT40 GOTO1 SEQ                                                              
*                                                                               
SETSRT50 CLC   KEY(TLNXSEQ-TLNXKEY),KEYSAVE                                     
         BNE   SETSRTX                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
******** TM    WHEN,X'40'          IF PROCESSING NOW                            
******** BZ    SETSRT60                                                         
*                                                                               
******** L     R3,ANXTEL           R3=A(NEXT TANPD ELEMENT)                     
******** L     R1,ASRTREC                                                       
******** AHI   R1,MAXSLNQ2                                                      
******** CR    R3,R1               TEST PAST END OF SORT AREA                   
******** BL    SETSRT20                                                         
******** BRAS  RE,CLRSREC                                                       
******** MVI   NOWERR,C'Y'         HOLD REC TO LARGE FOR NOW PROCESSING         
******** B     SETSRTX                                                          
*                                                                               
SETSRT60 BRAS  RE,CLRSREC          CLEAR SORT RECORD                            
         L     R3,ASRTREC          R3=A(SORT RECORD)                            
         MVC   SRTKEY(L'SVSRTKEY),SVSRTKEY                                      
         B     SETSRT10                                                         
*                                                                               
SETSRTX  B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE                                                                  
*              ROUTINE TO SET SORT CID, CLIENT,AND PRODUCT                      
*              IF TALENT CID NOT FOUND - CC NEQ                                 
         SPACE                                                                  
         USING SRTRECD,R3                                                       
SETSINFO NTR1                                                                   
         MVC   TGAGY,SRTAGY                                                     
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4                                                         
         MVC   SRTVERS,TANXVERS                                                 
         MVC   SRTXTYPE,TANXTYPE                                                
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'AC',TANXCOM),0                           
         BE    SINF10                                                           
         OI    STATUS,WHUNMAT                                                   
         GOTO1 =A(WRTHOLD),DMCB,(RC),RR=RELO                                    
         NI    STATUS,X'FF'-WHUNMAT                                             
         B     NO                                                               
*                                                                               
SINF10   L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   SRTCLI,TLCOCLI      SET TALENT CLIENT CODE                       
         MVC   SRTPRD,TLCOPRD      SET TALENT PRODUCT CODE                      
         MVC   SRTTTL,TGNAME       SET TALENT COMMERCIAL TITLE                  
*                                                                               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   SRTTCID,TACOCID     SET TALENT CID                               
         DROP  R4                                                               
*                                                                               
         USING TAVRD,R4                                                         
         CLI   SRTXTYPE,TANXTLFT   IF MATCHED BY LIFT                           
         BE    *+12                                                             
         CLI   SRTXTYPE,TANXTALF   OR ALIAS AS A LIFT                           
         BNE   SINF15                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      BUT COMMERCIAL HAS A VERSION 2               
         BRAS  RE,GETEL            ELEMENT                                      
         BNE   SINF15                                                           
         BRAS  RE,NEXTEL                                                        
         BNE   SINF15                                                           
         CLI   TAVRVERS,2                                                       
         BNE   SINF15                                                           
         MVI   SRTVERS,2           FIX THE SORT INFORMATION                     
         MVI   SRTXTYPE,TANXTVER                                                
         DROP  R4                                                               
                                                                                
SINF15   GOTO1 RECVAL,DMCB,TLCOICDQ,(X'AC',SRTTCID),0                           
         BE    SINF20                                                           
         OI    STATUS,WHUNMAT                                                   
         GOTO1 =A(WRTHOLD),DMCB,(RC),RR=RELO                                    
         NI    STATUS,X'FF'-WHUNMAT                                             
         B     NO                                                               
*                                                                               
SINF20   BRAS  RE,ADJUSE           ADJUST CAN,SPA AND ADDEN USES                
*                                                                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A8',SRTCLI),0                             
         MVC   SRTCLIN,TGNAME                                                   
*                                                                               
         MVC   SRTPRDN,SPACES                                                   
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A8',SRTPRD),0                             
         BNE   *+10                                                             
         MVC   SRTPRDN,TGNAME                                                   
         CR    RB,RB                                                            
*                                                                               
SETSINFX MVC   AIO,TIAREC          CC SET                                       
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO SET TANPD ELEMENTS IN SORTREC                         
*                                  XIT - (R3) PTS TO NEXT POS IN SRTREC         
         USING TLNXD,R6            R6=A(HOLD RECORD)                            
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
         SPACE                                                                  
SETSRTNP NTR1                                                                   
         USING TANXD,R4                                                         
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SETSNP10                                                         
         MVC   TGCOM,TANXCOM       SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   TGVER,TANXVERS      AND VERSION CODE                             
         DROP  R4                                                               
         SPACE 1                                                                
         USING TANPD,R4                                                         
SETSNP10 LR    R4,R6                                                            
         MVI   ELCODE,TANPELQ      GET USES                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SETSNP20 BAS   RE,NEXTEL                                                        
         BNE   SETSNPX                                                          
         SPACE 1                                                                
         BRAS  RE,SPTANPEL        SPECIAL PROCESSING FOR CNET/MKT/CSYS          
         BNE   SETSNP20           (ELIM ALREADY PAID/SHRINK EXISTING)           
         SPACE 1                                                                
         ZIC   R1,SORTLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TANPD       MOVE TO SORT RECORD                          
         LA    R1,1(R1)            RESTORE LENGTH                               
         STC   R1,1(R3)                                                         
         AR    R3,R1               BUMP TO NEXT POSITION IN SORT RECORD         
         B     SETSNP20                                                         
SETSNPX  XIT1  REGS=(R3)           RETURNS R3 SET TO NEXT POS IN SRTREC         
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*              ROUTINE TO SET ADVICE RECORD (AIO2)                              
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
SETAREC  NTR1                                                                   
         BRAS  RE,CHKCAST          CHECK TO SEE IF WE ARE ADDING PAX            
         MVC   GNPAXCLA,DMCB                         AND CLA ADVICES            
*                                                                               
         MVC   AIO,AIO2            SET AIO OF ADVICE RECORD                     
         XC    CNTTANP,CNTTANP     COUNT OF TANPD ELEMENTS                      
         MVI   SVBMNTH,0           RESET BINARY MONTH                           
         MVI   SEQCNT,1            SET START SEQ NUM FOR TANPD ELES             
         MVC   TGAGY,SRTAGY        SET GLOBAS AGENCY                            
         GOTO1 USEVAL,DMCB,SRTUSE                                               
*                                                                               
         XC    KEY,KEY             SET KEY OF ADVICE                            
         LA    R4,KEY                                                           
         USING TLDVD,R4                                                         
         MVI   TLDVCD,TLDVCDQ      SET RECORD CODE                              
         MVC   TLDVAGY,TGAGY       SET AGENCY                                   
         MVC   TLDVCID,SRTTCID     SET TALENT COMMERCIAL ID                     
         MVC   TGCLI,SRTCLI        AND SAVE CLIENT CODE                         
*                                                                               
SETAREC5 BRAS  RE,GETADV           GET NEXT ADVICE CODE                         
         MVC   TLDVADV,TGADV                                                    
         BRAS  RE,CHKDUP           CHECK ADVICE ALREADY ON FILE                 
         BE    SETAREC5            GET NEXT ADVICE NUMBER                       
*                                                                               
         L     R4,AIO              R4=A(NEW RECORD)                             
         MVC   TLDVKEY,KEY         SET KEY IN IOAREA                            
         MVC   TLDVLEN,DATADISP    SET START LENGTH                             
         XC    TLDVSTAT(10),TLDVSTAT                                            
*                                                                               
         BAS   RE,GETCOMM          ADD ELEMENTS FROM COMMERCIAL REC             
         BRAS  RE,ADDTADV          ADD ADVICE DETAILS ELEMENT                   
         BRAS  RE,ADDTAVU          ADD ADVICE USE DETAILS ELEMENT               
         BRAS  RE,ADDTACM          ADD TACM ELEMENT                             
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO ADD ELEMENTS FROM COMM'L REC TO ADV REC               
GETCOMM  NTR1                                                                   
         BRAS  RE,RDCOM            READ COMMERCIAL RECORD INTO AIO1             
         BRAS  RE,CPYCTTL          COPY TO ADVICE COMM'L TITLE NAME             
         BRAS  RE,CPYCPRD          COPY TO ADVICE COMM'L PROD NAME              
         BRAS  RE,SVCOMED          SAVE SOME TACOD INFORMATION                  
                                                                                
         GOTOR TRN2TACC,DMCB,AIO1,AIO2                                          
*                                                                               
         LA    R2,COMELTAB         R2=A(ELEMENTS TO ADD FROM COMML)             
GETC20   CLI   0(R2),X'FF'         TEST END OF TABLE                            
         BE    GETC30                                                           
         L     R4,AIO1             R4=A(COMMERCIAL RECORD)                      
         MVC   ELCODE,0(R2)        SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         BNE   GETC25                                                           
         MVC   ELEMENT,0(R4)                                                    
         BRAS  RE,ELEMADD          ADD ELEMENT TO ADVICE                        
GETC25   LA    R2,1(R2)            BUMP TO NEXT ELEMENT CODE IN TABLE           
         B     GETC20                                                           
*                                                                               
GETC30   MVC   AIO,AIO1            SET COMMERCIAL IOAREA                        
         GOTO1 EXTRACT             EXTRACT GOBASS                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'28',0),0                                  
         MVC   AIO,AIO2                                                         
         BNE   GETC40                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TAFND,R3                                                         
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TAFNNAME                                       
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TGNAME),TGNAME                                        
         BRAS  RE,ELEMADD          ADD CLIENT NAME TO ADVICE                    
*                                                                               
GETC40   GOTO1 DELL,DMCB,(1,=AL1(TAFNTPRD))                                     
*                                                                               
         OC    TGPRD,TGPRD         IF PRODUCT DEFINED                           
         BZ    GETCOMMX                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'28',0),0                                  
         MVC   AIO,AIO2                                                         
         BNE   GETCOMMX                                                         
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TAFND,R3                                                         
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TAFNNAME                                       
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TGNAME),TGNAME                                        
         BRAS  RE,ELEMADD          ADD PRODUCT NAME TO ADVICE                   
GETCOMMX B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              SORT BUFFEL , READ CLA USAGE, ADD TANPD ELEMENTS                 
         SPACE                                                                  
SETANP   NTR1                                                                   
         USING BUFFD,R2                                                         
         L     R2,ABUFFEL          R2=A(BUFFER OF TANPD ELEMENT)                
         SPACE 1                                                                
         LH    R3,BUFFCNT          SORT BY USE DATE                             
         GOTO1 XSORT,DMCB,BUFFD,(R3),BUFFLNQ,L'BUFFDATE,0                       
         SPACE 1                                                                
STANP10  MVI   USAGE,C'N'          SET NO USAGE RECORD FOUND                    
         MVI   SVBMNTH,0           RESET BINARY MONTH                           
         MVI   NEWBDT,C'N'         SAME BUFFER DATE                             
         XC    SVUHKEY,SVUHKEY                                                  
         XC    USENUM,USENUM                                                    
         XC    STRTDATE,STRTDATE                                                
         XC    ENDDATE,ENDDATE                                                  
         SPACE 1                                                                
         MVC   SVUSEQU,TGUSEQU     SAVE USE EQUATE                              
         MVC   SVUSCDE,TGUSCDE     AND USE NAME                                 
         SPACE 1                                                                
         CLI   SVUSEQU,ULCB        IF LOCAL CABLE USE                           
         BNE   STANP15             WE'RE GOING TO WANT TO SEARCH                
         MVC   TGUSCDE,=C'CBL'     FOR CABLE FIRST                              
         SPACE 1                                                                
STANP15  BRAS  RE,RDUSEDTS         FIRST SEE IF CYCLE IN ELEMENT                
         SPACE 1                                                                
         BRAS  RE,RDHUH            THEN READ HIGH FOR UH RECORD                 
         B     *+8                                                              
STANP20  BRAS  RE,RDSUH            READ NEXT                                    
         SPACE 1                                                                
         CLI   SVUSEQU,ULCB        IF LOCAL CABLE USE                           
         BNE   STANP23             WE'RE GOING TO WANT TO SEARCH                
         MVI   TGUSEQU,UCBL        FOR CABLE FIRST                              
         SPACE 1                                                                
STANP23  BRAS  RE,RDADV            MIGHT BE AN ADVICE TO MERGE                  
         SPACE 1                                                                
         CLI   ADDORCHG,C'A'       IF NO ADVICE TO MERGE FOUND                  
         BNE   STANP25                                                          
         CLI   BUFFTYP,TANPNET     AND TYPE IS NET CABLE                        
         BNE   STANP24                                                          
         MVI   TGUSEQU,ULCB        MIGHT BE AN LCB ADVICE TO MERGE              
         BRAS  RE,RDADV                                                         
         CLI   ADDORCHG,C'C'       IF THERE IS                                  
         BNE   STANP25                                                          
         USING TAVUD,R4                                                         
         L     R4,AIO2             CHANGE USE TYPE FROM LCB TO                  
         MVI   ELCODE,TAVUELQ      CBL OR SCB                                   
         BRAS  RE,GETEL                                                         
         BNE   STANP25                                                          
         MVC   TAVUUSE,SVUSEQU                                                  
         MVI   TAVUTYPE,0                                                       
         SPACE 1                                                                
STANP24  CLI   BUFFTYP,TANPCSYS    IF TYPE IS LOCAL CABLE                       
         BNE   STANP25                                                          
         MVI   TGUSEQU,USCB        MIGHT BE AN SCB ADVICE TO MERGE              
         BRAS  RE,RDADV                                                         
         CLI   ADDORCHG,C'C'       IF THERE IS NO SCB ADVICE                    
         BE    STANP25                                                          
         MVI   TGUSEQU,ULCB        MIGHT BE AN LCB ADVICE TO MERGE              
         BRAS  RE,RDADV                                                         
         SPACE 1                                                                
STANP25  MVC   TGUSEQU,SVUSEQU                                                  
         SPACE 1                                                                
         LH    R3,USENUM           R3=CURRENT USE NUMBER                        
         SPACE 1                                                                
         CLI   USAGE,C'Y'          IF NO USAGE RECORD FOUND                     
         BE    STANP40                                                          
         CLI   ADDORCHG,C'C'       AND NOT MERGING AN ADVICE                    
         BE    STANP40                                                          
         BAS   RE,LTNIGHT                                                       
         BNE   STANP30                                                          
         LA    R2,BUFFLNQ(R2)                                                   
         OC    BUFFD(BUFFLNQ),BUFFD                                             
         BNZ   STANP10                                                          
         B     XIT                                                              
STANP30  BRAS  RE,STNEWRNG         SET NEW CYCLE DATE RANGE                     
         SPACE 1                                                                
STANP40  OC    BUFFD(BUFFLNQ),BUFFD TEST END OF BUFFER                          
         BZ    STANP110                                                         
         SPACE 1                                                                
         BAS   RE,LTNIGHT          HANDLE LATE NIGHT USES                       
         BE    STANP80                                                          
         SPACE 1                                                                
         CLI   BUFFTYP,TANPNET     IF NOT A CABLE USE                           
         BE    STANP70                                                          
         CLI   BUFFTYP,TANPSPT     OR WILDSPOT USE                              
         BE    STANP70                                                          
         CLI   BUFFTYP,TANPCSYS    OR LOCAL CABLE USE                           
         BE    STANP70                                                          
         CLI   SVBMNTH,0           AND THERE'S A CHANGE IN MONTH OF USE         
         BE    STANP50                                                          
         CLC   SVBMNTH,BUFFDATE+1                                               
         BNE   STANP60                                                          
STANP50  LH    RE,CNTTANP          OR IF REACHED MAX USES ON A RECORD           
         CH    RE,=AL2(MAXTANP)                                                 
         BL    STANP70                                                          
STANP60  OI    STATUS,SMAXTANP                                                  
         BAS   RE,FINAREC          FINISH UP ADDING ADVICE REC/PRINT            
         BAS   RE,SETNREC          SET NEW ADVICE RECORD                        
         NI    STATUS,X'FF'-SMAXTANP                                            
         SPACE 1                                                                
STANP70  MVC   SVBMNTH,BUFFDATE+1  SAVE BUFFER MONTH                            
         SPACE 1                                                                
         BAS   RE,CKUHRNG          IF BUFFER DATE IN THE CURRENT CYCLE          
         BNE   STANP90                                                          
         LA    R3,1(R3)            INCREMENT USE NUMBER                         
         BRAS  RE,ADDMAJS          IF USE IS FOR MAJOR, ADD IT                  
         BAS   RE,ADDTANP          ELSE, ADD TANP ELEMENT TO ADVICE             
         MVI   NEWBDT,C'Y'         NEW BUFFER DATE                              
STANP80  LA    R2,BUFFLNQ(R2)      BUMP TO NEXT ELEMENT IN BUFFER               
         B     STANP40                                                          
         SPACE 1                                                                
STANP90  OC    CNTTANP,CNTTANP     IF TANPD ELEMENTS ADDED                      
         BZ    STANP100                                                         
         BAS   RE,FINAREC          FINISH UP ADDING ADVICE REC/PRINT            
         BAS   RE,SETNREC          SET NEW ADVICE RECORD                        
         SPACE 1                                                                
STANP100 CLI   NEWBDT,C'Y'         IF NEW BUFFER DATE                           
         BE    STANP10             READ HIGH FOR UHCYCLE FOR NEW DATE           
         B     STANP20             ELSE, READ SEQUENTIAL                        
         SPACE 1                                                                
STANP110 BRAS  RE,ADDCYC           ADD CYCLE DATES TO RECORD                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS BUFFER DATE WITHIN CURRENT USAGE CYCLE            
         SPACE                                                                  
CKUHRNG  NTR1                                                                   
         CLC   BUFFDATE,STRTDATE   IF DATE WITHIN USAGE CYCLE                   
         BL    NO                                                               
         CLC   BUFFDATE,ENDDATE                                                 
         BH    NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ADD REMAINING ELEMENTS TO ADVICE RECORD & PRINT IT               
         SPACE                                                                  
FINAREC  NTR1                                                                   
         BRAS  RE,PROCEDE          IF NOTHING ON THE ADVICE                     
         BNE   XIT                 DO NOT ADD IT                                
                                                                                
         BRAS  RE,WHAT2ADD                                                      
         MVC   GNPAXCLA,DMCB                                                    
         SPACE 1                                                                
         BRAS  RE,ADDCYC           ADD CYCLE DATES TO RECORD                    
         BAS   RE,SETWARN          READ HLD/BSS USAGE RECORD                    
         GOTO1 =A(ADDTAXC),DMCB,(RC),RR=RELO   ADD COMMENT ELEMENT              
         BRAS  RE,OK2ADD                                                        
         BNE   FREC10                                                           
         BRAS  RE,ADDIT                        ADD ADVICE RECORD                
         BAS   RE,PLINE                        PRINT ADVICE INFO                
FREC10   BAS   RE,ADDANOT                                                       
         XC    LNSTATUS,LNSTATUS                                                
FREC20   GOTOR ADDLNITE            ADD LATE NIGHT ADVICES?                      
         OC    LNSTATUS,LNSTATUS                                                
         BZ    XIT                                                              
         BAS   RE,PLINE                                                         
         GOTOR RESETCLA                                                         
         B     FREC20                                                           
         SPACE 2                                                                
*              ROUTINE TO SET UP NEXT ADVICE RECORD                             
         SPACE                                                                  
SETNREC  NTR1                                                                   
         MVI   ELCODE,TANPELQ      DELETE CLASS A/NETWORK                       
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAMTELQ      DELETE CNET/MKT/CSYS                         
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAUPELQ      AND DELETE UPGRADE ELEMENT                   
         GOTO1 REMELEM             FROM PREVIOUS ADVICE                         
         SPACE 1                                                                
         XC    CNTTANP,CNTTANP     COUNT OF TANPD ELEMENTS                      
         MVI   SVBMNTH,0           RESET BINARY MONTH                           
         MVI   SEQCNT,1            RESET SEQ NUM FOR TANPD ELEMENTS             
         SPACE 1                                                                
         MVI   ELCODE,TACMELQ      DELETE OLD COMMENTS                          
         GOTO1 DELL,DMCB,(1,=AL1(TACMTYPG))                                     
         MVI   ELCODE,TAXCELQ                                                   
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         USING TLDVD,R4                                                         
SETNREC5 BRAS  RE,GETADV           GET NEXT ADVICE CODE                         
         MVC   TLDVADV,TGADV       SET NEW ADVICE IN RECORD                     
         XC    KEY,KEY                            AND IN KEY                    
         MVC   KEY(L'TLDVADV),TLDVADV                                           
         BRAS  RE,CHKDUP           CHECK ADVICE ALREADY ON FILE                 
         BE    SETNREC5            GET NEXT ADVICE NUMBER                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO ADD TANPD ELEMENT TO ADVICE RECORD                    
*                                  R3=A(USE NUMBER)                             
         USING BUFFD,R2            R2=A(BUFFER OF TANPD ELEMENTS)               
         SPACE                                                                  
ADDTANP  NTR1                                                                   
         LA    R4,ELEMENT          INITIALIZE ELEMENT                           
         XC    ELEMENT,ELEMENT                                                  
         SPACE 1                                                                
         USING TANPD,R4                                                         
         CLI   BUFFTYP,0           IF NETWORK USE                               
         BNE   ADTANP10                                                         
         MVI   TANPEL,TANPELQ      SET ELEMENT CODE                             
         MVI   TANPLEN,TANPLNQ3    SET ELEMENT LENGTH                           
         MVC   TANPSEQ,SEQCNT      SEQUENCE NUMBER                              
         MVC   TANPDATE,BUFFDATE   USE DATE                                     
         MVC   TANPPNME,BUFFPNME   PROGRAM NAME                                 
         MVC   TANPNWK,BUFFNWK     NETWORK                                      
         MVC   TANPLFT,BUFFLFT     LIFT                                         
         MVC   TANPFEED,BUFFFEED   FEED CODE                                    
         SPACE 1                                                                
         CLI   TANPNWK,C'B'        IF NETWORK IS BOUNCE                         
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'E'        IF NETWORK IS ME TV                          
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'Y'        IF NETWORK IS ANTENNA                        
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'Z'        IF NETWORK IS COZI TV                        
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'P'        IF NETWORK IS ESCAPE                         
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'G'        IF NETWORK IS GET TV                         
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'R'        IF NETWORK IS GRIT                           
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'J'        IF NETWORK IS JUSTICE                        
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'L'        IF NETWORK IS LAFF                           
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'T'        IF NETWORK IS THIS TV                        
         BE    ADTANP05                                                         
         CLI   TANPNWK,C'X'        IF NETWORK IS PAX                            
         BNE   ADTANP50                                                         
ADTANP05 MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPD))                                     
         BNE   ADTANP50                                                         
         L     RE,TGELEM                                                        
         USING TACMD,RE                                                         
         CLI   TACMCOMM,C'X'       AND CAST IS ALL 00 OR LATER                  
         BNE   ADTANP50                                                         
         SHI   R3,1                                                             
         B     ADTANP70            DO NOT GIVE PROGRAM A USE NUMBER             
         DROP  R4,RE                                                            
         SPACE 1                                                                
         USING TAMTD,R4                                                         
ADTANP10 MVI   TAMTEL,TAMTELQ      FOR CABLE,WILDSPOT AND LOCAL CABLE           
         MVI   TAMTLEN,TAMTLNQ     BUILD TAMT ELEMENT                           
         MVC   TAMTCODE,BUFFCSYS                                                
         MVC   TAMTCYCS,BUFFDATE                                                
         XR    R3,R3                                                            
         SPACE 1                                                                
         L     R4,AIO              IF ELEMENT ALREADY EXISTS ON THIS            
         MVI   ELCODE,TAMTELQ      ADVICE FOR THIS CNET/MKT/CSYS,               
         BAS   RE,GETEL            DO NOT ADD IT AGAIN                          
         B     *+8                                                              
ADTANP20 BAS   RE,NEXTEL                                                        
         BNE   ADTANP30                                                         
         CLC   TAMTCODE,BUFFMKT                                                 
         BNE   ADTANP20                                                         
         LH    R1,CNTTANP                                                       
         LA    R1,1(R1)                                                         
         STH   R1,CNTTANP                                                       
         B     ADTANPX                                                          
         SPACE 1                                                                
ADTANP30 BRAS  RE,ONDIFADV         IF ELEMENT ALREADY EXISTS ON ANOTHER         
         BNE   ADTANP40            ADVICE FOR THIS CNET/MKT/CSYS,               
         LH    R1,CNTTANP          DO NOT ADD IT AGAIN                          
         LA    R1,1(R1)                                                         
         STH   R1,CNTTANP                                                       
         B     ADTANPX                                                          
         SPACE 1                                                                
ADTANP40 OC    BUFFMKTN,BUFFMKTN   IS THERE AN UNMATCHED MARKET?                
         BZ    *+8                                                              
         BRAS  RE,ADDMKT           ADD MKT TO MARKET TABLE                      
         SPACE 1                                                                
         BRAS  RE,STLAGEND                                                      
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLDVD,R4                                                         
ADTANP50 L     R4,AIO             IF ADDING THIS ELEMENT WILL PUSH              
         LHI   RE,1775            RECORD OVER THE MAXIMUM LENGTH                
         CH    RE,TLDVLEN                                                       
         BH    ADTANP60                                                         
         MVC   SVELEM,ELEMENT                                                   
         OI    STATUS,SMAXTANP                                                  
         BAS   RE,FINAREC         FINISH UP ADDING ADVICE REC/PRINT             
         BAS   RE,SETNREC         AND SET NEW ADVICE RECORD                     
         BRAS  RE,RDADV                                                         
         NI    STATUS,X'FF'-SMAXTANP                                            
         MVC   ELEMENT,SVELEM                                                   
         DROP  R4                                                               
         SPACE 1                                                                
ADTANP60 ZIC   R1,SEQCNT           INCREMENT SEQUENCE NUMBER                    
         LA    R1,1(R1)                                                         
         STC   R1,SEQCNT                                                        
         SPACE 1                                                                
         LH    R1,CNTTANP          INCREMENT TANPD ELEMENT COUNT                
         LA    R1,1(R1)                                                         
         STH   R1,CNTTANP                                                       
         SPACE 1                                                                
         USING TANPD,R4                                                         
         LA    R4,ELEMENT                                                       
         CLI   BUFFTYP,0           IF NETWORK USE                               
         BNE   ADTANP70                                                         
         STCM  R3,3,TANPUSEN       SET USE NUMBER                               
         DROP  R4                                                               
         SPACE 1                                                                
ADTANP70 STH   R3,TOTCNT                                                        
         BRAS  RE,ELEMADD                                                       
ADTANPX  XIT1  REGS=(R3)                                                        
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK USAGE HISTORY RECORDS AND                       
*              SET WRNMSG                                                       
         SPACE                                                                  
SETWARN  NTR1                                                                   
         GOTO1 CTYPVAL,DMCB,STACOTYP  VALIDATE COMMERCIAL TYPE                  
*                                                                               
         MVC   PTYPE(3),=C'HLD'                                                 
*        MVC   PTYPE+3(3),=C'BSS'                                               
*                                                                               
         CLI   TGCTEQU,CTYSPAN                                                  
         BNE   SETWRN5                                                          
         MVC   PTYPE(3),=C'SHL'      SPANISH COMMERCIALS                        
*        MVC   PTYPE+3(3),=C'SSS'                                               
         B     SETWRN10                                                         
*                                                                               
SETWRN5  CLI   TGCTEQU,CTYADD                                                   
         BNE   SETWRN10                                                         
         MVC   PTYPE(3),=C'ADH'      ADDENDUM COMMERCIALS                       
*        MVC   PTYPE+3(3),=C'ADT'                                               
*                                                                               
SETWRN10 MVC   AIO,AIO1            SET AIO FOR READING USAGE RECORDS            
         BAS   RE,CHKHLD           CHECK FOR HLD PYMTS (HLD/SHL/ADL)            
         CLI   WRNMSG,C'N'         IF HLD PAYMENT FOUND                         
         BE    *+8                 DONE                                         
         BRAS  RE,CHKSES           CHECK FOR SESSION PYMTS(BSS/SSS/ADT)         
*                                                                               
         MVC   AIO,AIO2            MAKE SURE AIO IS RESET TO ADVICE REC         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK FOR HLD/SHL/ADL USAGE RECORD WHOSE              
*              PERIOD IS WITHIN THE CYCLE START                                 
         SPACE                                                                  
CHKHLD   NTR1                                                                   
         MVI   WRNMSG,C'Y'         NO USAGE RECORD FOUND                        
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHUSE,PTYPE       SET USE HLD TYPE                             
         GOTO1 HIGH                                                             
         B     CHKHLD20                                                         
*                                                                               
CHKHLD10 GOTO1 SEQ                                                              
CHKHLD20 CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BNE   CHKHLDX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         MVI   WRNMSG,C'H'         HLD RECS EXIST (BUT NOT FOR PERIOD)          
         BRAS  RE,CHKDATE          CHECK CYCLE START WITHIN USAGE DATES         
         CLI   WRNMSG,C'N'                                                      
         BL    CHKHLD10                                                         
*                                                                               
CHKHLDX  MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*            ROUTINE TO ADD ANOTHER ADVICE                                      
         SPACE                                                                  
ADDANOT  NTR1                                                                   
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL            R4=A(ADVICE DETAILS ELEMENT)                 
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAVUD,R4                                                         
         BRAS  RE,CBLTEST                                                       
         BE    XIT                                                              
         BRAS  RE,WSPTEST                                                       
         BE    XIT                                                              
         BRAS  RE,LCBTEST                                                       
         BE    XIT                                                              
         MVC   SVCYC,TAVUCYC       SAVE CYCLE DATES FROM ADVICE                 
         MVC   SVDTS,STRTDATE      AND START/END DATE FROM W/S                  
         DROP  R4                                                               
         SPACE 1                                                                
         TM    GNPAXCLA,GNPAXSTA   IF MIXED OR ALL 00 CAST AND PAX              
         BNO   ADANOT10            PROGRAMS ON ADVICE, ADJUST CLA               
         MVI   SPLITADV,C'N'                                                    
ADANOT03 BRAS  RE,CLA2PAX          ADVICE TO PAX                                
         BNE   ADANOT10                                                         
         BRAS  RE,ADDIT            ADD IT                                       
         MVC   TGUSCDE,=C'PAX'                                                  
         BAS   RE,PLINE            AND PRINT IT                                 
         SPACE 1                                                                
         L     R4,AIO              IF STILL MORE PAX PROGRAMS                   
         MVI   ELCODE,X'BB'        ON ADVICE                                    
         BRAS  RE,GETEL            GO ADD ANOTHER ADVICE                        
*****    BE    ADANOT03                                                         
         BNE   ADANOT04                                                         
         MVI   SPLITADV,C'Y'                                                    
         B     ADANOT03                                                         
         SPACE 1                                                                
ADANOT04 MVC   TGUSCDE,=C'CLA'                                                  
         SPACE 1                                                                
         MVI   ELCODE,TANPELQ      NOW DELETE ALL NETWORK ELEMENTS              
         GOTO1 REMELEM             FROM ADVICE                                  
         SPACE 1                                                                
         SR    R3,R3                                                            
         L     R4,AIO2                                                          
         MVI   ELCODE,X'AA'        CHANGE ALL 'AA' ELEMENTS BACK                
         BAS   RE,GETEL            INTO NETWORK ELEMENTS                        
         B     *+8                                                              
ADANOT05 BAS   RE,NEXTEL                                                        
         BNE   ADANOT06                                                         
         USING TANPD,R4                                                         
         MVI   TANPEL,TANPELQ                                                   
         DROP  R4                                                               
         AHI   R3,1                ADD 1 TO CHANGED COUNTER                     
         B     ADANOT05                                                         
ADANOT06 CHI   R3,0                AND GO ADD ANOTHER ADVICE                    
         BNE   ADANOT03                                                         
         SPACE 1                                                                
ADANOT10 L     R4,AIO2             RESET THE USE CODE AND CYCLE DATES           
         MVI   ELCODE,TAVUELQ      IN BOTH THE ADVICE AND W/S                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAVUD,R4                                                         
         MVI   TAVUUSE,UCLA                                                     
         MVC   TAVUCYC,SVCYC                                                    
         MVC   STRTDATE(6),SVDTS                                                
         DROP  R4                                                               
         SPACE 1                                                                
         NI    GNPAXCLA,X'FF'-GNPAXSTA-GNCLASTA                                 
         B     XIT                                                              
         EJECT                                                                  
*            ROUTINE TO PRINT NEW RECORD INFORMATION                            
         SPACE                                                                  
PLINE    NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE A NEW PAGE FOR EACH ADVICE             
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         L     R6,AIO2             R4=A(ADVICE RECORD ADDED)                    
         USING TLDVD,R6                                                         
*                                                                               
         MVC   PRTCID(12),TLDVCID  TALENT COMMERCIAL ID                         
         BRAS  RE,PMAINLEN         POSSIBLY PRINT MAIN LENGTH                   
*                                                                               
         MVC   PRTADV,TLDVADV      ADVICE CODE                                  
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS                       
         BAS   RE,GETEL                                                         
         BNE   PLINE10                                                          
         USING TAVUD,R4                                                         
         MVC   PRTMED,TAVUMED      MEDIA                                        
         GOTO1 DATCON,DMCB,(1,TAVUCYCS),(5,PRTCYC)                              
         MVI   PRTCYC+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(1,TAVUCYCE),(5,PRTCYC+9)                            
*                                                                               
PLINE10  MVC   PRTUSE,=C'CLA'      SET USE                                      
         CLI   TAVUUSE,UPAX                                                     
         BNE   *+10                                                             
         MVC   PRTUSE,=C'PAX'                                                   
         CLI   TAVUUSE,ULNA                                                     
         BNE   *+10                                                             
         MVC   PRTUSE,=C'LNA'                                                   
         CLI   TAVUUSE,ULNC                                                     
         BNE   *+10                                                             
         MVC   PRTUSE,=C'LNC'                                                   
         CLI   TAVUUSE,ULNF                                                     
         BNE   *+10                                                             
         MVC   PRTUSE,=C'LNF'                                                   
         CLI   TAVUUSE,ULNN                                                     
         BNE   *+10                                                             
         MVC   PRTUSE,=C'LNN'                                                   
*                                                                               
         MVI   ELCODE,TANPELQ      GET NWK CLASS A PROGRAM DETAILS              
*                                                                               
         CLI   TAVUUSE,UCBL        IF USE IS CABLE                              
         BNE   PLINE12A            SET USE                                      
         MVC   PRTUSE,=C'CBL'                                                   
         B     PLINE12B                                                         
PLINE12A CLI   TAVUUSE,USCB        IF USE IS SPANISH CABLE                      
         BNE   PLINE13             SET USE                                      
         MVC   PRTUSE,=C'SCB'                                                   
PLINE12B LA    R1,HOOKC                                                         
         ST    R1,HEADHOOK         AND RESET HEADHOOK                           
         LA    R1,MYSPECSC                                                      
         ST    R1,SPECS            AND SPECS                                    
         MVI   ELCODE,TAMTELQ      GET MARKET PROGRAM DETAILS                   
*                                                                               
PLINE13  CLI   TAVUUSE,UWSP        IF USE IS WILDSPOT                           
         BNE   PLINE13A            SET USE                                      
         MVC   PRTUSE,=C'WSP'                                                   
         B     PLINE13E                                                         
PLINE13A CLI   TAVUUSE,USWS        IF USE IS SPANISH WILDSPOT                   
         BNE   PLINE13B            SET USE                                      
         MVC   PRTUSE,=C'SWS'                                                   
         B     PLINE13E                                                         
PLINE13B CLI   TAVUUSE,UWSC        IF USE IS CANADIAN WILDSPOT                  
         BNE   PLINE13C            SET USE                                      
         MVC   PRTUSE,=C'WSC'                                                   
         B     PLINE13E                                                         
PLINE13C CLI   TAVUUSE,UADW        IF USE IS ADDENDUM WILDSPOT                  
         BNE   PLINE13D            SET USE                                      
         MVC   PRTUSE,=C'ADW'                                                   
         B     PLINE13E                                                         
PLINE13D CLI   TAVUUSE,URAD        IF USE IS ACTRA RADIO                        
         BNE   PLINE14             SET USE                                      
         MVC   PRTUSE,=C'RAD'                                                   
PLINE13E LA    R1,HOOKW                                                         
         ST    R1,HEADHOOK         AND RESET HEADHOOK                           
         LA    R1,MYSPECSW                                                      
         ST    R1,SPECS            AND SPECS                                    
         MVI   ELCODE,TAMTELQ      GET MARKET PROGRAM DETAILS                   
*                                                                               
PLINE14  CLI   TAVUUSE,ULCB        IF USE IS LOCAL CABLE                        
         BNE   PLINE15             SET USE                                      
         MVC   PRTUSE,=C'LCB'                                                   
         LA    R1,HOOKS                                                         
         ST    R1,HEADHOOK         AND RESET HEADHOOK                           
         LA    R1,MYSPECSS                                                      
         ST    R1,SPECS            AND SPECS                                    
         MVI   ELCODE,TAMTELQ      GET MARKET PROGRAM DETAILS                   
*                                                                               
PLINE15  BAS   RE,PRTSTAT          PRINT STATUS MESSAGES                        
*                                                                               
         LR    R4,R6                                                            
         BAS   RE,GETEL                                                         
         BNE   PLINE20                                                          
         BRAS  RE,MVCUSE                                                        
         LA    R2,L'P(R2)          BUMP TO NEXT LINE                            
         BAS   RE,NEXTEL                                                        
         BNE   PLINE20                                                          
         BRAS  RE,MVCUSE                                                        
*                                                                               
PLINE20  BRAS  RE,P2LINE           PRINT NAMES ON 2ND LINE                      
         BAS   RE,PRNTIT           PRINT THE 1ST AND 2ND LINE                   
*                                                                               
         BRAS  RE,P3LINE           PRINT VERSION ON 3RD LINE                    
         BAS   RE,PRNTIT                                                        
*                                                                               
         BAS   RE,PRTRUSE          PRINT REMAINING USES                         
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         BRAS  RE,CBLTEST                                                       
         BE    PLINE30                                                          
         BRAS  RE,WSPTEST                                                       
         BE    PLINE30                                                          
         BRAS  RE,LCBTEST                                                       
         BE    PLINE30                                                          
         BAS   RE,PRTUCNT          PRINT USE COUNT                              
*                                                                               
PLINE30  LA    R1,HOOK             MAKE SURE HEADHOOK                           
         ST    R1,HEADHOOK         AND SPECS ARE SET CORRECTLY                  
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
PLINEX   B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*              ROUTINE TO PRINT STATUS                                          
         SPACE                                                                  
         USING PRNTD,R2            R2=A(PRINT LINE)                             
PRTSTAT  NTR1                                                                   
         CLI   USAGE,C'Y'          IF NO USAGE RECORD FOUND                     
         BE    *+14                                                             
         MVC   PRTWRN(L'LTNEWCYC),LTNEWCYC                                      
         LA    R2,L'P(R2)          BUMP TO NEXT LINE                            
*                                                                               
         CLI   WRNMSG,C'N'         IF HLD/SESSION WARNING                       
         BE    PRTSTATX                                                         
         LA    RE,PRTWRN                                                        
         MVC   0(L'LTCHKPAY,RE),LTCHKPAY                                        
         LA    RE,L'LTCHKPAY+1(RE)                                              
         MVC   0(3,RE),PTYPE       SET HLD TYPE                                 
         CLI   WRNMSG,C'S'         UNLESS SESSION NOT FOUND                     
         BNE   PRTSTATX                                                         
         MVC   0(3,RE),PTYPE+3     IN WHICH CASE, SET SESSION TYPE              
*                                                                               
PRTSTATX B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT 4TH AND REMAINING USES                          
         SPACE                                                                  
PRTRUSE  NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,CBLTEST                                                       
         BE    PRTRUSE0                                                         
         BRAS  RE,WSPTEST                                                       
         BE    PRTRUSE0                                                         
         BRAS  RE,LCBTEST                                                       
         BNE   PRTRUSE1                                                         
PRTRUSE0 MVI   ELCODE,TAMTELQ                                                   
*                                                                               
PRTRUSE1 LA    R2,P                                                             
         USING PRNTD,R2            R2=A(PRINT LINE)                             
         LR    R4,R6                                                            
         BAS   RE,GETEL                                                         
         BNE   PRTRUSEX                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PRTRUSEX                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PRTRUSEX                                                         
*                                                                               
PRTRUSE5 BAS   RE,NEXTEL           PRINT 4TH AND REMAINING USES                 
         BNE   PRTRUSEX                                                         
         BRAS  RE,MVCUSE           MOVE USE INFO TO PRINT LINE                  
         BAS   RE,PRNTIT           PRINT IT                                     
         B     PRTRUSE5            LOOP FOR MORE USES                           
*                                                                               
PRTRUSEX B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT USE COUNT                                       
         SPACE                                                                  
PRTUCNT  NTR1                                                                   
         LA    R2,P                                                             
         USING PRNTD,R2                                                         
         LA    R1,PRTUPGM+6        PRINT USE COUNT                              
         LH    RE,CNTTANP          IF MORE THAN ONE                             
         CHI   RE,1                                                             
         BNH   PRTUCNTX                                                         
         EDIT  (RE),(4,0(R1)),COMMAS=YES                                        
         MVC   5(4,R1),=C'USES'                                                 
         MVI   SPACING,2           SKIP LINE AFTER USES                         
         BAS   RE,PRNTIT                                                        
PRTUCNTX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO SET R3 TO POINT TO SORT RECORD                        
         SPACE                                                                  
GETSORT  NTR1                                                                   
         TM    WHEN,X'20'          IF SOON PROCESSING                           
         BZ    GETSRT5                                                          
******** TM    WHEN,X'40'          IF NOW PROCESSING                            
******** BZ    GETSRT5                                                          
         XR    R3,R3               NO MORE SORT RECORDS                         
         CLI   FRSTREC,C'Y'        IF FIRST TIME                                
         BNE   GETSRTX                                                          
         L     R3,ASRTREC          SET SORT RECORD                              
         OC    0(SRTKLNQ,R3),0(R3) IF NO SORT RECORD INFO                       
         BNZ   GETSRTX                                                          
         XR    R3,R3               COULDN'T PROCESS HOLD RECORD                 
******** CLI   NOWERR,C'Y'         IF BECAUSE HOLD RECORD TOO LARGE             
******** BNE   *+14                                                             
******** MVC   P(40),=CL40'HOLD RECORD TOO LARGE FOR NOW PROCESSING'            
******** BAS   RE,PRNTIT                                                        
         B     GETSRTX                                                          
*                                                                               
GETSRT5  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,4(R1)         R3=A(SORT RECORD)                            
*                                                                               
GETSRTX  LTR   R3,R3               IF SORT RECORD                               
         BZ    *+8                                                              
         BRAS  RE,TRCSGET          TRACE IT                                     
         XIT1  REGS=(R3)           RETURNS SORT RECORD IN R3                    
         EJECT                                                                  
*              ROUTINE TO ADD LATE NIGHT PROGRAM ELEMENTS                       
         USING BUFFD,R2                                                         
LTNIGHT  NTR1                                                                   
         TM    BUFFSTAT,TANPFLAT   IF NOT LATE NIGHT PROGRAM                    
         BZ    LNNO                OR                                           
*                                                                               
         USING TLDVD,R4                                                         
         L     R4,AIO             ENSURE THAT THIS ADD                          
         LHI   RE,1775            WON'T PUSH RECORD OVER                        
         CH    RE,TLDVLEN         MAX LENGTH                                    
         BH    LN10                                                             
         OI    STATUS,SMAXTANP                                                  
         BAS   RE,FINAREC          FINISH UP ADDING ADVICE REC/PRINT            
         BAS   RE,SETNREC          SET NEW ADVICE RECORD                        
         NI    STATUS,X'FF'-SMAXTANP                                            
         DROP  R4                                                               
*                                                                               
LN10     XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TANPD,R4                                                         
         MVI   TANPEL,X'CC'        SET ELEMENT CODE AS X'CC'                    
         MVI   TANPLEN,TANPLNQ3    SET ELEMENT LENGTH                           
         MVC   TANPDATE,BUFFDATE   USE DATE                                     
         MVC   TANPPNME,BUFFPNME   PROGRAM NAME                                 
         MVC   TANPNWK,BUFFNWK     NETWORK                                      
         MVC   TANPLFT,BUFFLFT     LIFT                                         
         MVC   TANPFEED,BUFFFEED   FEED CODE                                    
         BRAS  RE,ELEMADD                                                       
         SR    RC,RC                                                            
LNNO     LTR   RC,RC                                                            
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              PRINT A LINE                                                     
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES (HEADHOOK)                                 
         SPACE                                                                  
HOOK     NTR1                                                                   
         TM    NOPTION,NOBOX          IF BOXES                                  
         BO    HOOK10                                                           
         GOTO1 =A(INTBOX),DMCB,(RC),RR=RELO   INITIALIZE THEM                   
*                                                                               
HOOK10   MVC   H3+9(L'TGAGY),TGAGY    SET AGENCY CODE AND NAME                  
         MVC   H3+16(L'AGYNAME),AGYNAME                                         
         MVC   H4+9(L'SVCLI),SVCLI    SET CLIENT CODE AND NAME                  
         MVC   H4+16(L'SVCLIN),SVCLIN                                           
         OC    SVPRD,SVPRD                                                      
         BZ    HOOK20                                                           
         MVC   H5+1(7),=C'PRODUCT'                                              
         MVC   H5+9(L'SVPRD),SVPRD    SET PRODUCT CODE AND NAME                 
         MVC   H5+16(L'SVPRDN),SVPRDN                                           
*                                                                               
HOOK20   LA    R2,P                                                             
         USING PRNTD,R2                                                         
         CLC   PRTCID(12),SPACES   IF SPACES                                    
         BNE   HOOKX                                                            
         L     R6,AIO2             R6=A(ADVICE RECORD)                          
         USING TLDVD,R6                                                         
         CLI   TLDVCD,TLDVCDQ      IF ADVICE RECORD                             
         BNE   HOOKX                                                            
         MVC   PRTCID(12),TLDVCID  SET CONTINUED INFORMATION                    
         BRAS  RE,PMAINLEN         POSSIBLY PRINT MAIN LENGTH                   
         MVC   PRTADV,TLDVADV                                                   
         MVC   PRTCONT,=CL11'(CONTINUED)'                                       
HOOKX    B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES FOR CABLE USES (HEADHOOK)                  
         SPACE                                                                  
HOOKC    NTR1                                                                   
         TM    NOPTION,NOBOX          IF BOXES                                  
         BO    HOOKC10                                                          
         GOTO1 =A(INTBOXC),DMCB,(RC),RR=RELO   INITIALIZE THEM                  
*                                                                               
HOOKC10  MVC   H3+9(L'TGAGY),TGAGY    SET AGENCY CODE AND NAME                  
         MVC   H3+16(L'AGYNAME),AGYNAME                                         
         MVC   H4+9(L'SVCLI),SVCLI    SET CLIENT CODE AND NAME                  
         MVC   H4+16(L'SVCLIN),SVCLIN                                           
         OC    SVPRD,SVPRD                                                      
         BZ    HOOKC20                                                          
         MVC   H5+1(7),=C'PRODUCT'                                              
         MVC   H5+9(L'SVPRD),SVPRD    SET PRODUCT CODE AND NAME                 
         MVC   H5+16(L'SVPRDN),SVPRDN                                           
*                                                                               
HOOKC20  LA    R2,P                                                             
         USING PRNTD,R2                                                         
         CLC   PRTCID(12),SPACES   IF SPACES                                    
         BNE   HOOKCX                                                           
         L     R6,AIO2             R6=A(ADVICE RECORD)                          
         USING TLDVD,R6                                                         
         CLI   TLDVCD,TLDVCDQ      IF ADVICE RECORD                             
         BNE   HOOKCX                                                           
         MVC   PRTCID(12),TLDVCID  SET CONTINUED INFORMATION                    
         BRAS  RE,PMAINLEN         POSSIBLY PRINT MAIN LENGTH                   
         MVC   PRTADV,TLDVADV                                                   
         MVC   PRTCONT,=CL11'(CONTINUED)'                                       
HOOKCX   B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES FOR WILDSPOT USES (HEADHOOK)               
         SPACE                                                                  
HOOKW    NTR1                                                                   
         TM    NOPTION,NOBOX          IF BOXES                                  
         BO    HOOKW10                                                          
         GOTO1 =A(INTBOXW),DMCB,(RC),RR=RELO   INITIALIZE THEM                  
*                                                                               
HOOKW10  MVC   H3+9(L'TGAGY),TGAGY    SET AGENCY CODE AND NAME                  
         MVC   H3+16(L'AGYNAME),AGYNAME                                         
         MVC   H4+9(L'SVCLI),SVCLI    SET CLIENT CODE AND NAME                  
         MVC   H4+16(L'SVCLIN),SVCLIN                                           
         OC    SVPRD,SVPRD                                                      
         BZ    HOOKW20                                                          
         MVC   H5+1(7),=C'PRODUCT'                                              
         MVC   H5+9(L'SVPRD),SVPRD    SET PRODUCT CODE AND NAME                 
         MVC   H5+16(L'SVPRDN),SVPRDN                                           
*                                                                               
HOOKW20  LA    R2,P                                                             
         USING PRNTD,R2                                                         
         CLC   PRTCID(12),SPACES   IF SPACES                                    
         BNE   HOOKWX                                                           
         L     R6,AIO2             R6=A(ADVICE RECORD)                          
         USING TLDVD,R6                                                         
         CLI   TLDVCD,TLDVCDQ      IF ADVICE RECORD                             
         BNE   HOOKWX                                                           
         MVC   PRTCID(12),TLDVCID  SET CONTINUED INFORMATION                    
         BRAS  RE,PMAINLEN         POSSIBLY PRINT MAIN LENGTH                   
         MVC   PRTADV,TLDVADV                                                   
         MVC   PRTCONT,=CL11'(CONTINUED)'                                       
HOOKWX   B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES FOR LOCAL CABLE (HEADHOOK)                 
         SPACE                                                                  
HOOKS    NTR1                                                                   
         TM    NOPTION,NOBOX          IF BOXES                                  
         BO    HOOKS10                                                          
         GOTO1 =A(INTBOXS),DMCB,(RC),RR=RELO   INITIALIZE THEM                  
*                                                                               
HOOKS10  MVC   H3+9(L'TGAGY),TGAGY    SET AGENCY CODE AND NAME                  
         MVC   H3+16(L'AGYNAME),AGYNAME                                         
         MVC   H4+9(L'SVCLI),SVCLI    SET CLIENT CODE AND NAME                  
         MVC   H4+16(L'SVCLIN),SVCLIN                                           
         OC    SVPRD,SVPRD                                                      
         BZ    HOOKS20                                                          
         MVC   H5+1(7),=C'PRODUCT'                                              
         MVC   H5+9(L'SVPRD),SVPRD    SET PRODUCT CODE AND NAME                 
         MVC   H5+16(L'SVPRDN),SVPRDN                                           
*                                                                               
HOOKS20  LA    R2,P                                                             
         USING PRNTD,R2                                                         
         CLC   PRTCID(12),SPACES   IF SPACES                                    
         BNE   HOOKSX                                                           
         L     R6,AIO2             R6=A(ADVICE RECORD)                          
         USING TLDVD,R6                                                         
         CLI   TLDVCD,TLDVCDQ      IF ADVICE RECORD                             
         BNE   HOOKSX                                                           
         MVC   PRTCID(12),TLDVCID  SET CONTINUED INFORMATION                    
         BRAS  RE,PMAINLEN         POSSIBLY PRINT MAIN LENGTH                   
         MVC   PRTADV,TLDVADV                                                   
         MVC   PRTCONT,=CL11'(CONTINUED)'                                       
HOOKSX   B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
PFKMSG   OI    STATUS,PFKPEND      SET PF13 PENDING                             
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMTYP,GTMINF       MESSAGE TYPE                                 
         MVC   GTMSGNO,=AL2(35)    MESSAGE NUMBER                               
         MVC   GTMSYS,GETMSYS      TALENT SYSTEM                                
         B     ERRORXIT                                                         
         DROP  RF                                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRORXIT                                                         
*                                                                               
ERRMISS  MVI   ERROR,MISSING                                                    
         B     ERRORXIT                                                         
*                                                                               
ERRPRNT  MVI   ERROR,INVPRINT                                                   
         LA    R2,CONWHENH                                                      
         B     ERRORXIT                                                         
*                                                                               
ERRORXIT GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 10,20                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H1,58,C'ADVICE GENERATE'                                         
         SSPEC H2,58,15X'BF'                                                    
         SPACE 1                                                                
         SSPEC H7,9,C'COMMERCIAL ID AND TITLE'                                  
         SSPEC H7,44,C'MED USE ADVICE'                                          
         SSPEC H7,59,C'CYCLE DATES'                                             
         SSPEC H7,77,C'STATUS'                                                  
         SSPEC H7,95,C'USE# USE DATE PROGRAM NAME    LFT NWK'                   
         SPACE 1                                                                
         SPROG 20                                                               
         SSPEC H8,9,C'-----------------------'                                  
         SSPEC H8,44,C'--- --- ------'                                          
         SSPEC H8,59,C'-----------'                                             
         SSPEC H8,77,C'------'                                                  
         SSPEC H8,95,C'---- -------- ------------    --- ---'                   
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
MYSPECSC DS    0H                                                               
         SPROG 10,20                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H1,58,C'ADVICE GENERATE'                                         
         SSPEC H2,58,15X'BF'                                                    
         SPACE 1                                                                
         SSPEC H7,9,C'COMMERCIAL ID AND TITLE'                                  
         SSPEC H7,44,C'MED USE ADVICE'                                          
         SSPEC H7,59,C'CYCLE DATES'                                             
         SSPEC H7,77,C'STATUS'                                                  
         SSPEC H7,95,C'DATE      CNET'                                          
         SPACE 1                                                                
         SPROG 20                                                               
         SSPEC H8,9,C'-----------------------'                                  
         SSPEC H8,44,C'--- --- ------'                                          
         SSPEC H8,59,C'-----------'                                             
         SSPEC H8,77,C'------'                                                  
         SSPEC H8,95,C'--------  ----'                                          
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
MYSPECSW DS    0H                                                               
         SPROG 10,20                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H1,58,C'ADVICE GENERATE'                                         
         SSPEC H2,58,15X'BF'                                                    
         SPACE 1                                                                
         SSPEC H7,9,C'COMMERCIAL ID AND TITLE'                                  
         SSPEC H7,44,C'MED USE ADVICE'                                          
         SSPEC H7,59,C'CYCLE DATES'                                             
         SSPEC H7,77,C'STATUS'                                                  
         SSPEC H7,95,C'DATE      MKT'                                           
         SPACE 1                                                                
         SPROG 20                                                               
         SSPEC H8,9,C'-----------------------'                                  
         SSPEC H8,44,C'--- --- ------'                                          
         SSPEC H8,59,C'-----------'                                             
         SSPEC H8,77,C'------'                                                  
         SSPEC H8,95,C'--------  ----'                                          
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
MYSPECSS DS    0H                                                               
         SPROG 10,20                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H1,58,C'ADVICE GENERATE'                                         
         SSPEC H2,58,15X'BF'                                                    
         SPACE 1                                                                
         SSPEC H7,9,C'COMMERCIAL ID AND TITLE'                                  
         SSPEC H7,44,C'MED USE ADVICE'                                          
         SSPEC H7,59,C'CYCLE DATES'                                             
         SSPEC H7,77,C'STATUS'                                                  
         SSPEC H7,95,C'DATE      CSYS'                                          
         SPACE 1                                                                
         SPROG 20                                                               
         SSPEC H8,9,C'-----------------------'                                  
         SSPEC H8,44,C'--- --- ------'                                          
         SSPEC H8,59,C'-----------'                                             
         SSPEC H8,77,C'------'                                                  
         SSPEC H8,95,C'--------  ------'                                        
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
RELO     DS    F                                                                
         SPACE 1                                                                
LTNEWCYC DC    C'NEW CYCLE'                                                     
LTCHKPAY DC    C'CHK PAY'                                                       
*                                                                               
MAXTANP  EQU   20                  MAXIMUM # OF TANPD ELES ON ADVICE            
MAXSLNQ2 EQU   SRTKLNQ+TANXLNQ+(1000*TANPLNQ3)                                  
                                                                                
MAXEBUFF EQU   4000                MAX NUMBER OF ELEMENTS                       
BUFFELNQ EQU   MAXEBUFF*BUFFLNQ    MAX LENGTH OF ABUFFEL                        
                                                                                
MYAREAL  EQU   MAXMKTL+MAXSLNQ2                                                 
MAXMKTL  EQU   1277                                                             
         LTORG                                                                  
*              ELEMENTS TO ADD TO ADVICE FROM COMMERCIAL RECORD                 
COMELTAB DS    0X                                                               
         DC    AL1(TALFELQ)                                                     
******** DC    AL1(TACCELQ)                                                     
         DC    X'FF'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,70,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(2174,,,,130)'                         
         DROP  RB,R5                                                            
         EJECT                                                                  
*              ROUTINE TO SET AGENCY CODE, NAME & ADDRESS FOR LOGO              
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
SETAGY   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',SRTAGY),0                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AGYNAME,TGNAME      AGENCY NAME                                  
*                                                                               
         L     R4,AIO              R4=A(AGENCY DETAILS ELEMENT)                 
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYOFF,TAAYTPOF     SET TALENT AGENCY OFFICE                     
         MVC   AGYSTAT,TAAYSTAT    SET AGENCY STATUS BYTE 1                     
         MVC   AGYSTAT4,TAAYSTA4   SET AGENCY STATUS BYTE 4                     
*                                                                               
         MVC   AGYADDR,SPACES                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ      R4=A(AGENCY ADDRESS ELEMENT)                 
         BRAS  RE,GETEL                                                         
         BNE   SETAGYX                                                          
*                                                                               
         USING TAADD,R4                                                         
         ZIC   RE,TAADLNES                                                      
         CHI   RE,4                IF FOUR LINES                                
         BNE   *+6                                                              
         BCTR  RE,0                USE FIRST THREE ONLY                         
         LA    R1,TAADADD                                                       
         LA    R2,AGYADDR                                                       
*                                                                               
SETAGY10 MVC   0(L'TAADADD,R2),0(R1)                                            
         LA    R1,L'TAADADD(R1)                                                 
         LA    R2,L'AGYADDR1(R2)                                                
         BCT   RE,SETAGY10                                                      
*                                                                               
SETAGYX  XIT1                                                                   
         DROP  R3,R4                                                            
         SPACE 2                                                                
*              ROUTINE TO CHECK FOR BSS/SSS/ADT USAGE RECORD WHOSE              
*              PERIOD IS WITHIN THE CYCLE START                                 
         SPACE                                                                  
CHKSES   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHUSE,PTYPE+3     SESSION TYPE                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BE    CHKSES8                                                          
         MVI   WRNMSG,C'S'         SESSION PYMT NOT FOUND                       
         B     CHKSESX                                                          
*                                                                               
CHKSES5  GOTO1 SEQ                                                              
         CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BNE   CHKSESX                                                          
*                                                                               
CHKSES8  GOTO1 GETREC                                                           
         BRAS  RE,CHKDATE          CHECK CYCLE START WITHIN USAGE DATES         
         CLI   WRNMSG,C'N'                                                      
         BNE   CHKSES5                                                          
CHKSESX  MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK IF CNET/MKT/CSYS EXISTS ON ANOTHER          *         
*        ADVICE FOR THIS COMMERCIAL                                   *         
*        ON ENTRY ... R2=A(BUFFER OF TANPD ELEMENTS)                  *         
***********************************************************************         
                                                                                
         USING BUFFD,R2                                                         
ONDIFADV NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO2             READ ALL ADVICES FOR THE CURRENT             
         XC    KEY,KEY             AGENCY/COMMERCIAL                            
         MVC   KEY(TLDVADV-TLDVD),0(RE)                                         
         GOTO1 HIGH                                                             
         J     ODA20                                                            
ODA10    GOTO1 SEQ                                                              
ODA20    CLC   KEY(TLDVADV-TLDVD),KEYSAVE                                       
         JNE   NO                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO2            RESTORE I/O AREA                             
                                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO1             IF AIR/FLIGHT DATE FITS INTO                 
         MVI   ELCODE,TAVUELQ      THE ADVICE'S CYCLE ...                       
         BRAS  RE,GETEL                                                         
         JNE   ODA10                                                            
         CLC   BUFFDATE,TAVUCYCS                                                
         JL    ODA10                                                            
         CLC   BUFFDATE,TAVUCYCE                                                
         JH    ODA10                                                            
         DROP  R4                                                               
                                                                                
         USING TAMTD,R4                                                         
         L     R4,AIO1             AND IF ELEMENT ALREADY EXISTS                
         MVI   ELCODE,TAMTELQ      FOR THIS CNET/MKT/CSYS,                      
         BRAS  RE,GETEL            DO NOT ADD IT AGAIN                          
         J     *+8                                                              
ODA30    BRAS  RE,NEXTEL                                                        
         JNE   ODA10                                                            
         CLC   TAMTCODE,BUFFMKT                                                 
         JNE   ODA30                                                            
         J     YES                                                              
         DROP  R2,R4                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD MARKET TO MARKET TABLE                            
         USING BUFFD,R2            R2=A(BUFFER OF TANPD ELEMENTS)               
         SPACE                                                                  
         USING MKTTABD,RE                                                       
ADDMKT   NTR1  BASE=*,LABEL=*                                                   
         L     RE,AMKTTAB                                                       
AMKT10   CLI   0(RE),X'FF'         IF END OF TABLE, EXIT                        
         BE    AMKTX                                                            
         CLI   0(RE),0             IF THERE IS ALREADY AN ENTRY                 
         BE    AMKT20                                                           
         LA    RE,MKTTBLNQ(RE)     BUMP TO NEXT ENTRY                           
         B     AMKT10                                                           
AMKT20   MVC   MKTCOD,BUFFCSYS     SAVE MARKET CODE                             
         OC    MKTCOD,SPACES                                                    
         MVI   MKTSPACE,C' '       SPACE                                        
         MVC   MKTNAM,BUFFMKTN     SAVE MARKET NAME                             
AMKTX    XIT1                                                                   
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ELIMINATE ALREADY PAID CNET/MKT/CSYS                  
*              CODES AND CUT OFF THE NAME IF IT EXISTS ON SYSTEM                
*              ON ENTRY ... R4=A(PROGRAM DETAILS ELEMENT)                       
*                                                                               
SPTANPEL NTR1  BASE=*,LABEL=*                                                   
         USING TANPD,R3                                                         
         LR    R3,R4               R3=A(PROGRAM DETAILS ELEMENT)                
         MVC   SORTLEN,TANPLEN                                                  
*                                                                               
         CLI   TANPLEN,TANPLNQ3    EXIT IF NETWORK USE                          
         BNH   STYES                                                            
*                                                                               
         LHI   RF,TLMTCDQ          SET TO READ CNET/CSYS/MKT RECORDS            
         XC    DUB,DUB             DUB WILL STORE CNET/CSYS/MKT CODE            
*                                                                               
         LHI   R2,TANPLNQN                                                      
         MVC   DUB(L'TANPNTI),TANPNTI                                           
         MVI   TGMTTYPE,TANPNET    SET TO READ CNET RECORDS                     
         CLI   TANPTYP,TANPNET     IF NETWORK CABLE                             
         BE    SPTANP10                                                         
*                                                                               
         LHI   R2,TANPLNQC                                                      
         MVC   DUB(L'TANPSYS),TANPSYS                                           
         MVI   TGMTTYPE,C'S'       SET TO READ CSYS RECORDS                     
         CLI   TANPTYP,TANPCSYS    FOR LOCAL CABLE                              
         BE    SPTANP10                                                         
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         LA    RE,TANPMKT                                                       
         LHI   R1,L'TANPMKT                                                     
SPTANP05 CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,SPTANP05                                                      
*                                                                               
         LHI   R2,TANPLNQS                                                      
         MVC   DUB(L'TANPMKT),TANPMKT                                           
         MVI   TGMTTYPE,C'R'       ELSE, SET TO READ RMKT RECORDS               
         CLI   TIMED,C'R'          UNLESS MEDIA IS TELEVISION                   
         BE    SPTANP10            THEN SET TO READ TMKT RECORDS                
         MVI   TGMTTYPE,C'T'                                                    
         LHI   RF,TLMTALDQ                                                      
*                                                                               
SPTANP10 GOTO1 RECVAL,DMCB,(RF),(X'84',DUB)                                     
         BNE   STYES               IF CNET/MKT/CSYS IS ON FILE                  
         STC   R2,SORTLEN          NAME IS NOT REQUIRED                         
*                                                                               
         USING TLUHPD,R4                                                        
         LA    R4,KEY              READ ALL USAGE HISTORY KEYS                  
         XC    TLUHPKEY,TLUHPKEY   FOR THIS CNET/MKT/CSYS ON THIS               
         MVI   TLUHPCD,TLUHMTDQ    COMMERCIAL                                   
         MVC   TLUHMINM,TGMTINTC                                                
         MVC   TLUHMCOM,TGCOM                                                   
SPTANP20 GOTO1 HIGH                                                             
         B     SPTANP40                                                         
SPTANP30 GOTO1 SEQ                                                              
SPTANP40 CLC   KEY(TLUHMUSE-TLUHPCD),KEYSAVE                                    
         BNE   SPTANP60                                                         
*                                                                               
         CLI   TGVER,0             IF HOLD WAS MATCHED BY VERSION               
         BE    SPTANP50                                                         
         CLC   TLUHMVER,TGVER      ONLY CHECK HIST FOR THAT VERSION             
         BNE   SPTANP30                                                         
*                                                                               
SPTANP50 CLC   TLUHMC1D,TANPDATE   IF USE DATE FITS WITHIN AN EXISTING          
         BH    SPTANP30            CYCLE FOR THIS CNET/MKT/CSYS ...             
         CLC   TLUHMCYE,TANPDATE                                                
         BL    SPTANP30                                                         
*                                                                               
         TM    TLUHMSTA,TLUHMCRD   OK TO BRING OVER IF ON A CREDIT              
         BZ    STNO                PAYMENT, ELSE ELIMINATE                      
         DROP  R4                                                               
*                                                                               
***********************************************************************         
                                                                                
SPTANP60 CLI   TGVER,0             IF HOLD WAS MATCHED BY VERSION               
         BE    STYES                                                            
         LA    R2,CASTTAB          PREPARE TO BUILD CAST SEQUENCE TAB           
         MVI   0(R2),X'FF'                                                      
*                                                                               
         MVC   AIO,AIO2            PREPARE TO READ CAST INTO AIO2               
*                                                                               
         USING TLCAD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             READ ALL CAST RECORDS FOR COMMERCIAL         
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     SPTANP80                                                         
SPTANP70 GOTO1 SEQ                                                              
SPTANP80 CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   SPTAN110                                                         
         LA    R4,KEY                                                           
         TM    TLCASORT,X'80'      IF WE'VE REACHED THE MUSICIANS               
         BO    SPTAN110            WE'RE DONE BUILDING TABLE                    
         DROP  R4                                                               
*                                                                               
         GOTO1 GETREC              GET THE CAST RECORD                          
*                                                                               
         MVI   ELCODE,TAFNELQ      GET VERSIONS ELEMENT                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   SPTANP70                                                         
*                                                                               
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,251        IF ON ALL VERSIONS, ADD CAST TO              
         BE    SPTAN100            CAST TABLE                                   
         ZIC   R0,TAFNLEN                                                       
         AHI   R0,-3                                                            
         LA    R1,TAFNNAME                                                      
SPTANP90 CLC   TGVER,0(R1)         IF ON THIS VERSION, ADD CAST TO              
         BE    SPTAN100            CAST TABLE                                   
         LA    R1,1(R1)                                                         
         BCT   R0,SPTANP90                                                      
         B     SPTANP70            ELSE, GO READ NEXT CAST RECORD               
         DROP  R4                                                               
*                                                                               
SPTAN100 MVC   0(2,R2),KEY+TLCASEQ-TLCAD                                        
         LA    R2,2(R2)            ADD CAST SEQUENCE NUMBER TO TABLE            
         MVI   0(R2),X'FF'         AND MARK NEW END OF CAST TABLE               
         B     SPTANP70            GO READ NEXT CAST RECORD                     
*                                                                               
SPTAN110 LA    R2,CASTTAB          R2=A(VERSION'S CAST TABLE)                   
         MVI   CSTOVER,C'N'        SET CAST ON THIS VERSION INDICATOR           
                                                                                
SPTAN120 CLI   0(R2),X'FF'         IF EVERYONE ON CAST HAS BEEN PAID            
         BE    SPTAN170            THIS CNET/MKT/CSYS, DON'T ADD IT             
         MVI   CSTOVER,C'Y'                                                     
*                                                                               
         USING TLUHPD,R4                                                        
         LA    R4,KEY                                                           
         XC    TLUHPKEY,TLUHPKEY   READ ALL USAGE HISTORY KEYS                  
         MVI   TLUHPCD,TLUHMTDQ    FOR THIS CNET/MKT/CSYS FOR THIS              
         MVC   TLUHMINM,TGMTINTC   CAST MEMBER                                  
         MVC   TLUHMCOM,TGCOM                                                   
         MVC   TLUHMSEQ,0(R2)                                                   
SPTAN130 GOTO1 HIGH                                                             
         B     SPTAN150                                                         
SPTAN140 GOTO1 SEQ                                                              
SPTAN150 CLC   KEY(TLUHMUSE-TLUHPCD),KEYSAVE                                    
         BNE   SPTAN170                                                         
*                                                                               
         CLC   TLUHMCCS,TANPDATE   IF USE DATE FITS WITHIN AN EXISTING          
         BH    SPTAN140            CYCLE FOR THIS CNET/MKT/CSYS ...             
         CLC   TLUHMCYE,TANPDATE                                                
         BL    SPTAN140                                                         
         TM    TLUHMSTA,TLUHMCRD   AND THIS INVOICE WAS A CREDIT                
         BO    SPTAN170            OK TO ADD CNET/MKT/CSYS                      
         DROP  R4                                                               
*                                                                               
SPTAN160 LA    R2,2(R2)                                                         
         B     SPTAN120                                                         
*                                                                               
SPTAN170 CLI   0(R2),X'FF'         IF CNET/MKT/CSYS PAID TO ALL                 
         BE    SPTAN200            VERSION CAST MEMBERS, DON'T ADD              
*                                                                               
         USING TLUHPD,R4                                                        
         LA    R4,KEY              READ ALL USAGE HISTORY KEYS                  
         XC    TLUHPKEY,TLUHPKEY   FOR THIS CNET/MKT/CSYS ON THIS               
         MVI   TLUHPCD,TLUHMTDQ    COMMERCIAL                                   
         MVC   TLUHMINM,TGMTINTC   (REGARDLESS OF VERSION)                      
         MVC   TLUHMCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         B     SPTAN190                                                         
SPTAN180 GOTO1 SEQ                                                              
SPTAN190 CLC   KEY(TLUHMUSE-TLUHPCD),KEYSAVE                                    
         BNE   SPTAN200                                                         
         CLC   TLUHMC1D,TANPDATE   IF USE DATE FITS WITHIN AN EXISTING          
         BH    SPTAN180            CYCLE FOR THIS CNET/MKT/CSYS ...             
         CLC   TLUHMCYE,TANPDATE                                                
         BL    SPTAN180                                                         
         DROP  R4                                                               
*                                                                               
         GOTO1 GETREC              GET USAGE HISTORY RECORD                     
*                                                                               
         USING TAUHD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SPTAN180                                                         
         CLC   TAUHEND,TANPDATE    IF COMMERCIAL'S CYCLE END DATE               
         BNL   SPTAN180            IS EARLIER THAN MARKET DATE                  
         MVI   0(R2),X'FF'         ELIMINATE THE MARKET DATE                    
         DROP  R3,R4                                                            
*                                                                               
SPTAN200 XC    KEY,KEY             RESET TO HOLD RECORD INTO TIAREC             
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),TIKEY                                               
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,TIAREC                                                       
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,TANPELQ      RESET FOR NEXTEL                             
*                                                                               
         CLI   CSTOVER,C'N'        IF PERFORMERS ARE ON THE VERSION             
         BE    STYES                                                            
         CLI   0(R2),X'FF'         AND CNET/MKT/CSYS PAID TO ALL                
         BE    STNO                VERSION CAST MEMBERS, DON'T ADD              
*                                                                               
STYES    XR    RC,RC                                                            
STNO     LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
CASTTAB  DS    275XL2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET TANXD ELEMENT IN SORT                             
*                                  XIT - (R3) NEXT POS. IN SRTREC               
         USING TLNXD,R6            R6=A(HOLD RECORD)                            
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
         SPACE                                                                  
SETSRTNX NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ      GET HOLD DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   SETSNXX                                                          
         USING TANXD,R4                                                         
         MVC   TANXUDTE,TGTODAY1   SET DATE MARKED USED                         
         MVC   TANXTYPE,SVXTYPE    AND MATCH TYPE                               
         ZIC   R1,TANXLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TANXD       COPY IT TO SORT RECORD                       
         LA    R1,1(R1)            RESTORE LENGTH                               
         AR    R3,R1                                                            
SETSNXX  XIT1  REGS=(R3)                                                        
         DROP  R3,R4,R6                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET BROADCAST LAG CYCLE END DATE FOR THE              
*              CABLE NETWORK, MARKET OR CABLE SYSTEM                            
         SPACE 1                                                                
STLAGEND NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,STRTDATE),(0,TGDUB)                               
         GOTO1 DATCON,DMCB,(1,ENDDATE),(0,TGDUB+6)                              
         SPACE 1                                                                
         LHI   R2,1                SAVE LENGTH OF CYCLE (IN DAYS)               
SLE10    STC   R2,CYCLEN           IN CYCLEN                                    
         GOTO1 ADDAY,DMCB,TGDUB,WORK,(R2)                                       
         CLC   WORK(6),TGDUB+6                                                  
         BE    SLE20                                                            
         AHI   R2,1                                                             
         B     SLE10                                                            
         SPACE 1                                                                
SLE20    MVC   LAGEND,ENDDATE      IF CYCLE IS 8 WEEKS                          
         LHI   RF,14               LAG END IS 2 WEEKS LATER                     
         CLI   CYCLEN,56           IF CYCLE IS 13 WEEKS                         
         BL    SLE40               LAG END IS 4 WEEKS LATER                     
         BE    SLE30               ELSE, LAG END IS SAME AS                     
         LHI   RF,28               CYCLE END                                    
         SPACE 1                                                                
SLE30    GOTO1 ADDAY,DMCB,TGDUB+6,WORK,(RF)       SAVE LAG END DATE             
         GOTO1 DATCON,DMCB,(0,WORK),(1,LAGEND)    IN LAGEND                     
         SPACE 1                                                                
         USING TAMTD,R4                                                         
SLE40    LA    R4,ELEMENT                                                       
         GOTO1 DATCON,DMCB,(1,TAMTCYCS),(0,TGDUB)                               
         SPACE 1                                                                
         ZIC   RF,CYCLEN                          CALCULATE NEW CYCLE           
         GOTO1 ADDAY,DMCB,TGDUB,WORK,(RF)         END                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TAMTCYCE)                                
         SPACE 1                                                                
         CLC   TAMTCYCE,LAGEND                    IF LATER THAN LAG             
         BNH   *+10                               END DATE                      
         MVC   TAMTCYCE,LAGEND                    USE LAG END DATE              
         DROP  R4                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT MAIN COMMERCIAL'S LENGTH                        
         SPACE 1                                                                
         USING PRNTD,R2                                                         
PMAINLEN NTR1  BASE=*,LABEL=*                                                   
         USING TANXD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANXELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   PMLENX                                                           
         CLI   TANXTYPE,TANXTAKA   IF MATCHED BY ALIAS                          
         BE    PMLEN10                                                          
         CLI   TANXTYPE,TANXTTAL   OR TALENT COMMERCIAL                         
         BNE   PMLENX                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TADVD,R4                                                         
PMLEN10  L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   PMLENX                                                           
         LA    RE,PRTCID           PRINT OUT MAIN COMMERCIAL LENGTH             
PMLEN20  LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   PMLEN20                                                          
         MVI   1(RE),C':'                                                       
         EDIT  TADVSEC,(3,2(RE)),ALIGN=LEFT                                     
PMLENX   XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
ELEMADD  NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,ELEMENT+1        R1=LENGTH OF ELEMENT TO ADD                  
         SPACE 1                                                                
         LHI   RF,1899             RF=BIGGEST THE RECORD CAN ALREADY            
         SR    RF,R1                  BE BEFORE WE ADD THIS ELEMENT             
         SPACE 1                                                                
         USING TLDVD,RE                                                         
         L     RE,AIO              IF LENGTH OF RECORD EXCEEDS THAT             
         CH    RF,TLDVLEN          AMOUNT, DO NOT ADD ELEMENT                   
         BL    EANO                                                             
         DROP  RE                                                               
         SPACE 1                                                                
         GOTO1 ADDELEM             ELSE, ADD ELEMENT                            
EAYES    XR    RC,RC                                                            
EANO     LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF CYCLE START WITHIN USAGE DATES               
*                                  XIT WRNMSG=N IF WITHIN DATES                 
         SPACE                                                                  
CHKDATE  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAUHD,R4                                                         
         CLC   STRTDATE,TAUHSTRT   IF HLD USAGE RECORD COVERS                   
         BL    CHKDATEX                                                         
         CLC   STRTDATE,TAUHEND    CYCLE START DATE THEN                        
         BH    CHKDATEX                                                         
         MVI   WRNMSG,C'N'         NO WARNING NECESSARY(SES PYMT FND)           
CHKDATEX XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE                                                                  
RDHUH    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         XC    KEY,KEY             BUILD USAGE HISTORY KEY                      
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         CLC   TLUHUSE,=C'ITN'                                                  
         BNE   *+10                                                             
         MVC   TLUHUSE,=C'CLA'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BNE   RDHUH10                                                          
*                                                                               
         MVC   SVUHKEY,KEY         SAVE KEY                                     
         MVC   AIO,AIO1            SET IOAREA                                   
         GOTO1 GETREC                                                           
         BRAS  RE,SETUSAGE         SET INFO FROM USAGE HISTORY RECORD           
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            SET AIO TO ADVICE                            
         B     RDHUHX                                                           
*                                                                               
RDHUH10  BRAS  RE,LCBCHK                                                        
RDHUHX   XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO READ SEQUENTIAL                                       
         SPACE                                                                  
RDSUH    NTR1  BASE=*,LABEL=*                                                   
         USING BUFFD,R2                                                         
         TM    BUFFSTAT,TANPFLAT   IF LATE NIGHT PROGRAM                        
         BO    RDSUHX              DON'T BOTHER                                 
         DROP  R2                                                               
*                                                                               
         MVI   USAGE,C'N'          NO USAGE HISTORY RECORD FOUND                
         XC    USENUM,USENUM       RESET USAGE NUMBER                           
         OC    SVUHKEY,SVUHKEY     IF USAGE HISTORY KEY                         
         BZ    RDSUH10                                                          
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         XC    KEY,KEY             RESET READ SEQUENCE                          
         MVC   KEY(L'SVUHKEY),SVUHKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLUHKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                 GET NEXT KEY                                 
         CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BE    *+14                                                             
         XC    SVUHKEY,SVUHKEY                                                  
         B     RDSUH10                                                          
*                                                                               
         MVC   SVUHKEY,KEY         SAVE NEW USAGE HISTORY KEY                   
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BRAS  RE,SETUSAGE         SET INFO FROM USAGE HISTORY RECORD           
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         B     RDSUHX                                                           
*                                                                               
RDSUH10  BRAS  RE,LCBCHK                                                        
RDSUHX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
LCBCHK   NTR1  BASE=*,LABEL=*                                                   
         CLI   SVUSEQU,UCBL        IF NATIONAL CABLE USE                        
         BE    LCBCHK10                                                         
         CLI   SVUSEQU,USCB        OR SPANISH CABLE USE                         
         BE    LCBCHK10                                                         
         CLI   SVUSEQU,ULCB        OR LOCAL CABLE USE                           
         BNE   LCBCHKX                                                          
         USING TLUHD,R2                                                         
LCBCHK10 LA    R2,KEYSAVE          AND WE ALREADY SEARCHED FOR                  
         CLC   TLUHUSE,=C'LCB'     LOCAL CABLE, WE'RE DONE                      
         BE    LCBCHKX                                                          
*                                                                               
         CLI   SVUSEQU,ULCB        ELSE, IF USE IS CABLE                        
         BE    LCBCHK20            OR SPANISH CABLE                             
         MVC   TGUSCDE,=C'LCB'     GO SEARCH LOCAL CABLE                        
         BRAS  RE,RDHUH                                                         
         MVC   TGUSCDE,SVUSCDE                                                  
         B     LCBCHKX                                                          
*                                                                               
LCBCHK20 CLC   TLUHUSE,=C'CBL'     ELSE, IF USE IS LOCAL CABLE                  
         BNE   LCBCHK30                                                         
         MVC   TGUSCDE,=C'SCB'     SEARCH SPANISH CABLE                         
         BRAS  RE,RDHUH                                                         
         MVC   TGUSCDE,SVUSCDE                                                  
         CLI   USAGE,C'Y'                                                       
         BE    LCBCHKX                                                          
*                                                                               
LCBCHK30 CLC   TLUHUSE,=C'SCB'     THEN SEARCH LOCAL CABLE                      
         BNE   LCBCHKX                                                          
         MVC   TGUSCDE,=C'LCB'                                                  
         BRAS  RE,RDHUH                                                         
         MVC   TGUSCDE,SVUSCDE                                                  
         DROP  R2                                                               
LCBCHKX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT 2ND LINE                                        
P2LINE   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P2                                                            
         USING PRNT2D,R2                                                        
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTTTL                                  
         MVC   PRTCIDN,TGNAME      TALENT TITLE                                 
         DROP  R2                                                               
*                                                                               
         USING PRNTD,R2                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   P2LX                                                             
         MVC   PRTUSE,=C'UPG'                                                   
*                                                                               
P2LX     XIT1                                                                   
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT 3RD LINE                                        
P3LINE   NTR1  BASE=*,LABEL=*                                                   
         USING PRNT3D,R2                                                        
         LA    R2,P                                                             
         SPACE 1                                                                
         USING TANXD,R4                                                         
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   P3L40                                                            
         CLI   TANXTYPE,TANXTAKA       SKIP AHEAD IF MATCHED BY ALIAS           
         BE    P3L40                                                            
         CLI   TANXTYPE,TANXTTAL       OR TALENT COMM'L ID                      
         BE    P3L40                                                            
         MVC   PRTVERSH,=CL7'VERSION'                                           
         CLI   TANXTYPE,TANXTVER       PROCESS IF MATCHED BY VERSION            
         BE    P3L10                                                            
         CLI   TANXTYPE,TANXTALV       OR VERSION ALIAS                         
         BE    P3L10                                                            
         MVC   PRTVERSH,=CL7'LIFT ID'                                           
         CLI   TANXTYPE,TANXTLFT       PROCESS IF MATCHED BY LIFT               
         BE    P3L20                                                            
         CLI   TANXTYPE,TANXTALF       OR LIFT ALIAS                            
         BE    P3L20                                                            
         B     P3L40                                                            
         SPACE 1                                                                
         USING TAFND,R4                                                         
P3L10    LR    R4,R6                                                            
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   P3L40                                                            
         L     R4,TGELEM                                                        
         MVC   PRTVERSL(1),TAFNNAME                                             
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    P3L20                                                            
         EDIT  (1,TAFNNAME),(3,PRTVERSL),ALIGN=LEFT                             
         DROP  R4                                                               
         SPACE 1                                                                
P3L20    MVC   PRTVERSI(L'STACOLVI),STACOLVI                                    
         LA    RE,PRTVERSI                                                      
P3L30    LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   P3L30                                                            
         MVI   1(RE),C':'                                                       
         EDIT  STACOLVL,(3,2(RE)),ALIGN=LEFT                                    
         SPACE 1                                                                
         USING TAVUD,R4                                                         
P3L40    LR    R4,R6                                                            
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,CBLTEST                                                       
         BE    P3L50                                                            
         BRAS  RE,WSPTEST                                                       
         BE    P3L50                                                            
         BRAS  RE,LCBTEST                                                       
         BNE   P3L60                                                            
P3L50    BRAS  RE,PRTMISS                                                       
         MVI   ELCODE,TAMTELQ                                                   
         SPACE 1                                                                
         USING PRNTD,R2            R2=A(PRINT LINE)                             
P3L60    LR    R4,R6                                                            
         BRAS  RE,GETEL                                                         
         BNE   P3LX                                                             
         BRAS  RE,NEXTEL                                                        
         BNE   P3LX                                                             
         BRAS  RE,NEXTEL                                                        
         BNE   P3LX                                                             
         BRAS  RE,MVCUSE           MOVE USE INFO TO PRINT LINE                  
         DROP  R2                                                               
P3LX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO MOVE USE INFO TO PRINT LINE                           
         SPACE                                                                  
         USING PRNTD,R2                                                         
MVCUSE   NTR1  BASE=*,LABEL=*                                                   
         USING TANPD,R4                                                         
         CLI   ELCODE,TANPELQ                                                   
         BNE   MVCUSE10                                                         
         EDIT  TANPUSEN,(4,PRTUSEN) USE NUMBER                                  
         GOTO1 DATCON,DMCB,(1,TANPDATE),(5,PRTUSEDT)                            
         MVC   PRTUPGM,TANPPNME    PROGRAM NAME                                 
         CLI   TANPLFT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PRTULFT,C'Y'        LIFT                                         
         MVC   PRTUNWK,TANPNWK     NETWORK                                      
         B     MVCUSEX                                                          
         DROP  R4                                                               
         SPACE 2                                                                
         USING TAMTD,R4                                                         
MVCUSE10 CLI   ELCODE,TAMTELQ                                                   
         BNE   MVCUSEX                                                          
         GOTO1 DATCON,DMCB,(1,TAMTCYCS),(8,PRTMDATE)                            
         MVC   PRTCSYS,TAMTCODE                                                 
MVCUSEX  XIT1                                                                   
         DROP  R2,R4                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO INDICATE MAJORS ON A WSP OR CBL ADVICE                
         SPACE                                                                  
         USING BUFFD,R2                                                         
ADDMAJS  NTR1  BASE=*,LABEL=*                                                   
         CLI   BUFFTYP,TANPSPT   IF SPOT GENERATED USE                          
         BNE   AMAJSX                                                           
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO            R4=A(ADVICE USE DETAILS ELEMENT)               
         MVI   ELCODE,TAVUELQ    ELEMENT                                        
         BRAS  RE,GETEL                                                         
         BNE   AMAJSX                                                           
         CLI   TAVUUSE,UWSP      ONLY DO FOR REGULAR WILDSPOT                   
         BNE   AMAJSX                                                           
         SPACE 1                                                                
         CLI   TAVUMED,C'T'      IF MEDIA IS TELEVISION                         
         BNE   AMAJS10                                                          
         CLC   =X'D5E80000',BUFFMKT   IF MARKET IS NEW YORK                     
         BNE   *+12                                                             
         OI    TAVUMAJ,NY             TURN ON NY MAJOR                          
         B     AMAJSX                                                           
         CLC   =X'C3C8C900',BUFFMKT   IF MARKET IS CHICAGO                      
         BNE   *+12                                                             
         OI    TAVUMAJ,CHI            TURN ON CHICAGO MAJOR                     
         B     AMAJSX                                                           
         CLC   =X'D3C10000',BUFFMKT   IF MARKET IS LOS ANGELES                  
         BNE   AMAJSX                                                           
         OI    TAVUMAJ,LA             TURN ON LOS ANGELES MAJOR                 
         B     AMAJSX                                                           
         SPACE 1                                                                
AMAJS10  CLI   TAVUMED,C'R'      IF SPOT AND MEDIA IS RADIO                     
         BNE   AMAJSX                                                           
         CLC   =X'D5E80000',BUFFMKT   IF MARKET IS NEW YORK                     
         BNE   *+12                                                             
         OI    TAVUMAJ,NY             TURN ON NEW YORK MAJOR                    
         B     AMAJSX                                                           
         CLC   =X'C3C8C900',BUFFMKT   IF MARKET IS CHICAGO                      
         BNE   *+12                                                             
         OI    TAVUMAJ,CHI            TURN ON CHICAGO MAJOR                     
         B     AMAJSX                                                           
         CLC   =X'D3C10000',BUFFMKT   IF MAJOR IS LA                            
         BNE   AMAJSX                                                           
         OI    TAVUMAJ,LA             TURN ON LA MAJOR                          
         B     AMAJSX                                                           
AMAJSX   XIT1                                                                   
         DROP  R2,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO READ CURRENT COMMERCIAL RECORD                        
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
RDCOM    NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1            SET TO READ COMMERCIAL AIO1                  
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'A0',SRTTCID),0                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2            RESET TO ADVICE RECORD                       
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO MAKE SURE ADVICE REC NOT ALREADY ON FILE              
         SPACE 1                                                                
CHKDUP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 HIGH                READ HIGH FOR ADVICE KEY                     
         CLC   KEY(L'TLDVKEY),KEYSAVE SET CC CODE                               
         MVC   KEY(L'TLDVKEY),KEYSAVE RESTORE KEY                               
         XIT1                      CC EQUAL - DUPLICATE                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET SEQUENCE NUMBER FOR TAXC ELEMENTS                 
         SPACE 1                                                                
SETSEQ   NTR1  BASE=*,LABEL=*                                                   
         LHI   R2,1                                                             
         SPACE 1                                                                
         USING TAXCD,R4                                                         
SSEQ10   L     R4,AIO2                                                          
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
SSEQ20   BRAS  RE,NEXTEL                                                        
         BNE   SSEQ40                                                           
         CLC   =C'INCLUDES',TAXCCMNT                                            
         BNE   SSEQ30                                                           
         TM    STATUS,ADDNOFEL                                                  
         BO    SSEQ30                                                           
         MVI   TAXCEL,X'FF'                                                     
         B     SSEQ20                                                           
SSEQ30   AHI   R2,1                                                             
         B     SSEQ20                                                           
         SPACE 1                                                                
SSEQ40   MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
SSEQ50   LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAXCEL,TAXCELQ                                                   
         STC   R2,TAXCSEQ                                                       
         DROP  R4                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         USING BUFFD,R2                                                         
RDUSEDTS NTR1  BASE=*,LABEL=*                                                   
         MVC   STRTDATE,BUFFCYCS      IF ELEMENT CONTAINED CYCLE INFO           
         MVC   ENDDATE,BUFFCYCE       SAVE IT                                   
RUDX     XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
RDLCBADV NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCBL                                                     
         BNE   RLADVX                                                           
         MVI   TGUSEQU,ULCB                                                     
         BRAS  RE,RDADV                                                         
         MVI   TGUSEQU,UCBL                                                     
RLADVX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         USING BUFFD,R2                                                         
RDADV    NTR1  BASE=*,LABEL=*                                                   
         MVI   ADDORCHG,C'A'                                                    
         SPACE 1                                                                
         CLI   BUFFTYP,TANPNET   USE MUST BE CABLE NETWORK                      
         BE    RDADV10                                                          
         CLI   BUFFTYP,TANPSPT   OR SPOT                                        
         BE    RDADV10                                                          
         CLI   BUFFTYP,TANPCSYS  OR CABLE SYSTEM                                
         BNE   RDADVX            OR WE CANNOT MERGE                             
         SPACE 1                                                                
RDADV10  MVC   SVKEY,KEY         SAVE CURRENT KEY                               
         SPACE 1                                                                
         L     RE,AIO2           RE=A(ADVICE RECORD BEING BUILT)                
         SPACE 1                                                                
         XC    KEY,KEY                       SEE IF ADVICE FOR THIS             
         MVC   KEY(TLDVADV-TLDVCD),0(RE)       AGENCY/COMMERCIAL IS             
******** MVI   RDUPDATE,C'Y'                   ALREADY ON FILE                  
         GOTO1 HIGH                                                             
         B     RDADV30                                                          
******** MVI   RDUPDATE,C'Y'                                                    
RDADV20  GOTO1 SEQ                                                              
RDADV30  CLC   KEY(TLDVADV-TLDVCD),KEYSAVE                                      
         BNE   RDADV190                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TLDVD,R4                                                         
         L     R4,AIO1           ADVICE MUST HAVE ENOUGH ROOM TO                
         LHI   RE,1775           MERGE                                          
         CH    RE,TLDVLEN                                                       
         BNH   RDADV20                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO1           IF ADVICE IS ALREADY VERIFIED/                 
         MVI   ELCODE,TADVELQ    SENT/RECEIVED/PAID/COMPLETE                    
         BRAS  RE,GETEL          WE CANNOT MERGE                                
         BNE   RDADV20                                                          
         CLI   TADVSTAT,0                                                       
         BNE   RDADV20                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         SPACE 1                                                                
         USING TAFND,R4                                                         
         MVI   TGBYTE,0          SAVE ADVICE'S VERSION CODE IN TGBYTE           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   RDADV35                                                          
         L     R4,TGELEM                                                        
         MVC   TGBYTE,TAFNNAME                                                  
         SPACE 1                                                                
RDADV35  CLC   TGBYTE,SVVERS     VERSION CODE MUST MATCH                        
         BNE   RDADV20                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO1           IF ADVICE IS NOT FOR SAME USE                  
         MVI   ELCODE,TAVUELQ    WE CANNOT MERGE                                
         BRAS  RE,GETEL                                                         
         BNE   RDADV20                                                          
         CLC   TAVUUSE,TGUSEQU                                                  
         BNE   RDADV20                                                          
         SPACE 1                                                                
RDADV40  CLC   BUFFDATE,TAVUCYCS  IF BUFFER DATE FITS WITHIN                    
         BL    RDADV20            THE CYCLE RANGE WE WILL MERGE                 
         CLC   BUFFDATE,TAVUCYCE                                                
         BH    RDADV20                                                          
         SPACE 1                                                                
         MVC   STRTDATE,TAVUCYCS  SAVE CYCLE DATES                              
         MVC   ENDDATE,TAVUCYCE                                                 
         DROP  R4                                                               
         SPACE 1                                                                
         MVI   USAGE,C'N'                                                       
         MVI   ADDORCHG,C'C'                                                    
         XC    ELEMENT,ELEMENT                                                  
         SPACE 1                                                                
         L     R4,AIO2                                                          
         MVI   ELCODE,TANXELQ     SAVE TANX INFORMATION                         
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ELEMENT,0(R4)                                                    
         SPACE 1                                                                
         MVC   AIO,AIO2           AND LOAD ALREADY EXISTING ADVICE              
******** MVI   RDUPDATE,C'Y'      INTO AIO2                                     
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         OC    ELEMENT,ELEMENT                                                  
         BZ    RDADV50                                                          
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TANXELQ     IF NEW ADVICE DOES NOT HAVE                   
         BRAS  RE,GETEL           TANX ELEMENT                                  
         BE    RDADV50            ADD IT NOW                                    
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
RDADV50  GOTO1 ASAVPTRS,DMCB,SVPTRBLK                                           
         SPACE 1                                                                
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   RDADV190                                                         
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'GET ADVICE',AIO,0                       
         SPACE 1                                                                
RDADV190 MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO2                                                         
RDADVX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GIVE MESSAGE THAT CNET/MKT RECORD IS                  
*              MISSING FROM SYSTEM                                              
         USING PRNT3D,R2                                                        
PRTMISS  NTR1  BASE=*,LABEL=*                                                   
         USING TAXCD,R4                                                         
         LR    R4,R6                                                            
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PMISS10  BRAS  RE,NEXTEL                                                        
         BNE   PMISSX                                                           
         CLC   =C'INCLUDES',TAXCCMNT                                            
         BNE   PMISS10                                                          
         MVC   PRTWRN3,TAXCCMNT                                                 
         DROP  R2,R4                                                            
PMISSX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO CLEAR SORT RECORD                                     
         SPACE                                                                  
CLRSREC  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ASRTREC          RE=A(SORT RECORD)                            
         LH    RF,=AL2(MAXSLNQ2)   LENGTH OF LARGEST SORT RECORD                
         XCEFL                                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO CLEAR MARKET TABLE                                    
         SPACE                                                                  
CLRMKTB  NTR1  BASE=*,LABEL=*                                                   
         L     RE,AMKTTAB          RE=A(MARKET TABLE)                           
         LH    RF,=AL2(MAXMKTL)    LENGTH OF MARKET TABLE                       
         XCEFL                                                                  
         L     RE,AMKTTAB                                                       
         AH    RE,=AL2(MAXMKTL)                                                 
         SHI   RE,1                                                             
         MVI   0(RE),X'FF'         MARK END OF TABLE                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD PRODUCT NAME TO ADVICE RECORD                     
         SPACE                                                                  
CPYCPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1            SET TO COMMERCIAL IOAREA                     
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTPRD))                                     
         MVC   AIO,AIO2            RESET IOAREA TO ADVICE RECPRD                
         BNE   CPYCPRDX                                                         
         L     R4,TGELEM                                                        
         MVC   ELEMENT,0(R4)                                                    
         BRAS  RE,ELEMADD          ADD PRODUCT NAME TO ADVICE                   
CPYCPRDX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD TITLE NAME TO ADVICE RECORD                       
         SPACE                                                                  
CPYCTTL  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1            SET TO COMMERCIAL IOAREA                     
         XC    BLOCK(100),BLOCK    SET UP DUMMY FIELD HEADER                    
         MVI   BLOCK+5,L'TGNAME                                                 
         MVI   BLOCK,L'TGNAME+8                                                 
         GOTO1 CHAROUT,DMCB,TANAELQ,BLOCK                                       
*                                                                               
         MVC   AIO,AIO2            SET AIO TO ADVICE RECORD                     
         GOTO1 NAMIN,DMCB,TAFNELQ,BLOCK,TAFNTTTL                                
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SAVE COMMERCIAL INFORMATION                           
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
SVCOMED  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1             R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         XC    STACOINF(STACOLNQ),STACOINF                                      
         MVC   STACOMED,TACOMED    SAVE MEDIA                                   
         MVC   STACOTYP,TACOTYPE        MEDIA TYPE                              
         MVC   STACOSEC,TACOSEC         LENGTH IN SECONDS                       
         MVC   STACOEXP,TACOEXP         EXPIRATION DATE                         
         MVC   STACOCLG,TACOCLG         CLIENT GROUP                            
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
         CLI   SRTXTYPE,TANXTVER   IF MATCHED BY VERSION                        
         BE    *+12                                                             
         CLI   SRTXTYPE,TANXTALV   OR VERSION ALIAS                             
         BNE   SCMED60                                                          
         SPACE 1                                                                
         CLI   SRTVERS,26          IF VERSION CODE IS GREATER THAN              
         BNH   SCMED30             26                                           
         SPACE 1                                                                
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
SCMED10  CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   SRTVERS,VINDUPLM                                                 
         BNH   SCMED20                                                          
         LA    RE,VINDLNQ(RE)                                                   
         B     SCMED10                                                          
         SPACE 1                                                                
SCMED20  L     R4,AIO1                                                          
         SPACE 1                                                                
         XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   SCMED60                                                          
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE                                                                  
SCMED30  L     R4,AIO1                  TRY TO FIND VERSION ELEMENT             
         MVI   ELCODE,TAVRELQ           FOR THIS VERSION LETTER                 
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
SCMED40  BRAS  RE,NEXTEL                                                        
         BNE   SCMED50                                                          
         CLC   TAVRVERS,SRTVERS         IF FOUND                                
         BNE   SCMED40                                                          
         MVC   STACOLVI,TAVRCID         SAVE VERSION ID                         
         MVC   STACOLVL,TAVRSEC         AND VERSION LENGTH                      
         DROP  R4                                                               
         SPACE 1                                                                
SCMED50  BRAS  RE,RDCOM                 RE-READ COMM'L REC INTO AIO1            
         SPACE                                                                  
         USING TALFD,R4                                                         
SCMED60  CLI   SRTXTYPE,TANXTLFT        IF MATCHED BY LIFT                      
         BE    *+12                                                             
         CLI   SRTXTYPE,TANXTALF        OR ALIAS AS A LIFT                      
         BNE   SCMEDX                                                           
         MVC   STACOLVL,STACOSEC                                                
         MVC   STACOLVI,SRTTCID                                                 
         L     R4,AIO1                  TRY TO FIND LIFT ELEMENT                
         MVI   ELCODE,TALFELQ                                                   
         BRAS  RE,GETEL                 IF FOUND                                
         BNE   SCMED70                                                          
         MVC   STACOLVI,TALFLID         SAVE LIFT ID                            
         MVC   STACOLVL,TALFSEC         AND LIFT LENGTH                         
         B     SCMEDX                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
SCMED70  L     R4,AIO1                                                          
         MVI   ELCODE,TAVRELQ           IF LIFT ELEMENT NOT FOUND               
         BRAS  RE,GETEL                 LOOK FOR VERSION 2 ELEMENT              
         BNE   SCMEDX                                                           
         BRAS  RE,NEXTEL                                                        
         BNE   SCMEDX                                                           
         CLI   TAVRVERS,2                                                       
         BNE   SCMEDX                                                           
         MVC   STACOLVI,TAVRCID         IF FOUND, SAVE VERSION ID               
         MVC   STACOLVL,TAVRSEC         AND VERSION LENGTH                      
         DROP  R4                                                               
         SPACE 1                                                                
         MVI   SRTVERS,2                AND FIX THE SORT INFORMATION            
         MVI   SRTXTYPE,TANXTVER                                                
SCMEDX   XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD ADVICE DETAILS TO ADVICE RECORD                   
         SPACE                                                                  
ADDTADV  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TADVD,R3                                                         
         MVI   TADVEL,TADVELQ      SET ELEMENT CODE                             
         MVI   TADVLEN,TADVLNQ     SET LENGTH                                   
         MVI   TADVTYPE,TADVTYPR   REUSE                                        
         CLI   TGUSEQU,UCBL                                                     
         BE    ATADV10                                                          
         CLI   TGUSEQU,USCB                                                     
         BE    ATADV10                                                          
         CLI   TGUSEQU,UWSP                                                     
         BE    ATADV10                                                          
         CLI   TGUSEQU,USWS                                                     
         BE    ATADV10                                                          
         CLI   TGUSEQU,UWSC                                                     
         BE    ATADV10                                                          
         CLI   TGUSEQU,UADW                                                     
         BE    ATADV10                                                          
         CLI   TGUSEQU,ULCB                                                     
         BNE   ATADV20                                                          
ATADV10  MVI   TADVTYPE,TADVTYPB                                                
ATADV20  MVC   TADVOFF,AGYOFF      TALENT AGENCY OFFICE                         
         MVC   TADVSEC,STACOSEC    TALENT COMMERCIAL LENGTH IN SECONDS          
         MVC   TADVEXP,STACOEXP    TALENT COMMERCIAL EXPIRATION DATE            
         MVC   TADVCLI,TGCLI                                                    
         BRAS  RE,ELEMADD                                                       
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD ADVICE USE ELEMENT TO ADVICE RECORD               
         SPACE                                                                  
ADDTAVU  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAVUD,R4                                                         
         MVI   TAVUEL,TAVUELQ      SET ELEMENT CODE                             
         MVI   TAVULEN,TAVULNQ     SET LENGTH                                   
         MVC   TAVUMED,STACOMED    MEDIA                                        
         MVC   TAVUUSE,TGUSEQU                                                  
*                                                                               
ADDTAVU5 CLI   TAVUUSE,UCLA        IF CLA                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,UCLAREG    SET TYPE                                     
*                                                                               
         CLI   TAVUUSE,UCBL        IF CBL                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,UCBLREG    SET TYPE                                     
*                                                                               
         CLI   TAVUUSE,USCB        IF SCB                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,USCBREG    SET TYPE                                     
*                                                                               
         CLI   TAVUUSE,UWSP        IF WSP                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,UWSP13W    SET TYPE                                     
*                                                                               
         CLI   TAVUUSE,USWS        IF SWS                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,USWSREG    SET TYPE                                     
*                                                                               
         CLI   TAVUUSE,UWSC        IF WSC                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,UWSCREG    SET TYPE                                     
*                                                                               
         CLI   TAVUUSE,UADW        IF ADW                                       
         BNE   *+8                                                              
         MVI   TAVUTYPE,UADW13W    SET TYPE                                     
*                                                                               
         MVC   TAVUWKS,=C'13'      13 WEEK CYCLE                                
         BRAS  RE,ELEMADD                                                       
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD TACM ELEMENT                                      
         SPACE                                                                  
ADDTACM  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         USING TACMD,R4                                                         
         LA    R4,ELEM                                                          
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,4                                                        
         MVI   TACMTYPE,TACMTYPD                                                
         MVI   TACMCOMM,C'N'                NOBODY ON CAST                      
         TM    GNPAXCLA,GNPAXSTA+GNCLASTA                                       
         BNO   *+12                                                             
         MVI   TACMCOMM,C'B'                MIXED CAST                          
         B     ATACM10                                                          
         TM    GNPAXCLA,GNCLASTA                                                
         BZ    *+12                                                             
         MVI   TACMCOMM,C'C'                ALL PRE-00 CAST                     
         B     ATACM10                                                          
         TM    GNPAXCLA,GNPAXSTA                                                
         BZ    *+8                                                              
         MVI   TACMCOMM,C'X'                ALL 00 OR LATER CAST                
         DROP  R4                                                               
ATACM10  BRAS  RE,ELEMADD                                                       
ATACMX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TEST IF USE IS CABLE                                  
         SPACE 1                                                                
         USING TAVUD,R4                                                         
CBLTEST  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAVUUSE,UCBL                                                     
         BE    CBLYES                                                           
         CLI   TAVUUSE,USCB                                                     
         BNE   CBLNO                                                            
CBLYES   XR    RC,RC                                                            
CBLNO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TEST IF USE IS WILDSPOT                               
         SPACE 1                                                                
         USING TAVUD,R4                                                         
WSPTEST  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAVUUSE,UWSP                                                     
         BE    WSPYES                                                           
         CLI   TAVUUSE,USWS                                                     
         BE    WSPYES                                                           
         CLI   TAVUUSE,UWSC                                                     
         BE    WSPYES                                                           
         CLI   TAVUUSE,UADW                                                     
         BE    WSPYES                                                           
         CLI   TAVUUSE,URAD                                                     
         BNE   WSPNO                                                            
WSPYES   XR    RC,RC                                                            
WSPNO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TEST IF USE IS LOCAL CABLE                            
         SPACE 1                                                                
         USING TAVUD,R4                                                         
LCBTEST  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAVUUSE,ULCB                                                     
         BNE   LCBNO                                                            
LCBYES   XR    RC,RC                                                            
LCBNO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST CABLE AND WILDSPOT USES TO                     
*              CABLE,SPANISH OR ADDENDUM                                        
         SPACE 2                                                                
         USING SRTRECD,R3                                                       
ADJUSE   NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
         USING TACOD,R4                                                         
         L     R4,AIO              GET COMMERCIAL DETAILS ELEMENT               
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         CLC   SRTUSE,=C'CBL'                                                   
         BE    ADJUSE10                                                         
         CLC   SRTUSE,=C'WSP'                                                   
         BNE   ADJUSEX                                                          
         SPACE 1                                                                
         TM    TACOSTAT,TACOSCAN+TACOSCRT IF ADVICE IS WILDSPOT                 
         BNO   *+10                AND COMMERCIAL IS CANADIAN                   
         MVC   SRTUSE,=C'WSC'      MAKE USE CANADIAN WILDSPOT                   
         CLI   TACOTYPE,CTYSPAN                                                 
         BNE   *+10                AND COMMERCIAL IS SPANISH                    
         MVC   SRTUSE,=C'SWS'      MAKE USE SPANISH WILDSPOT                    
         CLI   TACOTYPE,CTYADD                                                  
         BNE   ADJUSEX             AND COMMERCIAL IS ADDENDUM                   
         MVC   SRTUSE,=C'ADW'      MAKE USE ADDENDUM WILDSPOT                   
         B     ADJUSEX                                                          
         SPACE 1                                                                
ADJUSE10 CLI   TACOTYPE,CTYSPAN    IF ADVICE IS CABLE                           
         BNE   ADJUSEX             AND COMMERCIAL IS SPANISH                    
         MVC   SRTUSE,=C'SCB'      MAKE USE SPANISH CABLE                       
         B     ADJUSEX                                                          
         DROP  R4                                                               
         SPACE 1                                                                
ADJUSEX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET INFO FROM USAGE HISTORY ELEMENT                   
         SPACE                                                                  
SETUSAGE NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAUHD,R4                                                         
         MVC   USENUM,TAUHUSN      USE NUMBER                                   
         MVC   STRTDATE,TAUHSTRT   CYCLE START                                  
         MVC   ENDDATE,TAUHEND     CYCLE END                                    
         MVI   USAGE,C'Y'          USAGE RECORD FOUND                           
         SPACE 1                                                                
         CLC   TGUSCDE,=C'CBL'     IF USE IS CABLE                              
         BE    SUSAGE10                                                         
         CLC   TGUSCDE,=C'SCB'     OR SPANISH CABLE                             
         BNE   SUSAGE20                                                         
SUSAGE10 MVC   PREVUNT,TAUHCBUN    SAVE PREVIOUS UNITS                          
         B     SUSAGEX                                                          
         SPACE 1                                                                
SUSAGE20 CLC   TGUSCDE,=C'LCB'     IF USE IS WILDSPOT                           
         BE    SUSAGE25                                                         
         MVC   PREVUNT,TAUHUNT     SAVE PREVIOUS UNITS                          
         MVC   PREVMAJ,TAUHMAJ     AND PREVIOUS MAJORS                          
         B     SUSAGEX                                                          
         SPACE 1                                                                
SUSAGE25 CLI   TAUHLEN,TAUHLNQ     IF USE IS LOCAL CABLE                        
         BL    SUSAGE30                                                         
         MVC   PREVSUB,TAUHSUBS    SAVE PREVIOUS SUBSCRIBERS                    
         MVC   PREVTYP,TAUHTYPE    AND PREVIOUS TYPE                            
         B     SUSAGEX                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLUHD,RE                                                         
SUSAGE30 L     RE,AIO              IS USE IS LOCAL CABLE                        
         SPACE 1                                                                
         USING TLIND,R4                                                         
         LA    R4,KEY              GO GET THE INVOICE RECORD                    
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,TGAGY                                                    
         MVC   TLININV,TLUHINV                                                  
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         BNE   SUSAGEX                                                          
         GOTO1 GETREC                                                           
         DROP  R4,RE                                                            
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO              FIND THE USE TYPE IN THE LOCAL CABLE         
         MVI   ELCODE,TAPDELQ      TABLE                                        
         BRAS  RE,GETEL                                                         
         BNE   SUSAGEX                                                          
         SPACE 1                                                                
         LA    RE,LCBLTAB2                                                      
SUSAGE40 CLI   0(RE),X'FF'         AND SAVE THE CORRECT NUMBER OF               
         BE    SUSAGEX             PREVIOUS SUBSCRIBERS                         
         CLC   TAPDTYPE,4(RE)                                                   
         BE    SUSAGE50                                                         
         LA    RE,L'LCBLTAB2(RE)                                                
         B     SUSAGE40                                                         
         SPACE 1                                                                
SUSAGE50 MVC   PREVSUB,0(RE)                                                    
         MVC   PREVTYP,4(RE)                                                    
SUSAGEX  XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         DC    0F                                                               
LCBLTAB2 DC    0XL8                                                             
         DC    F'50000',AL1(ULCB50)                                             
         DC    F'50000',AL1(ULCBU50)                                            
         DC    F'100000',AL1(ULCB100)                                           
         DC    F'100000',AL1(ULCBU100)                                          
         DC    F'100000',AL1(ULCBU12)                                           
         DC    F'150000',AL1(ULCB150)                                           
         DC    F'150000',AL1(ULCBU150)                                          
         DC    F'150000',AL1(ULCBU13)                                           
         DC    F'150000',AL1(ULCBU23)                                           
         DC    F'200000',AL1(ULCB200)                                           
         DC    F'200000',AL1(ULCBU200)                                          
         DC    F'200000',AL1(ULCBU14)                                           
         DC    F'200000',AL1(ULCBU24)                                           
         DC    F'200000',AL1(ULCBU34)                                           
         DC    F'250000',AL1(ULCB250)                                           
         DC    F'250000',AL1(ULCBU250)                                          
         DC    F'250000',AL1(ULCBU15)                                           
         DC    F'250000',AL1(ULCBU25)                                           
         DC    F'250000',AL1(ULCBU35)                                           
         DC    F'250000',AL1(ULCBU45)                                           
         DC    F'500000',AL1(ULCB500)                                           
         DC    F'500000',AL1(ULCBU500)                                          
         DC    F'500000',AL1(ULCBU16)                                           
         DC    F'500000',AL1(ULCBU26)                                           
         DC    F'500000',AL1(ULCBU36)                                           
         DC    F'500000',AL1(ULCBU46)                                           
         DC    F'500000',AL1(ULCBU56)                                           
         DC    F'750000',AL1(ULCB750)                                           
         DC    F'750000',AL1(ULCBU750)                                          
         DC    F'750000',AL1(ULCBU17)                                           
         DC    F'750000',AL1(ULCBU27)                                           
         DC    F'750000',AL1(ULCBU37)                                           
         DC    F'750000',AL1(ULCBU47)                                           
         DC    F'750000',AL1(ULCBU57)                                           
         DC    F'750000',AL1(ULCBU67)                                           
         DC    F'1000000',AL1(ULCB1M)                                           
         DC    F'1000000',AL1(ULCBU1M)                                          
         DC    F'1000000',AL1(ULCBU18)                                          
         DC    F'1000000',AL1(ULCBU28)                                          
         DC    F'1000000',AL1(ULCBU38)                                          
         DC    F'1000000',AL1(ULCBU48)                                          
         DC    F'1000000',AL1(ULCBU58)                                          
         DC    F'1000000',AL1(ULCBU68)                                          
         DC    F'1000000',AL1(ULCBU78)                                          
         DC    F'1000001',AL1(ULCBMAX)                                          
         DC    F'1000001',AL1(ULCBU19)                                          
         DC    F'1000001',AL1(ULCBU29)                                          
         DC    F'1000001',AL1(ULCBU39)                                          
         DC    F'1000001',AL1(ULCBU49)                                          
         DC    F'1000001',AL1(ULCBU59)                                          
         DC    F'1000001',AL1(ULCBU69)                                          
         DC    F'1000001',AL1(ULCBU79)                                          
         DC    F'1000001',AL1(ULCBU89)                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD CYCLE DATES TO ADVICE RECORD                      
         SPACE                                                                  
ADDCYC   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ADDED PREVIOUSLY                             
         USING TAVUD,R4                                                         
         MVC   TAVUCYCS,STRTDATE   SET CYCLE START                              
         MVC   TAVUCYCE,ENDDATE    SET CYCLE END                                
         SPACE 1                                                                
         CLI   TAVUUSE,UCLA        IF CLASS A USE                               
         BNE   ACYC10                                                           
         MVC   SVCYCLE,TAVUCYC     SAVE CYCLE                                   
         SPACE 1                                                                
ACYC10   CLI   TAVUUSE,UWSP        IF WILDSPOT USE                              
         BNE   ACYC20                                                           
         CLI   TAVUMED,C'R'        FOR RADIO                                    
         BNE   ACYC20                                                           
         GOTO1 DATCON,DMCB,(1,STRTDATE),(0,DUB)                                 
         GOTO1 ADDAY,DMCB,DUB,WORK,55                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   ENDDATE,TGDUB       AND END DATE IS 8 WEEKS                      
         BNE   ACYC30              FROM START DATE                              
         MVI   TAVUTYPE,UWSP8W     RESET TYPE                                   
         MVC   TAVUWKS,=C'8 '      AS 8 WEEKS                                   
         SPACE 1                                                                
ACYC20   CLI   TAVUUSE,UADW        IF ADDENDUM WILDSPOT                         
         BNE   ACYC30                                                           
         GOTO1 DATCON,DMCB,(1,STRTDATE),(0,DUB)                                 
         GOTO1 ADDAY,DMCB,DUB,WORK,2                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   ENDDATE,TGDUB       AND END DATE IS 3 DAYS                       
         BNE   ACYC21              FROM START DATE                              
         MVI   TAVUTYPE,UADW3D     RESET TYPE                                   
         XC    TAVUWKS,TAVUWKS     AS 3 DAYS                                    
         B     ACYC30                                                           
ACYC21   GOTO1 ADDAY,DMCB,DUB,WORK,6                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   ENDDATE,TGDUB       AND END DATE IS 7 DAYS                       
         BNE   ACYC22              FROM START DATE                              
         MVI   TAVUTYPE,UADW1W     RESET TYPE                                   
         XC    TAVUWKS,TAVUWKS     AS 1 WEEK                                    
         B     ACYC30                                                           
ACYC22   GOTO1 ADDAY,DMCB,DUB,WORK,27                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   ENDDATE,TGDUB       AND END DATE IS 28 DAYS                      
         BNE   ACYC23              FROM START DATE                              
         MVI   TAVUTYPE,UADW4W     RESET TYPE                                   
         MVC   TAVUWKS,=C'4 '      AS 4 WEEKS                                   
         B     ACYC30                                                           
ACYC23   GOTO1 ADDAY,DMCB,DUB,WORK,30                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   ENDDATE,TGDUB       AND END DATE IS 31 DAYS                      
         BNE   ACYC24              FROM START DATE                              
         MVI   TAVUTYPE,UADW31D    RESET TYPE                                   
         XC    TAVUWKS,TAVUWKS     AS 31 DAYS                                   
         B     ACYC30                                                           
ACYC24   MVI   TAVUTYPE,UADW13W    ELSE, DEFAULT TO 13 WEEKS                    
         MVC   TAVUWKS,=C'13'                                                   
         SPACE 1                                                                
ACYC30   CLI   USAGE,C'Y'          IF CYCLE CHANGED FOR USAGE                   
         BNE   ACYCX               HISTORY                                      
         CLI   TAVUUSE,UCBL        AND USE IS CABLE                             
         BE    ACYC40                                                           
         CLI   TAVUUSE,USCB        OR SPANISH CABLE                             
         BE    ACYC40                                                           
         CLI   TAVUUSE,USWS        OR SPANISH WILDSPOT                          
         BE    ACYC40                                                           
         CLI   TAVUUSE,UWSC        OR CANADIAN WILDSPOT                         
         BNE   ACYC50                                                           
ACYC40   MVI   TAVUTYPE,1          CHANGE TYPE TO UPGRADE                       
         B     ACYC70                                                           
ACYC50   CLI   TAVUUSE,UWSP        OR WILDSPOT                                  
         BNE   ACYC60                                                           
         ZIC   RE,TAVUTYPE                                                      
         AHI   RE,3                                                             
         STC   RE,TAVUTYPE         CHANGE TYPE TO UPGRADE                       
         B     ACYC70                                                           
ACYC60   CLI   TAVUUSE,UADW        OR ADDENDUM WILDSPOT                         
         BNE   ACYC65                                                           
         ZIC   RE,TAVUTYPE                                                      
         AHI   RE,1                                                             
         CHI   RE,UADWU31D                                                      
         BNL   *+8                                                              
         AHI   RE,3                                                             
         STC   RE,TAVUTYPE         CHANGE TYPE TO UPGRADE                       
         B     ACYC70                                                           
         SPACE 1                                                                
ACYC65   CLI   TAVUUSE,ULCB                                                     
         BNE   ACYCX                                                            
         SPACE 1                                                                
         USING TAUPD,R3                                                         
ACYC70   LA    R3,ELEMENT          AND ADD UPGRADE ELEMENT                      
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAUPEL,TAUPELQ                                                   
         MVI   TAUPLEN,TAUPLNQ                                                  
         SPACE 1                                                                
         CLI   TAVUUSE,UCBL        IF USE IS CABLE                              
         BE    ACYC80                                                           
         CLI   TAVUUSE,USCB        OR SPANISH CABLE                             
         BNE   ACYC90                                                           
ACYC80   MVC   TAUPICBU,PREVUNT    SAVE PREVIOUS UNITS                          
         B     ACYC110                                                          
         SPACE 1                                                                
ACYC90   CLI   TAVUUSE,ULCB         IF USE IS LOCAL CABLE                       
         BNE   ACYC100                                                          
         MVC   TAUPISUB,PREVSUB     SAVE PREVIOUS SUBSCRIBERS                   
         B     ACYC110                                                          
         SPACE 1                                                                
ACYC100  MVC   TAUPIUNT,PREVUNT     ELSE, SAVE PREVIOUS UNITS                   
         MVC   TAUPIMAJ,PREVMAJ     AND PREVIOUS MAJORS                         
         SPACE 1                                                                
ACYC110  BRAS  RE,ELEMADD                                                       
         DROP  R3,R4                                                            
ACYCX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CLEAR BUFFEL                                          
CLRBUFF  NTR1  BASE=*,LABEL=*                                                   
         XC    BUFFCNT,BUFFCNT     COUNT OF ELEMENTS IN BUFFER                  
*                                                                               
         LHI   R3,MAXEBUFF         MAXIMUM N'ENTRIES                            
         L     R2,ABUFFEL          R2=A(BUFFER OF TANPD ELEMENTS)               
         ST    R2,ANXTBUFF         SAVE A(NEXT POSITION IN BUFFER)              
         USING BUFFD,R2                                                         
*                                                                               
CLRBUFF5 XC    0(BUFFLNQ,R2),0(R2) CLEAR ENTRY                                  
         LA    R2,BUFFLNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R3,CLRBUFF5         LOOP                                         
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SAVE CURRENT SORT RECORD VALUES                       
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
SETVALS  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVSRTKEY,SRTKEY     SAVE CODES FROM KEY                          
         MVC   SVCLIN,SRTCLIN      SAVE CLIENT NAME                             
         MVC   SVPRDN,SRTPRDN      SAVE PRODUCT NAME                            
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO TRACE THEN PUT TO SORT                                
         SPACE                                                                  
PUTSORT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,TRCPSRT          TRACE PUT TO SORT                            
         TM    WHEN,X'20'          IF OVERNIGHT PROCESSING                      
         BO    PUTSORTX                                                         
******** TM    WHEN,X'40'          IF OVERNIGHT PROCESSING                      
******** BO    PUTSORTX                                                         
         GOTO1 SORTER,DMCB,=C'PUT',ASRTREC                                      
PUTSORTX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TRACE PUT TO SORT                                     
         SPACE                                                                  
TRCPSRT  NTR1  BASE=*,LABEL=*                                                   
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   TRCPSRTX                                                         
         L     R3,ASRTREC                                                       
         USING SRTRECD,R3                                                       
         LH    R2,SRTLEN                                                        
         AHI   R2,-4                                                            
         GOTO1 =A(MYTRACE),DMCB,(RC),=C'PUT TO SORT',4(R3),(R2)                 
TRCPSRTX XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TRACE GET FROM SORT                                   
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
TRCSGET  NTR1  BASE=*,LABEL=*                                                   
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   TRCSGETX                                                         
         LH    R2,SRTLEN                                                        
         AHI   R2,-4                                                            
         GOTO1 =A(MYTRACE),DMCB,(RC),=C'GET FROM SORT',4(R3),(R2)               
TRCSGETX XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT START LOGO PAGES                                
         SPACE                                                                  
PRTSLOGO NTR1  BASE=*,LABEL=*                                                   
******** TM    WHEN,X'40'          IF OVERNIGHT PROCESSING                      
******** BO    PRTSLOGX                                                         
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
*                                                                               
         L     R2,NLOGOC           R2=A(LOGOC)                                  
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'S'       START LOGOS                                  
         MVC   LOGO2,SPACES        SET AGENCY CODE                              
         MVC   LOGO2(L'TGAGY),TGAGY                                             
         MVC   LOGONAME,AGYNAME    AND AGENCY NAME                              
         MVC   LOGOADD,AGYADDR1    ADDRESS                                      
         MVC   LOGOADD2,AGYADDR2                                                
         MVC   LOGOADD3,AGYADDR3                                                
         GOTO1 NLOGO,DMCB,(R2)                                                  
         MVC   PAGE,=H'1'          START FROM PAGE 1                            
PRTSLOGX XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
*              ROUTINE TO PRINT END LOGO PAGES                                  
         SPACE                                                                  
PRTELOGO NTR1  BASE=*,LABEL=*                                                   
         L     R2,NLOGOC           R2=A(LOGOC)                                  
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'       END LOGOS                                    
         GOTO1 NLOGO,DMCB,(R2)                                                  
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ADD VERSION ELEMENT                                              
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
ADDVERS  NTR1  BASE=*,LABEL=*                                                   
         CLI   SRTVERS,0           IF ADVICE IS FOR A VERSION                   
         BE    AVX                                                              
*                                                                               
         USING TAFND,R4                                                         
         LA    R4,ELEMENT          ADD VERSION ELEMENT                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,4                                                        
         MVI   TAFNTYPE,TAFNTVER                                                
         MVC   TAFNNAME,SRTVERS                                                 
         BRAS  RE,ELEMADD                                                       
AVX      XIT1                                                                   
         DROP  R3,R4                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE THE KEY FOR NOW PROCESSING                              
         SPACE                                                                  
VKSOON   NTR1  BASE=*,LABEL=*                                                   
*                                  R2=A(AGENCY FIELD)                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SAGAGYH),SAGAGYNH                     
         MVC   PROCAGY,TGAGY       SET AGENCY CODE                              
*                                                                               
         LA    R2,SAGNIDH          R2=A(NETWORK ID FIELD)                       
         GOTO1 ANY                                                              
         MVC   TGNID,WORK                                                       
         MVC   TGADID,WORK                                                      
         CLI   TGADID+8,C' '       IF MORE THAN 8 CHARACTERS,                   
         BNH   VKSOON05                                                         
         XC    TGNID,TGNID                                                      
         GOTO1 ATRPACK,DMCB,(C'P',TGADID),TGNID  PACK IT                        
         MVI   ADIDFLG,C'Y'                                                     
*                                                                               
VKSOON05 LA    R2,SAGCLIH          R2=A(CLIENT FIELD)                           
         XC    TGNCLI,TGNCLI                                                    
         CLI   5(R2),0             IF INPUT                                     
         BE    VKSOON10                                                         
         GOTO1 ANY                                                              
         MVC   TGNCLI,WORK         SET GLOBAS NETWORK CLIENT                    
*                                                                               
VKSOON10 LA    R2,SAGPRDH          R2=A(PRODUCT FIELD)                          
         XC    TGNPRD,TGNPRD                                                    
         CLI   5(R2),0             IF INPUT                                     
         BE    VKSOON20                                                         
         GOTO1 ANY                                                              
         MVC   TGNPRD,WORK         SET GLOBAS NETWORK PRODUCT                   
*                                                                               
VKSOON20 LA    R2,SAGMEDH          R2=A(MEDIA FIELD)                            
         XC    TGMENAME,TGMENAME                                                
         CLI   5(R2),0                                                          
         BE    VKSOON22                                                         
         GOTO1 MEDVAL,DMCB,8(R2)                                                
         BNE   VINVERR                                                          
*                                                                               
VKSOON22 LA    R2,SAGUSEH          R2=A(USE FIELD)                              
         XC    TGUSCDE,TGUSCDE                                                  
         CLI   5(R2),0                                                          
         BE    VKSOON24                                                         
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   VINVERR                                                          
*                                                                               
VKSOON24 LA    R2,SAGADDH          R2=A(DATE ADDED FIELD)                       
         XC    TGDATE,TGDATE                                                    
         CLI   5(R2),0             IF INPUT                                     
         BE    VKSOON30                                                         
         GOTO1 DTVAL,DMCB,TGDATE   SET GLOBAS DATE                              
*                                                                               
VKSOON30 LA    R2,SAGUIDH          R2=A(USER ID FIELD)                          
         XC    TGUSER,TGUSER                                                    
         CLI   5(R2),0                                                          
         BE    VKSOON40                                                         
******** GOTO1 USERVAL,DMCB,(R2)   SET GLOBAS USERID                            
*                                                                               
VKSOON40 MVC   SVDATE,TGDATE       SAVE UNCOMPLEMENTED DATE                     
         OC    TGDATE,TGDATE                                                    
         BZ    *+10                                                             
         XC    TGDATE,XFFS         COMPLMENT FOR READ                           
         SPACE 1                                                                
         MVI   TGTYPE,0                                                         
         GOTO1 RECVAL,DMCB,TLNXCDQ,0                                            
         MVC   TGDATE,SVDATE       RESET UNCOMPLEMENTED DATE                    
         LA    R4,KEY                                                           
         USING TLNXD,R4                                                         
         B     VKSOON60                                                         
*                                                                               
VKSOON50 GOTO1 SEQ                                                              
*                                                                               
VKSOON60 CLC   KEY(TLNXNCLI-TLNXKEY),KEYSAVE  SAME AGENCY/NID                   
         BNE   VNTFND                                                           
         OC    TGNCLI,TGNCLI       IF GLOBAS CLIENT                             
         BZ    *+14                                                             
         CLC   TLNXNCLI,TGNCLI     MATCH ON IT                                  
         BNE   VKSOON50                                                         
         OC    TGNPRD,TGNPRD       IF GLOBAS PRODCUT                            
         BZ    *+14                                                             
         CLC   TLNXNPRD,TGNPRD     MATCH ON IT                                  
         BNE   VKSOON50                                                         
         OC    TGMENAME,TGMENAME   IF GLOBAS MEDIA                              
         BZ    *+14                                                             
         CLC   TLNXMED,TGMENAME    MATCH ON IT                                  
         BNE   VKSOON50                                                         
         OC    TGUSCDE,TGUSCDE     IF GLOBAS USE CODE                           
         BZ    *+14                                                             
         CLC   TLNXUSE,TGUSCDE     MATCH ON IT                                  
         BNE   VKSOON50                                                         
         OC    TGDATE,TGDATE       IF GLOBAS DATE ADDED                         
         BZ    VKSOON70                                                         
         MVC   SVDATE,TGDATE       COMPLEMENT DATE FOR MATCH                    
         XC    SVDATE,XFFS                                                      
         CLC   TLNXDATE,SVDATE     MATCH ON IT                                  
         BNE   VKSOON50                                                         
VKSOON70 OC    TGUSER,TGUSER       IF GLOBAS USER ID NUMBER                     
         BZ    *+14                                                             
         CLC   TLNXUID,TGUSER      MATCH ON IT                                  
         BNE   VKSOON50                                                         
         TM    KEY+TLDRSTAT-TLDRD,TLNXSDEL                                      
         BO    VKSOON50                                                         
         TM    KEY+TLDRSTAT-TLDRD,TLNXSUSD                                      
         BO    VKSOON50                                                         
         TM    KEY+TLDRSTAT-TLDRD,TLNXSMAT                                      
         BZ    VKSOON50                                                         
         GOTO1 GETREC                                                           
         BRAS  RE,DSPNAMES  DISPLAY NAMES                                       
         XIT1                                                                   
         DROP  R4                 AND EXIT                                      
         SPACE 2                                                                
VNTFND   MVI   ERROR,NOTFOUND                                                   
         LA    R2,SAGAGYH                                                       
         B     *+8                                                              
         SPACE 1                                                                
VINVERR  MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 2                                                                
XFFS     DC    3X'FF'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RESET LATE NIGHT ADVICE TO CLASS A                    
RESETCLA NTR1  BASE=*,LABEL=*                                                   
         USING TAVUD,R4                                                         
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL            R4=A(ADVICE USE DETAILS ELEMENT)             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   TAVUUSE,UCLA        SET USE AS CLASS A                           
         MVC   TAVUCYC,SVCYCLE     AND RESTORE CYCLE                            
         MVC   STRTDATE(6),SVCYCLE                                              
         DROP  R4                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD LATE NIGHT ADVICES                                
ADDLNITE NTR1  BASE=*,LABEL=*                                                   
         OC    LNSTATUS,LNSTATUS   IF ROUTINE WAS LAST EXITED ONLY              
         BNZ   ALN35               TO PRINT, GO BACK TO POINT OF EXIT           
         SPACE 1                                                                
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,X'CC'                                                     
         BRAS  RE,GETEL            EXIT IF NO 'CC' ELEMENTS EXIST               
         BNE   ALNX                                                             
         SPACE 1                                                                
         USING NWKTABD,R3                                                       
         LA    R3,NWKTAB           R3=A(NETWORKS TABLE)                         
         MVI   SEQCNT,0            INITIALIZE ADVICE SEQUENCE COUNTER           
         SPACE 1                                                                
ALN10    MVI   ELCODE,TANPELQ      DELETE ALL PROGRAM ELEMENTS                  
         GOTO1 REMELEM             FROM ADVICE                                  
         SPACE 1                                                                
         USING TANPD,R4                                                         
ALN15    L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,X'CC'                                                     
         BRAS  RE,GETEL            R4=A(LATE NIGHT PROGRAM ELEMENT)             
         B     *+8                                                              
ALN20    BRAS  RE,NEXTEL           IF LATE NITE PROGRAM ELEMENT'S               
         BNE   ALN30               NETWORK DOES NOT MATCH CURRENT               
         CLC   TANPNWK,NTLETTER    NWKTAB ENTRY, SKIP IT                        
         BNE   ALN20                                                            
         SPACE 1                                                                
         CLC   TGCOM,NTSVCOM       IF THIS COMMERCIAL HAS NOT BEEN              
         BE    ALN20AA             PROCESSED FOR THIS NETWORK                   
         XC    NTUSE#,NTUSE#       INITIALIZE NUMBER OF USES                    
         XC    NTCYCDTS,NTCYCDTS   AND CLEAR CYCLE DATES                        
         MVI   LSTMONTH,0          AND CLEAR LAST PROGRAM MONTH                 
         SPACE 1                                                                
ALN20AA  CLC   TANPDATE,NTCYCDTS   IF USE DATE DOESN'T FIT WITHIN               
         BL    ALN20A              PREVIOUS CYCLE                               
         CLC   TANPDATE,NTCYCDTS+3                                              
         BNH   ALN20B                                                           
ALN20A   OC    NTUSE#,NTUSE#       AND CYCLE DATES WERE SET FOR THIS            
         BNZ   ALN20               COMMERCIAL,SKIP PROGRAM                      
         SPACE 1                                                                
ALN20B   CLI   LSTMONTH,0          IF LAST PROGRAM MONTH IS SET                 
         BE    ALN20C                                                           
         CLC   LSTMONTH,TANPDATE+1 IT MUST MATCH CURRENT PROGRAM                
         BE    ALN20C                                                           
         OI    STATUS,NEWMONTH                                                  
         BNE   ALN20               MONTH OR SKIP PROGRAM                        
         SPACE 1                                                                
ALN20C   MVI   TANPEL,TANPELQ      CHANGE CC TO TANP ELEMENT                    
         MVC   NTSVCOM,TGCOM       AND SAVE COMMERCIAL ID                       
         MVC   LSTMONTH,TANPDATE+1 AND SAVE PROGRAM MONTH                       
         SPACE 1                                                                
         OC    NTUSE#,NTUSE#       IF CYCLE HISTORY ALREADY DETERMINED          
         BNZ   ALN24               SKIP AHEAD                                   
         SPACE 1                                                                
         USING BUFFD,R2                                                         
         L     R2,ABUFFEL          SET CYCLE DATES FOR THIS ADVICE              
         MVC   BUFFDATE,TANPDATE   BASED (INITIALLY) ON THIS PROGRAM'S          
         BRAS  RE,STNEWRNG         AIR DATE                                     
         MVC   NTCYCDTS,STRTDATE                                                
         DROP  R2,R4                                                            
         SPACE 1                                                                
         MVI   USAGE,C'N'                                                       
         XC    KEY,KEY             BUILD USAGE HISTORY KEY                      
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ      USAGE HISTORY RECORD CODE                    
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL CODE                     
         MVC   TLUHUSE,NTUSEDES    LATE NIGHT USE CODE                          
         GOTO1 HIGH                                                             
         B     ALN22                                                            
ALN21    GOTO1 SEQ                                                              
ALN22    CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BNE   ALN24                                                            
         MVC   SVUHKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              READ USAGE HISTORY RECORD                    
         SPACE 1                                                                
         L     R4,AIO              R4=A(USAGE HISTORY RECORD)                   
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL            R4=A(USAGE HISTORY ELEMENT)                  
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAUHD,R4                                                         
         L     R2,ABUFFEL                                                       
         USING BUFFD,R2                                                         
         CLC   BUFFDATE,TAUHSTRT   IF PROGRAM FITS WITHIN THIS                  
         BL    ALN21               EXISTING CYCLE                               
         CLC   BUFFDATE,TAUHEND                                                 
         BH    ALN21                                                            
         MVC   NTUSE#,TAUHUSN      SAVE LAST USE NUMBER AND                     
         MVC   NTCYCDTS,TAUHSTRT   RESET THE ADVICE CYCLE DATES                 
         MVI   USAGE,C'Y'                                                       
         DROP  R2,R4                                                            
         SPACE 1                                                                
ALN24    LH    RE,NTUSE#           BUMP USE NUMBER                              
         AHI   RE,1                                                             
         STH   RE,NTUSE#                                                        
         SPACE 1                                                                
         ZIC   RE,SEQCNT           BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         STC   RE,SEQCNT                                                        
         SPACE 1                                                                
         MVC   AIO,AIO2            RESET AIO TO ADVICE                          
         L     R4,AIO                                                           
         USING TANPD,R4                                                         
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL            R4=A(LATE NIGHT PROGRAM ELEMENT)             
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   SEQCNT,1                                                         
         BE    ALN26                                                            
         ZIC   R2,SEQCNT                                                        
         AHI   R2,-1                                                            
ALN25    BRAS  RE,NEXTEL                                                        
         BCT   R2,ALN25                                                         
         SPACE 1                                                                
ALN26    MVC   TANPSEQ,SEQCNT      PUT IN SEQUENCE NUMBER                       
         MVC   TANPUSEN,NTUSE#     AND USE NUMBER                               
         B     ALN15                                                            
         DROP  R4                                                               
         SPACE 1                                                                
ALN30    L     R4,AIO2             IF NO ELEMENTS NEED TO BE ADDED              
         MVI   ELCODE,TANPELQ      FOR THIS NETWORK, DO NOT ADD ADVICE          
         BRAS  RE,GETEL                                                         
         BNE   ALN40                                                            
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL            R4=A(ADVICE USE DETAILS ELEMENT)             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TAVUUSE,NTUSEEQU    INSERT CURRENT USE                           
         MVC   TAVUCYC,NTCYCDTS    AND CYCLE DATES FROM TABLE                   
         DROP  R4                                                               
         SPACE 1                                                                
         BRAS  RE,GETADV           GET NUMBER FOR THIS ADVICE                   
         L     R4,AIO                                                           
         USING TLDVD,R4                                                         
         MVC   TLDVADV,TGADV                                                    
         DROP  R4                                                               
         BRAS  RE,ADDIT            ADD                                          
         SPACE 1                                                                
         MVC   TGUSCDE,NTUSEDES                                                 
         ST    R3,LNSTATUS                                                      
         MVI   CNTTANP,0                                                        
         MVC   CNTTANP+1(1),SEQCNT                                              
         B     ALNX                AND GO PRINT IT                              
         SPACE 1                                                                
ALN35    MVC   TGUSCDE,=C'CLA'                                                  
         L     R3,LNSTATUS                                                      
         SPACE 1                                                                
         MVI   SEQCNT,0            RESET SEQUENCE COUNTER                       
         SPACE 1                                                                
         USING TANPD,R4                                                         
ALN40    L     R4,AIO2             R4=A(JUST ADDED ADVICE RECORD)               
         MVI   ELCODE,X'CC'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
ALN50    BRAS  RE,NEXTEL           IF CURRENT NETWORK STILL HAS MORE            
         BNE   ALN60               ELEMENTS (IN A DIFFERENT CYCLE)              
         CLC   TANPNWK,NTLETTER    RESET THE NUMBER OF USES AND GO              
         BNE   ALN50               PROCESS THEM                                 
         TM    STATUS,NEWMONTH                                                  
         BO    *+10                                                             
         XC    NTUSE#,NTUSE#                                                    
         NI    STATUS,X'FF'-NEWMONTH                                            
         MVI   LSTMONTH,0                                                       
         B     ALN10                                                            
         DROP  R4                                                               
         SPACE 1                                                                
ALN60    LA    R3,L'NWKTAB(R3)     BUMP UP NETWORK TABLE                        
         CLI   0(R3),X'FF'         AND REPEAT PROCESS FOR EACH ENTRY            
         BNE   ALN10                                                            
         XC    LNSTATUS,LNSTATUS                                                
         B     ALNX                                                             
         DROP  R3                                                               
         SPACE 1                                                                
NWKTAB   DC    0XL18                                                            
         DC    C'A',AL1(ULNA),CL3'LNA',X'00',H'00',6X'00',4X'00'                
         DC    C'C',AL1(ULNC),CL3'LNC',X'00',H'00',6X'00',4X'00'                
         DC    C'F',AL1(ULNF),CL3'LNF',X'00',H'00',6X'00',4X'00'                
         DC    C'N',AL1(ULNN),CL3'LNN',X'00',H'00',6X'00',4X'00'                
         DC    X'FF'                                                            
ALNX     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO ADD ADVICE RECORD                                     
         SPACE                                                                  
ADDIT    NTR1  BASE=*,LABEL=*                                                   
         MVI   TGMTTYPE,0                                                       
         NI    STATUS,X'FF'-ADDNOFEL                                            
         BRAS  RE,CVTMKT           CONVERT MARKET ELEMENTS TO TAMT              
*                                                                               
         CLI   TGMTTYPE,TANPNET    IF USE IS CABLE OR SPANISH CABLE             
         BNE   ADDIT05                                                          
         MVI   TGMTTYPE,C'S'                                                    
         BRAS  RE,CVTMKT           CONVERT CABLE SYSTEMS TOO                    
*                                                                               
ADDIT05  NI    STATUS,X'FF'-ADDNOFEL                                            
*                                                                               
         BRAS  RE,CVTRAD           CONVERT WSC ADVICES TO RAD                   
         BRAS  RE,LCBTYPE          DETERMINE LCB TYPE                           
         BRAS  RE,ADDDUES          BUILD ADVICE DUE DATE ELEMENT                
*                                                                               
         CLI   ADDORCHG,C'A'                                                    
         BNE   ADDIT30                                                          
*                                                                               
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   ADDIT10                                                          
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'ADD ADVICE',AIO,0                       
*                                                                               
ADDIT10  L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ      ENSURE ADVICE RECORD                         
         BRAS  RE,GETEL            HAS NETWORKS/MARKETS/CABLE SYSTEMS           
         BE    ADDIT20             ON IT                                        
         L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   ADDITX                                                           
ADDIT20  GOTO1 ADDREC              ADD RECORD                                   
         XC    SVPTRBLK(255),SVPTRBLK                                           
         B     ADDIT50                                                          
*                                                                               
ADDIT30  XC    KEY,KEY             AVOID PUTREC DRAMA                           
         L     RE,AIO2                                                          
         MVC   KEY(32),0(RE)                                                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLDVKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   ADDIT40                                                          
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'CHG ADVICE',AIO,0                       
ADDIT40  GOTO1 PUTREC              ADD RECORD                                   
         SPACE 1                                                                
ADDIT50  MVI   BYTE,8              UPDATE POINTERS                              
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   *+8                                                              
         OI    BYTE,X'10'          SET TRACE FOR ADDPTRS                        
         GOTO1 AADDPTRS,DMCB,(BYTE,SVPTRBLK),UPPTRBLK                           
*                                                                               
         L     R3,AMKTTAB                                                       
         CLI   0(R3),0              ANY MARKET NAMES IN MARKET TABLE?           
         BE    ADDITX                                                           
         BRAS  RE,PUTMKT            PUT MARKETS ONTO ADVICE DETAILS REC         
*                                                                               
ADDITX   XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              PUT MARKET NAMES ONTO ADVICE DETAILS COMMENT RECORD              
         SPACE 1                                                                
PUTMKT   NTR1  BASE=*,LABEL=*                                                   
         MVI   MYSTAT,0                                                         
         USING MKTTABD,R3                                                       
         L     R3,AMKTTAB                                                       
*                                                                               
         TM    NOPTION,NTRACE       IF TRACE ON                                 
         BNO   PMKT10                                                           
         GOTO1 HEXOUT,DMCB,(R3),P,200,=C'TOG'                                   
         GOTO1 SPOOL,DMCB,(R8)      PRINT MARKET TABLE                          
*                                                                               
PMKT10   CLI   0(R3),X'FF'          END OF MARKET NAME TABLE?                   
         BE    PMKT80                                                           
         CLI   0(R3),0              NO MORE MARKET NAMES?                       
         BE    PMKT80                                                           
*                                                                               
         CLI   MYSTAT,0             DO WE HAVE THE COMMENT IN AIO?              
         BNE   PMKT30                                                           
         L     RE,AIO2              ADVICE RECORD                               
         USING TLDVD,RE                                                         
         MVC   AIO,AIO1             READ ADVICE DETAILS COMMENT RECORD          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCMD,R4                                                         
         MVI   TLCMCD,TLCMCDQ                                                   
         MVC   TLCMAGY,TLDVAGY      AGENCY                                      
         MVI   TLCMTYP,TLCMTADV     COMMENT TYPE                                
         MVC   TLCMVCID,TLDVCID     COMM ID                                     
         MVC   TLCMVADV,TLDVADV     ADVICE NUMBER                               
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BE    PMKT20                                                           
         L     R4,AIO               IF COMMENT NOT FOUND, INIT NEW REC          
         XC    0(255,R4),0(R4)                                                  
         MVC   0(L'TLCMKEY,R4),KEYSAVE                                          
         MVI   MYSTAT,C'N'          NO, COMMENT REC DOES NOT EXIST YET          
         LHI   R5,1                 SEQUENCE NUMBER                             
         B     PMKT60                                                           
*                                                                               
PMKT20   MVI   MYSTAT,C'Y'          YES, COMMENT REC ALREADY EXISTS             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         TM    NOPTION,NTRACE       IF TRACE ON                                 
         BNO   PMKT30                                                           
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'GET CMNT',AIO,0                         
*                                                                               
PMKT30   LHI   R5,0                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PMKT40   BRAS  RE,NEXTEL                                                        
         BNE   PMKT50                                                           
         USING TAXCD,R4                                                         
         ZIC   R5,TAXCSEQ                                                       
         CLC   MKTCOD(MKTTBLNQ),TAXCCMNT    IF WE HAVE DUPLICATE, SKIP          
         BE    PMKT70                                                           
         B     PMKT40                                                           
         DROP  R4                                                               
*                                                                               
PMKT50   AHI   R5,1      INCREMENT SEQUENCE                                     
         CHI   R5,16     CANNOT HAVE MORE THAN 16 COMMENTS                      
         BH    PMKT80                                                           
*                                                                               
         USING TAXCD,R4                                                         
PMKT60   LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAXCEL,TAXCELQ       ELEMENT CODE                                
         LHI   RE,MKTTBLNQ                                                      
         AHI   RE,4                                                             
         STC   RE,TAXCLEN           ELEMENT LENGTH                              
         STC   R5,TAXCSEQ           SEQUENCE NUMBER                             
         MVC   TAXCCMNT(MKTTBLNQ),MKTCOD  MARKET CODE AND NAME                  
         DROP  R4                                                               
         GOTO1 ADDELEM              ADD ELEMENT TO RECORD                       
PMKT70   LA    R3,MKTTBLNQ(R3)                                                  
         B     PMKT10                                                           
*                                                                               
PMKT80   CLI   MYSTAT,C'Y'          IF COMMENT RECORD ALREADY EXISTS,           
         BNE   PMKT90                                                           
         TM    NOPTION,NTRACE       IF TRACE ON                                 
         BNO   PMKT85                                                           
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'CHG CMNT',AIO,0                         
PMKT85   GOTO1 PUTREC               PUT IT                                      
         B     PMKTX                                                            
*                                                                               
PMKT90   CLI   MYSTAT,C'N'          IF COMMENT RECORD DOES NOT EXIST,           
         BNE   PMKTX                                                            
         TM    NOPTION,NTRACE       IF TRACE ON                                 
         BNO   PMKT95                                                           
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'ADD CMNT',AIO,0                         
PMKT95   GOTO1 ADDREC               ADD RECORD                                  
*                                                                               
PMKTX    MVC   AIO,AIO2                                                         
         BRAS  RE,CLRMKTB                                                       
         XIT1                                                                   
         DROP  R3                                                               
MYSTAT   DS    XL1                                                              
         LTORG                                                                  
         EJECT                                                                  
*              CONVERT MARKET ELEMENTS INTO TAMT ELEMENTS                       
         SPACE 1                                                                
CVTMKT   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2                                                          
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CMX                                                              
         SPACE 1                                                                
         USING TAXCD,R3                                                         
         LA    R3,ELEMENT        PREPARE TO BUILD COMMENT ELEMENT               
         BRAS  RE,SETSEQ         SET SEQUENCE NUMBER                            
         SPACE 1                                                                
         MVI   GETALPH,C'N'      INITIALIZE GETALPH TO N                        
         XC    LASTCODE,LASTCODE                                                
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO2           GET ADVICE USE DETAILS ELEMENT                 
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CMX                                                              
         SPACE 1                                                                
         CLI   TGMTTYPE,0                                                       
         BE    CM05                                                             
         MVI   TGMTTYPE,C'S'                                                    
         MVC   TAXCCMNT(L'MISSCSYS),MISSCSYS                                    
         B     CM20                                                             
         SPACE 1                                                                
CM05     MVI   TGMTTYPE,TANPNET                                                 
         MVC   TAXCCMNT(L'MISSCNET),MISSCNET                                    
         CLI   TAVUUSE,UCBL                                                     
         BE    CM20                                                             
         CLI   TAVUUSE,USCB                                                     
         BE    CM20                                                             
         SPACE 1                                                                
         MVI   TGMTTYPE,C'S'                                                    
         MVC   TAXCCMNT(L'MISSCSYS),MISSCSYS                                    
         CLI   TAVUUSE,ULCB                                                     
         BE    CM20                                                             
         SPACE 1                                                                
         MVC   TGMTTYPE,TAVUMED                                                 
         MVC   TAXCCMNT(L'MISSMKT),MISSMKT                                      
         MVC   TAXCCMNT+9(1),TAVUMED                                            
         CLI   TAVUMED,C'R'      IF MEDIA IS NOT R,                             
         BE    *+8                                                              
         MVI   TAXCCMNT+9,C'T'   MEDIA IN COMMENT SHOULD BE T                   
         SPACE 1                                                                
         CLI   TAVUMED,C'T'      IF ADVICE IS FOR A TELEVISION                  
         BNE   CM20              COMMERCIAL                                     
         CLI   TAVUUSE,UWSP      AND USE IS A WILDSPOT                          
         BE    CM10                                                             
         CLI   TAVUUSE,USWS                                                     
         BE    CM10                                                             
         CLI   TAVUUSE,UWSC                                                     
         BE    CM10                                                             
         CLI   TAVUUSE,UADW                                                     
         BNE   CM20                                                             
CM10     MVI   GETALPH,C'Y'      SET TO GET ALPHA CODES                         
         DROP  R4                                                               
         SPACE 1                                                                
CM20     MVI   TAXCLEN,4+L'MISSMKT             R2=BEGINNING OF                  
         LA    R2,4+L'MISSMKT(R3)                 COMMENT IN EL                 
         SPACE 1                                                                
         USING TAMTD,R4                                                         
         L     R4,AIO2           GET EACH MARKET ELEMENT                        
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CM30     BRAS  RE,NEXTEL                                                        
         BNE   CM140                                                            
         SPACE 1                                                                
         CLI   TGMTTYPE,TANPNET IF READING CABLE NETWORKS                       
         BNE   CM40                                                             
         CLI   TAMTCODE,C'0'    SKIP THE CABLE SYSTEMS                          
         BL    CM40                                                             
         CLI   TAMTCODE,C'9'                                                    
         BNH   CM30                                                             
         SPACE 1                                                                
CM40     CLI   TGMTTYPE,C'S'    IF READING CABLE SYSTEMS                        
         BNE   CM50                                                             
         CLI   TAMTCODE,C'A'    SKIP THE CABLE NETWORKS                         
         BL    CM50                                                             
         CLI   TAMTCODE,C'Z'                                                    
         BNH   CM30                                                             
         SPACE 1                                                                
CM50     MVC   AIO,AIO1                               IF MARKET CODE            
         LHI   RF,TLMTCDQ                                                       
         CLI   GETALPH,C'Y'                                                     
         BNE   CM60                                                             
         CLI   TAMTCODE,C'A'                                                    
         BL    CM60                                                             
         CLI   TAMTCODE,C'Z'                                                    
         BH    CM60                                                             
         LHI   RF,TLMTALDQ                                                      
CM60     GOTO1 RECVAL,DMCB,(RF),(X'A4',TAMTCODE)      EXISTS                    
         MVC   AIO,AIO2                                                         
         BNE   CM100                                                            
         CLI   GETALPH,C'Y'                                                     
         BNE   CM30                                                             
         CLI   TAMTCODE,C'A'                                                    
         BL    CM70                                                             
         CLI   TAMTCODE,C'Z'                          AND WE WANT TO            
         BNH   CM30                                   GET ALPHA CODE            
         DROP  R4                                                               
         SPACE 1                                                                
         USING TASND,R4                                                         
CM70     LR    R1,R4             SAVE LOCATION OF TAMT ELEMENT                  
         L     R4,AIO1                                                          
         MVI   ELCODE,TASNELQ    FIND SHORT NAME FOR MARKET                     
         BRAS  RE,GETEL                                                         
         BNE   CM30                                                             
         SPACE 1                                                                
         USING TAMTD,R1                                                         
         MVC   TAMTCODE,TASNAME  AND WRITE IT OVER MARKET CODE                  
         SPACE 1                                                                
         LA    RE,TAMTCODE                                                      
         LHI   RF,L'TAMTCODE                                                    
CM80     CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,CM80                                                          
         DROP  R1                                                               
         SPACE 1                                                                
         L     R4,AIO2                                                          
         MVI   ELCODE,TAMTELQ    THEN RESET R4 TO CURRENT MARKET                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CM90     BRAS  RE,NEXTEL                                                        
         CR    R1,R4                                                            
         BNE   CM90                                                             
         B     CM30                                                             
         SPACE 1                                                                
         USING TAMTD,R4                                                         
CM100    MVC   AIO,AIO2                                                         
         CLC   LASTCODE,TAMTCODE                                                
         BE    CM30                                                             
         MVC   0(L'TAMTCODE,R2),TAMTCODE                                        
         MVC   LASTCODE,TAMTCODE                                                
         LHI   RE,L'TAMTCODE                                                    
CM110    CLI   0(R2),0                                                          
         BE    CM120                                                            
         LA    R2,1(R2)                                                         
         BCT   RE,CM110                                                         
CM120    MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LHI   RF,L'TAMTCODE                                                    
         SR    RF,RE                                                            
         ZIC   RE,TAXCLEN                                                       
         AR    RE,RF                                                            
         AHI   RE,1                                                             
         STC   RE,TAXCLEN                                                       
         B     CM30                                                             
         SPACE 1                                                                
CM140    SHI   R2,1                                                             
         CLI   0(R2),C','                                                       
         BNE   CMX                                                              
         MVI   0(R2),C' '                                                       
         BRAS  RE,ELEMADD        ADD NEW OR UPDATED ELEMENT                     
         OI    STATUS,ADDNOFEL   SET ADDED "NOT ON FILE" STATUS                 
         DROP  R3,R4                                                            
         SPACE 1                                                                
CMX      XIT1                                                                   
         SPACE 2                                                                
MISSCNET DC    C'INCLUDES CNET RECORDS NOT ON FILE: '                           
MISSMKT  DC    C'INCLUDES  MKT RECORDS NOT ON FILE: '                           
MISSCSYS DC    C'INCLUDES CSYS RECORDS NOT ON FILE: '                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CONVERT WSC ADVICES TO RAD                            
         SPACE 1                                                                
CVTRAD   NTR1  BASE=*,LABEL=*                                                   
         USING TAVUD,R4                                                         
         L     R4,AIO2          IF ADDING/CHANGING CANADIAN WILDSPOT            
         MVI   ELCODE,TAVUELQ   ADVICE FOR A RADIO COMMERCIAL                   
         BRAS  RE,GETEL                                                         
         BNE   CRX                                                              
         CLI   TAVUUSE,UWSC                                                     
         BNE   CRX                                                              
         CLI   TAVUMED,TACOMEDR                                                 
         BNE   CRX                                                              
         MVI   TAVUUSE,URAD     CHANGE WSC TO RAD                               
         MVI   TAVUTYPE,0       AND CLEAR TYPE                                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TADVELQ   AND CHANGE SCREEN FROM SPOT/NETTAL              
         BRAS  RE,GETEL         TO CANADIAN                                     
         BNE   CRX                                                              
         MVI   TADVTYPE,TADVTYPN                                                
CRX      XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DETERMINE THE LCB USE TYPE                                       
         SPACE 1                                                                
LCBTYPE  NTR1  BASE=*,LABEL=*                                                   
         USING TAVUD,R4                                                         
         L     R4,AIO2           IF ADDING/CHANGING LOCAL CABLE ADVICE          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   LCBTYX                                                           
         CLI   TAVUUSE,ULCB                                                     
         BNE   LCBTYX                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         XC    UNITCALC,UNITCALC                                                
         XC    DUB,DUB                                                          
         SPACE 1                                                                
         USING TAUPD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BE    LCBTY10                                                          
         L     R4,AIO2                                                          
         MVI   ELCODE,TAUPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   LCBTY10                                                          
         MVC   UNITCALC+2(L'TAUPICBU),TAUPICBU                                  
         DROP  R4                                                               
         SPACE 1                                                                
LCBTY10  MVC   AIO,AIO1                                                         
         MVI   TGMTTYPE,C'S'       READ CSYS RECORDS                            
         SPACE 1                                                                
         USING TAMTD,R4                                                         
LCBTY20  L     R4,AIO2           READ THROUGH CABLE SYSTEM RECORDS              
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    LCBTY40                                                          
         XC    UNITCALC,UNITCALC                                                
         B     LCBTY60                                                          
LCBTY30  BRAS  RE,NEXTEL                                                        
         BNE   LCBTY60                                                          
LCBTY40  CLC   TAMTCODE,LASTCODE IF CABLE SYSTEM ALREADY PROCESSED              
         BNH   LCBTY30           GET NEXT CABLE STATION ELEMENT                 
         MVC   LASTCODE,TAMTCODE                                                
         XC    DUB,DUB                                                          
         MVC   DUB(L'TAMTCODE),TAMTCODE                                         
         GOTO1 RECVAL,DMCB,TLMTCDQ,(X'A4',TAMTCODE)                             
         BNE   LCBTY20                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMSD,R4                                                         
         L     R4,AIO1           GET WEIGHT FOR THE PROPER YEAR                 
         MVI   ELCODE,TAMSELQ    FROM NET CABLE RECORD                          
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LCBTY50  BRAS  RE,NEXTEL                                                        
         BNE   LCBTY20                                                          
         L     RE,UNITCALC                                                      
         L     RF,TAMSWGHT                                                      
         AR    RE,RF                                                            
         ST    RE,UNITCALC                                                      
         B     LCBTY20                                                          
         DROP  R4                                                               
         SPACE 1                                                                
LCBTY60  MVC   DUB(4),UNITCALC   SAVE NUMBER OF SUBSCRIBERS ON ADVICE           
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         USING TAUPD,R4                                                         
         L     R4,AIO2           AND ADD IT TO NUMBER ALREADY PAID              
         MVI   ELCODE,TAUPELQ    IN THE CYCLE                                   
         BRAS  RE,GETEL                                                         
         BNE   LCBTY70                                                          
         L     RE,UNITCALC                                                      
         ZICM  RF,TAUPISUB,4                                                    
         AR    RE,RF                                                            
         ST    RE,UNITCALC                                                      
         DROP  R4                                                               
         SPACE 1                                                                
LCBTY70  MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BE    LCBTY80                                                          
         MVC   DUB(4),UNITCALC                                                  
         SPACE 1                                                                
         USING TAVUD,R4                                                         
LCBTY80  L     R4,AIO2                                                          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   LCBTYX                                                           
         SPACE 1                                                                
         CLC   DUB(4),=F'1000000' IF THE NUMBER OF SUBSCRIBERS ON               
         BNH   LCBTY90            THIS ADVICE EXCEEDS 1MIL                      
         MVI   TAVUUSE,UCBL       CHANGE USE TO NATIONAL CABLE                  
         MVI   TAVUTYPE,UCBLREG                                                 
         CLC   DUB(4),UNITCALC                                                  
         BE    LCBTYX                                                           
         MVI   TAVUTYPE,UCBLUPG                                                 
         B     LCBTYX                                                           
         SPACE 1                                                                
LCBTY90  LA    RE,LCBLTAB                                                       
LCBTY100 MVC   TAVUTYPE,4(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BE    LCBTY110                                                         
         CLC   UNITCALC,0(RE)                                                   
         BNH   LCBTY110                                                         
         LA    RE,L'LCBLTAB(RE)                                                 
         B     LCBTY100                                                         
         SPACE 1                                                                
LCBTY110 LA    R2,TAVUTYPE                                                      
         DROP  R4                                                               
         SPACE 1                                                                
         L     R4,AIO2             IF ADVICE IS AN UPGRADE                      
         MVI   ELCODE,TAUPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   LCBTYX                                                           
         SPACE 1                                                                
         MVC   TGUPFRTY,PREVTYP    FIX THE USE TYPE                             
         MVC   TGUPTOTY,0(R2)                                                   
         MVC   TGUSCDE,=C'LCB'                                                  
         GOTO1 UPGRVAL,DMCB,(X'80',TGUSCDE),TGUPFRTY,TGUSCDE,TGUPTOTY           
         BNE   LCBTYX                                                           
         MVC   0(1,R2),TGUPTYP                                                  
LCBTYX   XIT1                                                                   
         SPACE 2                                                                
         DC    0F                                                               
LCBLTAB  DC    0XL8                                                             
         DC    F'50000',AL1(ULCB50)                                             
         DC    F'100000',AL1(ULCB100)                                           
         DC    F'150000',AL1(ULCB150)                                           
         DC    F'200000',AL1(ULCB200)                                           
         DC    F'250000',AL1(ULCB250)                                           
         DC    F'500000',AL1(ULCB500)                                           
         DC    F'750000',AL1(ULCB750)                                           
         DC    F'1000000',AL1(ULCB1M)                                           
         DC    0F                                                               
         DC    4X'FF',AL1(ULCBMAX)                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADDS DUE DATE ELEMENT                                    
         SPACE 1                                                                
ADDDUES  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TADDELQ    GET RID OF DUE DATE ELEMENT                    
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO            DO NOT CALCULATE DUE DATES                     
         MVI   ELCODE,TADVELQ    FOR SESSION ADVICES                            
         BRAS  RE,GETEL                                                         
         BNE   ADUESX                                                           
         CLI   TADVTYPE,TADVTYPS                                                
         BE    ADUESX                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   ADUESX                                                           
         MVC   REGDATE,TAVUCYC                                                  
         MVC   USE1,TAVUUSE      SAVE ADVICE'S USES AND TYPES                   
         MVC   TYPE1,TAVUTYPE                                                   
         MVC   MUSDATE,TAVUMCYS                                                 
         MVC   USE2,TAVUUSE2                                                    
         MVC   TYPE2,TAVUTYP2                                                   
         DROP  R4                                                               
         SPACE 1                                                                
         USING TADDD,R4                                                         
         LA    R4,ELEMENT        INITIALIZE DUE DATE ELEMENT                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   TADDEL,TADDELQ                                                   
         MVI   TADDLEN,TADDLNQ                                                  
         SPACE 1                                                                
         OC    REGDATE,REGDATE   SET REGULAR USE'S GLOBALS                      
         BZ    ADUES20                                                          
         XC    TGUSDATA,TGUSDATA                                                
         MVI   TGUSXUNS,AFM                                                     
         CLI   USE1,0                                                           
         BE    ADUES30                                                          
         GOTO1 USEVAL,DMCB,(X'80',USE1),(X'80',TYPE1)                           
         B     ADUES30                                                          
         SPACE 1                                                                
*                                AFTER REGULAR USE DUE DATE HAS                 
*                                BEEN CALCULATED, SEE IF MUSIC DUE              
*                                DATE HAS BEEN CALCULATED                       
ADUES10  GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BZ    ADUES120                                                         
         SPACE 1                                                                
ADUES20  OC    MUSDATE,MUSDATE   IF NOT, SET MUSIC USE'S GLOBALS                
         BZ    ADUES120                                                         
         XC    TGUSDATA,TGUSDATA                                                
         XC    TGUSXUNS,TGUSXUNS                                                
         CLI   USE2,0                                                           
         BE    ADUES30                                                          
         GOTO1 USEVAL,DMCB,(X'80',USE2),(X'80',TYPE2)                           
         SPACE 1                                                                
ADUES30  LA    R2,TADDDATE                                                      
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BNZ   *+8                                                              
         LA    R2,TADDDATM                                                      
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   DUB(3),TGTODAY1   SET DUE DATE AS TODAY                          
         SPACE 1                                                                
         TM    AGYSTAT,TAAYSCOD  IF COD CLIENT OR COD AGENCY                    
         BO    ADUES115          DUE DATE IS TODAY                              
         SPACE 1                                                                
         CLI   TGUSEQU,UPRR      IF PROMO USE                                   
         BE    ADUES40                                                          
         CLI   TGUSEQU,UFGR      OR IF FOREIGN USE                              
         BNE   ADUES50                                                          
         CLI   TGUSTYP,UFGREXT   AND EXTENSION TYPE                             
         BNE   ADUES50                                                          
ADUES40  LA    R0,1              DUE DATE IS TODAY + ONE DAY                    
         B     ADUES90                                                          
         SPACE 1                                                                
ADUES50  MVC   DUB(3),REGDATE                                                   
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BNZ   *+10                                                             
         MVC   DUB(3),MUSDATE    SET DUE DATE AS CYCLE START DATE               
         SPACE 1                                                                
         CLI   TGUSEQU,UITN                                                     
         BE    ADUES60                                                          
         TM    TGUSTYST,UPGRADE                                                 
         BZ    ADUES70                                                          
ADUES60  MVC   DUB(3),REGDATE+3                                                 
         LA    R0,15                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,UITN      IF ITN USE                                     
         BE    ADUES90                                                          
         CLI   TGUSEQU,UWSP      OR WILDSPOT UPGRADE                            
         BE    ADUES90                                                          
         CLI   TGUSEQU,USWS      OR SPANISH WILDSPOT UPGRADE                    
         BE    ADUES90           USE CYCLE END + 15 DAYS                        
         SPACE 1                                                                
         CLC   TGTODAY1,DUB      ALL OTHER UPGRADES                             
         BNL   ADUES90                                                          
         MVC   DUB(3),TGTODAY1                                                  
         B     ADUES90           USE TODAY + 15 DAYS                            
         SPACE 1                                                                
ADUES70  TM    TGUSSTAT,CANUSE   IF CANADIAN USE                                
         BZ    ADUES80                                                          
         LA    R0,5              USE CYCLE START + 5 DAYS                       
         B     ADUES90                                                          
         SPACE 1                                                                
ADUES80  LA    R0,1                                                             
         TM    TGUSSTA2,HLDTYPE  FOR HOLDING FEE TYPE                           
         BO    ADUES90                                                          
         CLI   TGUSEQU,UREN      OR REN                                         
         BE    ADUES90                                                          
         CLI   TGUSEQU,UARN      OR ARN                                         
         BE    ADUES90                                                          
         CLI   TGUSEQU,USRE      OR SRE                                         
         BE    ADUES90           USE CYCLE START - 1 DAY                        
         SPACE                                                                  
         XR    R0,R0             ELSE 0 DAYS                                    
         TM    TGUSSTA2,ZERODUE  IF SPECIFIED                                   
         BO    ADUES90                                                          
         LA    R0,12             12 FOR EVERYTHING ELSE                         
         SPACE 1                                                                
         USING TANPD,R4                                                         
         TM    TGUSSTA3,NWKUSE   FOR NETWORK USES                               
         BZ    ADUES90                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ    GET THE FIRST PROGRAM DATE                     
         BRAS  RE,GETEL          DUE DATE IS 12 DAYS AFTER THE                  
         BNE   ADUES90           FRIDAY OF THE FIRST PROGRAM'S WEEK             
         MVC   DUB(3),TANPDATE                                                  
         GOTO1 DATCON,DMCB,(1,DUB),(0,WORK)                                     
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         ZIC   RF,0(R1)                                                         
         LA    R3,5                                                             
         SR    R3,RF                                                            
         BNP   ADUES90                                                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,DUB)                                   
         DROP  R4                                                               
         SPACE 1                                                                
ADUES90  MH    R0,=H'24'         MULTIPLY BY 24 TO GET TOTAL N'HOURS            
         SPACE 1                                                                
         XC    WORK,WORK         INITIALIZE GETRET BLOCK                        
         LA    R3,WORK                                                          
         USING GETRETD,R3                                                       
         STCM  R0,3,GRDHRS        SET N'HOURS                                   
***      OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU                                                  
         GOTO1 DATCON,DMCB,(1,DUB),(3,GRDIDY) AND START DATE                    
         SPACE 1                                                                
         USING TADDD,R4                                                         
         LA    R4,ELEMENT                                                       
         MVC   0(L'TADDDATE,R2),DUB                                             
         SPACE 1                                                                
         TM    TGUSSTA2,HLDTYPE    FOR HOLDING FEE TYPES                        
         BO    ADUES100                                                         
         CLI   TGUSEQU,UREN        OR REN                                       
         BE    ADUES100                                                         
         CLI   TGUSEQU,UARN        OR ARN                                       
         BE    ADUES100                                                         
         CLI   TGUSEQU,USRE        OR SRE                                       
         BNE   *+8                                                              
ADUES100 OI    GRDFLAG,GRDFBACK    SET TO GO BACKWARDS                          
         OI    GRDFLAG,GRDFHLOK    SET START ON WEEKEND/HOLIDAY OK              
         SPACE 1                                                                
         GOTO1 MYGETRET,(R3)       DETERMINE DUE DATE                           
         BNE   ADUES110            SKIP IF NONE FOUND (NO CALENDAR YET)         
         GOTOR UNIOND,DMCB,MYGETRET,(R3),PARAS                                  
         BE    *+6                 TP HOLIDAY                                   
         DC    H'0'                                                             
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,TGFULL)  CONVERT TO PWOS               
         MVC   0(L'TADDDATE,R2),TGFULL                                          
         SPACE 1                                                                
ADUES110 CLC   0(L'TADDDATE,R2),TGTODAY1                                        
         BNL   ADUES10                                                          
ADUES115 MVC   0(L'TADDDATE,R2),TGTODAY1                                        
         B     ADUES10                                                          
         SPACE 1                                                                
ADUES120 GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE 1                                                                
ADUESX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CHECK KEY FIELDS CHANGED                                         
*                                  XIT - CC EQUAL IF NO CHANGES                 
         SPACE                                                                  
CHKFVAL  NTR1  BASE=*,LABEL=*                                                   
         TM    SAGAGYH+4,X'20'     IF NO CHANGE TO AGENCY                       
         BZ    CNO                                                              
         TM    SAGNIDH+4,X'20'               OR TO NID                          
         BZ    CNO                                                              
         TM    SAGCLIH+4,X'20'               OR TO CLIENT                       
         BZ    CNO                                                              
         TM    SAGPRDH+4,X'20'               OR TO PRODUCT                      
         BZ    CNO                                                              
         TM    SAGMEDH+4,X'20'               OR TO MEDIA                        
         BZ    CNO                                                              
         TM    SAGUSEH+4,X'20'               OR TO USE                          
         BZ    CNO                                                              
         TM    SAGADDH+4,X'20'               OR TO DATE ADDED                   
         BZ    CNO                                                              
         TM    SAGUIDH+4,X'20'               OR TO USERID                       
         BZ    CNO                                                              
         SR    RC,RC               SET CONDITION CODE                           
CNO      LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF THERE ARE ANY MARKETS, NETWORKS OR SYSTEMS *         
*        ON A CBL/WSP/LCB ADVICE                                      *         
***********************************************************************         
                                                                                
PROCEDE  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2                                                          
         MVI   ELCODE,TAMTELQ      IF THERE ARE NO MARKETS, NETWORKS            
         BRAS  RE,GETEL            OR CABLE SYSTEMS ON THE ADVICE ...           
         JE    YES                                                              
*                                                                               
         USING TADVD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TADVELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         CLI   TADVTYPE,TADVTYPB   ... AND THIS IS A CABLE/WILDSPOT             
         JNE   YES                 ADVICE, DO NOT ADD IT                        
         J     NO                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              SHOULD ADVICE RECORD BE ADDED?                                   
         SPACE                                                                  
OK2ADD   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ADJTANP          ADJUST NETWORK PROGRAM ELEMS                 
         SPACE 1                                                                
         L     R4,AIO2             IF ANY TANP ELEMENTS STILL EXIST             
         MVI   ELCODE,TANPELQ      THEN WE WANT TO ADD RECORD                   
         BRAS  RE,GETEL                                                         
         BE    O2AYES                                                           
         USING TAVUD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   O2ANO                                                            
         CLI   TAVUUSE,UCLA                                                     
         BE    O2ANO                                                            
         CLI   TAVUUSE,ULNA                                                     
         BE    O2ANO                                                            
         CLI   TAVUUSE,ULNC                                                     
         BE    O2ANO                                                            
         CLI   TAVUUSE,ULNF                                                     
         BE    O2ANO                                                            
         CLI   TAVUUSE,ULNN                                                     
         BE    O2ANO                                                            
         CLI   TAVUUSE,UPAX                                                     
         BE    O2ANO                                                            
         CLI   TAVUUSE,UITN                                                     
         BE    O2ANO                                                            
O2AYES   XR    RC,RC                                                            
O2ANO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ADJUST THE NETWORK PROGRAM ELEMENTS                              
         SPACE                                                                  
ADJTANP  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,WHAT2ADD                                                      
         TM    DMCB,GNPAXSTA       IF NO 00 CAST MEMBERS PRESENT                
         BZ    ATANPX                                                           
         TM    DMCB,GNCLASTA       OR SOME PRE-00 CAST MEMBERS PRESENT          
         BO    ATANPX              NO NEED FOR ADJUSTMENT                       
         SPACE 1                                                                
         USING TANPD,R4                                                         
         L     R4,AIO              IF CAST IS ALL 00                            
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL            GET TANPD ELEMENTS                           
         B     *+8                                                              
ATANP10  BRAS  RE,NEXTEL                                                        
         BNE   ATANPX                                                           
         CLI   TANPNWK,C'B'        BOUNCE WORKS LIKE PAX                        
         BE    ATANP20                                                          
         CLI   TANPNWK,C'E'        METV WORKS LIKE PAX                          
         BE    ATANP20                                                          
         CLI   TANPNWK,C'Y'        ANTENNA TV WORKS LIKE PAX                    
         BE    ATANP20                                                          
         CLI   TANPNWK,C'Z'        COZI TV WORKS LIKE PAX                       
         BE    ATANP20                                                          
         CLI   TANPNWK,C'P'        ESCAPE WORKS LIKE PAX                        
         BE    ATANP20                                                          
         CLI   TANPNWK,C'G'        GET TV WORKS LIKE PAX                        
         BE    ATANP20                                                          
         CLI   TANPNWK,C'R'        GRIT WORKS LIKE PAX                          
         BE    ATANP20                                                          
         CLI   TANPNWK,C'J'        JUSTICE WORKS LIKE PAX                       
         BE    ATANP20                                                          
         CLI   TANPNWK,C'L'        LAFF WORKS LIKE PAX                          
         BE    ATANP20                                                          
         CLI   TANPNWK,C'T'        THIS TV WORKS LIKE PAX                       
         BE    ATANP20                                                          
         CLI   TANPNWK,C'X'        AND CHANGE THE ELEMENT CODE                  
         BNE   ATANP10             OF ANY SET UP FOR PAX                        
ATANP20  MVI   TANPEL,X'BB'        TO X'BB'                                     
         B     ATANP10                                                          
ATANPX   XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO GET NEXT ADVICE CODE                                  
         SPACE                                                                  
GETADV   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         MVC   AIO,AIO1            SAVE IOAREA                                  
         GOTO1 CHNADV              UPDATES AGY REC AND RETURNS TGADV            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY           RESET CURRENT KEY                            
         MVC   AIO,AIO2            RESET IOAREA                                 
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET START AND END DATES FOR NEW CYCLE RANGE           
         SPACE 1                                                                
         USING BUFFD,R2                                                         
STNEWRNG NTR1  BASE=*,LABEL=*                                                   
         OC    BUFFCYCS(6),BUFFCYCS                                             
         BNZ   SNR10                                                            
         MVC   STRTDATE,BUFFDATE   SAVE CYCLE START DATE                        
         GOTO1 DATCON,DMCB,(1,STRTDATE),(5,WORK)                                
         MVC   WORK+8(6),=CL6'-(3M)'  DEFAULT TO 3 MONTHS                       
         TM    AGYSTAT,TAAYS13W       UNLESS AGENCY IS SET                      
         BZ    *+10                                                             
         MVC   WORK+8(6),=CL6'-(13W)' FOR 13 WEEKS                              
         LA    RF,14                                                            
         GOTO1 PDVAL,DMCB,(X'40',BLOCK),((RF),WORK)                             
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   ENDDATE,PVALPEND    SET END DATE                                 
         B     SNRX                                                             
         SPACE 1                                                                
SNR10    MVC   STRTDATE(6),BUFFCYCS                                             
SNRX     XIT1                                                                   
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO CLEAR NAME FIELDS                                     
         SPACE                                                                  
CLRNAMES NTR1  BASE=*,LABEL=*                                                   
         XC    SAGNIDN,SAGNIDN     CLEAR NETWORK TITLE                          
         OI    SAGNIDNH+6,X'80'                                                 
         XC    SAGCLIN,SAGCLIN           CLIENT NAME                            
         OI    SAGCLINH+6,X'80'                                                 
         XC    SAGPRDN,SAGPRDN           PRODUCT NAME                           
         OI    SAGPRDNH+6,X'80'                                                 
         XC    SAGMEDN,SAGMEDN           MEDIA NAME                             
         OI    SAGMEDNH+6,X'80'                                                 
         XC    SAGUSEN,SAGUSEN           USE NAME                               
         OI    SAGUSENH+6,X'80'                                                 
         XIT1                                                                   
         EJECT                                                                  
*              SET KEY FIELDS PREVIOUSLY VALIDATED                              
         SPACE                                                                  
SETFVAL  NTR1  BASE=*,LABEL=*                                                   
         OI    SAGAGYH+4,X'20'     SET AGENCY PREVIOUSLY VALIDATED              
         OI    SAGNIDH+4,X'20'     SET NID PREVIOUSLY VALIDATED                 
         OI    SAGCLIH+4,X'20'     SET CLIENT PREVIOUSLY VALIDATED              
         OI    SAGPRDH+4,X'20'     SET PRODUCT PREVIOUSLY VALIDATED             
         OI    SAGMEDH+4,X'20'     SET MEDIA PREVIOUSLY VALIDATED               
         OI    SAGUSEH+4,X'20'     SET USE PREVIOUSLY VALIDATED                 
         OI    SAGADDH+4,X'20'     SET DATE ADDED PREVIOUSLY VALIDATED          
         OI    SAGUIDH+4,X'20'     SET USERID PREVIOUSLY VALIDATED              
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
* CHECK IF CAST HAS PEOPLE BEFORE YEAR 2000                                     
*    DMCB P1 HAS STATUS BITS SET BASED ON CAST                                  
*    CC NE, NOT PAX OR NO CAST                                                  
*----------------------------------------------------------------------         
         DS    0D                                                               
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
CHKCAST  NTR1  BASE=*,LABEL=*                                                   
         MVI   STATPC,0                                                         
*                                                                               
         CLC   SRTUSE,=C'CBL'                                                   
         BE    CCAST130                                                         
         CLC   SRTUSE,=C'SCB'                                                   
         BE    CCAST130                                                         
         CLC   SRTUSE,=C'WSP'                                                   
         BE    CCAST130                                                         
         CLC   SRTUSE,=C'SWS'                                                   
         BE    CCAST130                                                         
         CLC   SRTUSE,=C'WSC'                                                   
         BE    CCAST130                                                         
         CLC   SRTUSE,=C'ADW'                                                   
         BE    CCAST130                                                         
         CLC   SRTUSE,=C'LCB'                                                   
         BE    CCAST130                                                         
*                                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY             BUILD COMMERCIAL KEY                         
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOICDQ    ACTIVE KEY                                   
         MVC   TLCOIAGY,SRTAGY     AGENCY                                       
         MVC   TLCOICID,SRTTCID    COMMERCIAL ID                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOICID+L'TLCOICID-TLCOPD),KEYSAVE                          
         BNE   CCAST10                                                          
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL CODE                
         B     CCAST20                                                          
         DROP  R4                                                               
*                                                                               
CCAST10  XC    KEY,KEY             IF NOT FOUND, TRY ANOTHER WAY                
         USING TLCOD,R4            BUILD COMMERCIAL KEY                         
         MVI   TLCOCD,TLCOCDQ      ACTIVE KEY                                   
         MVC   TLCOAGY,SRTAGY      AGENCY                                       
         MVC   TLCOCLI,SRTCLI      CLIENT                                       
         MVC   TLCOPRD,SRTPRD      PRODUCT                                      
         MVC   TLCOCID,SRTTCID     CID                                          
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOCID+L'TLCOCID-TLCOCD),KEYSAVE                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL COMMERCIAL CODE                
         DROP  R3,R4                                                            
*                                                                               
CCAST20  XC    KEY,KEY             BUILD CAST KEY                               
         LA    R4,KEY                                                           
         USING TLCAD,R4                                                         
         MVI   TLCACD,TLCACDQ      ACTIVE KEY                                   
         MVC   TLCACOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         B     CCAST110                                                         
CCAST100 GOTO1 SEQ                                                              
CCAST110 CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   CCAST120            NO CAST, DONE                                
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      CAST DETAIL ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
         CLC   TACAUN,=C'AFM'      SKIP AFM MEMBERS                             
         BE    CCAST100                                                         
*                                                                               
         CLC   TACAYEAR,=C'27 '    YEAR 2000 DILEMMA                            
         BH    *+12                                                             
         OI    STATPC,GNPAXSTA     GENERATE PAX ADVICE                          
         B     *+8                                                              
         OI    STATPC,GNCLASTA     GENERATE CLA ADVICE                          
*                                                                               
         TM    STATPC,GNPAXSTA+GNCLASTA          NEED BOTH?                     
         BNO   CCAST100            YES, WE'RE DONE                              
CCAST120 MVC   AIO,AIO2                                                         
CCAST130 MVC   DMCB(1),STATPC                                                   
CCASTX   XIT1                                                                   
*                                                                               
STATPC   DS    XL1                                                              
         LTORG                                                                  
         EJECT                                                                  
WHAT2ADD NTR1  BASE=*,LABEL=*                                                   
         MVI   STAPC,0                                                          
         SPACE 1                                                                
         MVI   ELCODE,TACMELQ                 GET TACM ELEMENT FROM             
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPD))   ADVICE                            
         BNE   W2AX                                                             
         SPACE 1                                                                
         L     RE,TGELEM                                                        
         USING TACMD,RE                                                         
         CLI   TACMCOMM,C'B'                  MIXED CAST                        
         BNE   *+12                                                             
         OI    STAPC,GNPAXSTA+GNCLASTA                                          
         B     W2A10                                                            
         CLI   TACMCOMM,C'C'                  ALL PRE-00 CAST                   
         BNE   *+12                                                             
         OI    STAPC,GNCLASTA                                                   
         B     W2A10                                                            
         CLI   TACMCOMM,C'X'                  ALL 00 AND LATER CAST             
         BNE   *+12                                                             
         OI    STAPC,GNPAXSTA                                                   
         B     W2A10                                                            
         CLI   TACMCOMM,C'N'                  NO CAST                           
         BE    *+6                                                              
         DC    H'00'                                                            
         DROP  RE                                                               
W2A10    MVC   DMCB(1),STAPC                                                    
W2AX     XIT1                                                                   
STAPC    DS    XL1                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* CHANGE THE CURRENT CLA ADVICE TO A PAX ADVICE                                 
*----------------------------------------------------------------------         
         DS    0D                                                               
CLA2PAX  NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2               CLEAR PAX PROGRAM COUNTER                    
* IF WE ARE ADDING A SPLIT ADVICE FOR PAX                                       
* START BY DELETIING ALL PAX TANPELQ ELEMENTS FIRST                             
* ALL TANPELQ ELEMENET STHAT ARE PAX WOULD HAVE ALREADY                         
* BEEN PROCESSED BY A PRIOR ADVICE SO THIS SHOULD BE SAFE TO DO.                
*                                                                               
         CLI   SPLITADV,C'Y'                                                    
         BNE   C2P008                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ      ON THIS ADVICE ...                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
C2P006   BRAS  RE,NEXTEL                                                        
         BNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         B     C2P006                                                           
                                                                                
         SPACE 1                                                                
C2P008   L     R4,AIO                                                           
         MVI   ELCODE,X'BB'        CHANGE ALL BB ELEMENTS                       
         LA    R0,1                RESEQUENCE BB PAX TANPELQ ELEMENTS           
         BRAS  RE,GETEL            TO TANP ELEMENTS                             
         B     *+8                                                              
C2P010   BRAS  RE,NEXTEL                                                        
         BNE   C2P012                                                           
         MVI   0(R4),TANPELQ                                                    
         STC   R0,TANPSEQ-TANPEL(R4)                                            
         AHI   R0,1                BUMP BB TANP SEQUENCE NUMBER                 
         AHI   R2,1                CUT ADVICE WHEN WE EXCEED                    
         CH    R2,=AL2(MAXTANP)    20 PROGRAMS                                  
         BL    C2P010                                                           
         SPACE 1                                                                
C2P012   L     R4,AIO              IF THERE IS A PAX PROGRAM                    
         MVI   ELCODE,TANPELQ      ON THIS ADVICE ...                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
C2P014   BRAS  RE,NEXTEL                                                        
         BNE   C2PNO                                                            
         USING TANPD,R4                                                         
         CLI   TANPNWK,C'B'        OR BOUNCE                                    
         BE    C2P016                                                           
         CLI   TANPNWK,C'E'        OR ME TV                                     
         BE    C2P016                                                           
         CLI   TANPNWK,C'Y'        OR ANTENNA TV                                
         BE    C2P016                                                           
         CLI   TANPNWK,C'Z'        OR COZI TV                                   
         BE    C2P016                                                           
         CLI   TANPNWK,C'P'        OR ESCAPE                                    
         BE    C2P016                                                           
         CLI   TANPNWK,C'G'        OR GET TV                                    
         BE    C2P016                                                           
         CLI   TANPNWK,C'R'        OR GRIT                                      
         BE    C2P016                                                           
         CLI   TANPNWK,C'J'        OR JUSTICE                                   
         BE    C2P016                                                           
         CLI   TANPNWK,C'L'        OR LAFF                                      
         BE    C2P016                                                           
         CLI   TANPNWK,C'T'        OR THIS TV                                   
         BE    C2P016                                                           
         CLI   TANPNWK,C'X'                                                     
         BNE   C2P014                                                           
         SPACE 1                                                                
C2P016   CLC   SVCOM,TGCOM         IF COMMERCIAL HAS CHANGED                    
         BNE   C2P018                                                           
         CLC   TANPDATE,SVADATE    OR IF USE DATE DOESN'T FIT                   
         BL    C2P018              WITHIN PREVIOUS CYCLE                        
         CLC   TANPDATE,SVADATE+3                                               
         BNH   C2P020                                                           
C2P018   XC    USE#,USE#           CLEAR THE NUMBER OF USES                     
         SPACE 1                                                                
C2P020   MVC   SVCOM,TGCOM         SAVE INTERNAL COMMERCIAL NUMBER              
         USING BUFFD,R2                                                         
         L     R2,ABUFFEL                                                       
         MVC   BUFFDATE,TANPDATE                                                
*                                  IF COMMERCIAL CHANGED OR USE DATE            
         OC    USE#,USE#           DIDN'T FIT WITHIN PREVIOUS CYCLE             
         BNZ   C2P060              SET CYCLE DATES FOR THIS                     
         BRAS  RE,STNEWRNG         ADVICE BASED (INITIALLY) ON                  
         MVC   SVADATE,STRTDATE    THE FIRST PAX PROGRAM                        
         DROP  R2,R4                                                            
         SPACE 1                                                                
C2P030   XC    KEY,KEY             BUILD USAGE HISTORY KEY                      
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ      USAGE HISTORY RECORD CODE                    
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,=C'PAX'     PAX USE CODE                                 
         GOTO1 HIGH                                                             
         B     C2P050                                                           
C2P040   GOTO1 SEQ                                                              
C2P050   CLC   KEY(TLUHINV-TLUHKEY),KEYSAVE                                     
         BNE   C2P060                                                           
         MVC   SVUHKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET USAGE HISTORY RECORD                     
         L     R2,ABUFFEL                                                       
         USING BUFFD,R2                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAUHD,R4                                                         
         CLC   BUFFDATE,TAUHSTRT   IF PROGRAM FITS WITHIN                       
         BL    C2P040              THIS EXISTING CYCLE                          
         CLC   BUFFDATE,TAUHEND                                                 
         BH    C2P040                                                           
         MVC   USE#,TAUHUSN        SAVE LAST USE NUMBER                         
         MVC   SVADATE,TAUHSTRT    AND RESET THE ADV CYCLE DATES                
         DROP  R2,R4                                                            
         SPACE 1                                                                
C2P060   MVC   AIO,AIO2            RESET AIO TO ADVICE                          
         SPACE 1                                                                
         L     R4,AIO                                                           
         USING TLDVD,R4                                                         
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL            GET TAVU ELEMENT                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAVUD,R4                                                         
         MVI   TAVUUSE,UPAX        CHANGE USE TO PAX                            
         MVI   TAVUTYPE,UPAXREG                                                 
         MVC   TAVUCYC,SVADATE     AND MOVE IN NEW CYCLE DATE                   
         DROP  R4                                                               
         SPACE 1                                                                
         SR    R3,R3               R3=PAX PROGRAM COUNTER                       
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ      READ NETWORK ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     C2P200                                                           
C2P100   BRAS  RE,NEXTEL                                                        
C2P200   BNE   C2P300                                                           
         SPACE 1                                                                
         USING TANPD,R4                                                         
         CLI   TANPNWK,C'X'        IF ELEMENT NOT FOR PAX PROGRAM               
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'B'        IF ELEMENT NOT FOR BOUNCE PROGRAM            
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'E'        IF ELEMENT NOT FOR METV PROGRAM              
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'Y'        IF ELEMENT NOT FOR ANTENNA PROGRAM           
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'Z'        IF ELEMENT NOT FOR COZI PROGRAM              
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'P'        IF ELEMENT NOT FOR ESCAPE PROGRAM            
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'G'        IF ELEMENT NOT FOR GET TV PROGRAM            
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'R'        IF ELEMENT NOT FOR GRIT PROGRAM              
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'J'        IF ELEMENT NOT FOR JUSTICE PROGRAM           
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'L'        IF ELEMENT NOT FOR LAFF PROGRAM              
         BE    C2P220              SET IT FOR DELETE                            
         CLI   TANPNWK,C'T'        IF ELEMENT NOT FOR THIS TV PROGRAM           
         BE    C2P220              SET IT FOR DELETE                            
         MVI   TANPEL,X'FF'        AND GO READ NEXT ELEMENT                     
         B     C2P100                                                           
         SPACE 1                                                                
C2P220   CLC   TANPDATE,SVADATE    IF USE DOES NOT FIT WITHIN                   
         BL    *+14                THIS ADVICE'S CYCLE                          
         CLC   TANPDATE,SVADATE+3                                               
         BNH   C2P250                                                           
         MVI   TANPEL,X'AA'        MARK IT AS AN 'AA' ELEMENT                   
         B     C2P100              WHICH WILL REQUIRE ANOTHER ADVICE            
         SPACE 1                                                                
C2P250   LH    R1,USE#             IF ELEMENT IS FOR PAX PROGRAM                
         AHI   R1,1                BUMP UP TOTAL USE NUMBER                     
         STH   R1,USE#             STORE TOTAL USE NUMBER                       
         MVC   TANPUSEN,USE#                                                    
         AHI   R3,1                AND BUMP UP INDIVIDUAL ADVICE'S              
         B     C2P100              USE COUNTER                                  
         SPACE 1                                                                
C2P300   MVI   ELCODE,X'FF'        DELETE ALL MARKED FOR DELETE RECS            
         GOTO1 REMELEM                                                          
         STH   R3,CNTTANP                                                       
         SPACE 1                                                                
         BRAS  RE,GETADV           GET NEXT ADVICE CODE                         
         L     R4,AIO              RESTOREAIO OF HER TO ADVICE                  
         USING TLDVD,R4                                                         
         MVC   TLDVADV,TGADV       SET NEW ADVICE IN RECORD                     
         B     C2PYES                                                           
         DROP  R4                                                               
         SPACE 1                                                                
USE#     DC    H'00'                                                            
SVCOM    DC    XL4'00000000'                                                    
SVADATE  DC    XL6'000000000000'                                                
         SPACE 1                                                                
C2PYES   SR    RC,RC                                                            
C2PNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO ADD TANXD ELEMENTS TO ADVICE RECORD                   
         SPACE                                                                  
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
ADDTANX  NTR1  BASE=*,LABEL=*                                                   
         XR    R2,R2                                                            
         L     R4,AIO              DO NOT EXCEED 8 TANXD ELEMENTS               
         MVI   ELCODE,TANXELQ      ON ONE ADVICE                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
ATANX10  BRAS  RE,NEXTEL                                                        
         BNE   ATANX20                                                          
         AHI   R2,1                                                             
         CHI   R2,8                                                             
         BL    ATANX10                                                          
         B     ADDTANXX                                                         
         SPACE 1                                                                
ATANX20  LA    R3,SRTKLNQ(R3)      PT TO DATA IN SORTREC                        
         CLI   0(R3),TANXELQ       IF DETAILS ELEMENT                           
         BNE   ADDTANXX                                                         
         MVC   ELEMENT,0(R3)                                                    
         BRAS  RE,ELEMADD                                                       
ADDTANXX XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE TO DISPLAY NAMES (FOR NOW)                               
         SPACE                                                                  
DSPNAMES NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              R4=A(HOLD RECORD)                            
         USING TLNXD,R4                                                         
         MVC   TGUSER,TLNXUID                                                   
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SAGNIDNH,TAFNTTTL                           
*                                                                               
         MVC   SAGCLI,TLNXNCLI     SHOW CLIENT CODE AND NAME                    
         OI    SAGCLIH+6,X'80'                                                  
         GOTO1 CHAROUT,DMCB,TAFNELQ,SAGCLINH,TAFNTCLI                           
*                                                                               
         MVC   SAGPRD,TLNXNPRD     SHOW PRODUCT CODE AND NAME                   
         OI    SAGPRDH+6,X'80'                                                  
         GOTO1 CHAROUT,DMCB,TAFNELQ,SAGPRDNH,TAFNTPRD                           
*                                                                               
         MVC   SAGMED,TLNXMED      SHOW MEDIA                                   
         OI    SAGMEDH+6,X'80'                                                  
         MVC   SAGUSE,TLNXUSE      SHOW USE                                     
         OI    SAGUSEH+6,X'80'                                                  
*                                                                               
         MVC   TGDATE,TLNXDATE     MAKE SURE GLOBAS DATE UNCOMPLEMENTED         
         XC    TGDATE,COMPLM                                                    
         GOTO1 DATCON,DMCB,(1,TGDATE),(5,SAGADD)                                
         OI    SAGADDH+6,X'80'                                                  
*                                                                               
         MVI   ELCODE,TANXELQ      AND SHOW USER ID                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4                                                         
         MVC   SAGUID,TANXUID                                                   
         OI    SAGUIDH+6,X'80'                                                  
DSPNAMX  XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
COMPLM   DC    3X'FF'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAT2TACC                                                       
*              ROUTINE TO FILTER DIRECTORY HOOK                                 
         SPACE                                                                  
         DS    0D                                                               
FILTER   NMOD1 0,*FLTR*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         LA    R6,TIKEY                                                         
         USING TLNXD,R6                                                         
         CLI   TLNXSEQ,0           FOR BASE RECORDS THAT ARE                    
         BNE   FLTNO                                                            
         TM    TIKEYST,TLNXSDEL    NOT DELETED                                  
         BO    FLTNO                                                            
         TM    TIKEYST,TLNXSUSD    NOT USED                                     
         BO    FLTNO                                                            
         TM    TIKEYST,TLNXSMAT    AND MATCHED                                  
         BZ    FLTNO                                                            
*                                                                               
         OC    TIFUSE,TIFUSE       IF USE FILTER NOT DEFINED                    
         BNZ   FLT05                                                            
         CLC   TIUSE,=C'CBL'       DO NOT PROCESS CABLE                         
         BE    FLTNO                                                            
         CLC   TIUSE,=C'WSP'       OR WILDSPOT HOLDS                            
         BE    FLTNO                                                            
         CLC   TIUSE,=C'LCB'       OR LOCAL CABLE                               
         BE    FLTNO                                                            
         B     FLT10                                                            
*                                                                               
FLT05    CLC   TIFUSE,TIUSE        IF USE FILTER IS DEFINED                     
         BNE   FLTNO               HOLD RECORD USE MUST MATCH                   
*                                                                               
FLT10    TM    WHEN,X'20'          IF SOON PROCESSING                           
         BO    FLT50               MATCH AGAINST REST OF KEY                    
******** TM    WHEN,X'40'          IF NOW PROCESSING                            
******** BO    FLT50               MATCH AGAINST REST OF KEY                    
*                                  ELSE, FOR OVERNIGHT                          
         OC    PROCAGY,PROCAGY     IF MAIN RUN                                  
         BNZ   FLTYES                                                           
         OC    FILTAGY,FILTAGY     AND NOT FIRST TIME                           
         BZ    FLT20                                                            
         CLC   FILTAGY,TLNXAGY     AND STILL ON THAT AGENCY                     
         BE    *+14                CHECK FLAG                                   
FLT20    MVC   FILTAGY,TLNXAGY     ELSE, SET CURRENT AGENCY                     
         BAS   RE,CKAGY            CHECK OKAY TO PROCESS                        
         CLI   FILTFLAG,C'Y'       RETURN CC                                    
         B     FLTX                                                             
*                                                                               
FLT50    CLC   TGNID,TICID         NETWORK IDS MUST MATCH                       
         BE    FLT60                                                            
         CLI   ADIDFLG,C'Y'        OR IF USING ADID                             
         BNE   FLTNO                                                            
FLT60    CLC   TGADID,TICID        NETWORK ADIDS MUST MATCH                     
         BNE   FLTNO                                                            
         CLC   TGNCLI,TICLI        CLIENT CODES MUST MATCH                      
         BNE   FLTNO                                                            
         CLC   TGNPRD,TIPRD        PRODUCT CODES MUST MATCH                     
         BNE   FLTNO                                                            
         CLC   TGMENAME(1),TIMED   MEDIA MUST MATCH                             
         BNE   FLTNO                                                            
         MVC   SVDATE,TGDATE                                                    
         XC    SVDATE,COMPLM2      MATCH ON COMPLEMENTED DATE                   
         CLC   SVDATE,TLNXDATE     DATES MUST MATCH                             
         BNE   FLTNO                                                            
         CLC   TGUSER,TLNXUID      USER ID'S MUST MATCH                         
         BNE   FLTNO                                                            
         B     FLTYES                                                           
         SPACE                                                                  
FLTYES   SR    RC,RC                                                            
FLTNO    LTR   RC,RC                                                            
FLTX     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK IF AGENCY SHOULD BE PROCESSED                   
*              IN MAIN RUN         XIT - SETS FILTFLAG                          
CKAGY    NTR1                                                                   
         MVI   FILTFLAG,C'N'       DON'T PROCESS THIS AGENCY                    
         MVC   AIO,AIO2            SET IO FOR AGENCY RECORD                     
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',FILTAGY),0                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   AIO,AIO1            RESET IOAREA                                 
         L     R4,AIO2                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL4                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         TM    TAAYSTA3,TAAYSPNX   IF AGENCY PRINTS THEIR OWN                   
         BO    *+8                 DON'T PROCESS IN THIS RUN                    
         MVI   FILTFLAG,C'Y'       SET AGENCY OKAY TO PROCESS                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY  RESET READ SEQUENCE FOR SYSIO                
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FLTX                                                             
         SPACE 2                                                                
COMPLM2  DC    3X'FF'                                                           
         SPACE 2                                                                
         GETELN (R4),DATADISP,ELCODE,4                                          
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              INCREMENT TANPD ELEMENTS TO ADVICE RECORD                        
         SPACE                                                                  
         DS    0D                                                               
BUFTANP  NMOD1 0,*BTNP*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
         L     R3,4(R1)                                                         
         USING SRTRECD,R3          SORT RECORD                                  
*                                                                               
         L     R2,ANXTBUFF         R2=A(NEXT POSITION IN BUFFER)                
         USING BUFFD,R2                                                         
*                                                                               
         LA    R3,SRTKLNQ(R3)      R3=A(DATA AREA OF SORT RECORD)               
         CLI   0(R3),TANXELQ                                                    
         BNE   BUFT10                                                           
         USING TANXD,R3                                                         
         MVI   SVLIFT,0                                                         
         CLI   TANXTYPE,TANXTLFT   IF TALENT COMMERCIAL IS A LIFT               
         BE    *+12                                                             
         CLI   TANXTYPE,TANXTALF                                                
         BNE   BUFT10                                                           
         MVI   SVLIFT,C'Y'         SET MATCHED VIA LIFT ID                      
*                                                                               
         USING TANPD,R3                                                         
BUFT10   CLI   TANPEL,0                                                         
         BE    BUFTX                                                            
         CLI   TANPEL,TANPELQ                                                   
         BE    BUFT20                                                           
         BAS   RE,BUMPEL           BUMP TO NEXT SORT RECORD ELEMENT             
         B     BUFT10                                                           
*                                                                               
BUFT20   CLI   TANPLEN,TANPLNQ3    IF NETWORK USE                               
         BH    BUFT30                                                           
         MVC   BUFFDATE,TANPDATE   USE DATE                                     
         MVC   BUFFPNME,TANPPNME   PROGRAM NAME                                 
         MVC   BUFFNWK,TANPNWK     NETWORK                                      
         MVC   BUFFLFT,SVLIFT      LIFT                                         
         MVC   BUFFSTAT,TANPSTAT   STATUS                                       
         MVC   BUFFFEED,TANPFEED   FEED CODE                                    
         LH    RE,BUFFCNT          ADD TO BUFFER COUNT                          
         LA    RE,1(RE)                                                         
         STH   RE,BUFFCNT                                                       
         CH    RE,=Y(MAXEBUFF)     TEST REACHED TABLE MAX                       
         BNH   *+6                                                              
         DC    H'0'                DIE                                          
         B     BUFT60                                                           
*                                                                               
BUFT30   CLI   TANPTYP,TANPNET     IF CABLE USE                                 
         BNE   BUFT40                                                           
         MVC   BUFFDATE,TANPDATE   USE DATE                                     
         MVC   BUFFCNTR,TANPCNTR   USE COUNTER                                  
         MVC   BUFFLFT,SVLIFT      LIFT                                         
         MVC   BUFFSTAT,TANPSTAT   STATUS                                       
         MVC   BUFFFEED,TANPFEED   FEED CODE                                    
         MVC   BUFFTYP,TANPTYP     TYPE                                         
         MVC   BUFFNTI,TANPNTI     NTI CODE                                     
         CLI   TANPLEN,TANPLNQ4    IF NEW LENGTH, GET MARKET NAME               
         BNE   *+10                                                             
         MVC   BUFFMKTN,TANPMKTN   MARKET NAME                                  
         LH    RE,BUFFCNT          ADD TO BUFFER COUNT                          
         LA    RE,1(RE)                                                         
         STH   RE,BUFFCNT                                                       
         CH    RE,=Y(MAXEBUFF)     TEST REACHED TABLE MAX                       
         BNH   *+6                                                              
         DC    H'0'                DIE                                          
         B     BUFT60                                                           
*                                                                               
BUFT40   CLI   TANPTYP,TANPSPT     IF WILDSPOT USE                              
         BNE   BUFT50                                                           
         MVC   BUFFDATE,TANPDATE   USE DATE                                     
         MVC   BUFFCNTR,TANPCNTR   USE COUNTER                                  
         MVC   BUFFLFT,SVLIFT      LIFT                                         
         MVC   BUFFSTAT,TANPSTAT   STATUS                                       
         MVC   BUFFTYP,TANPTYP     TYPE                                         
         MVC   BUFFMKT,TANPMKT     MARKET CODE                                  
         MVC   BUFFCYCS,TANPCYCS   CYCLE START                                  
         MVC   BUFFCYCE,TANPCYCE   CYCLE END                                    
         CLI   TANPLEN,TANPLNQ4    IF NEW LENGTH, GET MARKET NAME               
         BNE   *+10                                                             
         MVC   BUFFMKTN,TANPMKTN   MARKET NAME                                  
         LH    RE,BUFFCNT          ADD TO BUFFER COUNT                          
         LA    RE,1(RE)                                                         
         STH   RE,BUFFCNT                                                       
         CH    RE,=Y(MAXEBUFF)     TEST REACHED TABLE MAX                       
         BNH   *+6                                                              
         DC    H'0'                DIE                                          
         B     BUFT60                                                           
*                                                                               
BUFT50   CLI   TANPTYP,TANPCSYS    IF CABLE SYSTEM USE                          
         BNE   BUFT60                                                           
         MVC   BUFFDATE,TANPDATE   USE DATE                                     
         MVC   BUFFCNTR,TANPCNTR   USE COUNTER                                  
         MVC   BUFFLFT,SVLIFT      LIFT                                         
         MVC   BUFFSTAT,TANPSTAT   STATUS                                       
         MVC   BUFFTYP,TANPTYP     TYPE                                         
         MVC   BUFFMKT,TANPSYS     SYSTEM CODE                                  
         MVC   BUFFCYCS,TANPCYCS   CYCLE START                                  
         MVC   BUFFCYCE,TANPCYCE   CYCLE END                                    
         CLI   TANPLEN,TANPLNQ4    IF NEW LENGTH, GET MARKET NAME               
         BNE   *+10                                                             
         MVC   BUFFMKTN,TANPMKTN   MARKET NAME                                  
         LH    RE,BUFFCNT          ADD TO BUFFER COUNT                          
         LA    RE,1(RE)                                                         
         STH   RE,BUFFCNT                                                       
         CH    RE,=Y(MAXEBUFF)     TEST REACHED TABLE MAX                       
         BNH   *+6                                                              
         DC    H'0'                DIE                                          
         B     BUFT60                                                           
*                                                                               
BUFT60   BAS   RE,BUMPEL           BUMP TO NEXT SORT RECORD ELEMENT             
         LA    R2,BUFFLNQ(R2)      BUMP TO NEXT BUFFER ENTRY                    
         B     BUFT10              GET NEXT TANPD ELEMENT                       
*                                                                               
BUFTX    ST   R2,ANXTBUFF          SAVE A(NEXT POSITION IN BUFFER)              
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO BUMP TO NEXT SORT RECORD ELEMENT                      
         SPACE                                                                  
BUMPEL   XR    R1,R1                                                            
         ICM   R1,1,TANPLEN                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R1                                                            
         BR    RE                                                               
         DROP  R2,R3                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              ROUTINE TO INITIALIZE BOXES                                      
         SPACE                                                                  
         DS    0D                                                               
INTBOX   NMOD1 0,*INTBX*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         L     R4,ABOX             R4=A(BOXES)                                  
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BR,C'R'                                                          
         XIT1                                                                   
         DROP  R2,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              ROUTINE TO INITIALIZE BOXES FOR CABLE ADVICES                    
         SPACE                                                                  
         DS    0D                                                               
INTBOXC  NMOD1 0,*INTBX*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         L     R4,ABOX             R4=A(BOXES)                                  
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC11,C'C'                                                        
         MVI   BRC,C'R'                                                         
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
*              ROUTINE TO INITIALIZE BOXES FOR WILDSPOT ADVICES                 
         SPACE                                                                  
         DS    0D                                                               
INTBOXW  NMOD1 0,*INTBX*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         L     R4,ABOX             R4=A(BOXES)                                  
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC11,C'C'                                                        
         MVI   BRC,C'R'                                                         
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
*              ROUTINE TO INITIALIZE BOXES FOR LOCAL CABLE ADVICES              
         SPACE                                                                  
         DS    0D                                                               
INTBOXS  NMOD1 0,*INTBX*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         L     R4,ABOX             R4=A(BOXES)                                  
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC11,C'C'                                                        
         MVI   BRC,C'R'                                                         
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
*              ROUTINE TO MARK CURRENT HOLD RECORD USED                         
         USING SRTRECD,R3          R3=A(SORT RECORD)                            
         DS    0D                                                               
WRTHOLD  NMOD1 0,*WRHD*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY             SET KEY OF HOLD RECORD                       
         MVC   KEY(L'SRTHKEY),SRTHKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'SRTHKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                HOLD RECORD MUST EXIST                       
*                                                                               
WRTHOLD1 MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET IT                                       
         GOTO1 ASAVPTRS,DMCB,SVPTRBLK                                           
*                                                                               
         L     R6,AIO                                                           
         USING TLNXD,R6                                                         
         OI    TLNXSTAT,TLNXSUSD   MARK IT USED                                 
         TM    STATUS,WHUNMAT                                                   
         BZ    *+8                                                              
         MVI   TLNXSTAT,0                                                       
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ      GET HOLD DETAILS ELEMENT                     
         BAS   RE,GETEL2                                                        
         BNE   WRTHOLD2                                                         
         USING TANXD,R4                                                         
         MVC   TANXUDTE,TGTODAY1   SET DATE MARKED USED                         
         TM    STATUS,WHUNMAT                                                   
         BZ    WRTHOLD2                                                         
         XC    TANXMDTE(6),TANXMDTE                                             
         XC    TANXCOM,TANXCOM                                                  
         MVI   TANXTYPE,0                                                       
*                                                                               
WRTHOLD2 TM    NOPTION,NTRACE IF TRACE ON                                       
         BNO   WRTHOLD5                                                         
         GOTO1 =A(MYTRAC2),DMCB,(RC),=C'HOLD REC',AIO,0                         
WRTHOLD5 GOTO1 PUTREC                                                           
*                                                                               
         OI    KEY+TLDRSTAT-TLDRKEY,TLNXSUSD                                    
         TM    STATUS,WHUNMAT                                                   
         BZ    *+8                                                              
         MVI   KEY+TLDRSTAT-TLDRKEY,0                                           
         GOTO1 WRITE                                                            
*                                                                               
         MVI   BYTE,X'28'          FORCE ALL                                    
         TM    WHEN,X'40'          IF PROCESSING OVERNIGHT                      
         BO    *+8                                                              
         OI    BYTE,X'01'          SET SEQ IN PASSIVE BASED ON TIME             
         TM    NOPTION,NTRACE                                                   
         BNO   *+8                                                              
         OI    BYTE,X'10'          SET TRACE FOR ADDPTRS                        
         GOTO1 AADDPTRS,DMCB,(BYTE,SVPTRBLK),UPPTRBLK                           
*                                                                               
         TM    WHEN,X'20'          IF SOON PROCESSING                           
         BZ    WRTHOLDX                                                         
******** TM    WHEN,X'40'          IF NOW PROCESSING                            
******** BZ    WRTHOLDX                                                         
         CLI   TLNXSEQ,0           IF BASE REOCRD                               
         BNE   WRTHOLD7                                                         
         XC    KEY,KEY             SET KEY OF HOLD RECORD                       
         MVC   KEY(L'SRTHKEY),SRTHKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'SRTHKEY),KEYSAVE                                           
         BE    WRTHOLD7                                                         
         DC    H'0'                HOLD RECORD MUST EXIST                       
*                                                                               
WRTHOLD7 TM    STATUS,WHUNMAT                                                   
         BO    WRTXIT                                                           
         GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXKEY),KEYSAVE                                     
         BE    WRTHOLD1                                                         
WRTHOLDX MVC   AIO,AIO2                                                         
WRTXIT   XIT1                                                                   
         DROP  R3,R6                                                            
         SPACE 2                                                                
         GETEL2 R4,DATADISP,ELCODE                                              
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              ROUTINE TO ADD COMMENT ELEMENT ON ADVICE RECORD                  
         SPACE                                                                  
         DS    0D                                                               
ADDTAXC  NMOD1 0,*ADCM*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         XC    ELEMENT,ELEMENT     PRE-CLEAR ELEMENT                            
         LA    R4,ELEMENT                                                       
         USING TAXCD,R4                                                         
         BRAS  RE,SETSEQ           SET SEQUENCE NUMBER                          
*                                                                               
         BAS   RE,ADDCMUSR         INFACE XFER FROM XXXXXX ON MMMDDYY           
         CLI   ADDORCHG,C'C'                                                    
         BE    ATAXC10                                                          
         CLI   USAGE,C'Y'          IF NO USAGE CLA RECORD FOUND                 
         BE    *+8                                                              
         BAS   RE,ADDCMCYC         SET NEW CYCLE                                
*                                                                               
         CLI   WRNMSG,C'N'         IF SESSION PAYMENT FOUND                     
         BE    *+8                 DONE                                         
         BAS   RE,ADDCMPAY         CHECK PAY (HLD/BSS, ETC..)                   
*                                                                               
ATAXC10  ZIC   R1,BLOCK            LENGTH                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),BLOCK+1 COMMENT                                      
         AHI   R1,5                                                             
         STC   R1,TAXCLEN          SET ELEMENT LENGTH                           
         BRAS  RE,ELEMADD          ADD NEW OR UPDATED ELEMENT                   
*                                                                               
ADDTAXCX XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE SETS UP FIRST PART OF COMMENT                            
*                                  R2=A(COMMENT)                                
*                                  R1=LENGTH OF COMMENT                         
         SPACE                                                                  
ADDCMUSR NTR1                                                                   
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         AH    R4,DATADISP                                                      
         XR    R1,R1                                                            
*                                                                               
ADDCMU2  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEMENT MUST BE ON RECORD                    
         CLI   0(R4),TANXELQ                                                    
         BE    ADDCMU5                                                          
         ICM   R1,1,1(R4)          BUMP TO NEXT ELEMENT                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R1                                                            
         B     ADDCMU2             LOOP                                         
*                                                                               
         USING TANXD,R4                                                         
ADDCMU5  MVC   BLOCK(L'SPACES),SPACES                                           
         LA    R2,BLOCK+1          R2=A(COMMENT)                                
         SR    R1,R1               LENGTH OF COMMENT                            
*                                                                               
         TM    TANXSTAT,TANXSAGY     IF GENERATED BY TNG                        
         BZ    ADDCMUS7                                                         
         MVC   0(L'LTDATAT,R2),LTDATAT AGENCY TRANSFER FROM                     
         LA    R2,L'LTDATAT(R2)        BUMP TO NEXT POSITION                    
         LA    R1,L'LTDATAT+1(R1)      AND ADD TO LENGTH                        
         B     ADDCMUS9                                                         
*                                                                               
ADDCMUS7 MVC   0(L'LTINFT,R2),LTINFT   ELSE, INT TRANSFER FROM                  
         LA    R2,L'LTINFT(R2)         BUMP TO NEXT POSITION                    
         LA    R1,L'LTINFT+1(R1)       AND ADD LENGTH UPDATE LENGTH             
*                                                                               
ADDCMUS9 MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         MVC   0(L'TANXUID,R2),TANXUID  USER ID                                 
*                                                                               
ADDCMU10 CLI   0(R2),C' '          BUMP PAST IT                                 
         BE    ADDCMU20                                                         
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         B     ADDCMU10                                                         
*                                                                               
ADDCMU20 MVC   1(3,R2),=C'ON '     ON                                           
         LA    R2,3(R2)            BUMP FIELD                                   
         LA    R1,14(R1)           ADD ON + DATE + . + BLANK                    
         STC   R1,BLOCK            SET COMMENT LENGTH                           
*                                                                               
         CLI   ADDORCHG,C'A'                                                    
         BNE   ADDCMU30                                                         
         GOTO1 DATCON,DMCB,(1,TANXADTE),(5,1(R2))                               
         B     ADDCMU40                                                         
*                                                                               
ADDCMU30 GOTO1 DATCON,DMCB,(1,TGTODAY1),(5,1(R2))                               
*                                                                               
ADDCMU40 MVC   9(2,R2),=C'. '                                                   
         B     ADDTAXCX                                                         
         SPACE 2                                                                
*              ROUTINE SETS UP SECOND PART OF COMMENT                           
*                                  R2=A(COMMENT)                                
*                                  R1=LENGTH OF COMMENT                         
         SPACE                                                                  
ADDCMCYC NTR1                                                                   
         LA    R2,BLOCK+1          R2=A(COMMENT)                                
         ZIC   R1,BLOCK            LENGTH SO FAR                                
         AR    R2,R1                                                            
*                                                                               
         MVC   0(L'LTNCYC,R2),LTNCYC                                            
         LA    R1,L'LTNCYC(R1)     SET NEW LENGTH                               
         STC   R1,BLOCK                                                         
         B     ADDTAXCX                                                         
         EJECT                                                                  
*              ROUTINE SETS UP THIRD PART OF COMMENT                            
*                                  R2=A(COMMENT)                                
*                                  R1=LENGTH OF COMMENT                         
         SPACE                                                                  
ADDCMPAY NTR1                                                                   
         LA    R2,BLOCK+1          R2=A(COMMENT)                                
         ZIC   R1,BLOCK            LENGTH SO FAR                                
         AR    R2,R1                                                            
*                                                                               
         MVC   0(12,R2),SPACES     PRE-CLEAR COMMENT AREA                       
*                                                                               
         CLI   WRNMSG,C'S'         UNLESS SESSION NOT FOUND                     
         BE    ACP10                                                            
         MVC   0(L'LTCKPAY,R2),LTCKPAY                                          
         LA    R2,L'LTCKPAY+1(R2)                                               
         MVC   0(3,R2),PTYPE       SET HLD TYPE                                 
         MVI   4(R2),C'.'                                                       
         B     ACP20                                                            
*                                                                               
ACP10    MVC   0(L'LTCKSES,R2),LTCKSES                                          
*                                                                               
ACP20    AHI   R1,13                                                            
         STC   R1,BLOCK            SET NEW LENGTH                               
         B     ADDTAXCX                                                         
         SPACE                                                                  
LTNCYC   DC    C'NEW CYCLE. '                                                   
LTCKPAY  DC    C'CHK PAY'                                                       
LTCKSES  DC    C'CHK SESSION.'                                                  
LTINFT   DC    C'INTERFACE TRANSFER FROM'                                       
LTDATAT  DC    C'DATA TRANSFER FROM'                                            
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
       ++INCLUDE TAUNIOND                                                       
         EJECT                                                                  
*              SET INFO FOR TRACE ROUTINE                                       
         SPACE                                                                  
         DS    0D                                                               
MYTRACE  NMOD1 0,*MYTC*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         L     R2,4(R1)            A(LITERAL)                                   
         L     RF,12(R1)           SET LENGTH OF RECORD                         
         L     R4,8(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
         XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         SPACE 2                                                                
*              SET INFO FOR TRACE ROUTINE                                       
         SPACE                                                                  
         DS    0D                                                               
MYTRAC2  NMOD1 0,*TRC2*                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             GENCON DSECT                                 
*                                                                               
         LM    R2,R3,4(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,4(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
         XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
WORKD    DSECT                                                                  
*                                                                               
         DS    0A                                                               
NLOGOC   DS    A                   A(LOGOC)                                     
NLOGO    DS    A                   A(LOGO)                                      
PRNTBL   DS    A                   A(PRNTBL)                                    
ANXTEL   DS    A                   A(NEXT ELEMENT POSITION IN SRTREC)           
AMKTTAB  DS    A                   A(MARKET TABLE)                              
ABUFFEL  DS    A                   A(BUFFER TABLE)                              
ANXTBUFF DS    A                   A(NEXT POSITION IF BUFFEL)                   
ASRTREC  DS    A                   A(SORT RECORD)                               
*                                                                               
BUFFCNT  DS    H                   BUFFER COUNTER                               
USENUM   DS    H                   USE NUMBER                                   
CNTTANP  DS    H                   USE COUNT                                    
PGNUM    DS    XL2                 PAGE NUMBER                                  
SVBMNTH  DS    XL1                 SAVED BUFFER MONTH OF USE DATE               
NEWBDT   DS    CL1                 CHECKING NEW BUFFER DATE                     
STRTDATE DS    XL3                 USE CYCLE START DATE                         
ENDDATE  DS    XL3                 USE CYCLE END DATE                           
PREVUNT  DS    H                   USE CYCLE PREVIOUS UNITS                     
PREVMAJ  DS    X                   USE CYCLE PREVIOUS MAJORS                    
PREVSUB  DS    XL4                 USE CYCLE PREVIOUS SUBSCRIBERS               
PREVTYP  DS    X                   USE CYCLE LCB TYPE                           
SEQCNT   DS    XL1                 TANPD SEQUENCE COUNT                         
BBSEQCNT DS    XL1                 X'BB' TANPD SEQUENCE COUNT                   
FRSTREC  DS    CL1                 FIRST RECORD FROM SORT                       
*NOWERR  DS    CL1                 Y= HOLD REC TOO LARGE FOR NOW PROC.          
WRNMSG   DS    CL1                 Y= HLD WARNING NECESSARY                     
USAGE    DS    CL1                 Y= FOUND CLA USAGE RECORD                    
ADDORCHG DS    CL1                 A=ADD C=CHANGE                               
AGYOFF   DS    CL1                 TALENT AGENCY OFFICE                         
AGYSTAT  DS    CL1                 TALENT AGENCY STATUS BYTE 1                  
AGYSTAT4 DS    CL1                 TALENT AGENCY STATUS BYTE 4                  
SVLIFT   DS    CL1                 Y=LIFT ID                                    
SVXTYPE  DS    CL1                 SAVED MATCH TYPE                             
*                                                                               
STACOINF DS    0X                                                               
STACOMED DS    CL1                 SAVED COMMERCIAL MEDIA                       
STACOTYP DS    CL1                 SAVED COMMERCIAL TYPE                        
STACOSEC DS    CL1                 SAVED COMMERCIAL LENGTH IN SECONDS           
STACOEXP DS    XL3                 SAVED COMMERCIAL EXPIRATION DATE             
STACOLVI DS    CL12                SAVED LIFT/VERSION ID                        
STACOLVL DS    XL1                 SAVED LIFT/VERSION LENGTH                    
STACOCLG DS    CL6                 SAVED COMMERCIAL CLIENT GROUP                
STACOLNQ EQU   *-STACOINF                                                       
*                                                                               
LSTMONTH DS    XL1                                                              
*                                                                               
NOPTION  DS    XL1                                                              
NTRACE   EQU   X'80'               TRACE RECORDS                                
NOBOX    EQU   X'40'               NO BOXES                                     
*                                                                               
PROCAGY  DS    CL(L'TGAGY)         REQUESTED ONE AGY/USER ID TO PROCESS         
FILTAGY  DS    CL(L'TGAGY)         CURRENT AGENCY     (MAIN RUN ONLY)           
FILTFLAG DS    CL1                 Y=PROCESS THIS AGY (MAIN RUN ONLY)           
PTYPE    DS    CL6                 HLD AND BSS TYPE                             
*                                                                               
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
SVUHKEY  DS    CL(L'TLUHKEY)       SAVED TLUHKEY                                
SVSRTKEY DS    0CL70               SAME ORDER AS KEY OF SORT RECORD             
SVAGY    DS    CL(L'TGAGY)         SAVED AGENCY CODE                            
SVCLI    DS    CL(L'TGCLI)                                                      
SVPRD    DS    CL(L'TGPRD)                                                      
SVTTTL   DS    CL(L'TGNAME)                                                     
SVTCID   DS    CL(L'TGCID)         SAVED TALENT ID                              
SVVERS   DS    CL1                 SAVED VERSION                                
SVMED    DS    CL1                 SAVED MEDIA                                  
SVUSE    DS    CL(L'TGUSCDE)       SAVED USE                                    
*                                                                               
SVCLIN   DS    CL(L'TGNAME)                                                     
SVPRDN   DS    CL(L'TGNAME)                                                     
*                                                                               
AGYNAME  DS    CL36                AGENCY NAME                                  
AGYADDR  DS    0CL99               AGENCY ADDRESS                               
AGYADDR1 DS    CL33                                                             
AGYADDR2 DS    CL33                                                             
AGYADDR3 DS    CL33                                                             
*                                                                               
SVPTRBLK DS    CL(10*L'TLDRREC+1)   SAVED POINTER BLOCK                         
UPPTRBLK DS    CL(10*L'TLDRREC+1)   UPDATED POINTER BLOCK                       
*                                                                               
         DS    0X                  ** WORKING STORAGE ALMOST FULL **            
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
         SPACE                                                                  
SRTRECD  DSECT                                                                  
SRTLEN   DS    XL2                 LENGTH OF RECORD                             
         DS    CL2                 SPARE                                        
*                                                                               
SRTKEY   DS    0X                  START OF SORT KEY                            
SRTAGY   DS    CL6                 TALENT AGENCY                                
SRTCLI   DS    CL6                 TALENT CLIENT                                
SRTPRD   DS    CL6                 TALENT PRODUCT                               
SRTTTL   DS    CL36                TALENT TITLE                                 
SRTTCID  DS    CL12                TALENT COMMERCIAL ID                         
SRTVERS  DS    CL1                 TALENT VERSION                               
SRTMED   DS    CL1                 MEDIA                                        
SRTUSE   DS    CL3                 USE CODE                                     
*                                                                               
SRTHKEY  DS    CL(L'TLNXKEY)       KEY OF HOLD RECORD                           
*                                                                               
SRTCLIN  DS    CL36                                                             
SRTPRDN  DS    CL36                                                             
SRTXTYPE DS    CL1                                                              
SRTKLNQ  EQU   *-SRTRECD                                                        
         SPACE                                                                  
*              DSECT TO COVER ABUFFEL                                           
         SPACE                                                                  
BUFFD    DSECT                                                                  
BUFFDATE DS    XL3                 USE DATE                                     
BUFFPNME DS    CL15                PROGRAM NAME                                 
BUFFNWK  DS    CL1                 NETWORK                                      
         ORG   BUFFNWK                                                          
BUFFCNTR DS    XL1                 COUNTER                                      
BUFFLFT  DS    CL1                 LIFT                                         
BUFFSTAT DS    XL1                 STATUS                                       
BUFFFEED DS    XL4                 FEED CODE                                    
BUFFTYP  DS    XL1                 TYPE                                         
BUFFMKT  DS    CL4                 MARKET                                       
         ORG   BUFFMKT                                                          
BUFFNTI  DS    CL4                 NTI CODE                                     
         ORG   BUFFMKT                                                          
BUFFCSYS DS    CL6                 CABLE SYSTEM                                 
         DS    CL1                                                              
BUFFCYCS DS    XL3                 CYCLE START                                  
BUFFCYCE DS    XL3                 CYCLE END                                    
BUFFMKTN DS    CL15                MARKET NAME                                  
BUFFLNQ  EQU   *-BUFFD                                                          
         SPACE                                                                  
*              DSECT TO COVER COMELTAB                                          
         SPACE                                                                  
COMELTBD DSECT                                                                  
COMELCD  DS    XL1                 ELEMENT CODE                                 
COMELLNQ EQU   *-COMELTBD                                                       
         EJECT                                                                  
*              DSECT TO COVER MKTTAB                                            
         SPACE                                                                  
MKTTABD  DSECT                                                                  
MKTCOD   DS    CL6                 MARKET CODE                                  
MKTSPACE DS    CL1                 SPACE                                        
MKTNAM   DS    CL15                MARKET NAME                                  
MKTTBLNQ EQU   *-MKTTABD                                                        
         EJECT                                                                  
*              DSECT TO PRINT LINE                                              
PRNTD    DSECT                                                                  
         DS    CL7                 CENTERED                                     
BL       DS    CL1                                                              
PRTCID   DS    CL17                TALENT ID                                    
         DS    CL1                                                              
PRTCONT  DS    CL11                (CONTINUED)                                  
         DS    CL5                                                              
BC1      DS    CL1                                                              
         DS    CL1                                                              
PRTMED   DS    CL1                 MEDIA                                        
         DS    CL1                                                              
BC2      DS    CL1                                                              
PRTUSE   DS    CL3                 USE CODE                                     
BC3      DS    CL1                                                              
PRTADV   DS    CL6                 ADVICE CODE                                  
BC4      DS    CL1                                                              
PRTCYC   DS    CL17                ADVICE CYCLE                                 
BC5      DS    CL1                                                              
PRTWRN   DS    CL17                WARNING                                      
BC6      DS    CL1                                                              
PRTUSEN  DS    CL4                 USE NUMBER                                   
BC7      DS    CL1                                                              
PRTUSEDT DS    CL8                 USE DATE                                     
BC8      DS    CL1                                                              
PRTUPGM  DS    CL15                USE PROGRAM NAME                             
BC9      DS    CL1                                                              
         DS    CL1                                                              
PRTULFT  DS    CL1                 USE LIFT                                     
         DS    CL1                                                              
BC10     DS    CL1                                                              
         DS    CL1                                                              
PRTUNWK  DS    CL1                 USE NETWORK                                  
         DS    CL1                                                              
BR       DS    CL1                                                              
         ORG   PRTUSEN                                                          
PRTMDATE DS    CL8                 DATE                                         
         DS    CL1                                                              
BC11     DS    CL1                                                              
PRTNTI   DS    CL4                 NTI CODE                                     
         DS    CL1                                                              
         ORG   PRTNTI                                                           
PRTMKT   DS    CL4                 MARKET CODE                                  
         DS    CL1                                                              
         ORG   PRTMKT                                                           
PRTCSYS  DS    CL6                 CABLE SYSTEM CODE                            
         DS    CL1                                                              
BRC      DS    CL1                                                              
         SPACE 2                                                                
*              DSECT TO COVER 2ND PRINT LINE                                    
         SPACE                                                                  
PRNT2D   DSECT                                                                  
         DS    CL7                 CENTERED                                     
         DS    CL1                                                              
PRTCIDN  DS    CL34                TALENT TITLE                                 
         SPACE 2                                                                
*              DSECT TO COVER 3RD PRINT LINE                                    
         SPACE                                                                  
PRNT3D   DSECT                                                                  
         DS    CL7                 CENTERED                                     
         DS    CL1                                                              
PRTVERSH DS    CL7                 VERSION HEADER                               
         DS    CL1                                                              
PRTVERSL DS    CL3                 VERSION LETTER                               
         DS    CL1                                                              
PRTVERSI DS    CL16                VERSION ID/LENGTH                            
         DS    CL22                                                             
PRTWRN3  DS    CL33                                                             
         EJECT                                                                  
               DSECT TO COVER NETWORK TABLE                                     
         SPACE                                                                  
NWKTABD  DSECT                                                                  
NTLETTER DS    C                   NETWORK LETTER                               
NTUSEEQU DS    X                   NETWORK LATE NIGHT USE EQU                   
NTUSEDES DS    CL3                 USE DESCRIPTION                              
         DS    X                                                                
NTUSE#   DS    H                   USES IN THIS CYCLE                           
NTCYCDTS DS    XL6                 CYCLE BEGIN AND END DATES                    
NTSVCOM  DS    XL4                 CURRENT COMMERCIAL                           
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
*TAREPFFD                                                                       
*TAREPC1D                                                                       
*DDGENTWA                                                                       
*DDTWACOND                                                                      
*DMDTFIS                                                                        
*DDMASTD                                                                        
*DMWRKRK                                                                        
*DDSPOOLD                                                                       
*DDLOGOD                                                                        
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE TAREPC2D                                                       
         ORG SAGWORK                                                            
MYGETRET DS    A                                                                
ATRPACK  DS    A                                                                
STATUS   DS    XL1                 STATUS FOR PFKEY                             
PFKPEND  EQU   X'80'               PFKEY 13 PENDING                             
SMAXTANP EQU   X'40'                                                            
NEWMONTH EQU   X'20'                                                            
WHUNMAT  EQU   X'10'                                                            
ADDNOFEL EQU   X'08'                                                            
SVDATE   DS    XL3                 DATE FOR (UN)COMPLEMENTING TGDATE            
LNSTATUS DS    XL4                 LATE NIGHT STATUS                            
TOTCNT   DS    H                   TOTAL COUNT FOR CYCLE                        
SVCYCLE  DS    XL6                 SAVED CLASS A CYCLE                          
GNPAXCLA DS    X                                                                
GNPAXSTA EQU   X'80'                                                            
GNCLASTA EQU   X'40'                                                            
SVDTS    DS    XL6                                                              
SVCYC    DS    XL6                                                              
GETALPH  DS    C                                                                
UNITCALC DS    F                                                                
SVUSEQU  DS    XL(L'TGUSEQU)                                                    
SVUSCDE  DS    XL(L'TGUSCDE)                                                    
USE1     DS    X                                                                
TYPE1    DS    X                                                                
USE2     DS    X                                                                
TYPE2    DS    X                                                                
REGDATE  DS    XL6                                                              
MUSDATE  DS    XL6                                                              
SVCMT    DS    CL70                                                             
SVELEM   DS    CL(L'ELEMENT)                                                    
LASTCODE DS    CL(L'TAMTCODE)                                                   
CYCLEN   DS    X                                                                
LAGEND   DS    XL3                                                              
SORTLEN  DS    XL(L'TANPLEN)                                                    
CSTOVER  DS    C                                                                
ADIDFLG  DS    C                                                                
SPLITADV DS    C                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'186TAREP42   03/09/17'                                      
         END                                                                    
