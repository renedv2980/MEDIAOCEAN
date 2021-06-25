*          DATA SET REWRIIO    AT LEVEL 018 AS OF 05/01/02                      
*PHASE T82111A,*                                                                
***********************************************************************         
*                                                                     *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - WORK REG                                                *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG                                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER                    *         
*        R7 - POINTER TO REWRIIOD                                     *         
*        R8 - SECOND BASE REG                                         *         
*        R9 - THIRD BASE REG                                          *         
*        RA - FOURTH BASE REG                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO MYD                                          *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
* LEV 47     JUL17/92 BYPASS BAD BLAIR CONTRACT 3034349 PWC REC       *         
* LEV 48     JUL20/92 BAD BLAIR CONTRACT 3034349 PWC REC IS FIXED     *         
* LEV 49     SEP14/94 ADD ADVERTISER NAME AND POINT PERSON KEYWORDS   *         
* LEV 51     NOV02/94 READ HN/DI PWC RECORDS IF NEEDED                *         
* LEV 56     INCREASE STATION OWNER TABLE SIZE                        *         
*                                                                     *         
* NOV10/95 (BU ) ---  CHANGE REGENALL -> REGENALL1 FOR 2K CONTRACTS   *         
*                                                                     *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
         TITLE 'T82111 - WRIIO - REP WRITER I/O CONTROLLER'                     
         PRINT NOGEN                                                            
REWRIIO  CSECT                                                                  
         NMOD1 (MYEND-MYD),**WRIO*,R8,R9,RA,CLEAR=YES                           
         USING MYD,RC                                                           
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         L     R7,0(R1)                                                         
         USING REWRIIOD,R7                                                      
         BAS   RE,RELINIT                                                       
         BAS   RE,SETLIM           SET LIMITS FOR OFFICE ACCESS                 
         BAS   RE,SETREAD          SET MAIN AND SUB READ FROM REREAD            
         BAS   RE,SETKEY           SET START KEY                                
         SPACE                                                                  
         LA    R1,READADDS         GET ADDRESS OF READ ROUTINE                  
         SPACE                                                                  
REWRIIO2 CLC   MAINREAD,0(R1)                                                   
         BE    REWRIIO4                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   REWRIIO2                                                         
         MVI   REERROR,RENOROUT    NO READ ROUTINE FOR THIS RECORD              
         B     XIT                                                              
         SPACE                                                                  
REWRIIO4 NI    STATUS,X'FF'-FOUNDONE                                            
         SPACE                                                                  
         LA    R4,KEY              PRESET A SIMPLE STARTING KEY                 
         XC    KEY,KEY                                                          
         MVC   KEY(1),MAINREAD                                                  
         L     RF,0(R1)            PICK UP ROUTINE DISPLACEMENT                 
         AR    RF,RB               ADD A(BEGIN OF WRIIO)                        
         BASR  RE,RF               AND EXECUTE                                  
         SPACE                                                                  
         TM    STATUS,FOUNDONE     DID WE PASS A RECORD TO APPLIC.              
         BO    REWRIIOX                                                         
         OC    REQSKEY,REQSKEY     TEST CONTINUE KEY PRESENT                    
         BZ    REWRIIOX                                                         
         XC    REQSKEY,REQSKEY     CLEAR CONTINUE KEY                           
         B     REWRIIO4            AND START OVER                               
         SPACE                                                                  
REWRIIOX XC    REKEY,REKEY         RELEASE WRIIO'S KEY                          
*                                  SHOW THAT WRIIO NO LONGER DOING IO           
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION CODE                                              
         SPACE 3                                                                
RELINIT  NTR1                                                                   
         L     R1,REACOMFC         COMFACS ADDRESSES                            
         USING COMFACSD,R1                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   HEXOUT,CHEXOUT                                                   
         L     RF,CCALLOV          GET ADDRESS OF SQUASHER                      
         GOTO1 (RF),DMCB,0,X'D9000A0D'                                          
         MVC   SQUASHER,0(R1)                                                   
         SPACE                                                                  
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         SPACE                                                                  
         MVC   DIRNAME,=CL8'REPDIR'                                             
         MVC   FILENAME,=CL8'REPFIL'                                            
         SPACE                                                                  
*                                  SEED DUMP FOR LEGIBILITY                     
         MVC   REWRIIOD,=CL8'*WRIIOD*'                                          
         MVC   REFILTSH,=CL8'**FILTS*'                                          
         MVC   REMODEH,=CL8'**MODE**'                                           
         MVC   READDSH,=CL8'**ADDS**'                                           
         MVC   REKEYH,=CL8'**REKEY*'                                            
         MVC   RECODESH,=CL8'**CODES*'                                          
         MVC   RESUBS,=CL8'**SUBS**'                                            
         B     XIT                                                              
         EJECT                                                                  
*              SET UP LIMIT ACCESS                                              
         SPACE 3                                                                
SETLIM   NTR1                                                                   
         SPACE                                                                  
* CODE OFFICE LIMITS HERE LATER *                                               
         SPACE                                                                  
         B     XIT                                                              
         SPACE                                                                  
*              FILTER ?                                                         
         SPACE 3                                                                
SETREAD  NTR1                                                                   
         MVC   MAINREAD,REREAD     USUALLY SAME AS REREAD                       
         MVI   SUBREAD,0           WITH NO SUB READ                             
         SPACE                                                                  
         XC    KEY,KEY             GET PASSIVE FOR INT COMM NO.                 
         LA    R4,KEY                                                           
         BAS   RE,HIGH                                                          
         SPACE                                                                  
         B     SETRX                                                            
         SPACE                                                                  
SETRX    DS    0H                                                               
         B     XIT                                                              
         SPACE 3                                                                
SETKEY   NTR1                                                                   
         ICM   RF,15,REKHOOK       TEST USER HOOK TO SET KEY                    
         BZ    XIT                                                              
         GOTO1 ,DMCB,(MAINREAD,REQSKEY) BUILD PARAMETER LIST                    
         L     RE,USERRD           RE=USER'S RD                                 
         L     RC,68(RE)           RESTORE USER'S W/S REGISTER                  
         BASR  RE,RF               OFF TO USER                                  
         B     XIT                                                              
         EJECT                                                                  
*              SET ACTUAL KEYS TO BE READ  (ALWAYS READ CONTRACTS)              
         SPACE                                                                  
CON      DS    0H                  CONTRACT                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCONKEY,R4                                                       
         MVI   RCONKTYP,RCONKTYQ                                                
         MVC   RCONKREP,REREP                                                   
         MVI   LKEY,3                                                           
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE                                                                  
CONNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    RECODES,RECODES     ZERO CODES FROM KEY AND RECORD               
         XC    REDATES,REDATES     ZERO DATES FROM REC AND PWC                  
         XC    REAPWC,REAPWC                                                    
         SPACE                                                                  
         BAS   RE,POSTKEY          SET CODES FROM KEY                           
         SPACE                                                                  
         TM    REQTABLE,REQSTOW    IS OWNER/RANK/TVB REQUIRED                   
         BZ    *+8                                                              
         BAS   RE,STOWN            GO FIND STATION OWNER/RANK/TVB               
         SPACE                                                                  
         TM    REQTABLE,REQOFRG    IS REGION REQUIRED                           
         BZ    *+8                                                              
         BAS   RE,OFRT                                                          
         SPACE                                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CONNEXT                                                          
         SPACE                                                                  
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,POSTPOST         SPECIAL AFTER-POST ROUTINES                  
         SPACE                                                                  
         UNPK  WORK(9),RECON(5)                                                 
         MVC   RECONZD,WORK                                                     
         LA    RE,RECONZD                                                       
         LA    RF,L'RECONZD                                                     
         CLI   0(RE),C'0'                                                       
         BNE   *+18                                                             
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
         DC    H'0'                                                             
         SPACE                                                                  
         SPACE                                                                  
         TM    REQTABLE,REQCTCL    IS CLASS REQUIRED                            
         BZ    *+8                                                              
         BAS   RE,CTCL                                                          
         SPACE                                                                  
CON06    BAS   RE,FILTREC          FILTER ON RECORD CODES                       
         BNE   CONNEXT                                                          
         SPACE                                                                  
         BAS   RE,FILTDATE         FILTER ON DATES                              
         BNE   CONNEXT                                                          
         SPACE                                                                  
         TM    REQTABLE,REQSPPP    IS POINT PERSON REQUIRED                     
         BZ    CON08                                                            
         BAS   RE,PTPL                                                          
         BNE   CONNEXT                                                          
         SPACE                                                                  
CON08    DS    0H                                                               
         TM    REQFLAGS,REQPWCR    NEED TO READ PWC RECS                        
         BZ    CON30                                                            
*                                                                               
*  ROIOPEN  -- OPEN ROI DIR/FIL FOR READING (NO UPDATE)                         
*                                                                               
         TM    REQFLAGS,REQPWCOP                                                
         BO    CON10                                                            
         OI    REQFLAGS,REQPWCOP                                                
         SPACE                                                                  
         LA    R2,ROIFLSTR         READ-ONLY FILE LIST                          
         LA    R3,IO2                                                           
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',(R2),(R3)                        
         EJECT                                                                  
*             PAPER WORK RECORDS READ IF TOTALS ARE REQUIRED *                  
         SPACE                                                                  
CON10    MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RPWCKEY,R4                                                       
         MVI   RPWCKTYP,RPWCKTYQ                                                
         MVC   RPWCKREP,REREP                                                   
         MVC   RPWCKCDE,RECON                                                   
         OI    STATUS,ROIREC                                                    
         BAS   RE,HIGH                                                          
         SPACE                                                                  
         CLC   RPWCKEY,KEYSAVE                                                  
         BE    CON15                                                            
*                                                                               
* SPECIAL FOR REP D4: D4 WAS THE RESULT OF A MERGE BETWEEN HN AND DI            
* IN 11/93. ALL CONTRACT NUMBERS FOR HN WERE BUMPED UP BY 100,000 WHILE         
* THE CONTRACT NUMBERS FOR DI REMAINED THE SAME. HOWEVER, THIS WAS NOT          
* UPDATED IN THE HN PWC RECORDS. THEREFORE, FOR D4 CONTRACTS WITHIN THE         
* RANGE 122,031 TO 196,708, IF THE PWC RECORD IS NOT FOUND, WE WILL             
* SUBTRACT THE CONTRACT NUMBER BY 100,000 AND CHECK IF IT IS UNDER HN.          
* ALSO, IF THE CONTRACT NUMBER IS LESS THAN 68,512, WE'LL CHECK TO SEE          
* THE IF THE PWC REC IS UNDER DI.                                               
                                                                                
         CLC   =C'D4',REREP        MUST BE D4                                   
         BNE   CON20                                                            
                                                                                
         XC    KEY,KEY                                                          
         MVI   RPWCKTYP,RPWCKTYQ                                                
                                                                                
         CLC   RECON,=X'00068512'                                               
         BH    CON12                                                            
         MVC   RPWCKREP,=C'DI'                                                  
         MVC   RPWCKCDE,RECON                                                   
         B     CON13                                                            
                                                                                
CON12    DS    0H                                                               
         CLC   RECON,=X'00122031'  MUST FALL IN RANGE                           
         BL    CON20                                                            
         CLC   RECON,=X'00196708'                                               
         BH    CON20                                                            
         MVC   RPWCKREP,=C'HN'                                                  
                                                                                
         ZAP   WORK(5),=P'0'                                                    
         ZAP   WORK+5(5),=P'0'                                                  
         MVO   WORK(5),RECON(4)                                                 
         SP    WORK(5),=P'100000'                                               
         MVO   WORK+5(5),WORK(5)                                                
         MVC   RPWCKCDE,WORK+5                                                  
                                                                                
CON13    DS    0H                                                               
         OI    STATUS,ROIREC                                                    
         BAS   RE,HIGH                                                          
                                                                                
         CLC   RPWCKEY,KEYSAVE                                                  
         BNE   CON20                                                            
*                                                                               
* BYPASS BAD CONTRACT FOR BLAIR WITH OVERRUN 03 ELEM CLOBBERING 04 ELEM         
*                                                                               
*        CLC   RPWCKEY,BADBLR                                                   
*        BE    CON20                                                            
                                                                                
CON15    DS    0H                                                               
         BAS   RE,GETREC                                                        
         SPACE                                                                  
         LA    R6,IO2+38                                                        
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPWCEL01,R6                                                      
         MVC   REPWCWDT,RPWCCWOD                                                
         DROP  R6                                                               
         SPACE                                                                  
         LA    R0,IO2                                                           
         ST    R0,REAPWC                                                        
         SPACE                                                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),02            BUY ACT ELEM                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         LR    R4,R0                                                            
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         MH    R0,=H'7'                                                         
         GOTO1 RADDAY,DMCB,REPWCWDT,REPCENDT,(R0)                               
         AR    R6,R4                                                            
         CLI   0(R6),03            CONTRACT ACT ELEM                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLM   R4,1,1(R6)                                                       
         BNL   CON18                                                            
         ZIC   R0,1(R6)                                                         
         LR    R4,R0                                                            
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         MH    R0,=H'7'                                                         
         GOTO1 RADDAY,DMCB,REPWCWDT,REPCENDT,(R0)                               
         SPACE                                                                  
CON18    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),04            SALES ACT ELEM                               
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         LR    R4,R0                                                            
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         MH    R0,=H'7'                                                         
         GOTO1 RADDAY,DMCB,REPWCWDT,REPSENDT,(R0)                               
         SPACE                                                                  
CON20    NI    STATUS,X'FF'-ROIREC                                              
         MVC   KEY,PRIMEKEY                                                     
         MVC   KEYSAVE,PRIMESAV                                                 
         BAS   RE,HIGH                                                          
         SPACE                                                                  
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
CON30    BAS   RE,GOHOOK                                                        
         B     CONNEXT                                                          
BADBLR   DC    X'2F',XL24'00',CL2'BL',X'03034349'                               
         DROP  R4                                                               
         EJECT                                                                  
* FIND STATION OWNER & RANK FROM TABLE                                          
         SPACE                                                                  
STOWN    DS    0H                                                               
         SPACE                                                                  
         ZIC   R3,REOWNTAB         TABLE SIZE                                   
         SLL   R3,2                TIMES 4                                      
         SR    R2,R2                                                            
         ICM   R2,7,REOWNTAB+1     ADDRESS                                      
         USING STADBLD,R2                                                       
         SPACE                                                                  
STOW10   CLC   RESTA,0(R2)                                                      
         BE    STOW20                                                           
         LA    R2,STDLENE(R2)                                                   
         CLI   0(R2),C' '                                                       
         BNH   STOW14                                                           
         BCT   R3,STOW10                                                        
STOW14   BR    RE                                                               
         SPACE                                                                  
STOW20   MVC   REOWN,STDOWN                                                     
         MVC   RERNK,STDRNK                                                     
         MVC   RETVB,STDTVB                                                     
         MVC   REMCD,STDMCD                                                     
         BR    RE                                                               
         DROP  R2                                                               
         SPACE                                                                  
* FIND REGION FROM TABLE                                                        
         SPACE                                                                  
OFRT     ZIC   R3,REREGTAB         TABLE SIZE                                   
         SR    R2,R2                                                            
         ICM   R2,7,REREGTAB+1     TABLE ADDRESS                                
         SPACE                                                                  
OFRG10   CLC   REOFF,0(R2)                                                      
         BE    OFRG20                                                           
         LA    R2,L'REOFF+L'REREG(,R2)                                          
         CLI   0(R2),C' '                                                       
         BNH   OFRG14                                                           
         BCT   R3,OFRG10                                                        
OFRG14   DC    H'0'                                                             
         SPACE                                                                  
OFRG20   MVC   REREG,L'REOFF(R2)                                                
         BR    RE                                                               
         SPACE                                                                  
* FIND CLASS FROM TABLE                                                         
         SPACE                                                                  
CTCL     ZIC   R3,RECLSTAB                                                      
         SR    R2,R2                                                            
         ICM   R2,7,RECLSTAB+1                                                  
         SPACE                                                                  
CTCL10   CLC   RECTG,0(R2)                                                      
         BE    CTCL20                                                           
         LA    R2,L'RECTG+L'RECLS(,R2)                                          
         CLI   0(R2),C' '                                                       
         BNH   CTCL14                                                           
         BCT   R3,CTCL10                                                        
CTCL14   DC    H'0'                                                             
         SPACE                                                                  
CTCL20   MVC   RECLS,L'RECTG(R2)                                                
         BR    RE                                                               
         EJECT                                                                  
* FIND POINT PERSON FROM PRODUCT RECORD                                         
         SPACE                                                                  
PTPL     NTR1                                                                   
         OC    REPRD,REPRD         ONLY FOR PRODUCT CODE                        
         BZ    PTPLNOK                                                          
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RPRDREC,R4                                                       
         MVI   RPRDKTYP,X'09'      GET PRODUCT RECORD                           
         MVC   RPRDKADV,READV                                                   
         MVC   RPRDKPRD,REPRD                                                   
         MVC   RPRDKREP,REREP                                                   
         DROP  R4                                                               
                                                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BE    PTPL10                                                           
PTPL08   MVC   KEY,SVKEY           RE-ESTABLISH SEQ ORDER                       
         BAS   RE,HIGH                                                          
         B     PTPLNOK                                                          
                                                                                
PTPL10   DS    0H                                                               
         BAS   RE,GETREC                                                        
                                                                                
         LA    R6,IO                                                            
         USING RPRDREC,R6                                                       
         MVI   ELCODE,X'02'        NO 02 ELEM IF NO NTWK CONTRACT NO            
         BAS   RE,GETEL                                                         
         BE    PTPL12                                                           
         MVC   REPTP,=C'???'       NO POINT PERSON                              
         B     PTPL14              RESET SEQ READ                               
PTPL12   LA    R6,IO                                                            
         OC    RPRDNPNT,RPRDNPNT                                                
         BZ    PTPL08              NOT OK RESTORE SEQ READ                      
         CLC   RPRDNPNT,SPACES                                                  
         BE    PTPL08              NOT OK RESTORE SEQ READ                      
         MVC   REPTP,RPRDNPNT                                                   
         DROP  R6                                                               
                                                                                
PTPL14   MVC   KEY,SVKEY           RE-ESTABLISH SEQ ORDER                       
         BAS   RE,HIGH                                                          
         BAS   RE,GETREC           PUT CONTRACT BACK IN IO AREA                 
                                                                                
PTPLOK   SR    R1,R1                                                            
         B     *+8                                                              
PTPLNOK  LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
*              READ ALL OFFICE RECS, SAVING OFFICE AND REGION *                 
         SPACE                                                                  
OFF      DS    0H                  OFFICE                                       
         SPACE                                                                  
         ZIC   R3,REREGTAB         TABLE SIZE                                   
         SR    R2,R2                                                            
         ICM   R2,7,REREGTAB+1     TABLE ADDRESS                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ROFFKEY,R4                                                       
         MVI   ROFFKTYP,ROFFKTYQ                                                
         MVC   ROFFKREP,REREP                                                   
         MVI   LKEY,24                                                          
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE                                                                  
OFFNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         SPACE                                                                  
         MVC   REOFF,ROFFKOFF      SET OFFICE FROM KEY                          
         SPACE                                                                  
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         SPACE                                                                  
         MVC   0(L'REOFF,R2),REOFF                                              
         MVC   L'REOFF(L'REREG,R2),REREG                                        
         LA    R2,L'REOFF+L'REREG(,R2)                                          
         SPACE                                                                  
         BCT   R3,OFFNEXT                                                       
         DC    H'0'                MAKE OFFREGTB LARGER IN REWRIDRV             
         DROP  R4                                                               
         EJECT                                                                  
*              READ ALL CATEGORY RECS, SAVING CATEGORY AND CLASS *              
         SPACE                                                                  
CTG      DS    0H                  CATEGORY                                     
         ZIC   R3,RECLSTAB                                                      
         SR    R2,R2                                                            
         ICM   R2,7,RECLSTAB+1                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCTGKEY,R4                                                       
         MVI   RCTGKTYP,RCTGKTYQ                                                
         MVC   RCTGKREP,REREP                                                   
         MVI   LKEY,24                                                          
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE                                                                  
CTGNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         SPACE                                                                  
         MVC   RECTG,RCTGKCTG      SET CATEGORY FROM KEY                        
         SPACE                                                                  
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         SPACE                                                                  
         SPACE                                                                  
         MVC   0(L'RECTG,R2),RECTG                                              
         MVC   L'RECTG(L'RECLS,R2),RECLS                                        
         LA    R2,L'RECTG+L'RECLS(,R2)                                          
         SPACE                                                                  
         BCT   R3,CTGNEXT                                                       
         DC    H'0'                MAKE CATCLSTB LARGER IN REWRIDRV             
         DROP  R4                                                               
         EJECT                                                                  
*              READ ALL STATION RECS, SAVING STATION OWNER/RANK/TVB *           
         SPACE                                                                  
STA      DS    0H                  STATION                                      
         ZIC   R3,REOWNTAB                                                      
         SLL   R3,3                TIMES 8                                      
         SR    R2,R2                                                            
         ICM   R2,7,REOWNTAB+1                                                  
         USING STADBLD,R2                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,RSTAKTYQ                                                
         MVC   RSTAKREP,REREP                                                   
         MVI   LKEY,21                                                          
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE                                                                  
STANEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         SPACE                                                                  
         MVC   RESTA,RSTAKSTA      SET CATEGORY FROM KEY                        
         SPACE                                                                  
         BAS   RE,GETREC                                                        
*********************************************************                       
         OC    REFOWN,REFOWN       FILTERING ON OWNER?                          
         BZ    *+14                                                             
         CLC   REFOWN,REOWN        ONLY TABLE OWNER'S STATIONS                  
         BNE   STANEXT                                                          
*********************************************************                       
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         SPACE                                                                  
         MVC   STDSTA,RESTA                                                     
         MVC   STDOWN,REOWN                                                     
         MVC   STDRNK,RERNK                                                     
         MVC   STDTVB,RETVB                                                     
         MVC   STDMCD,REMCD                                                     
***      GOTO1 =V(PRNTBL),DMCB,=C'STT',0(R2),C'DUMP',15,=C'1D'                  
         LA    R2,STDLENE(,R2)                                                  
         SPACE                                                                  
         BCT   R3,STANEXT                                                       
         DC    H'0'                MAKE STAOWNTB LARGER IN REWRIDRV             
         DROP  R4,R2                                                            
         EJECT                                                                  
* READ ALL OTHER RECORDS BY BUILDING KEY, THEN POSTING REC INFO *               
         SPACE                                                                  
OTH      BAS   RE,BLDKEY           GO BUILD KEY FOR REC                         
         BNE   XIT                  IF NOT FOUND, GET OUT                       
         SPACE                                                                  
         BAS   RE,POSTREC                                                       
         CLI   KEY,RGRPKTYQ        THIS GROUP RECORD                            
         BNE   XIT                                                              
         CLI   RESUB,0             GROUP ONLY                                   
         BNE   XIT                                                              
         MVC   RENAME+L'RGRPNAME(L'RENAME-L'RGRPNAME),SPACES                    
         B     XIT                                                              
         SPACE                                                                  
* BUILD KEYS FOR ALL RECORDS EXCEPT CONTRACT AND PAPER WORK CTS *               
         SPACE                                                                  
BLDKEY   NTR1                                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R3,R3               USED AS FLAG - NON-ZERO IF HIT               
         LA    R2,KEYBLD                                                        
         XC    KEY+1(L'KEY-1),KEY+1                                             
BLDK10   CLC   0(1,R2),KEY                                                      
         BE    BLDK20                                                           
         BH    BLDK30                                                           
BLDK14   LA    R2,L'KEYBLD(,R2)                                                 
         B     BLDK10                                                           
         SPACE                                                                  
BLDK20   LA    RE,KEY                                                           
         ICM   R0,1,1(R2)          DISP INTO KEY                                
         AR    RE,R0                                                            
         SPACE                                                                  
         LA    RF,RECODES                                                       
         ICM   R0,1,2(R2)          DISP INTO CODES                              
         AR    RF,R0                                                            
         ZIC   R1,3(R2)            LEN-1                                        
         EX    R1,BLDKMVC                                                       
         BCTR  R3,0                SET HIT FLAG                                 
         B     BLDK14                                                           
BLDKMVC  MVC   0(0,RE),0(RF)                                                    
BLDK30   LTR   R3,R3               GET A HIT?                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         BAS   RE,HIGH                                                          
         SPACE                                                                  
         CLC   KEY(27),KEYSAVE                                                  
         BE    BLDK40                                                           
         SPACE                                                                  
         CLI   KEY,RGRPKTYQ        THIS GROUP RECORD                            
         BNE   BLDK36                                                           
         CLI   RESUB,0             GROUP ONLY                                   
         BNE   XIT                                                              
         CLC   KEY(26),KEYSAVE                                                  
         BNE   XIT                                                              
         B     BLDK40                                                           
         SPACE                                                                  
BLDK36   CLI   KEY,RAGYKTYQ        THIS AGENCY RECORD                           
         BNE   XIT                                                              
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+RAGYKAOF-RAGYKEY(L'RAGYKAOF),SPACES                          
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(27),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE                                                                  
BLDK40   BAS   RE,GETREC                                                        
         CR    R0,R0                                                            
         B     XIT                                                              
         EJECT                                                                  
* POST CODES FROM KEY (FIRST LOOK UP WHICH KEY IS USED)                         
         SPACE                                                                  
POSTKEY  NTR1                                                                   
         SR    R0,R0                                                            
         SR    R1,R1               USED AS FLAG - NON-ZERO IF HIT               
         LA    R2,KEYTAB                                                        
POSTK10  CLC   0(1,R2),KEY                                                      
         BH    POSTK30                                                          
         BE    POSTK20                                                          
POSTK14  LA    R2,L'KEYTAB(,R2)                                                 
         B     POSTK10                                                          
         SPACE                                                                  
POSTK20  ZIC   R1,1(R2)            LEN                                          
         LA    RE,KEY                                                           
         ICM   R0,1,2(R2)          DISP INTO KEY                                
         AR    RE,R0                                                            
         SPACE                                                                  
         LA    RF,RECODES                                                       
         ICM   R0,1,3(R2)          DISP INTO CODES                              
         AR    RF,R0                                                            
         EX    R1,POSTKMVC                                                      
         B     POSTK14                                                          
POSTKMVC MVC   0(0,RF),0(RE)                                                    
POSTK30  LTR   R1,R1               GET A HIT?                                   
         BNZ   XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* POST CODES FROM RECORD *                                                      
         SPACE                                                                  
POSTREC  NTR1                                                                   
***      GOTO1 =V(PRNTBL),DMCB,=C'REB',REBOOK,C'DUMP',3,=C'1D'                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R3,R3                                                            
         LA    R2,ELEMTAB                                                       
POSTR10  CLC   KEY(1),0(R2)        THIS THE SAME REC TYPE                       
         BE    POSTR14                                                          
         LA    R2,L'ELEMTAB(,R2)                                                
         CLI   0(R2),X'FF'                                                      
         BNE   POSTR10                                                          
         DC    H'0'                                                             
POSTR14  LA    R6,IO                                                            
         MVC   ELCODE,1(R2)                                                     
         BAS   RE,GETEL                                                         
         BE    POSTR24                                                          
         B     POSTR30                                                          
         DC    H'0'                                                             
         SPACE                                                                  
* FIND NEXT ELEM                                                                
         SPACE                                                                  
*POSTR20  MVC   ELCODE,1(R2)                                                    
*         BAS   RE,NEXTEL                                                       
*         BNE   POSTR30                                                         
         SPACE                                                                  
POSTR24  DS    0H                                                               
         IC    R1,2(R2)             LEN                                         
         LR    RE,R6               START OF ELEM                                
         SR    R0,R0                                                            
         ICM   R0,1,3(R2)                                                       
         AR    RE,R0                                                            
         LA    RF,RECODES          START OF CODES                               
         IC    R0,4(R2)                                                         
         AR    RF,R0                                                            
         SPACE                                                                  
         EX    R1,POSTRMVC                                                      
*                                                                               
         BCTR  R3,0                                                             
POSTR30  LA    R2,L'ELEMTAB(,R2)                                                
         CLI   0(R2),X'FF'                                                      
         BE    POSTR40                                                          
         CLC   KEY(1),0(R2)        THIS THE SAME REC TYPE                       
         BNE   POSTR30             NO                                           
         B     POSTR14             YES/GO GET THE ELEMENT                       
         SPACE                                                                  
POSTR40  LTR   R3,R3               GET A HIT?                                   
         BNZ   XIT                                                              
         DC    H'0'                                                             
         SPACE                                                                  
POSTRMVC MVC   0(0,RF),0(RE)                                                    
         EJECT                                                                  
* - SPECIAL POSTING ROUTINES                                                    
POSTPOST NTR1                                                                   
*- IF BOP ELEMENT EXISTS, OVERRIDES PREVIOUS SAR ELEM DEMO                      
         LA    R6,IO                                                            
         USING RCONREC,R6                                                       
         CLC   RCONKTYP,=X'0C00'                                                
         BNE   PPOS5                                                            
         MVI   ELCODE,X'10'        BOP ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   PPOSX                                                            
         USING RCONBPEL,R6                                                      
         MVC   REDEMO,RCONBPDM+1   GET FIRST DEMO                               
***      GOTO1 =V(PRNTBL),DMCB,=C'DEM',REDEMO,C'DUMP',3,=C'1D'                  
         B     PPOS5                                                            
*                                                                               
PPOS5    DS    0H                                                               
*                                                                               
PPOSX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              TEST FILTERS                                                     
         SPACE 3                                                                
FILTKEY  NTR1                                                                   
         TM    KEY+27,X'80'        CHECK FOR DELETED KEY                        
         BZ    FILTALL                                                          
         B     NOTOK                                                            
         SPACE                                                                  
FILTREC  NTR1                                                                   
         TM    IO+29,X'80'         CHECK FOR DELETED RECORD                     
         BZ    FR10                                                             
         TM    REQFLAGS,REQFDEL    OK IF USER WANTS DELETES                     
         BO    FR10                                                             
         B     NOTOK                                                            
         SPACE                                                                  
FR10     BAS   RE,FILTDATE         TEST DATE FILTERS                            
         BNE   NOTOK                                                            
         SPACE                                                                  
FILTALL  DS    0H                  FIRST TEST LIMIT ACCESS RESTRICTIONS         
         LA    R2,REFOFF           OFFICE                                       
         LA    R3,REOFF                                                         
         BAS   RE,TESTLIM                                                       
         SPACE                                                                  
         OC    REFILTS,REFILTS     ANY FILTERS AT ALL?                          
         BZ    OK                      NO - SO WE'RE OK                         
         SPACE                                                                  
         LA    R2,REFREG           REGION                                       
         LA    R3,REREG                                                         
         LA    R4,L'REREG                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFOFF           OFFICE                                       
         LA    R3,REOFF                                                         
         LA    R4,L'REOFF                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFGRP           GROUP                                        
         LA    R3,REGRP                                                         
         LA    R4,L'REGRP                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFSUB           SUBGROUP                                     
         LA    R3,RESUB                                                         
         LA    R4,L'RESUB                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFSTA           STATION                                      
         LA    R3,RESTA                                                         
         LA    R4,L'RESTA                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFSAL           SALESPERSON                                  
         LA    R3,RESAL                                                         
         LA    R4,L'RESAL                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFDIV           DIVISION                                     
         LA    R3,REDIV                                                         
         LA    R4,L'REDIV                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFTEAM          TEAM                                         
         LA    R3,RETEAM                                                        
         LA    R4,L'RETEAM                                                      
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFADV           ADVERTISER                                   
         LA    R3,READV                                                         
         LA    R4,L'READV                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFAGY           AGENCY                                       
         LA    R3,REAGY                                                         
         LA    R4,L'REAGY                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFCLS           CLASS                                        
         LA    R3,RECLS                                                         
         LA    R4,L'RECLS                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFCTG           CATEGORY CODE                                
         LA    R3,RECTG                                                         
         LA    R4,L'RECTG                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFPRD           PRODUCT                                      
         LA    R3,REPRD                                                         
         LA    R4,L'REPRD                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFSTAT          STATION TYPE                                 
         LA    R3,RESTAT                                                        
         LA    R4,L'RESTAT                                                      
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFRNK           STATION RANK                                 
         LA    R3,RERNK                                                         
         LA    R4,L'RERNK                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFTVB           STATION TVB/REGION                           
         LA    R3,RETVB                                                         
         LA    R4,L'RETVB                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFOWN           STATION OWNER                                
         LA    R3,REOWN                                                         
         LA    R4,L'REOWN                                                       
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         CLI   REFCTY,0            CONTRACT TYPE                                
         BE    FILTDEM                                                          
         CLI   RECTY,0             CONTRACT TYPE AVAIL YET                      
         BE    FILTDEM              NO                                          
         CLI   REFCTYEX,C'*'       CONTRACT TYPE EXCLUDE                        
         BE    *+18                                                             
         CLC   REFCTY,RECTY                                                     
         BNE   NOTOK                                                            
         B     FILTDEM                                                          
         SPACE                                                                  
         CLC   REFCTY,RECTY                                                     
         BE    NOTOK                                                            
         SPACE                                                                  
FILTDEM  LA    R2,REFDEMO          DEMO                                         
         LA    R3,REDEMO                                                        
         LA    R4,L'REDEMO                                                      
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFFILT1         FILTER 1                                     
         LA    R3,REFILT1                                                       
         LA    R4,L'REFILT1                                                     
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFFILT2         FILTER 2                                     
         LA    R3,REFILT2                                                       
         LA    R4,L'REFILT2                                                     
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFFILT3         FILTER 3                                     
         LA    R3,REFILT3                                                       
         LA    R4,L'REFILT3                                                     
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         LA    R2,REFFILT4         FILTER 4                                     
         LA    R3,REFILT4                                                       
         LA    R4,L'REFILT4                                                     
         BAS   RE,TESTEM                                                        
         SPACE                                                                  
         B     OK                                                               
         EJECT                                                                  
*              LITTLE ROUTINE TO TEST FILTERS                                   
         SPACE 2                                                                
*                                  R2=A(REQUESTED FILTER)                       
*                                  R3=A(DEDUCED VALUE)                          
*                                  R4=LENGTH                                    
         SPACE                                                                  
TESTEM   LR    R1,R2               SAVE A(FIRST CHARACTER)                      
         CLI   0(R2),X'41'                                                      
         BLR   RE                                                               
         CLI   0(R3),X'41'                                                      
         BLR   RE                                                               
         SPACE                                                                  
TSTF2    CLI   0(R2),X'41'                                                      
         BL    TSTFNXT                                                          
         CLI   0(R2),C'*'          WILD CARD                                    
         BE    TSTFNXT                                                          
         TM    0(R1),X'40'         TEST FIRST BYTE HERE                         
         BNO   TSTFNEG                                                          
         CLC   0(1,R2),0(R3)                                                    
         BNE   NOTOK                                                            
         B     TSTFNXT                                                          
         SPACE                                                                  
TSTFNEG  MVC   BYTE,0(R2)          NEGATIVE TEST                                
         OI    BYTE,X'40'                                                       
         CLC   BYTE,0(R3)                                                       
         BNER  RE                  IF ANY BYTE DIFFERS, ITS OK                  
         SPACE                                                                  
TSTFNXT  LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,TSTF2                                                         
         TM    0(R1),X'40'         IF WE GOT THIS FAR FOR NEGATIVE              
         BNO   NOTOK               THEN IT'S NO GOOD                            
         BR    RE                                                               
         EJECT                                                                  
TESTLIM  NTR1                                                                   
         SPACE                                                                  
* TEST OFFICE LIMITS HERE LATER *                                               
         SPACE                                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              DATE FILTERING ROUTINE                                           
         SPACE                                                                  
FILTDATE NTR1                                                                   
         SPACE                                                                  
         OC    REQPSTRB(6),REQPSTRB   ANY REQUEST PERIOD DATES                  
         BZ    OK                      NO, ACCEPT ALL                           
         SPACE                                                                  
         CLC   REQPSTRB,RECONEND                                                
         BH    NOTOK                                                            
         CLC   REQPENDB,RECONSTR                                                
         BL    NOTOK                                                            
         B     OK                                                               
         EJECT                                                                  
*              DATA MANAGER INTERFACE                                           
         SPACE                                                                  
READ     MVC   COMMAND,=CL8'DMREAD'                                             
         MVC   KEYSAVE,KEY                                                      
         B     RDIR                                                             
         SPACE                                                                  
SEQ      MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     RDIR                                                             
         SPACE                                                                  
FIRSTHI  OC    REQSKEY,REQSKEY     POSSIBLE OVERRIDING KEY                      
         BZ    HIGH                                                             
         MVC   KEY(27),REQSKEY                                                  
         SPACE                                                                  
HIGH     MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
RDIR     NTR1                                                                   
         LA    R2,KEY+28                                                        
         TM    STATUS,ROIREC                                                    
         BZ    READ10                                                           
         MVC   DIRNAME,=CL8'ROIDIR'                                             
         LA    R2,KEY+32                                                        
         SPACE                                                                  
READ10   GOTO1 DATAMGR,DMCB,(DMINBITS,COMMAND),DIRNAME,KEY,KEY                  
         MVC   DIRNAME,=CL8'REPDIR'                                             
         TM    STATUS,ROIREC                                                    
         BO    READ20                                                           
         MVC   REKEY,KEY           PASS USER KEY                                
READ20   MVC   REDSKADD,0(R2)      AND DISK ADDRESS                             
         TM    DMCB+8,X'10'                                                     
         BO    NOTOK               RECORD NOT FOUND                             
         TM    DMCB+8,X'02'                                                     
         BO    NOTOK               RECORD IS DELETED                            
         B     DMCHECK                                                          
         SPACE                                                                  
GETREC   MVC   COMMAND,=CL8'GETREC'                                             
         B     RDFILE                                                           
         SPACE                                                                  
RDFILE   NTR1                                                                   
         LA    R2,KEY+28                                                        
         LA    R3,IO                                                            
         TM    STATUS,ROIREC                                                    
         BZ    READF10                                                          
         MVC   FILENAME,=CL8'ROIFIL'                                            
         LA    R2,KEY+32                                                        
         LA    R3,IO2                                                           
         SPACE                                                                  
READF10  GOTO1 DATAMGR,DMCB,(DMINBITS,COMMAND),                        X        
               FILENAME,(R2),(R3),DMWORK                                        
         MVC   FILENAME,=CL8'REPFIL'                                            
         ST    R3,REAREC                                                        
         SPACE                                                                  
DMCHECK  CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              ASSORTED ROUTINES                                                
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
KEYCOMP  ZIC   R1,LKEY             VARIABLE LENGTH FOR COMPARISON               
         EX    R1,*+6              SET CC                                       
         BR    RE                  AND EXIT                                     
         CLC   KEY(0),KEYSAVE                                                   
         SPACE                                                                  
OK       SR    R1,R1                                                            
         B     *+8                                                              
         SPACE                                                                  
NOTOK    LA    R1,1                                                             
         SPACE                                                                  
         LTR   R1,R1                                                            
         SPACE                                                                  
XIT      XIT1                                                                   
         SPACE                                                                  
GOHOOK   NTR1                                                                   
         OI    STATUS,FOUNDONE     SET WE PASSED A RECORD TO APPLIC.            
         MVI   REMODE,PROCREC                                                   
         L     RF,REHOOK                                                        
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE                                                                  
DATADISP DC    H'34'                                                            
DMINBITS DC    X'08'                                                            
         SPACE                                                                  
* ROUTINE ADDRESSES FOR DIFFERENT RECORDS *                                     
         SPACE                                                                  
READADDS DS    0A                                                               
         DC    AL1(RREPKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RSTAKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RREGKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(ROFFKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RTEMKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RSALKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RGRPKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RADVKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RPRDKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RAGYKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RCONKTYQ),AL3(CON-REWRIIO)                                   
         DC    AL1(RCTGKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RCLSKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RCTGKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(ROWNKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RMKTKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RPOPKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RCTYKTYQ),AL3(OTH-REWRIIO)                                   
         DC    AL1(RSTAOWNT),AL3(STA-REWRIIO)                                   
         DC    AL1(ROFFREGT),AL3(OFF-REWRIIO)                                   
         DC    AL1(RCTGCLST),AL3(CTG-REWRIIO)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
* USED TO BUILD ALL KEYS OTHER THAN CONTRACTS AND PWC RECS *                    
         SPACE                                                                  
*                 KEY TYPE  KEY DISP         RECODE DISP   DATA LEN             
* STATION                                                                       
KEYBLD   DS    0CL4                                                             
         SPACE                                                                  
* REP                                                                           
         SPACE                                                                  
         DC    AL1(RREPKTYQ,RREPKREP-RREPKEY,REREP-RECODES,L'REREP-1)           
         SPACE                                                                  
* STATION                                                                       
         SPACE                                                                  
         DC    AL1(RSTAKTYQ,RSTAKREP-RSTAKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RSTAKTYQ,RSTAKSTA-RSTAKEY,RESTA-RECODES,L'RESTA-1)           
         SPACE                                                                  
* REGION                                                                        
         SPACE                                                                  
         DC    AL1(RREGKTYQ,RREGKREP-RREGKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RREGKTYQ,RREGKREG-RREGKEY,REREG-RECODES,L'REREG-1)           
         SPACE                                                                  
* OFFICE                                                                        
         SPACE                                                                  
         DC    AL1(ROFFKTYQ,ROFFKREP-ROFFKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(ROFFKTYQ,ROFFKOFF-ROFFKEY,REOFF-RECODES,L'REOFF-1)           
         SPACE                                                                  
* TEAM                                                                          
         SPACE                                                                  
         DC    AL1(RTEMKTYQ,RTEMKREP-RTEMKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RTEMKTYQ,RTEMKTEM-RTEMKEY,REDVT-RECODES,L'REDVT-1)           
         SPACE                                                                  
* SALESMAN                                                                      
         SPACE                                                                  
         DC    AL1(RSALKTYQ,RSALKREP-RSALKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RSALKTYQ,RSALKSAL-RSALKEY,RESAL-RECODES,L'RESAL-1)           
         SPACE                                                                  
* GROUP/SUB-GROUP                                                               
         SPACE                                                                  
         DC    AL1(RGRPKTYQ,RGRPKREP-RGRPKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RGRPKTYQ,RGRPKGRP-RGRPKEY,REGRS-RECODES,L'REGRS-1)           
         SPACE                                                                  
* ADVERTISER                                                                    
         SPACE                                                                  
         DC    AL1(RADVKTYQ,RADVKREP-RADVKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RADVKTYQ,RADVKADV-RADVKEY,READV-RECODES,L'READV-1)           
         SPACE                                                                  
* PRODUCT                                                                       
         SPACE                                                                  
         DC    AL1(RPRDKTYQ,RPRDKREP-RPRDKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RPRDKTYQ,RPRDKADV-RPRDKEY,READV-RECODES,L'READV-1)           
         DC    AL1(RPRDKTYQ,RPRDKPRD-RPRDKEY,REPRD-RECODES,L'REPRD-1)           
         SPACE                                                                  
* AGENCY                                                                        
         SPACE                                                                  
         DC    AL1(RAGYKTYQ,RAGYKREP-RAGYKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RAGYKTYQ,RAGYKAGY-RAGYKEY,REAGY-RECODES,L'REAGY-1)           
         DC    AL1(RAGYKTYQ,RAGYKAOF-RAGYKEY,REAOF-RECODES,L'REAOF-1)           
         SPACE                                                                  
* CLASS                                                                         
         SPACE                                                                  
         DC    AL1(RCLSKTYQ,RCLSKREP-RCLSKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RCLSKTYQ,RCLSKCLS-RCLSKEY,RECLS-RECODES,L'RECLS-1)           
         SPACE                                                                  
* CATEGORY                                                                      
         SPACE                                                                  
         DC    AL1(RCTGKTYQ,RCTGKREP-RCTGKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RCTGKTYQ,RCTGKCTG-RCTGKEY,RECTG-RECODES,L'RECTG-1)           
         SPACE                                                                  
* STATION OWNER                                                                 
         SPACE                                                                  
         DC    AL1(ROWNKTYQ,ROWNKREP-ROWNKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(ROWNKTYQ,ROWNKOWN-ROWNKEY,REOWN-RECODES,L'REOWN-1)           
         SPACE                                                                  
* POINT PERSON                                                                  
         SPACE                                                                  
         DC    AL1(RPOPKTYQ,RPTPKREP-RPTPKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RPOPKTYQ,RPTPKREC-RPTPKEY,REPTP-RECODES,L'REPTP-1)           
         SPACE                                                                  
* CONTRACT TYPE                                                                 
         SPACE                                                                  
         DC    AL1(RCTYKTYQ,RCTYKREP-RCTYKEY,REREP-RECODES,L'REREP-1)           
         DC    AL1(RCTYKTYQ,RCTYKCTY-RCTYKEY,RECTY-RECODES,L'RECTY-1)           
         SPACE                                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
* TABLE USED TO POST KEY DATA INTO RECODES (THEN USED TO FILTER) *              
         SPACE                                                                  
KEYTAB   DS    0XL4                                                             
*              KEY         DATA      DISPL          DISP                        
*              CODE        LEN       INTO KEY       INTO CODE TBL               
         DC   X'0C',AL1(L'RCONKGRP-1,RCONKGRP-RCONKEY,REGRS-RECODES)            
         DC   X'0C',AL1(L'RCONKSTA-1,RCONKSTA-RCONKEY,RESTA-RECODES)            
         DC   X'0C',AL1(L'RCONKOFF-1,RCONKOFF-RCONKEY,REOFF-RECODES)            
         DC   X'0C',AL1(L'RCONKAGY-1,RCONKAGY-RCONKEY,REAGY-RECODES)            
         DC   X'0C',AL1(L'RCONKAOF-1,RCONKAOF-RCONKEY,REAOF-RECODES)            
         DC   X'0C',AL1(L'RCONKADV-1,RCONKADV-RCONKEY,READV-RECODES)            
         DC   X'0C',AL1(L'RCONKCON-1,RCONKCON-RCONKEY,RECON-RECODES)            
         SPACE                                                                  
         DC   X'8C',AL1(L'RCONPCON-1,RCONPCON-RCONKEY,RECON-RECODES)            
         SPACE                                                                  
         DC   X'9C',AL1(L'RCONQOFF-1,RCONQOFF-RCONKEY,REOFF-RECODES)            
         DC   X'9C',AL1(L'RCONQGRP-1,RCONQGRP-RCONKEY,REGRS-RECODES)            
         DC   X'9C',AL1(L'RCONQSTA-1,RCONQSTA-RCONKEY,RESTA-RECODES)            
         DC   X'9C',AL1(L'RCONQADV-1,RCONQADV-RCONKEY,READV-RECODES)            
         DC   X'9C',AL1(L'RCONQAGY-1,RCONQAGY-RCONKEY,REAGY-RECODES)            
         DC   X'9C',AL1(L'RCONQAOF-1,RCONQAOF-RCONKEY,REAOF-RECODES)            
         DC   X'9C',AL1(L'RCONQCON-1,RCONQCON-RCONKEY,RECON-RECODES)            
         SPACE                                                                  
         DC   X'AC',AL1(L'RCONROFF-1,RCONROFF-RCONKEY,REOFF-RECODES)            
         DC   X'AC',AL1(L'RCONRTEM-1,RCONRTEM-RCONKEY,REDVT-RECODES)            
         DC   X'AC',AL1(L'RCONRSAL-1,RCONRSAL-RCONKEY,RESAL-RECODES)            
         DC   X'AC',AL1(L'RCONRSTA-1,RCONRSTA-RCONKEY,RESTA-RECODES)            
         DC   X'AC',AL1(L'RCONRAGY-1,RCONRAGY-RCONKEY,REAGY-RECODES)            
         DC   X'AC',AL1(L'RCONRADV-1,RCONRADV-RCONKEY,READV-RECODES)            
         DC   X'AC',AL1(L'RCONRCON-1,RCONRCON-RCONKEY,RECON-RECODES)            
         SPACE                                                                  
* ADD OTHER PASSIVE KEYS HERE *                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
* THIS TABLE IS USED TO POST DATA FROM RECORDS TO RECODES *                     
         SPACE                                                                  
ELEMTAB  DS    0CL5                                                             
*        RECORD CODE                                                            
*              ELEM        DATA       DISPL               DISP                  
*              CODE        LEN       INTO ELEM        INTO CODE TBL             
         SPACE                                                                  
* REP     *                                                                     
         SPACE                                                                  
         DC    AL1(RREPKTYQ)                                                    
         DC    X'01',AL1(L'RREPSHRT-1,RREPSHRT-RREPELEM,RENAME-RECODES)         
         SPACE                                                                  
* STATION *                                                                     
         SPACE                                                                  
         DC    AL1(RSTAKTYQ)                                                    
         DC    X'01',AL1(L'RSTAMKT-1,RSTAMKT-RSTAELEM,RENAME-RECODES)           
         DC    AL1(RSTAKTYQ)                                                    
         DC    X'01',AL1(L'RSTAOWN-1,RSTAOWN-RSTAELEM,REOWN-RECODES)            
         DC    AL1(RSTAKTYQ)                                                    
         DC    X'01',AL1(L'RSTARANK-1,RSTARANK-RSTAELEM,RERNK-RECODES)          
         DC    AL1(RSTAKTYQ)                                                    
         DC    X'01',AL1(L'RSTATVB-1,RSTATVB-RSTAELEM,RETVB-RECODES)            
         DC    AL1(RSTAKTYQ)                                                    
         DC    X'08',AL1(L'RSTAMKTC-1,RSTAMKTC-RSTAXXEL,REMCD-RECODES)          
         SPACE                                                                  
* REGION *                                                                      
         SPACE                                                                  
         DC    AL1(RREGKTYQ)                                                    
         DC    X'01',AL1(L'RREGNAME-1,RREGNAME-RREGELEM,RENAME-RECODES)         
         SPACE                                                                  
* OFFICE *                                                                      
         SPACE                                                                  
         DC    AL1(ROFFKTYQ)                                                    
         DC    X'01',AL1(L'ROFFNAME-1,ROFFNAME-ROFFELEM,RENAME-RECODES)         
         DC    AL1(ROFFKTYQ)                                                    
         DC    X'01',AL1(L'ROFFREG-1,ROFFREG-ROFFELEM,REREG-RECODES)            
         SPACE                                                                  
* DIV/TEAM *                                                                    
         SPACE                                                                  
         DC    AL1(RTEMKTYQ)                                                    
         DC    X'01',AL1(L'RTEMDVNM-1,RTEMDVNM-RTEMELEM,RENAME-RECODES)         
         DC    AL1(RTEMKTYQ)                                                    
         DC    X'01',AL1(L'RTEMNAME-1,RTEMNAME-RTEMELEM)                        
         DC    AL1((RENAME+L'RTEMDVNM+1)-RECODES)                               
         SPACE                                                                  
* SALESPERSON *                                                                 
         SPACE                                                                  
         DC    AL1(RSALKTYQ)                                                    
         DC    X'01',AL1(L'RSALNAME-1,RSALNAME-RSALELEM,RENAME-RECODES)         
         SPACE                                                                  
* GROUP/SUB *                                                                   
         SPACE                                                                  
         DC    AL1(RGRPKTYQ)                                                    
         DC    X'01',AL1(L'RGRPNAME-1,RGRPNAME-RGRPELEM,RENAME-RECODES)         
         DC    AL1(RGRPKTYQ)                                                    
         DC    X'01',AL1(L'RGRPSBNM-1,RGRPSBNM-RGRPELEM)                        
         DC    AL1((RENAME+(L'RGRPNAME+1))-RECODES)                             
         SPACE                                                                  
* ADVERTISER *                                                                  
         SPACE                                                                  
         DC    AL1(RADVKTYQ)                                                    
         DC    X'01',AL1(L'RADVNAME-1,RADVNAME-RADVELEM,RENAME-RECODES)         
         SPACE                                                                  
* PRODUCT *                                                                     
         SPACE                                                                  
         DC    AL1(RPRDKTYQ)                                                    
         DC    X'01',AL1(L'RPRDNAME-1,RPRDNAME-RPRDELEM,RENAME-RECODES)         
         SPACE                                                                  
* AGENCY *                                                                      
         SPACE                                                                  
         DC    AL1(RAGYKTYQ)                                                    
         DC    X'01',AL1(L'RAGYNAM1-1,RAGYNAM1-RAGYELEM,RENAME-RECODES)         
         SPACE                                                                  
* CONTRACT RECORDS *                                                            
         SPACE                                                                  
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONPRD-1,RCONPRD-RCONELEM,REPRD-RECODES)            
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONTEM-1,RCONTEM-RCONELEM,REDVT-RECODES)            
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONSAL-1,RCONSAL-RCONELEM,RESAL-RECODES)            
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONDATE-1,RCONDATE-RCONELEM,REDTS-RECODES)          
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONCREA-1,RCONCREA-RCONELEM,RECADT-RECODES)         
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONCTGY-1,RCONCTGY-RCONELEM,RECTG-RECODES)          
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RCONTYPE-1,RCONTYPE-RCONELEM,RECTY-RECODES)          
         DC    AL1(RCONKTYQ)                                                    
         DC    X'01',AL1(L'RESRVC-1,RCONRTGS-RCONELEM,RESRVC-RECODES)           
         DC    AL1(RCONKTYQ)                                                    
         DC    X'05',AL1(L'RCONEXPR-1,RCONEXPR-RCONEXEL,REPRDN-RECODES)         
         DC    AL1(RCONKTYQ)                                                    
         DC    X'06',AL1(L'RCONSPST-1,RCONSPST-RCONSPEL,RESTA-RECODES)          
         DC    AL1(RCONKTYQ)                                                    
         DC    X'10',AL1(L'RCONBPMK-1,RCONBPMK-RCONBPEL,REMKT-RECODES)          
         DC    AL1(RCONKTYQ)                                                    
         DC    X'10',AL1(5,RCONBBKS-RCONBPEL,REBOOK6-RECODES)                   
         DC    AL1(RCONKTYQ)                                                    
         DC    X'12',AL1(2,RSARBKS-RSAREL,REBOOK3-RECODES)                      
         DC    AL1(RCONKTYQ)                                                    
         DC    X'12',AL1(L'REDEMO-1,RSARDEM-RSAREL,REDEMO-RECODES)              
         DC    AL1(RCONKTYQ)                                                    
         DC    X'12',AL1(0,RSARDPT-RSAREL,REDPT-RECODES)                        
         SPACE                                                                  
* CLASS RECORD                                                                  
         SPACE                                                                  
         DC    AL1(RCLSKTYQ)                                                    
         DC    X'01',AL1(L'RCLSNAME-1,RCLSNAME-RCLSELEM,RENAME-RECODES)         
         SPACE                                                                  
* CATEGORY RECORD                                                               
         SPACE                                                                  
         DC    AL1(RCTGKTYQ)                                                    
         DC    X'01',AL1(L'RCTGNAME-1,RCTGNAME-RCTGELEM,RENAME-RECODES)         
         DC    AL1(RCTGKTYQ)                                                    
         DC    X'01',AL1(L'RCTGCLSS-1,RCTGCLSS-RCTGELEM,RECLS-RECODES)          
         SPACE                                                                  
* CONTRACT TYPE                                                                 
         SPACE                                                                  
         DC    AL1(RCTYKTYQ)                                                    
         DC    X'01',AL1(L'RCTYDESC-1,RCTYDESC-RCTYELEM,RENAME-RECODES)         
         SPACE                                                                  
* STATION OWNER                                                                 
         SPACE                                                                  
         DC    AL1(ROWNKTYQ)                                                    
         DC    X'01',AL1(L'ROWNNAME-1,ROWNNAME-ROWNELEM,RENAME-RECODES)         
         SPACE                                                                  
* POINT PERSON                                                                  
         SPACE                                                                  
         DC    AL1(RPOPKTYQ)                                                    
         DC    X'01',AL1(L'RPTPNAME-1,RPTPNAME-RPTPELEM,RENAME-RECODES)         
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
ROIFLSTR DC    CL8' ROIDIR'        READ-ONLY LIST                               
         DC    CL8' ROIFILE'                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
*              LTORG FOR REWRIIO                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR THIS MODULE                                            
         SPACE 3                                                                
MYD      DSECT                                                                  
FILENAME DS    CL8                                                              
DIRNAME  DS    CL8                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMWORK   DS    CL96                                                             
FULL     DS    F                                                                
WORD     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
THREE    DS    CL3                                                              
BYTE     DS    CL1                                                              
FILTLEV  DS    CL1                                                              
         SPACE                                                                  
COMMAND  DS    CL8                                                              
USERRD   DS    A                   USER REGISTER 13                             
DATAMGR  DS    V                                                                
HEXOUT   DS    V                                                                
DATCON   DS    V                                                                
SQUASHER DS    V                                                                
         SPACE                                                                  
NLIMOFF  DS    XL1                 OFFICE LIMIT ACCESS                          
LIMOFF   DS    CL120                                                            
SPACES   DS    CL133                                                            
MAINREAD DS    XL1                                                              
SUBREAD  DS    XL1                                                              
ELCODE   DS    CL1                                                              
LKEY     DS    XL1                                                              
PRIMELKY DS    XL1                                                              
STATUS   DS    XL1                                                              
FOUNDONE EQU   X'80'               PASSED A RECORD TO APPLIC.                   
ROIREC   EQU   X'40'               USE ROIDIR/FIL - NOT REPDIR/FIL              
         DS    0D                                                               
INTCOMM  DS    F                                                                
TESTBYTE DS    XL1                                                              
         DS    0D                                                               
PRIMEKEY DS    CL40                                                             
PRIMESAV DS    CL40                                                             
KEY      DS    CL40                                                             
KEYSAVE  DS    CL40                                                             
SVKEY    DS    CL40                                                             
         SPACE                                                                  
IO       DS    CL1000              USED FOR PRIMARY REC (USUALLY CONTR)         
IO2      DS    CL1000              USED FOR PAPER WORK COUNT                    
MYEND    EQU   *                                                                
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE REWRIIOD                                                       
         SPACE                                                                  
*REGENALL1                                                                      
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REGENPWC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REWRISTBD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018REWRIIO   05/01/02'                                      
         END                                                                    
