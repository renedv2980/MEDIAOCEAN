*          DATA SET PPEZF18    AT LEVEL 102 AS OF 05/27/97                      
*PHASE T43018A,*                                                                
*        TITLE 'T43018 - INVOICE LIST'                                          
         TITLE 'T43018 - INVOICE LIST'                                          
***********************************************************************         
*                                                                     *         
*  TITLE: T43018 - PRINT EPIC INVOICE LIST                            *         
*  COMMENTS: THIS PROGRAM LISTS INVOICES                              *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, PZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (PPPZF00-T43000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2     - WORKER RECORD                                *         
*             AIO3     - PZBLOCK                                      *         
*             WRKFBUFR - WORKER BUFFER                                *         
*                      - INDEX RECORD                                 *         
*                                                                     *         
***********************************************************************         
         TITLE 'T43018 - INVOICE LIST - INITIALIZATION'                         
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
T43018   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3018**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(WRKFBUFR-SYSD)   SET UP WORKER BUFFER ADDRESS              
         LA    R1,SYSD(R1)                                                      
         ST    R1,WRKFBUFA                                                      
*                                                                               
*        CHECK TO SEE IF IN MIDDLE OF HELP CALL                                 
*                                                                               
         LA    R2,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R2                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RA,RF)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D1'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN FIRST TWA PAGE                       
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVI   HCBSEGS,10          DDVAL TAB IS 10*256 SEGMENTS LONG            
         MVC   HCBATAB,WRKFBUFA    AREA FOR TABLE                               
         GOTO1 VPRHELP,DMCB,0,0,HELPCBLK GO CHECK IF IN MIDDLE OF MENU          
*                                                                               
         TM    HCBRTRN,HCBRSELQ    IF SELECTION WAS MADE                        
         BNO   INIT10                                                           
*                                                                               
         GOTO1 ERREX2                 EXIT FOR USER TO COMPLETE ENTRY           
*                                                                               
         DROP  R2                                                               
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
*                                                                               
         TITLE 'T43018 - INVOICE LIST - MODE'                                   
***********************************************************************         
*                                                                     *         
*        DETERMINE PROCESSING MODE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    MODINVAL                                                         
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    MODINVAL                                                         
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MODINVAL                                                         
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    MODINVAL                                                         
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    MODINVAL                                                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
MODINVAL MVI   ERROR,INVACT        INVALID ACTION                               
         LA    R2,CONACTH                                                       
         B     MODERRX                                                          
*                                                                               
MODERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T43018 - INVOICE LIST - VKEY'                                   
***********************************************************************         
*                                                                     *         
*        VKEY - VALIDATE KEY                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKEY     CLI   ACTNUM,ACTLIST      SKIP UNLESS IN LIST/SELECT MODE              
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   MODINVAL                                                         
*                                                                               
*        IF NO FIELDS ARE CHANGED, JUST KEEP ON WITH DISPLAY *                  
*                                                                               
         TM    LINMEDH+4,X'20'     BATCH MEDIA                                  
         BZ    VK000                                                            
         TM    LINPUBH+4,X'20'     BATCH PUB                                    
         BZ    VK000                                                            
         TM    LINBDTH+4,X'20'     BATCH DATE                                   
         BZ    VK000                                                            
         TM    LINBSQH+4,X'20'     BATCH SEQ                                    
         BZ    VK000                                                            
         TM    LINFTRH+4,X'20'     FILTERS                                      
         BZ    VK000                                                            
         B     VKXIT               NO CHANGES TO SCREEN - EXIT                  
*                                                                               
*        VALIDATE KEY FIELDS                                                    
*                                                                               
VK000    MVI   NEWDISP,C'N'        ASSUME NO RE-DISPLAY                         
*                                                                               
         MVI   CURSYST,C'M'        INDICATE WE ARE IN A MEDIA SYS-PRINT         
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         LA    R2,LINMEDH          POINT TO MEDIA FIELD HEADER                  
*                                                                               
         GOTO1 VALIMED             VALIDATE MEDIA                               
*                                                                               
         MVC   RQMED,QMED          SAVE REQUESTED MEDIA                         
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
         OI    LINMEDH+4,X'20'     INDICATE VALID FIELD                         
         OI    LINMEDH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
VKPUB    DS    0H                                                               
*                                                                               
         LA    R2,LINPUBH          POINT TO PUB FIELD                           
*                                                                               
         XC    RQPUB,RQPUB         KILL FILTER PUB                              
         XC    RQPZPUB,RQPZPUB     KILL FILTER PUB                              
         XC    LSCRPUB,LSCRPUB     KILL LAST LIST SCREEN KEY PUB                
*                                                                               
         CLI   5(R2),0             SKIP IF PUB NOT ENTERED                      
         BE    VKPUBX                                                           
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIPUB             VALIDATE PUB ENTRY                           
*                                                                               
         CLC   LSCRPUB,QPUB        IF PUB HAS CHANGED                           
         BE    VKPUBX                                                           
*                                                                               
         MVI   NEWDISP,C'Y'           FORCE RE-DISPLAY OF SCREEN                
*                                                                               
         MVC   RQPUB,QPUB          SAVE PUB ID                                  
         MVC   RQPZPUB,PTRPZPUB-PTRTABD+PUBPTRTB   SAVE BATCH ID                
         MVC   LSCRPUB,QPUB        SAVE PUB ID                                  
         XC    LINPUBN,LINPUBN     PUT OUT NEW PUBNAME                          
         MVC   LINPUBN(L'PUBPNM),PUBPNM                                         
         OI    LINPUBNH+6,X'80'    FORCE FIELD TRANSMISSION                     
         XC    LINZONE,LINZONE     PUT OUT NEW ZONE NAME                        
         MVC   LINZONE(L'PUBPZNM),PUBPZNM                                       
         OI    LINZONEH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
VKPUBX   OI    4(R2),X'20'         INDICATE PUB IS VALID                        
*                                                                               
         LA    R2,LINBDTH          POINT TO BATCH DATE                          
*                                                                               
         MVI   DTPLUS,C'N'         ASSUME EXACT DATE ENTERED                    
*                                                                               
         CLI   5(R2),0             IF NO DATE                                   
         BNE   VK120                                                            
*                                                                               
         MVI   NEWDISP,C'Y'        THEN NEW BATCH - FORCE RE-DISPLAY            
         XC    RQDTE,RQDTE                                                      
*                                                                               
         B     VK200                                                            
*                                                                               
VK120    ZIC   RF,5(R2)               LOOK FOR FINAL +                          
*                                                                               
         LA    RE,FHDRLEN-1(R2,RF)    POINT TO LAST CHAR                        
*                                                                               
         CLI   0(RE),C'+'          IF DATE ENDS IN '+'                          
         BNE   VK130                                                            
*                                                                               
         MVI   0(RE),C' '             ELIMINATE '+' FROM DATE                   
         BCTR  RF,0                   DECREMENT INPUT LENGTH                    
         STC   RF,5(R2)                                                         
         MVI   DTPLUS,C'Y'            LOOK FOR THIS DATE OR LATER               
*                                                                               
VK130    GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK VALIDATE LENGTH                 
*                                                                               
         OC    DMCB,DMCB           MUST HAVE A VALID DATE                       
         BZ    VKBADATE                                                         
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,DUB)  CONVERT TO COMPRESSED DATE         
*                                                                               
         CLC   RQDTE,DUB           IF DATE WAS CHANGED                          
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'           FORCE SCREEN RE-DISPLAY                   
*                                                                               
         MVC   RQDTE,DUB                                                        
*                                                                               
VK200    OI    4(R2),X'20'         INDICATE DATE IS VALID                       
*                                                                               
         LA    R2,LINBSQH          SEQUENCE NUMBER FIELD                        
*                                                                               
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK210                                                            
*                                                                               
         MVI   NEWDISP,C'Y'           NEW BATCH-FORCE SCREEN RE-DISPLAY         
         XC    RQSEQ,RQSEQ            INIT SEQUENCE NUMBER                      
         XC    RQBSEQ,RQBSEQ                                                    
*                                                                               
         B     VK300                                                            
*                                                                               
VK210    MVC   WORK(8),=8C'0'      VALIDATE SQN AS NUMERIC                      
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   VKNUMERR                                                         
*                                                                               
         EX    R1,VKPK             PACK SQN                                     
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         CVB   R0,DUB              CVB                                          
         UNPK  FULL,DUB            FILL OUT TO 8 DIGITS                         
*                                                                               
         CLC   RQSEQ,FULL          IF SQN HAS CHANGED                           
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'           FORCE RE-DISPLAY OF SCREEN                
*                                                                               
         MVC   RQSEQ,FULL          SAVE SEQUENCE NUMBER                         
         STCM  R0,3,RQBSEQ                                                      
*                                                                               
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)       EXECUTED INSTRUCTIONS TO VALIDATE            
VKCLC    CLC   WORK(0),8(R2)         NUMERIC                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
VK300    OI    4(R2),X'20'         INDICATE SQN FIELD IS VALID                  
*                                                                               
*        FILTERS LINE                                                           
*                                                                               
         LA    R2,LINFTRH          POINT TO FILTERS FIELD                       
*                                                                               
         GOTO1 VALIFTR             VALIDATE FILTERS                             
*                                                                               
         OI    4(R2),X'20'         INDICATE FILTERS ARE VALID                   
*                                                                               
         XC    ATOT,ATOT           ZERO ALL TOTALS                              
         XC    SVLSTBAT(SVLSTBTL),SVLSTBAT   ZERO LAST BATCH DATA               
*                                                                               
VKXIT    XC    KEY,KEY             INIT KEY AREA                                
*                                                                               
         B     XIT                                                              
*                                                                               
VKNUMERR MVI   ERROR,NOTNUM                                                     
         B     VKERRX                                                           
*                                                                               
VKBADATE MVI   ERROR,INVDATE                                                    
         B     VKERRX                                                           
*                                                                               
VKERRX   DS    0H                                                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T43018 - INVOICE LIST - LIST'                                   
***********************************************************************         
*                                                                     *         
*        LIST - LIST RECORDS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LIST     DS    0H                                                               
*                                                                               
         OC    VPZMOD,VPZMOD       SKIP IF PZMOD LOADED ALREADY                 
         BNZ   LIST1                                                            
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA  LOAD PHASE T43010 - PZMOD            
*                                                                               
         CLI   DMCB+4,X'FF'        MUST FIND PZMOD                              
         BNE   *+6                                                              
         DC    H'0'                WHERE IS T43010                              
*                                                                               
         L     RF,DMCB             SAVE A(PZMOD)                                
         ST    RF,VPZMOD                                                        
*                                                                               
LIST1    DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       SKIP UNLESS LIST SCREEN                      
         BNE   LISTCSRX                                                         
*                                                                               
*        SET CURSOR TO LAST SELECTED LINE                                       
*                                                                               
         OC    SVINVLST,SVINVLST   SKIP UNLESS INV DISP LAST TIME               
         BZ    LISTCSRX                                                         
*                                                                               
         LA    R0,NUMLINS          NUMBER OF LINES ON SCREEN                    
         LA    R2,LINSELH          POINT TO FIRST SELECT FIELD                  
         LA    R5,INVLIST          POINT TO INVOICE TABLE                       
         USING INVLISTD,R5         ESTABLISH INVOICE TABLE                      
         SR    RF,RF                                                            
*                                                                               
LISTCSRL DS    0H                                                               
*                                                                               
         CLC   INVLISTD(INVENTL),SVINVLST   MATCH LAST INV TO TABLE             
         BE    LISTCSRF                                                         
*                                                                               
LISTCSRC DS    0H                                                               
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT SELECT FIELD                    
         LA    R2,0(RF,R2)                                                      
         IC    RF,0(R2)            FIELD LENGTH                                 
         LA    R2,0(RF,R2)                                                      
         LA    R5,INVLISTD+INVENTL   BUMP TO NEXT INV TABLE ENTRY               
         BCT   R0,LISTCSRL                                                      
*                                                                               
LISTCSRD DS    0H                                                               
*                                                                               
         XC    SVINVLST,SVINVLST   RESET LAST INVOICE DISPLAYED                 
*                                                                               
         B     LISTCSRX                                                         
*                                                                               
LISTCSRF DS    0H                                                               
*                                                                               
         ST    R2,ACURFORC         FORCE CURSOR TO THIS SELECT FIELD            
         XC    SVINVLST,SVINVLST   RESET LAST INVOICE DISPLAYED                 
*                                                                               
         B     LISTCSRX                                                         
*                                                                               
LISTCSRX DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'          SET USER WILL DO ALL I/O                     
*                                                                               
         CLI   MODE,PRINTREP       SKIP UNLESS PRINTING REPORT                  
         BNE   LS050                                                            
*                                                                               
         LM    R0,R1,=A(HEADING,HDHK)   SET PRINTING ADDRESSES                  
         A     R0,RELO                                                          
         ST    R0,SPECS                                                         
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         B     *+8                                                              
LS050    MVI   NLISTS,NUMLINS      SET NUMBER OF LINES ON SCREEN                
*                                                                               
         XC    WPZIND,WPZIND                                                    
         LA    R4,WPZIND           ESTABLISH WORKAREA                           
         USING EZWKRIXD,R4                                                      
*                                                                               
         XC    SVWPZIND,SVWPZIND   INIT SAVEAREA                                
         XC    INVLIST(256),INVLIST   CLEAR INVOICE NUMBER LIST                 
         XC    INVLIST+256(L'INVLIST-256),INVLIST+256                           
         LA    R5,INVLIST          R5 TO START OF LIST                          
         ST    R5,INVLPTR                                                       
         XC    WKRCTS,WKRCTS                                                    
*                                                                               
*        SEE IF THIS IS A CONTINUATION OF LIST                                  
*                                                                               
         OC    SVLSTBAT(SVLSTBTL),SVLSTBAT   IF ANY LAST BATCH DATA             
         BZ    LS052                                                            
*                                     SET DATA FROM LAST BATCH                  
         MVC   EZWIUID,SVBUID                                                   
         MVC   EZWIPUB,SVBPZPUB       PUB ID                                    
         MVC   EZWIMED,SVBMED         MEDIA                                     
         MVI   EZWIDAY,X'98'          DAY = 98                                  
         MVC   WPZIND+8(2),SVBWKFLN                                             
         MVC   UKCIADDR-UKRECD(,R4),SVCIADDR                                    
*                                                                               
LS052    DS    0H                                                               
*                                                                               
         MVC   SVWPZIND,WPZIND     SAVE STARTING INDEX                          
*                                                                               
         B     LSINVLP                GO PROCESS NEXT INVOICE                   
*                                                                               
         EJECT                                                                  
*                                                                               
*        READ NEXT BLOCK FROM WORKER FILE                                       
*                                                                               
LSBATLP  DS    0H                                                               
*                                                                               
* LOOP THRU ALL BATCHES - DISLIN DISPLAYS ALL INVOICES IN A BATCH *             
*                                                                               
         OI    UKFLAG-UKRECD(R4),UKFLDAT                                        
         MVC   UKCIADDR-UKRECD(,R4),SVCIADDR                                    
*                                                                               
         MVC   SVWPZIND,WPZIND     SAVE STARTING INDEX                          
*                                                                               
*        POINT WORKER FILE TO FIRST/NEXT WORKER BLOCK                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,WPZIND,AIO2,WRKFBUFA               
*                                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    LSBATDN              YES                                         
*                                                                               
         MVC   SVCIADDR,UKCIADDR-UKRECD(R4)                                     
*                                                                               
LSINVLP  DS    0H                                                               
*                                                                               
*        READ FIRST/NEXT INVOICE                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,WPZIND,AIO2,WRKFBUFA               
*                                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    LSBATDN              YES                                         
*                                                                               
         MVC   SVCIADDR,UKCIADDR-UKRECD(R4)                                     
*                                                                               
         MVC   SVWPZIND,WPZIND     SAVE FOUND INDEX                             
*                                                                               
         L     R1,IDXRDCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,IDXRDCT                                                       
*                                                                               
         USING UKRECD,R4                                                        
*                                                                               
         TM    UKSTAT,X'08'        THIS FILE ON KEEP                            
         BZ    LS104                NO                                          
*                                                                               
*        ALLOW ALL IF REQUESTING DONE, DEL, CONV                                
*                                                                               
         TM    FTRFLAG,FTRCVQ+FTRDEL+FTRDONE                                    
         BZ    LSINVCN                            BYPASS                        
*                                                                               
         USING EZWKRIXD,R4                                                      
*                                                                               
LS104    CLI   FUIDNUM,X'FF'       OKAY IF 'ALL' IDS REQUESTED                  
         BE    LS110                                                            
*                                                                               
         OC    FUIDNUM,FUIDNUM     IF USER ID REQUESTED                         
         BZ    LS106                                                            
*                                                                               
         CLC   EZWIUID,FUIDNUM        IT MUST BE RIGHT ID                       
         BNE   LSINVCN                                                          
*                                                                               
         B     LS110                                                            
*                                                                               
LS106    CLC   EZWIUID,TWAORIG     ELSE, TEST RIGHT ID                          
         BNE   LSINVCN                                                          
*                                                                               
LS110    CLI   EZWIDAY,X'98'       MUST BE DAY 98                               
         BNE   LSINVCN                                                          
*                                                                               
         OC    RQMED,RQMED         IF MEDIA REQUESTED                           
         BZ    *+14                                                             
         CLC   EZWIMED,RQMED          MUST MATCH ON MEDIA                       
         BNE   LSINVCN                                                          
*                                                                               
         OC    RQPZPUB,RQPZPUB     IF PUB REQUESTED                             
         BZ    *+14                                                             
         CLC   EZWIPUB,RQPZPUB        MUST MATCH ON BATCH PUB ID                
         BNE   LSINVCN                                                          
*                                                                               
         MVC   SRCEPUB,EZWIPUB     SAVE PUB ID                                  
         MVC   SRCEMED,EZWIMED     SAVE MEDIA                                   
         MVC   ORGPZPUB,SRCEPUB                                                 
         MVC   ORGMED,SRCEMED                                                   
*                                                                               
         OC    SVLSTBAT(SVLSTBTL),SVLSTBAT   ANY LAST BATCH DATA                
         BZ    LS120                                                            
*                                                                               
         CLC   EZWIUID,SVBUID      ELSE, TEST RIGHT ID                          
         BNE   LSINVCN                                                          
*                                                                               
         CLC   EZWIPUB,SVBPZPUB                                                 
         BNE   LSINVCN                                                          
         CLC   EZWIMED,SVBMED                                                   
         BNE   LSINVCN                                                          
*                                                                               
LS120    DS    0H                                                               
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    *+14                                                             
         CLC   SVWPZIND+8(2),RQBSEQ                                             
         BNE   LSINVCN                                                          
*                                                                               
         MVC   SVUID,EZWIUID                                                    
         MVC   SVPZPUB,EZWIPUB     SOURCE PUB CODE                              
         MVC   SVMED,EZWIMED                                                    
         MVC   QMED,SVMED                                                       
         MVC   PRNTPUB,PUBPRNT                                                  
*                                                                               
LS124    DS    0H                                                               
*                                                                               
         CLI   FTRMEDIA,0          FILTER ON MEDIA IF ASKED                     
         BE    LS128                                                            
*                                                                               
         CLC   FTRMEDIA,QMED                                                    
         BE    LS128                                                            
*                                                                               
LSINVCN  DS    0H                                                               
*                                                                               
         B     LSINVLP                                                          
*                                                                               
LS128    DS    0H                                                               
*                                                                               
         L     R1,RECRDCT          BUMP RECORD COUNTER                          
         LA    R1,1(,R1)                                                        
         ST    R1,RECRDCT                                                       
*                                                                               
         MVI   ERROR,0             CLEAR ERROR MESSAGE ID                       
         OI    GENSTAT1,CATCHIOR   SET ON RETURN                                
*                                                                               
         GOTO1 CATCHIOS            SEE IF 90% OF MAX                            
*                                                                               
         CLI   ERROR,0             IF ERROR, OVER MAX                           
         BNE   LSMAXIOS                                                         
*                                                                               
*        READ INVOICE INTO WORKAREA                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'READ',EPICWK,WPZIND,AIO2,WRKFBUFA                
*                                                                               
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BNZ   LSBATLP              YES, SKIP                                   
*                                                                               
         L     R3,WRKFBUFA         ESTABLISH RECORD                             
         USING W_RECD,R3                                                        
*                                                                               
         MVC   SVWCMNT,W_DESC                                                   
*                                                                               
         OC    SVLSTBAT(SVLSTBTL),SVLSTBAT   IF ANY LAST BATCH DATA             
         BZ    LS130                                                            
*                                                                               
         CLC   W_AGELD,SVBWKDTC       MATCH ON BATCH DATE                       
         BNE   LSBATLP                                                          
*                                                                               
LS130    OC    RQDTE,RQDTE         IF DATE FILTER ENTERED                       
         BZ    LS140                                                            
*                                                                               
         CLC   W_AGELD,RQDTE                                                    
         BL    LSBATCN                SKIP IF DATE LOW                          
         BE    LS140                  OKAY IF EQUAL                             
*                                                                               
         CLI   DTPLUS,C'Y'            ELSE OKAY IF DATE + ENTERED               
         BNE   LSBATCN                                                          
*                                                                               
LS140    OC    FTRBSDT,FTRBSDT     FILTERING ON BATCH DATE                      
         BZ    LS142                NO                                          
*                                                                               
         CLC   W_AGELD,FTRBSDT                                                  
         BL    LSBATCN             LOW, SKIP                                    
*                                                                               
         CLC   W_AGELD,FTRBEDT                                                  
         BH    LSBATCN             HIGH, SKIP                                   
*                                                                               
*                                  DISPLAY FOUND DATE AND BATCH SEQ             
LS142    MVC   SVWKDTEC,W_AGELD       SAVE DATE AND BATCH SEQ                   
         MVC   SVWKFILN,W_FILENO                                                
         MVC   SVWKSTAT,W_STAT                                                  
*                                                                               
         LA    R2,W_DESC                                                        
         USING EZWKRCMD,R2                                                      
*                                                                               
         OC    FTRSRCE,FTRSRCE     FILTERING ON SOURCE                          
         BZ    LS144                                                            
*                                                                               
         CLC   FTRSRCE,EZWCSRCE                                                 
         BNE   LSBATCN                                                          
*                                                                               
* FOR RECONVERT, CONVERTED, DELETED, DON'T CHECK BATCH CONVERT FLAG             
*                                                                               
* N O T E - UNTIL THE WORKER COMMNENT BUG IS FIXED, DON'T TEST IT *             
*                                                                               
LS144    TM    FTRFLAG,FTRCVQ+FTRDEL+FTRDONE                                    
         BNZ   LS145                                                            
*                                                                               
         TM    EZWCSTAT,X'40'      FULLY CONVERTED                              
         NOP   LSBATCN              YES, BYPASS                                 
*                                                                               
         B     LS146                                                            
*                                                                               
LS145    TM    EZWCSTAT,X'40'      FULLY CONVERTED                              
         NOP   LSBATCN              NO, BYPASS                                  
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
LS146    L     R6,AIO3             SET PZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         LR    R0,R6               CLEAR PZBLOCK                                
         LH    R1,=Y(EZBLOCKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        INITIALIZE PZBLOCK                                                     
*                                                                               
         L     RE,=A(DISLIN)                                                    
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
         MVI   DISLINSW,1          FORCE TO PRINT FIRST LINE                    
*                                                                               
         MVC   EZAGY,AGENCY                                                     
         MVI   EZOFF,C'N'                                                       
         MVC   EZWKRFIL,EPICWK                                                  
         MVC   EZWKRIND,SVWPZIND                                                
         L     R1,WRKFBUFA                                                      
         ST    R1,EZWKRBUF                                                      
         MVC   EZWKRREC,AIO2                                                    
         L     RE,=A(IOA4-(CONHEADH-64))                                        
         LA    RE,0(RA,RE)                                                      
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
*                                                                               
         TM    FTRFLAG,FTRTRACE                                                 
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 VPZMOD,DMCB,(R6)    INVOKE PZMOD                                 
*                                                                               
         XC    SVLSTBAT(SVLSTBTL),SVLSTBAT   ZERO LAST BATCH DATA               
*                                                                               
* RETURN HERE ONLY AT END OF BATCH *                                            
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BE    LS150                NO, NEXT BATCH                              
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BNZ   LSBATDN              YES, ALL DONE                               
*                                                                               
         B     LSBATCN              NO, NEXT BATCH                              
*                                                                               
LS150    LM    RE,R1,JTOT          ADD BATCH TO JOB TOTALS                      
         LA    RE,1(,RE)                                                        
         A     RF,BINV                                                          
         A     R0,BSPTS                                                         
         A     R1,BDOLS                                                         
         STM   RE,R1,JTOT                                                       
*                                                                               
* PRINT BATCH TOTALS *                                                          
*                                                                               
         OC    BINV,BINV          IF ZERO, BYPASS                               
         BZ    LS180                                                            
*                                                                               
         LA    R0,1                                                             
         C     R0,BINV             UNLESS ONLY 1                                
         BE    LS180                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  (B4,BINV),(5,PINV-6),COMMAS=YES,ZERO=NOBLANK                     
         MVC   PINV(8),=C'INVOICES'                                             
*                                                                               
         EDIT  (B4,BSPTS),(5,PSPTS),COMMAS=YES,ZERO=NOBLANK                     
         EDIT (B4,BDOLS),(13,PDOLS),2,COMMAS=YES,ZERO=NOBLANK,MINUS=YES         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS180    XC    BTOT,BTOT           CLEAR BATCH TOTALS                           
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BNZ   LSBATDN              YES, ALL DONE                               
*                                                                               
LSBATCN  DS    0H                                                               
*                                                                               
         B     LSBATLP                                                          
*                                                                               
LSBATDN  XC    SVLSTBAT(SVLSTBTL),SVLSTBAT   ZERO LAST BATCH DATA               
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   LS210                                                            
*                                                                               
         EDIT  (B4,IDXRDCT),(5,LINTL),COMMAS=YES                                
*                                                                               
         MVC   LINTL+5(6),=C'=INDEX'                                            
*                                                                               
         EDIT  (B4,RECRDCT),(5,LINTL+12),COMMAS=YES                             
*                                                                               
         MVC   LINTL+17(5),=C'=GETS'                                            
         OI    LINTLH+6,X'80'                                                   
*                                                                               
LS210    CLI   MODE,PRINTREP       UNLESS PRINTING REPORT                       
         BNE   LISTX                ALL DONE                                    
*                                                                               
*        PRINT JOB TOTALS *                                                     
*                                                                               
         MVC   SVWCSRCE,SPACES     BLANK OUT HEADINGS                           
         MVC   PRNTPUB,SPACES                                                   
         XC    SVWKFILN,SVWKFILN                                                
         XC    SVWKSTAT,SVWKSTAT                                                
         MVI   SVWCSTAT,0                                                       
         MVI   EOJSW,C'Y'          SET TO Y AT EOJ, SUPPRESS HDHK               
*                                                                               
         OC    JBAT,JBAT           DON'T PRINT NONSENSE TOTALS                  
         BZ    LS220                FOR NONE                                    
*                                                                               
         LA    R0,1                                                             
         C     R0,JBAT             DON'T PRINT NONSENSE TOTALS                  
         BE    LS220                OR 1                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  (B4,JBAT),(5,PPRDCD-6),COMMAS=YES,ZERO=NOBLANK                   
*                                                                               
         MVC   PPRDCD(7),=C'BATCHES'                                            
*                                                                               
         EDIT  (B4,JINV),(5,PINV-6),COMMAS=YES,ZERO=NOBLANK                     
*                                                                               
         MVC   PINV(7),=C'INVOICE'                                              
         LA    R0,1                                                             
         C     R0,JINV                                                          
         BE    *+8                                                              
         MVI   PINV+7,C'S'                                                      
*                                                                               
         EDIT  (B4,JSPTS),(5,PSPTS),COMMAS=YES,ZERO=NOBLANK                     
         EDIT (B4,JDOLS),(13,PDOLS),2,COMMAS=YES,ZERO=NOBLANK,MINUS=YES         
*                                                                               
         MVI   ALLOWLIN,2                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS220    NI    LINPUBH+4,X'FF'-X'20' FORCE START AT TOP OF LIST                 
         XC    SVLSTBAT(SVLSTBTL),SVLSTBAT     SET NO LAST BATCH DATA           
         XC    INVLIST(256),INVLIST   CLEAR INVOICE NUMBER LIST                 
         XC    INVLIST+256(L'INVLIST-256),INVLIST+256                           
         XC    WKRCTS,WKRCTS                                                    
*                                                                               
LISTX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
LSMAXIOS DS    0H                                                               
*                                                                               
         MVI   ERROR,PZEMAXIO      TOO MUCH DATA FOR ON-LINE                    
         B     LSERRX                                                           
*                                                                               
LSERRX   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'EPIC'                                                     
         SSPEC H4,3,C'SOURCE'                                                   
         SSPEC H5,3,C'PUBLICATION'                                              
         SSPEC H6,3,C'BATCH DATE'                                               
         SSPEC H6,24,C'SEQ'                                                     
         SSPEC H1,52,C'INVOICE LIST'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'ADVERTISER NAME'                                          
         SSPEC H9,3,C'---------------'                                          
         SSPEC H8,29,C'CODE'                                                    
         SSPEC H9,29,C'----'                                                    
         SSPEC H8,35,C'PRODUCT NAME'                                            
         SSPEC H9,35,C'------------'                                            
         SSPEC H8,62,C'EST'                                                     
         SSPEC H9,62,C'---'                                                     
         SSPEC H8,67,C'PRD'                                                     
         SSPEC H9,67,C'---'                                                     
         SSPEC H8,76,C'EST'                                                     
         SSPEC H9,76,C'---'                                                     
         SSPEC H8,81,C'INVOICE'                                                 
         SSPEC H9,81,C'-------'                                                 
         SSPEC H8,92,C'MONTH'                                                   
         SSPEC H9,92,C'-----'                                                   
         SSPEC H8,99,C'SPOTS'                                                   
         SSPEC H9,99,C'-----'                                                   
         SSPEC H8,110,C'DOLLARS'                                                
         SSPEC H9,110,C'-------'                                                
         SSPEC H8,119,C'STATUS'                                                 
         SSPEC H9,119,C'------'                                                 
         DC    X'00'                                                            
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
***********************************************************************         
*   HEADHOOK ROUTINE FOR REPORT                                       *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         CLI   EOJSW,C'Y'                                                       
         BE    HDHKX                                                            
         MVC   H4+14(4),SVWCSRCE                                                
         MVC   H5+14(13),PRNTPUB                                                
         GOTO1 DATCON,DMCB,(2,SVWKDTEC),(4,H6+14)                               
         SR    R0,R0                                                            
         ICM   R0,3,SVWKFILN                                                    
         EDIT  (R0),(5,H6+27),COMMAS=YES                                        
         SPACE                                                                  
         MVC   H6+34(10),=C'** DONE **'                                         
         TM    SVWKSTAT,X'08'      BATCH FULLY CONVERTED/DELETED                
         BO    *+10                                                             
         MVC   H6+34(19),=C'** NOT CONVERTED **'                                
HDHKX    XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
         TITLE 'T43018 - INVOICE LIST - DISLIN'                                 
***********************************************************************         
*                                                                     *         
*        DISLIN DISPLAY LINE ON LISTAR, OR PRINT LINE TO SPOOL        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DISLIN   NMOD1 0,**DISL**                                                       
*                                                                               
         L     RC,SVRC             ESTABLISH WORKING STORAGE                    
         USING GEND,RC                                                          
*                                                                               
         L     R6,AIO3             SET PZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         XC    LISTAR,LISTAR       INIT LIST DISPLAY AREA                       
*                                                                               
*        AT END OF INVOICE DISPLAY IN LIST                                      
*                                                                               
         CLI   EZMODE,EZINVL       END OF INVOICE                               
         BE    DLINVL                                                           
*                                                                               
*        AT START OF INVOICE DETERMINE IF IT SHOULD BE IN LIST                  
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BE    DLINVP                                                           
*                                                                               
*        AT NEXT INSERTION COUNT NUMBER OF INSERTIONS IN INVOICE                
*                                                                               
         CLI   EZMODE,EZSPTP       PROCESS INSERTION                            
         BE    DLSPTP                                                           
*                                                                               
         B     DLX                 ELSE IGNORE HOOK                             
*                                                                               
         TITLE 'T43018 - INVOICE LIST - DLINVP'                                 
***********************************************************************         
*                                                                     *         
*        CHECK IF INVOICE FITS FILTER CRITERIA FOR DISPLAY            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DLINVP   DS    0H                                                               
*                                                                               
         XC    WINSCTR,WINSCTR     INIT INSERTIONS COUNTER                      
         MVI   PRDSW,0             INIT PRODUCT MISSING SWITCH                  
         XC    SVPRD,SVPRD         INIT INVOICE PRODUCT                         
*                                                                               
         OC    SVLSTBAT(SVLSTBTL),SVLSTBAT   ANY LAST BATCH DATA                
         BZ    DL100                NO JUST SEE IF WE WANT THIS ONE             
*                                                                               
*        THIS IS A RESTART AT TOP OF SCREEN, SKIP TO NEXT INVOICE *             
*        UNLESS ONLY FIRST LINE FOR INVOICE WAS DISPLAYED                       
*                                                                               
         CLC   EZINVSEQ,SVBINVSQ   SAME INVOICE WITHIN BATCH                    
         BL    DLNEXT              BEFORE                                       
         BH    DL100               AFTER                                        
*                                                                               
         CLI   SVBDLNNO,1          IF ONLY 1ST LINE SHOWN                       
         BE    DL100                  REDISPLAY                                 
*                                                                               
         B     DLNEXT              ELSE SKIP                                    
*                                                                               
DL100    DS    0H                                                               
*                                                                               
DL140    CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   DL150                                                            
         CLC   EZIHDMOS,FTRMOS                                                  
         BNE   DLNEXT                                                           
*                                                                               
DL150    DS    0H                                                               
*                                                                               
*        INVOICE NUMBER FILTER                                                  
*                                                                               
         OC    FTRINVNO,FTRINVNO   SKIP IF NO INVOICE NUMBER FILTER             
         BZ    DLINVNX                                                          
*                                                                               
         TM    FTRINVNO,X'40'      LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   DLINVN5                                                          
*                                                                               
         CLI   FTRINVNO,X'FF'      IF LOOKING FOR ABSENCE OF INV NUMBER         
         BNE   DLINVN1                                                          
*                                                                               
         CLC   EZIHINV,SPACES         IF NO SOURCE NUMBER                       
         BNH   DLINVNX                   ACCEPT                                 
*                                                                               
         B     DLNEXT                 ELSE REJECT                               
*                                                                               
DLINVN1  DS    0H                  MATCHING TO A INVOICE NUMBER                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FTRINVLN         INVOICE NUMBER EXECUTE LENGTH                
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHINV(0),FTRINVNO   MATCH TO SOURCE INVOICE NUMBER             
         BE    DLINVNX                                                          
*                                                                               
         B     DLNEXT              ELSE REJECT                                  
*                                                                               
DLINVN5  DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLI   FTRINVNO,X'BF'      IF LOOKING FOR PRESENCE OF INV NAME          
         BNE   DLINVN6                                                          
*                                                                               
         CLC   EZIHINV,SPACES         IF SOURCE NUMBER PRESENT                  
         BH    DLINVNX                   ACCEPT                                 
*                                                                               
         B     DLNEXT                 ELSE REJECT                               
*                                                                               
DLINVN6  DS    0H                  EXCLUDING A INVOICE NUMBER                   
*                                                                               
         OI    FTRINVNO,X'40'      FORCE UPPERCASE                              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FTRINVLN         INVOICE NUMBER EXECUTE LENGTH                
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHINV(0),FTRINVNO    MATCH TO SOURCE INVOICE NUMBER            
         BE    DLINVN8                REJECT                                    
*                                                                               
         NI    FTRINVNO,X'FF'-X'40'   RETURN TO LOWERCASE                       
         B     DLINVNX             ELSE ACCEPT                                  
*                                                                               
DLINVN8  DS    0H                                                               
*                                                                               
         NI    FTRINVNO,X'FF'-X'40'      RETURN TO LOWERCASE                    
         B     DLNEXT                    REJECT                                 
*                                                                               
DLINVNX  DS    0H                                                               
*                                                                               
DL170    CLI   FTRDATE,0           FILTER ON CONVERTED DATE                     
         BNH   DL180                                                            
*                                                                               
         OC    EZIHCVDT,EZIHCVDT IS THIS CONVERTED                              
         BZ    DLNEXT               NO                                          
*                                                                               
         CLI   FTRDATES,0          FILTER ON CONVERTED DATE RANGE               
         BNE   DL174                                                            
*                                                                               
         CLC   EZIHCVDT,FTRDATE FILTER ON EXACT DATE                            
         BNE   DLNEXT                                                           
         B     DL180                                                            
*                                                                               
DL174    CLI   FTRDATES,C'+'       PLUS                                         
         BE    DL176                                                            
         CLI   FTRDATES,C'-'       MINUS                                        
         BE    DL178                                                            
         DC    H'0'                                                             
*                                                                               
DL176    CLC   EZIHCVDT,FTRDATE                                                 
         BL    DLNEXT                                                           
         B     DL180                                                            
*                                                                               
DL178    CLC   EZIHCVDT,FTRDATE                                                 
         BH    DLNEXT                                                           
*                                                                               
DL180    TM    FTRFLAG,FTRDONE     DONE ONLY                                    
         BZ    DL181                                                            
         TM    EZIHCVST,EZIHRCVQ RECONVERT                                      
         BO    DLNEXT                                                           
         TM    EZIHCVST,EZIHCVQ CONVERTED                                       
         BO    DLKEEP               YES                                         
         TM    EZIHCVST,EZIHCDEL DELETED                                        
         BO    DLKEEP               YES                                         
         B     DLNEXT                                                           
*                                                                               
DL181    TM    FTRFLAG,FTRUCVQ     UNCONVERTED ONLY                             
         BZ    DL182                YES                                         
*                                                                               
* IF CONVERTED, DELETED, OR EVEN A RECONVERT (HAS CONVERT ON) BYPASS *          
*                                                                               
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL CONVERTED OR DELETED 01807             
         BNZ   DLNEXT                       YES, BYPASS                         
         B     DLKEEP                                                           
*                                                                               
DL182    TM    FTRFLAG,FTRRCVQ     RECONVERT ONLY                               
         BZ    DL184                                                            
         TM    EZIHCVST,EZIHRCVQ RECONVERT                                      
         BO    DLKEEP               YES                                         
         B     DLNEXT                                                           
*                                                                               
DL184    TM    FTRFLAG,FTRCVQ      CONVERTED ONLY                               
         BZ    DL186                                                            
         TM    EZIHCVST,EZIHRCVQ RECONVERT                                      
         BO    DLNEXT                                                           
         TM    EZIHCVST,EZIHCVQ CONVERTED                                       
         BO    DLKEEP               YES                                         
         B     DLNEXT                                                           
*                                                                               
DL186    TM    FTRFLAG,FTROVR      OVERRIDES ONLY                               
         BZ    DL188                                                            
         TM    EZIHCVST,EZIHCOVR OVERRIDE                                       
         BO    DLKEEP               YES                                         
         B     DLNEXT                                                           
*                                                                               
DL188    TM    FTRFLAG,FTRDEL      DELETES ONLY                                 
         BZ    DL190                                                            
         TM    EZIHCVST,EZIHCDEL DELETED                                        
         BZ    DLNEXT               NO, BYPASS                                  
         B     DLKEEP                                                           
*                                                                               
DL190    TM    EZIHCVST,EZIHCDEL DELETED                                        
         BNO   DLKEEP               YES, BYPASS                                 
*                                                                               
*        UNWANTED INVOICE, SKIP TO NEXT INVOICE IN BATCH                        
*                                                                               
DLNEXT   XC    LISTAR,LISTAR                                                    
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         B     DLINVPX                                                          
*                                                                               
*        WANTED INVOICE                                                         
*                                                                               
DLKEEP   MVC   SVINVSEQ,EZRECSEQ     SAVE INVOICE HEADER SEQ                    
*                                                                               
DLINVPX  DS    0H                                                               
         B     DLX                                                              
*                                                                               
         TITLE 'T43018 - INVOICE LIST - DLSPTP'                                 
***********************************************************************         
*                                                                     *         
*        COUNT NUMBER OF INSERTIONS. THIS WAY WE DROP REGIONAL COMBOS *         
*           FROM COUNT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DLSPTP   DS    0H                                                               
*                                                                               
         ICM   RF,15,WINSCTR       BUMP INSERTIONS COUNTER                      
         LA    RF,1(RF)                                                         
         STCM  RF,15,WINSCTR                                                    
*                                                                               
         CLC   EZSPLPRC,SPACES     IF LOOKED UP PRODUCT                         
         BH    *+10                                                             
         CLC   EZSPCVPR,SPACES     AND OVERRIDE PRODUCT ARE MISSING             
         BH    *+12                                                             
         MVI   PRDSW,C'M'              RECORD FACT                              
         B     DLSPTPX                                                          
*                                                                               
         CLI   PRDSW,C'M'          SKIP IF WE HAVE A MISSING PRD                
         BE    DLSPTPX                                                          
*                                                                               
         OC    SVPRD,SVPRD         IF FIRST PRODUCT FOR INVOICE                 
         BNZ   DLSPTP5                                                          
*                                                                               
         MVC   SVPRD,EZSPLPRC         SAVE PRODUCT CODE                         
*                                                                               
         CLC   SVPRD,SPACES                                                     
         BH    *+10                                                             
         MVC   SVPRD,EZSPCVPR         ONE IS NON-SPACE                          
*                                                                               
DLSPTP5  DS    0H                                                               
*                                                                               
         CLC   EZSPCVPR,SPACES     SKIP IF NO OVERRIDE                          
         BNH   DLSPTP7                                                          
*                                                                               
         CLC   EZSPCVPR,SVPRD      OKAY IF SAME AS FIRST PRODUCT                
         BE    *+8                                                              
         MVI   PRDSW,C'X'          ELSE INDICATE MULTIPLE PRODUCTS              
*                                                                               
         B     DLSPTPX                                                          
*                                                                               
DLSPTP7  DS    0H                                                               
*                                                                               
         CLC   EZSPLPRC,SVPRD      OKAY IF SAME AS FIRST PRODUCT                
         BE    *+8                                                              
         MVI   PRDSW,C'X'          ELSE INDICATE MULTIPLE PRODUCTS              
*                                                                               
         B     DLSPTPX                                                          
*                                                                               
DLSPTPX  DS    0H                                                               
         B     DLX                                                              
*                                                                               
         TITLE 'T43018 - INVOICE LIST - DLINVL'                                 
***********************************************************************         
*                                                                     *         
*        HAVE FILTERED AT INVOICE HEADER, NOW HAVE HEADER AND TOTAL   *         
*           DISPLAY IN LIST                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DLINVL   DS    0H                                                               
*                                                                               
*        CLIENT CODE FILTER                                                     
*                                                                               
         OC    FTRQCLT,FTRQCLT     SKIP IF NO CLIENT CODE FILTER                
         BZ    DLQCLTX                                                          
*                                                                               
         TM    FTRQCLT,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   DLQCLT5                                                          
*                                                                               
         CLC   FTRQCLT,=X'FFFFFF'  IF LOOKING FOR ABSENCE OF CLT CODE           
         BNE   DLQCLT1                                                          
*                                                                               
         OC    EZIHCVAD,EZIHCVAD      IF NO OVERRIDE                            
         BNZ   *+10                                                             
         OC    EZIHLADC,EZIHLADC      OR LOOKED-UP CLIENT CODE                  
         BZ    DLQCLTX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLQCLT1  DS    0H                  MATCHING TO A CLIENT CODE                    
*                                                                               
         CLC   FTRQCLT,EZIHCVAD    IF CLIENT CODE MATCHES OVERRIDE              
         BE    DLQCLTX                 ACCEPT                                   
*                                                                               
         CLC   EZIHCVAD,SPACES     REJECT IF CONVERTED CLIENT PRESENT           
         BH    DLX                                                              
*                                                                               
         CLC   FTRQCLT,EZIHLADC    IF IT MATCHES  LOOKED-UP CLIENT CODE         
         BE    DLQCLTX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLQCLT5  DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLC   FTRQCLT,=X'BFFFFF'  IF LOOKING FOR PRESENCE OF CLT CODE          
         BNE   DLQCLT6                                                          
*                                                                               
         OC    EZIHCVAD,EZIHCVAD      IF NO OVERRIDE                            
         BNZ   *+10                                                             
         OC    EZIHLADC,EZIHLADC      OR LOOKED-UP CLIENT CODE                  
         BZ    DLX                       REJECT                                 
*                                                                               
         B     DLQCLTX                ELSE ACCEPT                               
*                                                                               
DLQCLT6  DS    0H                  EXCLUDING A CLIENT CODE                      
*                                                                               
         OI    FTRQCLT,X'40'       FORCE UPPERCASE                              
*                                                                               
         CLC   FTRQCLT,EZIHCVAD    IF CLIENT CODE MATCHES OVERRIDE              
         BE    DLQCLT7                REJECT                                    
*                                                                               
         CLC   EZIHCVAD,SPACES     ACCEPT IF CONVERTED CLIENT PRESENT           
         BH    DLQCLT8                                                          
*                                                                               
         CLC   FTRQCLT,EZIHLADC    IF CC MATCHES LOOKED-UP CLIENT CODE          
         BE    DLQCLT7                REJECT                                    
*                                                                               
         B     DLQCLT8             ELSE ACCEPT                                  
*                                                                               
DLQCLT7  DS    0H                                                               
*                                                                               
         NI    FTRQCLT,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     DLX                       REJECT                                 
*                                                                               
DLQCLT8  DS    0H                                                               
*                                                                               
         NI    FTRQCLT,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     DLQCLTX                ELSE ACCEPT                               
*                                                                               
DLQCLTX  DS    0H                                                               
*                                                                               
*        CLIENT NAME FILTER                                                     
*                                                                               
         OC    FTRCLTN,FTRCLTN     SKIP IF NO CLIENT NAME FILTER                
         BZ    DLCLTNX                                                          
*                                                                               
         TM    FTRCLTN,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   DLCLTN5                                                          
*                                                                               
         CLI   FTRCLTN,X'FF'       IF LOOKING FOR ABSENCE OF CLT NAME           
         BNE   DLCLTN1                                                          
*                                                                               
         CLC   EZIHLADN,SPACES        IF NO OVERRIDE                            
         BH    *+10                                                             
         CLC   EZIHAVNM,SPACES        OR SOURCE NAME                            
         BNH   DLCLTNX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLCLTN1  DS    0H                  MATCHING TO A CLIENT NAME                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FTRCLTNL         CLIENT NAME EXECUTE LENGTH                   
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHLADN(0),FTRCLTN    MATCH TO DDS OVERRIDE NAME                
         BE    DLCLTNX                                                          
*                                                                               
         CLC   EZIHLADN,SPACES     REJECT IF CONVERTED NAME PRESENT             
         BH    DLX                                                              
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHAVNM(0),FTRCLTN    MATCH TO SOURCE CLIENT NAME               
         BE    DLCLTNX                                                          
*                                                                               
         B     DLX                 ELSE REJECT                                  
*                                                                               
DLCLTN5  DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLI   FTRCLTN,X'BF'       IF LOOKING FOR PRESENCE OF CLT NAME          
         BNE   DLCLTN6                                                          
*                                                                               
         CLC   EZIHLADN,SPACES        IF OVERRIDE                               
         BH    *+10                                                             
         CLC   EZIHAVNM,SPACES        OR SOURCE NAME PRESENT                    
         BH    DLCLTNX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLCLTN6  DS    0H                  EXCLUDING A CLIENT NAME                      
*                                                                               
         OI    FTRCLTN,X'40'       FORCE UPPERCASE                              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FTRCLTNL         CLIENT NAME EXECUTE LENGTH                   
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHLADN(0),FTRCLTN    MATCH TO DDS OVERRIDE NAME                
         BE    DLCLTN8                REJECT                                    
*                                                                               
         CLC   EZIHLADN,SPACES     ACCEPT IF CONVERTED NAME PRESENT             
         BH    DLCLTN7                                                          
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHAVNM(0),FTRCLTN    MATCH TO SOURCE CLIENT NAME               
         BE    DLCLTN8                REJECT                                    
*                                                                               
DLCLTN7  DS    0H                                                               
*                                                                               
         NI    FTRCLTN,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     DLCLTNX             ELSE ACCEPT                                  
*                                                                               
DLCLTN8  DS    0H                                                               
*                                                                               
         NI    FTRCLTN,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     DLX                       REJECT                                 
*                                                                               
DLCLTNX  DS    0H                                                               
*                                                                               
*        PRODUCT CODE FILTER                                                    
*                                                                               
         OC    FTRQPRD,FTRQPRD     SKIP IF NO PRODUCT CODE FILTER               
         BZ    DLQPRDX                                                          
*                                                                               
         TM    FTRQPRD,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   DLQPRD5                                                          
*                                                                               
         CLC   FTRQPRD,=X'FFFFFF'  IF LOOKING FOR ABSENCE OF PRD CODE           
         BNE   DLQPRD1                                                          
*                                                                               
         OC    EZIHCVPR,EZIHCVPR      IF NO OVERRIDE                            
         BNZ   *+10                                                             
         OC    EZIHLPRC,EZIHLPRC      OR LOOKED-UP PRODUCT CODE                 
         BZ    DLQPRDX                   ACCEPT                                 
*                                                                               
         CLC   EZIHLPRC,=C'***'    IF VARIOUS PRODUCTS                          
         BNE   *+12                                                             
         CLI   PRDSW,C'M'          AND SOME DETAIL MISSING PRODUCT              
         BE    DLQPRDX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLQPRD1  DS    0H                  MATCHING TO A PRODUCT CODE                   
*                                                                               
         CLC   FTRQPRD,EZIHCVPR    IF PRODUCT CODE MATCHES OVERRIDE             
         BE    DLQPRDX                ACCEPT                                    
*                                                                               
         CLC   EZIHCVPR,SPACES     REJECT IF CONVERTED PROD PRESENT             
         BH    DLX                                                              
*                                                                               
         CLC   FTRQPRD,EZIHLPRC    IF LOOKED-UP PRODUCT CODE MATCHES            
         BE    DLQPRDX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLQPRD5  DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLC   FTRQPRD,=X'BFFFFF'  IF LOOKING FOR PRESENCE OF PRD CODE          
         BNE   DLQPRD6                                                          
*                                                                               
         OC    EZIHCVPR,EZIHCVPR      IF NO OVERRIDE                            
         BNZ   *+10                                                             
         OC    EZIHLPRC,EZIHLPRC      OR LOOKED-UP PRODUCT CODE                 
         BZ    DLX                       REJECT                                 
*                                                                               
         CLC   EZIHLPRC,=C'***'    IF VARIOUS PRODUCTS                          
         BNE   *+12                                                             
         CLI   PRDSW,C'M'          AND SOME DETAIL MISSING PRODUCT              
         BE    DLX                       REJECT                                 
*                                                                               
         B     DLQPRDX                ELSE ACCEPT                               
*                                                                               
DLQPRD6  DS    0H                  EXCLUDING A PRODUCT CODE                     
*                                                                               
         OI    FTRQPRD,X'40'       FORCE UPPERCASE                              
*                                                                               
         CLC   FTRQPRD,EZIHCVPR    IF PRODUCT CODE MATCHES OVERRIDE             
         BE    DLQPRD7                REJECT                                    
*                                                                               
         CLC   EZIHCVPR,SPACES     ACCEPT IF CONVERTED PROD PRESENT             
         BH    DLQPRD8                                                          
*                                                                               
         CLC   FTRQPRD,EZIHLPRC     IF LOOKED-UP PRODUCT CODE MATCHES           
         BE    DLQPRD7                REJECT                                    
*                                                                               
         B     DLQPRD8             ELSE ACCEPT                                  
*                                                                               
DLQPRD7  DS    0H                  EXCLUDING A PRODUCT CODE                     
*                                                                               
         NI    FTRQPRD,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     DLX                       REJECT                                 
*                                                                               
DLQPRD8  DS    0H                  EXCLUDING A PRODUCT CODE                     
*                                                                               
         NI    FTRQPRD,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     DLQPRDX                ELSE ACCEPT                               
*                                                                               
DLQPRDX  DS    0H                                                               
*                                                                               
*        PRODUCT NAME FILTER                                                    
*                                                                               
         OC    FTRPRDN,FTRPRDN     SKIP IF NO PRODUCT NAME FILTER               
         BZ    DLPRDNX                                                          
*                                                                               
         TM    FTRPRDN,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   DLPRDN5                                                          
*                                                                               
         CLI   FTRPRDN,X'FF'       IF LOOKING FOR ABSENCE OF PRD NAME           
         BNE   DLPRDN1                                                          
*                                                                               
         CLC   EZIHLPRN,SPACES        IF NO OVERRIDE                            
         BH    *+10                                                             
         CLC   EZIHPRNM,SPACES        OR SOURCE NAME                            
         BNH   DLPRDNX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLPRDN1  DS    0H                  MATCHING TO A PRODUCT NAME                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FTRPRDNL         PRODUCT NAME EXECUTE LENGTH                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHLPRN(0),FTRPRDN    MATCH TO DDS OVERRIDE NAME                
         BE    DLPRDNX                                                          
*                                                                               
         CLC   EZIHLPRN,SPACES     REJECT IF CONVERTED PROD PRESENT             
         BH    DLX                                                              
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHPRNM(0),FTRPRDN    MATCH TO SOURCE PRODUCT NAME              
         BE    DLPRDNX                                                          
*                                                                               
         B     DLX                 ELSE REJECT                                  
*                                                                               
DLPRDN5  DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLI   FTRPRDN,X'BF'       IF LOOKING FOR PRESENCE OF PRD NAME          
         BNE   DLPRDN6                                                          
*                                                                               
         CLC   EZIHLPRN,SPACES        IF OVERRIDE                               
         BH    *+10                                                             
         CLC   EZIHPRNM,SPACES        OR SOURCE NAME PRESENT                    
         BH    DLPRDNX                   ACCEPT                                 
*                                                                               
         B     DLX                    ELSE REJECT                               
*                                                                               
DLPRDN6  DS    0H                  EXCLUDING A PRODUCT NAME                     
*                                                                               
         OI    FTRPRDN,X'40'       FORCE UPPERCASE                              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FTRPRDNL         PRODUCT NAME EXECUTE LENGTH                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHLPRN(0),FTRPRDN    MATCH TO DDS OVERRIDE NAME                
         BE    DLPRDN8                REJECT                                    
*                                                                               
         CLC   EZIHLPRN,SPACES     ACCEPT IF CONVERTED PROD PRESENT             
         BH    DLPRDN7                                                          
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHPRNM(0),FTRPRDN    MATCH TO SOURCE PRODUCT NAME              
         BE    DLPRDN8                REJECT                                    
*                                                                               
DLPRDN7  DS    0H                                                               
*                                                                               
         NI    FTRPRDN,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     DLPRDNX             ELSE ACCEPT                                  
*                                                                               
DLPRDN8  DS    0H                                                               
*                                                                               
         NI    FTRPRDN,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     DLX                       REJECT                                 
*                                                                               
DLPRDNX  DS    0H                                                               
*                                                                               
         CLI   DISLINSW,2          SKIP IF SECOND LINE ONLY WANTED              
         BE    DLINVL2                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVWKDTEC),(4,LBDTE) BATCH DATE                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVWKFILN                                                    
         EDIT  (R0),(4,LBSEQ)      BATCH SEQUENCE NUMBER                        
*                                                                               
         MVC   LSRCE,SVWCSRCE      SOURCE                                       
*                                                                               
         MVC   LMON(3),EZIHDMOS    MONTH OF SERVICE                             
         MVC   LMON+3(2),EZIHDMOS+4   MMMYY FROM MMM/YY                         
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(C'0',EZSNPUB),(0,LPUB)                            
         MVC   LPUBN,EZSNANM1      PUB NAME                                     
         MVC   LZONE,EZSNANM2      ZONE NAME                                    
*                                                                               
         MVC   DMDSKADD,=X'0000FFFF'  DUMMY DISK ADDRESS                        
*                                                                               
*        ADD ENTRY TO INVOICE LIST                                              
*                                                                               
         L     R5,INVLPTR          POINTER INTO SAVE LIST                       
         USING INVLISTD,R5         ESTABLISH ENTRY IN INVOICE LIST              
*                                                                               
*        FILL IN ENTRY IN INVOICE LIST                                          
*                                                                               
         MVC   INVUID,SVUID        USER ID                                      
         MVC   INVMEDIA,SVMED      MEDIA                                        
         MVC   INVPZPUB,SVPZPUB    SOURCE PUB ID                                
         MVC   INVPUB,EZSNPUB      DDS PUB ID                                   
         MVC   INVBDTE,SVWKDTEC    BATCH DATE                                   
         MVC   INVBSEQ,SVWKFILN    BATCH SEQUENCE NUMBER                        
         MVC   INVRSEQ,EZINVSEQ    SEQUENCE NUMBER IN BATCH                     
         MVC   INVDLNNO,DISLINSW   LINE NUMBER OF DISPLAY                       
*                                                                               
         LA    R5,INVNEXT          BUMP NEXT AVAILABLE ENTRY PTR                
         ST    R5,INVLPTR                                                       
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVI   NLISTS,NUMLINS                                                   
*                                                                               
         MVC   SVBUID,SVUID                                                     
         MVC   SVBPZPUB,SVPZPUB                                                 
         MVC   SVBPUB,SVPUB                                                     
         MVC   SVBMED,SVMED                                                     
         MVC   SVBWKFLN,SVWKFILN                                                
         MVC   SVBWKDTC,SVWKDTEC                                                
         MVC   SVBINVSQ,EZINVSEQ                                                
         MVC   SVBDLNNO,DISLINSW   LINE NUMBER OF DISPLAY                       
*                                                                               
         GOTO1 LISTMON             PUT OUT LINE                                 
*                                                                               
         MVI   DISLINSW,2          SET TO PRINT SECOND LINE OF DISPLAY          
*                                                                               
DLINVL2  DS    0H                  DISPLAY SECOND LINE OF DISPLAY               
*                                                                               
         XC    LISTAR,LISTAR       INIT DISPLAY AREA                            
*                                                                               
*        CLIENT FIELDS                                                          
*                                                                               
         CLC   EZIHCVAD,SPACES     IF CLIENT CODE OVERWRITTEN                   
         BNH   *+14                                                             
         MVC   LCLT,EZIHCVAD          DISPLAY CODE                              
         B     DLDCLT05                                                         
*                                                                               
         CLC   EZIHLADC,SPACES     IF CLIENT CODE LOOKED UP                     
         BNH   DLDCLT10                                                         
*                                                                               
         MVC   LCLT,EZIHLADC          DISPLAY CLIENT CODE                       
*                                                                               
DLDCLT05 DS    0H                                                               
*                                                                               
* READ CLIENT HEADER *                                                          
*                                                                               
         MVI   LCLTSEP,C'/'        SET CODE-NAME SEPARATOR                      
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   PCLTKAGY,QAGY                                                    
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,PCLTKIDQ                                                
         MVC   PCLTKCLT,LCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DLDCLT20                                                         
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   LCLTN,PCLTNAME         DISPLAY CLIENT NAME                       
*                                                                               
         B     DLDCLT20                                                         
*                                                                               
DLDCLT10 DS    0H                                                               
*                                                                               
         MVC   LCLTWKR,EZIHAVNM    ELSE DISPLAY INCOMING CLIENT NAME            
*                                                                               
DLDCLT20 DS    0H                                                               
*                                                                               
*        PRODUCT FIELDS                                                         
*                                                                               
         CLC   EZIHCVPR,SPACES     IF PRODUCT CODE OVERWRITTEN                  
         BNH   *+14                                                             
         MVC   LPRD,EZIHCVPR          DISPLAY CODE                              
         B     DLDPRD0C                                                         
*                                                                               
         CLC   EZIHLPRC,SPACES     IF WE HAVE A PRINTPAK PRODUCT CODE           
         BNH   DLDPRD10                                                         
*                                                                               
         MVC   LPRD,EZIHLPRC          DISPLAY PRODUCT CODE                      
*                                                                               
DLDPRD0C DS    0H                                                               
*                                                                               
         CLC   LPRD,=C'***'        IF 'VARIOUS' PRODUCT CODE                    
         BNE   DLDPRD0F                                                         
*                                                                               
         CLI   PRDSW,C'M'          AND DETAIL IS MISSING PRODUCT                
         BNE   DLDPRD0D                                                         
*                                                                               
         MVC   LPRD,SPACES            ERASE PRODUCT CODE                        
*                                                                               
         B     DLDPRD20                                                         
*                                                                               
DLDPRD0D DS    0H                                                               
*                                                                               
         CLI   PRDSW,0             IF SINGLE PRODUCT FOR INVOICE                
         BNE   DLDPRD0F                                                         
*                                                                               
         MVC   LPRD,SVPRD             USE IT                                    
*                                                                               
DLDPRD0F DS    0H                                                               
*                                                                               
*                                                                               
* READ PRODUCT HEADER *                                                         
*                                                                               
         MVI   LPRDSEP,C'/'        SET CODE-NAME SEPARATOR                      
*                                                                               
         CLC   LPRD,=C'***'        SKIP IF 'VARIOUS' PRODUCT CODE               
         BE    DLDPRD20                                                         
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING PPRDRECD,R4                                                      
*                                                                               
         MVC   PPRDKAGY,QAGY                                                    
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,PPRDKIDQ                                                
         MVC   PPRDKCLT,LCLT                                                    
         MVC   PPRDKPRD,LPRD                                                    
         OC    PPRDKPRD,SPACES                                                  
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DLDPRD20                                                         
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         USING PPRDRECD,R4                                                      
*                                                                               
         MVC   LPRDN,PPRDNAME         DISPLAY PRODUCT NAME                      
*                                                                               
         B     DLDPRD20                                                         
*                                                                               
DLDPRD10 DS    0H                                                               
*                                                                               
         CLC   EZIHPRNM,SPACES     IF THERE IS A PRODUCT NAME                   
         BNH   *+20                                                             
         MVI   LPRDWKR-1,C' '         PUT IN SEPARATORS                         
         MVC   LPRD,SPACES                                                      
         MVC   LPRDWKR,EZIHPRNM       DISPLAY INCOMING PRODUCT NAME             
*                                                                               
DLDPRD20 DS    0H                                                               
*                                                                               
         CLC   LPRDN,SPACES        IF NO NAME GIVEN                             
         BH    *+20                                                             
         CLC   LPRD,=C'***'        AND 'VARIOUS' PRODUCT CODE                   
         BNE   *+10                                                             
         MVC   LPRDN(7),=C'VARIOUS'   SET 'VARIOUS' AS NAME                     
*                                                                               
*        ESTIMATE FIELDS                                                        
*                                                                               
         OC    EZIHCVES,EZIHCVES   SKIP IF NO ESTIMATE AVAILABLE                
         BZ    DLDEST20                                                         
*                                                                               
         EDIT  (B2,EZIHCVES),(3,LEST)                                           
*                                                                               
* READ ESTIMATE HEADER *                                                        
*                                                                               
         B     DLDESTX             SKIP ESTNAME FOR NOW                         
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING PESTRECD,R4                                                      
*                                                                               
         MVC   PESTKAGY,QAGY                                                    
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,PESTKIDQ                                                
         MVC   PESTKCLT,LCLT                                                    
         MVC   PESTKPRD,LPRD                                                    
         MVC   PESTKEST,EZIHCVES                                                
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DLDEST20                                                         
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         USING PESTRECD,R4                                                      
*                                                                               
*****    MVC   LESTN,PESTNAME      DISPLAY INCOMING ESTIMATE NAME               
*                                                                               
         B     DLDESTX                                                          
*                                                                               
DLDEST20 DS    0H                                                               
*                                                                               
         CLC   EZIHEST,SPACES      IF THERE IS AN ESTIMATE                      
         BNH   DLDESTX                                                          
*******  MVI   LESTWKR-1,C' '         PUT IN SEPARATORS                         
         MVC   LEST,SPACES                                                      
*******  MVC   LESTWKR,EZIHEST        DISPLAY ESTIMATE CODE                     
*                                                                               
DLDESTX  DS    0H                                                               
*                                                                               
         MVC   LINV,EZIHINV        INVOICE NUMBER                               
*                                                                               
         OC    EZIHCVDT,EZIHCVDT   IF A CONVERTED INVOICE                       
         BZ    DLCDTX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(4,LCDTE)  DISP CONV DATE               
*                                                                               
         TM    EZIHCVST,X'40'      THIS BETTER BE CONVERTED                     
         BO    *+6                                                              
         DC    H'0'                OOPS!                                        
*                                                                               
DLCDTX   DS    0H                                                               
*                                                                               
         EDIT  (B4,WINSCTR),(3,LINS),COMMAS=YES,ZERO=NOBLANK                    
*                                                                               
         TM    EZIHCVST,EZIHCVQ TEST CONVERTED                                  
         BZ    *+8                  NO                                          
         MVI   LCVQ,C'C'                                                        
*                                                                               
         TM    EZIHCVST,EZIHCDEL TEST DELETED                                   
         BZ    *+8                  NO                                          
         MVI   LCVQ,C'D'                                                        
*                                                                               
         TM    EZIHCVST,EZIHRCVQ TEST RECONVERT                                 
         BZ    *+8                  NO                                          
         MVI   LRCVQ,C'R'                                                       
*                                                                               
         TM    EZIHCVST,EZIHCOVR TEST OVERRIDE CLT/PRD/EST                      
         BZ    *+8                  NO                                          
         MVI   LOVR,C'O'                                                        
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL280                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EZRECSEQ                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LRECSEQ(3),DUB                                                   
*                                                                               
         CLI   LRECSEQ,C'0'                                                     
         BNE   *+8                                                              
         MVI   LRECSEQ,C' '        DON'T PRINT LEADING ZERO                     
*                                                                               
DL280    CLI   MODE,PRINTREP       DON'T DO IT FOR PRINTED REPORT               
         BE    DL290                                                            
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL284                                                            
*                                                                               
         EDIT  (B4,IDXRDCT),(5,LINTL),COMMAS=YES                                
         MVC   LINTL+5(6),=C'=INDEX'                                            
*                                                                               
         EDIT  (B4,RECRDCT),(5,LINTL+12),COMMAS=YES                             
         MVC   LINTL+17(5),=C'=GETS'                                            
*                                                                               
         OI    LINTLH+6,X'80'                                                   
*                                                                               
DL284    L     R5,INVLPTR          POINTER INTO SAVE LIST                       
         USING INVLISTD,R5                                                      
         SPACE                                                                  
         MVC   INVUID,SVUID                                                     
         MVC   INVPZPUB,SVPZPUB                                                 
         MVC   INVPUB,EZSNPUB      DDS PUB ID                                   
         MVC   INVMEDIA,SVMED      MEDIA                                        
         MVC   INVBDTE,SVWKDTEC                                                 
         MVC   INVBSEQ,SVWKFILN                                                 
         MVC   INVRSEQ,EZINVSEQ                                                 
         MVC   INVDLNNO,DISLINSW   LINE NUMBER OF DISPLAY                       
         LA    R5,INVNEXT                                                       
         ST    R5,INVLPTR                                                       
         DROP  R5                                                               
         SPACE                                                                  
         MVC   DMDSKADD(4),=X'0000FFFF'   DUMMY DISK ADDRESS                    
         MVI   NLISTS,NUMLINS                                                   
         MVC   SVBUID,SVUID                                                     
         MVC   SVBPZPUB,SVPZPUB                                                 
         MVC   SVBPUB,SVPUB                                                     
         MVC   SVBMED,SVMED                                                     
         MVC   SVBWKFLN,SVWKFILN                                                
         MVC   SVBWKDTC,SVWKDTEC                                                
         MVC   SVBINVSQ,EZINVSEQ                                                
         MVC   SVBDLNNO,DISLINSW   LINE NUMBER OF DISPLAY                       
         SPACE                                                                  
         GOTO1 LISTMON                                                          
         SPACE                                                                  
         XC    SVLSTBAT(SVLSTBTL),SVLSTBAT   CLEAR SCREEN RESTART               
         B     DLX                                                              
         SPACE                                                                  
DL290    MVC   PADVNM,EZIHAVNM                                                  
         MVC   PADVCD,LCLT                                                      
         MVC   PPRDNM,EZIHPRNM                                                  
         MVC   PESTO,EZIHEST                                                    
         MVC   PPRDCD,LPRD                                                      
         MVC   PEST,LEST                                                        
         MVC   PINV,EZIHINV                                                     
         SPACE                                                                  
         MVC   PMON(3),EZIHDMOS                                                 
         MVC   PMON+3(2),EZIHDMOS+4                                             
         SPACE                                                                  
         EDIT  (B4,EZIHTSPN),(5,PSPTS),COMMAS=YES,ZERO=NOBLANK                  
         EDIT  (B4,EZITBDUE),(13,PDOLS),2,COMMAS=YES,ZERO=NOBLANK,     C        
               MINUS=YES                                                        
         SPACE                                                                  
         MVC   PSTAT(1),LRCVQ                                                   
         MVC   PSTAT+1(1),LOVR                                                  
         TM    EZIHCVST,EZIHCVQ                                                 
         BZ    DL310                                                            
         OC    EZIHCVDT,EZIHCVDT CONVERTED DATE                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   PSTAT+2,C'C'                                                     
         MVI   PSTAT+3,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(5,PSTAT+4)                             
         SPACE                                                                  
DL310    CLI   LCLT+3,C'*'                                                      
         BNE   *+16                                                             
         MVC   PADVCD+3(1),EZIHAVCD+3                                           
         MVC   PADVCD+132,=C'***'                                               
         SPACE                                                                  
         CLI   LPRD+3,C'*'                                                      
         BNE   *+16                                                             
         MVC   PPRDCD+3(1),EZIHPRCD+3                                           
         MVC   PPRDCD+132,=C'***'                                               
         SPACE                                                                  
         LM    RE,R0,BTOT                                                       
         LA    RE,1(,RE)                                                        
         A     RF,EZIHTSPN                                                      
         ICM   R1,15,EZITBDUE                                                   
         AR    R0,R1                                                            
         STM   RE,R0,BTOT                                                       
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DLX                                                              
         MVC   PSTAT(3),=C'IH='                                                 
         EDIT  (B2,SVINVSEQ),(3,PSTAT+3),ALIGN=LEFT                             
         MVC   PSTAT+8(3),=C'IT='                                               
         EDIT  (B2,EZRECSEQ),(3,PSTAT+11),ALIGN=LEFT                            
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     DLX                                                              
         SPACE                                                                  
DLX      XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  R4,R6,RB,RC                                                      
NUMLINS  EQU   14                                                               
T43018X  DS    0D                                                               
         TITLE 'T43018 - INVOICE LIST - PPEZFWORKD'                             
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* PPEZFWORKD                                                                    
       ++INCLUDE PPEZFWORKD                                                     
* PPEZFCNVWD                                                                    
       ++INCLUDE PPEZFCNVWD                                                     
*                                                                               
         TITLE 'T43007 - EPIC- INVOICE HEADER WORKAREA'                         
***********************************************************************         
*                                                                     *         
*        WORKAREA FOR INVOICE HEADER PROCESSING                       *         
*                                                                     *         
***********************************************************************         
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSAPPLW                                                         
         DS    0D                                                               
WINSCTR  DS    F                   NUMBER OF INSERTIONS IN INVOICE              
*                                                                               
PRDSW    DS    CL1                 C'M' - DETAIL IS MISSING PROD CODE           
*                                                                               
SVPRD    DS    CL3                 SINGLE PRODUCT FOR INVOICE                   
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPGENPZ                                                        
         EJECT                                                                  
         PRINT ON                                                               
* PPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* PPEZFE8D                                                                      
       ++INCLUDE PPEZFE8D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONHEADH-64+X'2000'                                              
HELPSAVE DS    XL512                                                            
IOA4     DS    XL4096              4TH IOAREA                                   
*                                                                               
         EJECT                                                                  
* PZBLOCK                                                                       
         PRINT OFF                                                              
       ++INCLUDE PZBLOCK                                                        
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
         EJECT                                                                  
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
* PRGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
* PRHELPCB                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRHELPCB                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* PZGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PZGLOBEQUS                                                     
         PRINT ON                                                               
         TITLE 'T43018 - INVOICE LIST - LIST DSECT'                             
***********************************************************************         
*                                                                     *         
*        LAYOUT OF LIST LINE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GEND     DSECT                                                                  
         ORG   LISTAR        4                                                  
LBDTE    DS    CL5           9                                                  
         DS    CL1          10                                                  
LBSEQ    DS    CL4          14                                                  
         DS    CL1          15                                                  
LSRCE    DS    CL4          19                                                  
         DS    CL1          20                                                  
LMON     DS    CL5          25                                                  
         DS    CL1          26                                                  
LPUB     DS    CL13         39                                                  
         DS    CL1          40                                                  
LPUBN    DS    CL20         60                                                  
LZONE    DS    CL20         80                                                  
         ORG   LISTAR        4                                                  
         DS    CL2           6                                                  
LCLT     DS    CL3          09                                                  
LCLTSEP  DS    CL1          10                                                  
LCLTWKR  DS    0CL17          27                                                
LCLTN    DS    CL17         27                                                  
         DS    CL2          29                                                  
LPRD     DS    CL3          32                                                  
LPRDSEP  DS    CL1          33                                                  
LPRDWKR  DS    0CL14          47                                                
LPRDN    DS    CL14         47                                                  
         DS    CL1          48                                                  
LEST     DS    CL3          51                                                  
         DS    CL1          52                                                  
LINV     DS    CL10         62                                                  
         DS    CL1          63                                                  
LINS     DS    CL3          66                                                  
         DS    CL1          67                                                  
LCDTE    DS    CL5          72                                                  
         DS    CL1          73                                                  
LDEL     DS    CL1          74     DELETE PREVIOUS INVOICE                      
LCVQ     DS    CL1          75     CONVERTED                                    
LRCVQ    DS    CL1          76     RECONVERT REQUEST                            
LOVR     DS    CL1          77     CLT/PRD/EST OVERRIDE                         
LRECSEQ  DS    CL3          80     DDS - DISPLAY REQ SEQ                        
         TITLE 'T43018 - INVOICE LIST - REPORT LINE'                            
***********************************************************************         
*                                                                     *         
*        LAYOUT OF REPORT LINE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PADVNM   DS    CL25                                                             
         DS    CL1                                                              
PADVCD   DS    CL3                                                              
         DS    CL3                                                              
PPRDNM   DS    CL25                                                             
         DS    CL2                                                              
PESTO    DS    CL3                                                              
         DS    CL2                                                              
PPRDCD   DS    CL7                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PINV     DS    CL10                                                             
         DS    CL1                                                              
PMON     DS    CL5                                                              
         DS    CL2                                                              
PSPTS    DS    CL5                                                              
         DS    CL1                                                              
PDOLS    DS    CL13                                                             
         DS    CL1                                                              
PSTAT    DS    CL13          C-MONDA/YR OR                                      
PCVQ     DS    CL1                                                              
PRCVQ    DS    CL1                                                              
POVR     DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102PPEZF18   05/27/97'                                      
         END                                                                    
