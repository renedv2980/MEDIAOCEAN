*          DATA SET PRSFM10    AT LEVEL 037 AS OF 07/17/02                      
*PHASE T41C10A,*                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE NUMED                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'T41C10 CIRCULATION RECORDS'                                     
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 07/02   IN VK IF REPORTING AND LIMIT ACCESS ACTIVE, REQUIRE              
*              CLIENT CODE ENTRY                                                
*                                                                               
* KWAN 06/00   NO OP INSTRUCTION WITH GLV1GOTO EQUATE                           
*                                                                               
         EJECT                                                                  
T41C10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C10,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
         OI    GENSTAT4,NODELLST   STOP DELETING FROM LIST SCREEN               
         MVI   NLISTS,(CRLSELLH-CRLSEL1H)/(CRLSEL2H-CRLSEL1H)+1                 
*                                  # OF LIST LINES                              
*                                                                               
*        CHECK TO SEE IF IN MIDDLE OF HELP CALL                                 
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D2'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN FIRST TWA PAGE                       
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEM AS PRINT                          
         MVI   HCBSEGS,10          PRVAL TAB AREA IS 10*256 LONG                
         L     RF,=A(ELTAB-(CONHEADH-64))                                       
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBATAB          SET A(PRVAL TABLE)                           
*                                                                               
         GOTO1 VPRHELP,DMCB,0,0,HELPCBLK GO CHECK IF IN MIDDLE OF MENU          
*                                                                               
         TM    HCBRTRN,HCBRSELQ    IF SELECTION WAS MADE                        
         BNO   INIT10                                                           
*                                                                               
         GOTO1 ERREX2                 EXIT FOR USER TO COMPLETE ENTRY           
*                                                                               
         DROP  R6                                                               
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
*        INIT PRVAL BLOCK                                                       
*                                                                               
         XC    VLBLOCK(VLBLOCKL),VLBLOCK CLEAR PRVAL CONTROL BLOCK              
*                                                                               
         MVC   VLACFACS,ACOMFACS   A(COMFACS)                                   
         MVC   VLCTRY,CTRY         SET COUNTRY CODE                             
         MVC   VLLANG,LANG         LANGUAGE CODE                                
         MVC   VLAGENCY,TWAAGY     AGENCY                                       
         MVI   VLSYSTEM,VLSYSPRQ   PRINT SYSTEM                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,VLTODAYC) GET TODAY'S DATE                  
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   *+8                                                              
         MVI   CRCSCRLH+5,0           FORCE NO DATA IF SCROLL FIELD             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    RDEL                                                             
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    RRES                                                             
*                                                                               
         CLI   MODE,XRECREST       DISPLAY RESTORED RECORD                      
         BNE   *+12                                                             
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY                             
         B     DR                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
EXIT     DS    0H                                                               
*                                                                               
*        IF PFKEY HIT THEN CURSOR REMAINS WHERE IT WAS                          
*                                                                               
         CLI   PFAID,0             EXIT IF PFKEY NOT HIT                        
         BE    EXITX                                                            
*                                                                               
         CLI   PFAID,12            SKIP IF PFKEY 12  HIT                        
         BE    EXITX                                                            
*                                                                               
         CLI   PFAID,24            SKIP IF PFKEY 24  HIT                        
         BE    EXITX                                                            
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
         OI    CRCSCRLH+6,X'81'    TRANSMIT SCROLL AND MODIFY NEXT TIME         
*                                                                               
         OC    ACURFORC,ACURFORC   IF NO CURSOR ADDRESS SET                     
         BNZ   *+12                                                             
         LA    R2,CONACTH             PLACE IT AT ACTION FIELD                  
         ST    R2,ACURFORC                                                      
*                                                                               
EXITX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - VALKEY'                            
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VK       DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTREP       DIFFERENT CRITERIA FOR REPORT                
         BE    VKL                                                              
         CLI   ACTNUM,ACTLIST      DIFFERENT CRITERIA FOR LIST                  
         BE    VKL                                                              
*                                                                               
*     VALIDATE MEDIA - REQUIRED                                                 
*                                                                               
         LA    R2,CRCMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
*     VALIDATE PUB   - REQUIRED                                                 
*                                                                               
         LA    R2,CRCPUBH                                                       
*                                                                               
         BAS   RE,MYPUBVAL                                                      
*                                                                               
         MVC   CRCPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    CRCPUBNH+6,X'80'                                                 
         MVC   CRCPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    CRCPUBZH+6,X'80'                                                 
*                                                                               
*     VALIDATE SOURCE- REQUIRED                                                 
*                                                                               
         LA    R2,CRCSRCH          POINT TO ENTERED SOURCE                      
*                                                                               
         CLI   5(R2),0             ERROR IF NOT ENTERED                         
         BE    MISSERR                                                          
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   08(R2),C'+'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   08(R2),C'?'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   09(R2),C'?'         IF HELP REQUESTED                            
         BNE   VKSCHLPX                                                         
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
*                                                                               
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D1'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN SECOND TWA PAGE                      
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
*                                                                               
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVI   HCBSEGS,10          PRVAL TAB AREA IS 10*256 LONG                
         L     RF,=A(ELTAB-(CONHEADH-64))                                       
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBATAB          SET A(PRVAL TABLE)                           
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMCSR),(R2),HELPCBLK PUT OUT HELP           
*                                                                               
         DROP  R6                                                               
*                                                                               
VKSCHLPX DS    0H                                                               
*                                                                               
*        VALIDATE FIELD ENTRY                                                   
*                                                                               
         LH    R3,=Y(ELTAB-(CONHEAD-64))   GET A(TABLE BUILD AREA)              
         LA    R3,0(R3,RA)                                                      
*                                                                               
         GOTO1 VPRVAL,VLPARMS,('VLPVALQ',=Y(PRQCRSRC)),                X        
               (5(R2),8(R2)),(R3),0,0,0                                         
         CLI   VLPERR,0                                                         
         BNE   INVERR              INVALID SOURCE                               
*                                                                               
         USING VLTABD,R3           ESTABLISH RETURNED TABLE ENTRY               
*                                                                               
         MVC   WRKSRC,VLTFULL      SAVE SOURCE                                  
*                                                                               
VKSRCX   DS    0H                                                               
*                                                                               
*        VALIDATE PERIOD - MUST BE A YEAR - REQUIRED                            
*                                                                               
VKPER    DS    0H                                                               
*                                                                               
         LA    R2,CRCPERH          POINT TO PERIOD INPUT                        
*                                                                               
         CLI   5(R2),0             INPUT REQUIRED                               
         BE    MISSERR                                                          
*                                                                               
         MVC   WRKYR,=4C'0'        VALIDATE NUMERIC                             
         SR    RF,RF                                                            
         IC    RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         NC    WRKYR(0),CRCPER     RESULT MUST BE ALL '0'                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WRKYR(0),=4C'0'                                                  
         BNE   INVERR                                                           
*                                                                               
         CLI   5(R2),2             IF ONLY 2 BYTES                              
         BNE   VKPER10                                                          
*                                                                               
         MVC   WRKYR+2(2),CRCPER      ASSUME NO CENTURY ENTERED                 
*                                                                               
         CLC   WRKYR+2(2),=C'70'      IF AFTER 70                               
         BNH   *+14                                                             
         MVC   WRKYR(2),=C'19'            ASSUME 20TH CENTURY                   
         B     *+10                                                             
         MVC   WRKYR(2),=C'20'        ELSE ASSUME 21ST CENTRY                   
*                                                                               
         B     VKPER20                                                          
*                                                                               
VKPER10  DS    0H                                                               
*                                                                               
         CLI   5(R2),4             ELSE MUST BE 4 DIGITS                        
         BNE   INVERR                                                           
*                                                                               
         MVC   WRKYR,CRCPER        SAVE YEAR                                    
*                                                                               
VKPER20  DS    0H                                                               
*                                                                               
         XC    CRCPER,CRCPER                                                    
         MVC   CRCPER(4),WRKYR     RE-DISPLAY YEAR                              
         OI    CRCPERH+6,X'80'                                                  
*                                                                               
VKPERX   DS    0H                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKCLT    DS    0H                                                               
*                                                                               
         LA    R2,CRCCLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
         XC    QCLT,QCLT           SET TO DEFAULT                               
*                                                                               
         CLI   5(R2),0             NOT ENTERED IS THE SAME AS 'ALL'             
         BE    *+10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VKCLT1                                                           
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
VKCLT1   MVC   CRCCLTN,CLTNM       DISPLAY CLT NAME                             
         OI    CRCCLTNH+6,X'80'                                                 
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
*        BUILD CIRCULATION KEY                                                  
*                                                                               
VKCRC    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R5,KEY                                                           
         USING PCRCRECD,R5                                                      
*                                                                               
         MVC   PCRCKAGY,AGENCY                                                  
         MVC   PCRCKMED,QMED                                                    
         MVI   PCRCKTYP,PCRCKIDQ                                                
         MVC   PCRCKPUB,BPUB                                                    
         MVC   PCRCKSRC,WRKSRC                                                  
         MVC   PCRCKYR,WRKYR                                                    
         MVC   PCRCKCLT,QCLT                                                    
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
*        IF CLIENT RECORD, NEED TO READ ALL CLIENT RECORD INTO IOA3             
*                                                                               
         OC    PCRCKCLT,PCRCKCLT   SKIP IF NO CLIENT ENTERED                    
         BZ    VKRECX                                                           
*                                                                               
         MVC   AIO,AIO3            USE IOA3 FOR INPUT                           
*                                                                               
         XC    PCRCKCLT,PCRCKCLT   KILL CLIENT CODE                             
*                                                                               
         GOTO1 HIGH                READ IN ALL CLIENT RECORD                    
*                                                                               
         CLC   PCRCKEY,KEYSAVE     MUST FIND RECORD                             
         BNE   VKRECERR                                                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE CLIENT RECORD KEY                    
*                                                                               
VKRECX   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            USE IOA1 FOR INPUT                           
*                                                                               
*        READ IN DETAIL RECORD IF NEEDED                                        
*                                                                               
         L     RF,AIO1                                                          
*                                                                               
         CLC   PCRCKEY,0(RF)       AND CIRC RECORD NOT IN CORE                  
         BE    VKCRCX                                                           
*                                                                               
         GOTO1 HIGH                READ FOR RECORD                              
*                                                                               
         CLC   PCRCKEY,KEYSAVE     MUST FIND IT                                 
         BE    *+14                                                             
         MVC   KEY,KEYSAVE         RESTORE FILE POINTER                         
         B     VKCRCX                                                           
*                                                                               
         GOTO1 GETREC              READ IN CIRC RECORD                          
*                                                                               
VKCRCX   DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDEL       IF DELETING                                  
         BE    VKDEL                                                            
*                                                                               
         CLI   ACTNUM,ACTSEL       OR SELECTING WITH A 'D'                      
         BNE   VKDELX                                                           
         CLI   THISLSEL,C'D'                                                    
         BNE   VKDELX                                                           
*                                                                               
VKDEL    DS    0H                                                               
         OC    QCLT,QCLT           SKIP IF DELETING CLIENT RECORD               
         BNZ   VKDELX                                                           
*                                                                               
         GOTO1 SEQ                 READ FOR ANY CLIENT RECORDS                  
*                                                                               
         CLC   PCRCKEY(PCRCKCLT-PCRCKEY),KEYSAVE                                
         BNE   VKDELX              DONE ON CHANGE IN MAIN KEY                   
         B     VKDELERR            ALL CLIENT RECS MMUST BE DELETED 1ST         
*                                                                               
VKDELX   DS    0H                                                               
*                                                                               
         CLC   TWAKEYSV(L'PCRCKEY),ORIGKEY  CHECK FOR NEWKEY                    
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
*                                                                               
*        IF A NEW ADD MUST RE-DISPLAY SCREEN                                    
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
*****    BE    *+8                                                              
*****    CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   VKADDX                                                           
*                                                                               
         CLC   SAVKEY,ORIGKEY      IF KEY HAS CHANGED                           
         BE    VKADDX                                                           
*                                                                               
         MVC   SAVKEY,ORIGKEY         SAVE NEW KEY                              
         XC    SAVFRQ,SAVFRQ          FORCE NEW FREQUENCY                       
         XC    SAVFISS,SAVFISS        FORCE NEW FIRST ISSUE                     
*                                                                               
         GOTO1 =A(VRFRQ),RR=RELO      VALIDATE FREQUENCY                        
         BAS   RE,VRFISS              VALIDATE FIRST ISSUE                      
*                                                                               
         BAS   RE,DR1                 DISPLAY NEW SCREEN                        
*                                                                               
         OI    GENSTAT2,USMYOK+USGETTXT  SET OUR OWN OKAY MESSAGE               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1          ESTABLISH GETTXT CONTROL BLOCK               
*                                                                               
         MVI   GTMSGNO1,2             USER TO ENTER DETAILS                     
         MVI   GTMSYS,X'FF'           GENCON MESSAGE                            
         MVI   GTMTYP,GTMINF          INDICATE INFORMATIONAL MSG                
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   IOOPT,C'Y'             KILLS ADDING OF RECORD                    
*                                                                               
         LA    R2,CRCISS1H            CURSOR TO FIRST OPEN FIELD                
*                                                                               
         TM    1(R2),X'20'                                                      
         BNO   *+8                                                              
         BAS   RE,BUMPU               BUMP TO NEXT UNPROTECTED FIELD            
*                                                                               
         ST    R2,ACURFORC            FORCE CURSOR TO HERE                      
*                                                                               
         B     VKX                    AND GET OUT                               
*                                                                               
VKADDX   DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RE-SET KEY                                   
         MVC   SAVKEY,ORIGKEY      SAVE MASTER KEY                              
*                                                                               
         CLI   ACTNUM,ACTREST      IF ACTION RETORE                             
         BNE   *+8                                                              
         OI    DMINBTS,X'08'          READ DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ACTION IS ADD                        
         BE    VKCLRX                                                           
*                                                                               
         CLC   KEY(L'PCRCKEY),KEYSAVE SKIP IF RECORD FOUND                      
         BE    VKCLRX                                                           
*                                                                               
         TWAXC CRCFRQH,CRCSISSH          CLEAR FREQ, STARTING ISSUE             
         TWAXC CRCISS1H,CRCFINLH,PROT=Y  CLEAR SCREEN                           
*                                                                               
VKCLRX   DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY            RESTORE FILE POINTER                      
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VKDELERR DS    0H                  CAN'T DELETE MASTER REC BEFORE CLT'S         
*                                                                               
         MVI   ERROR,PWECLTFD                                                   
         LA    R2,CRCCLTH          CURSOR AT CLIENT                             
         B     VKERR                                                            
*                                                                               
VKRECERR DS    0H                  'ALL' CLIENT RECORD NOT FOUND                
*                                                                               
         MVI   ERROR,PWECRANF      ALL CIRC RECORD NOT FOUND                    
         LA    R2,CRCCLTH          CURSOR AT CLIENT                             
         B     VKERR                                                            
*                                                                               
VKERR    DS    0H                  VALKEY ERROR EXIT                            
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - VALLISTKEY'                        
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY                                            *         
*                                                                     *         
*        MEDIA IS ONLY REQUIRED KEY FIELD                             *         
*        FILTERING ALLOWED ON ALL OTHERS                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      DS    0H                                                               
*                                                                               
VKLMED   DS    0H                                                               
*                                                                               
         LA    R2,CRCMEDH          MEDIA - REQUIRED                             
         GOTO1 VALIMED                                                          
*                                                                               
VKLMEDX  DS    0H                                                               
*                                                                               
VKLPUB   DS    0H                                                               
*                                                                               
         LA    R2,CRCPUBH          PUB                                          
*                                                                               
         XC    BPUB,BPUB                                                        
         XC    CRCPUBN,CRCPUBN                                                  
         XC    CRCPUBZ,CRCPUBZ                                                  
         OI    CRCPUBNH+6,X'80'                                                 
         OI    CRCPUBZH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKLPUBX                                                          
*                                                                               
         BAS   RE,MYPUBVAL                                                      
*                                                                               
         MVC   CRCPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    CRCPUBNH+6,X'80'                                                 
         MVC   CRCPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    CRCPUBZH+6,X'80'                                                 
*                                                                               
VKLPUBX  DS    0H                                                               
*                                                                               
VKLSRC   DS    0H                                                               
*                                                                               
         LA    R2,CRCSRCH          POINT TO ENTERED SOURCE                      
         XC    WRKSRC,WRKSRC       INIT SOURCE FIELD                            
*                                                                               
         CLI   5(R2),0             OKAY IF NOT ENTERED                          
         BE    VKLSRCX                                                          
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   08(R2),C'+'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   08(R2),C'?'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   09(R2),C'?'         IF HELP REQUESTED                            
         BNE   VKSRHLPX                                                         
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
*                                                                               
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D2'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN SECOND TWA PAGE                      
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
*                                                                               
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVI   HCBSEGS,10          PRVAL TAB AREA IS 10*256 LONG                
         L     RF,=A(ELTAB-(CONHEADH-64))                                       
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBATAB          SET A(PRVAL TABLE)                           
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMCSR),(R2),HELPCBLK PUT OUT HELP           
*                                                                               
         DROP  R6                                                               
*                                                                               
VKSRHLPX DS    0H                                                               
*                                                                               
         ZIC   RF,0(R4)            GET LENGTH                                   
         MVC   WRKSRC,SPACES                                                    
         BCTR  RF,0                DECREMEMNT FOR EXECUTE                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKSRC,8(R2)        SAVE SOURCE                                  
*                                                                               
VKLSRCX  DS    0H                                                               
*                                                                               
*        VALIDATE PERIOD - MUST BE A YEAR - 2 DIGITS ALLOWED                    
*                                                                               
VKLPER   DS    0H                                                               
*                                                                               
         LA    R2,CRCPERH          POINT TO PERIOD INPUT                        
*                                                                               
         XC    WRKYR,WRKYR         INIT YEAR SAVEAREA                           
*                                                                               
         CLI   5(R2),0             NO INPUT OKAY                                
         BE    VKLPERX                                                          
*                                                                               
         MVC   WRKYR,=4C'0'        VALIDATE NUMERIC                             
         SR    RF,RF                                                            
         IC    RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         NC    WRKYR(0),CRCPER     RESULT MUST BE ALL '0'                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WRKYR(0),=4C'0'                                                  
         BNE   INVERR                                                           
*                                                                               
         CLI   5(R2),2             IF ONLY 2 BYTES                              
         BNE   VKLPER10                                                         
*                                                                               
         MVC   WRKYR+2(2),CRCPER      ASSUME NO CENTURY ENTERED                 
*                                                                               
         CLC   WRKYR+2(2),=C'70'      IF AFTER 70                               
         BNH   *+14                                                             
         MVC   WRKYR(2),=C'19'            ASSUME 20TH CENTURY                   
         B     *+10                                                             
         MVC   WRKYR(2),=C'20'        ELSE ASSUME 21ST CENTRY                   
*                                                                               
         B     VKLPER20                                                         
*                                                                               
VKLPER10 DS    0H                                                               
*                                                                               
         CLI   5(R2),4             ELSE MUST BE 4 DIGITS                        
         BNE   INVERR                                                           
*                                                                               
         MVC   WRKYR,CRCPER        SAVE YEAR                                    
*                                                                               
VKLPER20 DS    0H                                                               
*                                                                               
         XC    CRCPER,CRCPER                                                    
         MVC   CRCPER(4),WRKYR     RE-DISPLAY YEAR                              
         OI    CRCPERH+6,X'80'                                                  
*                                                                               
VKLPERX  DS    0H                                                               
*                                                                               
VKLCLT   DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           FOR LISTING CLEAR QCLT                       
         XC    CRCCLTN,CRCCLTN                                                  
         OI    CRCCLTNH+6,X'80'                                                 
*                                                                               
         LA    R2,CRCCLTH          CLIENT                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKLCLTD             CHECK FOR LIMIT ACCESS                       
*                                                                               
         CLC   =C'ALL',8(R2)                                                    
         BE    VKLCLTD             CHECK FOR LIMIT ACCESS                       
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   CRCCLTN,CLTNM                                                    
*                                                                               
         B     VKLCLTX                                                          
*                                                                               
VKLCLTD  DS    0H                  CLIENT IS ALL (OR NOT ENTERED)               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS ?                           
         BZ    VKLCLTX             NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV ?                           
         BE    VKLCLTX             NO                                           
         MVI   ERROR,NEEDCLT       SECURITY - CLIENT REQUIRED                   
         B     VKERR                                                            
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
VKLX     DS    0H                                                               
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
NEEDCLT  EQU   85             SPECIFIC CLIENT ENTRY REQUIRED (SECURITY)         
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - MYPUBVAL'                          
*                                                                               
*        VALIDATE PUB FIELD                                                     
*                                                                               
MYPUBVAL NTR1  LABEL=*                                                          
         LR    R4,R2                                                            
         XC    BPUB(6),BPUB                                                     
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VKPUB5                                                           
         CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VKPX                                                             
         B     ERRX                IF NOT THE MUST HAVE PUB                     
*                                                                               
VKPUB5   DS    0H                                                               
*                                                                               
         CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPX                                                             
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
*                                                                               
         LR    R2,R4                                                            
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    ERRX                                                             
*                                                                               
         LA    R1,SCANBLK                                                       
         LR    R2,R4                                                            
*                                                                               
         GOTO1 VALIPUB                                                          
*                                                                               
VKPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - DISKEY'                            
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DK       L     R6,AIO                                                           
         USING PCRCRECD,R6                                                      
*                                                                               
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY FROM START OF PUB           
*                                                                               
DKMED    DS    0H                                                               
*                                                                               
         MVC   CRCMED(L'PCRCKMED),PCRCKMED     MEDIA                            
         OI    CRCMEDH+4,X'80'     INDICATE INPUT THIS TIME                     
         OI    CRCMEDH+6,X'80'     TRANSMIT                                     
         MVI   CRCMEDH+5,L'PCRCKMED    SET FIELD LENGTH                         
*                                                                               
DKMEDX   DS    0H                                                               
*                                                                               
DKPUB    DS    0H                                                               
*                                  EXPAND PUB NUMBER                            
         GOTO1 =V(PUBEDIT),DMCB,(C'0',PCRCKPUB),(0,CRCPUB),RR=RELO              
*                                  EXPAND PUB NUMBER                            
         OI    CRCPUBH+4,X'80'     INDICATE INPUT THIS TIME                     
         OI    CRCPUBH+6,X'80'                                                  
         MVI   CRCPUBH+5,L'CRCPUB      SET FIELD LENGTH                         
*                                                                               
         LA    R2,CRCPUBH          POINT TO PUB FIELD                           
         GOTO1 VGETFLD             GET LENGTH MINUS TRAILING SPACES             
*                                                                               
         MVC   CRCPUBH+5(1),FLDH+5     SET TRUE INPUT LENGTH                    
*                                                                               
         MVC   MYPUB,PCRCKPUB      GET PUB NAME                                 
         GOTO1 =A(MYVPUB),RR=RELO                                               
*                                                                               
         MVC   CRCPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    CRCPUBNH+6,X'80'                                                 
         MVC   CRCPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    CRCPUBZH+6,X'80'                                                 
*                                                                               
DKPUBX   DS    0H                                                               
*                                                                               
DKPER    DS    0H                                                               
*                                                                               
         MVC   CRCPER(L'PCRCKYR),PCRCKYR      PERIOD                            
         OI    CRCPERH+4,X'80'     INDICATE INPUT THIS TIME                     
         OI    CRCPERH+6,X'80'     TRANSMIT                                     
*                                                                               
         OC    PCRCKYR,PCRCKYR     IF YEAR GIVEN                                
         BZ    *+8                                                              
         MVI   CRCPERH+5,L'PCRCKYR     SET FIELD LENGTH                         
*                                                                               
DKPERX   DS    0H                                                               
*                                                                               
DKSRC    DS    0H                                                               
*                                                                               
         MVC   CRCSRC(L'PCRCKSRC),PCRCKSRC     SOURCE                           
         OI    CRCSRCH+4,X'80'     INDICATE INPUT THIS TIME                     
         OI    CRCSRCH+6,X'80'     TRANSMIT                                     
*                                                                               
         OC    PCRCKSRC,PCRCKSRC   IF SOURCE GIVEN                              
         BZ    *+8                                                              
         MVI   CRCSRCH+5,L'PCRCKSRC   SET FIELD LENGTH                          
*                                                                               
DKSRCX   DS    0H                                                               
*                                                                               
DKCLT    DS    0H                                                               
*                                                                               
         MVC   CRCCLT(L'PCRCKCLT),PCRCKCLT     CLIENT                           
*                                                                               
         MVC   CLTNM,SPACES        INIT CLIENT NAME                             
*                                                                               
         MVI   CRCCLTH+5,0         DEFAULT INPUT LENGTH                         
*                                                                               
         OC    PCRCKCLT,PCRCKCLT   IF CLIENT GIVEN                              
         BZ    DKCLT10                                                          
*                                                                               
         MVI   CRCCLTH+5,L'PCRCKCLT   SET FIELD LENGTH                          
         BAS   RE,MYVCLT              READ THE CLIENT                           
*                                                                               
DKCLT10  OI    CRCCLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         OI    CRCCLTH+4,X'80'     INDICATE INPUT THIS TIME                     
         MVC   CRCCLTN,CLTNM       DISPLAY CLT NAME                             
         OI    CRCCLTNH+6,X'80'                                                 
*                                                                               
DKCLTX   DS    0H                                                               
*                                                                               
         TWAXC CRCFRQH,CRCSISSH  CLEAR FREQUENCY AND START ISSUE                
         MVI   CRCFRQH+5,0         SET LENGTHS TO ZERO                          
         MVI   CRCSISSH+5,0                                                     
*                                                                               
         B     VK                  VALIDATE THE KEY                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - VALREC'                            
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       DS    0H                                                               
*                                                                               
         CLI   IOOPT,C'Y'          EXIT IF TRYING TO DISPLAY SKELETON           
         BE    VRX                    SCREEN                                    
*                                                                               
         GOTO1 =A(VRFRQ),RR=RELO      VALIDATE FREQUENCY                        
         BAS   RE,VRFISS           VALIDATE FIRST ISSUE                         
*                                                                               
         CLI   NEWKEY,C'Y'         IF KEY CHANGED                               
         BNE   VRKYCHAX                                                         
*                                                                               
         BAS   RE,DR1                 DISPLAY NEW SCREEN                        
*                                                                               
         OI    GENSTAT2,USMYOK+USGETTXT  SET OUR OWN OKAY MESSAGE               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1             ESTABLISH GETTXT CONTROL BLOCK            
*                                                                               
         MVI   GTMSGNO1,2             USER TO ENTER DETAILS                     
         MVI   GTMSYS,X'FF'           GENCON MESSAGE                            
         MVI   GTMTYP,GTMINF          INDICATE INFORMATIONAL MSG                
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   IOOPT,C'Y'             KILLS ADDING OF RECORD                    
*                                                                               
         LA    R2,CRCISS1H            CURSOR TO FIRST OPEN FIELD                
*                                                                               
         TM    1(R2),X'20'                                                      
         BNO   *+8                                                              
         BAS   RE,BUMPU               BUMP TO NEXT UNPROTECTED FIELD            
*                                                                               
         ST    R2,ACURFORC            FORCE CURSOR TO HERE                      
*                                                                               
         B     VRX                    AND GET OUT                               
*                                                                               
VRKYCHAX DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE DETAIL INPUT VIA LINUP                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE COPY OF LINUP SAVE TABLE                
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         GOTO1 =A(LINSET),RR=RELO  LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         MVC   KEY,ORIGKEY         SET KEY                                      
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE BASE KEY FOR GENCON                  
*                                                                               
         LA    R2,CRCISS1H         POINT TO FIRST DETAIL FIELD                  
         ST    R2,AFRSTREC                                                      
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - DISREC'                            
***********************************************************************         
*                                                                     *         
*        DISPLAY RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DR1      NTR1                      SPECIAL INTERAL ENTRY                        
*                                                                               
         MVI   MODE,DISPREC           CHANGE MODE TO DISPLAY                    
*                                                                               
         B     DR2                                                              
*                                                                               
DR       DS    0H                                                               
*                                                                               
         TWAXC CRCFRQH,CRCSISSH    CLEAR FREQ                                   
         MVI   CRCFRQH+5,0         SET LENGTH TO 0                              
         MVI   CRCSISSH+5,0        SET LENGTH TO 0                              
*                                                                               
         GOTO1 =A(VRFRQ),RR=RELO   VALIDATE FREQUENCY                           
         BAS   RE,VRFISS           VALIDATE FIRST ISSUE                         
*                                                                               
DR2      DS    0H                                                               
*                                                                               
         GOTO1 =A(SETSCRN),RR=RELO FIX FIELDS ON SCREEN                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*         DISPLAY DETAILS VIA LINUP                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GOTO1 =A(LINSET),RR=RELO    INTERFACE WITH LINUP                       
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - RDEL'                              
***********************************************************************         
*                                                                     *         
*        DELETE RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDEL     DS    0H                                                               
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   RDEL10                                                           
*                                                                               
         OI    GENSTAT2,USMYOK+USGETTXT  SET OUR OWN OKAY MESSAGE               
*                                                                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1             ESTABLISH GETTXT CONTROL BLOCK            
*                                                                               
         MVI   GTMSGNO1,61            SET MESSAGE NUMBER                        
*                                                                               
RDEL10   DS    0H                                                               
*                                                                               
RDELX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - RRES'                              
***********************************************************************         
*                                                                     *         
*        RESTORE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RRES     DS    0H                                                               
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   RRES10                                                           
*                                                                               
         OI    GENSTAT2,USMYOK+USGETTXT  SET OUR OWN OKAY MESSAGE               
*                                                                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1             ESTABLISH GETTXT CONTROL BLOCK            
*                                                                               
         MVI   GTMSGNO1,62            SET MESSAGE NUMBER                        
*                                                                               
RRES10   DS    0H                                                               
*                                                                               
RRESX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - LR'                                
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LR       DS    0H                                                               
*                                                                               
         GOTO1 =A(LISTREC),RR=RELO                                              
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
MYVCLT   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   CLTNM,SPACES                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
         MVC   PCLTKMED,QMED                                                    
         MVC   PCLTKAGY,AGENCY                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),CRCCLT                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PRTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VCNO                                                             
*                                                                               
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'PRTFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   CLTNM,PCLTNAME                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         MVC   KEY,MYKEY           RESTORE KEY                                  
         XIT1                                                                   
*                                                                               
VCNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         MVC   KEY,MYKEY           RESTORE KEY                                  
         LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LR                  USE LIST REC LOGIC                           
*                                                                               
PRX      XIT1                                                                   
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'T41C10 CIRCULATION RECORDS - LISTREC'                           
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    GLSTSTAT,RETEXTRA                                                
*                                                                               
         LA    R4,KEY              ESTABLISH CIRCULATION RECORD KEY             
         USING PCRCRECD,R4                                                      
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR010               KEY IS LAST RECORD READ                      
*                                  SO GO RE-READ                                
*                                                                               
*        FIRST TIME                                                             
*                                                                               
         MVC   PCRCKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PCRCKMED,QMED                    MEDIA CODE                      
         MVI   PCRCKTYP,PCRCKIDQ                TYPE                            
*                                                                               
         CLI   CRCPUBH+5,0                                                      
         BE    *+10                                                             
         MVC   PCRCKPUB,BPUB                    PUB                             
*                                                                               
         MVC   PCRCKSRC,WRKSRC                  SOURCE                          
         MVC   PCRCKYR,WRKYR                    YEAR                            
*                                                                               
         CLI   CRCCLTH+5,0                                                      
         BE    *+10                                                             
         MVC   PCRCKCLT,QCLT                    CLIENT                          
*                                                                               
LR010    GOTO1 HIGH                READ FIRST OF TYPE                           
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH CIRCULATION RECORD KEY             
         USING PCRCRECD,R4                                                      
*                                                                               
         CLC   KEY(4),KEYSAVE      DONE IF CHANGE IN REC ID                     
         BNE   LREND                                                            
*                                                                               
LR035    DS    0H                                                               
*                                                                               
         OC    BPUB,BPUB           IF PUB GIVEN                                 
         BZ    *+14                                                             
         CLC   PCRCKPUB,BPUB          THEN PUB MUST MATCH                       
         BNE   LREND                                                            
*                                                                               
         OC    WRKSRC,WRKSRC       IF SOURCE GIVEN                              
         BZ    *+14                                                             
         CLC   PCRCKSRC,WRKSRC        MUST MATCH KEY                            
         BNE   LRCONT                                                           
*                                                                               
         OC    WRKYR,WRKYR         IF YEAR GIVEN                                
         BZ    *+14                                                             
         CLC   PCRCKYR,WRKYR          MUST BE EXACT MATCH                       
         BNE   LRCONT                                                           
*                                                                               
         OC    QCLT,QCLT          IF CLIENT GIVEN                               
         BZ    *+14                                                             
         CLC   PCRCKCLT,QCLT         MUST MATCH KEY                             
         BNE   LRCONT                                                           
*                                                                               
         GOTO1 GETREC                GET CIRC RECORD                            
*                                                                               
         MVC   MYDSKADD,DMDSKADD     SAVE D/A FOR LIST                          
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         USING LISTD,R5            ESTABLISH LIST LINE                          
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BE    *+8                                                              
         LA    R5,LISTAR           ELSE USE LIST AREA                           
*                                                                               
         MVC   LISTAR,SPACES       INIT LIST LINE                               
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(C'0',PCRCKPUB),(0,LPUB),RR=RELO                
*                                                                               
         MVC   MYPUB,PCRCKPUB      DISPLAY PUB NUMBER                           
         GOTO1 =A(MYVPUB),RR=RELO                                               
*                                                                               
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
         MVC   LSRC,PCRCKSRC       DISPLAY SOURCE                               
         MVC   LYR,PCRCKYR         DISPLAY YEAR                                 
         MVC   LCLT,PCRCKCLT       DISPLAY CLIENT CODE                          
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LRPRTN                 PRINT CIRCULATION FIGURES                 
*                                                                               
         LA    R6,PCRCELEM         POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
LRPRTLP  DS    0H                                                               
*                                                                               
         USING PCRCELM,R6          FIND CIRCULATION ELEMENTS                    
*                                                                               
         CLI   PCRCELM,0           DONE IF END OF RECORD                        
         BE    LRPRTDN                                                          
*                                                                               
         CLI   PCRCELM,PCRCELQ     SKIP IF NOT CIRCULATION ELEMENT              
         BNE   LRPRTCN                                                          
*                                                                               
*              DISPLAY ISSUE DATE                                               
*                                                                               
         OC    PCRCISS,PCRCISS     SKIP IF NO ISSUE DATE                        
         BZ    LRISSX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCRCISS),(17,DUB)   CONVERT DATE                  
*                                                                               
         LA    RF,5                DEFAULT TO MMMDD                             
*                                                                               
         CLI   PCRCISS+2,0         IF THERE ARE NO DAYS                         
         BNE   *+8                                                              
         LA    RF,3                    SHOW MMM ONLY                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LISS(0),DUB            DISPLAY ISSUE                             
*                                                                               
LRISSX   DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
*        DISPLAY CIRCULATION                                                    
*                                                                               
         LA    R4,4                NUMBER OF CIRC FIGURES                       
         LA    R2,LTOT             POINT TO FIRST CIRC FIGURE OUTAREA           
         LA    R3,PCRCTOT          POINT TO FIRST CIRC FIGURE                   
*                                                                               
LRCRCLP DS     0H                                                               
*                                                                               
         OC    0(4,R3),0(R3)       SKIP IF NO CIRC AVAILABLE                    
         BZ    LRCRCCN                                                          
*                                                                               
         ST    R0,SAVER0                                                        
         EDIT  (4,(R3)),(9,0(R2)),0,COMMAS=YES  DISPLAY CIRC                    
         L     R0,SAVER0                                                        
*                                                                               
LRCRCCN DS     0H                                                               
*                                                                               
         LA    R2,13(R2)           BUMP TO NEXT CIRC OUTAREA                    
         LA    R3,4(R3)            BUMP TO NEXT CIRC FIGURE                     
         BCT   R4,LRCRCLP                                                       
*                                                                               
LRCRCDN  DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
*                                                                               
LRPRTCN  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PCRCLEN        ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     LRPRTLP                                                          
*                                                                               
LRPRTDN  DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
*                                                                               
         B     LRCONT                                                           
*                                                                               
LRPRTN   DS    0H                  NON-REPORT                                   
*                                                                               
         MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
*                                                                               
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
         B     LISTRECX                                                         
*                                                                               
LREND    DS    0H                  END OF LIST                                  
*                                                                               
         XC    KEY,KEY             CLEAR RECORD KEY                             
*                                                                               
LISTRECX XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* SET UP THE PUB NUMBER & GET NAME                                              
*                                                                               
MYVPUB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),MYPUB    MOVE PUB/ZONE/EDTN                           
         CLC   MYPUB+4(2),=X'FFFF'    ALL ZONES/EDTS                            
         BNE   *+10                                                             
         XC    PUBKPUB+4(2),PUBKPUB+4   READ "BASE" PUB                         
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
         MVC   PUBZNM(3),=C'ALL'                                                
         CLC   MYPUB+4(2),=X'FFFF'   ALL ZONES/EDTS                             
         BE    *+10                                                             
         MVC   PUBZNM,PUBZNAME                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         MVC   KEY,MYKEY           RESTORE KEY                                  
         XIT1                                                                   
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         MVC   KEY,MYKEY           RESTORE KEY                                  
         LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        VALIDATE PAGE START ISSUE                                              
*                                                                               
         LA    R2,CRCSISSH         POINT TO DATE FIELD                          
         XC    WRKSISS,WRKSISS     INIT PAGE STARTING ISSUE                     
*                                                                               
         TM    4(R2),X'80'         IF FIELD ENTERED THIS TIME                   
         BNO   *+8                                                              
         MVI   NEWKEY,C'Y'            FORCE REDISPLAY                           
*                                                                               
         CLI   5(R2),0             SKIP IF THERE IS NO ENTRY                    
         BE    LSSISSX                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALESTA+2(4),=C'0101'  DEFAULT IS JAN01                         
         MVC   PVALESTA(2),WRKYR+2     OF CIRC YEAR                             
*                                                                               
         GOTO1 DATCON,DMCB,PVALESTA,PVALESTA CONVERT TO Y2K FORMAT              
*                                                                               
         LA    RF,PVINTOD+PVINSGLO+PVINSGLS                                     
*                           DEFAULT DATE GIVEN & SINGLE DATES ONLY              
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),((RF),PVALOUTB) VAL PERIOD             
*                                                                               
         CLI   4(R1),4             SINGLE DATE ENTERED OKAY                     
         BE    *+8                                                              
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   LSSISSE1                                                         
*                                                                               
         MVC   WRKSISS,PVALBSTA    SAVE ISSUE DATE                              
*                                                                               
         TM    PVALASSM,PVALASD    IF START DAY ASSUMED                         
         BNO   *+8                                                              
         MVI   WRKSISSD,0             KILL DAY IN ISSUE DATE                    
*                                                                               
         B     LSSISSX                                                          
*                                                                               
LSSISSE1 DS    0H                                                               
*                                                                               
         MVI   ERROR,INVDATE                                                    
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
LSSISSX  DS    0H                                                               
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,5               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,CRCISS1H         A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUPAGEQ    SCROLL FACTOR OF A PAGE IS DEFAULT           
*                                                                               
         CLI   CRCSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   CRCSCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   CRCSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   CRCSCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    CRCSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CRCSCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CRCSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
         MVI   LUAPMODE,LUAPDSPQ   DEFAULT TO DISPLAY MODE                      
*                                                                               
         CLI   MODE,VALREC         CHANGE FOR VALREC MODE                       
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=Y(LSVTABL) SAVED BYTES PER LINE                         
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BAS   RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
         CLI   MODE,VALREC         IF VALIDATING RECORD                         
         BNE   LSPFKX                                                           
*                                     ANALYZE PFKEYS                            
         CLI   PFAID,0             IF PFKEY ENTERED                             
         BE    LSPFKX                                                           
*                                                                               
         BAS   RE,PFKEYS              GO ANALYZE                                
         BE    LSPFKX                 NO ERRORS                                 
*                                                                               
         GOTO1 ERREX                  CHECK FOR ERRORS                          
*                                                                               
LSPFKX   DS    0H                                                               
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
*****    B     *+6                 DUMPS SECOND TIME IN                         
*****    DC    H'0'                                                             
*****    MVI   *-5,0                                                            
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
         CLI   NEWKEY,C'Y'         IF KEY CHANGED                               
         BE    *+16                   DON'T WRITE CHANGES TO FILE               
         TM    LUWSTAT,LUSNPVQ     UPDATE RECORD IF THERE WAS AT LEAST          
         BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
         BAS   RE,LSWRTTAB         WRITES CHANGES TO RECORD                     
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
         MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,CRCMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,CRCMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(LSVTABL)      GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
*NLINS    EQU   6                                                               
NLINS    EQU   ((CRCISSLH-CRCISS1H)/(CRCISS2H-CRCISS1H))+1                      
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1  LABEL=*                                                          
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         BAS   RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+10                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BAS   RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         TITLE 'PRSFM10 - BUILD TABLE OF ELEMENTS ON FILE'                      
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1  LABEL=*                                                          
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
         CLC   KEY(L'PCRCKEY),KEYSAVE   SKIP IF NOT FOUND                       
         BNE   LSBRECX                                                          
*                                                                               
         L     RF,AIO1                                                          
         CLC   KEY(L'PCRCKEY),0(RF)  SKIP IF RECORD IN CORE                     
         BE    LSBRECX                                                          
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
LSBRECX  DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE ORIGINAL KEY                         
*                                                                               
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
*                                                                               
         LH    R1,=Y(ELTAB-(CONHEAD-64)) POINT TO ELEMENT TABLE                 
         AR    R1,RA                                                            
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
*        BUILD TABLE OF ELEMENTS                                                
*                                                                               
*        FIND CIRCULATION RECORD                                                
*                                                                               
         L     R6,AIO1             CIRC ASSUMED TO BE FOR ALL CLIENTS           
*                                                                               
         OC    QCLT,QCLT           IF DOING ONE CLIENT                          
         BZ    LSBCLTN                                                          
*                                                                               
         L     R6,AIO3                ALL CLIENTS RECORD IN IOA3                
*                                                                               
*        BUILD CIRCULATION KEY                                                  
*                                                                               
         XC    KEY,KEY             BUILD ALL CLIENT KEY                         
         LA    R5,KEY                                                           
         USING PCRCRECD,R5                                                      
*                                                                               
         MVC   KEY,ORIGKEY         COPY ORGINAL KEY                             
         XC    PCRCKCLT,PCRCKCLT   KILL CLIENT CODE                             
*                                                                               
         CLC   PCRCKEY,0(R6)       SKIP IF RECORD IN CORE                       
         BE    LSBCLTX                                                          
*                                                                               
         GOTO1 HIGH                READ IN ALL CLIENT RECORD                    
*                                                                               
         CLC   PCRCKEY,KEYSAVE     MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3            USE IOA3 FOR INPUT                           
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
LSBCLTX  DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE CLIENT RECORD KEY                    
*                                                                               
         GOTO1 HIGH                READ FOR RECORD                              
*                                                                               
         CLC   PCRCKEY,KEYSAVE     MUST FIND IT                                 
         BE    LSBCLTX1                                                         
*                                                                               
         CLI   ACTNUM,ACTADD       OKAY IF ADDING A RECORD                      
         BE    LSBCLTX2                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
LSBCLTX1 DS    0H                  RECORD FOUND                                 
*                                                                               
         CLI   ACTNUM,ACTADD       ERROR IF ADD                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         DROP  R5                                                               
*                                                                               
LSBCLTX2 DS    0H                                                               
*                                                                               
LSBCLTN  DS    0H                                                               
*                                                                               
         GOTO1 =A(BLDISS),RR=RELO  CREATE ISSUE RECORD                          
*                                                                               
         MVC   AIO,AIO1            REVERT TO ORIGINAL IOAREA                    
         MVC   KEY,ORIGKEY                                                      
*                                                                               
         L     R5,AIO2             ESTABLISH FOUND ISSUE RECORD                 
         USING PISSREC,R5                                                       
*                                                                               
         L     R3,AIO1             POINT TO CIRCULATION RECORD                  
         USING PCRCRECD,R3         ESTABLISH CIRCULATION RECORD                 
*                                                                               
         CLI   ACTNUM,ACTADD          IF ACTION IS ADD                          
         BNE   *+8                                                              
         L     R3,AIO3                   USE ALL CLIENT DATA AS DEFAULT         
*                                                                               
         LA    R3,PCRCELEM         POINT TO FIRST ELEMENT                       
         USING PCRCELM,R3          ESTABLISH AS CIRCULATION ELEMENT             
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         LA    R5,PISSELEM         POINT TO FIRST ELEMENT IN RECORD             
         USING PISSEL29,R5         ESTABLISH AS ISSUE ELEMENT                   
*                                                                               
         LA    R6,PCRCELEM-PCRCRECD(R6) POINT TO 1ST ELM IN ALL CLT REC         
*                                                                               
LSBTLOOP DS    0H                                                               
*                                                                               
         CLI   PISSEL29,0          DONE IF END OF ISSUE RECORD                  
         BE    LSBTDONE                                                         
*                                                                               
         CLI   PISSEL29,X'29'      SKIP IF NOT ISSUE ELEMENT                    
         BNE   LSBTCONT                                                         
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         MVI   PCRCELM-PCRCELM+ELTELEM,PCRCELQ   CIRC ELM ID                    
         MVI   PCRCLEN-PCRCELM+ELTELEM,PCRCELLN  CIRC ELM LENGTH                
*                                                                               
         MVC   PCRCISS-PCRCELM+ELTELEM,PISSDAT   SET ISSUE DATE                 
*                                                                               
*        MATCH CIRCULATION AND ISSUE DATES                                      
*                                                                               
LSBTISS  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
*                                                                               
LSBTISSL DS    0H                                                               
*                                                                               
         CLI   PCRCELM,0           DONE IF NO CIRC DATA LEFT                    
         BE    LSBTISSD                                                         
*                                                                               
         CLI   PCRCELM,PCRCELQ     SKIP IF NOT A CIRC ELEMENT                   
         BNE   LSBTISSC                                                         
*                                                                               
         CLC   PCRCISS,PISSDAT     DONE IF CIRC ISSUE AFTER ISSUE ONE           
         BH    LSBTISSD                                                         
*                                                                               
         BL    LSBTISSC            CONTINUE MATCHING IF BEFORE                  
*                                                                               
         MVC   ELTELEM,PCRCELM     COPY CIRC DATA                               
*                                                                               
         B     LSBTISSD                                                         
*                                                                               
LSBTISSC DS    0H                                                               
*                                                                               
         IC    RF,PCRCLEN          ELEMENT LENGTH                               
         LA    R3,PCRCELM(RF)      BUMP TO NEXT ELEMENT                         
         B     LSBTISSL                                                         
*                                                                               
LSBTISSD DS    0H                                                               
*                                                                               
*        FIND ALL CLIENTS DATA IF NECESSARY                                     
*                                                                               
LSBTALL  DS    0H                                                               
*                                                                               
         OC    QCLT,QCLT           SKIP IF ALL CLIENTS BEING DONE               
         BZ    LSBTALLD                                                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
LSBTALLL DS    0H                                                               
*                                                                               
         CLI   PCRCELM-PCRCELM(R6),0 DONE IF NO CIRC DATA LEFT                  
         BE    LSBTALLD                                                         
*                                                                               
         CLI   PCRCELM-PCRCELM(R6),PCRCELQ SKIP IF NOT A CIRC ELEMENT           
         BNE   LSBTALLC                                                         
*                                                                               
         CLC   PCRCISS-PCRCELM(L'PCRCISS,R6),PISSDAT DONE IF CIRC ISS           
         BH    LSBTALLD                                 AFTER                   
*                                                                               
         BL    LSBTALLC            CONTINUE MATCHING IF BEFORE                  
*                                                                               
*        FILL IN ALL CLIENT DATA WHERE NEEDED                                   
*                                                                               
         MVC   PCRCTOT-PCRCELM+ELTELEM,PCRCTOT-PCRCELM(R6)   TOTAL CIRC         
         MVC   PCRCPLIM-PCRCELM+ELTELEM,PCRCPLIM-PCRCELM(R6) PRELIM             
         MVC   PCRCFIN-PCRCELM+ELTELEM,PCRCFIN-PCRCELM(R6)   FINAL              
*                                                                               
         B     LSBTALLD                                                         
*                                                                               
LSBTALLC DS    0H                                                               
*                                                                               
         IC    RF,PCRCLEN-PCRCELM(R6)  ELEMENT LENGTH                           
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     LSBTALLL                                                         
*                                                                               
LSBTALLD DS    0H                                                               
*                                                                               
         MVC   ELTSORT,PCRCISS-PCRCELM+ELTELEM     SET KEY                      
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FITS INTO TABLE              
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTCONT DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PISSELEM+1       ISSUE ELEMENT LENGTH                         
         LA    R5,PISSELEM(RF)     POINT TO NEXT ISSUE ELEMENT                  
*                                                                               
         B     LSBTLOOP                                                         
*                                                                               
LSBTDONE DS    0H                                                               
*                                                                               
LSBTX    DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0               MINUS ONE                                    
         MH    R1,=Y(ELTABL)       TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE FILE POINTER                         
         GOTO1 HIGH                                                             
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE BASE KEY FOR GENCON                  
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORDS - LHSRCH'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO SEARCH TABLE FOR ELEMENT                          *         
*        AND SET ADDRESS IN ELTENT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1  LABEL=*                                                          
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BZ    LHSRCHX                RETURN EMPTY-HANDED                       
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         OC    WRKSISS,WRKSISS     IF PAGE STARTING ISSUE GIVEN                 
         BZ    LHSFISSX                                                         
*                                     FIND IN TABLE                             
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',WRKSISS),RR=RELO                  
*                                                                               
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BE    LHSSISE1                                                         
         OC    BSPAREC,BSPAREC                                                  
         BZ    LHSSISE1                                                         
*                                                                               
         ICM   R4,15,BSPAREC       RESET DEFAULT ISSUE                          
*                                                                               
         B     LHSFISSX                                                         
*                                                                               
LHSSISE1 DS    0H                                                               
         MVI   ERROR,MISSING       NOT STARTING ISSUE FOUND                     
         GOTO1 ERREX                                                            
*                                                                               
LHSFISSX DS    0H                                                               
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         OC    LSVKEY,LSVKEY       NO PREVIOUS ENTRY MEANS FIRST TIME           
         BZ    LHSRCH11            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLC   LSVKEY,HIVALS       IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
         BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',LSVKEY),RR=RELO                   
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
*                                                                               
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18               DONT GO FURTHER                           
         SH    R4,=Y(ELTABL)          BACK UP AN ENTRY                          
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ          TELL LINUP THIS IS LAST                   
LHSRCH30 DS    0H                                                               
         EJECT                                                                  
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORDS - LHVALLIN'                       
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE WINDOW LINE                              *         
*        BUILD ENTRY IN APWORK AND ADD TO ELEM TABLE                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1  LABEL=*                                                          
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         XC    WRKISS,WRKISS       CLEAR ISSUE DATE                             
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING PCRCELM,R3          ESTABLISH CIRCULATION ELEMENT                
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =ISSUE          
*                                                                               
         CLI   5(R2),0             SKIP IF NO ISSUE DATE ENTERED                
         BE    LHVISS10                                                         
*                                                                               
         L     RF,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,RF                                                       
         MVC   WRKISS,LSVSORT      COPY ISSUE DATE                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(VALISS),RR=RELO  VALIDATE ISSUE DATE                          
*                                                                               
LHVISS10 DS    0H                                                               
*                                                                               
         OC    WRKISS,WRKISS       IF NO INPUT IN ISSUE FIELD                   
         BNZ   LHVIS30                                                          
*                                                                               
         LR    R1,R2                  SAVE R2                                   
         SR    R0,R0                                                            
         ICM   R0,1,LUNFLDS           GET NUMBER OF FIELDS ON SCREEN            
         BCTR  R0,0                   DECREMENT FOR ISSUE FIELD                 
*                                                                               
         BAS   RE,BUMP                & SEE IF ANY INPUT ON THIS LINE           
         CLI   5(R2),0                                                          
         BE    *+10                   NO ENTRY ALLOWED                          
         LR    R2,R1                  RESET ISSUE FIELD POINTER                 
         B     MISSERR                NO ENTRY ALLOWED                          
         BCT   R0,*-18                                                          
*                                                                               
         LR    R2,R1                  RESET R2                                  
*                                                                               
         B     LHVISOK                OKAY IF NO INPUT ON LINE                  
*                                                                               
LHVIS30  DS    0H                                                               
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
         MVI   PCRCELM,PCRCELQ     ELEMENT CODE                                 
         MVI   PCRCLEN,PCRCELLN    ELEMENT LENGTH                               
         MVC   PCRCISS,WRKISS      SET ISSUE DATE                               
*                                                                               
         MVC   ELTSORT,PCRCISS     SET SORT KEY TO ISSUE DATE                   
*                                                                               
LHVISOK  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD IS VALID                      
*                                                                               
*        VALIDATE CIRCULATION FIELDS                                            
*                                                                               
LHVCRC   DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,LUNFLDS        NUMBER OF FIELDS ON LINE                     
         BCTR  R0,0                DECREMENT FOR ISSUE FIELD                    
         LA    R6,PCRCTOT          POINT TO FIRST CIRC FLD IN ELM               
*                                                                               
LHVCRCLP DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO FIRST/NEXT CIRC FIELD                
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVCRCOK               SKIP VALIDATION                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)             GET INPUT LENGTH                          
         BZ    LHVCRCOK               NO INPUT CHECK NEXT FIELD                 
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),(RF) VALIDATE AS INTEGER               
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BE    INVERR                                                           
*                                                                               
         L     RF,DMCB+4           GET INPUT VALUE                              
*                                                                               
         C     RF,=F'0'          CAN'T BE MINUS                                 
         BL    INVERR                                                           
*                                                                               
         STCM  RF,15,0(R6)       SAVE IN PCRCELM                                
*                                                                               
LHVCRCOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
LHVCRCCN DS    0H                                                               
*                                                                               
         LA    R6,L'PCRCTOT(R6)    BUMP TO NEXT CIRCULATION FIELD               
         BCT   R0,LHVCRCLP                                                      
*                                                                               
LHVCRCDN DS    0H                                                               
*                                                                               
*        ALL FIELDS ON LINE ARE VALID                                           
*                                                                               
*                                                                               
*        ADD NEW ELEMENT TO TABLE                                               
*                                                                               
LHVCMP   DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE CURRENT MESSAGE NUMBER                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO                    
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1       SET CURRENT ELEMENT ADDRESS                  
*                                                                               
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    LHVL92                                                           
*                                                                               
         OC    LSVKEY,LSVKEY       IF NO PREVIOUS ENTRY                         
         BNZ   *+14                                                             
         MVC   LSVKEY(LSVKEYL),ELTKEY   ADD TO SCREEN                           
         B     LHVL92                                                           
*                                                                               
         B     LHVCMPE2            ELSE WE HAVE DUPLICATE                       
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,PCRCELM     UPDATE ELEM IN TABLE                         
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,RECFULL             TOO MANY DETAIL LINES                  
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =ISSUE          
         MVI   ERROR,DUPEDATA      DUPLICATE                                    
         B     LHVCMPER                                                         
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R3                                                               
         DROP  R2                                                               
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORDS - LHDISLIN'                       
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1  LABEL=*                                                          
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDISSX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING PCRCELM,R3                                                       
*                                                                               
*              DISPLAY ISSUE DATE                                               
*                                                                               
         OC    PCRCISS,PCRCISS     SKIP IF NO ISSUE DATE                        
         BZ    LHDISSX                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCRCISS),(17,DUB)   CONVERT DATE                  
*                                                                               
         LA    RF,5                DEFAULT TO MMMDD                             
*                                                                               
         CLI   PCRCISS+2,0         IF THERE ARE NO DAYS                         
         BNE   *+8                                                              
         LA    RF,3                    SHOW MMM ONLY                            
*                                                                               
         STC   RF,5(R2)            SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DUB         DISPLAY ISSUE                                
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
LHDISSX  DS    0H                                                               
*                                                                               
*        DISPLAY CIRCULATION                                                    
*                                                                               
         LA    R6,PCRCTOT          POINT TO FIRST CIRC FIGURE                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         LA    R3,4                NUMBER OF CIRC FIGURES                       
*                                                                               
LHDCRCLP DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO FIRST/NEXT CIRCULATION FIELD         
*                                                                               
         MVC   FLD,SPACES          DEFAULT TO CLEARING                          
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDCRCOK                                                         
*                                                                               
         OC    0(4,R6),0(R6)       SKIP IF NO CIRC AVAILABLE                    
         BZ    LHDCRCOK                                                         
*                                                                               
         EDIT  (4,(R6)),(9,8(R2)),0,ALIGN=LEFT,COMMAS=YES  DISPLAY CIRC         
*                                                                               
         STC   R0,5(R2)            SET FIELD LENGTH                             
*                                                                               
LHDCRCOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
LHDCRCCN DS    0H                                                               
*                                                                               
         LA    R6,4(R6)            BUMP TO NEXT CIRC FIGURE                     
         BCT   R3,LHDCRCLP                                                      
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORDS - LHWRTTAB'                       
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1  LABEL=*                                                          
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
*        FIRST DELETE ALL CURRENT ELEMENTS                                      
*                                                                               
         MVI   ELCODE,PCRHELQ      SET TO REMOVE CIRC HEADER ELEMENTS           
*                                                                               
         GOTO1 REMELEM             CLEAR THEM OUT OF THE RECORD                 
*                                                                               
         MVI   ELCODE,PCRCELQ      SET TO REMOVE CIRC ELEMENTS                  
*                                                                               
         GOTO1 REMELEM             CLEAR THEM OUT OF THE RECORD                 
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELEMENT                                                       
         USING PCRHELM,R3          ESTABLISH CIRC HEADER ELEMENT                
*                                                                               
         MVI   PCRHELM,PCRHELQ     SET AS CIRC HEADER ELEMENT                   
         MVI   PCRHLEN,PCRHELML    SET ELEMENT LENGTH                           
*                                                                               
         MVC   PCRHFRQ,WRKFRQ      SET FREQUENCY                                
         MVC   PCRHSRC,WRKSRC      SET SOURCE                                   
         MVC   PCRHFISS,WRKFISS    SET STARTING ISSUE                           
*                                                                               
         GOTO1 ADDELEM             PUT ELEMENT IN RECORD                        
*                                                                               
         CLI   ERROR,0             ONLY ERROR CAN BE NO ROOM IN REC             
         BNE   NOMORE              NO ERROR TOLERATED                           
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING PCRCELM,R3          ESTABLISH CIRC ELEMENT                       
*                                                                               
         ICM   RF,1,PCRCLEN        GET ELEMENT LENGTH                           
         BZ    LSWTADDX            SKIP IF NO ENTRY                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),ELTELEM     MOVE ELEMENT TO WORKAREA                  
*                                                                               
         OC    QCLT,QCLT           IF CLIENT SPECIFIED                          
         BZ    LSWCLTX                                                          
*                                                                               
         LA    R3,ELEMENT             POINT TO ELM IN WORK                      
*                                                                               
         XC    PCRCTOT,PCRCTOT        ELIMINATE TOTAL CIRC                      
         XC    PCRCPLIM,PCRCPLIM      PRELIMINARY CIRC                          
         XC    PCRCFIN,PCRCFIN        FINAL CIRC                                
*                                                                               
LSWCLTX  DS    0H                                                               
*                                                                               
         GOTO1 ADDELEM             PUT ELEMENT IN RECORD                        
*                                                                               
         CLI   ERROR,0             ONLY ERROR CAN BE NO ROOM IN REC             
         BNE   NOMORE              NO ERROR TOLERATED                           
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTLPCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTLPCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTLOOP                                                         
*                                                                               
LSWTLPDN DS    0H                                                               
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         LTORG                                                                  
         TITLE 'PRSFM10 - CIRCULATION RECORDS - SETSCRN'                        
***********************************************************************         
*                                                                     *         
* ROUTINE TO PROTECT OR UNPROTECT FIELDS ON SCREEN                    *         
*        ISSUE FIELDS OPEN ONLY FOR FREQUENCY X AND CLIENT 'ALL'      *         
*        IF NO CLIENT, ALL CIRC FIELDS ARE UNPROTECTED                *         
*        ELSE ONLY RATEBASE FIELDS ARE UNPROTECTED                    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SETSCRN  NMOD1 0,**#SCR                                                         
*                                                                               
         LA    R2,CRCISS1H         POINT TO FIRST CIRCULATION FIELD             
         LA    R3,CRCFINLH         POINT TO LAST  CIRCULATION FIELD             
*                                                                               
SSCRLOOP DS    0H                                                               
*                                                                               
*        ISSUE DATE FIELD - CLOSED UNLESS NON-PERIODIC FREQUENCY                
*                                                                               
SSCRISS  DS    0H                                                               
*                                                                               
         CLI   WRKFRQ,C'X'         IF FREQUENCY 'X' AND                         
         BNE   SSCRISS1                                                         
         OC    QCLT,QCLT           IF NO CLIENT THEN FIELD IS OPEN              
         BNZ   SSCRISS1                                                         
*                                                                               
         TM    1(R2),X'20'            IF PROTECTED OPEN FIELD UP                
         BNO   *+16                                                             
         XI    1(R2),X'20'               TURN OFF PROTECT BIT                   
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
*                                                                               
         B     SSCRISSX                                                         
*                                                                               
SSCRISS1 DS    0H                  ELSE CLOSE FIELD                             
*                                                                               
         TM    1(R2),X'20'            IF UNPROTECTED CLOSE FIELD                
         BO    *+16                                                             
         OI    1(R2),X'20'               TURN ON PROTECT BIT                    
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
SSCRISSX DS    0H                                                               
*                                                                               
*        TOTAL CIRCULATION FIELD - CLOSED IF DOING CLIENT                       
*                                                                               
         BAS   RE,BUMP             POINT TO TOTAL CIRC FIELD                    
*                                                                               
SSCRTOT  DS    0H                                                               
*                                                                               
         OC    QCLT,QCLT           IF NO CLIENT THEN FIELD IS OPEN              
         BNZ   SSCRTOT1                                                         
*                                                                               
         TM    1(R2),X'20'            IF PROTECTED OPEN FIELD UP                
         BNO   *+16                                                             
         XI    1(R2),X'20'               TURN OFF PROTECT BIT                   
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
         B     SSCRTOTX                                                         
*                                                                               
SSCRTOT1 DS    0H                  ELSE CLOSE FIELD                             
*                                                                               
         TM    1(R2),X'20'            IF UNPROTECTED CLOSE FIELD                
         BO    *+16                                                             
         OI    1(R2),X'20'               TURN ON PROTECT BIT                    
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
SSCRTOTX DS    0H                                                               
*                                                                               
*        RATEBASE FIELD -ALWAYS OPEN                                            
*                                                                               
         BAS   RE,BUMP             POINT TO RATEBASE FIELD                      
*                                                                               
*        PRELIMINARY CIRCULATION FIELD - CLOSED IF DOING CLIENT                 
*                                                                               
         BAS   RE,BUMP             POINT TO PRELIMINARY CIRC FIELD              
*                                                                               
SSCRPRE  DS    0H                                                               
*                                                                               
         OC    QCLT,QCLT           IF NO CLIENT THEN FIELD IS OPEN              
         BNZ   SSCRPRE1                                                         
*                                                                               
         TM    1(R2),X'20'            IF PROTECTED OPEN FIELD UP                
         BNO   *+16                                                             
         XI    1(R2),X'20'               TURN OFF PROTECT BIT                   
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
         B     SSCRPREX                                                         
*                                                                               
SSCRPRE1 DS    0H                  ELSE CLOSE FIELD                             
*                                                                               
         TM    1(R2),X'20'            IF UNPROTECTED CLOSE FIELD                
         BO    *+16                                                             
         OI    1(R2),X'20'               TURN ON PROTECT BIT                    
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
SSCRPREX DS    0H                                                               
*                                                                               
*        FINAL CIRCULATION FIELD - CLOSED IF DOING CLIENT                       
*                                                                               
         BAS   RE,BUMP             POINT TO FINAL CIRC FIELD                    
*                                                                               
SSCRFIN  DS    0H                                                               
*                                                                               
         OC    QCLT,QCLT           IF NO CLIENT THEN FIELD IS OPEN              
         BNZ   SSCRFIN1                                                         
*                                                                               
         TM    1(R2),X'20'            IF PROTECTED OPEN FIELD UP                
         BNO   *+16                                                             
         XI    1(R2),X'20'               TURN OFF PROTECT BIT                   
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
         B     SSCRFINX                                                         
*                                                                               
SSCRFIN1 DS    0H                  ELSE CLOSE FIELD                             
*                                                                               
         TM    1(R2),X'20'            IF UNPROTECTED CLOSE FIELD                
         BO    *+16                                                             
         OI    1(R2),X'20'               TURN ON PROTECT BIT                    
         OI    6(R2),X'80'               TRANSMIT FIELD                         
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
SSCRFINX DS    0H                                                               
*                                                                               
SSCRCONT DS    0H                                                               
*                                                                               
         BAS   RE,BUMP                                                          
*                                                                               
         CR    R2,R3               CONTINUE IF NOT PAST DETAIL LINES            
         BNH   SSCRLOOP                                                         
*                                                                               
SSCRDONE DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORDS - BLDISS'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD DUMMY ISSUE RECORD IF NO SUCH RECORD EXISTS *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
BLDISS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        CREATE ISSUE RECORD KEY                                                
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY FOR ISSUE RECORD               
         LA    R5,KEY                                                           
         USING PISSREC,R5                                                       
*                                                                               
         MVC   PISSKAGY,AGENCY     SET AGENCY                                   
         MVC   PISSKMED,QMED       SET MEDIA                                    
         MVI   PISSKTYP,X'29'      SET RECORD TYPE                              
         MVC   PISSKPUB,BPUB       SET PUB                                      
         MVC   PISSKYR,WRKYR       SET YEAR                                     
*                                                                               
*        READ IN ISSUE RECORD IF NEEDED                                         
*                                                                               
         CLI   WRKFRQ,C'I'         SKIP IF NOT USING ISSUE RECORD               
         BNE   BLIISSN                                                          
*                                                                               
         L     RF,AIO2             POINT TO ISSUE RECORD IOAREA                 
*                                                                               
         CLC   PISSKEY,0(RF)       SKIP IF RECORD ALREADY READ                  
         BE    BLIISSX                                                          
*                                                                               
*        READ IN ISSUE RECORD                                                   
*                                                                               
         MVC   AIO,AIO2            USE IOA2                                     
*                                                                               
         GOTO1 HIGH                READ KEY                                     
*                                                                               
         CLC   PISSKEY,KEYSAVE     MUST FIND KEY                                
         BNE   BLIISSNO                                                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
*        IF CIRC RECORD EXISTS WE MUST RE-READ IT FOR GENCON                    
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE CIRC REC FILE POINTERS               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PCRCKEY),KEYSAVE IF RECORD FOUND                           
         BNE   BLIISSX                                                          
*                                                                               
         MVC   AIO,AIO1            USE IOA1                                     
*                                                                               
         GOTO1 GETREC              RE-READ RECORD                               
*                                                                               
BLIISSX  DS    0H                                                               
*                                                                               
         B     BLICRC              GO ADD CIRCULATION DATES                     
*                                                                               
*        NO ISSUE RECORD USED - BUILD A DUMMY ONE                               
*                                                                               
BLIISSN  DS    0H                                                               
*                                                                               
         L     R5,AIO2             ESTABLISH ISSUE RECORD AREA                  
         ST    R5,AIO              SET FOR ADDELEM ROUTINE                      
*                                                                               
         XC    0(256,R5),0(R5)     INIT RECORD                                  
*                                                                               
         MVC   PISSKEY,KEY         COPY ISSUE RECORD KEY                        
*                                                                               
         LA    RF,PISSELEM-PISSKEY  MINIMUM RECORD LENGTH                       
         STCM  RF,3,PISSRLEN                                                    
*                                                                               
*                                                                               
*        ADD ANY EXTRA ISSUE ELMS FROM CIRCULATION REC                          
*                                                                               
BLICRC   DS    0H                                                               
*                                                                               
         L     R2,AIO1             POINT TO CIRCULATION RECORD                  
         USING PCRCRECD,R2         ESTABLISH CIRCULATION RECORD                 
*                                                                               
         OC    QCLT,QCLT           IF DOING ONE CLIENT                          
         BZ    *+8                                                              
         L     R2,AIO3                ALL CLIENTS RECORD IN IOA3                
*                                                                               
         CLI   PCRCKTYP,PCRCKIDQ   IF THERE IS A CIRCULATION RECORD             
         BNE   BLICRCX                USE IT TO BUILD TABLE                     
*                                                                               
*        USE ALL CLIENT CIRCULATION RECORD TO BUILD ISSUE RECORD                
*                                                                               
         LA    R2,PCRCELEM         POINT TO FIRST ELEMENT IN RECORD             
         USING PCRCELM,R2          ESTABLISH AS CIRCULATION ELEMENT             
         SR    RF,RF                                                            
*                                                                               
*        FIND FIRST CIRCULATION ELEMENT                                         
*                                                                               
         CLI   PCRCELM,0                                                        
         BE    BLICRCX             DONE IF NO CIRC YET                          
         CLI   PCRCELM,PCRCELQ                                                  
         BE    *+16                                                             
         IC    RF,PCRCLEN                                                       
         LA    R2,PCRCELM(RF)                                                   
         B     *-24                                                             
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH WORKAREA AS ISSUE ELM              
         USING PISSEL29,R6                                                      
*                                                                               
         PACK  DUB,WRKYR+2(2)      GET YEAR IN BINARY                           
         CVB   R0,DUB                                                           
*                                                                               
         CHI   R0,70               CHECK IF 21ST CENTURY                        
         BH    *+8                                                              
         AHI   R0,100                                                           
*                                                                               
         SR    RF,RF                                                            
*                                                                               
BLILOOP  DS    0H                                                               
*                                                                               
         XC    PISSEL29(PISSELLN),PISSEL29  INIT ELEMENT                        
*                                                                               
         MVI   PISSEL29,X'29'      SET ELEMENT CODE                             
         MVI   PISSEL29+1,PISSELLN SET ELEMENT LENGTH                           
*                                                                               
         MVC   PISSDAT,PCRCISS     USE ISSUE DATE FROM CIRC REC                 
*                                                                               
         CLI   PISSDAT,0           IF NO YEAR PRESENT                           
         BNE   *+8                                                              
         STC   R0,PISSDAT             USE YEAR FROM CIRC YEAR                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(PISSEL29,AIO),                X        
               (4,PISSDAT)         CHECK IF ELEMENT IN RECORD                   
         CLI   DMCB+12,0           OKAY IF ELEMENT FOUND                        
         BE    BLICONT                                                          
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
BLICONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PCRCLEN          BUMP TO NEXT CIRC ELEMENT                    
         LA    R2,0(RF,R2)                                                      
         CLI   0(R2),PCRCELQ       MUST BE A CIRC ELEMENT                       
         BE    *+12                                                             
         CLI   0(R2),0             REPEAT ABOVE IF NOT EOR                      
         BNE   *-20                                                             
*                                                                               
         CLI   0(R2),0                                                          
         BNE   BLILOOP             CONTINUE IF MORE IN TABLE/RECORD             
*                                                                               
BLIDONE  DS    0H                                                               
*                                                                               
BLICRCX  DS    0H                                                               
*                                                                               
         CLI   WRKFRQ,PCRHFRXQ     IF NON-PERIODIC                              
         BE    BLDISSX                DONE - EMPTY RECORD                       
*                                                                               
         CLI   WRKFRQ,PCRHFRIQ     IF ISSUE RECORD ORIENTED                     
         BE    BLDISSX                DONE                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
*        BUILD ISSUE RECORD FROM DATE PARAMETERS                                
*                                                                               
BLIDTS   DS    0H                                                               
*                                                                               
         MVI   WISSSW,0            INIT ISSUE SWITCH                            
*                                                                               
         MVC   WSTRTDT,WRKFISS     COPY STARTING ISSUE DATE                     
*                                                                               
         CLI   WSTRTDTD,0          IF NO DAYS SPECIFIED                         
         BNE   *+16                                                             
         CLI   WINC1D,0            AND DAYS ARE IN FORMULA                      
         BE    *+8                                                              
         MVI   WSTRTDTD,1             ASSUME FIRST OF MONTH                     
*                                                                               
         MVC   WNXTISS,WSTRTDT     INIT NEXT ISSUE DATE                         
         XC    W1STDTE,W1STDTE     INIT FIRST FORMULA DATE                      
*                                                                               
         OC    WRKFISS,WRKFISS     SKIP IF NO FIRST ISSUE FOUND                 
         BZ    BLIDDONE                                                         
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH WORKAREA AS ISSUE ELM              
         USING PISSEL29,R6                                                      
*                                                                               
BLIDLOOP DS    0H                                                               
*                                                                               
         CLC   WNXTISSY,WRKFISSY   DONE IF NEXT YEAR REACHED                    
         BH    BLIDDONE                                                         
*                                                                               
*        ADD AN ISSUE ELEMENT                                                   
*                                                                               
         XC    PISSEL29(PISSELLN),PISSEL29  INIT ELEMENT                        
*                                                                               
         MVI   PISSEL29,X'29'      SET ELEMENT CODE                             
         MVI   PISSEL29+1,PISSELLN SET ELEMENT LENGTH                           
*                                                                               
         MVC   PISSDAT,WNXTISS     USE NEXT CALCULATED DATE                     
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(PISSEL29,AIO),                X        
               (4,PISSDAT)         CHECK IF ELEMENT IN RECORD                   
         CLI   DMCB+12,0           OKAY IF ELEMENT FOUND                        
         BE    BLID10                                                           
*                                                                               
         GOTO1 ADDELEM             WON'T CREATE DUPLICATES                      
*                                                                               
BLID10   DS    0H                                                               
*                                                                               
*        DETERMINE NEXT ISSUE DATE                                              
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
         LA    R3,WINC1Y           POINT TO FIRST FORMULA                       
*                                                                               
         TM    WCNTL,X'80'         IF THERE IS A SECOND DATE FORMULA            
         BNO   BLIDLP03                                                         
*                                                                               
         XI    WISSSW,X'80'        FLIP SWITCH                                  
         BZ    BLIDLP02               FIRST FORMULA OKAY                        
*                                                                               
         LA    R3,WINC2Y              POINT TO SECOND FORMULA                   
         MVC   W1STDTE,WNXTISS        SAVE CURRENT 1ST FORM DATE                
*                                                                               
BLIDLP02 DS    0H                                                               
*                                                                               
         MVC   WNXTISS(3),W1STDTE  ALWAYS USE FIRST ISSUE DATE                  
*                                                                               
BLIDLP03 DS    0H                                                               
*                                                                               
         ICM   RE,1,WINC1Y-WINC1Y(R3) GET YEARS TO NEXT DATE                    
         BZ    *+14                   SKIP IF NONE                              
         IC    RF,WNXTISSY         GET CURRENT YEAR                             
         AR    RF,RE               INCREMENT YEAR                               
         STC   RF,WNXTISSY         SET NEW YEAR                                 
*                                                                               
         ICM   RE,1,WINC1M-WINC1Y(R3) GET MONTHS TO NEXT DATE                   
         BZ    BLIDLP05               SKIP IF NONE                              
*                                                                               
         IC    RF,WNXTISSM         GET CURRENT MONTH                            
         AR    RF,RE               INCREMENT MONTH                              
         CLM   RF,1,=AL1(12)       CHECK FOR NEW YEAR                           
         BH    *+12                                                             
         STC   RF,WNXTISSM         SET NEW MONTH                                
         B     BLIDLP05                                                         
*                                                                               
         MVI   WNXTISSM,1          SET TO JANUARY                               
         IC    RF,WNXTISSY         GET CURRENT YEAR                             
         LA    RF,1(RF)            BUMP TO NEXT YEAR                            
         STC   RF,WNXTISSY         SET NEW YEAR                                 
                                                                                
*                                                                               
BLIDLP05 DS    0H                                                               
*                                                                               
         ICM   RF,1,WINC1D-WINC1Y(R3) GET DAYS TO NEXT DATE                     
         BZ    BLIDLP10               SKIP IF NONE                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,WNXTISS),WRKDAT1C  YYMMDD                         
         SR    RF,RF                                                            
         ICM   RF,1,WINC1D-WINC1Y(R3) GET DAYS TO NEXT DATE                     
         GOTO1 ADDAY,DMCB,WRKDAT1C,WRKDAT2C,(RF)  ADD DAYS                      
         GOTO1 DATCON,DMCB,WRKDAT2C,(3,WNXTISS)   YMD                           
*                                                                               
BLIDLP10 DS    0H                                                               
*                                                                               
BLIDCONT DS    0H                                                               
*                                                                               
         B     BLIDLOOP            CONTINUE IF MORE IN TABLE/RECORD             
*                                                                               
BLIDDONE DS    0H                                                               
*                                                                               
BLDISSX  DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            RESTORE RECORD POINTER                       
*                                                                               
         XIT1                                                                   
*                                                                               
BLIISSNO DS    0H                  ISSUE RECORD NOT FOUND                       
*                                                                               
         LA    R2,CRCFRQH          PUT CURSOR AT FREQUENCY FIELD                
         MVI   ERROR,PWEISSNO      SET ERROR CODE                               
         GOTO1 ERREX                                                            
*                                                                               
         DROP  R2,R5,R6                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORDS - SUBROUTS'                       
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
         TITLE 'VALIDATE ISSUE DATE - VALISS'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE ISSUE DATE - VALISS                                 *         
*                                                                     *         
*NTRY    R2  ==>  ISSUE DATE FIELD. YEAR NEVER ENTERED                *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VALISS   NTR1  LABEL=*                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALESTA+2(4),=C'0101'  DEFAULT IS JAN01                         
         MVC   PVALESTA(2),WRKYR+2     OF CIRC YEAR                             
*                                                                               
         GOTO1 DATCON,DMCB,PVALESTA,PVALESTA CONVERT TO Y2K FORMAT              
*                                                                               
         LA    RF,PVINTOD+PVINSGLO+PVINSGLS                                     
*                           DEFAULT DATE GIVEN & SINGLE DATES ONLY              
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),((RF),PVALOUTB) VAL PERIOD             
*                                                                               
         CLI   4(R1),4             SINGLE DATE ENTERED OKAY                     
         BE    *+8                                                              
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VALISSE1                                                         
*                                                                               
         MVC   WRKISS,PVALBSTA     SAVE ISSUE DATE                              
*                                                                               
         TM    PVALASSM,PVALASD    IF START DAY ASSUMED                         
         BNO   *+8                                                              
         MVI   WRKISSDY,0             KILL DAY IN ISSUE DATE                    
*                                                                               
         CLI   WRKFRQ,C'I'         SKIP IF NOT USING ISSUE RECORD               
         BNE   VLISFND                                                          
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY FOR ISSUE RECORD               
         LA    R4,KEY                                                           
         USING PISSREC,R4                                                       
*                                                                               
         MVC   PISSKAGY,AGENCY     SET AGENCY                                   
         MVC   PISSKMED,QMED       SET MEDIA                                    
         MVI   PISSKTYP,X'29'      SET RECORD TYPE                              
         MVC   PISSKPUB,BPUB       SET PUB                                      
         MVC   PISSKYR,WRKYR       SET YEAR                                     
*                                                                               
         L     RF,AIO2             POINT TO ISSUE RECORD IOAREA                 
*                                                                               
         CLC   PISSKEY,0(RF)       SKIP IF RECORD ALREADY READ                  
         BE    VLIRECX                                                          
*                                                                               
*        READ IN ISSUE RECORD                                                   
*                                                                               
         MVC   AIO,AIO2            USE IOA2                                     
*                                                                               
         GOTO1 HIGH                READ KEY                                     
*                                                                               
         CLC   PISSKEY,KEYSAVE     MUST FIND KEY                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   AIO,AIO1            REVERT TO ORIGINAL IOAREA                    
         MVC   KEY,ORIGKEY                                                      
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
*        IF CIRC RECORD EXISTS WE MUST RE-READ IT FOR GENCON                    
*                                                                               
         CLC   KEY(L'PCRCKEY),KEYSAVE IF RECORD FOUND                           
         BNE   VLIISSX                                                          
*                                                                               
         GOTO1 GETREC              RE-READ RECORD                               
*                                                                               
VLIISSX  DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE BASE KEY FOR GENCON                  
*                                                                               
VLIRECX  DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE ORIGINAL KEY                         
*                                                                               
         L     R4,AIO2             POINT TO ISSUE RECORD                        
         USING PISSREC,R4          ESTABLISH RECORD                             
*                                                                               
         LA    R3,PISSELEM         POINT TO FIRST ELEMENT IN RECORD             
         USING PISSEL29,R3         ESTABLISH ELEMENT                            
*                                                                               
         SR    RF,RF                                                            
*                                                                               
VLISLOOP DS    0H                                                               
*                                                                               
         CLI   PISSEL29,0          ERROR IF END OF RECORD REACHED               
         BE    VALISSE2                                                         
*                                                                               
         CLI   PISSEL29,X'29'      LOOKING FOR ISSUE ELEMENTS                   
         BNE   VLISCONT                                                         
*                                                                               
         CLC   WRKISS,PISSDAT      LOOKING FOR DATE MATCH                       
         BE    VLISFND                                                          
*                                                                               
VLISCONT DS    0H                                                               
*                                                                               
         IC    RF,PISSEL29+1       GET ELEMENT LENGTH                           
         LA    R3,PISSEL29(RF)     BUMP TO NEXT ELEMENT                         
         B     VLISLOOP                                                         
*                                                                               
VLISFND  DS    0H                                                               
*                                                                               
         CLI   WRKFRQ,PCRHFRXQ     SKIP IF NON-PERIODIC FREQUENCY               
         BE    VLISATBX                                                         
*                                                                               
         TM    1(R2),X'20'            IF UNPROTECTED CLOSE FIELD                
         BO    *+12                                                             
         OI    1(R2),X'20'               TURN ON PROTECT BIT                    
         OI    6(R2),X'80'               TRANSMIT FIELD                         
*                                                                               
VLISATBX DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'               INDICATE FIELD VALIDATED               
*                                                                               
         B     VALISSX                                                          
*                                                                               
VALISSE1 DS    0H                  INVALID DATE                                 
         MVI   ERROR,INVDATE                                                    
         B     VALISSER                                                         
*                                                                               
VALISSE2 DS    0H                  INVALID ISSUE                                
         MVI   ERROR,INVALID                                                    
         B     VALISSER                                                         
*                                                                               
VALISSER DS    0H                                                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
VALISSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
         CLI   SAVMSGNO,0          IF NOT FIRST ERROR                           
         BE    *+14                                                             
         MVC   ERROR,SAVMSGNO      RESTORE PRIOR MESSAGE                        
         B     ERRFLDX                                                          
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
***********************************************************************         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R1               *         
***********************************************************************         
         SPACE 2                                                                
DSPFLD   NTR1  LABEL=*             BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         USING FLDHDRD,R1          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),FLD         MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,X'80'       TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
EDT      DS    0H          USED TO DISPLAY OUTDOOR SRI SPACES                   
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R6),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R6)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
*                                                                               
ALLCNFER MVI   ERROR,NOTFOUND      ALL CLIENT CIRC NOT FOUND                    
         LA    R2,CRCCLTH          CURSOR AT CLIENT                             
         B     ERRX                                                             
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPEDATA      DUPLICATE DATA                               
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,CRCISS1H                                                      
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
*                                                                               
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,57,C'CIRCULATION REPORT'                                      
         SSPEC H2,57,C'------------------'                                      
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,66,C'TOTAL CIRC'                                              
         SSPEC H7,79,C'RATE BASE'                                               
         SSPEC H7,92,C'PRELIM CIRC'                                             
         SSPEC H7,105,C'FINAL CIRC'                                             
         SSPEC H8,1,C'PUB CODE'                                                 
         SSPEC H8,17,C'PUB NAME'                                                
         SSPEC H8,39,C'SOURCE'                                                  
         SSPEC H8,46,C'YEAR'                                                    
         SSPEC H8,52,C'CLIENT'                                                  
         SSPEC H8,59,C'ISSUE'                                                   
         SSPEC H8,66,C'  (000)'                                                 
         SSPEC H8,79,C'  (000)'                                                 
         SSPEC H8,92,C'  (000)'                                                 
         SSPEC H8,105,C'  (000)'                                                
         SSPEC H9,1,C'--------'                                                 
         SSPEC H9,17,C'--------'                                                
         SSPEC H9,39,C'------'                                                  
         SSPEC H9,46,C'----'                                                    
         SSPEC H9,52,C'------'                                                  
         SSPEC H9,59,C'-----'                                                   
         SSPEC H9,66,C'----------'                                              
         SSPEC H9,79,C'---------'                                               
         SSPEC H9,92,C'-----------'                                             
         SSPEC H9,105,C'----------'                                             
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORD - PFKEYS'                          
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT A LINE                                   *         
*              PF6  - DELETE A LINE                                   *         
*              PF9  - CASCADE DATA                                    *         
*              PF10 - ISSUE RECORD                                    *         
*                                                                     *         
*NTRY    R5 ==>  LINUP CONTROL BLOCK                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1  LABEL=*                                                          
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         CLI   TIOBAID,9           SKIP IF PF9 OR PF21                          
         BE    *+8                                                              
         CLI   TIOBAID,21                                                       
         BE    PFKEYS1                REPEAT LINES                              
*                                                                               
         CLI   TIOBAID,10          SKIP IF TRANSFER TO ISSUE RECORD             
         BE    *+8                                                              
         CLI   TIOBAID,22                                                       
         BE    PFKEYS1                                                          
*                                                                               
         CLC   CONACT(3),=C'ADD'   IF ACTION 'ADD'                              
         BNE   PFKEYS1                                                          
*                                                                               
         FOUT  CONACTH,=C'CHA'        CHANGE TO 'CHANGE'                        
*                                                                               
PFKEYS1  DS    0H                                                               
*                                                                               
         CLI   TIOBAID,10          CHECK FOR TRANSFER TO ISSUE RECORD           
         BE    *+8                                                              
         CLI   TIOBAID,22          CHECK FOR TRANSFER TO ISSUE RECORD           
         BE    PFKISS                                                           
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
*                                                                               
         CLI   TIOBAID,9           SKIP IF DITTOING                             
         BE    *+8                                                              
         CLI   TIOBAID,21                                                       
         BE    PFKLNX                                                           
*                                                                               
         LA    R4,LSVTAB           POINT TO START OF LINUP SAVEAREA             
         USING LSVTABD,R4          ESTABLISH LINUP SAVEAREA ENTRY               
*                                                                               
*        FIND LINE WITH CURSOR                                                  
*                                                                               
         CLC   TIOBCURD,0(R6)      MUST BE AFTER START OF LINE                  
         BL    PFKEYSX                                                          
         CLC   TIOBCURD,2(R6)      AND BEFORE START OF NEXT LINE                
         BL    *+16                                                             
         LA    R6,2(R6)            BUMP TO NEXT TABLE ENTRY                     
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         B     *-28                                                             
*                                                                               
PFKLNX   DS    0H                                                               
*                                                                               
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TIOBAID,4           PF4 OR PF16                                  
         BE    *+8                                                              
         CLI   TIOBAID,16                                                       
         BE    LNADD                  ADD A LINE                                
*                                                                               
         CLI   TIOBAID,6           PF6 OR PF18                                  
         BE    *+8                                                              
         CLI   TIOBAID,18                                                       
         BE    LNDEL                  DELETE A LINE                             
*                                                                               
         CLI   TIOBAID,9           PF9 OR PF21                                  
         BE    *+8                                                              
         CLI   TIOBAID,21                                                       
         BE    LNRPT                  REPEAT LINES                              
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION RECORD - LNADD'                           
***********************************************************************         
*                                                                     *         
*        ADD A LINE                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
         OC    QCLT,QCLT           MUST BE ALL CLIENT RECORD                    
         BNZ   LNCLTER                                                          
*                                                                               
         CLI   WRKFRQ,PCRHFRIQ     CAN NOT BE ISSUE RECORD FREQUENCY            
         BE    LNFRIER                                                          
*                                                                               
         LA    R4,NLINS            NUMBER OF LINES ON SCREEN                    
         BCTR  R4,0                DECREMENT FOR INDEXING                       
         BCTR  R4,0                DECREMENT FOR NEXT TO LAST ENTRY             
         MH    R4,=Y(LSVTABL)      DISP  TO NEXT TO LAST IN LINUP SAVE          
         LA    R4,LSVTAB(R4)       POINT TO NEXT TO LAST IN LINUP SAVE          
*                                                                               
         LA    R6,LNTBLLS          POINT TO LAST ENTRY IN TABLE                 
         LR    R5,R6                                                            
         SH    R5,=H'2'            BACK UP A TABLE ENTRY                        
         SR    R1,R1                                                            
*                                                                               
LNADDLP  DS    0H                                                               
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         LA    R0,5                NUMBER OF FIELDS ON A LINE                   
         SR    R2,R2                                                            
*                                                                               
LNADDLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE DOWN ONE                           
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNADDCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADDLP1                                                      
*                                                                               
LNADDDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD+LSVTABL(LSVTABL),LSVTABD  COPY LINUP SAVE                
*                                                                               
LNADDCN  DS    0H                                                               
*                                                                               
         SH    R4,=Y(LSVTABL)      BACK UP LINUP SAVE ENTRY                     
*                                                                               
         LR    R6,R5               BACK UP ENTRIES IN TABLE                     
         SH    R5,=H'2'                                                         
*                                                                               
         CLC   0(2,R5),ALINCUR     STOP IF PASSED LINE WITH CURSOR              
         BNL   LNADDLP                                                          
*                                                                               
LNADDDN  DS    0H                                                               
*                                                                               
         AH    R4,=Y(LSVTABL)      POINT TO DATA FOR CURSOR LINE                
         XC    LSVTABD(LSVTABL),LSVTABD  CLEAR LINUP SAVE ENTRY                 
*                                                                               
         LH    R6,0(R6)            POINT TO FIRST OF CURSOR LINE                
         LA    R6,0(R6,RA)                                                      
*                                                                               
         NI    1(R6),X'FF'-X'20'   UNPROTECT ISSUE FIELD                        
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
         ST    R6,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
         LA    R0,5                NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNADXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
LNADXCCN DS    0H                                                               
*                                                                               
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADXCLP                                                      
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PRSFM10 - CRC FILE - INSTRUCTION RECORD - LNDEL'                
***********************************************************************         
*                                                                     *         
*        DELETE A LINE                                                *         
*                                                                     *         
*NTRY    R6==> TABLE ENTRY FOR LINE WITH CURSOR                       *         
*        R4==> LINUP TABLE ENTRY FOR LINE WITH CURSOR                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNDEL    DS    0H                  DELETE LINE AT CURSOR                        
*                                                                               
         OC    QCLT,QCLT           MUST BE ALL CLIENT RECORD                    
         BNZ   LNCLTER                                                          
*                                                                               
         CLI   WRKFRQ,PCRHFRIQ     CAN NOT BE ISSUE RECORD FREQUENCY            
         BE    LNFRIER                                                          
*                                                                               
         LH    R5,0(R6)            POINT TO FIRST FIELD OF CURSOR LINE          
         LA    R5,0(R5,RA)                                                      
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         TM    1(R5),X'20'         IF PROTECTED FIELD                           
         BNO   *+16                                                             
         IC    RF,0(R5)                                                         
         LA    R5,0(RF,R5)            BUMP TO NEXT FIELD                        
         B     *-16                                                             
*                                                                               
         ST    R5,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
*        FIND DATA FOR THIS LINE                                                
*                                                                               
         OC    LSVTABD(LSVTABL),LSVTABD    SKIP IF NO DATA ON LINE              
         BZ    LNDELD10                                                         
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R4)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,BSPAREC          POINT TO FOUND ELEMENT                       
         OI    ELTCTL-ELTABD(R1),ELTDELQ   FLAG FOR DELETE                      
*                                                                               
         XC    LSVTABD(LSVTABL),LSVTABD   CLEAR TABLE ENTRY                     
*                                                                               
LNDELD10 DS    0H                                                               
*                                                                               
         LR    R5,R6                                                            
         AH    R5,=H'2'            POINT TO NEXT TABLE ENTRY                    
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
LNDELLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R5),=X'FFFF'    STOP IF PASSED END OF TABLE                  
         BNL   LNDELDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         LA    R0,5                NUMBER OF FIELDS ON A LINE                   
*                                                                               
LNDELLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE UP ONE                             
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNDELCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDELLP1                                                      
*                                                                               
LNDELDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD(LSVTABL),LSVTABD+LSVTABL  COPY LINUP SAVEAREA            
*                                                                               
LNDELCN  DS    0H                                                               
*                                                                               
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         LR    R6,R5               ADVANCE ENTRIES IN TABLE                     
         AH    R5,=H'2'                                                         
*                                                                               
         B     LNDELLP                                                          
*                                                                               
LNDELDN  DS    0H                                                               
         SR    R6,R6                                                            
         ICM   R6,3,LNTBLLS        POINT TO LAST LINE IN TABLE                  
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         LA    R0,5                NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNDLXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
LNDLXCCN DS    0H                                                               
*                                                                               
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDLXCLP                                                      
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION POSTING - LNRPT'                          
***********************************************************************         
*                                                                     *         
*        REPEAT DATA FROM LINE ABOVE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNRPT    DS    0H                                                               
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
LNRPTLP  DS    0H                                                               
*                                                                               
         LA    R5,2(R6)            POINT TO NEXT LINE IN TABLE                  
*                                                                               
         CLC   0(2,R5),=X'FFFF'    DONE IF END OF TABLE REACHED                 
         BE    LNRPTDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         CLI   5(RE),0             DONE IF NO ISSUE DATE ON                     
         BE    LNRPTDN                RECEIVING LINE                            
*                                                                               
*        BUMP PAST ISSUE DATE FIELD ON BOTH LINES                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         LA    R0,4                NUMBER OF NUMERIC FIELDS ON LINE             
*                                                                               
         OC    QCLT,QCLT           IF CLIENT REC                                
         BNZ   LNRPTCN1               THEN TOTAL CIRC IS IMMUNE                 
*                                                                               
LNRPTLP1 DS    0H                                                               
*                                                                               
         CLI   5(RE),0             SKIP IF DATA ALREADY IN NEXT LINE            
         BNE   LNRPTCN1                                                         
*                                                                               
         CLI   5(RF),0             SKIP IF NOTHING TO MOVE                      
         BE    LNRPTCN1                                                         
*                                                                               
         IC    R2,0(RE)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RE),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),8(RF)       COPY LINE DOWN ONE                           
*                                                                               
         MVC   4(2,RE),4(RF)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RE),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RE),5(RE)       SET OUTPUT LENGTH                            
         NI    4(RE),X'FF'-X'20'   SET TO NOT VALIDATED                         
*                                                                               
LNRPTCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNRPTLP1                                                      
*                                                                               
LNRPTCN  DS    0H                                                               
*                                                                               
         LA    R6,2(R6)            BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         B     LNRPTLP                                                          
*                                                                               
LNRPTDN  DS    0H                                                               
*                                                                               
LNRPTX   DS    0H                                                               
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION POSTING - ERRORS'                         
***********************************************************************         
*                                                                     *         
*        ERROR ROUTINES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNCLTER  DS    0H                                                               
         LA    R2,CRCCLTH          CLIENT                                       
         MVI   ERROR,PWELNCLT      NOT ALLOWED FOR CLT REC                      
         B     PFKEYERX                                                         
*                                                                               
LNFRIER  DS    0H                                                               
         LA    R2,CRCFRQH          PUT CURSOR AT FREQUENCY FIELD                
         MVI   ERROR,PWELNFRI      NOT ALLOWED FOR FRQ ISSUE REC                
         B     PFKEYERX                                                         
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION POSTING - PFKISS'                         
***********************************************************************         
*                                                                     *         
*        TRANSFER TO ISSUE RECORD USING GLOBBER                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKISS   DS    0H                  NORMAL EXIT                                  
*                                                                               
         XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1SEPS                                                
******** OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL  SEND XCTL ELM            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'ISSUE',5,GLVXREC  RECORD FLD           
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'DISPLAY',7,GLVXACT ACTION              
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CRCMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CRCMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CRCPUBH,,GLVPRPUB   PUB                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CRCPERH,,GLVPRPER   YEAR                  
*                                                                               
         B     PFKEYERX                                                         
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION POSTING - EXITS'                          
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - FIRST ISS - VRFISS'                
***********************************************************************         
*                                                                     *         
*        VALIDATE FIRST ISSUE DATES                                   *         
*                                                                     *         
*        ADD  - IF A CLIENT OVERRIDE RECORD,                          *         
*                  MUST MATCH START ISS OF MASTER CIRC RECORD         *         
*               IF MASTER CIRC RECORD                                 *         
*                  MUST USE ISSUE RECORD IF IT EXISTS                 *         
*                  ELSE REQUIRED INPUT                                *         
*                                                                     *         
*               ANY CHANGE FORCES A RE-DISPLAY                        *         
*                                                                     *         
*        DIS  - FIRST ISS FORCED TO THAT OF RECORD TO BE DISPLAYED    *         
*        CHA                                                          *         
*        DEL  - FIRST ISS FORCED TO THAT OF RECORD TO BE DELETED      *         
*        RES                                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
VRFISS   NTR1  LABEL=*                                                          
*                                                                               
         LA    R2,CRCFISSH         POINT TO ENTERED FIRST ISSUE                 
         XC    WRKFISS,WRKFISS     INIT ENTERED FIRST ISSUE                     
         XC    DFLTFISS,DFLTFISS   INIT DEFAULT FIRST ISSUE                     
         XC    HDRFISS,HDRFISS     INIT HEADER  FIRST ISSUE                     
*                                                                               
*        FIND DEFAULT FIRST ISSUE                                               
*                                                                               
VRFISSDF DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADD                                   
         BE    VRFISSHD                                                         
*                                                                               
*        USE FIRST ISSUE IN CIRCULATION RECORD                                  
*                                                                               
         L     RF,AIO1                USE ISSUE IN CIRC RECORD                  
         CLC   ORIGKEY(L'PCRCKEY),0(RF)  SKIP IF INCORRECT REC IN CORE          
         BNE   VRFISCLN                                                         
*                                                                               
         SR    RE,RE                                                            
*                                                                               
         LA    R6,PCRCELEM-PCRCRECD(RF) POINT TO 1ST ELEM IN RECORD             
         USING PCRHELM,R6          ESTABLISH AS HEADER ELEMENT                  
*                                                                               
         CLI   PCRHELM,0           DONE IF END OF RECORD                        
         BE    *+34                                                             
         CLI   PCRHELM,PCRHELQ     FIND CIRC HEADER ELEMENT                     
         BNE   *+14                                                             
         MVC   HDRFISS,PCRHFISS    USE FIRST ISSUE IN CIRC RECORD               
         B     *+16                                                             
         IC    RE,PCRHLEN          ELEMENT LENGTH                               
         LA    R6,PCRHELM(RE)      BUMP TO NEXT ELEMENT                         
         B     *-34                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
VRFISSHD DS    0H                                                               
*                                                                               
         OC    QCLT,QCLT           IF CLIENT OVERRIDE RECORD                    
         BZ    VRFISCLN                                                         
*                                                                               
*        USE FIRST ISSUE FROM ALL CLIENT RECORD                                 
*                                                                               
         SR    RE,RE                                                            
         L     RF,AIO3                USE FREQUENCY IN CIRC RECORD              
         LA    R6,PCRCELEM-PCRCRECD(RF) POINT TO 1ST ELEM IN RECORD             
         USING PCRHELM,R6          ESTABLISH AS HEADER ELEMENT                  
*                                                                               
         CLI   PCRHELM,0           DONE IF END OF RECORD                        
         BE    *+34                                                             
         CLI   PCRHELM,PCRHELQ     FIND CIRC HEADER ELEMENT                     
         BNE   *+14                                                             
         MVC   DFLTFISS,PCRHFISS   USE START ISSUE IN CIRC RECORD               
         B     *+16                                                             
         IC    RE,PCRHLEN          ELEMENT LENGTH                               
         LA    R6,PCRHELM(RE)      BUMP TO NEXT ELEMENT                         
         B     *-34                                                             
*                                                                               
         B     VRFISDFX                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
VRFISCLN DS    0H                                                               
*                                                                               
*        CHECK TO SEE IF THERE IS AN ISSUE RECORD                               
*                                                                               
VRFISFIS DS    0H                                                               
*                                                                               
         XC    WISSKEY,WISSKEY     INIT ISSUE KEY SAVEAREA                      
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY FOR ISSUE RECORD               
         LA    R5,KEY                                                           
         USING PISSREC,R5                                                       
*                                                                               
         MVC   PISSKAGY,AGENCY     SET AGENCY                                   
         MVC   PISSKMED,QMED       SET MEDIA                                    
         MVI   PISSKTYP,X'29'      SET RECORD TYPE                              
         MVC   PISSKPUB,BPUB       SET PUB                                      
         MVC   PISSKYR,WRKYR       SET YEAR                                     
*                                                                               
         L     RF,AIO2             POINT TO ISSUE RECORD AREA                   
*                                                                               
         CLC   PISSKEY,0(RF)       SKIP IF RECORD IN CORE                       
         BNE   *+14                                                             
         MVC   WISSKEY,PISSKEY     SAVE KEY                                     
         B     VRFISISX                                                         
*                                                                               
         GOTO1 HIGH                READ FOR RECORD                              
*                                                                               
         CLC   PISSKEY,KEYSAVE     IF ISSUE RECORD KEY FOUND                    
         BNE   VRFISISX                                                         
*                                                                               
         MVC   WISSKEY,PISSKEY        SAVE KEY                                  
*                                                                               
         MVC   AIO,AIO2            READ IN RECORD                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
*        IF CIRC RECORD EXISTS WE MUST RE-READ IT FOR GENCON                    
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE CIRC REC FILE POINTERS               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PCRCKEY),KEYSAVE IF RECORD FOUND                           
         BNE   VRFISISX                                                         
*                                                                               
         MVC   AIO,AIO1            USE IOA1                                     
*                                                                               
         GOTO1 GETREC              RE-READ RECORD                               
*                                                                               
VRFISISX DS    0H                                                               
*                                                                               
         OC    WISSKEY,WISSKEY     IF THERE IS AN ISSUE RECORD                  
         BZ    VRFISISN                                                         
*                                                                               
*        DEFAULT TO FIRST ISSUE FOUND IN ISSUE RECORD                           
*                                                                               
         SR    RE,RE                                                            
         L     RF,AIO2                USE FREQUENCY IN CIRC RECORD              
         LA    R6,PISSELEM-PISSREC(RF) POINT TO 1ST ELEM IN RECORD              
         USING PISSEL29,R6         ESTABLISH AS ISSUE ELEMENT                   
*                                                                               
         CLI   PISSEL29,0          DONE IF END OF RECORD                        
         BE    *+34                                                             
         CLI   PISSEL29,X'29'      FIND ISSUE ELEMENT                           
         BNE   *+14                                                             
         MVC   DFLTFISS,PISSDAT    USE FIRST ISSUE IN ISSUE RECORD              
         B     *+16                                                             
         IC    RE,PISSEL29+1       ELEMENT LENGTH                               
         LA    R6,PISSEL29(RE)     BUMP TO NEXT ELEMENT                         
         B     *-34                                                             
*                                                                               
VRFISISN DS    0H                                                               
*                                                                               
VRFISDFX DS    0H                                                               
*                                                                               
*        VALIDATE FIELD ENTRY                                                   
*                                                                               
         LA    R2,CRCFISSH         POINT TO FIRST ISSUE FLD                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRFISVLX            SKIP IF NO INPUT                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES      SKIP IF NO ENTRY                             
         BNH   VRFISVLX            SKIP IF NO INPUT                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALESTA+2(4),=C'0101'  DEFAULT IS JAN01                         
         MVC   PVALESTA(2),WRKYR+2     OF CIRC YEAR                             
*                                                                               
         GOTO1 DATCON,DMCB,PVALESTA,PVALESTA CONVERT TO Y2K FORMAT              
*                                                                               
         LA    RF,PVINTOD+PVINSGLO+PVINSGLS                                     
*                           DEFAULT DATE GIVEN & SINGLE DATES ONLY              
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),((RF),PVALOUTB) VAL PERIOD             
*                                                                               
         CLI   4(R1),4             SINGLE DATE ENTERED OKAY                     
         BE    *+8                                                              
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VRFISSE1                                                         
*                                                                               
         PACK  DUB,WRKYR+2(2)      YEAR                                         
         CVB   R0,DUB                                                           
         CHI   R0,70               Y2K CHECK                                    
         BH    *+8                                                              
         AHI   R0,100                                                           
*                                                                               
         CLM   R0,1,PVALBSTA       MUST BE IN CIRC YEAR                         
         BNE   VRFISSE1                                                         
*                                                                               
         MVC   WRKFISS,PVALBSTA    SAVE ISSUE DATE                              
*                                                                               
         TM    PVALASSM,PVALASD    IF START DAY ASSUMED                         
         BNO   *+8                                                              
         MVI   WRKFISSD,0             KILL DAY IN ISSUE DATE                    
*                                                                               
VRFISVLX DS    0H                                                               
*                                                                               
         OC    DFLTFISS,DFLTFISS   IF NO DEFAULT ISSUE                          
         BNZ   *+10                                                             
         MVC   DFLTFISS,HDRFISS       USE HEADER  ISSUE                         
*                                                                               
         OC    WRKFISS,WRKFISS     IF NO ISSUE ENTERED                          
         BNZ   *+10                                                             
         MVC   WRKFISS,HDRFISS        USE HEADER  ISSUE                         
*                                                                               
         OC    WRKFISS,WRKFISS     IF NO ISSUE ENTERED                          
         BNZ   *+10                                                             
         MVC   WRKFISS,DFLTFISS       USE DEFAULT ISSUE                         
*                                                                               
         OC    WRKFISS,WRKFISS     IF NO FIRST SPECIFIED                        
         BNE   VRFIS20                                                          
*                                                                               
         CLI   MODE,DISPREC        SKIP IF DISPLAYING RECORD                    
         BE    VRFIS19                                                          
*                                                                               
         CLI   ACTNUM,ACTADD          ERROR IF ADD                              
         BE    *+8                                                              
         CLI   ACTNUM,ACTCHA          ERROR IF CHANGE                           
         BE    VRFISMIS                                                         
*                                  ELSE                                         
VRFIS19  DS    0H                                                               
*                                                                               
         MVI   NEWKEY,C'Y'            FORCE RE-DISPLAY                          
*                                                                               
         B     VRFISSX                                                          
*                                                                               
VRFIS20  DS    0H                                                               
*                                                                               
         OC    HDRFISS,HDRFISS     SKIP IF NO HEADER ISSUE                      
         BZ    VRFIS30                                                          
*                                                                               
         CLC   WRKFISS,HDRFISS     SKIP IF HEADER ISSUE BEING USED              
         BE    VRFIS30                                                          
*                                                                               
         CLI   MODE,DISPREC        SKIP IF DISPLAYING RECORD                    
         BE    VRFIS30                                                          
*                                                                               
*        CAN ONLY CHANGE ISSUE IF NO DETAILS HAVE BEEN ENTERED                  
*                                                                               
         MVI   ELCODE,PCRCELQ      SET TO FIND CIRC ELEMENTS                    
         L     R6,AIO1             POINT TO CIRC RECORD                         
*                                                                               
         BAS   RE,GETEL                                                         
*                                                                               
VFISDTLL DS    0H                                                               
*                                                                               
         BNE   VFISDTLD            NO MORE DETAIL ELEMENTS                      
*                                                                               
         USING PCRCELM,R6          ESTABLISH CIRCULATION DETAILS                
*                                                                               
         OC    PCRCTOT(20),PCRCTOT ERROR IF DETAILS EXIST                       
         BNZ   VRFISCHE                                                         
*                                                                               
VFISDTLC DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     VFISDTLL                                                         
*                                                                               
VFISDTLD DS    0H                                                               
*                                                                               
         GOTO1 REMELEM             CLEAR OUT OLD DETAIL ELEMENTS                
*                                                                               
         CLC   WRKFISS,DFLTFISS    SKIP IF USING DEFAULT ISSUE                  
         BE    VRFIS30                                                          
*                                                                               
         OC    WISSKEY,WISSKEY     ERROR IF DEFAULT IS ISSUE RECORD             
         BNZ   VRFISISE                                                         
*                                                                               
VRFIS30  DS    0H                                                               
*                                                                               
         CLC   WRKFISS,WRKFISSV    IF FIRST ISSUE DATE CHANGED                  
         BE    *+24                                                             
         MVC   WRKFISSV,WRKFISS       SAVE FOR NEXT TIME                        
         MVC   SAVFISS,WRKFISS        SAVE FOR NEXT TIME                        
         MVI   NEWKEY,C'Y'            FORCE RE-DISPLAY                          
         OI    GENSTAT2,RETEQSEL      HAVE GENCON RETURN THIS SCREEN            
*                                                                               
         OI    CRCFISSH+4,X'20'    INDICATE VALID FIELD                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,WRKFISS),(17,CRCFISS) DISPLAY ISSUE DATE          
*                                                                               
         LA    RF,CRCFISS+L'CRCFISS-1  LAST BYTE OF OUTPUT                      
         LA    R0,L'CRCFISS        MAX LENGTH OF FIELD                          
*                                                                               
         CLI   0(RF),C'/'          FIND '/YY'                                   
         BE    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     *+10                YEAR NOT FOUND                               
*                                                                               
         MVC   0(3,RF),SPACES      ELIMINATE YEAR                               
*                                                                               
         OI    CRCFISSH+6,X'80'    FORCE RE-DISPLAY OF FIELD                    
*                                                                               
VRFISSX  DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE CIRC REC KEY                         
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE BASE KEY FOR GENCON                  
*                                                                               
VRADDX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VRFISMIS DS    0H                  NO FIRST ISSUE DATE FOUND                    
*                                                                               
         MVI   ERROR,MISSING                                                    
         B     VRFISERR                                                         
*                                                                               
VRFISCHE DS    0H                  NO CHANGE IF DETAILS EXIST                   
*                                                                               
         MVI   ERROR,PWECFRQE                                                   
*                                                                               
         LA    R2,CRCISS1H            CURSOR TO FIRST OPEN FIELD                
*                                                                               
         TM    1(R2),X'20'                                                      
         BNO   *+8                                                              
         BAS   RE,BUMPU               BUMP TO NEXT UNPROTECTED FIELD            
*                                                                               
         XC    CRCFISS,CRCFISS                                                  
         GOTO1 DATCON,DMCB,(3,HDRFISS),(17,CRCFISS) DISPLAY HDR ISS DTE         
*                                                                               
         LA    RF,CRCFISS+L'CRCFISS-1  LAST BYTE OF OUTPUT                      
         LA    R0,L'CRCFISS        MAX LENGTH OF FIELD                          
*                                                                               
         CLI   0(RF),C'/'          FIND '/YY'                                   
         BE    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     *+10                YEAR NOT FOUND                               
*                                                                               
         MVC   0(3,RF),SPACES      ELIMINATE YEAR                               
*                                                                               
         OI    CRCFISSH+6,X'80'    FORCE RE-DISPLAY OF FIELD                    
*                                                                               
         B     VRFISERR                                                         
*                                                                               
VRFISSE1 DS    0H                  INVALID FIRST ISSUE DATE                     
*                                                                               
         MVI   ERROR,INVDATE                                                    
*                                                                               
         B     VRFISERR                                                         
*                                                                               
VRFISIRE DS    0H                  ISSUE RECORD NOT FOUND                       
*                                                                               
         MVI   ERROR,PWEISSNO                                                   
         LA    R2,CRCSRCH          CURSOR AT SOURCE                             
         B     VRFISERR                                                         
*                                                                               
VRFISISE DS    0H                  MUST USE ISSUE RECORD                        
*                                                                               
         XC    CRCFISS,CRCFISS                                                  
         GOTO1 DATCON,DMCB,(3,DFLTFISS),(17,CRCFISS) DISP ISSUE REC DTE         
*                                                                               
         LA    RF,CRCFISS+L'CRCFISS-1  LAST BYTE OF OUTPUT                      
         LA    R0,L'CRCFISS        MAX LENGTH OF FIELD                          
*                                                                               
         CLI   0(RF),C'/'          FIND '/YY'                                   
         BE    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     *+10                YEAR NOT FOUND                               
*                                                                               
         MVC   0(3,RF),SPACES      ELIMINATE YEAR                               
*                                                                               
         OI    CRCFISSH+6,X'80'    FORCE RE-DISPLAY OF FIELD                    
*                                                                               
         MVI   ERROR,PWEISSYI                                                   
         LA    R2,CRCFISSH         CURSOR AT FIRST ISSUE DATE                   
         B     VRFISERR                                                         
*                                                                               
VRFISERR DS    0H                                                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'PRSFM10 - CIRCULATION POSTING - LNTBL'                          
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNTBL    DS    0D                                                               
         DC    Y(CRCISS1H-T41CFFD)  ISSUE 1                                     
         DC    Y(CRCISS2H-T41CFFD)  ISSUE 2                                     
         DC    Y(CRCISS3H-T41CFFD)  ISSUE 3                                     
         DC    Y(CRCISS4H-T41CFFD)  ISSUE 4                                     
         DC    Y(CRCISS5H-T41CFFD)  ISSUE 5                                     
         DC    Y(CRCISS6H-T41CFFD)  ISSUE 6                                     
         DC    Y(CRCISS7H-T41CFFD)  ISSUE 7                                     
         DC    Y(CRCISS8H-T41CFFD)  ISSUE 8                                     
         DC    Y(CRCISS9H-T41CFFD)  ISSUE 9                                     
         DC    Y(CRCISSAH-T41CFFD)  ISSUE 10                                    
         DC    Y(CRCISSBH-T41CFFD)  ISSUE 11                                    
LNTBLLS  DC    Y(CRCISSLH-T41CFFD)  ISSUE LAST                                  
         DC    4X'FF'               END OF TABLE                                
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'T41C10 CIRCULATION RECORDS - FREQUENCY VAL - VRFRQ'             
***********************************************************************         
*                                                                     *         
*        VALIDATE FREQUENCY ON ADDS                                   *         
*                                                                     *         
*        ADD  - IF A CLIENT OVERRIDE RECORD,                          *         
*                  MUST MATCH FREQUENCY OF MASTER CIRC RECORD         *         
*               IF MASTER CIRC RECORD                                 *         
*                  MUST USE ISSUE RECORD IF IT EXISTS                 *         
*                  ELSE REQUIRED INPUT                                *         
*               ANY CHANGE FORCES A RE-DISPLAY                        *         
*                                                                     *         
*        DIS  - FREQUENCY FORCED TO THAT OF RECORD TO BE DISPLAYED    *         
*        CHA                                                          *         
*        DEL  - FREQUENCY FORCED TO THAT OF RECORD TO BE DELETED      *         
*        RES                                                          *         
*                                                                     *         
*        ORDER OF PRIORITY -                                          *         
*        FREQUENCY IN RECORD TO BE DISPLAYED                          *         
*        ISSUE RECORD                                                 *         
*        ENTERED FREQUENCY - TAKES AFFECT ONLY ON ADDS OR EMPTY       *         
*                            CIRC RECORDS AND NO ISSUE RECORDS        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
VRFRQ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CRCFRQH          POINT TO ENTERED FREQUENCY                   
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   08(R2),C'+'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   08(R2),C'?'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   09(R2),C'?'         IF HELP REQUESTED                            
         BNE   VRFQHLPX                                                         
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
*                                                                               
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D1'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN SECOND TWA PAGE                      
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
*                                                                               
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVI   HCBSEGS,10          PRVAL TAB AREA IS 10*256 LONG                
         L     RF,=A(ELTAB-(CONHEADH-64))                                       
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBATAB          SET A(PRVAL TABLE)                           
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMCFR),(R2),HELPCBLK PUT OUT HELP           
*                                                                               
         DROP  R6                                                               
*                                                                               
VRFQHLPX DS    0H                                                               
*                                                                               
*        FIND DEFAULT FREQUENCY                                                 
*                                                                               
VRFRQDF  DS    0H                                                               
*                                                                               
         XC    WRKFRQ,WRKFRQ       INIT FREQUENCY                               
         XC    DFLTFRQ,DFLTFRQ     INIT DEFAULT FREQUENCY                       
         XC    HDRFRQ,HDRFRQ       INIT HEADER  FREQUENCY                       
*                                                                               
         MVC   CRCFRQN,SPACES      INIT FREQUENCY DESCRIPTION                   
         OI    CRCFRQNH+6,X'80'    FORCE RE-DISPLAY OF FIELD                    
         OI    CRCFRQH+6,X'80'     FORCE RE-DISPLAY OF FIELD                    
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADD                                   
         BE    VRFRQHDX                                                         
*                                                                               
*        USE FREQUENCY IN CIRCULATION RECORD                                    
*                                                                               
         L     RF,AIO1                USE FREQUENCY IN CIRC RECORD              
         CLC   ORIGKEY(L'PCRCKEY),0(RF)  SKIP IF INCORRECT REC IN CORE          
         BNE   VRFRQCLN                                                         
*                                                                               
         SR    RE,RE                                                            
         LA    R6,PCRCELEM-PCRCRECD(RF) POINT TO 1ST ELEM IN RECORD             
         USING PCRHELM,R6          ESTABLISH AS HEADER ELEMENT                  
*                                                                               
         CLI   PCRHELM,0           DONE IF END OF RECORD                        
         BE    *+34                                                             
         CLI   PCRHELM,PCRHELQ     FIND CIRC HEADER ELEMENT                     
         BNE   *+14                                                             
         MVC   HDRFRQ,PCRHFRQ      SAVE FREQUENCY IN CIRCULATION RECORD         
         B     *+16                                                             
         IC    RE,PCRHLEN          ELEMENT LENGTH                               
         LA    R6,PCRHELM(RE)      BUMP TO NEXT ELEMENT                         
         B     *-34                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
VRFRQHDX DS    0H                                                               
*                                                                               
         OC    QCLT,QCLT           IF CLIENT OVERRIDE RECORD                    
         BZ    VRFRQCLN                                                         
*                                                                               
*        USE FREQUENCY FROM ALL CLIENT RECORD                                   
*                                                                               
         SR    RE,RE                                                            
         L     RF,AIO3                USE FREQUENCY IN CIRC RECORD              
         LA    R6,PCRCELEM-PCRCRECD(RF) POINT TO 1ST ELEM IN RECORD             
         USING PCRHELM,R6          ESTABLISH AS HEADER ELEMENT                  
*                                                                               
         CLI   PCRHELM,0           DONE IF END OF RECORD                        
         BE    *+34                                                             
         CLI   PCRHELM,PCRHELQ     FIND CIRC HEADER ELEMENT                     
         BNE   *+14                                                             
         MVC   DFLTFRQ,PCRHFRQ     USE FREQUENCY IN CIRCULATION RECORD          
         B     *+16                                                             
         IC    RE,PCRHLEN          ELEMENT LENGTH                               
         LA    R6,PCRHELM(RE)      BUMP TO NEXT ELEMENT                         
         B     *-34                                                             
*                                                                               
         B     VRFRQDFX                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
VRFRQCLN DS    0H                                                               
*                                                                               
*        MUST BE 'I' IF THERE IS AN ISSUE RECORD                                
*                                                                               
VRFRQIS  DS    0H                                                               
*                                                                               
         XC    WISSKEY,WISSKEY     INIT ISSUE KEY SAVEAREA                      
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY FOR ISSUE RECORD               
         LA    R5,KEY                                                           
         USING PISSREC,R5                                                       
*                                                                               
         MVC   PISSKAGY,AGENCY     SET AGENCY                                   
         MVC   PISSKMED,QMED       SET MEDIA                                    
         MVI   PISSKTYP,X'29'      SET RECORD TYPE                              
         MVC   PISSKPUB,BPUB       SET PUB                                      
         MVC   PISSKYR,WRKYR       SET YEAR                                     
*                                                                               
         GOTO1 HIGH                READ FOR RECORD                              
*                                                                               
         CLC   PISSKEY,KEYSAVE     IF ISSUE RECORD KEY FOUND                    
         BNE   *+10                                                             
         MVC   WISSKEY,PISSKEY        SAVE KEY                                  
*                                                                               
         OC    WISSKEY,WISSKEY     IF THERE IS AN ISSUE RECORD                  
         BZ    *+8                                                              
         MVI   DFLTFRQ,PCRHFRIQ       DEFAULT TO ISSUE RECORD                   
*                                                                               
VRFRQISX DS    0H                                                               
*                                                                               
VRFRQDFX DS    0H                                                               
*                                                                               
*        VALIDATE FIELD ENTRY                                                   
*                                                                               
VRFRQVL  DS    0H                                                               
*                                                                               
         CLI   5(R2),0             SKIP IF NOT ENTERED                          
         BE    VRFRQVLX                                                         
*                                                                               
         LH    R3,=Y(ELTAB-(CONHEAD-64))   GET A(TABLE BUILD AREA)              
         LA    R3,0(R3,RA)                                                      
*                                                                               
         GOTO1 VPRVAL,VLPARMS,('VLPVALQ',=Y(PRQCRFRQ)),                X        
               (5(R2),8(R2)),(R3),0,0,0                                         
         CLI   VLPERR,0                                                         
         BNE   VRFRQNV             INVALID FREQUENCY                            
*                                                                               
         USING VLTABD,R3           ESTABLISH RETURNED TABLE ENTRY               
*                                                                               
         MVC   WRKFRQ,VLTICODE+1   SAVE FREQUENCY                               
*                                                                               
VRFRQVLX DS    0H                                                               
*                                                                               
         CLI   DFLTFRQ,0           IF NO DEFAULT FREQUENCY                      
         BNE   *+10                                                             
         MVC   DFLTFRQ,HDRFRQ         USE HEADER  FREQUENCY                     
*                                                                               
         CLI   WRKFRQ,0            IF NO FREQUENCY ENTERED                      
         BNE   *+10                                                             
         MVC   WRKFRQ,HDRFRQ          USE HEADER  FREQUENCY                     
*                                                                               
         CLI   WRKFRQ,0            IF NO FREQUENCY ENTERED                      
         BNE   *+10                                                             
         MVC   WRKFRQ,DFLTFRQ         USE DEFAULT FREQUENCY                     
*                                                                               
         CLI   WRKFRQ,0            IF NO FREQUENCY SPECIFIED                    
         BNE   VRFRQ20                                                          
*                                                                               
         CLI   MODE,DISPREC        SKIP IF DISPLAYING RECORD                    
         BE    VRFRQ19                                                          
*                                                                               
         CLI   ACTNUM,ACTADD          ERROR IF ADD                              
         BE    *+8                                                              
         CLI   ACTNUM,ACTCHA          ERROR IF CHANGE                           
         BE    VRFRQMIS                                                         
*                                  ELSE                                         
VRFRQ19  DS    0H                                                               
*                                                                               
         MVI   NEWKEY,C'Y'            FORCE RE-DISPLAY                          
*                                                                               
         B     VRFRQX                                                           
*                                                                               
VRFRQ20  DS    0H                                                               
*                                                                               
         CLI   HDRFRQ,0            SKIP IF NO HEADER FREQUENCY                  
         BE    VRFRQ30                                                          
*                                                                               
         CLC   WRKFRQ,HDRFRQ       SKIP IF HEADER FREQUENCY BEING USED          
         BE    VRFRQ30                                                          
*                                                                               
         CLI   MODE,DISPREC        SKIP IF DISPLAYING RECORD                    
         BE    VRFRQ30                                                          
*                                                                               
*        CAN ONLY CHANGE FREQUENCY IF NO DETAILS HAVE BEEN ENTERED              
*                                                                               
         MVI   ELCODE,PCRCELQ      SET TO FIND CIRC ELEMENTS                    
         L     R6,AIO1             POINT TO CIRC RECORD                         
*                                                                               
         BAS   RE,GETEL                                                         
*                                                                               
VFRQDTLL DS    0H                                                               
*                                                                               
         BNE   VFRQDTLD            NO MORE DETAIL ELEMENTS                      
*                                                                               
         USING PCRCELM,R6          ESTABLISH CIRCULATION DETAILS                
*                                                                               
         OC    PCRCTOT(20),PCRCTOT ERROR IF DETAILS EXIST                       
         BNZ   VRFRQCHE                                                         
*                                                                               
VFRQDTLC DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     VFRQDTLL                                                         
*                                                                               
VFRQDTLD DS    0H                                                               
*                                                                               
         GOTO1 REMELEM             CLEAR OUT OLD DETAIL ELEMENTS                
*                                                                               
         CLC   WRKFRQ,DFLTFRQ      SKIP IF USING DEFAULT FREQUENCY              
         BE    VRFRQ30                                                          
*                                                                               
         CLI   DFLTFRQ,PCRHFRIQ    ERROR IF DEFAULT IS ISSUE RECORD             
         BE    VRFRQISE                                                         
*                                                                               
VRFRQ30  DS    0H                                                               
*                                                                               
*        TRANSLATE FREQUENCY CODE                                               
*                                                                               
         CLC   SAVFRQ,WRKFRQ       IF FREQUENCY CHANGED                         
         BE    *+14                                                             
         MVI   NEWKEY,C'Y'            FORCE RE-DISPLAY                          
         MVC   SAVFRQ,WRKFRQ          SAVE FREQUENCY                            
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),WRKFRQ                                                 
*                                                                               
         LH    R3,=Y(ELTAB-(CONHEAD-64))   GET A(TABLE BUILD AREA)              
         LA    R3,0(R3,RA)                                                      
*                                                                               
         GOTO1 VPRVAL,VLPARMS,('VLPTRAQ',=Y(PRQCRFRQ)),                X        
               HALF,(R3),0,0,0                                                  
         CLI   VLPERR,0                                                         
         BNE   VRFRQNV             INVALID SOURCE                               
*                                                                               
         USING VLTABD,R3           ESTABLISH RETURNED TABLE ENTRY               
*                                                                               
         MVC   WRKFRQ,VLTICODE+1   SAVE FREQUENCY                               
*                                                                               
         MVC   WDTPRMS,VLTEXTRA    SAVE DATE CALCULATION PARAMETERS             
*                                                                               
         CLI   WRKFRQ,PRQCFISS     IF FREQ IS ISSUE                             
         BNE   *+14                                                             
         OC    WISSKEY,WISSKEY        THERE MUST BE AN ISSUE RECORD             
         BZ    VRFRQIRE                                                         
*                                  THEN MUST HAVE ISSUE RECORD                  
         XC    CRCFRQ,CRCFRQ       INIT FREQUENCY CODE                          
         MVC   CRCFRQ(4),VLTSHORT  DISPLAY FREQUENCY CODE                       
         OI    CRCFRQH+6,X'80'     FORCE RE-DISPLAY OF FIELD                    
         MVC   CRCFRQN,VLTFULL     DISPLAY EXPLANATION                          
         OI    CRCFRQNH+6,X'80'    FORCE RE-DISPLAY OF FIELD                    
*                                                                               
VRFRQX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VRFRQMIS DS    0H                  FREQUENCY NOT SPECIFIED                      
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,CRCFRQH          CURSOR AT FREQUENCY                          
         B     VRERR                                                            
*                                                                               
VRFRQNV  DS    0H                  INVALID FREQUENCY                            
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CRCFRQH          CURSOR AT FREQUENCY                          
         B     VRERR                                                            
*                                                                               
VRFRQCHE DS    0H                  NO CHANGE IF DETAILS EXIST                   
*                                                                               
         MVI   ERROR,PWECFRQE                                                   
*                                                                               
         LA    R2,CRCISS1H            CURSOR TO FIRST OPEN FIELD                
*                                                                               
         TM    1(R2),X'20'                                                      
         BNO   *+8                                                              
         BAS   RE,BUMPU               BUMP TO NEXT UNPROTECTED FIELD            
*                                                                               
         XC    CRCFRQ,CRCFRQ                                                    
         MVC   CRCFRQ(1),HDRFRQ    RETURN TO HEADER FREQUENCY                   
*                                                                               
         FOUT  CRCFRQH             RE-DISPLAY                                   
*                                                                               
         B     VRERR                                                            
*                                                                               
VRFRQIRE DS    0H                  ISSUE RECORD NOT FOUND                       
*                                                                               
         MVI   ERROR,PWEISSNO                                                   
         LA    R2,CRCSRCH          CURSOR AT SOURCE                             
         B     VRERR                                                            
*                                                                               
VRFRQISE DS    0H                  MUST USE ISSUE RECORD                        
*                                                                               
         MVI   ERROR,PWEISSYS                                                   
         LA    R2,CRCFRQH          CURSOR AT FREQUENCY                          
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD0D                                                       
*                                                                               
         DS    0D                                                               
SAVER0   DS    F                   REGISTER R0 SAVEAREA                         
SAVKEY   DS    XL(L'KEY)           KEY FROM LAST TIME                           
SAVFRQ   DS    XL1                 FREQUENCY FROM LAST TIME                     
SAVFISS  DS    XL3                 STARTING ISSUE FROM LAST TIME                
WRKFISSV DS    XL3                 START ISSUE DATE SAVEAREA                    
*                                                                               
LSVTAB   DS    XL(NLINS*LSVTABL)                                                
         DS    0D                                                               
*                                                                               
         ORG   CONHEAD-64+X'1800'                                               
HELPSAVE DS    XL512               HELP CONTROL BLOCK                           
ELTMAX   EQU   100                                                              
         DS    0D                                                               
*                                                                               
ELTAB    DS    XL(ELTABL*ELTMAX)      TABLE FOR SPACE/RATE ELEMS                
*                                                                               
         DS    0D                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
WRKYR    DS    CL4                 YEAR   FOR CIRCULATION                       
WRKSRC   DS    CL3                 SOURCE FOR CIRCULATION                       
*                                                                               
WRKISS   DS    0XL3                ISSUE DATE                                   
WRKISSYR DS    XL1                 ISSUE YEAR                                   
WRKISSMN DS    XL1                 ISSUE MONTH                                  
WRKISSDY DS    XL1                 ISSUE DAY                                    
*                                                                               
WRKFISS  DS    0XL3                FIRST ISSUE DATE                             
WRKFISSY DS    XL1                 FIRST ISSUE YEAR                             
WRKFISSM DS    XL1                 FIRST ISSUE MONTH                            
WRKFISSD DS    XL1                 FIRST ISSUE DAY                              
*                                                                               
HDRFISS  DS    XL3                 HEADER  FIRST ISSUE DATE                     
DFLTFISS DS    XL3                 DEFAULT FIRST ISSUE DATE                     
*                                                                               
WRKSISS  DS    0XL3                PAGE STARTING ISSUE DATE                     
WRKSISSY DS    XL1                 PAGE STARTING ISSUE YEAR                     
WRKSISSM DS    XL1                 PAGE STARTING ISSUE MONTH                    
WRKSISSD DS    XL1                 PAGE STARTING ISSUE DAY                      
*                                                                               
W1STDTE  DS    XL3                 1ST FORMULA DATE SAVEAREA                    
*                                                                               
WNXTISS  DS    0XL3                NEXT ISSUE DATE                              
WNXTISSY DS    XL1                 YEAR                                         
WNXTISSM DS    XL1                 MONTH                                        
WNXTISSD DS    XL1                 DAY                                          
*                                                                               
WSTRTDT  DS    0XL3                STARTING ISSUE DATE                          
WSTRTDTY DS    XL1                 YEAR                                         
WSTRTDTM DS    XL1                 MONTH                                        
WSTRTDTD DS    XL1                 DAY                                          
*                                                                               
WRKDAT1C DS    CL6                 WORK DATE YYMMDD                             
WRKDAT2C DS    CL6                 WORK DATE YYMMDD                             
WRKDATEC DS    CL6                 DATE WORKAREA                                
*                                                                               
WISSSW   DS    XL1                 DATE ROUTINE SWITCH                          
WDTPRMS  DS    0XL7                DATE CALCULATION PARAMETERS                  
WCNTL    DS    XL1                 CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  C'40' - REQUIRES STARTING ISSUE DATE         
WINC1Y   DS    AL1                 YEARS  TO NEXT DATE                          
WINC1M   DS    AL1                 MONTHS TO NEXT DATE                          
WINC1D   DS    AL1                 DAYS   TO NEXT DATE                          
WINC2Y   DS    AL1                 YEARS  TO SECOND DATE                        
WINC2M   DS    AL1                 MONTHS TO SECOND DATE                        
WINC2D   DS    AL1                 DAYS   TO SECOND DATE                        
*                                                                               
WRKFRQ   DS    XL1                 WORKING FREQUENCY CODE                       
HDRFRQ   DS    XL1                 WORKING HEADER  FREQUENCY CODE               
DFLTFRQ  DS    XL1                 WORKING DEFAULT FREQUENCY CODE               
WISSKEY  DS    XL(L'PISSKEY)       ISSUE RECORD KEY SAVEAREA                    
         DS    0F                                                               
*                                                                               
LASTLIN  DS    XL1                 NUMBER OF LAST USED LINE                     
CURLIN   DS    XL1                 NUMBER OF CURRENT   LINE                     
LINCT    DS    PL2                                                              
ALINCUR  DS    A                   A(LINE WITH CURSOR)                          
*                                                                               
         DS    0F                                                               
       ++INCLUDE PRVALPARMS                                                     
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
HELPCBLK DS    XL(HELPCBL)         HELP CONTROL BLOCK                           
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
         DS    0D                                                               
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL15                PUBLICATION NUMBER ZONE/EDT                  
         DS    CL1                                                              
LPUBN    DS    CL20                PUB NAME                                     
         DS    CL3                                                              
LSRC     DS    CL3                 SOURCE                                       
         DS    CL2                                                              
LYR      DS    CL4                 YEAR                                         
         DS    CL4                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL3                                                              
LISS     DS    CL5                                                              
         DS    CL2                                                              
LTOT     DS    CL11                TOTAL CIRCULATION                            
         DS    CL2                                                              
LRTB     DS    CL11                RATE BASE CIRCULATION                        
         DS    CL2                                                              
LPLIM    DS    CL11                PRELIMNARY AUDITTED CIRCULATION              
         DS    CL2                                                              
LFIN     DS    CL11                FINAL AUDITTED CIRCULATION                   
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*PPSRCHPARM                                                                     
*PRGENFILE                                                                      
*PRGLOBEQUS                                                                     
*DDGLOBEQUS                                                                     
*DDGLVXCTLD                                                                     
*DDLINUPD                                                                       
*DDPERVALD                                                                      
*PRHELPCB                                                                       
*PRVALTABD                                                                      
*FAGETTXTD                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE PRGLOBEQUS                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDLINUPD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PRHELPCB                                                       
       ++INCLUDE PRVALTABD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL(L'PCRCISS)                                                    
LSVKEYL  EQU   *-LSVTABD                                                        
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL(L'PCRCISS)       SORT VALUE                                   
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(PCRCELLN)        ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PRSFM10   07/17/02'                                      
         END                                                                    
