*          DATA SET PRSFM18    AT LEVEL 116 AS OF 07/17/02                      
*PHASE T41C18A,*                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE BINSRCH2                                                               
*        TITLE 'T41C18 - PUB LIST RECORDS'                                      
         TITLE 'T41C18 - PUB LIST RECORDS - CHANGE LOG'                         
***********************************************************************         
*                                                                     *         
*        CHANGE LOG                                                   *         
*                                                                               
* SMYE   07/02   IN VK IF REPORTING AND LIMIT ACCESS ACTIVE, REQUIRE            
*                CLIENT CODE ENTRY                                              
*                                                                               
* SMYE   OCT/00     ADDED "NO OUTDOOR BUYING" MESSAGE TO DISPLAY                
*        11/03/00   ADDED ABOVE MSG TO SCREEN - COMMENTED OUT (*NOP*)           
*                     THE MOVES, ETC. IN THIS PROGRAM                           
*                                                                               
* SMYE   MAR/98     DISALLOWED MODE RECDEL (RDEL LOGIC LEFT INTACT)             
*                                                                               
* SMYE   JUN/97     SHIFT POSITION OF CLIENT AND PUBLIST ON SCREENS             
*                   AND REWRITE THE REPORT                                      
*                                                                               
***********************************************************************         
         SPACE 2                                                                
*                                                                               
*BOBY    JUL01/96   CREATION                                                    
*                                                                               
         TITLE 'T41C18 - PUB LIST RECORDS - INIT'                               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41C18   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T41C18,RR=R3                                                   
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKING STORAGE             
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKING STORAGE             
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOLER WORKING STORAGE            
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*                                                                               
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IOOPT,C'Y'          WE DO ALL I/O                                
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         OI    GENSTAT4,NODELLST   STOP DELETING FROM LIST SCREEN               
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
         MVI   NLISTS,(PLLSELLH-PLLSEL1H)/(PLLSEL2H-PLLSEL1H)+1                 
*                                  # OF LIST LINES                              
*                                                                               
*        BYPASS - NO FIELD USES HELP                                            
*                                                                               
         B     INIT10                                                           
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
*        INIT DDVAL BLOCK                                                       
*                                                                               
*        NOT NEEDED                                                             
*                                                                               
         B     INIT20                                                           
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
INIT20   DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   *+8                                                              
         MVI   PBLSCRLH+5,0           FORCE NO DATA IN SCROLL FIELD             
*                                                                               
         CLI   TWASCR,X'D8'        IF WE ARE WORKING ON MAINT SCREEN            
         BNE   INITFLDX                                                         
*                                                                               
         SR    R4,R4               INIT FIELD COUNTER                           
         LA    R2,PBLPUB1H         POINT TO FIRST PUB FIELD                     
         LA    R3,PBLPUB2H         POINT TO SECOND PUB FIELD                    
*                                                                               
         LA    R4,1(R4)            BUMP FIELD COUNTER                           
         BAS   RE,BUMP             BUMP TO NEXT FIELD ON LINE                   
         CR    R2,R3               COTINUE IF STILL ON LINE                     
         BL    *-10                                                             
*                                                                               
         STC   R4,NFLDS            SAVE NUMBER OF LINES ON SCREEN               
*                                                                               
INITFLDX DS    0H                                                               
*                                                                               
         TITLE 'T41C18 - PUB LIST RECORDS - MODE'                               
***********************************************************************         
*                                                                     *         
*        DETERMINE GENCON CALLING MODE                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
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
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    RRES                                                             
         CLI   MODE,RECDEL         DELETE RECORD                                
*NOP*    BE    RDEL                                                             
         BNE   XIT                                                              
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         GOTO1 ERREX                                                            
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
         OI    PBLSCRLH+6,X'81'    TRANSMIT SCROLL AND MODIFY NEXT TIME         
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
         TITLE 'T41C18 PUB LIST RECORDS - VALKEY'                               
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
*        VALIDATE MEDIA - REQUIRED                                              
*                                                                               
         LA    R2,PBLMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKCLT    DS    0H                                                               
*                                                                               
         LA    R2,PBLCLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
         XC    QCLT,QCLT           SET TO DEFAULT                               
*                                                                               
         CLI   5(R2),0             NOT ENTERED IS THE SAME AS 'ALL'             
         BE    *+10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    *+10                                                             
         CLC   =C'ZZZ',8(R2)       OR 'ZZZ'                                     
         BNE   VKCLT1                                                           
*                                                                               
         MVC   QCLT,=C'ZZZ'        ALL CLIENTS REPRESENTED BY ZZZ               
         MVC   8(3,R2),=C'ZZZ'                                                  
         OI    6(R2),X'80'         FORCE RE-TRANSMISSION                        
         MVC   CLTNM,=CL20'ALL CLIENTS'                                         
*                                                                               
         B     VKCLT2                                                           
*                                                                               
VKCLT1   DS    0H                                                               
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
VKCLT2   MVC   PBLCLTN,CLTNM       DISPLAY CLT NAME                             
         OI    PBLCLTNH+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
*        BUILD PUB LIST KEY                                                     
*                                                                               
VKPBL    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R5,KEY                                                           
         USING PLISRECD,R5                                                      
*                                                                               
         MVC   PLISKAGY,AGENCY     AGENCY                                       
         MVC   PLISKMED,QMED       MEDIA                                        
         MVI   PLISKRCD,PLISTIDQ   RECORD ID                                    
         MVC   PLISKCLT,QCLT       CLIENT                                       
***********************************************************************         
* (10/19/00) OUTPUT MESSAGE THAT OUTDOOR PUBLIST BUYING NOT AVAILABLE *         
***********************************************************************         
*NOP*    MVC   PBLTEMP,SPACES                                                   
*NOP*    CLI   PLISKMED,C'O'       OUTDOOR ?                                    
*NOP*    BNE   *+10                NO                                           
*NOP*    MVC   PBLTEMP(L'TEMPMSG),TEMPMSG                                       
*NOP*    OI    PBLTEMPH+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
         LA    R2,PBLPBLCH         POINT TO ENTERED CODE                        
         MVC   WRKCOD,SPACES       INIT TO SPACES                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BZ    VKNONE              REQUIRED FIELD                               
*                                                                               
         CH    RF,=H'3'                                                         
         BH    VKNOTV                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKCOD(0),8(R2)     SAVE INPUT KEY                               
*                                                                               
         MVC   PLISKCOD,WRKCOD     PUT PUB LIST CODE IN KEY                     
*                                                                               
         MVI   PLISKLIN,1          SET FOR FIRST RECORD IN SET                  
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
VKRECX   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            USE IOA1 FOR INPUT                           
*                                                                               
*        READ IN DETAIL RECORD IF NEEDED                                        
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                   READ FOR RECORD                           
*                                                                               
         CLC   PLISKEY,KEYSAVE           MUST FIND IT                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE               RESTORE FILE POINTER                   
         B     VKDISX                                                           
*                                                                               
         CLI   ACTNUM,ACTREST      IF ACTION IS RESTORE                         
         BNE   VKRESN                                                           
*                                                                               
         TM    PLISKEY+25,X'80'       RECORD MUST BE DELETED                    
         BO    VKDELX                                                           
*                                                                               
         MVI   ERROR,59               SET ERROR MESSAGE                         
         B     VKX                                                              
*                                                                               
VKRESN   DS    0H                                                               
*                                                                               
         TM    PLISKEY+25,X'80'    MASTER RECORD CAN'T BE DELETED               
         BNO   *+12                                                             
         MVI   ERROR,56                                                         
         B     VKX                                                              
*                                                                               
VKDELX   DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 GETREC                 READ IN PUBLIST RECORD                    
*                                                                               
VKPBLX   DS    0H                                                               
*                                                                               
         L     R5,AIO1             POINT TO FOUND RECORD                        
*                                                                               
         CLC   TWAKEYSV(L'PLISKEY),ORIGKEY    CHECK FOR NEWKEY                  
         BE    VKDISX                                                           
*                                                                               
         MVI   NEWKEY,C'Y'                                                      
*                                                                               
         BAS   RE,DR1                 DISPLAY NEW SCREEN                        
*                                                                               
VKDISX   DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RE-SET KEY                                   
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS NOT ADD                         
         BNE   *+16                                                             
         MVI   NEWKEY,C'Y'            INDICATE NEW RECORD                       
         OI    IPSTAT,LUSNPVQ         SOME FIELD WAS UNVALIDATED                
         B     VKADDX                                                           
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                   RESTORE FILE POINTERS                     
*                                                                               
         CLC   PLISKEY,KEYSAVE        MUST FIND IT                              
         BE    *+10                                                             
         MVC   KEY,KEYSAVE            RESTORE FILE POINTER                      
*                                                                               
VKADDX   DS    0H                                                               
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
*NOP*    DC    C'*NOTE* OUTDOOR PUBLIST BUYING NOT AVAILABLE AT THIS TIX        
               ME'            LABEL WAS TEMPMSG (11/03/00 - *NOP*)              
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VKNOTV   DS    0H                                                               
*                                                                               
         MVI   ERROR,INVALID       PUB CODE NOT VALID                           
*                                                                               
         B     VKERR                                                            
*                                                                               
VKNONE   DS    0H                                                               
*                                                                               
         MVI   ERROR,MISSING       PUB CODE REQUIRED                            
*                                                                               
VKERR    DS    0H                  VALKEY ERROR EXIT                            
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - VKL'                                  
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
         MVC   ORIGKEY,KEY         SAVE ORIGINAL KEY                            
*                                                                               
VKLMED   DS    0H                                                               
*                                                                               
         LA    R2,PBLMEDH          MEDIA - REQUIRED                             
         GOTO1 VALIMED                                                          
*                                                                               
VKLMEDX  DS    0H                                                               
*                                                                               
*        SAVE ANY START CODE ENTERED                                            
*                                                                               
         LA    R2,PLLPBLCH         POINT TO ENTERED CODE                        
         MVC   WRKCOD,SPACES       INIT TO SPACES                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BZ    VKLCODX             REQUIRED FIELD                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKCOD(0),8(R2)     SAVE INPUT KEY                               
*                                                                               
VKLCODX  DS    0H                                                               
*                                                                               
VKLCLT   DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           FOR LISTING CLEAR QCLT                       
         XC    PLLCLTN,PLLCLTN                                                  
         OI    PLLCLTNH+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
         LA    R2,PLLCLTH          CLIENT                                       
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKLCLTD                                                          
*                                                                               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS ?                           
         BZ    VKLCLTX             NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV ?                           
         BE    VKLCLTX             NO                                           
         MVI   ERROR,NEEDCLT       SECURITY - CLIENT REQUIRED                   
         B     VKERR                                                            
*                                                                               
VKLCLTD  CLC   =C'ZZZ',8(R2)                                                    
         BE    *+10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VKLCLTR                                                          
*                                                                               
VKLCLTM  MVC   QCLT,=C'ZZZ'                                                     
         MVC   CLTNM,=CL20'ALL CLIENTS'                                         
         B     VKLCLTV                                                          
*                                                                               
VKLCLTR  GOTO1 VALICLT                                                          
*                                                                               
VKLCLTV  DS    0H                                                               
*                                                                               
         MVC   PLLCLTN,CLTNM                                                    
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RE-SET KEY                                   
*                                                                               
VKLX     DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R5                                                               
*                                                                               
NEEDCLT  EQU   85             SPECIFIC CLIENT ENTRY REQUIRED (SECURITY)         
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - DISKEY'                               
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DK       DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         USING PLISRECD,R6                                                      
*                                                                               
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY FROM START OF LIST          
*                                                                               
DKMED    DS    0H                                                               
*                                                                               
         MVC   PBLMED(L'PLISKMED),PLISKMED     MEDIA                            
         OI    PBLMEDH+6,X'80'     TRANSMIT                                     
         MVI   PBLMEDH+5,L'PLISKMED    SET FIELD LENGTH                         
***********************************************************************         
* (10/19/00) OUTPUT MESSAGE THAT OUTDOOR PUBLIST BUYING NOT AVAILABLE *         
***********************************************************************         
*NOP*    MVC   PBLTEMP,SPACES                                                   
*NOP*    CLI   PLISKMED,C'O'       OUTDOOR ?                                    
*NOP*    BNE   *+10                NO                                           
*NOP*    MVC   PBLTEMP(L'TEMPMSG),TEMPMSG                                       
*NOP*    OI    PBLTEMPH+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
DKMEDX   DS    0H                                                               
*                                                                               
*        DISPLAY PUBLIST CODE                                                   
*                                                                               
DKCOD    DS    0H                  DISPLAY PUBLIST CODE                         
*                                                                               
         OI    PBLPBLCH+6,X'80'    FORCE TRANSMISSION                           
         MVI   PBLPBLCH+5,L'PLISKCOD   SET FIELD LENGTH                         
         MVC   PBLPBLC(L'PLISKCOD),PLISKCOD     PUBLIST CODE                    
*                                                                               
DKCODX   DS    0H                                                               
*                                                                               
DKCLT    DS    0H                  DISPLAY CLIENT CODE                          
*                                                                               
         OI    PBLCLTH+6,X'80'     FORCE TRANSMISSION                           
         MVC   PBLCLT(L'PLISKCLT),PLISKCLT     CLIENT                           
         MVI   PBLCLTH+5,L'PLISKCLT   SET FIELD LENGTH                          
*                                                                               
DKCLTX   DS    0H                                                               
*                                                                               
         B     VK                  VALIDATE THE KEY                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - VALREC'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       DS    0H                                                               
*                                                                               
*        VALIDATE LIST NAME                                                     
*                                                                               
         LA    R2,PBLPBLNH         POINT TO DESCRIPTION FIELD                   
*                                                                               
         TM    4(R2),X'80'         IF INPUT THIS TIME                           
         BNO   *+8                                                              
         OI    IPSTAT,LUSNPVQ         INDICATE NOT PREVIOUSLY VALIDATED         
*                                                                               
         TM    4(R2),X'20'         SKIP IF PREVIOULY VALIDATED                  
         BO    *+8                                                              
         OI    IPSTAT,LUSNPVQ         INDICATE NOT PREVIOUSLY VALIDATED         
*                                                                               
*        NO RESTRICTIONS ON NAME                                                
*                                                                               
         OI    4(R2),X'20'         SET AS PREVIOULY VALIDATED                   
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
         MVC   ORIGKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         GOTO1 =A(LINSET),RR=RELO  LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE KEY                                  
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - DISREC'                               
***********************************************************************         
*                                                                     *         
*        DISPLAY RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DR1      NTR1                      SPECIAL INTERAL ENTRY                        
*                                                                               
DR       DS    0H                                                               
*                                                                               
*        DISPLAY PUBLIST DESCRIPTION                                            
*                                                                               
         MVI   ELCODE,PLISDTEQ     LOOKING FOR DATE ELEMENT                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
*                                                                               
         USING PLISDTED,R6         ESTABLISH PUB LIST DATE ELEMENT              
*                                                                               
         MVC   PBLPBLN(L'PLISDESC),PLISDESC   DISPLAY LIST DESCRIPTION          
         OI    PBLPBLNH+6,X'80'    FORCE TRANSMISSION                           
         OI    PBLPBLNH+4,X'20'    INDICATE PREVIOUSLY VALIDATED                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PLISNPBS       NUMBER OF PUBS IN LIST                       
         EDIT  (2,PLISNPBS),(L'PBLTPUB,PBLTPUB),ALIGN=LEFT  DISPLAY             
         OI    PBLTPUBH+6,X'80'    FORCE TRANSMISSION                           
         OI    PBLTPUBH+4,X'20'    INDICATE PREVIOUSLY VALIDATED                
*                                                                               
         MVC   ORIGKEY,KEY         SAVE CURRENT KEY                             
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
         MVC   KEY,ORIGKEY         RESTORE KEY                                  
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - RDEL'                                 
***********************************************************************         
*                                                                     *         
*        DELETE RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDEL     DS    0H                                                               
*                                                                               
         LA    R5,KEY              ESTABLISH PUBLIST KEY                        
         USING PLISKEY,R5                                                       
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ MASTER RECORD                           
*                                                                               
RDELLP   DS    0H                                                               
*                                                                               
         CLC   PLISKEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF CODE CHANGES          
         BNE   RDELDN                                                           
*                                                                               
         TM    PLISKEY+25,X'80'    RECORD MUST NOT BE DELETED                   
         BNO   RDELLP10                                                         
*                                                                               
         MVI   ERROR,RECISDEL                                                   
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
RDELLP10 DS    0H                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
         OI    PLISCTL-PLISKEY(R6),X'80'   FLAG AS DELETED                      
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         OI    PLISKEY+25,X'80'    FLAG POINTER AS DELETED                      
*                                                                               
         GOTO1 WRITE               DELETE POINTER                               
*                                                                               
RDELCN   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ FOR NEXT RECORD IN SET                  
         B     RDELLP                                                           
*                                                                               
RDELDN   DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'   TURN OFF READING DELETED RECORDS           
*                                                                               
RDELX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - RRES'                                 
***********************************************************************         
*                                                                     *         
*        RESTORE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RRES     DS    0H                                                               
*                                                                               
         LA    R5,KEY              ESTABLISH PUBLIST KEY                        
         USING PLISKEY,R5                                                       
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ MASTER RECORD                           
*                                                                               
RRESLP   DS    0H                                                               
*                                                                               
         CLC   PLISKEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF CODE CHANGES          
         BNE   RRESDN                                                           
*                                                                               
         TM    PLISKEY+25,X'80'    RECORD MUST BE DELETED                       
         BO    RRESLP10                                                         
*                                                                               
         MVI   ERROR,59            RECORD IS NOT DELETED                        
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
RRESLP10 DS    0H                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
         NI    PLISCTL-PLISKEY(R6),X'FF'-X'80'   UNDELETE                       
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         NI    PLISKEY+25,X'FF'-X'80'    UNDELETE                               
*                                                                               
         GOTO1 WRITE               RESTORE POINTER                              
*                                                                               
RRESCN   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ FOR NEXT RECORD IN SET                  
         B     RRESLP                                                           
*                                                                               
RRESDN   DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'   TURN OFF READING DELETED RECORDS           
*                                                                               
RRESX    DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE MASTER KEY                           
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         B     DR                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - LR'                                   
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
         B     XIT                                                              
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - PR'                                   
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R5,KEY              ESTABLISH PUB LIST RECORD KEY                
         USING PLISRECD,R5                                                      
*                                                                               
*        FIRST TIME                                                             
*                                                                               
         MVC   PLISKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PLISKMED,QMED                    MEDIA CODE                      
         MVI   PLISKRCD,PLISTIDQ                TYPE                            
*                                                                               
         CLI   PLLCLTH+5,0                                                      
         BE    *+16                                                             
         MVC   PLISKCLT,QCLT                    CLIENT                          
         OC    PLISKCLT,SPACES                                                  
*                                                                               
         CLI   PLLPBLCH+5,0                                                     
         BE    *+10                                                             
         MVC   PLISKCOD,WRKCOD                  PUBLIST CODE                    
*                                                                               
PR010    GOTO1 HIGH                READ FIRST OF TYPE                           
*                                                                               
PRLOOP   DS    0H                                                               
*                                                                               
         LA    R5,KEY              ESTABLISH PUB LIST RECORD KEY                
         USING PLISRECD,R5                                                      
*                                                                               
         CLC   PLISKEY(PLISKCLT-PLISKEY),KEYSAVE DONE IF CHG IN REC ID          
         BNE   PREND                                                            
*                                                                               
         CLI   PLLCLTH+5,0         CLIENT GIVEN ?                               
         BE    PRLOOPD             NO                                           
         CLC   PLISKEY(PLISKCOD-PLISKEY),KEYSAVE   DONE IF CHG IN CLT           
         BNE   PREND                                                            
*                                                                               
         CLI   PLLPBLCH+5,0        LIST CODE GIVEN ?                            
         BE    PRLOOPD             NO                                           
         CLC   PLISKEY(PLISKLIN-PLISKEY),KEYSAVE   DONE IF CHG IN LIST          
         BNE   PREND                                                            
*                                                                               
PRLOOPD  GOTO1 GETREC                GET PUBLIST RECORD                         
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLI   PLISKLIN,1          SKIP IF NOT FIRST RECORD OF SET              
         BNE   PR10                                                             
*                                                                               
         MVC   PPBLC,PLISKCOD      SAVE PUBLIST CODE FOR HEADER                 
*                                                                               
         MVC   PCLT,PLISKCLT       SAVE CLIENT CODE FOR HEADER                  
*                                                                               
         LR    R6,R5                                                            
         MVI   ELCODE,PLISDTEQ     LOOK FOR DATE ELEMENT                        
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING PLISDTED,R6         ESTABLISH DATE ELEMENT                       
*                                                                               
         MVC   PPBLN,PLISDESC      SAVE PUBLIST DESCRIPTION FOR HDR             
*                                  SAVE NUMBER OF PUBS IN LIST FOR HDR          
         EDIT (2,PLISNPBS),(4,PNPBS),ALIGN=LEFT                                 
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
         XC    PCLTNM,PCLTNM                                                    
         MVC   PCLTNM(11),=C'ALL CLIENTS'                                       
         CLC   PCLT,=C'ZZZ'                                                     
         BE    PR10                                                             
*                               GET CLIENT NAME FOR HEADERS                     
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
         XC    KEY,KEY             BUILD CLIENT KEY                             
         LA    R3,KEY                                                           
         USING PCLTRECD,R3                                                      
*                                                                               
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,PCLTKIDQ   RECORD CODE                                  
         MVC   PCLTKCLT,PCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ CLIENT KEY                              
*                                                                               
         MVC   PCLTNM(20),=C'*CLIENT NOT ON FILE*'                              
         CLC   KEY(25),KEYSAVE     FOUND ?                                      
         BNE   PR05                NO                                           
         XC    PCLTNM,PCLTNM                                                    
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LR    R0,R6               SAVE ELEMENT REGISTER                        
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         MVI   ELCODE,X'02'        FIND CLIENT NAME ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PCLTELEM,R6         ESTABLISH CLIENT NAME ELEMENT                
*                                                                               
         MVC   PCLTNM,PCLTNAME     SAVE NAME                                    
*                                                                               
         LR    R6,R0               RESTORE ELEMENT POINTER                      
*                                                                               
PR05     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     FOUND ?                                      
         BE    *+6                                                              
         DC    H'0'                NO - VERY BAD                                
*                                                                               
PR10     DS    0H                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
*                                            CREATE PRINT LINES HERE            
*                                                                               
         MVI   ELCODE,PLISPBQ      FIND PUB ELEMENTS                            
         LR    R6,R5                                                            
         BAS   RE,GETEL            GET FIRST ELEMENT                            
*                                                                               
PRPRTLP  DS    0H                                                               
*                                                                               
         BNE   PRCONT              NO MORE ELEMENTS - NEXT RECORD               
*                                                                               
         USING PRNTD,R4            ESTABLISH PRINT LINE                         
         LA    R4,P1               USE P LINES                                  
         XC    P1(PRNTLEN),P1      CLEAR                                        
*                                                                               
         USING PLISPBEL,R6         ESTABLISH PUB ELEMENT                        
*                                                                               
         XC    KEY,KEY             BUILD PUB KEY                                
         LA    R3,KEY                                                           
         USING PUBRECD,R3                                                       
*                                                                               
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),PLISPUB  MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                  EXPAND PUB NUMBER                            
         GOTO1 =V(PUBEDIT),DMCB,(C'0',PUBKPUB),(0,PPUBNO),RR=RELO               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                READ PUB KEY                                 
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   PPUBNM(33),=C'***  PUBLICATION NOT ON FILE  ***'                 
         CLC   KEY(25),KEYSAVE     FOUND ?                                      
         BNE   PRPUBPT             NO                                           
         XC    PPUBNM(33),PPUBNM                                                
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         LR    R0,R6               SAVE ELEMENT REGISTER                        
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         MVI   ELCODE,X'10'        FIND PUB NAME ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PUBNAMEL,R6         ESTABLISH PUB NAME ELEMENT                   
*                                                                               
         MVC   PPUBNM,PUBNAME      PRINT NAME                                   
         MVC   PPUBCITY,PUBCITY    PRINT CITY                                   
         MVC   PPUBSTA,PUBSTATE    PRINT STATE                                  
         MVC   PPUBZNM,PUBZNAME    PRINT ZONE NAME                              
*                                                                               
         LR    R6,R0               RESTORE ELEMENT POINTER                      
         MVI   ELCODE,PLISPBQ      RESTORE ELEMENT ID                           
*                                                                               
         USING PLISPBEL,R6         ESTABLISH PUBLIST ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PLISPBEL+1       GET ELEMENT LENGTH                           
         CH    RF,=Y(PLISFILT-PLISPBEL)  IF FILTER IS PRESENT                   
         BNH   *+10                                                             
         MVC   PFILT,PLISFILT               PRINT FILTER                        
*                                                                               
PRPUBPT  DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT PUB ELEMENT                     
*                                                                               
         B     PRPRTLP                                                          
*                                                                               
PRCONT   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 READ NEXT RECORD                             
         B     PRLOOP                                                           
*                                                                               
PREND    DS    0H                  END OF REPORT                                
*                                                                               
         XC    KEY,KEY             CLEAR RECORD KEY                             
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4,R5,R6                                                      
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - LISTREC'                              
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
         LA    R5,KEY              ESTABLISH PUB LIST RECORD KEY                
         USING PLISRECD,R5                                                      
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR010               KEY IS LAST RECORD READ                      
*                                  SO GO RE-READ                                
*                                                                               
*        FIRST TIME                                                             
*                                                                               
         MVC   PLISKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PLISKMED,QMED                    MEDIA CODE                      
         MVI   PLISKRCD,PLISTIDQ                TYPE                            
*                                                                               
         CLI   PLLCLTH+5,0                                                      
         BE    *+16                                                             
         MVC   PLISKCLT,QCLT                    CLIENT                          
         OC    PLISKCLT,SPACES                                                  
*                                                                               
         CLI   PLLPBLCH+5,0                                                     
         BE    *+10                                                             
         MVC   PLISKCOD,WRKCOD                  PUBLIST CODE                    
*                                                                               
LR010    GOTO1 HIGH                READ FIRST OF TYPE                           
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         LA    R5,KEY              ESTABLISH PUB LIST RECORD KEY                
         USING PLISRECD,R5                                                      
*                                                                               
         CLC   PLISKEY(PLISKCLT-PLISKEY),KEYSAVE DONE IF CHG IN REC ID          
         BNE   LREND                                                            
*                                                                               
         CLI   PLISKLIN,1          SKIP IF NOT FIRST RECORD OF SET              
         BNE   LRCONT                                                           
*                                                                               
         OC    QCLT,QCLT          IF CLIENT GIVEN                               
         BZ    *+14                                                             
         CLC   PLISKCLT,QCLT         MUST MATCH KEY                             
         BNE   LRCONT                                                           
*                                                                               
         GOTO1 GETREC                GET PUBLIST RECORD                         
*                                                                               
         MVC   MYDSKADD,DMDSKADD     SAVE D/A FOR LIST                          
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
*                                                                               
         USING LISTD,R4            ESTABLISH LIST LINE                          
*                                                                               
         LA    R4,LISTAR           ELSE USE LIST AREA                           
*                                                                               
         MVC   LISTAR,SPACES       INIT LIST LINE                               
*                                                                               
         CLI   PLISKLIN,1          SKIP IF NOT FIRST RECORD IN SET              
         BNE   LRDISP                                                           
*                                                                               
         MVC   LPBLC,PLISKCOD      DISPLAY PUBLIST CODE                         
*                                                                               
         LR    R6,R5                                                            
         MVI   ELCODE,PLISDTEQ     LOOK FOR DATE ELEMENT                        
*                                                                               
         BAS   RE,GETEL                                                         
         BNZ   LRLPDTEX            NONE FOUND                                   
*                                                                               
         USING PLISDTED,R6         ESTABLISH DATE ELEMENT                       
*                                                                               
         MVC   LPBLN,PLISDESC      DISPLAY PUBLIST DESCRIPTION                  
*                                                                               
*****    SR    RF,RF                                                            
*****    ICM   RF,3,PLISNPBS       NUMBER OF PUBS IN LIST                       
         EDIT (2,PLISNPBS),(L'LNPBS,LNPBS),ALIGN=LEFT  DISPLAY                  
*                                                                               
LRLPDTEX DS    0H                                                               
*                                                                               
         MVC   LCLT,PLISKCLT       DISPLAY CLIENT CODE                          
*                                                                               
LRDISP   DS    0H                                                               
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
         LTORG                                                                  
*                                                                               
         DROP  R4,R5,R6                                                         
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - MYPUBVAL'                             
***********************************************************************         
*                                                                     *         
*        VALIDATE PUB                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MYPUBVAL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WRKPUB,WRKPUB       INIT PUB WORK AREA                           
         XC    PUBNM,PUBNM         INIT PUB NAME WORK AREA                      
         XC    WRKPUBN,WRKPUBN     INIT PUB NAME WORK AREA                      
         XC    WRKZNAM,WRKZNAM     INIT PUB ZONE NAME WORK AREA                 
         XC    WRKCITY,WRKCITY     INIT PUB CITY WORK AREA                      
         XC    WRKSTA,WRKSTA       INIT PUB STATE WORK AREA                     
*                                                                               
         CLI   5(R2),0             OKAY IF NOT ENTERED                          
         BE    MPVOK                                                            
*                                                                               
         CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   MPV10                                                            
*                                                                               
         SR    R2,RA               DISPLACEMENT INTO TWA                        
*                                                                               
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
*                                                                               
         MVC   DSMEDCOD,QMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
*                                                                               
         B     MPVX                                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
MPV10    DS    0H                                                               
*                                                                               
         ZIC   R0,5(R2)                                                         
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),WRKPUB   VALIDATE PUB                  
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BE    MPVINVPB                                                         
*                                                                               
*        READ IN PUB RECORD                                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,QMED        MEDIA                                        
         MVC   PUBKPUB(6),WRKPUB   MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY      AGENCY                                       
         MVI   PUBKCOD,X'81'       RECORD ID                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   MPVINVPB            MUST FIND PUB RECORD                         
*                                                                               
         L     R6,AIO2             READ INTO IOAREA2                            
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'        FIND NAME ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST                                   
*                                                                               
         USING PUBNAMEL,R6         ESTABLISH PUB NAME ELEMENT                   
*                                                                               
         MVC   PUBNM,PUBNAME       SAVE PUB NAME                                
         MVC   WRKPUBN,PUBNAME     SAVE PUB NAME                                
         MVC   WRKZNAM,PUBZNAME    SAVE PUB ZONE NAME                           
         MVC   WRKCITY,PUBCITY     SAVE PUB CITY                                
         MVC   WRKSTA,PUBSTATE     SAVE PUB STATE                               
*                                                                               
MPVOK    DS    0H                                                               
*                                                                               
MPVX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
MPVINVPB DS    0H                  INVALID PUB                                  
*                                                                               
         MVI   ERROR,INVPUB                                                     
*                                                                               
MPVERR   DS    0H                  ERROR EXIT                                   
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'T41C18 PUB LIST RECORDS - MYVPUB'                               
***********************************************************************         
*                                                                     *         
*        SET UP THE PUB NUMBER AND GET NAME                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MYVPUB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
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
         CLC   MYPUB+4(2),=X'FFFF'   ALL ZONES/EDTS                             
         BE    VPREST                                                           
         MVC   WRKZNAM,PUBZNAME                                                 
         MVC   WRKCITY,PUBCITY                                                  
         MVC   WRKSTA,PUBSTATE                                                  
*                                                                               
VPREST   MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         XIT1                                                                   
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
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
*        VALIDATE STARTING PUB NUMBER - POSITION IN LIST NOT ITS ID             
*                                                                               
LSSPUB   DS    0H                                                               
*                                                                               
         LA    R2,PBLSPUBH         POINT TO STARTING PUB FLD                    
*                                                                               
         XC    WRKSPUB,WRKSPUB     INIT STARTING PUB NUMBER                     
         MVI   WKNEWKEY,0          INIT WORK NEWKEY                             
*                                                                               
         TM    4(R2),X'80'         VALIDATE IF INPUT THIS TIME                  
         BO    LSSPUB1                                                          
*                                                                               
         CLI   5(R2),0             SKIP IF NO START PUB GIVEN                   
         BE    LSSPUBX                                                          
*                                                                               
         TM    4(R2),X'20'         VALIDATE IF NOT PREVIOUSLY VALIDATED         
         BO    LSSPUBX                                                          
*                                                                               
LSSPUB1  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BZ    LSSPUBOK                                                         
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
         C     RF,BSPNOR           MUST NOT EXCEED NUMBER IN LIST               
         BH    INVERR                                                           
*                                                                               
         MVC   WRKSPUB,DMCB+4      SAVE STARTING PUB                            
*                                                                               
LSSPUBOK DS    0H                                                               
*                                                                               
         MVI   WKNEWKEY,C'Y'       FORCE RE-DISPLAY ON A DISPLAY                
*                                                                               
         OI    PBLSPUBH+4,X'20'    INDICATE VALID FIELD                         
*                                                                               
LSSPUBX  DS    0H                                                               
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
         MVC   LUNFLDS,NFLDS           FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,PBLPUB1H         A(FIRST FIELD)                               
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
         CLI   PBLSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   PBLSCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   PBLSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   PBLSCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    PBLSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PBLSCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PBLSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
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
         CLI   LUAPMODE,LUAPVALQ   IF NOT VALIDATING                            
         BE    *+8                                                              
         CLI   LUNEW,C'Y'          AND NOT RE-DISPLAYING FROM SCRATCH           
         BE    *+10                                                             
         MVC   LUNEW,WKNEWKEY         USE WORK NEWKEY                           
*                                                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
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
         TM    IPSTAT,LUSNPVQ      UPDATE RECORD IF THERE WAS AT LEAST          
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
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         CLI   LUNEW,C'Y'          IF NOT RE-DISPLAYING FROM SCRATCH            
         BE    *+8                                                              
         MVC   LUNEW,WKNEWKEY         USE WORK NEWKEY                           
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
*        FIND POSITION IN LIST OF FIRST DIPLAYED PUB                            
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LS1STX              NO                                           
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R3)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
*                                                                               
         ICM   RE,15,BSPATAB       POINT TO FIRST ENTRY IN TABLE                
         SR    RF,RE               DISPLACEMENT INTO TABLE                      
*                                                                               
         SR    RE,RE                                                            
         D     RE,=A(ELTABL)       RELATIVE NUMBER OF ENTRY IN TABLE            
*                                                                               
         LA    RF,1(RF)            ORDINAL OF ENTRY IN TABLE                    
*                                                                               
         ST    RF,FULL                                                          
*                                                                               
         EDIT  (B4,FULL),(5,PBLSPUB),ALIGN=LEFT   DISPLAY ORDINAL               
*                                                                               
         OI    PBLSPUBH+4,X'20'    INDICATE VALID FIELD                         
*                                                                               
         OI    PBLSPUBH+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
LS1STX   DS    0H                                                               
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,PBLMOR1H         POINT TO FIRST MORE FIELD                    
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
         LA    R1,PBLMORLH         POINT TO LAST MORE FIELD                     
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
NLINS    EQU   ((PBLPUBLH-PBLPUB1H)/(PBLPUB2H-PBLPUB1H))+1                      
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
*                                                                               
         DROP  R2                                                               
*                                                                               
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
         TITLE 'PRSFM18 - BUILD TABLE OF ELEMENTS ON FILE'                      
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1  LABEL=*                                                          
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
         L     R5,AIO1             POINT TO PUB LIST RECORD                     
         USING PLISRECD,R5         ESTABLISH PUB LIST RECORD                    
*                                                                               
         MVI   ELCODE,PLISPBQ      LOOKING FOR PUB ELEMENTS                     
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF PUBLIST CHGS              
         BNE   LSBTRDDN                                                         
*                                                                               
LSBTRDLP DS    0H                                                               
*                                                                               
         LR    R6,R5               POINT TO START OF RECORD                     
         BAS   RE,GETEL            FIND FIRST ELEMENT                           
*                                                                               
LSBTLOOP DS    0H                                                               
*                                                                               
         BNE   LSBTDONE            END OF RECORD                                
*                                                                               
         USING PLISPBD,R6          ESTABLISH AS PUB ELEMENT                     
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PLISPBEL+1     ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTELEM(0),PLISPBEL SAVE PUB ELEMENT                             
*                                                                               
         MVC   ELTSORT,PLISPUB-PLISPBD+ELTELEM     SET KEY                      
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FITS INTO TABLE              
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTCONT DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
*                                                                               
         B     LSBTLOOP                                                         
*                                                                               
LSBTDONE DS    0H                                                               
*                                                                               
LSBTRDCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT PUBLIST RECORD                     
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF PUBLIST CHGS              
         BNE   LSBTRDDN                                                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         B     LSBTRDLP                                                         
*                                                                               
LSBTRDDN DS    0H                                                               
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
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF PUBLIST CHGS              
         BNE   LSBLDTBX                                                         
*                                                                               
         GOTO1 GETREC              RE-READ MASTER RECORD                        
*                                                                               
LSBLDTBX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         DROP  R4                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORDS - LHSRCH'                            
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
         ICM   RF,15,WRKSPUB       IF PAGE STARTING PUB GIVEN                   
         BZ    LHSSPUBX                                                         
*                                     FIND IN TABLE                             
         BCTR  RF,0                   DECREMENT FOR INDEXING                    
         LA    RE,ELTABL              LENGTH OF TABLE ENTRY                     
         MR    RE,RE                  INDEX TO ENTRY IN TABLE                   
         A     RF,BSPATAB             ADD ON START OF TABLE                     
         LR    R4,RF                  SAVE ADDRESS                              
*                                                                               
*******  B     LHSRCH11               DONE                                      
*                                                                               
LHSSPUBX DS    0H                                                               
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         OC    LSVKEY,LSVKEY       NO PREVIOUS ENTRY MEANS FIRST TIME           
         BNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         CLC   LSVKEY,HIVALS       IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
******   BCTR  RE,0                   DECREMENT FOR INDEXING                    
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
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   *+12                AFTER AN UP SCROLL                           
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
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
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORDS - LHVALLIN'                          
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
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKPUB,WRKPUB       INIT PUB WORKAREA                            
         XC    WRKPUBN,WRKPUBN     INIT NAME WORKAREA                           
         XC    WRKZNAM,WRKZNAM     INIT ZONE WORKAREA                           
         XC    WRKCITY,WRKCITY     INIT CITY WORKAREA                           
         XC    WRKSTA,WRKSTA       INIT STATE WORKAREA                          
         XC    WRKFILT,WRKFILT     INIT FILTER WORKAREA                         
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING PLISPBD,R3          ESTABLISH PUB ELEMENT                        
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =PUB            
*                                                                               
         CLI   5(R2),0             SKIP IF NO PUB ENTERED                       
         BE    LHVPUB10                                                         
*                                                                               
         L     RF,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,RF                                                       
*                                                                               
         MVC   WRKPUB,LSVSORT      COPY PUB NUMBER                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(MYPUBVAL),RR=RELO  VALIDATE PUB                               
*                                                                               
LHVPUB10 DS    0H                                                               
*                                                                               
         OC    WRKPUB,WRKPUB       IF NO INPUT IN PUB FIELD                     
         BNZ   LHVPUB30                                                         
*                                                                               
         B     LHVPUBOK               OKAY IF NO INPUT ON LINE                  
*                                                                               
         LR    R1,R2                  SAVE R2                                   
         SR    R0,R0                                                            
         ICM   R0,1,LUNFLDS           GET NUMBER OF FIELDS ON SCREEN            
         BCTR  R0,0                   DECREMENT FOR FIRST FIELD                 
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
         B     LHVPUBOK               OKAY IF NO INPUT ON LINE                  
*                                                                               
LHVPUB30 DS    0H                                                               
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
         MVI   PLISPBEL,PLISPBQ    ELEMENT CODE                                 
         MVI   PLISPBEL+1,PLISFILT-PLISPBEL+L'PLISFILT   ELEMENT LENGTH         
         MVC   PLISPUB(6),WRKPUB   SET PUB CODE                                 
*                                                                               
         MVC   ELTSORT,PLISPUB     SET SORT KEY TO PUB NUMBER                   
*                                                                               
LHVPUBOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD IS VALID                      
*                                                                               
*        DISPLAY PUB NAME                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         MVC   8(L'WRKPUBN,R2),WRKPUBN  DISPLAY PUB NAME                        
         OI    6(R2),X'80'         RE-TRANSMIT                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ENTRY IN PUB                      
         BZ    LHVPNAMX                                                         
*                                                                               
LHVPNAMX DS    0H                                                               
*                                                                               
*        DISPLAY PUB ZONE NAME                                                  
*                                                                               
         BAS   RE,BUMP                                                          
         MVC   8(L'WRKZNAM,R2),WRKZNAM DISPLAY PUB ZONE NAME                    
         OI    6(R2),X'80'         RE-TRANSMIT                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ENTRY IN PUB                      
         BZ    LHVCNAMX                                                         
*                                                                               
LHVCNAMX DS    0H                                                               
*                                                                               
*        DISPLAY PUB CITY                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         MVC   8(L'WRKCITY,R2),WRKCITY   DISPLAY PUB CITY                       
         OI    6(R2),X'80'         RE-TRANSMIT                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ENTRY IN PUB                      
         BZ    LHVSNAMX                                                         
*                                                                               
LHVSNAMX DS    0H                                                               
*                                                                               
*        DISPLAY PUB STATE                                                      
*                                                                               
         BAS   RE,BUMP                                                          
         MVC   8(L'WRKSTA,R2),WRKSTA    DISPLAY PUB STATE                       
         OI    6(R2),X'80'         RE-TRANSMIT                                  
*                                                                               
*****    LTR   R4,R4               SKIP IF NO ENTRY IN PUB                      
*****    BZ    LHVZNAMX                                                         
*                                                                               
         B     LHVPBLDN            **** SKIP FILTER PROCESSING *****            
*                                  * NO ROOM ON SCREEN FOR FILTER **            
*                                                                               
LHVZNAMX DS    0H                                                               
*                                                                               
*        VALIDATE FILTER INPUT                                                  
*                                                                               
LHVFLT   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO FILTER FIELD                         
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVFLTOK               SKIP VALIDATION                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BZ    LHVFLTOK            OKAY IF NO INPUT CHECK NEXT FIELD            
*                                                                               
         CH    RF,=Y(L'PLISFILT)   CHECK FOR INVALID LENGTH                     
         BH    LHVLONGE                                                         
*                                                                               
         MVC   WRKFILT,SPACES      INIT FILTER WORK AREA                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKFILT(0),8(R2)    SAVE FILTER INPUT                            
*                                                                               
         MVC   PLISFILT,WRKFILT    ADD FILTER TO ELEMENT                        
*                                                                               
LHVFLTOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         MVC   8(L'WRKFILT,R2),WRKFILT DISPLAY FILTER                           
         OI    6(R2),X'80'         RE-TRANSMIT                                  
*                                                                               
LHVPBLDN DS    0H                                                               
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
         BNE   LHVCMPE2            ELSE WE HAVE DUPLICATE                       
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,PLISPBEL    UPDATE ELEM IN TABLE                         
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
LHVLONGE DS    0H                                                               
         MVI   ERROR,TOOLONG             INPUT TOO LONG                         
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,RECFULL             TOO MANY DETAIL LINES                  
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =PUB            
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
         TITLE 'PRSFM18 - PUB LIST RECORDS - LHDISLIN'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1  LABEL=*                                                          
*                                                                               
         XC    WRKZNAM,WRKZNAM     CLEAR WORK AREAS                             
         XC    WRKCITY,WRKCITY                                                  
         XC    WRKSTA,WRKSTA                                                    
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDPUBX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING PLISPBEL,R3                                                      
*                                                                               
*              DISPLAY ISSUE DATE                                               
*                                                                               
         OC    PLISPUB,PLISPUB     SKIP IF NO PUB                               
         BZ    LHDPUBX                                                          
*                                                                               
*                                  EXPAND PUB NUMBER                            
         GOTO1 =V(PUBEDIT),DMCB,(C'0',PLISPUB),(0,8(R2)),RR=RELO                
*                                                                               
         MVC   MYPUB,PLISPUB                                                    
*                                                                               
         GOTO1 =A(MYVPUB),RR=RELO  GET PUB NAME                                 
*                                                                               
LHDPUBX  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
*        DISPLAY PUB NAME                                                       
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDNAMOK                                                         
*                                                                               
         MVC   8(L'PUBNM,R2),PUBNM   DISPLAY PUB NAME                           
         MVI   5(R2),L'PUBNM         SET LENGTH OF FIELD                        
*                                                                               
LHDNAMOK DS    0H                                                               
*                                                                               
*        DISPLAY PUB ZONE NAME                                                  
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDZNMOK                                                         
*                                                                               
         MVC   8(L'WRKZNAM,R2),WRKZNAM DISPLAY PUB ZONE NAME                    
         MVI   5(R2),L'WRKZNAM       SET LENGTH OF FIELD                        
*                                                                               
LHDZNMOK DS    0H                                                               
*                                                                               
*        DISPLAY PUB CITY                                                       
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDCTYOK                                                         
*                                                                               
         MVC   8(L'WRKCITY,R2),WRKCITY   DISPLAY PUB CITY                       
         MVI   5(R2),L'WRKCITY       SET LENGTH OF FIELD                        
*                                                                               
LHDCTYOK DS    0H                                                               
*                                                                               
*        DISPLAY PUB STATE                                                      
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDSTAOK                                                         
*                                                                               
         MVC   8(L'WRKSTA,R2),WRKSTA    DISPLAY PUB STATE                       
         MVI   5(R2),L'WRKSTA        SET LENGTH OF FIELD                        
*                                                                               
LHDSTAOK DS    0H                                                               
*                                                                               
         B     LHDFLTOK            *****  FILTER NOT IN USE                     
*                                                                               
*        DISPLAY FILTER IF PRESENT                                              
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         TWAXC (R2),(R2),PROT=Y    CLEAR FIELD                                  
*                                                                               
         MVI   5(R2),0             KILL ANY INPUT LENGTH                        
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDFLTOK                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PLISPBEL+1       GET ELEMENT LENGTH                           
*                                                                               
         SH    RF,=Y(PLISFILT-PLISPBEL)  CHECK FOR PRESENCE OF FILTER           
         BNP   LHDFLTOK                                                         
*                                                                               
         MVC   8(L'PLISFILT,R2),PLISFILT    DISPLAY FILTER                      
         MVI   5(R2),L'PLISFILT      SET LENGTH OF FIELD                        
*                                                                               
LHDFLTOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORDS - LHWRTTAB'                          
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1  LABEL=*                                                          
*                                                                               
*        DETERMINE THE NUMBER OF PUBS IN THE LIST                               
*                                                                               
         SR    RF,RF               INIT PUB COUNTER                             
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTPBDN                                                         
*                                                                               
LSWTPBLP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      COUNT ELEMENTS FLAGGED FOR ADD               
         BO    *+12                                                             
         TM    ELTCTL,ELTDELQ      COUNT THOSE NOT TO BE DELETED                
         BO    LSWTPBCN                                                         
*                                                                               
         LA    RF,1(RF)            BUMP PUB COUNTER                             
*                                                                               
LSWTPBCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTPBLP                                                         
*                                                                               
LSWTPBDN DS    0H                                                               
*                                                                               
         STCM  RF,3,WRKNPBS        SAVE NUMBER OF PUBS                          
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE ORIGINAL KEY                         
         MVC   KEYSAVE,ORIGKEY     RESTORE ORIGINAL KEY                         
         MVC   AIO,AIO1            USE I/OAREA1                                 
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         NI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
*        READ MASTER RECORD AND UPDATE DATE ELEMENT                             
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADDING                               
         BE    LSWADDX                                                          
*                                                                               
         GOTO1 HIGH                READ RECORD                                  
*                                                                               
         CLC   KEY(L'PLISKEY),KEYSAVE  MUST FIND IT                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
*        FIRST UPDATE DATE ELEMENT                                              
*                                                                               
         MVI   ELCODE,PLISDTEQ     SET TO FIND DATE ELEMENT                     
         L     R6,AIO1                                                          
*                                                                               
         GOTO1 REMELEM             REMOVE THE ELEMENT                           
*                                                                               
LSW10    DS    0H                                                               
*                                                                               
LSWADDX  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH AS DATE ELEMENT                    
         USING PLISDTED,R6                                                      
*                                                                               
         MVI   PLISDTEL,PLISDTEQ   SET ELEMENT ID                               
         MVI   PLISDTEL+1,27       ELEMENT LENGTH                               
*                                                                               
         MVC   PLISDAT,BTODAY      SET ACTIVITY DATE                            
         MVC   PLISNPBS,WRKNPBS    SET NUMBER OF PUBS IN LIST                   
         MVC   PLISDESC,PBLPBLN    SET PUB LIST DESCRIPTION                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PLISNPBS       NUMBER OF PUBS IN LIST                       
         EDIT  (2,PLISNPBS),(L'PBLTPUB,PBLTPUB),ALIGN=LEFT  DISPLAY             
         OI    PBLTPUBH+6,X'80'    FORCE TRANSMISSION                           
         OI    PBLTPUBH+4,X'20'    INDICATE PREVIOUSLY VALIDATED                
*                                                                               
         GOTO1 ADDELEM             PUT ELEMENT IN RECORD                        
*                                                                               
*        UPDATE PUB ELEMENTS                                                    
*                                                                               
         MVI   ELCODE,PLISPBQ      SET TO HANDLE PUB ELEMENTS                   
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         MVC   WKACT,ACTNUM        INIT WORK ACTION                             
*                                                                               
LSWTRDLP DS    0H                                                               
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE IF LIST ID CHANGED                 
         BE    LSWTRD10                                                         
*                                                                               
         L     RE,AIO              POINT TO I/OAREA                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PLISKLIN-PLISKEY(RE)  BUMP LINE NUMBER                      
         LA    RF,1(RF)                                                         
         STCM  RF,1,PLISKLIN-PLISKEY(RE)                                        
*                                                                               
         MVC   PLISLEN-PLISKEY(L'PLISLEN,RE),=H'33'  SET REC LNGTH              
         XC    PLISCTL-PLISKEY(32,RE),PLISCTL-PLISKEY(RE) INIT                  
*                                  RECORD START                                 
         XC    KEY,KEY                                                          
         MVC   KEY(L'PLISKEY),0(RE) COPY NEW KEY                                
         MVC   KEYSAVE,KEY                                                      
         MVI   WKACT,ACTADD        SET TO ADD THE RECORD                        
*                                                                               
LSWTRD10 DS    0H                                                               
*                                                                               
         CLI   WKACT,ACTADD        SKIP IF ADDING NEW RECORD                    
         BE    LSWTRD20                                                         
*                                                                               
         MVI   ELCODE,PLISPBQ      SET TO HANDLE PUB ELEMENTS                   
*                                                                               
         GOTO1 REMELEM             CLEAR PUB ELEMENTS OUT OF RECORD             
*                                                                               
LSWTRD20 DS    0H                                                               
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTWRDN                                                         
*                                                                               
*****    LA    R0,1900             SET MAX AREA FOR PUB ELEMENTS                
         LA    R0,880              SET MAX AREA FOR PUB ELEMENTS                
*                                                                               
LSWTWRLP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BNO   LSWTADD                                                          
*                                                                               
         B     LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R6,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING PLISPBD,R6          ESTABLISH AS PUB ELEMENT                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PLISPBEL+1     GET ELEMENT LENGTH                           
*                                                                               
         SR    R0,RF               DECREMENT LENGTH COUNTER                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),PLISPBEL MOVE ELEMENT TO WORK AREA                    
*                                                                               
         GOTO1 ADDELEM             PUT ELEMENT IN RECORD                        
*                                                                               
         CLI   ERROR,0             ONLY ERROR CAN BE NO ROOM IN REC             
         BNE   LSWTWRDN            NO ERROR TOLERATED                           
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTWRCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTWRCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BH    LSWTWRDN                                                         
*                                                                               
         LTR   R0,R0               CONTINUE IF ROOM LEFT IN RECORD              
         BP    LSWTWRLP                                                         
*                                                                               
LSWTWRDN DS    0H                                                               
*                                                                               
         TM    KEY+25,X'80'        IF RECORD DELETED                            
         BNO   LSWTDN30                                                         
*                                                                               
         NI    KEY+25,X'FF'-X'80'     TURN OFF DELETE BIT                       
*                                                                               
         GOTO1 WRITE                  RE-WRITE POINTER                          
*                                                                               
LSWTDN30 DS    0H                                                               
*                                                                               
         CLI   WKACT,ACTADD        IF ADDING RECORD                             
         BNE   LSWTDN10                                                         
*                                                                               
         GOTO1 ADDREC                 ADD THE RECORD                            
*                                                                               
         B     LSWTDN20                                                         
*                                                                               
LSWTDN10 DS    0H                  ELSE RE-WRITE RECORD                         
*                                                                               
         L     RE,AIO                                                           
         NI    PLISCTL-PLISKEY(RE),X'FF'-X'80'  TURN OFF DELETE BIT             
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
LSWTDN20 DS    0H                                                               
*                                                                               
         OC    BSPNOR,BSPNOR       DONE IF NO ENTRIES                           
         BZ    LSWTRDDN                                                         
*                                                                               
         C     R4,ELTLAST          DONE IF TABLE EXHAUSTED                      
         BH    LSWTRDDN                                                         
*                                                                               
LSWTRDCN DS    0H                                                               
*                                                                               
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
         GOTO1 SEQ                 FIND NEXT RECORD IN GROUP                    
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE SKIP IF LIST ID CHANGED            
         BNE   LSWTRDC1                                                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
LSWTRDC1 DS    0H                                                               
*                                                                               
         B     LSWTRDLP                                                         
*                                                                               
LSWTRDDN DS    0H                                                               
*                                                                               
*        DELETE ANY REMAINING RECORDS IN LIST                                   
*                                                                               
         CLI   WKACT,ACTADD        DONE IF ADDING RECORDS TO LIST               
         BE    LSWTDLDN                                                         
*                                                                               
LSWTDLLP DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD IN GROUP                    
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF CODE CHANGES              
         BNE   LSWTDLDN                                                         
*                                                                               
         TM    KEY+25,X'80'        SKIP IF ALREADY DELETED                      
         BO    LSWTDLCN                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
         OI    PLISCTL-PLISKEY(R6),X'80'   FLAG AS DELETED                      
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         OI    KEY+25,X'80'        FLAG POINTER AS DELETED                      
*                                                                               
         GOTO1 WRITE               DELETE POINTER                               
*                                                                               
LSWTDLCN DS    0H                                                               
*                                                                               
         B     LSWTDLLP                                                         
*                                                                               
LSWTDLDN DS    0H                                                               
*                                                                               
         MVC   KEY,ORIGKEY         RESTORE FILE POINTER                         
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PLISKLIN-PLISKEY),KEYSAVE  DONE IF PUBLIST CHGS              
         BNE   LSWTRESX                                                         
*                                                                               
         GOTO1 GETREC              RE-READ MASTER RECORD                        
*                                                                               
LSWTRESX DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'   TURN OFF READING DELETED RECORDS           
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORDS - SUBROUTS'                          
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
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
         LA    R2,PBLCLTH          CURSOR AT CLIENT                             
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
         LA    R2,PBLPUB1H                                                      
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
*                                                                               
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,48,C'PUBLICATION LIST REPORT'                                 
         SSPEC H2,48,C'-----------------------'                                 
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H6,95,REQUESTOR                                                  
         SSPEC H6,121,PAGE                                                      
         SSPEC H1,2,C'MEDIA'                                                    
         SSPEC H2,2,C'CLIENT'                                                   
         SSPEC H3,2,C'PUB LIST'                                                 
         SSPEC H4,2,C'NO. OF PUBS'                                              
         SSPEC H8,02,C'PUBLICATION'                                             
         SSPEC H9,02,C'-----------'                                             
         SSPEC H8,20,C'NAME'                                                    
         SSPEC H9,20,C'----'                                                    
         SSPEC H8,45,C'ZONE NAME'                                               
         SSPEC H9,45,C'---------'                                               
         SSPEC H8,70,C'CITY'                                                    
         SSPEC H9,70,C'----'                                                    
         SSPEC H8,91,C'ST'                                                      
         SSPEC H9,91,C'--'                                                      
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORD - PFKEYS'                             
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT A LINE                                   *         
*              PF6  - DELETE A LINE                                   *         
*              PF9  - CASCADE DATA                                    *         
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
         CLC   CONACT(3),=C'ADD'   IF ACTION 'ADD'                              
         BNE   PFKEYS1                                                          
*                                                                               
         FOUT  CONACTH,=C'CHA'        CHANGE TO 'CHANGE'                        
*                                                                               
PFKEYS1  DS    0H                                                               
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
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
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TIOBAID,3           PF3 OR PF15                                  
         BE    *+8                                                              
         CLI   TIOBAID,15                                                       
         BE    LNCLR                  CLEAR BOTTOM OF SCREEN                    
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
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORD - LNADD'                              
***********************************************************************         
*                                                                     *         
*        CLEAR BOTTOM OF SCREEN FOR INPUT - PUT '++' AT START OF LINE *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNCLR    DS    0H                  CLEAR FROM HERE ON                           
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,ALINCUR        POINT TO FIRST FIELD OF CURSOR LINE          
         LA    R2,0(R2,RA)                                                      
*                                                                               
*                                                                               
         MVI   5(R2),2             INPUT LENGTH OF 2                            
         MVC   8(2,R2),=C'++'      FORCE CLEARING                               
*                                                                               
LNCLRX   DS    0H                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PRSFM18 - PUB LIST RECORD - LNADD'                              
***********************************************************************         
*                                                                     *         
*        ADD A LINE                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
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
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON A LINE                   
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
         ST    R6,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNADXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
         OI    4(R6),X'20'         SET AS VALIDATED                             
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
         TITLE 'PRSFM18 - PBL FILE - INSTRUCTION RECORD - LNDEL'                
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
         OI    IPSTAT,LUSNPVQ      FORCE RE-WRITE TO FILE                       
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
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON A LINE                   
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
         SR    R0,R0                                                            
         IC    R0,NFLDS            NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNDLXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
         OI    4(R6),X'20'         SET AS VALIDATED                             
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
         TITLE 'PRSFM18 - PUB LIST POSTING - ERRORS'                            
***********************************************************************         
*                                                                     *         
*        ERROR ROUTINES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNCLTER  DS    0H                                                               
         MVI   ERROR,PWELNCLT      NOT ALLOWED FOR CLT REC                      
         B     PFKEYERX                                                         
*                                                                               
LNFRIER  DS    0H                                                               
         MVI   ERROR,PWELNFRI      NOT ALLOWED FOR FRQ ISSUE REC                
         B     PFKEYERX                                                         
*                                                                               
         TITLE 'PRSFM18 - PUB LIST POSTING - EXITS'                             
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
         TITLE 'T41C18 PUB LIST RECORDS - HOOK'                                 
***********************************************************************         
*                                                                     *         
*        HEADLINE HOOK                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HOOK     NTR1  BASE=*,LABEL=*             HEADLINE ROUTINES                     
         MVC   H1+15(1),QMED       PRINT MEDIA CODE                             
         MVC   H2+15(3),PCLT       PRINT CLIENT CODE                            
         MVC   H2+20(20),PCLTNM    PRINT CLIENT NAME                            
         MVC   H3+15(3),PPBLC      PRINT PUBLIST CODE                           
         MVC   H3+20(20),PPBLN     PRINT PUBLIST DESCRIPTION                    
         MVC   H4+15(3),PNPBS      PRINT NUMBER OF PUBS IN LIST                 
*                                                                               
HOOKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM18 - PUB LIST POSTING - LNTBL'                             
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNTBL    DS    0D                                                               
         DC    Y(PBLPUB1H-T41CFFD)  PUB 1                                       
         DC    Y(PBLPUB2H-T41CFFD)  PUB 2                                       
         DC    Y(PBLPUB3H-T41CFFD)  PUB 3                                       
         DC    Y(PBLPUB4H-T41CFFD)  PUB 4                                       
         DC    Y(PBLPUB5H-T41CFFD)  PUB 5                                       
         DC    Y(PBLPUB6H-T41CFFD)  PUB 6                                       
         DC    Y(PBLPUB7H-T41CFFD)  PUB 7                                       
         DC    Y(PBLPUB8H-T41CFFD)  PUB 8                                       
         DC    Y(PBLPUB9H-T41CFFD)  PUB 9                                       
         DC    Y(PBLPUBAH-T41CFFD)  PUB 10                                      
         DC    Y(PBLPUBBH-T41CFFD)  PUB 11                                      
         DC    Y(PBLPUBCH-T41CFFD)  PUB 12                                      
LNTBLLS  DC    Y(PBLPUBLH-T41CFFD)  PUB LAST                                    
         DC    4X'FF'               END OF TABLE                                
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD8D                                                       
*                                                                               
         DS    0D                                                               
*                                                                               
LSVTAB   DS    XL(NLINS*LSVTABL)                                                
         DS    0D                                                               
*                                                                               
         ORG   CONHEAD-64+X'1800'                                               
HELPSAVE DS    XL512               HELP CONTROL BLOCK                           
ELTMAX   EQU   600                                                              
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
NFLDS    DS    X                   NUMBER OF FIELDS ON LINE OF SCREEN           
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
WRKCOD   DS    CL3                 WORK PUBLIST CODE                            
WRKPUB   DS    XL6                 WORK PUB NUMBER                              
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
WRKPUBN  DS    CL20                WORK PUB NAME                                
WRKZNAM  DS    CL20                WORK PUB ZONE NAME                           
WRKCITY  DS    CL15                WORK PUB CITY                                
WRKSTA   DS    CL2                 WORK PUB STATE                               
WRKFILT  DS    CL3                 WORK PUB FILTER                              
WRKNPBS  DS    XL2                 WORK NUMBER OF PUBS IN LIST                  
PCLT     DS    CL3                 SAVE FOR RPT HDR - CLIENT CODE               
PCLTNM   DS    CL20                SAVE FOR RPT HDR - CLIENT NAME               
PPBLC    DS    CL3                 SAVE FOR RPT HDR - PUB LIST CODE             
PPBLN    DS    CL20                SAVE FOR RPT HDR - PUB LIST "NAME"           
PNPBS    DS    CL4                 SAVE FOR RPT HDR - # OF PUBS IN LIST         
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
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
SVDIR    DS    XL1                 DIRECTION SAVEAREA                           
WKNEWKEY DS    XL1                 WORK NEWKEY                                  
WKACT    DS    XL1                 WORK ACTION                                  
WRKSPUB  DS    XL4                 STARTING PUB NUMBER                          
*                                                                               
         DS    0D                                                               
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
         DS    CL2                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL4                                                              
LPBLC    DS    CL3                 PUB LIST CODE                                
         DS    CL4                                                              
LPBLN    DS    CL20                PUB LIST DESCRIPTION                         
         DS    CL5                                                              
LNPBS    DS    CL4                 NUMBER OF PUBS IN LIST                       
         DS    CL3                                                              
*                                                                               
* *******************                                                           
*  REPORT PRINT LINE                                                            
* *******************                                                           
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PPUBNO   DS    CL15                PUB NUMBER                                   
         DS    CL3                                                              
PPUBNM   DS    CL20                PUB NAME                                     
         DS    CL5                                                              
PPUBZNM  DS    CL20                PUB ZONE NAME                                
         DS    CL5                                                              
PPUBCITY DS    CL16                PUB CITY                                     
         DS    CL5                                                              
PPUBSTA  DS    CL2                 PUB STATE                                    
         DS    CL5                                                              
PFILT    DS    CL3                 PUB FILTER                                   
PRNTLEN  EQU   *-PRNTD                                                          
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
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL(L'PLISPUB+L'PLISZON+L'PLISED)                                 
LSVKEYL  EQU   *-LSVTABD                                                        
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL(L'PLISPUB+L'PLISZON+L'PLISED)       SORT VALUE                
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(11)              PUB ELEMENT LENGTH                           
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116PRSFM18   07/17/02'                                      
         END                                                                    
