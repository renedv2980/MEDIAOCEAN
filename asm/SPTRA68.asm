*          DATA SET SPTRA68    AT LEVEL 016 AS OF 11/16/18                      
*PHASE T21668C                                                                  
         TITLE 'T21668 NETWORK TRAFFIC - PRODUCT TAL AGY MAINT/LIST'            
*  TITLE: T21668 - TRAFFIC NETWORK PRODUCT TALENT AGY MAINT/LIST      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG                                              *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* MOD LOG:                                                            *         
* --------                                                            *         
*                                                                     *         
* LEV WHO  WHEN    WHAT                                               *         
* --- ---  ----    ----                                               *         
*                                                                     *         
* 01  EJOR 10MAY93 ORIGINAL DEVELOPMENT                               *         
* 02  BGRI 06JAN94 CHECK KEY PASSED FROM GENCON                       *         
* 03  BGRI 04MAR94 ALLOW BLANK TALENT AGENCY ENTRY                    *         
* 04  ???  12APR94                                                    *         
* 05  BGRI 21JAN99 BYPASS TAL AGENCY VALIDATION IF SJR                *         
* 06  BGRI 01JUL02 PUT IN CLIENT STRING SECURITY                      *         
* 07  SMUR 23JUL02 FIX DISP KEY ROUTINE TO SHOW PRODUCT               *         
* 08  SMUR 25FEB03 FIX CLIENT STRING SECURITY                         *         
* 09  SMUR 22OCT03 BRAND LEVEL SECURITY                               *         
* 10  GHOA 03JUN04 TALENT AGENCY CHANGES ON LIST ONLY, MAINT REMOVED  *         
* 11  GHOA 26JUL04 ADD SOX COMPLIANT CODE AND SUPPORT EXPANDED        *         
*                  NETWORK ID'S FROM TALENT AGENCY RECORD             *         
*                                                                     *         
* GHOA 015 15MAR18 SPSUG-1239  ELIM NEED AGENCY STATUSES: NX, SX, CX  *         
* GHOA 016 16NOV18             COMMENT CODE FOR TST AND FQA TESTING   *         
*                                                                     *         
***********************************************************************         
T21668   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PROD**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR68RR                                                      
         SPACE                                                                  
         MVI   ACTELOPT,C'N'       DO *NOT* MONITOR ACTIVITY                    
         OI    GENSTAT1,RDUPAPPL                                                
         OI    GENSTAT4,NODELLST                                                
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BE    LVR                                                              
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEYS                                                                 
         SPACE                                                                  
VK       OI    GLSTSTAT,APPLCDSP+NOSELFLD+CHNGLIST                              
         MVC   LLIST,=Y(LINLNQ)                                                 
         MVI   NLISTS,15                                                        
*                                                                               
         TM    TRACLTH+4,X'80'     INPUT AT THIS TIME?                          
         BO    VK02                                                             
         TM    TRAPROD+4,X'80'     INPUT AT THIS TIME?                          
         BZ    VK04                                                             
*                                                                               
VK02     XC    CURKEY,CURKEY                                                    
         XC    POLKEY,POLKEY                                                    
*                                                                               
VK04     CLI   ACTNUM,ACTDEL       ACTION DELETE INVALID                        
         BE    ADDELER                                                          
         CLI   ACTNUM,ACTADD       ACTION ADD INVALID                           
         BE    ADDELER                                                          
         SPACE                                                                  
* FAKE VALIDATE MEDIA                                                           
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         MVI   ELEM+5,1                                                         
         MVI   ELEM+8,C'N'                                                      
         GOTO1 VALIMED                                                          
         SPACE                                                                  
* VALIDATE CLIENT                                                               
         SPACE                                                                  
         LA    R2,TRACLTH                                                       
         TM    4(R2),X'20'                                                      
*NOP     BNZ   VK20                                                             
         XC    SVBCLT,SVBCLT                                                    
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                 YES                                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   MISSERR              NO                                          
         CLI   TRAPRODH+5,0        CLT MUST BE USED IF PROD                     
         BNE   MISSERR                                                          
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK20                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         SPACE                                                                  
         B     VK20                                                             
         SPACE                                                                  
VK10     GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVC   SVBCLT,BCLT                                                      
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
* MAKE SURE PROD IS PRESENT EXCEPT FOR LIST                                     
         SPACE                                                                  
VK20     LA    R2,TRAPRODH                                                      
         CLI   5(R2),0             ANY ENTRY?                                   
         BNE   VK30                 YES                                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   MISSERR              NO                                          
         B     VK40                                                             
         SPACE                                                                  
VK30     CLC   =C'POL',TRAPROD                                                  
         BE    INVERR                                                           
         GOTO1 VALIPRD                                                          
         OC    TRAPROD+2(1),SPACES                                              
         SPACE                                                                  
* BUILD KEY *                                                                   
         SPACE                                                                  
VK40     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRODD,R4                                                         
         MVI   PKEYTYPE,X'00'                                                   
         MVC   PKEYAM,BAGYMD                                                    
*NOP     MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYCLT,SVBCLT                                                   
         MVC   PKEYPRD,TRAPROD                                                  
         OC    PKEYPRD,SPACES                                                   
*                                                                               
         SR    R1,R1               PROTECT CLI/PRD ON LIST                      
         IC    R1,NLISTS                                                        
         LA    R3,TRACLT1H                                                      
         USING LINED,R3                                                         
VK50     OI    LINCLTH+6,X'20'                                                  
         OI    LINPRDH+6,X'20'                                                  
         LA    R3,LINLNQ(R3)                                                    
         BCT   R1,VK50                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE VALIDATES LISTED RECORDS                                             
*                                                                               
         USING LINED,R3                                                         
LVR      MVC   SVKEY,KEY           SAVE KEY                                     
         XC    POLTAGY,POLTAGY                                                  
*                                                                               
         LA    R4,KEY                                                           
         USING PRODD,R4                                                         
         CLC   PKEYPRD,=C'POL'     TEST IF POL                                  
         BE    LVR02                                                            
         MVC   PKEYPRD,=C'POL'      NO - GET POL'S TALENT AGENCY                
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   POLTAGY,PTALAGY     SAVE POL TALENT AGENCY                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PKEY),SVKEY   REREAD NON POL                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
LVR02    TM    SOXSW,SOXOKFLG      DDS & FACTST?                                
         BO    LVR04                                                            
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    LVR04                                                            
         GOTO1 VSOXERR                                                          
*                                                                               
LVR04    L     R3,ATHISLST         R3=A(THIS SCREEN LINE)                       
*                                                                               
         XC    SVTALAGY,SVTALAGY                                                
         CLI   LINTAGYH+5,0                                                     
         BE    LVR08                                                            
*                                                                               
         MVC   SVTALAGY,LINTAGY                                                 
         CLC   =C'N/A',LINTAGY     ALLOW N/A                                    
         BNE   LVR06                                                            
         CLC   =C'POL',LINPRD      BUT NOT FOR POL                              
         BE    INVERR                                                           
         B     LVR12                                                            
*                                                                               
LVR06    CLC   AGENCY,=C'SJ'                                                    
         BE    LVR08                                                            
         BAS   RE,READTAL                                                       
*                                                                               
LVR08    LA    R4,KEY                                                           
         CLC   PKEYPRD,=C'POL'     IF POL, CHECK NON POL FOR SAME               
         BNE   LVR12               TALENT AGY                                   
         XC    PKEYPRD,PKEYPRD                                                  
         GOTO1 HIGH                                                             
         B     LVR10                                                            
*                                                                               
LVRSEQ   GOTO1 SEQ                                                              
LVR10    LA    R4,KEY                                                           
         CLC   KEY(4),KEYSAVE      GET NON POL RECORD                           
         BNE   LVR12                                                            
         CLC   PKEYPRD,=C'POL'                                                  
         BE    LVRSEQ                                                           
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         OC    PTALAGY,PTALAGY                                                  
         BZ    LVRSEQ                                                           
*                                                                               
         CLC   PTALAGY,LINTAGY     TEST POL TAL AGY = NON POL TAL AGY           
         BE    LVRSEQ                                                           
         CLI   FLAG,1                                                           
         BNE   TAGYERR2             NO - WARNING                                
*                                                                               
LVR12    MVI   FLAG,0                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   DMWORK+4(4),KEY+14                                               
*                                                                               
         OC    POLTAGY,POLTAGY     TEST POL TAL AGY EXISTS                      
         BZ    *+14                                                             
         CLC   POLTAGY,SVTALAGY    POL TAL AGY = PROD TAL AGY?                  
         BE    TAGYERR1             YES - ERROR                                 
*                                                                               
         L     R4,AIO                                                           
         MVC   PTALAGY,SVTALAGY    SET TAL AGY                                  
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LR       OC    KEY(L'PKEY),KEY                                                  
         BZ    LR02                                                             
         CLI   TRAPRODH+5,0        ANY PRODUCT FILTER?                          
         BNE   LR03                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'PKEY),POLKEY  NEW SCREEN, REDISPLAY POL                    
         B     LR04                                                             
*                                                                               
LR02     LA    R4,KEY                                                           
         USING PRODD,R4                                                         
         XC    KEY,KEY             GET POL RECORD FOR THIS CLIENT               
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,SVBCLT                                                   
*                                                                               
         CLI   TRAPRODH+5,0        ANY PRODUCT FILTER?                          
         BE    LR04                                                             
         MVC   PKEYPRD,TRAPROD      YES - SKIP POL AND GET PRODUCT              
LR03     GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LR04     GOTO1 HIGH                                                             
         B     LR06                                                             
*                                                                               
LRPSEQ   GOTO1 SEQ                                                              
*                                                                               
LR06     LA    R4,KEY                                                           
         CLC   KEY(2),KEYSAVE                                                   
         BNE   EXIT                                                             
         OC    PKEYPRD,PKEYPRD     TEST IT'S A PRODUCT RECORD                   
         BZ    LRPSEQ               NO                                          
         OC    PKEY+7(6),PKEY+7                                                 
         BNZ   LRPSEQ               NO                                          
*                                                                               
         OC    SVBCLT,SVBCLT       ANY CLIENT FILTER?                           
         BZ    *+14                                                             
         CLC   PKEYCLT,SVBCLT                                                   
         BNE   EXIT                                                             
*                                                                               
         CLI   TRAPRODH+5,0        ANY PRODUCT FILTER?                          
         BE    *+14                                                             
         CLC   PKEYPRD,=C'POL'                                                  
         BE    LRSEQ                                                            
*                                                                               
         CLC   PKEYPRD,=C'POL'                                                  
         BNE   LRPSEQ                                                           
         MVC   POLKEY,KEY                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,LISTPRD          DISPLAY POL                                  
*                                                                               
         LA    R4,KEY                                                           
         OC    CURKEY,CURKEY       TEST IN THE MIDDLE OF A CLIENT               
         BZ    LR08                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'PKEY),CURKEY  RESTORE LAST NON POL                         
         B     *+10                                                             
LR08     XC    PKEYPRD,PKEYPRD     GET NON POL FOR THIS CLIENT                  
         GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     LA    R4,KEY                                                           
         CLC   KEY(2),KEYSAVE                                                   
         BNE   EXIT                                                             
         OC    PKEYPRD,PKEYPRD     TEST IT'S A PRODUCT RECORD                   
         BZ    LRSEQ                NO                                          
         OC    PKEY+7(6),PKEY+7                                                 
         BNZ   LRSEQ                NO                                          
*                                                                               
         CLC   PKEYCLT,KEYSAVE+2   TEST CHANGE IN CLIENT                        
         BE    LR12                                                             
         XC    CURKEY,CURKEY                                                    
         XC    PKEYPRD,PKEYPRD      YES - FIND POL FOR THIS CLIENT              
         B     LR04                                                             
*                                                                               
LR12     OC    SVBCLT,SVBCLT       ANY CLIENT FILTER?                           
         BZ    *+14                                                             
         CLC   PKEYCLT,SVBCLT                                                   
         BNE   LRSEQ                                                            
         CLI   TRAPRODH+5,0        ANY PRODUCT FILTER?                          
         BE    *+14                                                             
         CLC   PKEYPRD,TRAPROD                                                  
         BL    LRSEQ                                                            
*                                                                               
         CLC   PKEYPRD,=C'POL'                                                  
         BE    LRSEQ                                                            
         MVC   CURKEY,KEY                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,LISTPRD          DISPLAY NON POL                              
         B     LRSEQ                                                            
*                                                                               
* DISPLAY THE LIST LINE                                                         
*                                                                               
LISTPRD  NTR1                                                                   
         L     R4,AIO                                                           
         CLC   LISTNUM,NLISTS             IF ALREADY DISPLAYED MAX              
         BE    LPRD02                        GO BACK TO LISTMON                 
*                                                                               
         L     R3,ATHISLST         R3=A(THIS LINE)                              
         USING LINED,R3                                                         
         MVC   LINPRD,PKEYPRD                                                   
         OI    LINPRDH+4,X'20'                                                  
         OI    LINPRDH+6,X'80'                                                  
         MVC   LINTAGY,PTALAGY                                                  
         OI    LINTAGYH+4,X'20'                                                 
         OI    LINTAGYH+6,X'80'                                                 
*                                                                               
         MVC   BCLT,KEY+(PKEYCLT-PKEY)    DISPLAY CLIENT                        
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
         XC    FLDH,FLDH           FIX AAN BUG                                  
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVC   SAVEKEY,KEY                                                      
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   LINCLT,QCLT                                                      
         OI    LINCLTH+4,X'20'     MARK AS PREVIOUSLY VALIDATED                 
         OI    LINCLTH+6,X'80'     TRANSMIT                                     
*                                                                               
LPRD02   XC    KEY,KEY             RESTORE PRODUCT KEY                          
         MVC   KEY(L'PKEY),SAVEKEY                                              
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   DMWORK+4(4),KEY+14                                               
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LISTPRDX B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ON-LINE VALIDATION OF TALENT AGENCY FIELD                                     
*                                                                               
* VALIDATE SPLTAL FIELD BY READING FOR ITS TALENT AGENCY REC                    
* AND MATCHING ITS NETWORK USERID ELEM TO REQUESTING AGENCY'S ID                
* TO SEE IF REQ AGY IS ALLOWED ACCESS TO SPLTAL AGY                             
*                                                                               
READTAL  NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
* - READ ID REC OF REQUESTING AGENCY TO GET AGENCY'S FULL ID                    
         BAS   RE,SWCON                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG           SET ORIGIN ID NUMBER                   
         DROP  R4                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 READ                                                             
*                                                                               
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST/ELSE BOMB                         
         ZIC   R1,1(R6)                                                         
         A     R1,=F'-3'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVAGYORG(0),2(R6)              SAVE AGENCY ID                    
*                                                                               
* READ TALENT AGENCY REC                                                        
*                                                                               
         BAS   RE,SWTAL        SWITCH TO TALENT SYSTEM                          
         XC    KEY,KEY         READ FOR TALENT AGENCY RECORD                    
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,SVTALAGY     SCREEN TAL AGY FIELD                        
         OC    TLAYAGY,SPACES                                                   
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   GERROR,=Y(TALAGYNF)                                              
         B     TRAPERR2                                                         
*                                                                               
*&&DO                              SPSUG-1239                                   
         L     R6,AIO2             POINT R2 TO TALENT RECORD                    
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'16'        FIND NETWORK USERID LIMIT ELEM               
         MVC   DATADISP,=H'40'                                                  
*                                                                               
* - LOOK AT TALENT AGENCY REC AND SEE IF NX STATUS ON                           
*                                                                               
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RDT30                                                            
         USING TAAYEL,R6                                                        
         TM    TAAYSTA5,TAAYNETI   CAN USE NET INTERFACE?                       
         BO    RDT40                                                            
*                                                                               
* - LOOK AT TALENT AGENCY REC AND MATCH WITH REQUESTING AGENCY'S ID             
*                                                                               
RDT30    MVI   ELCODE,TANLELQ      FIND NETWORK USERID LIMIT ELEM               
         L     R6,AIO              POINT R2 TO TALENT RECORD                    
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   GERROR,=Y(TALNAUTH)                                              
         B     TRAPERR2                                                         
*                                                                               
         USING TANLEL,R6                                                        
         ZIC   R3,TANLNUID         GET NUMBER OF USERIDS                        
         N     R3,=X'0000007F'     STRIP HOB                                    
         LA    R5,6                                                             
         TM    TANLNUID,TANLSPID   ARE WE USING 10 INSTEAD?                     
         BZ    *+8                                                              
         LA    R5,10                                                            
*                                                                               
         LA    R6,TANLUID          POINT TO USERID                              
*                                                                               
RDT35    LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),SVAGYORG    IS IT VALID USERID                           
         BE    RDT40               YES                                          
         AR    R6,R5               NO/TRY AGAIN                                 
         BCT   R3,RDT35                                                         
         MVC   GERROR,=Y(TALNAUTH)                                              
         B     TRAPERR2                                                         
*&&                                                                             
RDT40    DS    0H                                                               
         MVC   DATADISP,HALF                                                    
         BAS   RE,SWBACK           SWITCH BACK TO ORIGINAL SYSTEM               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE KEY                                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         MVC   DMWORK+4(4),KEY+14  SPOOF GENCON SAFETY @CHANGE2                 
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINES TO CONTROL SYSTEM SWITCHING                             
         SPACE 2                                                                
SWCON    DS    0H                                                               
         MVI   USEIO,C'Y'                                                       
         MVC   SYSDIR,=CL8'CTFILE'                                              
         MVC   SYSFIL,=CL8'CTFILE'                                              
         MVC   LKEY,=H'25'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVI   DMCB,X'0A'          SET TO SWITCH TO CONTROL                     
         B     SWSYS                                                            
         SPACE 2                                                                
SWTAL    DS    0H                                                               
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'2'                                                    
         MVI   DMCB,X'10'          SET TO SWITCH TO TAL1                        
         B     SWSYS                                                            
*&&DO                                 FOR TST AND FQA                           
         NTR1                                                                   
         LA    RF,TALSYS           SWITCH TO TALENT                             
         ST    RF,DMCB                                                          
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                SWITCH ERROR                                 
         B     EXIT                                                             
*                                                                               
TALSYS   DC    C'TAL'                                                           
         DC    X'FF'                                                            
*&&                                                                             
SWBACK   DS    0H                                                               
         MVI   USEIO,0                                                          
         MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   LKEY,SVLKEY                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DMCB(1),SVSYS       SET TO SWITCH BACK TO ORIG. SYSTEM           
         B     SWSYS                                                            
         SPACE 2                                                                
SWSYS    NTR1                                                                   
         MVC   DMCB+1(3),=X'FFFFFF'                                             
SWCALL   GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                SWITCH ERROR                                 
SWX      B     EXIT                                                             
*                                                                               
*&&DO                                                                           
*                                                                               
* FIND CLIENT HEADER *****AND SAVE CLIST                                        
*                                                                               
FCLT     NTR1                                                                   
         SPACE                                                                  
FCLT10   DS    0H                                                               
         MVC   BCLT,KEY+(PKEYCLT-PKEY)                                          
         MVC   SVPKPRD,KEY+(PKEYPRD-PKEY)                                       
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         SPACE                                                                  
         MVC   ELEM(8),TRACLTH                                                  
         MVI   ELEM+5,3                                                         
         MVC   ELEM+8(3),QCLT                                                   
         LA    R2,ELEM                                                          
         SPACE                                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
         SPACE                                                                  
         CLI   ERROR,0             ANY ERRORS                                   
         BNE   FCLT15                                                           
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT30               NO                                          
         SPACE                                                                  
         OC    SVPKPRD,SVPKPRD     ANY PRODUCT                                  
         BO    FCLT18                                                           
         B     FCLT30                                                           
         SPACE                                                                  
FCLT15   CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT30                                                           
         SPACE                                                                  
FCLT18   DS    0H                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
         SPACE                                                                  
FCLT20   CLC   SVPKPRD,0(R1)       THIS PROD CODE?                              
         BE    FCLT30                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,FCLT20                                                        
         SPACE                                                                  
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BE    FCLT25                                                           
         CLI   ACTNUM,ACTLIST      OK TO LIST IT                                
         BNE   TRAPERR                                                          
         SPACE                                                                  
FCLT25   DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 GET NEXT RECORD                              
         SPACE                                                                  
         CLC   KEY(3),KEYSAVE      SAME REC TYOE, A/M                           
         BNE   EXIT                                                             
         B     FCLT10                                                           
         SPACE                                                                  
FCLT30   MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   ERROR,0             SET CC FOR RETURN                            
         B     EXIT                                                             
*&&                                                                             
*                                                                               
*         MVC   AIO,AIO2                                                        
*         LA    R1,KEY                                                          
*         USING CLTHDR,R1                                                       
*         XC    KEY,KEY                                                         
*         MVC   CKEYAM,BAGYMD                                                   
*         MVC   CKEYCLT,BCLT                                                    
*         GOTO1 HIGH                                                            
*         CLC   KEY(13),KEYSAVE                                                 
*         BE    *+6                                                             
*         DC    H'0'                                                            
*         GOTO1 GETREC                                                          
*         L     R1,AIO                                                          
*         USING CLTHDR,R1                                                       
*         MVC   CLTNM,CNAME                                                     
*         LA    R4,CLIST                                                        
*         LA    R5,CLISTSZ * 4                                                  
*         LA    RE,SVCLIST                                                      
*         LR    RF,R5                                                           
*         MVCL  RE,R4                                                           
*         DROP  R1                                                              
*         SPACE                                                                 
*FCLTX    MVC   AIO,AIO1                                                        
*         MVC   KEY(L'LASTKEY),LASTKEY                                          
*         GOTO1 HIGH                RESTORE SEQUENCE                            
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
* ERROR/WARNING MESSAGES                                                        
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ADDELER  MVC   GERROR,=Y(NOADDDEL)                                              
         B     TRAPERR2                                                         
*                                                                               
         USING LINED,R3                                                         
TAGYERR1 XC    LINTAGY,LINTAGY     REMOVE TALENT AGY ON SCREEN                  
         OI    LINTAGYH+6,X'80'                                                 
         MVC   GERROR,=Y(TAGYDEFN)                                              
         B     TRAPERR2                                                         
*                                                                               
TAGYERR2 MVI   FLAG,1                                                           
         OI    CONSERVH+6,X'81'                                                 
         MVC   GERROR,=Y(TAGYDIFF)                                              
         B     TRAPERR2                                                         
         DROP  R3                                                               
*                                                                               
TRAPERR2 GOTO1 VTRAERR                                                          
*                                                                               
TAGYDEFN EQU   795                                                              
TAGYDIFF EQU   796                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PRODD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA8AD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR68RR DS    F                                                                
SVBCLT   DS    CL2                                                              
SAVEKEY  DS    CL48                                                             
SVAGYORG DS    CL8                                                              
SVPKPRD  DS    CL3                                                              
SVTALAGY DS    CL6                                                              
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
*                                                                               
FLAG     DS    XL1                                                              
*                                                                               
CURKEY   DS    XL13                                                             
POLKEY   DS    XL13                                                             
POLTAGY  DS    CL6                                                              
         EJECT                                                                  
* ONLINE LIST LINE                                                              
         SPACE                                                                  
LINED    DSECT                                                                  
LINCLTH  DS    CL8                                                              
LINCLT   DS    CL3                                                              
LINPRDH  DS    CL8                                                              
LINPRD   DS    CL3                                                              
LINTAGYH DS    CL8                                                              
LINTAGY  DS    CL6                                                              
         DS    CL8                                                              
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPTRA68   11/16/18'                                      
         END                                                                    
