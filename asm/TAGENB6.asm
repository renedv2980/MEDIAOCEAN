*          DATA SET TAGENB6    AT LEVEL 019 AS OF 07/14/15                      
*PHASE T702B6C,*                                                                
         TITLE 'T702B6 - PRINT COMMERCIAL LIST'                                 
T702B6   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B6                                                         
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
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     MAINX                                                            
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   MAIN30                                                           
         MVI   NLISTS,15           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OK TO RETURN EXTRA FOR EOL               
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR           R2=A(DISPLAY AREA)                           
         B     MAIN40                                                           
*                                                                               
MAIN30   CLI   MODE,PRINTREP                                                    
         BNE   MAINX                                                            
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,HDHOOK                                                        
         ST    R2,HEADHOOK                                                      
         LA    R2,P                R2=A(DISPLAY AREA)                           
*                                                                               
MAIN40   BAS   RE,LREC             GO LIST THE RECORDS                          
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE AGENCY                                        *         
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         LA    R2,SCOAGYH                                                       
         TM    4(R2),X'20'         HAS AGENCY CHANGED?                          
         BO    CLIVAL              NO                                           
         NI    SCOCLIH+4,X'DF'     YES, REVALIDATE CLIENT                       
*                                                                               
AGYVAL8  GOTO1 RECVAL,DMCB,TLAYCDQ,(X'2A',(R2)),0                               
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         USING TAAYD,R4                                                         
         CLI   TAAYTPOF,C'0'       IS OFFICE A LETTER?                          
         BNL   BADREC              NO, ERROR                                    
         OI    4(R2),X'20'         MANUALLY SET VALIDATION BIT                  
*                                                                               
***********************************************************************         
*              VALIDATE CLIENT                                        *         
***********************************************************************         
*                                                                               
CLIVAL   LA    R2,SCOCLIH                                                       
         TM    4(R2),X'20'         HAS CLIENT CHANGED?                          
         BO    PRDVAL              NO                                           
         NI    SCOPRDH+4,X'DF'     YES, REVALIDATE PRODUCT                      
*                                                                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),0                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   PRDVAL                                                           
         USING TABRD,R4                                                         
         OC    TABROEOR,TABROEOR   IS EMPLOYER OF RECORD SPECIFIED?             
         BZ    PRDVAL              NO                                           
         CLC   TABROEOR,TGTPEMP    YES, CANNOT BE TALENT PARTNERS               
         BE    BADCREC                                                          
*                                                                               
***********************************************************************         
*              VALIDATE PRODUCT                                       *         
***********************************************************************         
*                                                                               
PRDVAL   LA    R2,SCOPRDH                                                       
         TM    4(R2),X'20'         HAS PRODUCT CHANGED?                         
         BO    FRMVAL              NO                                           
         NI    SCOFMTH+4,X'DF'     YES, REVALIDATE FORMAT                       
*                                                                               
         CLI   5(R2),0             PRODUCT INPUT?                               
         BNE   PRDVAL2             YES                                          
*                                                                               
         CLI   SCOFMT,C'A'         NO, IS SEQUENCE BY NAME?                     
         BE    FRMVAL              YES, OK WITH OR WITHOUT START                
         CLI   SCOSTRTH+5,0        NO, IS START INPUT?                          
         BNE   FLDMISS             YES, REQUIRE PRODUCT                         
         B     FRMVAL                                                           
*                                                                               
PRDVAL2  GOTO1 RECVAL,DMCB,TLPRCDQ,(R2)                                         
*                                                                               
***********************************************************************         
*              VALIDATE FORMAT                                        *         
***********************************************************************         
*                                                                               
FRMVAL   LA    R2,SCOFMTH                                                       
         TM    4(R2),X'20'         HAS FORMAT CHANGED?                          
         BO    STRVAL              NO                                           
         NI    SCOSTRTH+4,X'DF'    YES, REVALIDATE START                        
*                                                                               
         MVI   RDSEQ,C'C'          DEFAULT IS CODE SEQUENCE                     
         CLI   5(R2),0             FORMAT INPUT?                                
         BE    FRMVAL2             NO                                           
*                                                                               
         CLI   8(R2),C'C'          CODE SEQUENCE REQUESTED?                     
         BE    FRMVAL2             YES                                          
*                                                                               
         MVI   RDSEQ,C'A'          NO, MUST BE ALPHA                            
         CLI   8(R2),C'A'                                                       
         BNE   FLDINV                                                           
*                                                                               
FRMVAL2  OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
***********************************************************************         
*              VALIDATE START                                         *         
***********************************************************************         
*                                                                               
STRVAL   LA    R2,SCOSTRTH                                                      
         TM    4(R2),X'20'         HAS START CHANGED?                           
         BO    FLTVAL              NO                                           
         NI    SCOFLTSH+4,X'DF'    YES, REVALIDATE AGENCY AND FILTER            
         XC    SVSTART,SVSTART     CLEAR SAVE AREA                              
*                                                                               
         CLI   5(R2),0             START ENTERED?                               
         BE    FLTVAL              NO                                           
*                                                                               
         ZIC   R3,5(R2)            SAVE FOR LENGTH INPUT                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SVSTART(0),8(R2)                                                 
*                                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
***********************************************************************         
*              VALIDATE FILTERS                                       *         
***********************************************************************         
*                                                                               
FLTVAL   LA    R2,SCOFLTSH                                                      
         TM    4(R2),X'20'         HAVE FILTERS CHANGED?                        
         BO    OPTVAL              NO                                           
         NI    SCOOPTSH+4,X'DF'    YES, REVALIDATE OPTIONS                      
*                                                                               
         XC    SVFILTS,SVFILTS     CLEAR SAVE AREA                              
         CLI   5(R2),0             FILTER ENTERED?                              
         BE    *+10                NO                                           
         XC    SVFILTS,8(R2)       YES, MOVE IT TO CONTROL BLOCK                
*                                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
***********************************************************************         
*              VALIDATE OPTIONS                                       *         
***********************************************************************         
*                                                                               
OPTVAL   TM    SCOOPTSH+4,X'20'    HAVE OPTIONS CHANGED?                        
         BO    INIBLK              NO                                           
         BAS   RE,VALOPTS          YES, VALIDATE THEM                           
*                                                                               
***********************************************************************         
*              INITIALIZE SYSIO BLOCK                                 *         
***********************************************************************         
*                                                                               
INIBLK   LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         MVC   TIFAGY,TGAGY        PASS AGENCY                                  
         MVC   AGYNAME,TGNAME      SAVE AGENCY NAME                             
*                                                                               
         MVC   TIFCLI,TGCLI        PASS CLIENT                                  
         MVC   CLINAME,TGNAME      SAVE CLIENT NAME                             
*                                                                               
         CLI   SCOPRDH+5,0         ANY PRODUCT?                                 
         BE    *+16                NO                                           
         MVC   TIFPRD,SCOPRD       PASS PRODUCT                                 
         OC    TIFPRD,SPACES                                                    
*                                                                               
         MVC   TIFFILT1(4),SVFILTS PASS FILTERS                                 
*                                                                               
         MVC   TIQSTART,SVSTART                                                 
*                                                                               
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
*                                                                               
         MVI   TIREAD,TLCOPCDQ     PASSIVE KEY IS PRINT COMMERCIAL              
         CLI   RDSEQ,C'C'          FORMAT IS CODE?                              
         BE    *+8                 YES                                          
         MVI   TIREAD,TLCONCDQ     NO, PASSIVE IS NAME                          
*                                                                               
         MVI   TIFMED,TACOMEDP     INITIALIZE MEDIA TO PRINT                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS FIELD                                           
*                                                                               
VALOPTS  NTR1                                                                   
         XC    OPTS,OPTS           CLEAR OPTION FIELDS                          
*                                                                               
         LA    R2,SCOOPTSH         R2 = A(FIELD)                                
         CLI   5(R2),0                                                          
*                                                                               
         BE    VOPTX                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    FLDINV                                                           
*                                                                               
VOPT20   MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0            LHS IS REQUIRED                              
         BE    FLDINV                                                           
         LA    R4,OPTTAB           LOOK UP IN OPTIONS TABLE                     
         USING OPTD,R4                                                          
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                                                             
*                                                                               
VOPT30   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),OPTLHS   MATCH ON LHS                                 
         BE    VOPT35                                                           
         LA    R4,OPTNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         BE    FLDINV              END OF TABLE                                 
         B     VOPT30                                                           
*                                                                               
VOPT35   LH    RF,OPTDISP          DISP TO VAL. ROUTINE                         
         AR    RF,RB                                                            
         MVC   ERRDISP,SCDISP2                                                  
         BASR  RE,RF               GO VALIDATE                                  
*                                                                               
VOPT40   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
*                                                                               
VOPTX    OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
*                                                                               
         USING SCAND,R3            R3 = A(SCAN BLOCK ENTRY)                     
         SPACE 1                                                                
VALREL   DS    0H                  RELEASED COMMERCIALS                         
         CLI   SCDATA2,C'Y'        INCLUDE                                      
         BE    VALRELX                                                          
         CLI   SCDATA2,C'N'        EXCLUDE (DEFAULT)                            
         BNE   FLDINV                                                           
VALRELX  MVC   OPTREL,SCDATA2                                                   
         BR    RE                                                               
*                                                                               
VALLOCK  DS    0H                  LOCKED COMMERCIALS                           
         CLI   SCDATA2,C'Y'        INCLUDE                                      
         BE    VALLOCKX                                                         
         CLI   SCDATA2,C'N'        EXCLUDE (DEFAULT)                            
         BNE   FLDINV                                                           
VALLOCKX MVC   OPTLOCK,SCDATA2                                                  
         BR    RE                                                               
         SPACE 2                                                                
VALDATE  DS    0H                                                               
         LR    R5,RE               SAVE ADDRESS OF RTRN POINT                   
         TM    SCVAL2,X'80'        TEST VALID NUMERIC                           
         BZ    VALD10                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SCBIN2+1       YES, MOVE TO RF                              
         LCR   RF,RF               AND COMPLEMENT                               
         GOTO1 ADDAY,DMCB,TGTODAY0,WORK,(RF) GO BACK N'DAYS                     
         B     VALD20                                                           
         SPACE 1                                                                
VALD10   GOTO1 DATVAL,DMCB,SCDATA2,WORK  ELSE MUST BE A DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    DATINV                                                           
         SPACE 1                                                                
VALD20   GOTO1 DATCON,DMCB,(0,WORK),(1,OPTDATE) LAST ACTIVE DATE                
         LR    RE,R5               RESTORE RE                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE CONTROLS RECORD LISTING                                  
*                                                                               
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(4,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(18,R1),=C'COMMERCIAL RECORDS'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
*                                                                               
         USING LINED,R2            R2=A(OUTPUT AREA)                            
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
*                                                                               
         BAS   RE,FILTER           CHECK OPTIONS                                
         BNE   LRHX                                                             
*                                                                               
         CLC   LISTNUM,NLISTS      IF WE'VE ALREADY LISTED MAX                  
         BE    LRH10               GO BACK TO LISTMON                           
*                                                                               
         CLI   RDSEQ,C'A'          READING ALPHA SEQUENCE?                      
         BNE   LRH03               NO, PRODUCT OK THEN                          
*                                                                               
         USING TAPRD,R4                                                         
         L     R4,TIAREC           YES, GET CORRECT PRODUCT CODE                
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LRH04                                                            
         MVC   TIPRD,TAPRPRD                                                    
*                                                                               
LRH03    CLI   SCOPRDH+5,0         WAS PRODUCT REQUESTED?                       
         BE    LRH04               NO                                           
         OC    SCOPRD,SPACES       YES, FILL WITH BLANKS                        
         CLC   TIPRD,SCOPRD        DO WE HAVE A MATCH?                          
         BNE   LRHX                NO, REJECT IT                                
         GOTO1 XNAME,DMCB,TLPRCDQ,PRDNAME,TIKEY  GET PRODUCT NAME               
*                                                                               
LRH04    MVC   LINPRD,TIPRD        PRODUCT                                      
         MVC   LINCID,TICID        COMMERCIAL ID                                
         MVC   LINCIDN,TINAME      COMMERCIAL NAME                              
         MVC   LINFILT,SPACES                                                   
         CLI   TIFILT1,X'FE'       FILTER 1                                     
         BE    *+10                                                             
         MVC   LINFILT(1),TIFILT1                                               
         CLI   TIFILT2,X'FE'       FILTER 2                                     
         BE    *+10                                                             
         MVC   LINFILT+1(1),TIFILT2                                             
         CLI   TIFILT3,X'FE'       FILTER 3                                     
         BE    *+10                                                             
         MVC   LINFILT+2(1),TIFILT3                                             
         CLI   TIFILT4,X'FE'       FILTER 4                                     
         BE    *+10                                                             
         MVC   LINFILT+3(1),TIFILT4                                             
*                                                                               
         CLC   TIPRD,TGPRD         IF PRODUCT CHANGED                           
         BE    LRH06                                                            
         MVC   TGPRD,TIPRD         SET NEW PRODUCT                              
         GOTO1 XNAME,DMCB,TLPRCDQ,PRDNAME,TIKEY  GET PRODUCT NAME               
*                                                                               
LRH06    MVC   LINPRDN,PRDNAME     PRODUCT NAME                                 
*                                                                               
         MVI   ELCODE,TACSELQ      GET COMMERCIAL STUDIO ELEMENT                
         MVC   AIO,TIAREC                                                       
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPS))                                     
         MVC   AIO,AIO1                                                         
         BNE   LRH08                                                            
         L     R3,TGELEM                                                        
         USING TACSD,R3                                                         
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,LINSDTE)  SHOOT DATE                 
*                                                                               
LRH08    CLI   MODE,PRINTREP       IF PRINTING                                  
         BNE   LRH10                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRHX                                                             
*                                                                               
LRH10    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRHX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO FILTER ON OPTIONS                                     
         SPACE 1                                                                
FILTER   NTR1                                                                   
         OC    OPTDATE,OPTDATE     IF ANY ACTIVITY DATE OPTION                  
         BZ    FIL20                                                            
*                                                                               
         L     R4,TIAREC           R4=A(COMMERCIAL ELEMENT)                     
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         OC    TACOPDTE,TACOPDTE   IF ANY LAST PAYMENT DATE                     
         BZ    FIL10                                                            
         CLC   TACOPDTE,OPTDATE    IF LAST PYMNT DATE BELOW CUTOFF DATE         
         BL    NO                  REJECT RECORD                                
         B     FIL20                                                            
         DROP  R4                                                               
*                                                                               
FIL10    MVC   AIO,TIAREC          ELSE, IF NO LAST PYMT DATE                   
         MVI   ELCODE,TAACELQ                                                   
         GOTO1 ACTVOUT,DMCB,(X'20',0)                                           
         MVC   AIO,AIO1                                                         
         ICM   R4,15,DMCB          R4=A(ACTIVITY ELEMENT)                       
         BZ    NO                                                               
         USING TAACD,R4                                                         
         CLC   TAACCDTE,OPTDATE    COMPARE AGAINST LAST ACTIVITY DATE           
         BL    NO                                                               
         DROP  R4                                                               
*                                                                               
FIL20    L     R4,TIAREC                                                        
         MVI   ELCODE,TACOELQ      R4=A(COMMERCIAL ELEMENT)                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TACOD,R4                                                         
         CLI   OPTREL,C'Y'         IF RELEASED REQUESTED                        
         BNE   *+12                                                             
         TM    TACOSTAT,TACOSTRL   AND COMMERCIAL IS RELEASED                   
         BO    YES                 GO LIST                                      
*                                                                               
         CLI   OPTLOCK,C'Y'        IF LOCKED REQUESTED                          
         BNE   *+16                                                             
         TM    TACOSTAT,TACOSTLO   AND COMMERCIAL IS LOCKED                     
         BO    YES                 GO LIST                                      
         B     NO                  REJECT IF DIDN'T PASS EITHER TEST            
*                                                                               
         CLI   OPTREL,C'Y'         IF RELEASED REQUESTED                        
         BE    NO                  REJECT IF DIDN'T PASS EITHER TEST            
*                                                                               
         TM    TACOSTAT,TACOSTRL+TACOSTLO  ELSE IF COMML REL OR LOCKED          
         BNZ   NO                          REJECT                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              HEADLINE ROUTINES  (HEADHOOK)                                    
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   HEAD4+9(6),TIFAGY     AGENCY                                     
         MVC   HEAD4+17(16),AGYNAME AGENCY NAME                                 
         MVC   HEAD5+9(6),TIFCLI     CLIENT                                     
         MVC   HEAD5+17(16),CLINAME  CLIENT NAME                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              EERRORS, EXITS, ETC.                                   *         
***********************************************************************         
*                                                                               
DATINV   MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
*                                                                               
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
BADREC   MVI   ERROR,ERINVRAG      INVALID RECORD FOR THIS AGENCY               
         B     *+8                                                              
*                                                                               
BADCREC  MVI   ERROR,ERINVRCL      INVALID RECORD FOR THIS CLIENT               
         L     R2,EFHREC           CURSOR TO REC/USE FIELD                      
         B     THEEND                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
PFTAB    DS    0C                                                               
         DC    AL1(PF10X-*,10,0,(PF10X-PF10)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CAST    ',CL8'LIST    '                               
PF10     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
PF10X    EQU   *                                                                
*                                                                               
         DC    AL1(PF11X-*,11,0,(PF11X-PF11)/KEYLNQ,0)                          
         DC    CL3' ',CL8'HISTORY ',CL8'LIST    '                               
PF11     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
PF11X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
OPTTAB   DS    0H                                                               
         DC    CL10'RELEASED  ',AL2(VALREL-T702B6)                              
         DC    CL10'LOCKED    ',AL2(VALLOCK-T702B6)                             
         DC    CL10'ACTIVE    ',AL2(VALDATE-T702B6)                             
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              REPORT SPECS                                           *         
***********************************************************************         
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,31,C'COMMERCIAL LIST'                                         
         SSPEC H2,31,C'---------------'                                         
         SPACE 1                                                                
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H5,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H7,2,C'PRODUCT  COMMERCIAL ID  COMMERCIAL NAME'                  
         SSPEC H8,2,C'-------  -------------  ---------------'                  
         SSPEC H7,44,C'PRODUCT NAME      SHOOT    FILT'                         
         SSPEC H8,44,C'------------      -----    ----'                         
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              DSECT TO COVER LIST LINE                               *         
***********************************************************************         
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LINPRD   DS    CL6                 PRODUCT CODE                                 
         DS    CL3                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
         DS    CL3                                                              
LINCIDN  DS    CL16                COMMERCIAL NAME                              
         DS    CL2                                                              
LINPRDN  DS    CL16                PRODUCT NAME                                 
         DS    CL2                                                              
LINSDTE  DS    CL8                 SHOOT DATE                                   
         DS    CL1                                                              
LINFILT  DS    CL4                 FILTERS                                      
         EJECT                                                                  
***********************************************************************         
*              DSECT TO COVER OPTIONS TABLE                           *         
***********************************************************************         
*                                                                               
OPTD     DSECT                                                                  
OPTLHS   DS    CL10                                                             
OPTDISP  DS    AL2                                                              
OPTNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB6D                                                       
*                                                                               
COUNTER  DS    PL4                 RECORD COUNTER                               
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
*                                                                               
AGYNAME  DS    CL16                AGENCY NAME                                  
CLINAME  DS    CL16                CLIENT NAME                                  
PRDNAME  DS    CL16                PRODUCT NAME                                 
SVSTART  DS    CL24                STARTING FIELD                               
SVFILTS  DS    CL4                 FILTERS                                      
*                                                                               
OPTS     DS    0CL11               OPTIONS                                      
OPTREL   DS    CL1                 LIST RELEASED COMMERCIALS                    
OPTLOCK  DS    CL1                 LIST LOCK COMMERCIALS                        
         DS    CL6                 N/D                                          
OPTDATE  DS    XL3                 LAST ACTIVE DATE                             
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
         PRINT ON                                                               
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
* TASYSDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
* TASYSEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019TAGENB6   07/14/15'                                      
         END                                                                    
