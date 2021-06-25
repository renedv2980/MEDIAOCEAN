*          DATA SET SPTRA0A    AT LEVEL 032 AS OF 05/07/14                      
*PHASE T2160AA                                                                  
         TITLE 'T2160A FLIGHT RECORD DISPLAY, CHANGE, ADD, DELETE, LIST'        
                '                                                               
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 - REC READ IN FOR CHANGE COMPARE                             
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - WORK                                                              
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
*  LEV 19    JAN08/87 ADDED OFFICE PROFILE                                      
*  LEV 20    JUL07/88 PRINT QPRD=??? FOR MISSING PROD, NO PC                    
*  LEV 21    JAN04/89 DON'T ALLOW ANY FLIGHT LONGER THAN 1 YEAR                 
*  LEV 22    JUN17/92 ADD AGENCY TO RECORD                            *         
*  LEV 23    APR01/93 CHANGE FOR TRAFFIC FILE                         *         
*  LEV 24    JUL20/94 CHANGE TO FILENAME                              *         
*  LEV 25    DEC07/94 ALLOW MORE THAN 1 YEAR BACK IF NEW CLIENT       *         
*  LEV 26    MAR27/01 USE TRAFFIC OFFICE                              *         
*  LEV 27 SMUR JUN26/02 CLIENT STRING SECURITY                        *         
*  LEV 28 BGRI JUL29/04 SOX                                           *         
*  LEV 29 SMUR NOV08/07 DDS ONLY - DELETE FLIGHT RECORD               *         
*  LEV 31 SMUR JAN08/14 FIX AAN CLIENT DISPLAY FROM LIST (MD6 NOT MDF)*         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T2160A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FLTR**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         NI    GENSTAT2,X'FF'-USMYOK   RESET USE MY MSG FLAG                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECPUT         BEFORE REWRITING, CK IF REQ NEEDED           
         BE    PUT                                                              
         CLI   MODE,XPREASON       EXPLAIN REASON CODE                          
         BE    EXPR                                                             
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    AAR                                                              
         SPACE                                                                  
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BNE   *+12                NO, DELETES NOT ALLOWED                      
         CLI   DELFTR,C'D'         IS FILTER DELETE SET                         
         BE    EXIT                YES, DELETES ALLOWED                         
         CLI   MODE,RECDEL         BEFORE DELETE RECORD (INVALID CML)           
         BE    DELREC                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BNE   VK02                NO, DELETES NOT ALLOWED                      
         CLI   DELFTR,C'D'         IS FILTER DELETE SET                         
         BE    VK04                YES, DELETES ALLOWED                         
         SPACE                                                                  
VK02     CLI   ACTNUM,ACTDEL       BEFORE DELETE RECORD (INVALID CML)           
         BE    DELREC                                                           
         SPACE                                                                  
VK04     LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         XC    BPRD,BPRD                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                YES                                          
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK07                 NO                                          
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
VK07     CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK30                YES, NOT NEEDED FOR LIST                     
         B     MISSERR             NO, MUST BE ENTRY                            
VK10     GOTO1 VALICLT                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'3'         READ T3 PROFILE                              
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF,ELEM                                                    
*                                                                               
         MVI   WORK+3,C'3'         READ T3PROF+6 FOR MEDIA T                    
         MVI   WORK+6,C'T'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF+6(1),ELEM+6                                             
*                                                                               
VK20     LA    R2,TRAPRDH          PRODUCT CODE                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         OC    BCLT,BCLT           IF CLIENT NOT ENTERED                        
         BZ    MISSCLT             ERROR                                        
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK        PRODUCT POL INVALID                          
         BE    INVPRDER                                                         
         MVC   QPRD,WORK           SAVE EBCIC PRODUCT                           
         MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
*                                                                               
VK30     LA    R2,TRAEDTH          END DATE VALIDATION                          
         XC    ENDATE,ENDATE                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK40                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK50                                                             
VK40     BAS   RE,VEDT                                                          
         SPACE                                                                  
VK50     LA    R2,TRAFLTRH         FILTER VALIDATION                            
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK60                                                             
         BAS   RE,VFTR                                                          
         EJECT                                                                  
* BUILD KEY *                                                                   
         SPACE                                                                  
VK60     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING FLTKEY,R4                                                        
         MVC   FLTKID,=XL2'0A27'                                                
         MVC   FLTKAM,BAGYMD                                                    
         MVC   FLTKCLT,BCLT        MOVE IN CLIENT                               
         MVC   FLTKPRD,BPRD        MOVE PRODUCT INTO KEY                        
         MVC   FLTKEDT,ENDATE      END DATE                                     
         CLI   ACTNUM,ACTLIST      ONLY SET COMPARE KEY                         
         BNE   VK80                FOR LIST                                     
                                                                                
* CHECK FOR ANY MISSING FIELDS (MUST ALL BE ENTERED LEFT TO RIGHT)              
                                                                                
         OC    ENDATE,ENDATE                                                    
         BZ    VK70                                                             
         OC    BCLT,BCLT                                                        
         BNZ   VK70                                                             
         LA    R2,TRACLTH                                                       
         B     MISSERR                                                          
                                                                                
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
                                                                                
VK70     LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF CMLKCML                      
VK72     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   VK74                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,VK72                                                          
VK74     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
         B     EXIT                                                             
                                                                                
* FOR ACTION ADD, UNPROTECT ALL FIELDS *                                        
                                                                                
VK80     CLI   ACTNUM,ACTADD                                                    
         BNE   EXIT                                                             
         LA    RF,TRAFL01H                                                      
*                                                                               
VK82     TM    1(RF),X'20'         PROTECTED                                    
         BZ    VK84                                                             
         OI    6(RF),X'80'         TRANSMIT                                     
         NI    1(RF),X'FF'-X'20'   UNPROTECT                                    
*                                                                               
VK84     LLC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         LLC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         LA    R0,TRATAGH                                                       
         CR    RF,R0                                                            
         BL    VK82                                                             
*                                                                               
         CLI   SPOTCAN,C'C'                                                     
         BNE   VKX                                                              
         CLI   SVT3PROF+6,C'Y'     AUTO ADD MEDIA N CMML?                       
         BNE   VKX                                                              
*                                                                               
         CLI   TRAMED,C'N'         ADDING MEDIA N DIRECTLY?                     
         BNE   VKX                                                              
*                                                                               
         MVC   WORK(13),KEY        SAVE KEY                                     
         NI    KEY+2,X'F1'         CHANGE MEDIA TO T                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IS MEDIA T REC THERE?                        
         BE    VK86                                                             
         LA    R2,TRAMEDH                                                       
         LHI   R0,NOTVCMML                                                      
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
VK86     MVC   KEY(13),WORK        RESTORE KEY                                  
*                                                                               
VKX      B     EXIT                                                             
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE                                                                  
VR       DS    0H                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VR01     DS   0H                                                                
         L     R4,AIO                                                           
         MVC   20(2,R4),AGENCY                                                  
         USING FLTKEY,R4                                                        
         MVC   BAGYMD,FLTKAM                                                    
         CLC   BCLT,FLTKCLT        IS CLIENT SAME                               
         BE    VR02                YES                                          
         SPACE                                                                  
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BE    *+6                 SHOULD BE OKAY                               
         DC    H'0'                                                             
         SPACE                                                                  
VR02     MVC   BPRD,FLTKPRD                                                     
         MVC   ENDATE,FLTKEDT                                                   
         DROP  R4                                                               
         BAS   RE,FEDT             FIND END DATE OF PREV FLIGHT REC             
         SPACE                                                                  
         MVI   ELCODE,X'10'        ADDRESS PART OF ELEMENT                      
         SPACE                                                                  
*        CLI   1(RA),C'*'          IS THIS A DDS TRMINAL                        
*        BNE   VR06                NO                                           
*        CLI   CHAFTR,C'C'         IS CHANGE FILTER SET                         
*        BNE   VR06                NO                                           
         GOTO1 REMELEM                                                          
         LA    R2,TRAFL01H         FIRST DATE PAIR                              
         LA    R3,24               MAX DATE PAIRS POSSIBLE                      
VR04     NI    4(R2),X'FF'-X'20'   SET OFF VALIDATED                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,VR04                                                          
         SPACE                                                                  
VR06     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING FLTDTAEL,R6                                                      
         MVI   FLTDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   FLTDTALN,FLTDTAX-FLTDTAEL ELEMENT LENGTH                         
         SPACE                                                                  
**********************************************************************          
* LOOP THRU VALIDATING ZERO TO 24 ELEMENTS - ANY OLD ELEMENTS WERE   *          
* DISPLAYED WITH PROTECTED FIELDS, AND ARE NOT ALLOWED TO BE CHANGED *          
* EXCEPT FOR DDS TERMINALS WITH FILTER CHANGE ENTERED                *          
**********************************************************************          
         SPACE                                                                  
         LA    R2,TRAFL01H         FIRST DATE PAIR                              
         LA    R3,24               MAX DATE PAIRS POSSIBLE                      
         MVI   VALDTESW,0          SET VALID DATE SW OFF                        
VR10     CLI   ACTNUM,ACTADD       IF ADD, USE ALL FIELDS                       
         BE    VR12                                                             
*        TM    4(R2),X'20'         ALREADY VALIDATED                            
*        BO    VR18                                                             
VR12     CLI   5(R2),0             ANY DATA ENTERED                             
         BE    VR20                NO                                           
         BAS   RE,VFLT             GO VALIDATE DATE PAIR                        
         CLC   FLTSTART,FLTEND                                                  
         BH    DATSQER                                                          
         CLC   FLTEND,ENDATE       END VS FLIGHT END DATE                       
         BH    ENDTEEX             CAN NOT BE PAST FLIGHT                       
         CLC   FLTSTART,LASTEDT    START VS LAST END DATE                       
         BNH   ENDTERR             CAN NOT BE PAST FLIGHT                       
         GOTO1 ADDELEM                                                          
VR18     MVI   VALDTESW,1          SET-FOUND 1 (OR MORE) DATE(S)                
         SPACE                                                                  
VR20     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,VR10                                                          
         CLI   VALDTESW,1          FIND ANY VALID DATES                         
         BE    VR30                YES                                          
         MVI   ERROR,MISSING                                                    
         LA    R2,TRAFL01H         POINT TO FIRST MISSING FIELD                 
         B     TRAPERR                                                          
         SPACE                                                                  
VR30     CLI   SVPROF+9,C'Y'       AUTO TURNAROUND                              
         BE    VR34                YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BE    VR34                NO                                           
         MVC   CHREASON,=C'NC'                                                  
         B     *+10                                                             
VR34     MVC   CHREASON,=C'TC'     SET UP AS MAINT CHANGE                       
         CLI   ACTNUM,ACTADD       UNLESS ADD                                   
         BNE   VR36                                                             
         MVI   CHREASON+1,C'A'                                                  
VR36     B     DR                  NOW DISPLAY VALIDATED RECORD                 
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       BAS   RE,CLRSCR                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE 1 DATES ELEMENT                    
         USING FLTDTAEL,R6                                                      
         LA    R2,TRAFL01H                                                      
         LA    R3,24               MAX DATE PAIR FIELDS                         
         SPACE                                                                  
DR10     LA    R4,8(,R2)                                                        
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,(R4))                                
         MVI   8(R4),C'-'                                                       
         LA    R4,9(,R4)                                                        
         GOTO1 DATCON,DMCB,(3,FLTEND),(5,(R4))                                  
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         TM    1(R2),X'01'         MODIFIED                                     
         BZ    DR12                                                             
         NI    1(R2),X'FF'-X'01'   SET OFF MODIFIED                             
         OI    TRATAGH+1,X'01'     SET ON ELSEWHERE                             
         SPACE                                                                  
*R12     CLI   1(RA),C'*'          IS THIS A DDS TRMINAL                        
*        BNE   DR14                NO                                           
*        CLI   CHAFTR,C'C'         IS CHANGE FILTER SET                         
*        BE    DR16                NO                                           
*R14     OI    1(R2),X'20'         PROTECT EXISTING FIELDS                      
DR12     OI    4(R2),X'20'         VALIDATED                                    
         SPACE                                                                  
DR16     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         BCT   R3,DR10                                                          
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         DC    H'0'                MUST NOT HAVE MORE THAN 24 ELEMENTS          
DR20     LTR   R3,R3                                                            
         BZ    DR30                                                             
         SPACE                                                                  
         LR    R4,R2                                                            
DR22     OC    8(L'TRAFL01,R4),8(R4)                                            
         BZ    *+14                                                             
         MVC   8(L'TRAFL01,R4),WORK                                             
         OI    6(R4),X'80'                                                      
         ZIC   R1,0(R4)                                                         
         AR    R4,R1                                                            
         ZIC   R1,0(R4)                                                         
         AR    R4,R1                                                            
         BCT   R3,DR22                                                          
DR30     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DELETE RECORD INVALID FOR COMMERCIALS                                         
         SPACE                                                                  
DELREC   MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     TRAPERR                                                          
         SPACE 3                                                                
* GENERATE TURN-AROUND REQ AFTER ADDED REC                                      
         SPACE                                                                  
AAR      DS    0H                                                               
         CLI   SPOTCAN,C'C'        TEST CANADA                                  
         JNE   AARX                                                             
         CLI   SVT3PROF+6,C'Y'     TEST AUTO ADD MEDIA N                        
         JNE   AARX                                                             
         CLI   TRAMED,C'T'         TEST SEL TV                                  
         JE    AAR2                                                             
         CLI   TRAMED,C'N'         TEST DOING N DIRECTLY                        
         JNE   AARX                                                             
         MVC   CONHEAD(40),=C'Media N REC added. Media T already there'         
         LA    R2,TRAMEDH                                                       
         OI    GENSTAT2,USMYOK     TELL GENCON USE MY MESSAGE                   
         J     EXIT                                                             
*                                                                               
AAR2     L     RE,AIO              POINT TO THE RECORD                          
         MVC   COMPKEY,0(RE)       SAVE THE TV KEY                              
         NI    2(RE),X'F0'                                                      
         OI    2(RE),X'03'         SET MEDIA TO NETWORK                         
         MVC   KEY(13),0(RE)       AND SAVE THE KEY                             
                                                                                
* NOW ADD THE NETWORK RECORD IF IT'S NOT ALREADY THERE                          
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST ALREADY THERE                           
         BNE   AAR4                                                             
         LA    R2,TRAMEDH                                                       
         LHI   R0,NETTHERE         TELL THEM NET CMML ALREADY THERE             
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
AAR4     GOTO1 ADDREC                                                           
*                                                                               
         LA    R2,TRAMEDH                                                       
         MVC   CONHEAD(31),=C'Records added for media T and N'                  
         OI    GENSTAT2,USMYOK     TELL GENCON USE MY MESSAGE                   
                                                                                
* REREAD TV RECORD                                                              
                                                                                
         MVC   KEY(13),COMPKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                WHERE DID IT GO?                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
AARX     DS    0H                  CODE DID NOT PROCESS MODE=XRECADD            
**NOP**  BAS   RE,GENR             SO I'M NOPING THIS CALL MHER 4/14            
         J     EXIT                                                             
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
         SPACE                                                                  
PUT      MVC   AIO,AIO3                                                         
         L     R2,AIO1                                                          
         MVC   KEY,0(R2)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO3                                                          
         CLC   0(256,R2),0(R4)     COMPARE START OF 2 RECS FOR CHANGE           
         BNE   PUT10                                                            
         CLC   =H'256',14(R2)      SEE IF REC LENGTH MORE THAN 256              
         BNL   PUT06                                                            
         LH    R3,14(,R2)          GET LENGTH                                   
         LA    R2,256(,R2)                                                      
         LA    R4,256(,R4)                                                      
         SH    R3,=H'256'                                                       
         LR    R5,R3                                                            
         CLCL  R2,R4                                                            
         BNE   PUT10                                                            
PUT06    MVI   IOOPT,C'Y'                                                       
         B     PUT12                                                            
PUT10    BAS   RE,GENR             GO GENERATE AUTO-TURNAROUND REQ              
PUT12    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING FLTKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRACLT),WORK                                              
         GOTO1 CLUNPK,DMCB,FLTKCLT,WORK                                         
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
*                                                                               
         XC    ELEM,ELEM                                                        
         SPACE                                                                  
         MVI   ELEM+5,3                                                         
         MVC   ELEM+8(3),WORK                                                   
         LA    R2,ELEM                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
         CLI   ERROR,0             ANY ERRORS                                   
         BE    *+6                  NO                                          
         DC    H'0'                                                             
         SPACE                                                                  
* MOVE FLIGHT REC BACK TO AIO1                                                  
         SPACE                                                                  
         L     R0,AIO1                                                          
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
         CLC   TRACLT(3),QCLT                                                   
         BE    *+14                                                             
         MVC   TRACLT(3),QCLT      MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
*NOP     MVC   QCLT,WORK                                                        
         CLC   BCLT,FLTKCLT        IS CLIENT SAME                               
         BE    DK10                YES                                          
         SPACE                                                                  
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BNE   EXIT                                                             
         SPACE                                                                  
DK10     MVC   BPRD,FLTKPRD                                                     
         BAS   RE,PPRD                                                          
         XC    WORK(L'TRAPRD),WORK                                              
         MVC   WORK(L'QPRD),QPRD                                                
         CLC   TRAPRD,WORK                                                      
         BE    *+14                                                             
         MVC   TRAPRD,WORK         MOVE IN PROD                                 
         OI    TRAPRDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,FLTKEDT),(8,ENDATE)                               
         XC    WORK(L'TRAEDT),WORK                                              
         MVC   WORK(8),ENDATE                                                   
         CLC   TRAEDT,WORK                                                      
         BE    *+14                                                             
         MVC   TRAEDT,WORK         MOVE IN END DATE                             
         OI    TRAEDTH+6,X'80'     SET ON TRANSMIT BIT                          
         OC    FILTERS,FILTERS                                                  
         BZ    EXIT                                                             
         BAS   RE,DFTR             DISPLAY FILTER(S)                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GET THIS KEY                             
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    PRINTCTR,PRINTCTR                                                
         LA    R4,KEY              BUILD KEY FOR READHI                         
         USING FLTKEY,R4                                                        
         MVC   FLTKID(2),=XL2'0A27'                                             
         MVC   FLTKAM,BAGYMD                                                    
         MVC   FLTKCLT,BCLT                                                     
         MVC   FLTKPRD,BPRD                                                     
         MVC   FLTKEDT,ENDATE                                                   
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         MVC   P(21),=CL21'NO FLIGHT RECS FOUND'                                
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LR40                YES                                          
LR22     ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   LR40                YES, ALL DONE                                
LR30     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING FLTKEY,R4                                                        
         CLC   BCLT,FLTKCLT        IS CLIENT SAME                               
         BE    LR34                YES                                          
         SPACE                                                                  
         BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BNE   EXIT                ALL DONE                                     
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
LR34     BAS   RE,FTR                                                           
         BNE   LR20                                                             
         SPACE                                                                  
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
LR40     CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BNE   EXIT                NO, JUST EXIT                                
         OC    PRINTCTR,PRINTCTR   WERE ANY RECS LISTED                         
         BNZ   EXIT                YES                                          
         MVC   CONHEAD,=CL60'* NOTE * NO FLIGHT RECS SELECTED *'                
         B     ERREXIT                                                          
LRCLC    CLC   COMPKEY(0),KEY                                                   
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
         SPACE                                                                  
LRR      CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   LRR08               NO                                           
         MVI   ELCODE,C'1'         CK ACTIVITY ELEMENT                          
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR08                                                            
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(5,0),(3,WORK) GET TODAY'S DATE                      
         CLC   ACTVADDT,WORK       WAS ADD TODAY                                
         BNE   LRR02               NO, CK CHANGE                                
         MVC   PPROD+3+132(3),=C'ADD'                                           
         B     LRR04                                                            
LRR02    CLC   ACTVCHDT,WORK       WAS CHANGE TODAY                             
         BNE   LRR08                                                            
         MVC   PPROD+3+132(3),=C'CHG'                                           
LRR04    MVI   PPROD+4,C'*'                                                     
LRR08    MVC   BPRD,FLTKPRD                                                     
         CLI   BPRD,0                                                           
         BE    LRR10                                                            
         BAS   RE,PPRD                                                          
         BAS   RE,FPRDNM                                                        
         MVC   PPROD,QPRD                                                       
         MVC   PPRDNM,PRDNM                                                     
         B     LRR14                                                            
LRR10    MVC   PPRDNM,=CL20'  * ALL PRODUCTS *'                                 
LRR14    GOTO1 DATCON,DMCB,(3,FLTKEDT),(8,PEDT)                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING FLTDTAEL,R6                                                      
         LA    R2,1                                                             
         LA    R5,4                MAX OF 12 FLIGHTS                            
LRR20    LA    R3,3                EDITS PER LINE                               
         LA    R4,PFLTS                                                         
LRR22    EDIT  (R2),(2,(R4)),ZERO=NOBLANK                                       
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,3(R4))                               
         MVI   11(R4),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,FLTEND),(5,12(R4))                                
         LA    R4,23(,R4)                                                       
         BAS   RE,NEXTEL                                                        
         BNE   LRR30                                                            
         LA    R2,1(,R2)                                                        
         BCT   R3,LRR22                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BCT   R5,LRR20                                                         
LRR30    OC    P,P                 ANYTHING TO PRINT                            
         BZ    LRR32               NO, JUST PRINT BLANK LINE                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRR32    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST                                                            
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES                                                    
         MVC   LMED,QMED                                                        
         MVC   LCLT,QCLT                                                        
         MVC   BPRD,FLTKPRD                                                     
         CLI   BPRD,0                                                           
         BE    LRL10                                                            
         BAS   RE,PPRD                                                          
         BAS   RE,FPRDNM                                                        
         MVC   LPROD,QPRD                                                       
         MVC   LPRDNM,PRDNM                                                     
         B     LRL12                                                            
LRL10    MVC   LPRDNM,=CL20'  * ALL PRODUCTS *'                                 
LRL12    GOTO1 DATCON,DMCB,(3,FLTKEDT),(8,LEDT)                                 
         GOTO1 LISTMON                                                          
         LH    R1,PRINTCTR                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,PRINTCTR                                                      
         B     LR20                                                             
         DROP  R4                                                               
         SPACE 3                                                                
* EXPLAIN REASON CODE FOR ACTION ACT                                            
         SPACE                                                                  
EXPR     XC    WORK,WORK                                                        
         CLI   CHREASON,C'T'                                                    
         BNE   EXPR10                                                           
         MVC   WORK(12),=CL12'TURN AROUND'                                      
         B     EXPR20                                                           
EXPR10   CLI   CHREASON,C'N'                                                    
         BNE   EXPR30                                                           
         MVC   WORK(12),=CL12'NORMAL MAINT'                                     
EXPR20   CLI   CHREASON+1,C'A'                                                  
         BNE   EXPR22                                                           
         MVC   WORK+13(3),=C'ADD'                                               
         B     EXIT                                                             
EXPR22   CLI   CHREASON+1,C'C'                                                  
         BNE   EXPR30                                                           
         MVC   WORK+13(6),=C'CHANGE'                                            
         B     EXIT                                                             
EXPR30   MVC   WORK(7),=C'UNKNOWN'                                              
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE END DATE IN KEY, FROM VK                                             
         SPACE                                                                  
VEDT     NTR1                                                                   
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,TRAEDT),DATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,DATE),(3,ENDATE)                                  
         CLI   ACTNUM,ACTADD       IS ACTION ADD                                
         BNE   EXIT                NO, BYPASS ADD CHECKING                      
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING FLTKEY,R4                                                        
         MVC   FLTKID,=XL2'0A27'                                                
         MVC   FLTKAM,BAGYMD                                                    
         MVC   FLTKCLT,BCLT                                                     
         MVC   FLTKPRD,BPRD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      ANY FLT RECS FOR THIS CLIENT/PRD             
         BNE   VEDT20              NO, SET UP ARBITRARY LOW LIMIT               
         MVI   FLTYRSW,C'F'        SET LASTEDT SOURCE TO FLIGHT                 
VEDT10   MVC   LASTEDT,FLTKEDT     SET LOW LIMIT TO THIS REC                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE      SAME CLIENT/PRD                              
         BE    VEDT10              YES SAVE THIS DATE AS LOW                    
         CLC   ENDATE,LASTEDT                                                   
         BNH   ENDTERR                                                          
         B     EXIT                                                             
VEDT20   GOTO1 DATCON,DMCB,(5,0),(3,LASTEDT)                                    
         ZIC   R1,LASTEDT          GET YEAR                                     
         BCTR  R1,0                AND SUBTRACT                                 
         STC   R1,LASTEDT                                                       
         MVC   LASTEDT+1(2),=XL2'0C1E' AND FORCE MO/DA TO 12/30                 
         MVI   FLTYRSW,C'Y'        SET LASTEDT SOURCE TO YEAR                   
         CLC   ENDATE,LASTEDT                                                   
         BNH   ENDTERR                                                          
         MVC   WORK(3),ENDATE      SET LASTEDT TO 1 YEAR PREV TO ENDATE         
         ZIC   R1,WORK             GET YEAR                                     
         BCTR  R1,0                SUBTR 1                                      
         STC   R1,WORK                                                          
         MVC   LASTEDT,WORK                                                     
         MVI   LASTEDT+2,X'01'     ALLOW BACK TO FIRST OF MONTH                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* VALIDATE FILTER - ONLY OPTIONS ARE CHANGE/DELETE - LIMITED TO DDS *           
* TERMINALS                                                         *           
*********************************************************************           
         SPACE                                                                  
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELPA),FTRHELPA                                     
         CLI   1(RA),C'*'          DDS TERMINAL                                 
         BNE   ERREXIT                                                          
         MVC   CONHEAD(L'FTRHELPB),FTRHELPB                                     
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK)                             
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         SPACE                                                                  
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         DATE                                         
         BNE   VFTR20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         L     R6,DMCB             WAS DATE VALID                               
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,DMCB,(0,DATE),(3,DATEFTR)                                 
         CLM   R6,1,1(R4)          WAS THERE ONLY 1 DATE                        
         BE    VFTR18              YES                                          
         LA    R5,1(R6,R5)                                                      
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         L     R6,DMCB             WAS DATE VALID                               
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,DMCB,(0,DATE),(3,DATE2FTR)                                
         B     VFTR70                                                           
VFTR18   MVC   DATESFTR,HOLDSIGN                                                
         B     VFTR70                                                           
VFTR20   CLI   1(RA),C'*'          DDS TERMINAL                                 
         BNE   VFTR80                                                           
*        EX    R1,VFTRCLCB         CHANGE                                       
*        BNE   VFTR30                                                           
*        MVI   CHAFTR,C'C'                                                      
*        B     VFTR70                                                           
VFTR30   EX    R1,VFTRCLCC         DELETE                                       
         BNE   VFTR80                                                           
         MVI   DELFTR,C'D'                                                      
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELPA),FTRMSG                              
         CLI   1(RA),C'*'          DDS TERMINAL                                 
         BNE   ERREXIT                                                          
         MVC   CONHEAD+L'FTRMSG(L'FTRHELPB),FTRHELPB                            
         B     ERREXIT                                                          
VFTRCLCA CLC   12(0,R4),=CL4'DATE'                                              
VFTRCLCB CLC   12(0,R4),=C'CHANGE'                                              
VFTRCLCC CLC   12(0,R4),=C'DELETE'                                              
         EJECT                                                                  
* FILTER - ONLY OPTIONS ARE DATE *                                              
         SPACE                                                                  
         USING FLTKEY,R4                                                        
FTR      NTR1                                                                   
         OC    DATEFTR,DATEFTR                                                  
         BZ    EXIT                                                             
         CLI   DATESFTR,0                                                       
         BNE   FTR10                                                            
         OC    DATE2FTR,DATE2FTR                                                
         BNZ   FTR20                                                            
         CLC   DATEFTR,FLTKEDT                                                  
         BE    EXIT                                                             
         B     FTRNE                                                            
FTR10    CLI   DATESFTR,X'4C'      LESS THAN                                    
         BE    FTR14                                                            
         CLI   DATESFTR,X'6E'      GREATER THAN                                 
         BE    FTR16                                                            
         DC    H'0'                                                             
FTR14    CLC   FLTKEDT,DATEFTR                                                  
         BH    FTRNE                                                            
         B     FTREQ                                                            
FTR16    CLC   FLTKEDT,DATEFTR                                                  
         BL    FTRNE                                                            
         B     FTREQ                                                            
FTR20    CLC   FLTKEDT,DATEFTR                                                  
         BL    FTRNE                                                            
         CLC   FLTKEDT,DATE2FTR                                                 
         BH    FTRNE                                                            
FTREQ    CR    RB,RB                                                            
         B     EXIT                                                             
FTRNE    LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY FILTER - DATE(S), CHANGE, DELETE *                                    
         SPACE                                                                  
DFTR     NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         LR    R3,R2                                                            
         OC    DATEFTR,DATEFTR                                                  
         BZ    DFTR20                                                           
         MVC   0(4,R3),=C'DATE'                                                 
         CLI   DATESFTR,0                                                       
         BE    DFTR04                                                           
         MVC   4(1,R3),DATESFTR                                                 
         LA    R3,1(,R3)                                                        
DFTR04   MVI   4(R3),C'='                                                       
         GOTO1 DATCON,DMCB,(3,DATEFTR),(8,5(R3))                                
         LA    R3,13(,R3)                                                       
         OC    DATE2FTR,DATE2FTR                                                
         BZ    DFTR20                                                           
         MVI   0(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,DATE2FTR),(8,1(R3))                               
         LA    R3,9(,R3)                                                        
*FTR10   CLI   CHAFTR,0                                                         
*        BE    DFTR20                                                           
*        CR    R2,R3                                                            
*        BE    DFTR14                                                           
*        MVI   0(R3),C','                                                       
*        LA    R3,1(,R3)                                                        
*FTR14   MVC   0(6,R3),=C'CHANGE'                                               
*        LA    R3,6(,R3)                                                        
DFTR20   CLI   DELFTR,0                                                         
         BE    DFTR30                                                           
         CR    R2,R3                                                            
         BE    DFTR24                                                           
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
DFTR24   MVC   0(6,R3),=C'DELETE'                                               
DFTR30   CLC   TRAFLTR,WORK                                                     
         BE    EXIT                                                             
         MVC   TRAFLTR,WORK                                                     
         OI    TRAFLTRH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE START AND END DATES                                                  
         SPACE                                                                  
         USING FLTDTAEL,R6                                                      
VFLT     NTR1                                                                   
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         LA    R3,1(R4,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(3,FLTSTART)                                
         GOTO1 ADDAY,(R1),DATE,WORK,F'366'                                      
         GOTO1 DATVAL,(R1),(R3),DATE                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         SPACE                                                                  
         CLC   DATE,WORK           MUST NOT COVER MORE THAN 1 YR                
         BH    PERSIZER                                                         
         SPACE                                                                  
         GOTO1 DATCON,(R1),(0,DATE),(3,FLTEND)                                  
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   EXIT                NO EXISTING, ADD THIS                        
VFLT10   CLC   FLTSTART(6),ELEM+2  CK THIS DATE PAIR TO EXISTING                
         BE    DATERR              THIS EXISTS, ERROR                           
         BL    VFLT20              GO CK NO OVERLAP                             
         CLC   FLTSTART,ELEM+5     EXIST TO NEW END                             
         BNH   DATERR                                                           
         B     VFLT30                                                           
VFLT20   CLC   FLTEND,ELEM+2       EXIST END TO NEW START                       
         BNL   DATERR              OVERLAP                                      
VFLT30   BAS   RE,NEXTEL                                                        
         BE    VFLT10                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FIND LAST END DATE                                                            
         SPACE                                                                  
FEDT     NTR1                                                                   
         XC    LASTEDT,LASTEDT     CLEAR LAST END DATE                          
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         MVC   KEY(6),0(R4)        ID, BAGYMD, CLT, PRD                         
         XC    KEY+6(7),KEY+6                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FEDT20                                                           
FEDT10   CLC   KEY(9),0(R4)                                                     
         BNL   FEDT12                                                           
         MVC   LASTEDT,KEY+6                                                    
         GOTO1 SEQ                                                              
         B     FEDT10                                                           
FEDT12   OC    LASTEDT,LASTEDT     WAS LAST END DATE FOUND                      
         BNZ   FEDT30              YES, ALL DONE                                
FEDT20   MVC   LASTEDT,ENDATE      MAKE LAST 1 YEAR PREV TO END DATE            
         ZIC   R1,LASTEDT          GET YEAR                                     
         BCTR  R1,0                AND SUBTRACT 1                               
         STC   R1,LASTEDT                                                       
         MVI   LASTEDT+2,X'01'     ALLOW BACK TO FIRST OF MONTH                 
FEDT30   MVC   KEY(L'SVKEY),SVKEY                                               
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    FEDT40              BYPASS GETREC                                
         GOTO1 GETREC              REPOSITION                                   
FEDT40   MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
         USING FLTKEY,R4                                                        
FCLT10   DS    0H                                                               
         MVC   BCLT,FLTKCLT                                                     
         DROP  R4                                                               
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
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
         CLI   ERROR,0             ANY ERRORS                                   
         BE    FCLT30               NO                                          
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   EXIT                                                             
         SPACE                                                                  
* DO GETREC & THEN SAVE REC                                                     
         SPACE                                                                  
         GOTO1 GETREC                                                           
         B     FCLT10                                                           
         SPACE                                                                  
* MOVE FLIGHT REC BACK TO AIO1, DO GETPROF                                      
         SPACE                                                                  
FCLT30   DS    0H                                                               
         SPACE                                                                  
         L     R0,AIO1                                                          
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    FCLT40              BYPASS                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
FCLT40   MVC   AIO,AIO1                                                         
         CR    RB,RB               SET COND CODE OKAY                           
         B     EXIT                                                             
         EJECT                                                                  
* PRINT PRODUCT CODE                                                            
         SPACE                                                                  
PPRD     NTR1                                                                   
         XC    QPRD,QPRD                                                        
         CLI   BPRD,0              ANY PRODUCT TO LOOK UP                       
         BE    EXIT                NO, GET OUT                                  
         L     R1,ASVCLIST                                                      
PPRD10   CLI   0(R1),C' '                                                       
         BL    PPRD16                                                           
         CLC   BPRD,3(R1)                                                       
         BE    PPRD20                                                           
         LA    R1,4(R1)                                                         
         B     PPRD10                                                           
         SPACE                                                                  
PPRD16   LA    R1,=C'???'                                                       
         SPACE                                                                  
PPRD20   MVC   QPRD,0(R1)                                                       
         B     EXIT                                                             
         SPACE 3                                                                
* FIND PRODUCT CODE                                                             
         SPACE                                                                  
FPRDNM   NTR1                                                                   
         CLC   QPRD,=C'???'                                                     
         BE    FPRDNM10                                                         
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVC   FILENAME,=C'SPTDIR'   SWITCH TO SPT SYSTEM                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'SPTFIL'   SWITCH TO SPT SYSTEM                       
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   PRDNM,PNAME-PRDHDRD(R1)                                          
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         SPACE                                                                  
FPRDNM10 MVC   PRDNM,=CL20'UNKNOWN PRODUCT'                                     
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
         SPACE                                                                  
GENR     NTR1                                                                   
         CLI   SVPROF+9,C'Y'       AUTO TURNAROUND                              
         BE    GENR10              YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BNE   EXIT                NO                                           
GENR10   XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),AGENCY                                              
         MVC   REQUEST+4(23),=CL23'*.FLI.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(1),QMED                                               
         MVI   REQUEST+28,C'.'                                                  
         MVC   REQUEST+29(3),QCLT                                               
         MVC   REQUEST+32(2),=C'.*'                                             
         CLI   SVPROF+9,C'D'       IF REQ IS DDS                                
         BE    *+10                BYPASS                                       
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         SPACE 2                                                                
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   EXIT                   NO                                        
         MVC   H3+39(11),=C'TURN-AROUND'                                        
         B     EXIT                                                             
         SPACE                                                                  
* CLEAR DISPLAY AREA OF SCREEN *                                                
         SPACE                                                                  
CLRSCR   LA    RF,TRAFL01H                                                      
*                                                                               
CLRSCR10 TM    1(RF),X'20'         PROTECTED                                    
         BO    CLRSCR20                                                         
         OC    8(L'TRAFL01,RF),8(RF)                                            
         BZ    CLRSCR30                                                         
         CLC   8(L'TRAFL01,RF),SPACES                                           
         BE    CLRSCR30                                                         
CLRSCR20 XC    8(L'TRAFL01,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         NI    1(RF),X'FF'-X'20'                                                
*                                                                               
CLRSCR30 ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         LA    R0,TRATAGH                                                       
         CR    RF,R0                                                            
         BL    CLRSCR10                                                         
         BR    RE                                                               
         EJECT                                                                  
DATSQER  XC    CONHEAD,CONHEAD     DATE PAIR OUT OF SEQ                         
         MVC   CONHEAD(L'DATSQMS),DATSQMS                                       
         B     ERREXIT                                                          
ENDTEEX  XC    CONHEAD,CONHEAD     DATES ARE PAST FLIGHT END DATE               
         MVC   CONHEAD(L'DTEXMS),DTEXMS                                         
         B     ERREXIT                                                          
PERSIZER XC    CONHEAD,CONHEAD     DATES ARE MORE THAN 1 YEAR APART             
         MVC   CONHEAD(L'PERSIZMS),PERSIZMS                                     
         B     ERREXIT                                                          
ENDTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ENDTEMS),ENDTEMS                                       
         LA    R0,CONHEAD+10                                                    
         GOTO1 DATCON,DMCB,(3,LASTEDT),(8,(R0))                                 
         CLI   FLTYRSW,C'F'        WAS LASTEDT SOURCE FLIGHT                    
         BNE   ERREXIT             NO, YEAR                                     
         MVC   CONHEAD+42(6),=CL6'FLIGHT'                                       
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
INVPRDER MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ENDTEMS  DC    CL48'* ERROR * MMMDD/YR WAS END DATE FROM LAST YEAR   *'         
*                                                             FLIGHT            
DATSQMS  DC    C'* ERROR * DATE PAIR OUT OF SEQUENCE *'                         
DTEXMS   DC    C'* ERROR * DATE PAIR PAST END DATE OF FLIGHT *'                 
PERSIZMS DC    C'* ERROR * NO FLIGHT CAN BE MORE THAN 1 YEAR *'                 
FTRMSG   DC    C'* ERROR * '                                                    
FTRHELPA DC    C'VALID FILTERS - DATE='                                         
FTRHELPB DC    C'VALID FILTERS - DATE/DELETE='                                  
*TRHELPB DC    C'VALID FILTERS - DATE/CHANGE/DELETE='                           
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,36,C'F L I G H T  L I S T'                                    
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,36,C'--------------------'                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'PROD'                                                     
         SSPEC H9,3,C'----'                                                     
         SSPEC H8,10,C'PRODUCT NAME'                                            
         SSPEC H9,10,C'--------------------'                                    
         SSPEC H8,34,C'END DATE'                                                
         SSPEC H9,34,C'--------'                                                
         SSPEC H8,45,C'FLIGHT PERIODS'                                          
         SSPEC H9,45,C'--------------------'                                    
         SSPEC H9,68,C'--------------------'                                    
         SSPEC H9,91,C'--------------------'                                    
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAFAD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
FLD      DS    CL8                                                              
ENDATE   DS    XL3                                                              
DATE     DS    CL6                                                              
DATESET  DS    CL32                                                             
VALDTESW DS    XL1                                                              
FLTYRSW  DS    CL1                 IF LAST EDT IS END OF LAST YEAR = Y          
*                                  IF LAST EDT IS FROM LAST FLIGHT = F          
LASTEDT  DS    XL3                                                              
COMPKEY  DS    CL13                COMPARE KEY FOR ONLINE LIST                  
COMPKEYL DS    CL1                                                              
PRINTCTR DS    H                                                                
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
FILTERS  DS    0CL9 WAS 10                                                      
DATEFTR  DS    XL3                                                              
DATE2FTR DS    XL3                                                              
DATESFTR DS    CL1                                                              
*HAFTR   DS    CL1                                                              
DELFTR   DS    CL1                                                              
HOLDSIGN DS    CL1                                                              
         SPACE                                                                  
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PPROD    DS    CL3                                                              
         DS    CL3                                                              
PPRDNM   DS    CL20                                                             
         DS    CL4                                                              
PEDT     DS    CL8                                                              
         DS    CL3                                                              
PFLTS    DS    CL66                                                             
         SPACE                                                                  
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMED     DS    CL1                                                              
         DS    CL3                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPROD    DS    CL3                                                              
         DS    CL2                                                              
LPRDNM   DS    CL20                                                             
         DS    CL2                                                              
LEDT     DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPTRA0A   05/07/14'                                      
         END                                                                    
