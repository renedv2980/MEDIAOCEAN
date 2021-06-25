*          DATA SET SPTRA22    AT LEVEL 126 AS OF 10/16/19                      
*PHASE T21622A                                                                  
T21622   TITLE 'NETWORK COMMERCIAL ADD/DIS/CHG/DEL/LIST'                        
                                                                                
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ CML SEQ RECORD FOR ADDS (IN PAR RTN)                  
*                    READ IN PRDHDR FOR PROD NAMES IN OFFLINE LIST              
*                    READ IN ACTUAL ID REC IN VID                               
*                    READ IN ACTUAL ID REC IN UPACT (XRECADD/XRECPUT)           
*             AIO3 - REC READ IN FOR CHANGE COMPARE                             
*                    REC READ IN FOR CCUSA CML CHECKING                         
*                    TABLE OF STARCOM PIGGYBACK COMMLS                          
*                    TABLE OF VALIDATED PRODS TO BE DISPLAYED (IN VR)           
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG & A(SORT ENTRY) IN LR                                    
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
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
         EJECT                                                                  
***********************************************************************         
* LEV  76 MNAS SEP20/06 REMOVE RESTRICTION THAT FIRST FOUR POSITIONS  *         
*                       OF ADID BE ALPHA                              *         
* LEV  77 MNAS NOV01/06 REPLACE RESTRICTION THAT FIRST FOUR POSITIONS *         
*                       OF ADID BE ALPHA                              *         
* LEV  77 MNAS NOV30/06 ADD MATCHMAKER FIELDS TO COMML SCREEN         *         
* LEV  78 SMUR MAR13/07 FIX NO PRD ERR MSG, CHG DUP PRD ERR MSG       *         
*                       ADD FILTER BY ADID                            *         
* LEV  79 SMUR AUG09/07 STOP OFFLINE DUMPS FOR CMLS W/DEL PRODS       *         
* LEV  83 MNAS AUG21/07 BUG IN ADID/ALLOWING NUMERIC IN POS 1-4       *         
* LEV  85 MNAS DEC03/07 FIX BUG IN READING COMML CLASS RECORDS        *         
*                       INTRODUCED WITH MORE BRANDS CODE              *         
* LEV  86 SMUR DEC05/07 NO CHGING ADID ERROR MSG, DO NOT PROTECT ADID *         
* LEV  87 MNAS APR30/08 ADD NEW FIELDS TO REC FOR HIDEF/CENTERCUT     *         
*                       CREATE PASSIVES FOR HIDEF/CENTERCUT           *         
*                       ACCEPT 9-12 CHAR ADID CODES                   *         
*                       ADD NEW CHANGE HISTORY SCREEN/NEW PFKEYS      *         
* LEV  90 MNAS JUN06/08 BUG FIXES FOR ADDING HIDEC/CC POINTERS        *         
* LEV  91 MNAS JUN16/08 FIX ERROR MSG IN VALCMML                      *         
* LEV  92 MNAS JUN26/08 CHANGES RULES FOR CHECKING DUPLICATES IN      *         
*                       ADID/HIDEF/CC : CANNOT CHECK CC AGAINST ADID  *         
* LEV  94 MNAS JUL17/08 ADDITIONAL CHECKS FOR HIDEF AND CENTERCUT     *         
*                       WHEN INDICATING A KEY NEEDS TO BE ADDED       *         
* LEV  95 MNAS JUN21/08 ONE ADDITIONAL CLEAR ON CENTERCUT KEY         *         
* LEV 102 SMUR AUG08/08 PF6 FOR COMTEXT, HIDEF, CENTERCUT FILTERS     *         
*                       AND PRINT ON REPORT                           *         
*                       MOVE SOME CODE TO ITS OWN ROUTINES            *         
* LEV 103 MNAS OCT01/08 MOVE ADID POINTER ADD CODE TO BE DONE AFTER   *         
*                       REC CHANGES COMPLETE (IN MODE XRECPUT)        *         
* LEV 104 MNAS OCT15/08 BUG FIX - DISPLAY EXITS AFTER PROCESSING      *         
*                       NETWORK FIELDS - FIELDS MISSING FROM SCREEN   *         
* LEV 105 MNAS OCT21/08 RELAX ADID INPUT RESTRICTIONS:ALL ALPHA/NUMER *         
* LEV 106 MNAS OCT23/08 BUG FIX IN DR EXIT ROUTINE WHEN NO '24'EL     *         
* LEV 107 SMUR OCT10/08 PRINT  COMTEXT PRESENT MESSAGE ON REPORT      *         
*                       FIX BUGS IN THE REPORT                        *         
* LEV 108 SMUR JAN15/08 BUG FIX WHEN INVALID DESTROY TIME IS ENTERED  *         
* LEV 111 MHER MAY/09   ADID SUPPORT                                  *         
* LEV 116 MNAS NOV13/09 THERE SHOULD BE NO A0 ELEMENT ON COMMERCIAL   *         
*                       RECORDS WITH 8 CHAR ISCII IN KEY              *         
* LEV 117 MNAS MAR26/10 STAT BIT IN KEY IS BEING SET TO INDICATE ADID *         
*                       IN KEY WHEN THEY ARE 8 CHAR ISCII'S - THIS IS *         
*                       HAPPENING WHEN C SELECT FROM LIST             *         
* LEV 118 MNAS JUL14/10 PREVENT 9-2 COMML CODE = 9-12 ADID CODE       *         
* LEV 121 MNAS FEB17/12 CL0356156N - FIX BUG IN TN1+6=S               *         
* LEV 123 SMUR OCT10/13 ADD CML 99999999 PRODUCTION HOUSE RECORD      *         
* LEV 125 SMUR OCT10/13 FIX BAD BRANCH IN DR WHEN NO X'24' ELEM       *         
*                       TURN OFF CML CHANGED IN OPTICA FLAG           *         
*                       FIX DISPLAY OF TAL TRANSFER EXCL FIELD        *         
* SPEC-34650  SMUR REPORT FOR ALL CLIENTS WITH VARIOUS DETAILS        *         
*                         *** TERMINUS ***                            *         
***********************************************************************         
         EJECT                                                                  
T21622   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1622**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR22RR                                                      
*                                                                               
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   MAIN20                                                           
         BRAS  RE,DR                                                            
         B     EXIT                                                             
*                                                                               
MAIN20   CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECADD         PRIOR TO ADD RECORD                          
         BE    PAR                                                              
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    AAR                                                              
         CLI   MODE,RECPUT         BEFORE REWRITING, CK IF REQ NEEDED           
         BE    BPUT                                                             
         CLI   MODE,RECDEL         BEFORE DELETE RECORD (INVALID CML)           
         BE    DELREC                                                           
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BNE   EXIT                                                             
         BRAS  RE,APUT                                                          
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       CLI   ACTNUM,ACTDEL       DELETE IS INVALID                            
         BE    DELREC                                                           
*                                                                               
         CLI   ACTNUM,ACTADD       CLEAR HELP ON ACTN ADD                       
         BNE   VK05                                                             
         BRAS  RE,CLRHELP                                                       
*                                                                               
VK05     LA    R2,TRAMEDH                                                       
         GOTO1 VALIMED                                                          
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
*                                                                               
         NI    FLAGS2,X'FF'-ALLCLTR INIT ALL CLIENT REQUEST                     
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BNE   VK12                                                             
*                                                                               
         OI    FLAGS2,ALLCLTR      ALL CLIENT REQUEST                           
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK11                                                             
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
VK11     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         B     MISSERR                                                          
VK12     GOTO1 VALICLT                                                          
*                                                                               
         L     R6,AIO1                                                          
         MVC   SVCLTINT,CCLTIFC-CLTHDR(R6)                                      
*                                                                               
         BRAS  RE,GETPRF           GET PROFILES                                 
*                                                                               
* VALIDATE FAX & OPTIONS FIELDS BEFORE PERIOD *                                 
*                                                                               
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
VK20     LA    R2,TRACMLH          COMMERCIAL IDENTIFICATION                    
         CLI   TRACMLH+5,0                                                      
         BE    VK21                                                             
         TM    FLAGS2,ALLCLTR      ALL CLIENT REQUEST                           
         BZ    VK21                                                             
         LA    R2,TRACLTH          MUST ENTER CML FOR ALL CLT                   
         B     MISSERR                                                          
*                                                                               
VK21     XC    HOLDCML,HOLDCML                                                  
         XC    HOLDCM12,HOLDCM12   SAVE CML AS INPUT                            
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK22                                                             
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
*NOP     CLC   TRACML(8),=8C'9'                                                 
******   BE    INV9999                                                          
*                                                                               
VK22     DS    0H                                                               
         BRAS  RE,VCML             VALIDATE COMMERCIAL                          
         MVC   HOLDCML,WORK                                                     
         MVC   HOLDCM12,TRACML     SAVE CML AS INPUT                            
         OI    4(R2),X'20'         SET CMML VALIDATED                           
*                                                                               
         LA    R2,TRAFLTRH         VALIDATE ANY FILTERS                         
         BRAS  RE,VFTR                                                          
         EJECT                                                                  
* BUILD KEY                                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,HOLDCML                                                  
* ON ADD, NEED TO MAKE SURE ADID IS NOT ALREADY ON FILE                         
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK24                                                             
         MVC   MYKEY,KEY           SAVE KEY JUST BUILT                          
         MVC   CMLKID,=X'0AC1'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF KEYS EQUAL                                
         BE    VK24                EXIT TO GIVE REC EXISTS ERRPR                
         MVC   KEY,MYKEY           ELSE RESTORE KEY BEING ADDED                 
*                                                                               
VK24     CLI   ACTNUM,ACTLIST      IF LIST                                      
         BNE   EXIT                NOT, SO EXIT                                 
*                                                                               
* CHECK FOR ANY MISSING FIELDS (MUST ALL BE ENTERED, LEFT TO RIGHT)             
*                                                                               
         OC    HOLDCML,HOLDCML     IF ANY ENTRY                                 
         BZ    VK40                NO                                           
         OC    BCLT,BCLT           MUST HAVE ENTERED CLT                        
         BNZ   VK40                DID                                          
         LA    R2,TRACLTH          ERROR                                        
         B     MISSERR             MISSING CLIENT ENTRY                         
*                                                                               
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
*                                                                               
VK40     LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF CMLKCML                      
VK42     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   VK44                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,VK42                                                          
VK44     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         MVC   SVDSKADR,DMDSKADD                                                
         XC    CHGFLAGS,CHGFLAGS    TEST ANY CHANGES                            
         XC    SVPPKEY(SVPPKEYL),SVPPKEY   CLEAR PASSIVE KEY STORAGE            
                                                                                
         L     RE,AIO1             SAVE ORIGINAL RECORD IN AIO1+2000            
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LA    R0,2000(RE)                                                      
         LA    R1,2(RF)                                                         
         MVCL  R0,RE                                                            
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         CLI   TWASCR,X'6F'        BROADCST BUSINESS SCREEN ?                   
         BE    VRB                                                              
*                                                                               
         CLI   TWASCR,X'9F'        LEGAL SCREEN ?                               
         BE    VRL                                                              
*                                                                               
         CLC   =C'SJ',AGENCY       AGENCY IS SJR TEST                           
         BE    VR06                                                             
         CLC   =C'GZ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    VR04                                                             
         CLC   =C'H9',AGENCY       AGENCY IS STARCOM                            
         BNE   VR06                                                             
VR04     DS    0H                                                               
         TM    TWAAUTH+1,X'02'     AUTHORIZED TO CHANGE COMML SCREEN            
         BO    AUTHERR              NO, AUTHORIZATION ERROR                     
*                                                                               
VR06     DS    0H                                                               
         BRAS  RE,CLRHELP                                                       
         MVI   FLAGS,0                                                          
         MVI   BYTE,0              INIT GENERATE PATTER T/A REQ                 
*                                                                               
         L     R4,AIO              SAVE KEY CONTENTS                            
         USING CMLKEY,R4                                                        
         MVC   CMLAGYA,AGENCY                                                   
         CLC   BCLT,CMLKCLT        IS CLIENT SAME                               
         BE    VR10                 YES                                         
*                                                                               
         BRAS  RE,FCLT             GET CLIENT CLIST                             
         BE    *+6                                                              
         DC    H'0'                                                             
VR10     MVC   HOLDCML,CMLKCML                                                  
         DROP  R4                                                               
*                                                                               
* THE 2 PLST FIELDS OF 58 BYTES EACH WILL HOLD A MAX OF 19 2 BYTE               
* PRODUCT CODES FOR A TOTAL POSSIBLE PRODUCT LIST ELEM OF 38 ENTRIES            
* OR AN ELEMENT LENGTH OF 116 BYTES                                             
                                                                                
         XC    HEAD1,HEAD1                                                      
         XC    HEAD2,HEAD2                                                      
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'        SAVE OFF ELEMENT FOR LATER                   
         BRAS  RE,GETEL            COMPARISON TO LOG CHANGES                    
         BNE   VR14                                                             
         ZIC   R2,1(R6)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   HEAD1(0),0(R6)                                                   
                                                                                
VR14     LA    R2,TRAPLSTH         PRODUCT LIST                                 
         MVI   ELCODE,X'29'        MORE PROD LIST ELEM CODE                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'20'        PROD LIST ELEM CODE                          
         GOTO1 REMELEM                                                          
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    VR20                 YES, ONLY VAL TITLE (PROD HOUSE)            
*                                                                               
         BRAS  RE,VPRDL            VALIDATE PROD LIST & BUILD ELEM              
                                                                                
         CLC   MORPRDEL,HEAD1                                                   
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_PRDL                                              
                                                                                
VR20     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         OI    FLAGS,ISCOVCML      SET IS COVER CMML                            
*                                                                               
         MVI   ELCODE,X'10'        DATA ELEMENT                                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING CMLDTAEL,R6                                                      
         OC    CMLCOVCT,CMLCOVCT   IS THIS A COVD (ACT) CML?                    
         BZ    *+8                                                              
         OI    FLAGS,ISCOVERD                                                   
         MVI   CMLDTAEL,X'10'      FORMAT ELEMENT IDENTIFIER                    
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL   AND ELEMENT LENGTH FOR ADD           
*                                                                               
         LA    R2,TRADSC1H         COMMERCIAL TITLE                             
         GOTO1 ANY                                                              
         CLC   DELETE,WORK         SOFT DELETE THIS CML                         
         BNE   VR22                NO                                           
         TM    FLAGS,ISCOVERD      IS CMML COVERED?                             
         BZ    *+14                 NO                                          
         MVC   GERROR,=Y(CMLCOVRD)                                              
         B     TRAPERR2                                                         
*                                                                               
         OI    FLAGS,SOFTDEL                                                    
         OI    CMLSTAT,X'80'       SOFT DELETE RECORD                           
         B     VR26                DO NOT OVERLAY TITLE                         
VR22     CLC   =C'RESTORE',WORK    RESTORE SOFT DELETE?                         
         BNE   VR24                NO                                           
         TM    CMLSTAT,X'80'       WAS CML SOFT DELETED                         
         BNZ   *+14                                                             
         MVC   GERROR,=Y(NORESTOR)                                              
         B     TRAPERR2                                                         
*                                                                               
         NI    CMLSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         OI    FLAGS,SOFTREST                                                   
         B     VR26                                                             
VR24     MVC   CMLTITLE,WORK                                                    
*                                                                               
VR26     BRAS  RE,VHSE             GO VALIDATE PROD HOUSE IF NEEDED             
*                                                                               
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    VR80                YES, ONLY VAL TITLE (PROD HOUSE)             
*                                                                               
* VALIDATE TYPE BEFORE CML LEN FOR SLIDE LEN ALL *                              
*                                                                               
         LA    R2,TRATYPEH         TYPE                                         
         BRAS  RE,VTYP                                                          
                                                                                
         CLC   CMLTYPE,WORK                                                     
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_TYPE                                              
                                                                                
         MVC   CMLTYPE,WORK        STORE TYPE                                   
*                                                                               
         LA    R2,TRASLNH                                                       
         BRAS  RE,VSLN                                                          
*                                                                               
         NI    CMLOPFLG,X'FF'-CMLOPCHG  TURN OFF CML CHANGED IN OPTICA          
*                                                                               
         LA    R2,TRAOVRDH                                                      
         BRAS  RE,VOVRD                                                         
         EJECT                                                                  
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VR30                                                             
*                                                                               
         XC    WORK(6),WORK                                                     
         MVC   WORK(6),CMLRLSE     SAVE RELEASE/RECALL DATES                    
         MVC   SVCMLSTR,CMLRLSE    NEED RELEASE DATE FOR LATER                  
                                                                                
VR30     LA    R2,TRARLSEH         RELEASE DATE - REQUIRED                      
         MVC   WORK(L'CMLRLSE),CMLRLSE                                          
         XC    CMLRLSE,CMLRLSE                                                  
         GOTO1 DATVAL,DMCB,(0,TRARLSE),DATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,CMLRLSE)                                 
                                                                                
         CLC   CMLRLSE,WORK                                                     
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SDAT                                              
                                                                                
         LA    R2,TRARCLH          RECALL DATE                                  
         MVC   WORK(L'CMLRCL),CMLRCL                                            
         XC    CMLRCL,CMLRCL                                                    
         CLC   =CL3'UFN',TRARCL    UNTIL FURTHUR NOTICE                         
         BNE   VR34                NO, PROCESS IT                               
         CLI   SVT1PROF+11,C'Y'    UFN IS INVALID                               
         BE    RCLERR               YES, ERROR                                  
*                                                                               
         MVC   CMLRCL,=XL3'FFFFFF' FORCE IT                                     
         B     VR36                DONE                                         
VR34     GOTO1 DATVAL,DMCB,(0,TRARCL),DATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,CMLRCL)                                  
         CLC   CMLRLSE,CMLRCL      CAN'T RECALL BEFORE RELEASE                  
         BH    DATERR                                                           
                                                                                
VR36     DS    0H                                                               
         CLC   CMLRCL,WORK                                                      
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_EDAT                                              
                                                                                
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VR38                                                             
*                                                                               
         CLC   CMLRLSE,WORK        LATER RELEASE DATE                           
         BH    *+14                                                             
         CLC   CMLRCL,WORK+3       OR EARLIER RECALL DATE                       
         BNL   VR38                                                             
*                                                                               
         MVI   BYTE,1              GENERATE PATTERN T/A REQ                     
*                                                                               
VR38     LA    R2,TRACLTNH         CLIENT COMMERCIAL NUMBER                     
         XC    WORK,WORK                                                        
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         GOTO1 ANY                                                              
                                                                                
VR40     CLC   CMLCLTNO,WORK                                                    
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_CLNO                                              
         MVC   CMLCLTNO,WORK                                                    
                                                                                
*                                                                               
VR64     LA    R2,TRASOLOH         PIGGYBACK/SOLO                               
         MVI   WORK,0                                                           
         MVC   BYTE,CMLSOLO                                                     
         MVI   CMLSOLO,0                                                        
         CLI   5(R2),0                                                          
         BE    VR66                                                             
         MVC   WORK(1),8(R2)                                                    
         CLI   8(R2),C'S'                                                       
         BE    VR66                                                             
         CLI   8(R2),C'P'                                                       
         BE    VR66                                                             
         MVC   GERROR,=Y(SOLOERR)                                               
         B     TRAPERR2                                                         
*                                                                               
VR66     DS    0H                                                               
         CLC   BYTE,WORK                                                        
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_OTHR                                              
         MVC   CMLSOLO,WORK                                                     
*                                                                               
VR68     TM    4(R2),X'80'         INPUT THIS TIME                              
         BZ    VR70                                                             
*                                                                               
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VR70                                                             
*                                                                               
         MVI   BYTE,1              GENERATE PATTERN T/A REQ                     
*                                                                               
         EJECT                                                                  
VR70     LA    R2,TRACLSH          CLASS                                        
         MVC   WORK(L'CMLCLASS),CMLCLASS                                        
         XC    CMLCLASS,CMLCLASS                                                
         CLI   5(R2),0                                                          
         BNE   VR72                                                             
*                                                                               
         CLI   SVTN1PR8,C'Y'       IS COMMERCIAL CLASS REQUIRED                 
         BE    MISSERR                                                          
         CLI   SVTN1PR8,C'V'       IS COMML CLASS REQUIRED & VALIDATED          
         BE    MISSERR                                                          
         B     VR74                                                             
*                                                                               
VR72     DS    0H                                                               
         CLI   5(R2),4                                                          
         BNH   *+14                                                             
         MVC   GERROR,=Y(BADCLS)                                                
         B     TRAPERR2                                                         
                                                                                
         CLC   WORK(L'CMLCLASS),8(R2)                                           
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_CLS                                               
                                                                                
         MVC   CMLCLASS,8(R2)                                                   
*                                                                               
         CLI   SVTN1PR8,C'V'       IS COMMERCIAL CLASS VALIDATED                
         BNE   VR74                                                             
*                                                                               
         BRAS  RE,VCLS             GO VALIDATE CLASS                            
*                                                                               
VR74     LA    R2,TRATTEXH         TALENT TRANSFER EXCLUDE                      
         CLI   5(R2),0                                                          
         BE    VR76                                                             
         CLI   8(R2),C'Y'          YES, EXCLUDE FROM TALENT TRANSFER            
         BE    VR76                                                             
         CLI   8(R2),C'N'                                                       
         BNE   TALTRNER                                                         
VR76     DS    0H                                                               
         CLC   CMLTALEX,8(R2)                                                   
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_TAL                                               
         MVC   CMLTALEX,8(R2)                                                   
                                                                                
VR80     GOTO1 ADDELEM                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR82                                                             
         BRAS  RE,DUPCHK                                                        
         CLI   TRACML+8,C' '       ONLY ADD A0 EL IF TRUE ADID                  
         BNH   VR82                                                             
         BRAS  RE,AADIDEL          INSERT ADIDEL IN RECORD ON ADD               
*                                                                               
VR82     CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    VR151                                                            
*                                                                               
         BRAS  RE,VALMAT                                                        
                                                                                
VR82C    LA    R2,TRADSC1H         1ST LINE EXT DESCRIPTION                     
         GOTO1 ANY                                                              
*                                                                               
* IF DELETE, LEAVE TITLES ALONE, IF RESTORE, VALIDATE ACT CMLS IF COV *         
*                                                                               
         TM    FLAGS,SOFTDEL       SOFT DELETE THIS CML                         
         BNZ   VRDONE               YES - NO OTHER CHANGES                      
         TM    FLAGS,SOFTREST      RESTORE SOFT DELETE?                         
         BZ    *+16                 NO, CONTINUE                                
         TM    FLAGS,ISCOVCML      IS THIS A COVER?                             
         BNZ   VR100                YES - MAKE SURE ACTUALS STILL VALID         
         B     VRDONE               NO - DISALLOW OTHER CHANGES                 
*                                                                               
* ACCEPT NO CHANGES ON A DELETED CMML                                           
*                                                                               
         TM    CMLSTAT,X'80'                                                    
         BZ    *+18                                                             
         MVC   GERROR,=Y(CMLISDEL)                                              
         LA    R2,TRACMLH                                                       
         B     TRAPERR2                                                         
                                                                                
         BRAS  RE,VALTTL                                                        
                                                                                
*----------------------------------------------------------------*              
* VALIDATE HIDEF COMMERCIAL                                                     
*----------------------------------------------------------------*              
VRHD     DS    0H                                                               
         MVC   NWHIDEFX,SVHIDEFX                                                
         XC    SVHDADDK,SVHDADDK                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VRHD005                                                          
         XC    SVHIDEF,SVHIDEF                                                  
         XC    SVHIDEFX,SVHIDEFX                                                
         B     VRHD010                                                          
                                                                                
VRHD005  LA    R2,TRAHDEFH                                                      
         OC    TRAHDEF,SPACES                                                   
         CLC   SVHIDEF,8(R2)                                                    
         BE    VRPC                                                             
                                                                                
VRHD010  XC    WORK,WORK                                                        
         XC    NWHIDEFX,NWHIDEFX                                                
         LA    R2,TRAHDEFH                                                      
         CLI   5(R2),0                                                          
         BE    VRHD050                                                          
         CLI   5(R2),9                                                          
         BL    VHD912ER                                                         
                                                                                
         LLC   R0,5(R2)            MAKE SURE NO SPCL CHARS                      
         LA    R1,8(R2)                                                         
                                                                                
VRHD020  CLI   0(R1),C'A'                                                       
         BL    VRHDERR                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VRHD030                                                          
         CLI   0(R1),C'0'                                                       
         BL    VRHDERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    VRHDERR                                                          
                                                                                
VRHD030  LA    R1,1(R1)                                                         
         BCT   R0,VRHD020                                                       
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(12),8(R2)                                                   
         OC    WORK(12),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         BNE   VRHDERR                                                          
                                                                                
         MVC   NWHIDEFX,WORK+12                                                 
                                                                                
         OC    TRAHDEF,SPACES                                                   
         OC    TRAADID,SPACES                                                   
         OC    TRACNTR,SPACES                                                   
         OC    TRACML,SPACES                                                    
                                                                                
         CLC   TRAHDEF,TRACML                                                   
         BE    VRHDSCR                                                          
         CLC   TRAHDEF,TRAADID                                                  
         BE    VRHDSCR                                                          
         CLC   TRAHDEF,TRACNTR                                                  
         BE    VRHDSCR                                                          
                                                                                
         BRAS  RE,HDDUPS                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    VRHDDUP                                                          
                                                                                
VRHD050  DS    0H                                                               
         XC    SVHDDELK,SVHDDELK                                                
         OC    SVHIDEF,SVHIDEF                                                  
         BZ    VRHD070                                                          
         MVC   SVHDDELK(2),=X'0AC2'                                             
         MVC   SVHDDELK+2(3),BAGYMD                                             
         MVC   SVHDDELK+5(8),SVHIDEFX                                           
*        BRAS  RE,DELPSSV                                                       
                                                                                
VRHD070  DS    0H                                                               
         XC    SVHDADDK,SVHDADDK                                                
         LA    R2,TRAHDEFH                                                      
         CLI   5(R2),0                                                          
         BE    VRPC                                                             
                                                                                
         XC    SVHDADDK,SVHDADDK                                                
         MVC   SVHDADDK(2),=X'0AC2'                                             
         MVC   SVHDADDK+2(3),BAGYMD                                             
         MVC   SVHDADDK+5(8),WORK+12                                            
*        BRAS  RE,ADDPSSV                                                       
                                                                                
         B     VRPC                                                             
                                                                                
VRHDERR  MVC   GERROR,=Y(CMLBADMS)                                              
         J     TRAPERR2                                                         
                                                                                
VRHDDUP  MVC   GERROR,=Y(DUPHIDEF)                                              
         J     TRAPERR2                                                         
                                                                                
VRHDSCR  MVC   GERROR,=Y(DUPCMSCR)                                              
         J     TRAPERR2                                                         
                                                                                
VHD912ER MVC   GERROR,=Y(CMML912)                                               
         J     TRAPERR2                                                         
                                                                                
*----------------------------------------------------------------*              
* VALIDATE PARENT COMMERCIAL                                                    
*----------------------------------------------------------------*              
                                                                                
VRPC     MVI   CHKCMML,C'Y'                                                     
         LA    R2,TRAPRNTH                                                      
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BRAS  RE,VALCMML                                                       
                                                                                
*----------------------------------------------------------------*              
* VALIDATE CENTERCUT COMMERCIAL                                                 
*----------------------------------------------------------------*              
VRCC     DS    0H                                                               
         MVC   NWCNTCTX,SVCNTCTX                                                
         XC    SVCCADDK,SVCCADDK                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VRCC005                                                          
         XC    SVCNTCT,SVCNTCT                                                  
         XC    SVCNTCTX,SVCNTCTX                                                
         B     VRCC010                                                          
                                                                                
VRCC005  LA    R2,TRACNTRH                                                      
         OC    TRACNTR,SPACES                                                   
         CLC   SVCNTCT,8(R2)                                                    
         BE    VRSWAP                                                           
                                                                                
VRCC010  XC    WORK,WORK                                                        
         XC    NWCNTCTX,NWCNTCTX                                                
         LA    R2,TRACNTRH                                                      
         CLI   5(R2),0                                                          
         BE    VRCC015                                                          
         CLI   5(R2),9                                                          
         BL    VHD912ER                                                         
         BRAS  RE,VALCMML                                                       
                                                                                
         TM    KEY+13,X'80'              CANNOT ADD IF ADID DIR IS              
         BO    VRCCERR                   MARKED DELETED                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(12),8(R2)                                                   
         OC    WORK(12),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         BNE   VRCCERR                                                          
                                                                                
         MVC   NWCNTCTX,WORK+12                                                 
                                                                                
         OC    TRAHDEF,SPACES                                                   
         OC    TRAADID,SPACES                                                   
         OC    TRACNTR,SPACES                                                   
         OC    TRACML,SPACES                                                    
                                                                                
         CLC   TRACNTR,TRACML                                                   
         BE    VRCCSCR                                                          
         CLC   TRACNTR,TRAADID                                                  
         BE    VRCCSCR                                                          
         CLC   TRACNTR,TRAHDEF                                                  
         BE    VRCCSCR                                                          
                                                                                
         BRAS  RE,CCDUPS                 HALF PASSED TO REDUPS AS PARAM         
         CLC   KEY(13),KEYSAVE                                                  
         BE    VRCCDUP                                                          
                                                                                
VRCC015  DS    0H                                                               
         XC    SVCCDELK,SVCCDELK                                                
         OC    SVCNTCT,SVCNTCT                                                  
         BZ    VRCC020                                                          
         MVC   SVCCDELK(2),=X'0AC3'                                             
         MVC   SVCCDELK+2(3),BAGYMD                                             
         MVC   SVCCDELK+5(8),SVCNTCTX                                           
*        BRAS  RE,DELPSSV                                                       
                                                                                
VRCC020  DS    0H                                                               
         XC    SVCCADDK,SVCCADDK                                                
         LA    R2,TRACNTRH                                                      
         CLI   5(R2),0                                                          
         BE    VRSWAP                                                           
                                                                                
         XC    SVCCADDK,SVCCADDK                                                
         MVC   SVCCADDK(2),=X'0AC3'                                             
         MVC   SVCCADDK+2(3),BAGYMD                                             
         MVC   SVCCADDK+5(8),WORK+12                                            
*        BRAS  RE,ADDPSSV                                                       
                                                                                
         B     VRSWAP                                                           
                                                                                
VRCCERR  MVC   GERROR,=Y(CMLBADMS)                                              
         J     TRAPERR2                                                         
                                                                                
VRCCDUP  MVC   GERROR,=Y(DUPCNTRC)                                              
         J     TRAPERR2                                                         
                                                                                
VRCCSCR  MVC   GERROR,=Y(DUPCMSCR)                                              
         J     TRAPERR2                                                         
                                                                                
*----------------------------------------------------------------*              
* VALIDATE HD/CC SWAP FIELD                                                     
*----------------------------------------------------------------*              
VRSWAP   DS    0H                                                               
         MVI   SVSWAP,0                                                         
         LA    R2,TRASWAPH                                                      
         CLI   5(R2),0                                                          
         BE    VR84                                                             
         MVI   SVSWAP,C'N'                                                      
         CLI   TRASWAP,C'N'                                                     
         BE    VR84                                                             
         MVI   SVSWAP,C'Y'                                                      
         CLI   TRASWAP,C'Y'                                                     
         BE    VR84                                                             
                                                                                
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*----------------------------------------------------------------*              
* VALIDATE DESTROY DATE AND TIME                                                
*----------------------------------------------------------------*              
                                                                                
VR84     DS    0H                                                               
         XC    DSTRYDAT(5),DSTRYDAT   CLEAR DATE/TIME SAVE AREA                 
                                                                                
         LA    R2,TRADSDTH                                                      
         CLI   5(R2),0                                                          
         BE    VR90                                                             
                                                                                
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATE                                       
         OC    DMCB(4),DMCB                                                     
         JZ    DATERRB                                                          
         GOTO1 DATCON,DMCB,(0,DATE),(3,DSTRYDAT)                                
                                                                                
         CLC   DSTRYDAT,SVCMLSTR                                                
         BNL   VR85                                                             
         MVC   GERROR,=Y(DESTERR)                                               
         GOTO1 VTRAERR                                                          
                                                                                
VR85     LA    R2,TRADSTMH                                                      
         CLI   5(R2),0                                                          
         BE    VR90                                                             
                                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                 
                                                                                
         L     RF,0(R1)                                                         
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),WORK                                              
         CLI   0(R1),X'FF'                                                      
         JE    INVTERR                                                          
         MVC   DSTRYTIM,WORK                                                    
         OC    DSTRYTIM,DSTRYTIM   TEST FOR MIDNIGHT                            
         BNZ   *+10                                                             
         MVC   DSTRYTIM,=H'2400'                                                
                                                                                
VR90     MVI   ELCODE,X'24'        UPDATE EXTENDED DATA ELEM                    
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING CMLXDTEL,R6                                                      
                                                                                
         CLC   TRAHDEF,SPACES                                                   
         BNE   *+10                                                             
         XC    TRAHDEF,TRAHDEF                                                  
         CLC   TRACNTR,SPACES                                                   
         BNE   *+10                                                             
         XC    TRACNTR,TRACNTR                                                  
                                                                                
         CLC   CMLXPRNT,TRAPRNT                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_PRNT                                              
                                                                                
         CLC   CMLXHDEF,TRAHDEF                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_HDEF                                              
                                                                                
         CLC   CMLXCNTR,TRACNTR                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_CNTR                                              
                                                                                
         XC    ELEM,ELEM                                                        
         MVI   CMLXDTEL,X'24'                                                   
         MVI   CMLXDTLN,CMLXDTX-CMLXDTEL                                        
         MVC   CMLXHDEF,TRAHDEF                                                 
         MVC   CMLXCNTR,TRACNTR                                                 
         MVC   CMLXPRNT,TRAPRNT                                                 
         MVC   CMLXDSDT,DSTRYDAT                                                
         MVC   CMLXDSTM,DSTRYTIM                                                
                                                                                
         MVC   CMLXHDPK,NWHIDEFX                                                
         MVC   CMLXCCPK,NWCNTCTX                                                
                                                                                
         MVC   CMLXSWAP,TRASWAP                                                 
                                                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
* SAVE 22 ELEMENT IN SVNETS TO PRESERVE DATES                                   
*                                                                               
VR100    DS    0H                                                               
         XC    SVNETS,SVNETS                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'        COMMERCIAL NETWORK ELEMENT                   
         BRAS  RE,GETEL                                                         
         BNE   VR100F                                                           
*                                                                               
         LA    R3,SVNETS           SAVE 4 ELEMENTS MAX                          
*                                                                               
VR100B   ZIC   R1,1(R6)            ELEM LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)       SAVE ELEMENT                                 
         AR    R3,R1               BUMP TO NEXT                                 
         LA    R3,1(R3)            AVAILABLE SLOT                               
         BRAS  RE,NEXTEL                                                        
         BE    VR100B                                                           
*                                                                               
VR100F   LA    R2,TRANET1H                                                      
         L     R6,AIO                                                           
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R0,MAXNETS                                                       
*                                                                               
VR102    CLI   5(R2),0             ANY INPUT                                    
         BE    VR105                                                            
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         BRAS  RE,VNET               VALIDATE NETWORK                           
*                                                                               
         BRAS  RE,CHKDUP             CHECK FOR DUPLICATE NETWORK                
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLNETEL,R6                                                      
*                                                                               
         MVI   CMLNETEL,X'22'      ELEMENT ID                                   
         MVI   CMLNETLN,CMLNETLE   AND ELEMENT LENGTH                           
         MVC   CMLDATE,BTODAY      PRESET ADD DATE TO TODAY'S DATE              
         MVC   WORK(5),8(R2)                                                    
         LA    R1,WORK+4                                                        
         CLI   0(R1),C'*'          DELETE THIS NETWORK                          
         BE    VR103                                                            
         CLI   0(R1),C'-'                                                       
         BE    VR103F                                                           
         BCTR  R1,0                                                             
         CLI   0(R1),C'*'                                                       
         BNE   VR103F                                                           
*                                                                               
* SAVE DELETED NETWORK                                                          
*                                                                               
VR103    DS    0H                                                               
         MVI   0(R1),C' '          BLANK OUT ASTERIK(*)                         
         MVI   CMLNETEL,X'23'      DELETED NETWORK ELEMENT                      
         MVI   CMLNETLN,CMLDNTLE   ELEMENT LENGTH                               
         MVC   CMLDDATE,BTODAY     SET DELETE DATE TO TODAY'S DATE              
         MVC   CMLNET,WORK                                                      
         OC    CMLNET,SPACES                                                    
         B     VR103K                                                           
*                                                                               
VR103F   CLI   0(R1),C'-'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '          GET RID OF (-)                               
         MVC   CMLNET,WORK                                                      
         OC    CMLNET,SPACES                                                    
*                                                                               
         LA    RF,8(R2)                                                         
         ZIC   R1,5(R2)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         CLC   =C'-',0(RF)         EXCLUDE THIS NETWORK?                        
         BNE   *+16                                                             
         OI    CMLFLG,CMLEXNET                                                  
         OI    FLAGS,NETWORKX      EXCLUDE THIS NETWORK                         
         B     VR103K                                                           
*                                                                               
         CLI   5(R2),5                                                          
         BNE   *+12                                                             
         CLI   0(RF),C' '          FIFTH CHAR SHOULD BE ' '                     
         BH    NONETERR                                                         
         OI    FLAGS,NETWSPEC      NETWORK SPECIFIC CML                         
*                                                                               
* GET NETWORK ADD DATE                                                          
*                                                                               
VR103K   LA    RE,SVNETS                                                        
*                                                                               
VR103L   CLC   CMLNET,2(RE)        IS THIS THE NETWORK                          
         BNE   VR103N                                                           
         OC    7(3,RE),7(RE)       ANY DATE                                     
         BZ    VR104                                                            
         MVC   CMLDATE,7(RE)       MOVE NETWORK ADD DATE                        
         B     VR104                                                            
*                                                                               
VR103N   ZIC   R1,1(RE)            ENTRY LENGTH                                 
         AR    RE,R1               BUMP TO NEXT ENTRY IN ELEM                   
         CLI   0(RE),0             ANY MORE ELEMS                               
         BNE   VR103L               YES                                         
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
VR104    GOTO1 ADDELEM                                                          
*                                                                               
VR105    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,VR102                                                         
         TM    FLAGS,NETWSPEC+NETWORKX                                          
         BO    EXNETERR                                                         
         DROP  R6                                                               
*                                                                               
VR106    XC    OLDACTS(L'OLDACTS*NUMACTS),OLDACTS                               
         XC    NEWACTS(L'NEWACTS*NUMACTS),NEWACTS                               
         XC    ACTSLN,ACTSLN                                                    
*                                                                               
* SAVE OFF OLD ACTUALS                                                          
*                                                                               
         LA    R0,NUMACTS                                                       
         LA    R1,OLDACTS                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         USING CMLACTEL,R6                                                      
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VR108    BRAS  RE,NEXTEL                                                        
         BNE   VR108X                                                           
         MVC   0(8,R1),CMLACTID                                                 
         CLI   1(R6),10            TEST OLD ELEMENT                             
         BNH   *+10                                                             
         MVC   0(12,R1),CMLACTID                                                
         LA    R1,L'OLDACTS(R1)                                                 
         BCT   R0,VR108                                                         
*                                                                               
VR108X   GOTO1 REMELEM             A-DING-DONG                                  
*                                                                               
* VALIDATE ID'S ON SCREEN & BUILD TABLE OF NEW ACTUAL ID'S                      
*                                                                               
         XC    ACTCT,ACTCT         COUNT OF ACTUAL CMMLS                        
         CLC   TRAACT1(7),=C'UNCOVER'                                           
         BNE   *+12                                                             
         OI    FLAGS,UNCOVER                                                    
         B     VRDONE                                                           
*                                                                               
         LA    R2,TRAACT1H                                                      
         LA    R0,NUMACTS                                                       
         LA    R1,NEWACTS                                                       
         SR    RF,RF                                                            
*                                                                               
VR110    CLI   5(R2),0             ANY INPUT?                                   
         BE    VR120                NO                                          
         TM    FLAGS,ISCOVERD      IS THIS CMML COVERED?                        
         BZ    *+14                 NO                                          
         MVC   GERROR,=Y(NOCOVCOV)                                              
         B     TRAPERR2                                                         
*                                                                               
         IC    RF,ACTCT            INC ACTUAL COUNT                             
         LA    RF,1(RF)                                                         
         STC   RF,ACTCT                                                         
*                                                                               
         BRAS  RE,VID                                                           
*                                                                               
         MVC   0(12,R1),8(R2)                                                   
         OC    0(12,R1),SPACES                                                  
         LA    R1,L'NEWACTS(R1)                                                 
*                                                                               
VR120    IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,VR110                                                         
*                                                                               
* MAKE SURE 2+ ACTUALS USED                                                     
*                                                                               
         CLI   ACTCT,0              ANY ACTUALS?                                
         BNE   *+16                 YES                                         
         TM    FLAGS,ISCOVCML      WERE THERE ACTUALS BEFORE?                   
         BNZ   *+16                 YES                                         
         B     VRDONE                                                           
         CLI   ACTCT,2                                                          
         BNL   *+18                                                             
         LA    R2,TRAACT1H                                                      
         MVC   GERROR,=Y(COVREQ2)                                               
         B     TRAPERR2                                                         
*                                                                               
* CHECK THAT ACCUM SLN'S = COV SLN                                              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACTSLN,CMLSLN-CMLDTAEL(R6)                                       
         BE    *+18                                                             
         LA    R2,TRAACT1H                                                      
         MVC   GERROR,=Y(SLNNEQ)                                                
         B     TRAPERR2                                                         
*                                                                               
* ADD X'60' ELS                                                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R0,ACTCT            COUNT OF ACT CMMLS                           
         LA    R4,NEWACTS                                                       
*                                                                               
VR130    MVC   ELEM(2),=X'600E'                                                 
         MVC   ELEM+2(12),0(R4)                                                 
         OC    ELEM+2(12),SPACES                                                
         GOTO1 ADDELEM                                                          
         LA    R4,12(R4)                                                        
         BCT   R0,VR130                                                         
*                                                                               
VRDONE   CLI   BYTE,1              GENERATE PATTERN T/A REQUEST?                
         BNE   VR140                                                            
         BRAS  RE,PATREQ           GENERATE PATTERN T/A REQUEST                 
                                                                                
VR140    SR    R1,R1                                                            
         LA    R0,NUMACTS                                                       
*                                                                               
VR150    LA    RE,OLDACTS(R1)                                                   
         LA    RF,NEWACTS(R1)                                                   
         CLC   0(L'NEWACTS,RE),0(RF)  NEW MATCH OLD                             
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_ACTS   SET CHANGE FLAG - NOT IN OLD LIST          
         LA    R1,L'OLDACTS(R1)                                                 
         BCT   R0,VR150                                                         
                                                                                
VR151    BRAS  RE,SETCHGEL                                                      
*                                                                               
         L     R6,AIO                                                           
         CLI   TRACMLH+5,8         TEST CMML IS ADID                            
         BNH   VR154               NO                                           
                                                                                
         CLI   ACTNUM,ACTADD       CLEAR HELP ON ACTN ADD                       
         BE    VR152                                                            
         CLC   HOLDCM12,TRACML     SAVE CML AS INPUT                            
         BNE   VR154                                                            
         TM    TRACMLH+4,X'80'                                                  
         BO    VR154                                                            
                                                                                
VR152    OI    15(R6),X'01'        TURN ON STATUS IN RECORD                     
         OI    KEY+13,X'01'        AND IN KEY FOR PACKED CML                    
                                                                                
VR154    BRAS  RE,DR               NOW DISPLAY VALIDATED RECORD                 
         B     EXIT                                                             
                                                                                
VRPACK   PACK  DUB,WORK(1)                                                      
*                                                                               
VRB      DS   0H                                                                
         CLC   =C'SJ',AGENCY       AGENCY IS SJR TEST                           
         BE    *+12                                                             
*                                                                               
         TM    TWAAUTH+1,X'04'     AUTHORIZED TO CHANGE BRDCST SCREEN           
         BO    AUTHERR              NO, AUTHORIZATION ERROR                     
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD IS INVALID                        
         BE    DELREC                                                           
*                                                                               
         BAS   RE,CHKDEL           MAKE SURE CML IS NOT DELETED                 
*                                                                               
         BRAS  RE,VRBROAD          VALIDATE BROADCAST SCREEN                    
*                                                                               
         BRAS  RE,DR                                                            
         B     EXIT                                                             
*                                                                               
VRL      DS   0H                                                                
         CLC   =C'SJ',AGENCY       AGENCY IS SJR TEST                           
         BE    *+12                                                             
*                                                                               
         TM    TWAAUTH+1,X'08'     AUTHORIZED TO CHANGE LEGAL SCREEN            
         BO    AUTHERR              NO, AUTHORIZATION ERROR                     
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD IS INVALID                        
         BE    DELREC                                                           
*                                                                               
         BAS   RE,CHKDEL           MAKE SURE CML IS NOT DELETED                 
*                                                                               
         BRAS  RE,VRLEGAL          VALIDATE LEGAL SCREEN                        
*                                                                               
         BRAS  RE,DR               GO DISPLAY RECORD                            
         B     EXIT                                                             
*                                                                               
* SEE THAT THIS CML IS NOT DELETED                                              
*                                                                               
CHKDEL   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    CMLSTAT,X'80'                                                    
         BZ    CHKDELX                                                          
         MVC   GERROR,=Y(CMLISDEL)                                              
         LA    R2,TRACMLH                                                       
         B     TRAPERR2                                                         
CHKDELX  B     EXIT                                                             
NONETERR MVC   GERROR,=Y(NONET)                                                 
         GOTO1 VTRAERR                                                          
*                                                                               
EXNETERR LA    R2,TRANET1H                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EXINMSG),EXINMSG                                       
         B     TRAPERR1                                                         
EXINMSG  DC    C'ENTER EITHER INCLUDED OR EXCLUDED NETWORKS'                    
*                                                                               
AUTHERR  LA    R2,TRAMEDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'AUTHMSG),AUTHMSG                                       
         B     TRAPERR1                                                         
AUTHMSG  DC    C'* ERROR * YOU ARE NOT AUTHORIZED FOR THIS'                     
*                                                                               
RCLERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'* ERROR * RECALL DATE REQUIRED'                   
         B     TRAPERR1                                                         
         DROP  R6                                                               
         EJECT                                                                  
* GET NEXT SEQUENCE NUMBER FROM MASTER RECORD FOR ADDS                          
*                                                                               
PAR      DS    0H                                                               
         BRAS  RE,NPAR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*                                                                               
BPUT     DS    0H                                                               
         BRAS  RE,PUT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* UPDATE SEQUENCE NUMBER IN MASTER RECORD FOR LAST ADD                          
*                                                                               
AAR      MVC   SVKEY,KEY           SAVE DISK ADDR OF ADDED RECORD               
         MVC   SVDSKAD,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         MVC   CMLKCML,HOLDCML                                                  
         GOTO1 HIGH                GET DISK ADDR FOR ADDED REC                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    CMLKID+1,X'80'       CHANGE 21 TO A1                             
         MVC   CMLKCML(3),HOLDSEQ                                               
         XC    CMLKCML+3(5),CMLKCML+3                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
                                                                                
* CREATE ADID POINTER FOR 8-12 CHAR CMML                                        
                                                                                
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         L     R6,AIO1                                                          
         MVC   KEY+5(8),5(R6)      PRESET CML IN KEY                            
         TM    15(R6),X'01'        PACKED CML IN KEY                            
         BO    AAR02               YES, KEY IS SET                              
* ADDING ISCI CMML - SO NEED TO PACK IT IF IT'S VALID                           
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),5(R6)       PRESET CML IN KEY                            
         GOTO1 VTRPACK,DMCB,(C'P',WORK),CMLKCML                                 
         BNE   AAR10                                                            
*                                                                               
AAR02    MVC   KEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    AAR10                                                            
         DC    H'0'                                                             
*                                                                               
* SEE IF ANY ACTUAL CMMLS NEED TO BE UPDATED                                    
*                                                                               
AAR10    DS    0H                                                               
         XC    KEY,KEY                                                          
         OC    SVHDDELK,SVHDDELK                                                
         BZ    AAR15                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
*        BNE   AAR15                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDDELK),SVHDDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
                                                                                
AAR15    XC    KEY,KEY                                                          
         OC    SVCCDELK,SVCCDELK                                                
         BZ    AAR20                                                            
         CLC   SVCCDELK(2),=X'0AC3'                                             
*        BNE   AAR20                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCDELK),SVCCDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVCCDELK,SVCCDELK                                                
                                                                                
AAR20    XC    KEY,KEY                                                          
         OC    SVHDADDK,SVHDADDK                                                
         BZ    AAR25                                                            
         CLC   SVHDADDK(2),=X'0AC2'                                             
*        BNE   AAR25                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDADDK),SVHDADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
                                                                                
AAR25    XC    KEY,KEY                                                          
         OC    SVCCADDK,SVCCADDK                                                
         BZ    AAR40                                                            
         CLC   SVCCADDK(2),=X'0AC3'                                             
*        BNE   AAR40                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCADDK),SVCCADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVCCADDK,SVCCADDK                                                
                                                                                
AAR40    L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         LA    R0,1                                                             
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
AAR50    BRAS  RE,NEXTEL                                                        
         BNE   AARXIT                                                           
         LA    R1,2(R6)                                                         
         LA    R0,1                                                             
*                                                                               
         USING CMLACTEL,R6                                                      
         CLI   CMLACTLN,CMLACTL1              OLD ELEM                          
         BE    AAR52                                                            
         CLI   8(R1),C' '                     TEST ISCI                         
         BNH   AAR52                          YES                               
         GOTO1 VTRPACK,DMCB,(C'P',2(R6)),DUB   PACK 12 CHAR ADID                
         LA    R1,DUB                         AND POINT TO IT                   
*                                                                               
AAR52    BRAS  RE,UPDACT                                                        
         B     AAR50                                                            
*                                                                               
AARXIT   MVC   KEY(L'SVKEY),SVKEY  RESTORE                                      
         B     EXIT                                                             
         DROP  R4,R6                                                            
*                                                                               
* DELETE RECORD INVALID FOR COMMERCIALS                                         
*                                                                               
DELREC   MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     TRAPERR                                                          
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    DK10                                                             
         MVC   TRAMED,WORK                                                      
         OI    TRAMEDH+6,X'80'                                                  
*                                                                               
DK10     L     R4,AIO                                                           
         USING CMLKEY,R4                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF6,CMLKCLT),QCLT                              
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    DK12                                                             
         MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
DK12     XC    TRACML,TRACML                                                    
         MVC   TRACML(L'CMLKCML),CMLKCML                                        
         OI    TRACMLH+6,X'80'                                                  
*                                                                               
         TM    15(R4),X'01'        IS CML PACKED IN KEY                         
         BZ    DK12C                                                            
                                                                                
         GOTO1 VTRPACK,DMCB,(C'U',CMLKCML),TRACML                               
*                                                                               
DK12C    CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    DK20                 YES                                         
*                                                                               
         BRAS  RE,FCLT             GET CLIENT CLIST                             
         BNE   EXIT                                                             
*                                                                               
DK20     MVC   HOLDCML,CMLKCML                                                  
         DROP  R4                                                               
*                                                                               
* PRINT OUT ANY FILTERS                                                         
*                                                                               
         OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    EXIT                NO                                           
         LA    R3,FLD              OUTPUT AREA                                  
         LR    R4,R3               COMPARAND                                    
         XC    FLD,FLD                                                          
         OC    RLDTFTR,RLDTFTR     RELEASE DATE FILTER                          
         BZ    DK24                                                             
         MVC   0(3,R3),=C'REL'                                                  
         CLI   RLDTSFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   3(1,R3),RLDTSFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   3(R3),C'='                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RLDTFTR),(5,4(R3))                                
         LA    R3,12(,R3)                                                       
DK24     OC    RCDTFTR,RCDTFTR     RECALL DATE FILTER                           
         BZ    DK28                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'RCL'                                                  
         CLI   RCDTSFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   3(1,R3),RCDTSFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   3(R3),C'='                                                       
*                                                                               
         CLC   RCDTFTR,=XL3'FFFFFF'                                             
         BNE   DK26                                                             
         MVC   4(3,R3),=C'UFN'                                                  
         LA    R3,7(,R3)                                                        
         B     DK28                                                             
DK26     GOTO1 DATCON,DMCB,(3,RLDTFTR),(5,4(R3))                                
         LA    R3,12(,R3)                                                       
DK28     TM    FTRFLAG,DELFTR      SHOW DELETED CML'S ONLY                      
         BZ    DK30                 NO                                          
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'DEL'                                                  
         LA    R3,4(,R3)                                                        
         EJECT                                                                  
DK30     OC    PRODFTR,PRODFTR     PRODUCT FILTER                               
         BZ    DK32                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'P'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),PRODFTR                                                  
         CLI   4(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,5(,R3)                                                        
DK32     OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    DK34                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'T'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),TYPEFTR                                                  
         CLI   4(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,5(,R3)                                                        
DK34     OC    SLNFTR,SLNFTR       CML LENGTH FILTER                            
         BZ    DK40                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'L'                                                       
         MVI   1(R3),C'='                                                       
         LA    R3,2(,R3)                                                        
         CLI   SLNFTR,255                                                       
         BNE   *+14                                                             
         MVC   0(3,R3),=C'ALL'                                                  
         B     DK36                                                             
         EDIT  (B1,SLNFTR),(3,(R3)),ALIGN=LEFT                                  
         CLI   2(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
DK36     LA    R3,3(,R3)                                                        
*                                                                               
DK40     DS    0H                                                               
         MVC   TRAFLTR,FLD                                                      
         OI    TRAFLTRH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LR       DS    0H                                                               
         LA    R3,SORTTAB                                                       
         LA    RE,1000(R3)                                                      
         ST    R3,ASORTTAB                                                      
         ST    RE,ASORTEND                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A50'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VQSORT,DMCB                                                      
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   LR01                                                             
         L     R3,VADUMMY                                                       
         LR    RE,R3                                                            
         A     RE,=F'100000'                                                    
         ST    R3,ASORTTAB                                                      
         ST    RE,ASORTEND                                                      
*                                                                               
LR01     LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
* NEED TO RESET ADIDFLAG IF NOT FIRST TIME - NOT IN SAVED STORAGE               
         CLI   KEY,0               TEST FIRST TIME                              
         BE    LR02                YES                                          
*                                                                               
         MVC   KEY,SVLSTKEY        RE-ESTABLISH READ SEQUENCE                   
         MVI   ADIDFLAG,C'A'                                                    
         CLI   KEY+1,X'C1'                                                      
         BE    *+8                                                              
         MVI   ADIDFLAG,C'I'                                                    
         B     LR10                                                             
*                                                                               
LR02     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0AC1'                                                  
         MVI   COMPKEY+1,X'C1'                                                  
         CLI   ADIDFLAG,C'A'       TEST TO READ ADIDS                           
         BE    *+14                                                             
         MVC   CMLKID,=X'0A21'     ELSE JUST DO ISCI'S NOW                      
         MVI   COMPKEY+1,X'21'                                                  
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,HOLDCML     (NOTE MAY BE PACKED CMML)                    
*                                                                               
         MVI   RCSUBPRG,0                                                       
         XC    RECCT,RECCT         ZERO RECORD CT                               
         LM    R0,R1,=A(HEADING,HDHK)                                           
         A     R0,SPTR22RR                                                      
         ST    R0,SPECS                                                         
         A     R1,SPTR22RR                                                      
         ST    R1,HEADHOOK                                                      
*                                                                               
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(3),KEYSAVE      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         BRAS  RE,LRNONE                                                        
         B     EXIT                                                             
*                                                                               
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
*                                                                               
* DON'T TASKNEXT OUT ON MAXIOS                                                  
*                                                                               
         MVI   ERROR,0             JUST IN CASE...                              
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    LR22                 NO                                          
         LA    R2,CONWHENH         FOR CURSOR POSN                              
         OC    SPOOLKEY,SPOOLKEY   TEST REPORT GENERATED                        
         BZ    TRAPERR                                                          
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TRAPERR                                                          
*                                                                               
LR22     CLC   KEY(5),KEYSAVE      AT END OF THIS A-M/CLT                       
         BE    LR24                 NO                                          
         TM    FLAGS2,ALLCLTR      ALL CLIENTS REQUEST?                         
         BZ    LR28                 NO                                          
         CLC   KEY(3),KEYSAVE      AT END OF THIS A-M                           
         BNE   LR28                YES                                          
*                                                                               
LR24     OC    KEY+5(8),KEY+5      SKIP CMML SEQNUM REC                         
         BZ    LR20                                                             
         CLC   KEY+5(8),=8C'9'     SKIP PROD HOUSE RECS                         
         BE    LR20                                                             
*                                                                               
         CLI   ADIDFLAG,C'I'       TEST READING ISCI                            
         BNE   LR25                NO                                           
         TM    KEY+13,X'01'        TEST PACKED CMML                             
         BO    LR20                YES - IGNORE                                 
         MVC   WORK(8),KEY+5       MOVE KEY FOR COMPARE BELOW                   
         B     LR26                                                             
*                                                                               
LR25     LA    R0,CMLKCML-CMLKEY+KEY    UNPACK ADID                             
         GOTO1 VTRPACK,DMCB,(C'U',(R0)),WORK                                    
         CLI   WORK+8,C' '              TEST ONLY 8 CHARS                       
         BNH   LR20                     YES - SKIP FOR NOW                      
                                                                                
LR26     SR    R1,R1                                                            
         ICM   R1,1,TRACMLH+5        GET INPUT LEN                              
         BZ    LR30                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRACML(0),WORK      CML MATCH ?                                  
         BE    LR30                YES - PROCESS                                
*                                                                               
LR28     CLI   ADIDFLAG,C'I'       DID WE READ ISCI YET                         
         BNE   LR28A               NO                                           
         BRAS  RE,LRANY                                                         
         B     EXIT                                                             
*                                                                               
LR28A    MVI   ADIDFLAG,C'I'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVI   COMPKEY+1,X'21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         TM    FLAGS2,ALLCLTR      ALL CLIENTS REQUEST?                         
         BZ    *+10                                                             
         XC    KEY+3(2),KEY+3                                                   
         SR    RE,RE                                                            
         ICM   RE,1,TRACMLH+5                                                   
         BZ    LR10                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+5(0),TRACML     USE EBCDIC COMMERCIAL                        
         B     LR10                                                             
*                                                                               
LR30     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING CMLKEY,R4                                                        
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
*                                                                               
         USING CMLDTAEL,R6                                                      
         BRAS  RE,FTR              GO FILTER RECS                               
         BNE   LR20                GOT FILTERED OUT                             
*                                                                               
         LH    R1,RECCT                                                         
         LA    R1,1(R1)                                                         
         STH   R1,RECCT                                                         
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
LRCLC    CLC   COMPKEY(1),KEY                                                   
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
*                                                                               
LRR      BRAS  RE,LRR00                                                         
         B     LR20                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
*                                                                               
         USING CMLKEY,R4                                                        
         USING CMLDTAEL,R6                                                      
LRL      DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         TM    SECFLAG,BLSSW       IF BRAND LEVEL SECURITY CLIENT               
         BO    LRL01               THEN GO TO FCLT EVERYTIME                    
*                                                                               
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    LRL02                                                            
         MVC   BCLT,CMLKCLT                                                     
LRL01    BRAS  RE,FCLT             GET CLIENT CLIST                             
         BNE   LR20                                                             
*                                                                               
LRL02    GOTO1 CLUNPK,DMCB,(SVCPROF6,CMLKCLT),LCLT                              
         TM    CMLSTAT,X'80'       SOFT DELETE                                  
         BZ    *+8                 NO                                           
         MVI   LCLT+3,C'*'                                                      
*                                                                               
        MVC   LCML(L'CMLKCML),CMLKCML  MOVE CMML CODE                           
        MVC   LCML+8(4),SPACES                                                  
        CLI   KEY+1,X'C1'        TEST READING ADID'S                            
        BNE   LRL02C                                                            
        LA    R0,CMLKCML-CMLKEY+KEY     PACKED ADID IS IN KEY                   
        GOTO1 VTRPACK,DMCB,(C'U',(R0)),LCML                                     
                                                                                
        CLC   CMLKCML-CMLKEY+KEY(8),CMLKCML   KEY/REC CMML MATCH                
        BE    LRL02C                                                            
                                                                                
        LA    R1,LCML+L'LCML-1    SET FLAG TO SHOW IT IS ISCI                   
        CLI   0(R1),C' '                                                        
        BH    *+8                                                               
        BCT   R1,*-8                                                            
        MVI   1(R1),C'!'                                                        
*                                                                               
LRL02C   CLI   CMLSLN,X'FF'        ALL SECONDS LENGTH                           
         BNE   *+14                                                             
         MVC   LLEN,=C'ALL'                                                     
         B     LRL06                                                            
*                                                                               
         OC    CMLOVRD1(2),CMLOVRD1  ANY PRINT OVERRIDE?                        
         BZ    LRL04                  NO                                        
         EDIT  (1,CMLOVRD1),(3,LLEN),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R1,LLEN                                                          
         AR    R1,R0               NEXT BLANK SPACE                             
         MVI   0(R1),C'/'                                                       
         EDIT  (1,CMLOVRD2),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     LRL06                                                            
*                                                                               
LRL04    EDIT  (1,CMLSLN),(3,LLEN),ZERO=BLANK                                   
*                                                                               
LRL06    MVC   LTITLE,CMLTITLE                                                  
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,LRELSE)                               
         CLC   CMLRCL,=XL3'FFFFFF'                                              
         BNE   LRL10                                                            
         MVC   LRECALL(3),=CL3'UFN'                                             
         B     LRL12                                                            
*                                                                               
LRL10    GOTO1 (RF),(R1),(3,CMLRCL),(5,LRECALL)                                 
*                                                                               
LRL12    MVC   LTYPE,CMLTYPE                                                    
         MVC   LCLASS,CMLCLASS                                                  
         TM    CMLSTAT,X'40'       COMML TEXT REC FOR THIS COMML                
         BZ    LRL13                                                            
         MVC   LCOMTXT,=C'COM TEXT'                                             
*                                                                               
LRL13    TM    FTRFLAG,SEQFTR      DO THEY WANT COMML SEQ NO                    
         BZ    LRL16                                                            
*                                                                               
         EDIT  CMLSEQ,(6,LCOV+1)                                                
*                                                                               
LRL16    DS   0H                                                                
         OC    CMLCOVCT,CMLCOVCT   IS THIS AN ACTUAL FOR A COVER?               
         BZ    *+12                 NO                                          
         MVI   LCOV,C'A'                                                        
         B     LRL20                                                            
         LR    R6,R4                                                            
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   LCOV,C'C'                                                        
*                                                                               
LRL20    TM    FTRFLAG,HDEFFTR     SHOW HDEF CMML INSTEAD OF TITLE              
         BZ    LRL22                                                            
         MVC   LTITLE,SPACES       THEN DON'T SHOW TITLE                        
         MVI   ELCODE,X'24'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LRL22                                                            
         USING CMLXDTEL,R6                                                      
         MVC   LTITLE(12),CMLXHDEF                                              
*                                                                               
LRL22    MVC   SVLSTKEY,KEY        SAVE LAST KEY FOR RESUME IN SVSTOR           
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         DROP  R4,R6                                                            
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*        ERROR ROUTINES                                                         
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
*INV9999  DS    0H                                                              
*        MVI   ERROR,0                                                          
*        XC    CONHEAD,CONHEAD                                                  
*        MVC   CONHEAD(L'INV99MSG),INV99MSG                                     
*        B     TRAPERR1                                                         
***INV99MSG DC    C'* ERROR * COMML CODE 99999999 NOT VALID'                    
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
NUMERR   MVI   ERROR,NOTNUM                                                     
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TALTRNER MVC   GERROR,=Y(TALTRNMS)                                              
         B     TRAPERR2                                                         
                                                                                
TRAPERR1 GOTO1 ERREX2                                                           
*                                                                               
TRAPERR2 GOTO1 VTRAERR             GETTXT CALL                                  
         EJECT                                                                  
         LTORG                                                                  
                                                                                
DELETED  DS    0CL7                                                             
DELETE   DC    CL6'DELETE'                                                      
         DC    C'D*'                                                            
                                                                                
CTYPTABN DC    CL3'ABB'            AUDIO BILLBOARD                              
         DC    CL3'BET'                                                         
         DC    CL3'D2 '            DIGITAL FORMAT                               
CTYPACD  DC    CL3'ACD'            ART CARD                                     
         DC    CL3'AT '            AUDIO TAPES                                  
         DC    CL3'CLR'            CLR/VTR                                      
         DC    CL3'CPY'            COPY                                         
CTYPCSL  DC    CL3'CSL'            COLOR SLIDE                                  
CTYPCSS  DC    CL3'CSS'            COLOR SUPERSLIDE                             
         DC    CL3'EPS'            ENCAPSULATED POSTSCRIPT                      
         DC    CL3'FBB'            FILM BILLBOARD                               
CTYPFBS  DC    CL3'FBS'            FULL SCREEN B/W SLIDE                        
CTYPFCS  DC    CL3'FCS'            FULL SCREEN COLOR SLIDE                      
         DC    CL3'HDT'            HDVT HIGH DEFINITION TV                      
         DC    CL3'HV1'            HIGH BAND 1                                  
         DC    CL3'GIF'            GRAPHIC FORMAT                               
         DC    CL3'JPG'            .JPEG GRAPHIC FORMAT                         
         DC    CL3'SC '            SLIDE COPY                                   
CTYPSSL  DC    CL3'SSL'            SUPERSLIDE                                   
         DC    CL3'VTR'            VIDEO TAPE                                   
         DC    CL3'ACM'            AUDIO COMMERCIAL                             
         DC    CL3'VCM'            VIDEO COMMERCIAL                             
         DC    CL3'BAC'            BILLBOARD ANNOUNCER COPY                     
         DC    CL3'BLG'            BILLBOARD LOGO                               
         DC    CL3'BPS'            BILLBOARD PRODUCT SHOT                       
         DC    CL3'BPN'            BILLBOARD PRODOUCT NAME                      
CTYPTBCN EQU   (*-CTYPTABN)/3                                                   
         DC    X'00'               END OF TABLE MARKER                          
         EJECT                                                                  
HEADING  SPROG 0,1                                                              
         SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'C O M M E R C I A L  L I S T'                            
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,35,C'----------------------------'                            
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SPROG 0                                                                
         SSPEC H8,1,C'COMMERCIAL'                                               
         SSPEC H9,1,C'----------'                                               
         SSPEC H8,15,C'LEN'                                                     
         SSPEC H9,15,C'---'                                                     
         SSPEC H8,24,C'TITLE'                                                   
         SSPEC H9,24,C'---------------'                                         
         SSPEC H8,41,C'P'                                                       
         SSPEC H9,41,C'S'                                                       
         SSPEC H8,43,C' START '                                                 
         SSPEC H9,43,C'--DATE--'                                                
         SSPEC H8,53,C' END  '                                                  
         SSPEC H9,52,C'--DATE--'                                                
         SSPEC H8,61,C'TYPE'                                                    
         SSPEC H9,61,C'----'                                                    
         SSPEC H8,66,C'PRODUCT LIST'                                            
         SSPEC H9,66,C'------------------------'                                
         SSPEC H8,91,C'OTHER DETAILS'                                           
         SSPEC H9,91,C'--------------------------'                              
         SPROG 1                                                                
         SSPEC H6,37,C'C O V E R   X R E F'                                     
         SSPEC H7,37,C'--------------------'                                    
         SSPEC H8,03,C'ACTUAL'                                                  
         SSPEC H9,03,C'------'                                                  
         SSPEC H8,15,C'COVERS'                                                  
         SSPEC H9,15,C'------'                                                  
         DC    X'00'               END MARKER FOR SSPECS                        
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
LRR00    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CMLKEY,R4                                                        
*                                                                               
         LA    R5,P                                                             
         USING PRTLINE,R5                                                       
*                                                                               
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         TM    SECFLAG,BLSSW       IF BRAND LEVEL SECURITY CLIENT               
         BO    LRR05               THEN GO TO FCLT EVERYTIME                    
*                                                                               
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    LRR10                YES                                         
*                                                                               
LRR05    BRAS  RE,FCLT             GET CLIENT CLIST                             
         BE    *+12                                                             
         BRAS  RE,LRANY            ALL DONE                                     
         B     LRRX                                                             
*                                                                               
         TM    FTRFLAG,FILFTR      THIS A DUMP COMMLS RUN                       
         BZ    LRR10                                                            
         MVI   FORCEHED,C'N'        YES, NO FORCE PAGE BREAK                    
*                                                                               
LRR10    DS   0H                                                                
         MVC   PCML(8),CMLKCML                                                  
         CLI   KEY+1,X'C1'        TEST READING ADID'S                           
         BNE   LRR11                                                            
         LA    R0,CMLKCML-CMLKEY+KEY     PACKED ADID IS IN KEY                  
         GOTO1 VTRPACK,DMCB,(C'U',(R0)),PCML                                    
                                                                                
         CLC   CMLKCML-CMLKEY+KEY(8),CMLKCML   KEY/REC CMML MATCH               
         BE    LRR11                                                            
                                                                                
         LA    R1,PCML+L'PCML-1    SET FLAG TO SHOW IT IS ISCI                  
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'!'                                                       
         DROP  R4                                                               
*                                                                               
LRR11    CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   LRR15                   NO                                       
         MVI   ELCODE,C'1'         CK ACTIVITY ELEMENT                          
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LRR14                                                            
*                                                                               
         USING ACTVD,R6                                                         
         CLC   ACTVADDT,BTODAY     WAS ADD TODAY                                
         BNE   LRR12                NO, CK CHANGE                               
         MVC   PCML+8+132(3),=C'ADD'                                            
         B     LRR13                                                            
*                                                                               
LRR12    CLC   ACTVCHDT,BTODAY     WAS CHANGE TODAY                             
         BNE   LRR13                                                            
         MVC   PCML+8+132(3),=C'CHG'                                            
*                                                                               
LRR13    MVI   PCML+9,C'*'                                                      
*                                                                               
LRR14    MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
LRR15    DS   0H                                                                
         TM    FTRFLAG,SEQFTR      DO THEY WANT COMML SEQ NO                    
         BZ    LRR16                                                            
         EDIT  CMLSEQ,(7,PCML+1+132)                                            
*                                                                               
LRR16    DS   0H                                                                
         CLI   CMLSLN,X'FF'        ALL SECONDS LENGTH                           
         BNE   *+14                                                             
         MVC   PSLN,=C'ALL'                                                     
         B     LRR18                                                            
*                                                                               
         OC    CMLOVRD1(2),CMLOVRD1  ANY PRINT OVERRIDE?                        
         BZ    LRR17                  NO                                        
         EDIT  (1,CMLOVRD1),(3,PSLN),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R1,PSLN                                                          
         AR    R1,R0               NEXT BLANK SPACE                             
         MVI   0(R1),C'/'                                                       
         EDIT  (1,CMLOVRD2),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     LRR18                                                            
*                                                                               
LRR17    EDIT  (1,CMLSLN),(3,PSLN),ZERO=BLANK                                   
*                                                                               
LRR18    MVC   PTITLE,CMLTITLE                                                  
*                                                                               
LRR18A   LR    R2,R6                                                            
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   LRR19                                                            
*                                                                               
         LA    R1,PTITLE                                                        
         OC    19(8,R6),19(R6)     ANY CHARACTERS OVER 16                       
         BZ    LRR18B               NO                                          
         CLC   19(8,R6),SPACES                                                  
         BE    LRR18B                                                           
         XC    PTITLE,PTITLE       SET TO NULLS, NOT SPACES                     
         MVC   PTITLE+132(24),3(R6)                                             
         LA    R1,PTITLE+132                                                    
*                                                                               
LRR18B   LA    R1,132(R1)                                                       
         BRAS  RE,NEXTEL                                                        
         BNE   LRR19                                                            
         MVC   0(24,R1),3(R6)                                                   
         B     LRR18B                                                           
*                                                                               
LRR19    LR    R6,R2                                                            
                                                                                
* PRINT ANY AD-ID NUMBER                                                        
                                                                                
         LA    R0,3                PRESET 3 PRINT LINES                         
         LA    R1,PTITLE+132       PT TO PRINT AREA                             
         XC    SVCNTCT,SVCNTCT     INIT SAVE CNTRCUT                            
         XC    SVHIDEF,SVHIDEF      AND HIDEF                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR19C                                                           
*                                                                               
         CLI   KEY+1,X'C1'         IF DOING ADID POINTERS                       
         BE    LRR19C              DON'T PRINT THE ADID AGAIN NOW               
*                                                                               
LRR19A   CLC   0(L'PTITLE,R1),SPACES                                            
         BE    LRR19B                                                           
         LA    R1,132(R1)                                                       
         BCT   R0,LRR19A                                                        
         AHI   R1,-132             BACK UP OVER LAST TITLE LINE                 
*                                                                               
LRR19B   MVC   0(5,R1),=C'ADID='                                                
         MVC   5(12,R1),2(R6)                                                   
         LA    R1,132(,R1)                                                      
*                                                                               
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL?                         
         BNE   LRR19C                                                           
         B     LRR19C   <<<<<<<<<<< NOP HEX ADID >>>>>>>>>>>>                   
*                                                                               
         GOTO1 HEXOUT,DMCB,2+12(R6),P+40+132,8,0,0                              
*                                                                               
LRR19C   LA    R1,PTITLE+132                                                    
         LA    R0,3                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'24'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR19M                                                           
*                                                                               
LRR19D   DS    0H                                                               
         USING CMLXDTEL,R6                                                      
         OC    CMLXHDEF,CMLXHDEF   ANY HIDEF?                                   
         BZ    LRR19F                                                           
*                                                                               
LRR19D1  CLC   0(L'PTITLE,R1),SPACES                                            
         BE    LRR19E                                                           
         LA    R1,132(R1)                                                       
         BCT   R0,LRR19D1                                                       
*                                                                               
LRR19D4  MVC   SVHIDEF,CMLXHDEF    SAVE HDEF TO PRINT LATER                     
         B     LRR19F                                                           
*                                                                               
LRR19E   MVC   0(5,R1),=C'HDEF='                                                
         MVC   5(12,R1),CMLXHDEF                                                
         LA    R1,132(R1)                                                       
         BCTR  R0,0                                                             
*                                                                               
LRR19F   OC    CMLXCNTR,CMLXCNTR   ANY CENTERCUT?                               
         BZ    LRR19M                                                           
*                                                                               
         LA    R1,PTITLE+132                                                    
         LA    R0,3                                                             
LRR19F1  CLC   0(L'PTITLE,R1),SPACES                                            
         BE    LRR19G                                                           
         LA    R1,132(R1)                                                       
         BCT   R0,LRR19F1                                                       
*                                                                               
LRR19F4  MVC   SVCNTCT,CMLXCNTR    SAVE CNTRCUT TO PRINT LATER                  
         B     LRR19M                                                           
*                                                                               
LRR19G   MVC   0(5,R1),=C'CNTR='                                                
         MVC   5(12,R1),CMLXCNTR                                                
         LA    R1,132(,R1)                                                      
         BCTR  R0,0                                                             
         DROP  R6                                                               
*                                                                               
LRR19M   LR    R6,R2                                                            
         USING CMLDTAEL,R6                                                      
*                                                                               
         CLI   CMLSOLO,0           ANY ENTRY                                    
         BE    LRR20               NO                                           
         MVC   PSOLO,CMLSOLO                                                    
*                                                                               
LRR20    GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,PRELSE)                               
         CLC   CMLRCL,=XL3'FFFFFF'                                              
         BNE   LRR22                                                            
         MVC   PRECALL+3(3),=CL3'UFN'                                           
         B     LRR24                                                            
*                                                                               
LRR22    GOTO1 (RF),(R1),(3,CMLRCL),(5,PRECALL)                                 
*                                                                               
LRR24    MVC   PTYPE,CMLTYPE                                                    
*                                                                               
         MVC   SVKEY,KEY                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    LRR25C                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    LRR25C                                                           
         DC    H'0'                                                             
LRR25C   DS    0H                                                               
         CLI   2(R6),X'FF'         IS THIS PRD=ALL                              
         BNE   LRR26                NO                                          
         MVC   PLIST(7),=CL7'PRD=ALL'                                           
         MVI   PRDCTR,0                                                         
         B     LRR28                                                            
*                                                                               
LRR26    DS    0H                                                               
         LLC   RF,1(R6)            GET PROD LIST ELEM LEN                       
         BCTR  RF,0                                                             
         BCTR  RF,0                NOW # PRODS IN LIST                          
         LA    R4,2(R6)            POINT TO START PROD LIST                     
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   PRDTYP,0(R6)                                                     
         STC   RF,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
*                                                                               
         BAS   RE,PPRDS                                                         
*                                                                               
         LA    R5,P1               RESTORE R5 TO P1                             
LRR28    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       SOFT DELETE                                  
         BZ    LRR30               NO                                           
         MVC   PTITLE+132(9),DELMSG                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
LRR30    OC    CMLPROD,CMLPROD     PRODUCTION HOUSE?                            
         BZ    LRR40               NO                                           
         MVC   PMISC(18),=CL18'PRODUCTION HOUSE ='                              
         MVC   PMISC+19(L'CMLPROD),CMLPROD                                      
         LA    R5,132(R5)         POINT TO NEXT PRINT LINE                      
*                                                                               
LRR40    OC    CMLCLTNO,CMLCLTNO                                                
         BZ    LRR50                                                            
         MVC   PMISC(7),=CL7'CLIENT='                                           
         MVC   PMISC+7(L'CMLCLTNO),CMLCLTNO                                     
         LA    R5,132(R5)         POINT TO NEXT PRINT LINE                      
*                                                                               
LRR50    DS    0H                                                               
         TM    CMLSTAT,X'40'       COMMERCIAL TEXT REC                          
         BZ    LRR60                NO                                          
         MVC   PMISC(26),=CL31'TEXT RECORD FOR THIS COMML'                      
         LA    R5,132(R5)         POINT TO NEXT PRINT LINE                      
*                                                                               
LRR60    LA    R1,P4               LAST LINE                                    
         CR    R1,R5               PAST LAST LINE                               
         BNL   LRR70               NO                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         BAS   RE,PHDCNTR          PRINT HIDEF/CENTERCUT IF ANY                 
*                                                                               
LRR70    OC    CMLCLASS,CMLCLASS   ANY COMM CLASS                               
         BZ    LRR80                                                            
         LA    R1,P4               LAST LINE                                    
         CR    R1,R5               PAST LAST LINE                               
         BNL   LRR74               NO                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         BAS   RE,PHDCNTR          PRINT HIDEF/CENTERCUT IF ANY                 
*                                                                               
LRR74    MVC   PMISC(6),=CL6'CLASS='                                            
         MVC   PMISC+6(4),CMLCLASS                                              
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
LRR80    CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR90               YES                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         BAS   RE,PHDCNTR          PRINT HIDEF/CENTERCUT IF ANY                 
*                                                                               
LRR84    MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
         B     LRR80                                                            
LRR90    BAS   RE,PHDCNTR          PRINT HIDEF/CENTERCUT IF ANY                 
*                                                                               
LRR90C   L     R6,AIO                                                           
         MVI   ELCODE,X'22'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR95                                                            
*                                                                               
         USING CMLNETEL,R6                                                      
*                                                                               
         MVC   PMISC(5),=C'NET= '                                               
         LA    R1,PMISC+5                                                       
LRR91    MVC   0(L'CMLNET,R1),CMLNET                                            
         LA    R1,3(R1)                                                         
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   CMLNETLN,6          OLD RECORD?                                  
         BZ    LRR92                YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    LRR92                NO                                          
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
LRR92    MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    LRR91                                                            
*                                                                               
         BCTR  R1,0                                                             
         MVI   0(R1),C' '          CLEAR LAST COMMA                             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT NETWORK INFO FIRST                     
         LA    R5,P1                                                            
         BAS   RE,PHDCNTR          PRINT HIDEF/CENTERCUT IF ANY                 
*                                                                               
LRR95    DS    0H                                                               
         OC    SVHIDEF,SVHIDEF     ANY HIDEF TO PRINT                           
         BNZ   LRR96                                                            
         OC    SVCNTCT,SVCNTCT     ANY CNTRCUT TO PRINT                         
         BZ    LRR97                                                            
*                                                                               
LRR96    LA    R5,P1                                                            
         LA    R0,4                                                             
         LA    R2,PTITLE           PT TO PRINT AREA                             
         CLC   132(L'PTITLE,R2),SPACES ANYTHING HERE?                           
         BNH   LRR96C                                                           
         LA    R2,132(R2)                                                       
         BCTR  R0,0                                                             
*                                                                               
LRR96C   CLC   0(L'PTITLE,R2),SPACES ANYTHING HERE?                             
         BNH   LRR96F                                                           
         LA    R2,132(R2)                                                       
         BCT   R0,LRR96C                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT WHAT WE'VE GOT FIRST                   
*                                                                               
LRR96F   BAS   RE,PHDCNTR          PRINT HIDEF/CENTERCUT IF ANY                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* SAVE OFF COV INFO FOR XREF AT END                                             
LRR97    MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         LR    R4,R6                                                            
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR120                                                           
         GOTO1 SPOOL,DMCB,(R8)     PRINT EVERYTHING ELSE FIRST                  
         LA    R1,PTITLE                                                        
         MVC   0(07,R1),=C'COVERS='                                             
         LA    R1,8(R1)                                                         
         B     *+12                                                             
LRR100   BRAS  RE,NEXTEL                                                        
         BNE   LRR120                                                           
         MVC   0(8,R3),2(R6)                                                    
         MVC   8(8,R3),CMLKCML-CMLKEY(R4)                                       
         MVC   0(8,R1),2(R6)                                                    
         LA    R1,10(R1)                                                        
         LA    R3,16(R3)                                                        
         C     R3,ASORTEND         PAST END OF SORT TABLE?                      
         BL    LRR100                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                FULL SORT TABLE                              
         MVC   GERROR,=Y(TOOBIG)                                                
         LA    R2,CONWHENH         FOR CURPOS                                   
         OC    SPOOLKEY,SPOOLKEY   TEST REPT GENERATED                          
         JZ    TRAPERR2                                                         
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0             CHECK FOR ERROR                              
         JE    TRAPERR2                                                         
         DC    H'0'                                                             
*                                                                               
LRR120   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
LRRX     XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------                  
* PRINT PROD LIST OF PROD/NAMES                                                 
*-------------------------------------------------------------                  
*                                                                               
         USING PRTLINE,R5                                                       
PPRDS    NTR1                                                                   
*                                                                               
         ZIC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         L     R4,PRDPTR           SAVE PROD LIST PTR                           
*                                                                               
         CLI   PRDTYP,X'29'        THIS A NEW 3 CHAR LIST                       
         BE    PPRDS40                                                          
*                                                                               
PPRDS10  LA    R1,P4                                                            
         CR    R1,R5                                                            
         BL    PPRDS20                                                          
         LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
PPRDS14  DS   0H                                                                
         CLC   0(1,R4),3(R1)                                                    
         BE    PPRDS16                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   PPRDS15                                                          
         BCT   R0,PPRDS14                                                       
*                                                                               
PPRDS15  DS   0H                                                                
         MVC   PLIST(3),=C'***'                                                 
         MVC   PLIST+4(7),=C'UNKNOWN'                                           
         B     PPRDS18                                                          
*                                                                               
PPRDS16  MVC   PLIST(3),0(R1)       STORE IN WORK AREA                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD      A-M/CLT                                     
         MVC   KEY+4(3),0(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   PLIST+4(20),PNAME                                                
         DROP  R1                                                               
PPRDS18  LA    R5,132(,R5)                                                      
         LA    R4,1(,R4)           POINT TO NEXT IN PROD LIST                   
         BCT   R3,PPRDS10                                                       
PPRDS20  STC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
         B     LRRX                                                             
*                                                                               
* MORE PROD USER - USE 29 ELEM                                                  
*                                                                               
PPRDS40  DS   0H                                                                
         LA    R1,P4                                                            
         CR    R1,R5                                                            
         BL    PPRDS50                                                          
*                                                                               
         MVC   PLIST(3),0(R4)                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD & BCLT                                           
         MVC   KEY+4(3),0(R4)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PPRDS44                                                          
*                                                                               
         MVC   PLIST(3),0(R4)                                                   
         MVC   PLIST+4(7),=C'UNKNOWN'                                           
         B     PPRDS46                                                          
*                                                                               
PPRDS44  GOTO1 GETREC                                                           
*                                                                               
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   PLIST+4(20),PNAME                                                
         DROP  R1                                                               
PPRDS46  LA    R5,132(,R5)                                                      
         LA    R4,3(,R4)           POINT TO NEXT IN PROD LIST                   
         SHI   R3,3                                                             
         LTR   R3,R3                                                            
         BNZ   PPRDS40                                                          
PPRDS50  STC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
         B     LRRX                                                             
         EJECT                                                                  
         DROP  R5                                                               
*------------------------------------------------------------------*            
*        PRINT HIDEF AND CENTERCUT IF ANY                                       
*        (R2 = POINTS TO PRINT AREA)                                            
*------------------------------------------------------------------*            
PHDCNTR  NTR1                                                                   
         OC    SVHIDEF,SVHIDEF     ANY HIDEF TO PRINT                           
         BNZ   PHDCNT00                                                         
         OC    SVCNTCT,SVCNTCT     ANY CNTRCUT TO PRINT                         
         BZ    PHDCNX                                                           
*                                                                               
PHDCNT00 LA    R5,P1                                                            
         USING PRTLINE,R5                                                       
         LA    R0,4                4 PRINT LINES                                
         LA    R2,PTITLE           PT TO PRINT AREA                             
         CLC   132(L'PTITLE,R2),SPACES                                          
         BNH   PHDCN02                                                          
         LA    R2,132(R2)                                                       
         BCTR  R0,0                                                             
*                                                                               
PHDCN02  CLC   0(L'PTITLE,R2),SPACES ANYTHING HERE?                             
         BNH   PHDCN05                                                          
*                                                                               
         LA    R2,132(R2)                                                       
         BCT   R0,PHDCN02                                                       
         B     PHDCNX                                                           
*                                                                               
PHDCN05  OC    SVHIDEF,SVHIDEF     ANY HIDEF TO PRINT                           
         BZ    PHDCN10                                                          
*                                                                               
         MVC   0(5,R2),=C'HDEF='                                                
         MVC   5(12,R2),SVHIDEF                                                 
         LA    R2,132(R2)                                                       
         XC    SVHIDEF,SVHIDEF                                                  
         BCTR  R0,0                                                             
*                                                                               
PHDCN10  OC    SVCNTCT,SVCNTCT     ANY CNTRCUT TO PRINT                         
         BZ    PHDCNX                                                           
         LTR   R0,R0                                                            
         BZ    PHDCNX                                                           
         MVC   0(5,R2),=C'CNTR='                                                
         MVC   5(12,R2),SVCNTCT                                                 
         XC    SVCNTCT,SVCNTCT                                                  
PHDCNX   B     LRRX                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
DELMSG   DS    0CL9                                                             
         DC    C'*'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------*            
* SEE IF ANY RECORDS WERE SELECTED                                              
*------------------------------------------------------------------*            
LRANY    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    FTRFLAG,FILFTR      THIS A DUMP COMMLS RUN                       
         BZ    LRANY20                                                          
*                                                                               
* IF NO COMMERCIALS WRITTEN, NO REPORT                                          
*                                                                               
         CP    CMLFILCT,=P'0'      ANY COMMLS WRITTEN                           
         BE    LRANYX                                                           
*                                                                               
         BRAS  RE,PUTREP           GO PUT OUT REPORT(S)                         
         B     LRANYX                                                           
*                                                                               
LRANY20  DS   0H                                                                
         OC    RECCT,RECCT         WERE ANY RECS SELECTED                       
         BNZ   *+12                                                             
         BRAS  RE,LRNONE                                                        
         B     LRANYX                                                           
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRANYX                                                           
         BRAS  RE,PXREF                                                         
LRANYX   XIT1                                                                   
*                                                                               
LRNONE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    RECCT,RECCT         WERE ANY RECS SELECTED                       
         BNZ   *+12                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   LRN10                                                            
         XIT1                                                                   
*                                                                               
LRN10    MVC   GERROR,=Y(NORECSEL)                                              
         MVI   GMSGTYPE,C'I'       SET TEXT MESSAGE                             
         LA    R2,TRAMEDH                                                       
         GOTO1 VTRAERR             GETTXT CALL                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
FTR      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING CMLDTAEL,R6                                                      
         TM    FTRFLAG,DELFTR      ONLY DELETED CML'S                           
         BZ    FTR04                NO                                          
         TM    CMLSTAT,X'80'       THIS DELETED                                 
         BZ    FTRNO                                                            
         B     FTR06                                                            
FTR04    TM    CMLSTAT,X'80'                                                    
         BNZ   FTRNO                                                            
*                                                                               
FTR06    OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    FTR10               NO                                           
         CLC   TYPEFTR,CMLTYPE     THIS IT                                      
         BNE   FTRNO                                                            
*                                                                               
FTR10    CLI   SLNFTR,0            SPOT LEN FILTER                              
         BE    FTR20                NO                                          
         CLC   SLNFTR,CMLSLN       DOES THIS MATCH                              
         BNE   FTRNO               NO                                           
*                                                                               
FTR20    OC    RLDTFTR,RLDTFTR     RELEASE DATE FILTER                          
         BZ    FTR30                                                            
*                                                                               
         CLI   RLDTSFTR,0                                                       
         BE    FTR24                                                            
*                                                                               
         CLI   RLDTSFTR,X'6E'      GREATER THAN                                 
         BNE   FTR22                MUST BE LESS THAN                           
*                                                                               
         CLC   CMLRLSE,RLDTFTR     FILTER TO RELEASE                            
         BL    FTRNO                BYPASS                                      
         B     FTR30               CK NEXT FILTER                               
*                                                                               
FTR22    CLI   RLDTSFTR,X'4C'      LESS THAN                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CMLRLSE,RLDTFTR                                                  
         BH    FTRNO                                                            
         B     FTR30                                                            
*                                                                               
FTR24    CLC   RLDTFTR,CMLRLSE                                                  
         BNE   FTRNO                                                            
*                                                                               
FTR30    OC    RCDTFTR,RCDTFTR     RECALL DATE FILTER                           
         BZ    FTR40                                                            
*                                                                               
         CLI   RCDTSFTR,0                                                       
         BE    FTR34                                                            
         CLI   RCDTSFTR,X'6E'      GREATER THAN                                 
         BNE   FTR32                MUST BE LESS THAN                           
         CLC   CMLRCL,RCDTFTR      FILTER TO RECALL                             
         BL    FTRNO                BYPASS                                      
         B     FTR40               CK NEXT FILTER                               
*                                                                               
FTR32    CLI   RCDTSFTR,X'4C'      LESS THAN                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CMLRCL,RCDTFTR                                                   
         BH    FTRNO                                                            
         B     FTR40                                                            
*                                                                               
FTR34    CLC   RCDTFTR,CMLRCL                                                   
         BNE   FTRNO                                                            
         EJECT                                                                  
FTR40    OC    PRODFTR,PRODFTR     IS THERE A PROD FILTER                       
         BZ    FTR50                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'        PRD LIST ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   FTRNO                                                            
*                                                                               
         LLC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'3'            SET FOR BCT                                  
         LTR   R0,R1                                                            
         BZ    FTRNO                                                            
         LA    R1,2(R6)                                                         
*                                                                               
FTR42    CLC   PRODFTR,0(R1)                                                    
         BE    FTR50                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,FTR42                                                         
         B     FTRNO                                                            
*                                                                               
FTR50    DS    0H                                                               
         TM    FTRFLAG,CMLFTR      SHOW SPECIFIC CML (HIDEF,CNTR,ADID)          
         BO    *+12                                                             
         TM    FTRFLAG,ADIDFTR     ONLY ADID COMMLS?                            
*NOP     BZ    FTR60                                                            
         BZ    FTR100                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BE    FTR54                                                            
*                                                                               
         TM    FTRFLAG,CMLFTR      SHOW SPECIFIC CML (HIDEF,CNTR,ADID)          
         BZ    FTRNO                                                            
         BO    FTR60                                                            
*                                                                               
FTR54    TM    FTRFLAG,CMLFTR      CML=  FILTER (HIDEF,CNTR,ADID)               
         BO    *+14                                                             
         OC    ADIDFLT,ADIDFLT     LOOKING FOR SPECIFIC ADID                    
         BZ    FTR100               NO, DONE                                    
*                                                                               
         USING CMLADIEL,R6                                                      
*                                                                               
         OC    CMLADID,CMLADID     ANY ADID?                                    
         BZ    FTR60                                                            
         CLC   CMLADID,ADIDFLT     SAME ?                                       
         BE    FTR100               YES                                         
         CLC   CMLADID,CMLFLT      SAME CML                                     
         BE    FTR100               YES                                         
*                                                                               
FTR60    DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'24'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FTRNO                                                            
*                                                                               
         USING CMLXDTEL,R6                                                      
*                                                                               
         OC    CMLXHDEF,CMLXHDEF   ANY HIDEF?                                   
         BZ    FTR70                                                            
         TM    FTRFLAG,CMLFTR      CML=  FILTER (HIDEF,CNTR,ADID)               
         BZ    FTR100                                                           
         CLC   CMLXHDEF,CMLFLT     SAME ?                                       
         BE    FTR100               YES                                         
*                                                                               
FTR70    DS    0H                                                               
         TM    FTRFLAG,CMLFTR      CML=  FILTER (HIDEF,CNTR,ADID)               
         BO    FTR72                                                            
         TM    FTRFLAG,CCUTFTR     ONLY CNTRCUT COMMLS?                         
         BO    FTR72                                                            
         TM    FTRFLAG,ADIDFTR                                                  
         BZ    FTR100                                                           
         B     FTRNO                                                            
*                                                                               
FTR72    CLI   0(R6),X'24'         DID WE GET THE ELEM ALREADY                  
         BE    FTR74                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'24'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FTRNO                                                            
*                                                                               
         USING CMLXDTEL,R6                                                      
*                                                                               
FTR74    OC    CMLXCNTR,CMLXCNTR   ANY CENTERCUT?                               
         BZ    FTRNO                                                            
         TM    FTRFLAG,CMLFTR      CML=  FILTER (HIDEF,CNTR,ADID)               
         BZ    FTR100                                                           
         CLC   CMLXCNTR,CMLFLT     SAME ?                                       
         BNE   FTRNO                                                            
*                                                                               
FTR100   CR    R1,R1               SET COND CODE FILTERED OK                    
         J     EXIT                                                             
FTRNO    CR    RB,RD               SET COND CODE NO FILTER                      
         J     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        VALIDATE MATCH MAKER FIELDS                                            
*------------------------------------------------------------------*            
CLRHELP  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TRATYPLH                                                      
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         ZIC   R0,0(R2)            TO NEXT HELP LINE                            
         AR    R2,R0                                                            
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
* FOR ACTION ADD, ADD AD-ID ELEMENT FOR PACKED CML                              
* SO ALWAYS HAVE PACKED AND UNPACKED ADID IN A0 ELEMENT FOR KATZ                
*--------------------------------------------------------------                 
*                                                                               
AADIDEL  NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'A0'        AD-ID  ELEM                                  
         LA    R6,ELEM                                                          
         USING CMLADIEL,R6                                                      
*                                                                               
         LA    R6,ELEM                                                          
         USING CMLADIEL,R6                                                      
         MVI   CMLADIEL,X'A0'                                                   
         MVI   CMLADILN,CMLADIDL-CMLADIEL                                       
         MVC   CMLADID,TRACML                                                   
         OC    CMLADID,SPACES                                                   
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',CMLADID),CMLADIDP                             
*                                                                               
         MVC   CMLADIDT(6),ADIDYTM    MOVE IN BOTH DATE & TIME                  
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,CMLADIDT)  SAVE DATE                        
*                                                                               
         TIME  DEC                                                              
         STCM  R0,15,CMLADITM      SAVE TIME                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* MAKE SURE THIS ADID DOES NOT EXIST AS ADID OR HIDEF ON ANOTHER REC            
*---------------------------------------------------------------------          
DUPCHK   NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(8),HOLDCML    PACKED CML                                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUPADINO                                                         
                                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C2'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   EXIT                                                             
                                                                                
DUPADINO DS    0H                                                               
         LA    R2,TRACMLH                                                       
         MVC   GERROR,=Y(DUPCMFIL)                                              
         J     TRAPERR2                                                         
                                                                                
         EJECT                                                                  
*------------------------------------------------------------------*            
*        VALIDATE MATCH MAKER FIELDS                                            
*------------------------------------------------------------------*            
VALMAT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         NI    FLAGS2,X'FF'-MATELEM                                             
         XC    FLD,FLD                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,CMLMATEQ                                                  
         BRAS  RE,GETEL            COMPARISON TO LOG CHANGES                    
         BNE   VALMAT05                                                         
                                                                                
         GOTO1 REMELEM                                                          
         MVC   FLD,ELEM                                                         
                                                                                
VALMAT05 LA    R2,TRADT1H                                                       
         LHI   R3,6                                                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLMATCH,R6                                                      
         MVI   CMLMATEL,CMLMATEQ                                                
         MVI   CMLMATLN,CMLMATDL                                                
         LA    R5,CMLMPER1                                                      
                                                                                
*        PERIOD VALIDATION LOOP                                                 
VALMAT10 DS    0H                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VALMAT50            NO, GO TO NEXT FIELD                         
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),PERVALST                                          
         CLI   DMCB+4,0                                                         
         BNE   INVPER                                                           
                                                                                
         MVC   0(4,R5),PERVALST+PVALCSTA-PERVALD                                
         OI    FLAGS2,MATELEM                                                   
                                                                                
VALMAT50 DS    0H                                                               
         LA    R2,L'TRADT1H+L'TRADT1(R2)                                        
         LA    R5,L'CMLMPER1(R5)                                                
         BCT   R3,VALMAT10                                                      
                                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                 
         L     RF,0(R1)                                                         
                                                                                
         XC    CMLMSTIM,CMLMSTIM                                                
         MVC   CMLMETIM,=X'FFFF'                                                
                                                                                
         LA    R2,TRASTIMH                                                      
         CLI   5(R2),0                                                          
         BE    VALMAT60                                                         
                                                                                
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),WORK                                              
         CLI   0(R1),X'FF'                                                      
         BE    INVTERR                                                          
         MVC   CMLMSTIM,WORK                                                    
         OI    FLAGS2,MATELEM                                                   
                                                                                
VALMAT60 DS    0H                                                               
         LA    R2,TRAETIMH                                                      
         CLI   5(R2),0                                                          
         BE    VALMAT70                                                         
                                                                                
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),WORK                                              
         CLI   0(R1),X'FF'                                                      
         BE    INVTERR                                                          
         MVC   CMLMETIM,WORK                                                    
         OI    FLAGS2,MATELEM                                                   
                                                                                
VALMAT70 DS    0H                                                               
         LA    R2,TRADLYH                                                       
         CLI   TRADLYH+5,0                                                      
         BE    VALMAT80                                                         
                                                                                
         CLI   TRADLY,C'N'                                                      
         BE    VALMAT80                                                         
         CLI   TRADLY,C'Y'                                                      
         BNE   INVDLY                                                           
         OI    CMLMFLAG,CMLMFDAY                                                
         OI    FLAGS2,MATELEM                                                   
                                                                                
VALMAT80 DS    0H                                                               
         TM    FLAGS2,MATELEM                                                   
         BNZ   VALMAT85                                                         
         OC    FLD,FLD                                                          
         BZ    VALMATX                                                          
                                                                                
VALMAT85 DS    0H                                                               
E        USING CMLMATEL,FLD                                                     
         USING CMLMATEL,R6                                                      
         OC    E.CMLMETIM,E.CMLMETIM   MAINTAIN INTEGRITY OF CHANGE             
         BNZ   *+10                    HISTORY                                  
         MVC   E.CMLMETIM,=X'FFFF'                                              
                                                                                
         CLC   E.CMLMSTIM(4),CMLMSTIM                                           
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_TIME                                              
*                                                                               
         CLC   E.CMLMFLAG,CMLMFLAG                                              
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_DLY                                               
*                                                                               
         CLC   E.CMLMPER1(24),CMLMPER1                                          
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_MDTS                                              
*                                                                               
VALMAT90 TM    FLAGS2,MATELEM                                                   
         BZ    VALMATX                                                          
         GOTO1 ADDELEM                                                          
                                                                                
VALMATX  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
INVPER   MVC   GERROR,=Y(INVPERMS)                                              
         J     TRAPERR2                                                         
                                                                                
INVTERR  MVI   ERROR,INVTIME                                                    
         GOTO1 ERREX                                                            
                                                                                
INVDLY   MVC   CONHEAD(L'INVDLYMS),INVDLYMS                                     
         B     VALMERR                                                          
                                                                                
VALMERR  GOTO1 ERREX2                                                           
                                                                                
INVDLYMS DC    C'* ERROR * INVALID INPUT *'                                     
         EJECT                                                                  
*----------------------------------------------------------------*              
* VALIDATE TITLE FIELDS -                                                       
* NOTE THAT ON ENTRY R6 POINTS TO X'10' ELEMENT IN RECORD                       
*----------------------------------------------------------------*              
                                                                                
         USING CMLDTAEL,R6                                                      
VALTTL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,TRADSC1H         COMMERCIAL TITLE 1                           
         GOTO1 ANY                                                              
                                                                                
*        MVC   WORK(24),TRADSC1                                                 
         OC    WORK(24),SPACES                                                  
         MVC   WORK+24(24),TRADSC2                                              
         OC    WORK+24(24),SPACES                                               
         MVC   WORK+48(24),TRADSC3                                              
         OC    WORK+48(24),SPACES                                               
                                                                                
         CLI   TRADSC2H+5,0        ANY TITLE 2 ENTERED                          
         BNE   VALTTL2             YES                                          
         CLI   TRADSC3H+5,0        IF NOT, SHOULD BE NO  TITLE 3                
         BE    VALTTL2                                                          
         LA    R2,TRADSC2H                                                      
         MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
                                                                                
VALTTL2  DS    0H                                                               
*        CLC   CMLTITLE,WORK       UPDATE TITLE IN CMLDTAEL                     
*        BE    *+8                 BUT DO NOT INSERT IN RECORD YET              
*        OI    CHGFLAG1,CMLCH_DESC                                              
         MVC   CMLTITLE,WORK                                                    
                                                                                
         L     R6,AIO              TEST FOR CHANGES IT TITLE2/3                 
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VALTTL10                                                         
         OC    3(24,R6),SPACES                                                  
         CLC   WORK(24),3(R6)                                                   
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   VALTTL10                                                         
         OC    3(24,R6),SPACES                                                  
         CLC   WORK+24(24),3(R6)                                                
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   VALTTL10                                                         
         OC    3(24,R6),SPACES                                                  
         CLC   WORK+48(24),3(R6)                                                
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
                                                                                
VALTTL10 GOTO1 REMELEM             DELETE X'30' ELEMENTS                        
         XC    ELEM,ELEM                                                        
                                                                                
         MVI   ELEM,X'30'          BUILD NEW X'30' ELEMS AND INSERT             
         MVI   ELEM+1,27                                                        
         MVI   ELEM+2,0            SEQUENCE NUMBER                              
         MVC   ELEM+3(L'TRADSC2),WORK                                           
         OC    ELEM+3(24),SPACES                                                
         GOTO1 ADDELEM                                                          
                                                                                
         MVI   ELEM+2,1            SEQUENCE NUMBER                              
         MVC   ELEM+3(L'TRADSC2),WORK+24                                        
         OC    ELEM+3(24),SPACES                                                
         GOTO1 ADDELEM                                                          
                                                                                
         MVI   ELEM+2,2                                                         
         MVC   ELEM+3(L'TRADSC3),WORK+48                                        
         OC    ELEM+3(24),SPACES                                                
         GOTO1 ADDELEM                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF/CENTER CUT FIELD ABOUT TO BE ADDED                     
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
*----------------------------------------------------------------*              
HDDUPS   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0AC1'                                                
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+5(8),WORK+12                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDDXIT                                                           
                                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C2'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDDXIT                                                           
                                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C3'                                                      
         GOTO1 HIGH                                                             
                                                                                
HDDXIT   J     EXIT                                                             
         LTORG                                                                  
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF/CENTER CUT FIELD ABOUT TO BE ADDED                     
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
*----------------------------------------------------------------*              
CCDUPS   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0AC2'                                                
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+5(8),WORK+12                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CCDXIT                                                           
                                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C3'                                                      
         GOTO1 HIGH                                                             
                                                                                
CCDXIT   J     EXIT                                                             
         LTORG                                                                  
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF/CENTER CUT FIELD ABOUT TO BE ADDED                     
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
* ASSUME KEY IS SET TO THE KEY TO BE ADDED OR WRITTEN                           
*----------------------------------------------------------------*              
DELPSSV  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEYSAVE,0         
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DELPXIT                                                          
                                                                                
         MVC   KEY,KEYSAVE                                                      
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'SPTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BNE   DELPERRX                                                         
                                                                                
DELPXIT  SR    RB,RB                                                            
DELPERRX LTR   RB,RB                                                            
         J     EXIT                                                             
                                                                                
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF/CENTER CUT FIELD ABOUT TO BE ADDED                     
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
* ASSUME KEY IS SET TO THE KEY TO BE ADDED OR WRITTEN                           
*----------------------------------------------------------------*              
ADDPSSV  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEYSAVE,0         
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADDPSS                                                           
                                                                                
WRTPSS   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         NI    KEY+13,X'FF'-X'80'                                               
         MVC   KEY+14(4),SVDSKAD                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'SPTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    ADDPXIT                                                          
         BNE   ADDPERRX                                                         
                                                                                
ADDPSS   DS    0H                                                               
         MVC   KEY+14(4),SVDSKAD                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD ',=C'SPTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BNE   ADDPERRX                                                         
                                                                                
ADDPXIT  SR    RB,RB                                                            
ADDPERRX LTR   RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
*----------------------------------------------------------------*              
* VALIDATE DATA IN COMMERCIAL FIELD AT 0(R2)                                    
* FOR NOW, 8 CHARS IS CMML, 12 IS ADID                                          
*----------------------------------------------------------------*              
                                                                                
VALCMML  NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),8                                                          
         BNE   VALC10                                                           
*                                                                               
         MVC   DUB,8(R2)           PRESUMING IT'S GOOD                          
         LA    R0,8                                                             
         LA    R1,8(R2)                                                         
*                                                                               
VALC2    CLI   0(R1),C'A'                                                       
         BL    VALC4                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VALC2                                                         
         B     VALC20                                                           
*                                                                               
VALC4    CLI   SVT1PR10,C'A'       ALLOW ALL NON-STD CML CODES T1 PR 10         
         BE    VALC20                                                           
         CLI   SVT1PR10,C'Y'       ALLOW NON-STD COML CODES T1 PROF10           
         JNE   BADCMLER                                                         
         CLI   QMED,C'T'           NOT ALLOWED FOR TV                           
         JE    BADCMLER                                                         
         B     VALC20                                                           
*                                                                               
VALC10   DS    0H                                                               
         CLI   5(R2),9             MUST BE AT LEAST 9 CHAR                      
         BL    VALCERR                                                          
*                                                                               
         LLC   R0,5(R2)                                                         
         LA    R1,8(R2)                                                         
*                                                                               
VALC14   CLI   0(R1),C'A'                                                       
         BL    VALCERR                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VALC16                                                           
                                                                                
         CLI   0(R1),C'0'                                                       
         BL    VALCERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    VALCERR                                                          
*                                                                               
VALC16   LA    R1,1(R1)                                                         
         BCT   R0,VALC14                                                        
*                                                                               
         OC    8(12,R2),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',8(R2)),DUB                                    
         BNE   VALCERR                                                          
*                                                                               
VALC20   CLI   CHKCMML,C'N'               TEST VALIDATE                         
         JE    EXIT                                                             
         CLC   DUB,SVKEY+CMLKCML-CMLKEY   TEST SAME AS THIS CMML                
         JE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),DUB                                                     
*                                                                               
         CLI   5(R2),8             TEST INPUT IS ADID                           
         BNH   *+10                                                             
         MVC   KEY(2),=X'0AC1'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VALC30                                                           
         MVI   ERROR,NOTFOUND                                                   
         J     ERRX                                                             
                                                                                
* NEED TO CHECK FOR SOFT DELETE                                                 
                                                                                
VALC30   L     R0,AIO              SAVE CURRENT VALUE                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         ST    R0,AIO              RESTORE                                      
*                                                                               
         L     RE,AIO2                                                          
         TM    CMLSTAT-CMLRECD(RE),X'80'   TEST SOFT DELETED                    
         BO    DELCERR                                                          
         J     EXIT                                                             
*                                                                               
VALCERR  MVC   GERROR,=Y(CMLLENMS)                                              
         J     TRAPERR2                                                         
*                                                                               
DELCERR  MVC   GERROR,=Y(CMLISDEL)                                              
         J     TRAPERR2                                                         
         LTORG                                                                  
*----------------------------------------------------------------*              
* KEEP LAST 6 CHANGE ELEMENTS                                                   
* AIO1+2000 HAS PREVIOUS VERSION OF RECORD                                      
* SAVE 8 PRODUCTS, START DATE, END DATE, AND SLN FROM IT                        
* ELEMENT LEN IS FIXED, SO FIELDS SAVED EVEN IF THEY DON'T CHANGE               
*----------------------------------------------------------------*              
                                                                                
SETCHGEL NTR1  BASE=*,LABEL=*                                                   
         OC    CHGFLAGS,CHGFLAGS    TEST ANY CHANGES                            
         JZ    EXIT                                                             
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         JE    EXIT                OBVIOUSLY THIS IS NOT A CHANGE!              
                                                                                
         XC    ELEM,ELEM                                                        
C        USING CMLCHEL,ELEM                                                     
         MVI   C.CMLCHEL,X'C0'                                                  
         MVI   C.CMLCHLEN,CMLCHELX-CMLCHEL                                      
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,C.CMLCHDAT)                                 
         BAS   RE,GETTIME                                                       
         MVC   C.CMLCHTIM,FULL                                                  
                                                                                
         MVC   C.CMLCHWHO,TWAORIG   MOVE IN ID                                  
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    SETCHG5                                                          
         MVC   C.CMLCHWHO,FAPASSWD YES SO USE THIS ID                           
         OI    C.CMLCHFLG,X'80'    SET FLAG TO SAY THIS IS ID                   
         DROP  R1                                                               
                                                                                
SETCHG5  OC    ASECBLK,ASECBLK     IS NEW SECURITY ACTIVE                       
         JZ    SETCHG10                                                         
         L     R1,ASECBLK          NEW SECURITY BLOCK                           
         USING SECD,R1                                                          
         MVC   C.CMLCHWHO,SECPID   USER'S PERSONAL ID                           
         DROP  R1                                                               
                                                                                
SETCHG10 MVC   C.CMLCHDT1(3),CHGFLAGS MOVE CHANGE FLAGS                         
                                                                                
         L     R6,AIO1             SAVE OLD DATA                                
         LA    R6,2000(R6)                                                      
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
                                                                                
         MVC   C.CMLCHSDT,CMLRLSE                                               
         MVC   C.CMLCHEDT,CMLRCL                                                
         MVC   C.CMLCHSLN,CMLSLN                                                
                                                                                
         L     R6,AIO1                                                          
         LA    R6,2000(R6)                                                      
         MVI   ELCODE,X'20'        PRODUCT LIST                                 
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         CHI   RF,8                                                             
         BNH   *+8                                                              
         LHI   RF,8                                                             
         AHI   RF,-3               SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   C.CMLCHPRD(0),2(R6)                                              
                                                                                
         L     R6,AIO1                                                          
         LA    R6,2000(R6)                                                      
         MVI   ELCODE,X'B0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SETCHG18                                                         
         USING CMLMATEL,R6                                                      
                                                                                
         MVC   C.CMLCHSTM,CMLMSTIM                                              
         MVC   C.CMLCHETM,CMLMETIM                                              
                                                                                
SETCHG18 L     R6,AIO                                                           
         MVI   ELCODE,X'C0'        FIND MOST RECENT CHANGEL                     
         BRAS  RE,GETEL                                                         
         BNE   SETCHG30                                                         
         USING CMLCHEL,R6                                                       
                                                                                
         CLC   CMLCHDAT(11),C.CMLCHDAT   TEST SAME DATE/PERSON                  
         BNE   SETCHG20                                                         
         MVC   CMLCHTIM,C.CMLCHTIM       MOVE IN LATEST TIME                    
         OC    CMLCHDT1(3),C.CMLCHDT1    'OR' IN NEW ACTIVITY                   
         J     EXIT                                                             
                                                                                
SETCHG20 L     R6,AIO              IF > 6 ELEMENTS, DELETE OLDEST               
         SR    R5,R5                                                            
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
                                                                                
SETCHG22 BRAS  RE,NEXTEL                                                        
         BNE   SETCHG30                                                         
         AHI   R5,1                                                             
         CHI   R5,6                                                             
         BL    SETCHG22                                                         
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
                                                                                
SETCHG30 L     R6,AIO              ADD NEW BEFORE OTHER CHGELS                  
         BRAS  RE,GETEL               OR AT EOR                                 
         GOTO1 VRECUP,DMCB,AIO,ELEM,(R6)                                        
         J     EXIT                                                             
                                                                                
GETTIME  NTR1                                                                   
         XC    FULL,FULL                                                        
         THMS                      TIME R1=0HHMMSS+                             
         LR    R0,R1               CONVERT TO BINARY HMS                        
         SRDL  R0,12                                                            
         SRL   R1,20                                                            
         XC    DUB,DUB                                                          
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+2                                                        
         SRDL  R0,8                                                             
         SRL   R1,20                                                            
         LA    RF,X'0C'                                                         
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+1                                                        
         SRDL  R0,28                                                            
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* READ TN PROFILE *                                                             
*                                                                               
GETPRF   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    GETPRF10                                                         
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    GETPRF10                                                         
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
GETPRF10 GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* MUST READ TN1 PROFILE                                                         
*                                                                               
         MVC   WORK(4),=X'A2E3D5F1'   READ TN1 PROFILE                          
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
* MUST READ TN2 PROFILE                                                         
*                                                                               
         MVI   WORK+3,X'F2'           READ TN2 PROFILE                          
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVTN2PR1,ELEM                                                    
         MVC   SVTN2PR5,ELEM+4                                                  
         MVC   SVTN2PR9,ELEM+8                                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* VALIDATE COMMERCIAL CODE. PARTIAL INPUT ALLOWED FOR LIST                      
* IF 8 CHARS, INPUT IS ISCI                                                     
* FIRST 4  ALPHA, 5TH NUMERIC OR -, LAST 3 NUMERIC                              
* 9-12 CHARS IS ADID                                                            
* ON EXIT, 8 BYTE CMML IN WORK                                                  
*=================================================================              
*                                                                               
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ADIDFLAG,C'N'       ASSUME ISCI CMML                             
         XC    WORK,WORK           CLEAR CMML OUTPUT                            
         CLI   5(R2),0                                                          
         BE    VCML2                                                            
         GOTO1 ANY                 GET SPACE FILLED INPUT IN WORK               
*                                                                               
VCML2    CLI   ACTNUM,ACTLIST                                                   
         BE    VCML60                                                           
*                                                                               
         CLI   5(R2),8             MUST BE LEN OF 8                             
         BH    VCML50               UNLESS AD-ID                                
         BL    BADLENER                                                         
*                                                                               
         CLC   TRACML(8),=8C'9'    HSE RECORD                                   
         BE    VCMLX                                                            
*                                                                               
         LA    R0,4                                                             
         LA    R1,TRACML                                                        
*                                                                               
VCML10   CLI   0(R1),C'A'                                                       
         BL    VCML30                                                           
         CLI   0(R1),C'Z'                                                       
         BH    VCML30                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,VCML10                                                        
*                                                                               
         LA    R0,4                                                             
         CLI   0(R1),C'-'                                                       
         BE    VCML24                                                           
*                                                                               
VCML20   CLI   0(R1),C'0'                                                       
         BL    VCML30                                                           
         CLI   0(R1),C'9'                                                       
         BH    VCML30                                                           
*                                                                               
VCML24   LA    R1,1(R1)                                                         
         BCT   R0,VCML20                                                        
         B     VCMLX                                                            
*                                                                               
VCML30   CLI   SVPROF+3,C'Y'       IS NUMERIC WITH DASHES ALLOWED               
         BE    *+14                                                             
         MVC   GERROR,=Y(BADCML)                                                
         J     TRAPERR2                                                         
*                                                                               
         LA    R0,8                                                             
         LA    R1,TRACML+7                                                      
VCML32   CLI   0(R1),C'-'          SEARCH BACKWARDS FOR DASHES                  
         BNE   VCML34                                                           
         BCTR  R1,0                                                             
         BCT   R0,VCML32                                                        
         MVC   GERROR,=Y(BDCMLD)                                                
         J     TRAPERR2            DO NOT ALLOW ALL 8 DASHES                    
*                                                                               
VCML34   CLI   0(R1),C' '          REST MUST NOT BE BLANK                       
         BNL   *+14                                                             
         MVC   GERROR,=Y(BDCMLD)                                                
         J     TRAPERR2                                                         
*                                                                               
         BCTR  R1,0                                                             
         BCT   R0,VCML34                                                        
VCMLX    XIT1                                                                   
         EJECT                                                                  
* SEE IF THIS IS AN AD-ID CODE                                                  
                                                                                
VCML50   CLI   5(R2),12                                                         
         BH    BADLENER                                                         
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',WORK),DUB                                     
         BNE   BADLENER                                                         
*                                                                               
         MVI   ADIDFLAG,C'Y'       SET THAT INPUT IS ADID                       
         MVC   WORK,DUB            MOVE PACKED CMML TO WORK                     
         CLI   ACTNUM,ACTADD                                                    
         BE    VCMLX                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD    A-M/CLT                                       
         MVC   KEY+5(8),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCML52                                                           
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
VCML52   LA    R0,TRACMLH                                                       
         CR    R0,R2               ARE WE DOING CMML IN KEY                     
         BNE   VCMLX               NO - EXIT                                    
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),5(R6)       MOVE CMML FROM REC                           
         MVC   8(12,R2),WORK       MOVE CMML TO SCREEN                          
         OI    6(R2),X'80'         FORCE TRANSMIT                               
                                                                                
         MVI   ADIDFLAG,C'N'                                                    
         TM    15(R6),X'01'        IS CML PACKED IN KEY                         
         BZ    VCMLX               NO - SO IT IS ISCI                           
         GOTO1 VTRPACK,DMCB,(C'U',WORK),8(R2)    REDISPLAY CMML                 
         MVI   ADIDFLAG,C'Y'                                                    
         B     VCMLX                                                            
         EJECT                                                                  
*===========================================================                    
* ACTION IS LIST                                                                
* TO GET A VALID STARTING POINT FOR READ HIGH,                                  
* REPLACE C' ' WITH C'A'. SPACE FILLED FIELD IS IN WORK.                        
*===========================================================                    
                                                                                
VCML60   CLI   5(R2),12                                                         
         JH    BADLENER                                                         
         MVI   ADIDFLAG,C'A'       IF NO INPUT, SET DOING ADIDS                 
         CLI   5(R2),0             TEST NO INPUT                                
         BE    VCMLX                                                            
* NEED TO REPLACE SPACES IN FIRST 8 CHARS WITH C'A'                             
         LA    R1,WORK+1                                                        
         LA    R0,7                                                             
*                                                                               
VCML62   CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         MVI   0(R1),C'A'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,VCML62                                                        
*                                                                               
VCML64   MVC   WORK+12(12),WORK                                                 
         GOTO1 VTRPACK,DMCB,(C'P',WORK+12),WORK   SEE IF IT PACKS               
         BE    VCMLX                                                            
*                                                                               
VCMLERR  MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
                                                                                
BADLENER MVC   GERROR,=Y(NOT812)                                                
         J     TRAPERR2                                                         
                                                                                
BADCMLER MVC   GERROR,=Y(BADCML)                                                
         J     TRAPERR2                                                         
                                                                                
CMLBLKER MVC   GERROR,=Y(CMLNOBLK) BLANKS NOT ALLOWED IN CML CODE               
         J     TRAPERR2                                                         
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE ACTUAL ID AND IT'S SLN TO ACTSLN                                     
*  R2 = A(FLD HDR)                                                              
*                                                                               
VID      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),8             MUST BE AT LEAST 8 CHARS                     
         BL    BADLENEV                                                         
         BRAS  RE,VCML             VALIDATE COMMERCIAL                          
*                                  CMML RETURNED IN WORK                        
* CMML CAN'T COVER ITSELF                                                       
*                                                                               
         L     RF,AIO                                                           
         CLC   CMLKCML-CMLKEY(8,RF),WORK                                        
         BNE   *+14                                                             
         MVC   GERROR,=Y(SAMECMML)                                              
         B     TRAPERRV                                                         
*                                                                               
* NOW READ REC                                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,WORK        CMML RETURNED BY VCML                        
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         DROP  R4                                                               
*                                                                               
VID2     GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VID4                                                             
         MVI   ERROR,NOCOMM                                                     
         GOTO1 ERREX                                                            
VID4     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
* CHECK IF COV CMML                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   GERROR,=Y(CMLISCOV)                                              
         B     TRAPERRV                                                         
*                                                                               
* CHECK IF CMML IS DELETED                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'                                                    
         BZ    *+14                                                             
         MVC   GERROR,=Y(CMLISDEL)                                              
         B     TRAPERRV                                                         
*                                                                               
* CHECK IF CMML COVERS DATE SPREAD                                              
*                                                                               
         LR    R4,R6               SAVE PTR TO ACT X'10'                        
         L     R6,AIO1             GET X'10' FROM COV                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CODE FOR DATE CHECK IS NOP'ED PER KARI                                        
*                                                                               
         B     VID10                                                            
         CLC   CMLRLSE-CMLDTAEL(3,R4),CMLRLSE                                   
         BH    *+14                ERROR                                        
         CLC   CMLRCL-CMLDTAEL(3,R4),CMLRCL                                     
         BNL   *+14                NO ERROR                                     
         MVC   GERROR,=Y(CMLBDDT)                                               
         B     TRAPERRV                                                         
*                                                                               
* CHECK IF CMML HAS SAME TYPE AS COVER                                          
*                                                                               
VID10    DS    0H                                                               
         CLC   CMLTYPE,CMLTYPE-CMLDTAEL(R4)                                     
         BE    *+14                                                             
         MVC   GERROR,=Y(TYPEERR)                                               
         B     TRAPERRV                                                         
         LR    R6,R4                                                            
*                                                                               
* ADD SPOT LENGTH TO ACCUM                                                      
*                                                                               
         ZIC   R1,ACTSLN                                                        
         ZIC   RF,CMLSLN                                                        
         AR    R1,RF                                                            
         STC   R1,ACTSLN                                                        
         DROP  R6                                                               
*                                                                               
* CHECK IF CMML HAS PROD IN COV CMML                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R6               SAVE ACTUAL'S X'20' EL                       
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ZIC   R1,1(R4)            OUTER LOOP COUNT                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         LA    R4,2(R4)            R4 = A(1'ST PROD) IN ACTUAL                  
OUTER    IC    RF,1(R6)            INNER LOOP COUNT                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RE,2(R6)            RE = A(1'ST PROD) IN COV                     
INNER    CLC   0(1,RE),0(R4)                                                    
         BE    HIT                                                              
         LA    RE,1(RE)                                                         
         BCT   RF,INNER                                                         
         LA    R4,1(R4)                                                         
         BCT   R1,OUTER                                                         
         MVC   GERROR,=Y(NOPRDHIT)                                              
         B     TRAPERRV                                                         
HIT      DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
TRAPERRV GOTO1 VTRAERR                                                          
BADLENEV MVI   ERROR,INVCMMLN      COMML ID MUST BE 8 CHAR                      
         GOTO1 ERREX                                                            
         EJECT                                                                  
* GENERATE REPORT OF CMMLS UPDATED OR ADDED TODAY                               
*                                                                               
PUTCML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'F1'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACTVD,R6                                                         
         CLC   BTODAY,ACTVADDT     ADDED TODAY                                  
         BE    PUTCML10                                                         
         CLC   BTODAY,ACTVCHDT     CHANGED TODAY                                
         NOP   PUTCMLX                                                          
*                                                                               
PUTCML10 DS    0H                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BNE   PUTCMLX                                                          
         USING CMLBBEL,R6                                                       
         CLI   CMLBBBAG,C'Y'       BRAND AGENCY (LEO B. OR NOT)                 
         BNE   PUTCMLX                                                          
*                                                                               
         LA    R2,FILTABL                                                       
         USING FILTSECT,R2                                                      
         LA    R5,P1                                                            
         ST    R5,SVNEXT                                                        
*                                                                               
         CLI   PQSW,1              HAS REPORT BEEN OPENED YET?                  
         BNE   *+8                                                              
         BRAS  RE,OPNPQ            GO OPEN PRINT FILE                           
*                                                                               
PUTCML20 DS   0H                                                                
         ZIC   R3,FILTOLN          TO FIELD LEN                                 
         ZIC   R4,FILFRLN          FROM FIELD LEN                               
*                                                                               
         L     R5,SVNEXT           GET NEXT ADDR                                
*                                                                               
         LA    RE,P2               START OF SECOND LINE                         
         LA    RF,P3                                                            
*                                                                               
         CR    RF,R5               IS THIS AFTER START OF P3                    
         BL    PUTCML24             YES, WILL FINNISH IN P3                     
*                                                                               
         LA    R1,4(R3,R5)         ADD 1 + 2 " PLUS 1 BLANK + LEN               
*                                                                               
         CR    RE,R5               IS THIS BEFORE P2                            
         BL    PUTCML22             NO, CK IF RUNS OVER P3                      
*                                                                               
         CR    R1,RE               IS END BEFORE  P2                            
         BL    PUTCML24                                                         
*                                                                               
         LR    R5,RE               START AT P2                                  
         B     PUTCML24                                                         
*                                                                               
PUTCML22 DS    0H                                                               
         CR    R1,RF               IS END BEFORE P3                             
         BL    PUTCML24             YES                                         
*                                                                               
         LR    R5,RF               START AT P3                                  
*                                                                               
PUTCML24 DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         CLI   FILTEL,0            THIS IN KEY FIELD OR NO FIELD                
         BE    PUTCML26                                                         
*                                                                               
         MVC   ELCODE,FILTEL                                                    
         BRAS  RE,GETEL                                                         
         BE    PUTCML26                                                         
*                                                                               
         SR    R6,R6                                                            
         B     PUTCML28                                                         
*                                                                               
PUTCML26 DS    0H                                                               
         A     R6,FILTFROM                                                      
*                                                                               
PUTCML28 DS    0H                                                               
         ICM   RF,15,FILTRTN                                                    
         BZ    PUTCML30                                                         
         A     RF,SPTR22RR         RELOCATE                                     
*                                                                               
         BASR  RE,RF                                                            
*                                                                               
         B     PUTCML50                                                         
*                                                                               
PUTCML30 DS    0H                                                               
         LTR   R6,R6               NO SOURCE ELEM                               
         BZ    PUTCML40                                                         
*                                                                               
         MVI   0(R5),C'"'                                                       
         LA    R5,1(,R5)                                                        
         EX    R4,PUTCML34                                                      
*                                                                               
         EX    R4,PUTCML36                                                      
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
         LR    RE,R5                                                            
         LR    RF,R3                                                            
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
*                                                                               
* NOW POINT PAST FIELD AND PUT ENDING " THERE                                   
*                                                                               
         LA    R5,0(R3,R5)                                                      
         MVI   0(R5),C'"'                                                       
         LA    R5,2(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCML50                                                         
*                                                                               
PUTCML34 DS    0H                                                               
         MVC   0(0,R5),0(R6)                                                    
PUTCML36 DS    0H                                                               
         OC    0(0,R5),SPACES                                                   
*                                                                               
* PUT IN BLANK FIELD FOR MISSING ELEMENT                                        
*                                                                               
PUTCML40 DS    0H                                                               
         MVI   0(R5),C'"'                                                       
         LA    R5,1(R3,R5)                                                      
         MVI   0(R5),C'"'                                                       
         LA    R5,2(,R5)                                                        
         ST    R5,SVNEXT                                                        
*                                                                               
PUTCML50 DS    0H                                                               
         LA    R2,FILTNXT                                                       
*                                                                               
         CLI   FILTFROM,X'FF'      END OF TABLE                                 
         BNE   PUTCML20             NO, GET NEXT FIELD                          
*                                                                               
         L     R5,SVNEXT                                                        
         MVI   1(R5),X'5E'         END OF RECORD CHARACTER                      
*                                                                               
         LA    R0,396                                                           
         LA    R1,P1                                                            
         CLI   0(R1),X'4A'         THIS A CENT SIGN                             
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(,R1)                                                        
         BCT   R0,*-16                                                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1                                                           
         AP    CMLFILCT,=P'1'                                                   
*                                                                               
* NOW SEE IF P/B COMML                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         CLI   CMLSOLO,C'P'        THIS A P/B                                   
         BNE   PUTCMLX              NO BYPASS                                   
*                                                                               
         MVI   ELCODE,X'60'        ANY ACTUAL COMMLS                            
         BRAS  RE,NEXTEL                                                        
         BNE   PUTCMLX              NO BYPASS                                   
         LA    R0,116               116*34 = 3944                               
         SR    R1,R1                                                            
         L     R6,AIO                                                           
         L     R5,AIO3                                                          
PUTCML60 DS    0H                                                               
         OC    0(16,R5),0(R5)      EMPTY SLOT                                   
         BZ    PUTCML62                                                         
         LA    R1,1(,R1)                                                        
         LA    R5,34(,R5)                                                       
         BCT   R0,PUTCML60                                                      
PUTCML62 DS    0H                                                               
         CVD   R1,DUB                                                           
         CP    DUB,CMLPBCT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PUTCML64 DS    0H                                                               
         CLC   KEY+3(10),2(R6)     IF EQUAL, BYPASS                             
         BE    PUTCML66                                                         
*                                                                               
         MVC   0(10,R5),KEY+3      MOVE BCLT & COMML CODE                       
         MVC   18(8,R5),2(R6)      MOVE IN FIRST ACT COMML                      
         AP    CMLPBCT,=P'1'                                                    
         LA    R5,34(,R5)                                                       
PUTCML66 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    PUTCML64                                                         
*        L     R6,AIO                                                           
*        MVI   ELCODE,X'90'                                                     
*        BRAS  RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        USING CMLBBEL,R6                                                       
*        OC    CMLBBBCP,CMLBBBCP                                                
*        BZ    PUTCML68                                                         
*        MVC   18(16,R5),CMLBBBCP                                               
*        DROP  R6                                                               
*UTCML68 DS    0H                                                               
PUTCMLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
* PUT IN EITHER CLT CML OR CML CODE                                             
*                                                                               
PCMCML   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
*                                                                               
* IS THERE A CLT CMML                                                           
*                                                                               
         OC    CMLCLTNO-CMLKEY(,R6),CMLCLTNO-CMLKEY(R6)                         
         BZ    PCMCML10                                                         
*                                                                               
         MVC   1(L'FCML,R5),CMLCLTNO-CMLKEY(R6)                                 
         B     PCMCML20                                                         
*                                                                               
PCMCML10 MVC   1(8,R5),CMLKCML-CMLKEY(R6)                                       
*                                                                               
PCMCML20 DS   0H                                                                
         OC    1(L'FCML,R5),SPACES                                              
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
         LA    RE,1(,R5)                                                        
         LA    RF,L'FCML                                                        
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
*                                                                               
         MVI   1+L'FCML(R5),C'"'                                                
         LA    R5,3+L'FCML(,R5)                                                 
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
* MOVE IN CLIENT CODE                                                           
*                                                                               
PCMCLT   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
         MVC   1(2,R5),SVCLTINT                                                 
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
         CLI   1(R5),C'"'                                                       
         BNE   *+8                                                              
         MVI   1(R5),C' '                                                       
         CLI   2(R5),C'"'                                                       
         BNE   *+8                                                              
         MVI   2(R5),C' '                                                       
*                                                                               
         MVI   3(R5),C'"'                                                       
         LA    R5,5(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
* MOVE IN COMMERCIAL DESCRIPTION                                                
*                                                                               
PCMDSC   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
         USING CMLDTAEL,R6                                                      
         MVC   1(L'CMLTITLE,R5),0(R6)                                           
         DROP  R6                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'        OPTIONAL LONGER DESC                         
         BRAS  RE,GETEL                                                         
         BNE   PCMDSC20                                                         
         MVC   1(24,R5),3(R6)                                                   
PCMDSC20 DS   0H                                                                
         OC    1(24,R5),SPACES                                                  
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
         LA    RE,1(,R5)                                                        
         LR    RF,R3                                                            
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
*                                                                               
         LA    R5,1(R3,R5)                                                      
         MVI   0(R5),C'"'                                                       
         LA    R5,2(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
* CHANGE LENGTH FROM BINARY TO ZD                                               
*                                                                               
PCMLEN   NTR1                                                                   
         ZIC   R0,0(R6)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R5),C'"'                                                       
         UNPK  1(5,R5),DUB                                                      
         MVI   6(R5),C'"'                                                       
         LA    R5,8(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
PCMPRD   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
         MVC   SVKEY,KEY                                                        
*                                                                               
         TM    SECFLAG,NEMORPRD    IS THIS MORE PRODUCTS USER                   
         BO    PCMPRD16             YES                                         
*        NEVER TESTED - I ADDED THESE INSTRUCTIONS 11/30/07                     
         TM    SECFLAG,NECONPRD    IS THIS MORE PRODUCTS USER                   
         BO    PCMPRD16             YES                                         
*                                                                               
*                                                                               
         LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
PCMPRD10 DS    0H                                                               
         CLC   2(1,R6),3(R1)                                                    
         BE    PCMPRD20                                                         
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,PCMPRD10                                                      
         DC    H'0'                                                             
PCMPRD16 DS    0H                                                               
         LR    R1,R6                                                            
*                                                                               
PCMPRD20 DS    0H                                                               
         MVC   QPRD,0(R1)          SAVE CODE FOR NAME LOOK-UP                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY+2    MOVE IN A/M, BCLT                            
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   1(3,R5),PACCT                                                    
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
         LA    RE,1(,R5)                                                        
         LR    RF,R3                                                            
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
*                                                                               
         MVI   4(R5),C'"'                                                       
         LA    R5,6(,R5)                                                        
         ST    R5,SVNEXT                                                        
         MVC   KEY,SVKEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PUTCMLX                                                          
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
* GET PRODUCT NAME                                                              
*                                                                               
PCMPNM   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
*                                                                               
         L     R6,AIO2                                                          
         USING PRDHDRD,R6                                                       
         MVC   1(L'FPRODSC,R5),PNAME                                            
         OC    1(L'FPRODSC,R5),SPACES                                           
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
         LA    RE,1(,R5)                                                        
         LA    RF,L'FPRODSC                                                     
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
         MVI   1+L'FPRODSC(R5),C'"'                                             
         LA    R5,2+1+L'FPRODSC(R5)                                             
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
         DROP  R6                                                               
*                                                                               
* MOVE IN MEDIA N FOR ALL                                                       
*                                                                               
PCMMED   NTR1                                                                   
         MVC   0(3,R5),=C'"T"'                                                  
         LA    R5,4(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
* CONVERT DATES FROM BINARY - PUT OUT AS MM/DD/YYYY                             
*                                                                               
PCMDTE   NTR1                                                                   
         LTR   R6,R6               IF ZERO, NO ELEM, NO DATA                    
         BZ    PCMDTE10            NO, DO DEFAULT                               
*                                                                               
         OC    0(3,R6),0(R6)      IS THERE A DATE PRESENT?                      
         BZ    PCMDTE10            NO, DO DEFAULT                               
*                                                                               
         CLC   =X'FFFFFF',0(R6)    THIS UFN                                     
         BE    PCMDTE10             YES, DO DEFAULT                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R6)),(23,WORK)                                   
*                                                                               
* CONVERTS BINARY TO YYYY-MM-DA                                                 
*                                                                               
         MVI   0(R5),C'"'                                                       
         MVC   1(2,R5),WORK+5                                                   
         MVI   3(R5),C'/'                                                       
         MVC   4(2,R5),WORK+8                                                   
         MVI   6(R5),C'/'                                                       
         MVC   7(4,R5),WORK                                                     
         MVI   11(R5),C'"'                                                      
         LA    R5,13(,R5)                                                       
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
PCMDTE10 DS   0H                                                                
         MVC   0(12,R5),=C'"00/00/0000"'                                        
         LA    R5,13(,R5)                                                       
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
* GET VARIABLE LENGTH INACTIVE REASON                                           
*                                                                               
PCMIRC   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
*                                                                               
         LTR   R6,R6               NO ELEM FOUND?                               
         BZ    PCMIRC10                                                         
         LR    RE,R6               GET FROM ADDR                                
         BCTR  RE,0                BACK UP TO ELEM LEN                          
         ZIC   RF,0(RE)                                                         
         AHI   RF,-3     SUBTRACT 2 + 1 FOR MOVE                                
*                                                                               
         CHI   RF,L'FINACTRS       IF REASON CODE TOO LONG                      
         BL    *+8                                                              
         LA    RF,L'FINACTRS-1                                                  
         EX    RF,PCMIRCM                                                       
*                                                                               
*                                                                               
* ELIMINATE ANY "                                                               
*                                                                               
PCMIRC10 DS    0H                                                               
         LA    RE,1(,R5)                                                        
         LA    RF,L'FINACTRS                                                    
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
*                                                                               
         LA    RE,1(,R5)                                                        
         LA    RF,L'FPRODSC                                                     
         CLI   0(RE),C'"'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,*-16                                                          
*                                                                               
         LA    R5,1(R3,R5)                                                      
         MVI   0(R5),C'"'                                                       
         LA    R5,2(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
PCMIRCM  MVC   1(0,R5),0(R6)       MOVE REASON                                  
*                                                                               
* GET BRAND AGENCY                                                              
*                                                                               
PCMBAG   NTR1                                                                   
         MVI   0(R5),C'"'                                                       
*                                                                               
         LTR   R6,R6               NO ELEM FOUND?                               
         BZ    PCMBAG10                                                         
*                                                                               
         CLI   0(R6),C'Y'          IS BRAND AGENCY = Y                          
         BNE   PCMBAG10             NO, SEND NOTHING                            
         MVC   1(3,R5),=C'   '                                                  
*        MVC   1(3,R5),=C'YES'                                                  
*                                                                               
PCMBAG10 DS    0H                                                               
         LA    R5,1(R3,R5)                                                      
         MVI   0(R5),C'"'                                                       
         LA    R5,2(,R5)                                                        
         ST    R5,SVNEXT                                                        
         B     PUTCMLX                                                          
*                                                                               
* ENTRIES ARE:                                                                  
*         4    FROM LABEL - DISP INTO INPUT ELEM                                
*         4    TO LABEL                                                         
*         4    OPTIONAL RTN ADDRESS                                             
*         1    FROM ELEMENT - ZERO = USE KEY DISPLACEMENT                       
*         1    FROM LENGTH                                                      
*         1    TO FIELD LENGTH                                                  
*         1    SPARE                                                            
*                                                                               
FILTABL  DS    0D                                                               
         DC    A(0),A(FCML-CMLFILE),A(PCMCML),X'00'                             
         DC    AL1(7),AL1(L'FCML),X'00'                                         
*                                                                               
         DC    A(0),A(FCLTCD-CMLFILE),A(PCMCLT),X'00'                           
         DC    AL1(L'FCLTCD-1),AL1(L'FCLTCD),X'00'                              
*                                                                               
         DC    A(CMLTITLE-CMLDTAEL),A(FCMLTITL-CMLFILE),A(PCMDSC),X'10'         
         DC    AL1(14),AL1(L'FCMLTITL),X'00'                                    
*                                                                               
* COMMERCIAL LENGTH                                                             
*                                                                               
         DC    A(CMLSLN-CMLDTAEL),A(FCMLEN-CMLFILE),A(PCMLEN),X'10'             
         DC    AL1(4),AL1(L'FCMLEN),X'00'                                       
*                                                                               
* PRODUCTION JOB #                                                              
*                                                                               
         DC    A(CMLBBJOB-CMLBBEL),A(FPRODNO-CMLFILE),A(0),X'90'                
         DC    AL1(L'FPRODNO-1),AL1(L'FPRODNO),X'00'                            
*                                                                               
* PRODUCT CODE                                                                  
*                                                                               
         DC    A(0),A(FPRODCD-CMLFILE),A(PCMPRD),X'20'                          
         DC    AL1(2),AL1(3),X'00'                                              
*                                                                               
* PRODUCT DESC                                                                  
*                                                                               
         DC    A(0),A(FPRODSC-CMLFILE),A(PCMPNM),X'00'                          
         DC    AL1(L'FPRODSC-1),AL1(L'FPRODSC),X'00'                            
*                                                                               
* BASIC ISCI - (SAG/AFTRA)                                                      
*                                                                               
         DC    A(CMLBBBCP-CMLBBEL),A(FSAGISCI-CMLFILE),A(0),X'90'               
         DC    AL1(L'FSAGISCI-1),AL1(L'FSAGISCI),X'00'                          
*                                                                               
* AFM ISCI - (AFM)                                                              
*                                                                               
         DC    A(CMLBBBCM-CMLBBEL),A(FAFMISCI-CMLFILE),A(0),X'90'               
         DC    AL1(L'FAFMISCI-1),AL1(L'FAFMISCI),X'00'                          
*                                                                               
* MEDIA                                                                         
*                                                                               
         DC    A(0),A(FCMLMED-CMLFILE),A(PCMMED),X'00'                          
         DC    AL1(0),AL1(L'FCMLMED),X'00'                                      
*                                                                               
* COMMERCIAL TYPE                                                               
*                                                                               
         DC    A(CMLTYPE-CMLDTAEL),A(FPRODFMT-CMLFILE),A(0),X'10'               
         DC    AL1(L'FPRODFMT-1),AL1(L'FPRODFMT),X'00'                          
*                                                                               
* FIRST FILM  DATE (RELEASE DATE)                                               
*                                                                               
         DC    A(CMLBBPDT-CMLBBEL),A(FACTDATE-CMLFILE),A(PCMDTE),X'90'          
         DC    AL1(0),AL1(L'FACTDATE),X'00'                                     
*                                                                               
* CLIENT APPROVAL DATE                                                          
*                                                                               
         DC    A(CMLBBCAD-CMLBBEL),A(FAPRVDTE-CMLFILE),A(PCMDTE),X'90'          
         DC    AL1(0),AL1(L'FAPRVDTE),X'00'                                     
*                                                                               
* MAX USE DATE                                                                  
*                                                                               
         DC    A(CMLBBMXD-CMLBBEL),A(FMPUDATE-CMLFILE),A(PCMDTE),X'90'          
         DC    AL1(0),AL1(L'FMPUDATE),X'00'                                     
*                                                                               
* RELEASE DATE = ACTIVE FILM DATE                                               
*                                                                               
         DC    A(CMLRLSE-CMLDTAEL),A(FACTDATE-CMLFILE),A(PCMDTE),X'10'          
         DC    AL1(0),AL1(L'FACTDATE),X'00'                                     
*                                                                               
* RECALL DATE = INACTIVE FILM DATE                                              
*                                                                               
         DC    A(CMLRCL-CMLDTAEL),A(FINACTDT-CMLFILE),A(PCMDTE),X'10'           
         DC    AL1(0),AL1(L'FINACTDT),X'00'                                     
*                                                                               
* INACTIVE REASON CODE                                                          
*                                                                               
         DC    A(CMLINRSN-CMLINAEL),A(FINACTRS-CMLFILE),A(PCMIRC),X'92'         
         DC    AL1(0),AL1(L'FINACTRS),X'00'                                     
*                                                                               
* BRAND AGENCY CODE                                                             
*                                                                               
         DC    A(CMLBBBAG-CMLBBEL),A(FBRDAGY-CMLFILE),A(PCMBAG),X'90'           
         DC    AL1(L'CMLBBBAG-1),AL1(L'FBRDAGY),X'00'                           
         DC    X'FF'                                                            
         DROP  R2                                                               
         EJECT                                                                  
* OPEN PRINT QUEUE FOR FILE DOWN LOAD                                           
*                                                                               
OPNPQ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVI   QLCLASS,C'Z'                                                     
*NOP     MVI   QLRETNL+1,36                                                     
*****    MVI   QLRETND+1,12                                                     
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         MVI   REQRTYP,REQTDOWN    SET TYPE AS DOWNLOAD                         
*                                                                               
         MVC   REMUSER,=C'FTL'                                                  
*                                                                               
         MVC   CONOUT(4),=C'DOWN'                                               
         MVI   CONOUTH+5,4                                                      
         OI    CONOUTH+6,X'80'                                                  
*                                                                               
         MVC   QLDESC(1),QMED                                                   
         MVC   QLDESC+2(3),QCLT                                                 
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
         TM    WHEN,X'40'          IF WHEN = NOW PRINT FAKE REQUEST             
         BZ    OPNPQX                                                           
*                                                                               
         MVC   P+5(30),=30C'*'                                                  
         MVI   P2+5,C'*'                                                        
         MVC   P2+10(20),=C'PRINT TO FORCE PIANO'                               
         MVC   P3+5(30),=30C'*'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*        GOTO1 REQTWA,DMCB,(3,ATWA),0,VPRINT,(C'B',AIO3)                        
*                                                                               
OPNPQX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* PUT OUT LAST REPORT(S)                                                        
*                                                                               
PUTREP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CLOSE  PRINT FILE REPORT                                                      
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE, FORCE SEPARATE ENTRY                
         BNE   PUTREP10             NO, NOT NEEDED                              
*                                                                               
         ICM   R4,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FORCE CHANGE TO REMOTKEY, WHICH WILL FORCE NEW QUEUE ENTRY *                  
*                                                                               
         L     R6,MCVREMOT-MASTD(,R4)                                           
         USING REMOTED,R6                                                       
         MVI   REMOTSUB,C'A'                                                    
         DROP  R6                                                               
PUTREP10 DS    0H                                                               
         MVI   PQSW,1                                                           
         BRAS  RE,OPNPQ            GO OPEN PRINT FILE                           
*                                                                               
         TM    WHEN,X'40'          IF WHEN NOT NOW PRINT FAKE REQUEST           
         BO    PUTREP20             NOW DONE IN OPNPQ RTN                       
*                                                                               
         MVC   P+5(30),=30C'*'                                                  
         MVI   P2+5,C'*'                                                        
         MVC   P2+10(20),=C'PRINT TO FORCE PIANO'                               
         MVC   P3+5(30),=30C'*'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
PUTREP20 DS    0H                                                               
         MVI   P,C'"'                                                           
         GOTO1 DATCON,DMCB,(5,0),(X'20',P+1)                                    
         MVI   P+7,C'"'                                                         
*                                                                               
         MVI   P+9,C'"'                                                         
         MVI   P+10,C'|'                                                        
         MVI   P+11,C'"'                                                        
*                                                                               
         MVI   P+13,C'"'                                                        
         MVC   P+14(8),=C'COMMERCI'                                             
         MVI   P+22,C'"'                                                        
*                                                                               
         MVI   P+24,C'"'                                                        
         MVI   P+25,C'|'                                                        
         MVI   P+26,C'"'                                                        
*                                                                               
         MVI   P+28,C'"'                                                        
         OI    CMLFILCT+2,X'0F'                                                 
         UNPK  P+29(6),CMLFILCT                                                 
         MVI   P+35,C'"'                                                        
         MVI   P+38,X'5E'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* CLOSE  CML TOTALS FILE REPORT                                                 
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE, FORCE SEPARATE ENTRY                
         BNE   PUTREP30             NO, NOT NEEDED                              
*                                                                               
         ICM   R4,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* OPEN DIRECT ENTRY (WHICH WILL BE LOST)  TO PRESERVE REAL QUE ENTRY *          
*                                                                               
         L     R6,MCVREMOT-MASTD(,R4)                                           
         USING REMOTED,R6                                                       
*                                                                               
         MVI   REMOTSUB,C'B'                                                    
         DROP  R6                                                               
PUTREP30 DS    0H                                                               
         MVI   PQSW,1                                                           
*                                                                               
         CP    CMLPBCT,=P'0'       ANY P/B TO REPORT                            
         BE    PUTREP60             NO, BUT STILL PUT OUT TOTALS                
*                                                                               
         BRAS  RE,OPNPQ            GO OPEN PRINT FILE                           
*                                                                               
         TM    WHEN,X'40'          IF WHEN NOT NOW PRINT FAKE REQUEST           
         BO    PUTREP40             NOW IS DONE IN OPNPQ RTN                    
*                                                                               
         MVC   P+5(30),=30C'*'                                                  
         MVI   P2+5,C'*'                                                        
         MVC   P2+10(20),=C'PRINT TO FORCE PIANO'                               
         MVC   P3+5(30),=30C'*'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
PUTREP40 DS    0H                                                               
         ZAP   DUB,CMLPBCT                                                      
         CVB   R0,DUB                                                           
         L     R4,AIO3                                                          
PUTREP50 DS    0H                                                               
         MVI   P,C'"'                                                           
         MVC   P+1(16),2(R4)                                                    
         MVI   P+17,C'"'                                                        
         MVI   P+19,C'"'                                                        
         MVC   P+20(16),18(R4)                                                  
         MVI   P+36,C'"'                                                        
* BUILD KEY FOR COMML REC                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(1),BAGYMD    A/M                                          
         MVC   CMLKCLT(2),0(R4)    BCLT                                         
         MVC   CMLKCML,2(R4)                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CMLCLTNO-CMLDTAEL(,R6),CMLCLTNO-CMLDTAEL(R6)                     
         BZ    *+10                                                             
         MVC   P+1(16),CMLCLTNO-CMLDTAEL(R6)                                    
*                                                                               
* NOW DO SAME TO ACT COMML                                                      
*                                                                               
         LA    R6,KEY                                                           
         MVC   CMLKCML,18(R4)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         L     R6,AIO1                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CMLCLTNO-CMLDTAEL(,R6),CMLCLTNO-CMLDTAEL(R6)                     
         BZ    *+10                                                             
         MVC   P+20(16),CMLCLTNO-CMLDTAEL(R6)                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,34(,R4)                                                       
         BCT   R0,PUTREP50                                                      
*                                                                               
* CLOSE  CML TOTALS FILE REPORT                                                 
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE, FORCE SEPARATE ENTRY                
         BNE   PUTREP60             NO, NOT NEEDED                              
*                                                                               
         ICM   R4,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* OPEN DIRECT ENTRY (WHICH WILL BE LOST)  TO PRESERVE REAL QUE ENTRY *          
*                                                                               
         L     R6,MCVREMOT-MASTD(,R4)                                           
         USING REMOTED,R6                                                       
         MVI   REMOTSUB,C'C'                                                    
         DROP  R6                                                               
*                                                                               
PUTREP60 DS    0H                                                               
         MVI   PQSW,1                                                           
         BRAS  RE,OPNPQ            GO OPEN PRINT FILE                           
*                                                                               
         TM    WHEN,X'40'          IF WHEN NOT NOW PRINT FAKE REQUEST           
         BO    PUTREP70             NOW DONE IN OPNPQ RTN                       
*                                                                               
         MVC   P+5(30),=30C'*'                                                  
         MVI   P2+5,C'*'                                                        
         MVC   P2+10(20),=C'PRINT TO FORCE PIANO'                               
         MVC   P3+5(30),=30C'*'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PUTREP70 DS    0H                                                               
         MVI   P,C'"'                                                           
         GOTO1 DATCON,DMCB,(5,0),(X'20',P+1)                                    
         MVI   P+7,C'"'                                                         
*                                                                               
         MVI   P+9,C'"'                                                         
         MVI   P+10,C'|'                                                        
         MVI   P+11,C'"'                                                        
*                                                                               
         MVI   P+13,C'"'                                                        
         MVC   P+14(8),=C'PIGGY   '                                             
         MVI   P+22,C'"'                                                        
*                                                                               
         MVI   P+24,C'"'                                                        
         MVI   P+25,C'|'                                                        
         MVI   P+26,C'"'                                                        
*                                                                               
         MVI   P+28,C'"'                                                        
         OI    CMLPBCT+2,X'0F'                                                  
         UNPK  P+29(6),CMLPBCT                                                  
         MVI   P+35,C'"'                                                        
         MVI   P+38,X'5E'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PUTREPX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* GENERATE PATTERN LIST T/A REQUEST                                             
*                                                                               
PATREQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BYTE,1              GENERATE PATTERN T/A REQUEST                 
         BNE   PATREQX              NO                                          
*                                                                               
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),AGENCY                                              
         MVC   REQUEST+4(23),=CL23'*.PAT.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(3),QCLT                                               
         MVC   REQUEST+30(8),=C'........'                                       
         MVC   REQUEST+38(5),=C'ERROR'                                          
         MVC   REQUEST+43(2),=C'.*'                                             
*TEMP    CLI   SVPROF10,C'D'       IF REQ IS DDS                                
*****    BE    *+10                BYPASS                                       
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE,0              RESET GENERATE PATTERN T/A REQUEST           
PATREQX  XIT1                                                                   
*                                                                               
*                                                                               
         LTORG                                                                  
REQHDR   DS    CL26                REQUEST HDR FOR TURNAROUND REPORT            
REQUEST  DS    CL80                                                             
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PFKEY,5             PF5 - CHANGE HISTORY                         
         BE    DRC                                                              
                                                                                
         MVI   SVNXTCHG,0                                                       
         CLI   PFKEY,2             PF2 - LCOM                                   
*&&DO*&& BE    DRL                                                              
         CLI   PFKEY,3             PF3 - BCOM                                   
*&&DO*&& BE    DRB                                                              
         CLI   PFKEY,4             PF4 - COM                                    
         BE    DR03                                                             
*                                                                               
         ZIC   RE,CONRECH+5        LENGTH OF INPUT (RECORD TYPE)                
         BCTR  RE,0                                                             
*                                                                               
         EX    RE,CLCLEGAL         IS IT LCOM                                   
*&&DO*&& BE    DRL                                                              
         EX    RE,CLCBROAD         IS IT BROADCAST BUSINESS                     
*&&DO*&& BE    DRB                                                              
*                                                                               
DR03     CLI   TWASCR,X'F4'        IS COMML SCREEN LOADED ALREADY               
         BE    DR07                 YES                                         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,CONTAGH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'F4'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TWASCR,X'F4'                                                     
         MVC   CONREC(6),=C'COMML '                                             
         MVC   CONACT(7),=C'DISPLAY'                                            
*                                                                               
         LA    R2,CONHEADH                                                      
*                                                                               
DR05     OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP                                         
*                                                                               
         CLI   0(R2),0                                                          
         BNE   DR05                                                             
*                                                                               
         MVC   1(2,R2),=X'0101'    POP IN INDICATORS                            
*                                                                               
* RE-DISPLAY MEDIA/CLIENT/COMMERCIAL                                            
*                                                                               
         MVC   TRAMED(L'QMED),QMED                                              
         OI    TRAMEDH+6,X'80'                                                  
*                                                                               
         MVC   TRACLT(L'QCLT),QCLT                                              
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
         MVC   TRACML(L'HOLDCML),HOLDCML                                        
         OI    TRACMLH+6,X'80'                                                  
         L     R6,AIO                                                           
         TM    15(R6),CMLKSTA_PCKD                                              
         BZ    DR07                                                             
         GOTO1 VTRPACK,DMCB,(C'U',HOLDCML),TRACML                               
                                                                                
* HIDE PFKEY OPTIONS (IT'S FOR STARCOM ONLY)                                    
                                                                                
DR07     DS    0H                                                               
         BRAS  RE,SETPFK                                                        
                                                                                
* CLEAR VALID TYPE HELP FROM SCREEN  (R0 & R2 NOT PRESERVED)                    
*                                                                               
DR08     LA    R2,TRATYPLH                                                      
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         ZIC   R0,0(R2)            TO NEXT HELP LINE                            
         AR    R2,R0                                                            
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
*                                                                               
         LA    R2,TRAPLSTH         CLEAR PROD LINES                             
         XC    8(L'TRAPLST,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         ZIC   R0,0(R2)            TO NEXT PROD LINE                            
         AR    R2,R0                                                            
         XC    8(L'TRAPLST,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
*                                                                               
         L     R6,AIO                                                           
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    DR09X               YES, ONLY DISPL TITLE (PROD HOUSE)           
*                                                                               
         BRAS  RE,PPRD             CHECK FOR SECURITY LOCKOUT                   
         CLI   ERROR,SECLOCK                                                    
         BE    TRAPER0                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(256),BLOCK                                                  
*                                                                               
         CLC   TRAPLST,ELEM                                                     
         BE    *+14                                                             
         MVC   TRAPLST,ELEM                                                     
         OI    TRAPLSTH+6,X'80'                                                 
*                                                                               
         LA    R2,TRAPLST          PROD LIST LINE 1                             
         CLI   3(R2),C' '          JUST 1 PROD                                  
         BE    DR09X                YES                                         
         CLI   3(R2),C'='          PRD=ALL                                      
         BE    DR09X                                                            
*                                                                               
         LA    RF,ELEM             POINT TO THE LIST OF PRODUCTS                
         LA    RF,L'TRAPLST(RF)    END OF PROD LINE 1                           
         CLI   0(RF),C' '                                                       
         BNH   DR09X               DONE ALL PRODS                               
         CLI   0(RF),C','                                                       
         BNE   *+12                                                             
         LA    RF,1(RF)            BUMP TO NEXT PROD                            
         B     DR09F                                                            
*                                                                               
         LA    R1,L'TRAPLST-1(R2)  END OF PROD LINE 1                           
         LA    RF,4                LOOK AT LAST 3 CHARS OF THE LINE             
DR09     CLI   0(R1),C','                                                       
         BE    DR09C                                                            
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                BUMP BACK 1 ON PROD LIST LINE                
         BCT   RF,DR09                                                          
         B     DR09X               DONE, ALL PRODS                              
*                                                                               
DR09C    MVI   0(R1),C' '          BLANK OUT COMMA                              
         SR    R1,R2                                                            
         LA    RF,ELEM                                                          
         AR    RF,R1               POINT TO NEXT PROD TO DISPLAY                
         CLI   0(RF),C','                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
DR09F    LA    R2,TRAPLSTH                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NEXT PROD LINE                               
         MVC   8(L'TRAPLST,R2),0(RF)                                            
         OI    6(R2),X'80'         TRANSMIT PROD LINE 2                         
*                                                                               
DR09X    DS    0H                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
         USING CMLDTAEL,R6                                                      
*                                                                               
         CLC   TRADSC1(15),CMLTITLE    TITLE                                    
         BNE   DR10                                                             
         OC    TRADSC1+15(L'TRADSC1-15),TRADSC1+15                              
         BZ    DR14                                                             
*                                                                               
DR10     MVC   TRADSC1(15),CMLTITLE                                             
         XC    TRADSC1+15(L'TRADSC1-15),TRADSC1+15                              
         OI    TRADSC1H+6,X'80'                                                 
*                                                                               
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BNE   DR14                                                             
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE REST OF THE SCREEN                 
         B     DRXIT                                                            
*                                                                               
DR14     XC    TRADEL,TRADEL                                                    
         OI    TRADELH+6,X'80'                                                  
         TM    CMLSTAT,X'80'       IS RECORD SOFT DELETED                       
         BZ    DR16                NO                                           
         MVC   TRADEL,=C'*DELETED*'                                             
*                                                                               
DR16     DS    0H                                                               
         TM    FTRFLAG,SEQFTR      SHOW COMML SEQ NO                            
         BZ    DR20                                                             
*                                                                               
         EDIT  CMLSEQ,(6,TRADEL+3)                                              
*                                                                               
DR20     XC    PARAS(24),PARAS                                                  
         CLI   CMLSLN,X'FF'        ALL SECONDS LENGTH                           
         BNE   DR30                                                             
         MVC   PARAS(3),=C'ALL'                                                 
         B     DR34                                                             
DR30     EDIT  (1,CMLSLN),(3,PARAS),ZERO=BLANK,ALIGN=LEFT                       
*                                                                               
DR34     CLC   TRASLN,PARAS       COMMERCIAL LENGTH                             
         BE    *+14                                                             
         MVC   TRASLN,PARAS                                                     
         OI    TRASLNH+6,X'80'                                                  
*                                                                               
         XC    TRAOVRD,TRAOVRD                                                  
         OI    TRAOVRDH+6,X'80'                                                 
         OC    CMLOVRD1(2),CMLOVRD1 ANY PRINT OVERRIDE?                         
         BZ    DR36                                                             
         EDIT  (1,CMLOVRD1),(3,TRAOVRD),ALIGN=LEFT,ZERO=NOBLANK                 
         LA    R1,TRAOVRD                                                       
         AR    R1,R0               NEXT BLANK SPACE                             
         MVI   0(R1),C'/'                                                       
         EDIT  (1,CMLOVRD2),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
DR36     XC    WORK(L'TRARLSE),WORK                                             
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,WORK)                                 
         CLC   TRARLSE,WORK        RELEASE DATE                                 
         BE    *+14                                                             
         MVC   TRARLSE,WORK                                                     
         OI    TRARLSEH+6,X'80'                                                 
*                                                                               
         MVC   WORK(L'TRARCL),WORK                                              
         CLC   CMLRCL,=XL3'FFFFFF'                                              
         BNE   DR50                                                             
         MVC   WORK(3),=CL3'UFN'                                                
         XC    WORK+3(5),WORK+3    CLEAR PREV DATE                              
         B     DR52                                                             
*                                                                               
DR50     GOTO1 (RF),(R1),(3,CMLRCL),(5,WORK)                                    
*                                                                               
DR52     CLC   TRARCL,WORK         RECALL DATE                                  
         BE    *+14                                                             
         MVC   TRARCL,WORK                                                      
         OI    TRARCLH+6,X'80'                                                  
*                                                                               
         XC    WORK(L'TRATYPE),WORK                                             
         MVC   WORK(L'CMLTYPE),CMLTYPE                                          
         CLC   TRATYPE,WORK        COMMERCIAL TYPE                              
         BE    *+14                                                             
         MVC   TRATYPE,WORK                                                     
         OI    TRATYPEH+6,X'80'                                                 
*                                                                               
         XC    TRACTXT,TRACTXT                                                  
         TM    CMLSTAT,X'40'       COMML TEXT REC FOR THIS COMML                
         BZ    *+10                                                             
         MVC   TRACTXT(15),=C'COMTEXT PRESENT'                                  
         XC    TRACTXT+1(14),SPACES    LOWERCASE                                
         OI    TRACTXTH+6,X'80'                                                 
*                                                                               
         CLC   TRACLTN,CMLCLTNO    CLIENT COMMERCIAL NUMBER                     
         BE    *+14                                                             
         MVC   TRACLTN,CMLCLTNO                                                 
         OI    TRACLTNH+6,X'80'                                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),CMLSOLO                                                  
         CLC   TRASOLO,WORK                                                     
         BE    *+14                                                             
         MVC   TRASOLO,WORK                                                     
         OI    TRASOLOH+6,X'80'                                                 
*                                                                               
         MVC   WORK(1),CMLTALEX                                                 
         CLC   TRATTEX,WORK                                                     
         BE    *+14                                                             
         MVC   TRATTEX(L'CMLTALEX),CMLTALEX                                     
         OI    TRATTEXH+6,X'80'                                                 
*                                                                               
         MVC   WORK(4),CMLCLASS                                                 
         CLC   TRACLS,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLS,WORK                                                      
         OI    TRACLSH+6,X'80'                                                  
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   TRADSC1,3(R6)                                                    
         OI    TRADSC1H+6,X'80'                                                 
*                                                                               
         XC    TRADSC2,TRADSC2                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   TRADSC2,3(R6)                                                    
         OI    TRADSC2H+6,X'80'                                                 
*                                                                               
         XC    TRADSC3,TRADSC3                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   TRADSC3,3(R6)                                                    
         OI    TRADSC3H+6,X'80'                                                 
*                                                                               
* DISPLAY AD-ID NO                                                              
*                                                                               
         XC    TRAADID,TRAADID                                                  
         OI    TRAADIDH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                                                           
         TM    15(RE),CMLKSTA_PCKD IF CMML NOT PACKED                           
         BZ    DR54                DISPLAY ADID                                 
*                                                                               
         L     R6,AIO                                                           
         GOTO1 VTRPACK,DMCB,(C'U',5(R6)),WORK                                   
         CLI   WORK+8,C' '         IF CMML IS AN ADID                           
         BH    DR58                DON'T DISPLAY ADID                           
*                                                                               
DR54     L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR58                                                             
*                                                                               
         USING CMLADIEL,R6                                                      
         CLI   CMLADID+8,C' '      DO NOT DISPLAY IF NOT A REAL ADID            
         BNH   DR58                *** TEMPORARY UNTIL FIX ***                  
         MVC   TRAADID(L'CMLADID),CMLADID                                       
         OI    TRAADIDH+6,X'80'          FORCE TRANSMIT                         
         B     DR58                                                             
*                                                                               
*  DISPLAY ACTUAL CMMLS                                                         
*                                                                               
DR58     DS    0H                                                               
         LA    R2,TRAACT1H                                                      
         LA    R0,NUMACTS                                                       
         SR    RF,RF                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR60     BRAS  RE,NEXTEL                                                        
         BNE   DR70                                                             
*                                                                               
         XC    8(12,R2),8(R2)      CLEAR OLD VALUE                              
         MVC   8(8,R2),2(R6)                                                    
         CLI   1(R6),10            TEST OLD ELEM                                
         BNH   *+10                                                             
         MVC   8(12,R2),2(R6)                                                   
*                                                                               
         OI    6(R2),X'80'         XMIT                                         
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR60                                                          
         B     DR75                                                             
*                                                                               
DR70     XC    8(12,R2),8(R2)                                                   
         OI    6(R2),X'80'         XMIT                                         
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR70                                                          
*                                                                               
*  DISPLAY CMML NETWORKS                                                        
*                                                                               
DR75     LA    R2,TRANET1H                                                      
         LR    R1,R2                                                            
         LA    R0,MAXNETS                                                       
*                                                                               
DR76     XC    8(L'TRANET1,R2),8(R2)    CLEAR NETWORK SUB-FIELDS                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR76                                                          
*                                                                               
         LR    R2,R1                                                            
         LA    R0,MAXNETS                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR80     BRAS  RE,NEXTEL                                                        
         BNE   DR90                                                             
*                                                                               
         USING CMLNETEL,R6                                                      
*                                                                               
         MVC   8(4,R2),2(R6)                                                    
*                                                                               
         CLI   CMLNETLN,6          OLD RECORD?                                  
         BE    DR82                 YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    DR82                 NO                                          
         LA    RF,11(R2)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'                                                       
*                                                                               
DR82     OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR80                                                          
         B     DR100                                                            
*                                                                               
DR90     XC    8(4,R2),8(R2)                                                    
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR90                                                          
                                                                                
DR100    DS    0H                                                               
         MVI   TRADLY,0                                                         
         OI    TRADLYH+6,X'80'                                                  
         TM    CMLMFLAG,CMLMFDAY                                                
         BZ    *+8                                                              
         MVI   TRADLY,C'Y'                                                      
                                                                                
         XC    TRAPRNT,TRAPRNT                                                  
         OI    TRAPRNTH+6,X'80'                                                 
                                                                                
         XC    TRACNTR,TRACNTR                                                  
         OI    TRACNTRH+6,X'80'                                                 
                                                                                
         XC    TRAHDEF,TRAHDEF                                                  
         OI    TRAHDEFH+6,X'80'                                                 
                                                                                
         XC    TRASWAP,TRASWAP                                                  
         OI    TRASWAPH+6,X'80'                                                 
                                                                                
         XC    TRADSDT,TRADSDT                                                  
         OI    TRADSDTH+6,X'80'                                                 
                                                                                
         XC    TRADSTM,TRADSTM                                                  
         OI    TRADSTMH+6,X'80'                                                 
                                                                                
         XC    SVHIDEF,SVHIDEF                                                  
         XC    SVCNTCT,SVCNTCT                                                  
         XC    SVHIDEFX,SVHIDEFX                                                
         XC    SVCNTCTX,SVCNTCTX                                                
*                                                                               
         MVI   ELCODE,X'24'        SEARCH FOR EXTENDED DATA ELEMENT             
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DR102               FIX BAD BRANCH BELOW                         
         BNE   EXIT1                                                            
*                                                                               
         USING CMLXDTEL,R6                                                      
         MVC   TRAHDEF,CMLXHDEF    HIDEF                                        
         MVC   SVHIDEF,CMLXHDEF    SAVE FOR LATER COMPARISON                    
         MVC   SVHIDEFX,CMLXHDPK   SAVE FOR LATER COMPARISON                    
                                                                                
         MVC   TRACNTR,CMLXCNTR    CENTER CUT                                   
         MVC   SVCNTCT,CMLXCNTR    SAVE FOR LATER COMPARISON                    
         MVC   SVCNTCTX,CMLXCCPK   SAVE FOR LATER COMPARISON                    
                                                                                
         MVC   TRAPRNT,CMLXPRNT    PARENT COMMERCIAL                            
*                                                                               
         MVC   TRASWAP,CMLXSWAP    HIDEF/CENTERCUT SWAP INDICATOR               
*                                                                               
         CLI   CMLXDSDT,0                                                       
         BE    DR102                                                            
         GOTO1 DATCON,DMCB,(3,CMLXDSDT),(8,TRADSDT)                             
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET ADDRESS OF UNTIME                 
         L     RF,0(R1)                                                         
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),CMLXDSTM                                                 
         GOTO1 (RF),DMCB,WORK,TRADSTM                                           
         DROP  R6                                                               
                                                                                
DR102    DS    0H                                                               
*        CLEAR ALL THE MATCH ELEMENT FIELDS FIRST                               
         XC    TRASTIM,TRASTIM                                                  
         OI    TRASTIMH+6,X'80'                                                 
         XC    TRAETIM,TRAETIM                                                  
         OI    TRAETIMH+6,X'80'                                                 
         MVI   TRADLY,C'N'                                                      
         OI    TRADLYH+6,X'80'                                                  
                                                                                
         LHI   R3,6                                                             
         LA    R2,TRADT1H                                                       
DR105    XC    8(L'TRADT1,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         LA    R2,L'TRADT1H+L'TRADT1(R2)                                        
         BCT   R3,DR105                                                         
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,CMLMATEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   DRXIT                                                            
         USING CMLMATCH,R6                                                      
                                                                                
         LHI   R3,6                                                             
         LA    R2,TRADT1H                                                       
         LA    R5,CMLMPER1                                                      
                                                                                
*        MATCH DATE RANGE DISPLAY LOOP                                          
DR120    DS    0H                                                               
         OC    0(L'CMLMPER1,R5),0(R5)  ANYTHING HERE?                           
         BZ    DR140                   NO, GO TO NEXT FIELD                     
                                                                                
         GOTO1 DATCON,DMCB,(X'12',0(R5)),(5,8(R2))                              
                                                                                
DR140    DS    0H                                                               
*M       ZIC   RF,0(R2)                                                         
*M       AR    R2,RF                                                            
         LA    R2,L'TRADT1H+L'TRADT1(R2)                                        
         LA    R5,L'CMLMPER1(R5)                                                
         BCT   R3,DR120                                                         
                                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET ADDRESS OF UNTIME                 
         L     RF,0(R1)                                                         
                                                                                
*        MATCH START TIME                                                       
         OC    CMLMSTIM,CMLMSTIM                                                
         BZ    DR150                                                            
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMSTIM                                                 
         GOTO1 (RF),DMCB,WORK,TRASTIM                                           
         OI    TRASTIMH+6,X'80'                                                 
                                                                                
*        MATCH END TIME                                                         
DR150    DS    0H                                                               
         OC    CMLMETIM,CMLMETIM                                                
         BZ    DR170                                                            
         CLC   CMLMETIM,=X'FFFF'                                                
         BE    DR170                                                            
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMETIM                                                 
         GOTO1 (RF),DMCB,WORK,TRAETIM                                           
         OI    TRAETIMH+6,X'80'                                                 
                                                                                
*        CHECK TIMES DAILY                                                      
DR170    DS    0H                                                               
         TM    CMLMFLAG,CMLMFDAY                                                
         BZ    *+8                                                              
         MVI   TRADLY,C'Y'                                                      
                                                                                
DRXIT    DS    0H                                                               
         B     EXIT1                                                            
*                                                                               
TRAPER0  GOTO1 ERREX                                                            
*                                                                               
CLCLEGAL CLC   CONRECH+8(0),=C'LCOMML'                                          
CLCBROAD CLC   CONRECH+8(0),=C'BCOMML'                                          
* DISPLAY CHANGE HISTORY SCREEN                                                 
                                                                                
DRC      DS    0H                                                               
         BRAS  RE,DSPCHG                                                        
         B     DRX                                                              
* DISPLAY LEGAL SCREEN                                                          
*                                                                               
DRL      DS    0H                                                               
         CLC   =C'SJ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    DRL010                                                           
         CLC   =C'GZ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    DRL010                                                           
         CLC   =C'H9',AGENCY       STARCOM ?                                    
         BNE   DR03                NO, GO RE-DISPLAY DISPLAY SCREEN             
                                                                                
DRL010   DS    0H                                                               
         BRAS  RE,DRLEGAL         DISPLAY LEGAL SCREEN                          
         B     DRX                                                              
         EJECT                                                                  
* DISPLAY BROADCAST BUSINESS SCREEN                                             
*                                                                               
DRB      DS    0H                                                               
         CLC   =C'SJ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    DRB010                                                           
         CLC   =C'GZ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    DRB010                                                           
         CLC   =C'H9',AGENCY       STARCOM ?                                    
         BNE   DR03                NO, GO RE-DISPLAY DISPLAY SCREEN             
                                                                                
DRB010   DS    0H                                                               
         BRAS  RE,DRBROAD          DISPLAY BROADCAST SCREEN                     
         B     DRX                                                              
*                                                                               
EXIT1    OI    TRAPLSTH+1,X'01'     MODIFIED                                    
         OI    TRAPLSTH+6,X'80'     TRANSMIT                                    
DRX      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
*======================================================                         
*  PROD HOUSE RECORD. CLEAR ALL FIELDS EXCEPT TITLE 1                           
*======================================================                         
*                                                                               
CLRFLD   NTR1                                                                   
         XC    TRAPLST,TRAPLST     CLEAR PROD LIST 1                            
         LA    R2,TRAPLSTH                                                      
         OI    6(R2),X'80'         TRANSMIT PROD LINE 1                         
         LLC   R0,0(R2)                                                         
         AR    R2,R0               NEXT PROD LINE                               
         XC    8(L'TRAPLST,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT PROD LINE 2                         
*                                                                               
         LA    R2,TRAADIDH                                                      
         LA    R0,TRATAGH          END OF SCREEN                                
         BRAS  RE,CLRSCR           CLEAR THE REST OF THE SCREEN                 
         B     DRX                                                              
*                                                                               
         EJECT                                                                  
* FIND, EDIT, AND PRINT PRODUCT LIST                                            
*                                                                               
PPRD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERROR,0             INIT ERROR                                   
*                                                                               
         LR    R3,R6                                                            
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    PPRD02                                                           
*                                                                               
         LR    R6,R3                                                            
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PPRD02   DS    0H                                                               
         MVC   BLOCK(256),SPACES                                                
         CLI   2(R6),X'FF'         IS THIS PRD=ALL                              
         BNE   PPRD06               NO                                          
         MVC   BLOCK(7),=CL7'PRD=ALL'                                           
         B     PPRDX                                                            
PPRD06   ZIC   R3,1(R6)            GET PROD LIST ELEM LEN                       
         BCTR  R3,0                                                             
         BCTR  R3,0                NOW # PRODS IN LIST                          
         LA    R4,2(,R6)           POINT TO START PROD LIST                     
         LA    R5,BLOCK            OUTPUT AREA                                  
PPRD10   DS   0H                                                                
         CLI   0(R6),X'29'           IS THIS 3 CHAR PROD LIST                   
         BE    PPRD13I                                                          
         LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
*                                                                               
PPRD12   DS   0H                                                                
         CLI   0(R1),C' '                                                       
         BNH   PPRD13              PROD NOT IN TABLE                            
         CLC   0(1,R4),3(R1)                                                    
         BE    PPRD14                                                           
         LA    R1,4(R1)                                                         
         BCT   R0,PPRD12                                                        
*                                                                               
PPRD13   DS   0H                                                                
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    PPRD13F                                                          
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     PPRDX                                                            
*                                                                               
PPRD13I  LA    R1,0(R4)            ALREADY 3 CHAR PROD                          
         B     PPRD14                                                           
*                                                                               
PPRD13F  LA    R1,=C'***'          UNKNOWN PRODUCT                              
*                                                                               
PPRD14   MVC   0(3,R5),0(R1)       STORE IN BLOCK AREA                          
         LA    R5,2(,R5)                                                        
PPRD20   CLI   0(R5),C' '          FIND END OF PROD CODE                        
         BNH   PPRD22                                                           
         LA    R5,1(,R5)                                                        
PPRD22   MVI   0(R5),C','          SEPARATE PRODUCTS                            
         AHI   R5,1                OUTPUT LIST POINTER                          
         AHI   R4,1                PRODUCT LIST POINTER                         
*                                                                               
         CLI   0(R6),X'29'         IS THIS MORE PRODUCTS USER                   
         BNE   PPRD26                                                           
         AHI   R4,2                PRODUCT LIST POINTER                         
         SHI   R3,3                                                             
         LTR   R3,R3                                                            
         BNZ   PPRD10                                                           
         B     PPRD30                                                           
*                                                                               
PPRD26   DS    0H                                                               
         BCT   R3,PPRD10                                                        
*                                                                               
PPRD30   DS    0H                                                               
         BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
PPRDX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY LEGAL SCREEN                                                          
*                                                                               
DRLEGAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWASCR,X'9F'        IS LEGAL SCREEN LOADED ALREADY               
         BE    DRL20                YES                                         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,CONTAGH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'9F'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TWASCR,X'9F'                                                     
         MVC   CONREC(6),=C'LCOMML'                                             
         MVC   CONACT(7),=C'DISPLAY'                                            
                                                                                
         BRAS  RE,SETPFK                                                        
                                                                                
         LA    R2,CONHEADH                                                      
*                                                                               
DRL10    OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP                                         
*                                                                               
         CLI   0(R2),0                                                          
         BNE   DRL10                                                            
*                                                                               
         MVC   1(2,R2),=X'0101'    POP IN INDICATORS                            
*                                                                               
* RE-DISPLAY MEDIA/CLIENT/COMMERCIAL                                            
*                                                                               
         MVC   TRAMED(L'QMED),QMED                                              
         OI    TRAMEDH+6,X'80'                                                  
*                                                                               
         MVC   TRACLT(L'QCLT),QCLT                                              
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
         MVC   TRACML(L'HOLDCML),HOLDCML                                        
         OI    TRACMLH+6,X'80'                                                  
*                                                                               
         OI    TRLLCMTH+6,X'40'    CURSOR POSITION                              
*                                                                               
* LEGAL COMMENTS                                                                
*                                                                               
DRL20    L     R6,AIO                                                           
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    DRL25                                                            
         BRAS  RE,PPRD             CHECK BRAND LEVEL SECURITY                   
         CLI   ERROR,SECLOCK                                                    
         BE    TRAPERL                                                          
*                                                                               
DRL25    L     R6,AIO                                                           
         LA    R2,TRLLCMTH         COMMENT FIELD                                
         LA    R0,TRLLCMXH                                                      
*                                                                               
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BAS   RE,DCOMMENT         DISPLAY COMMENTS                             
*                                                                               
* SCHEDULING RESTRICTIONS                                                       
*                                                                               
         L     R6,AIO                                                           
         LA    R2,TRLSCMTH         SCHEDULING COMMENT FIELD                     
         LA    R0,TRLSCMXH         LAST SCHEDULING COMMENT FLD                  
*                                                                               
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,GETEL                                                         
         BAS   RE,DCOMMENT         DISPLAY COMMENTS                             
*                                                                               
         XC    TRLATA,TRLATA       CLEAR APPROVED TO AIR                        
         OI    TRLATAH+6,X'80'     TRANSMIT                                     
*                                                                               
         LA    R2,TRLNET1H         NETWORK FIELD                                
         LA    R0,TRLTAGH          END OF SCREEN                                
         BAS   RE,CLRSCR                                                        
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   SVRLSE,CMLRLSE      SAVE CML RELEASE                             
         MVC   SVRCL,CMLRCL        AND RECALL DATES                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BE    *+14                                                             
         XC    SVANET,SVANET                                                    
         B     DRL30                                                            
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         MVC   TRLATA(L'CMLATAIR),CMLATAIR APPROVED TO AIR                      
*                                                                               
         MVC   SVANET,CMLANET      SAVE APPROVED NETWORKS                       
*                                                                               
DRL30    LA    R2,TRLNET1H                                                      
         BAS   RE,GSTATION         GET STATION APPRL RECS & SHOW NETS           
*                                                                               
DRLX     OI    TRLLCMTH+1,X'01'    MODIFIED                                     
         OI    TRLLCMTH+6,X'80'    TRANSMIT                                     
         XIT1                                                                   
*                                                                               
TRAPERL  GOTO1 ERREX                                                            
*                                                                               
* READ STATION APPROVAL RECORDS AND DISPLAY ON SCREEN                           
*                                                                               
GSTATION NTR1                                                                   
*                                                                               
         MVI   LKEY+1,20                                                        
         MVI   DATADISP+1,27                                                    
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
         MVC   FILENAME,=CL8'UNTDIR'                                            
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVC   STATKID,=X'29'      RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
*                                                                               
         GOTO1 HIGH                                                             
         B     GSTAT08                                                          
*                                                                               
GSTAT05  MVC   FILENAME,=CL8'UNTDIR'                                            
         GOTO1 SEQ                                                              
*                                                                               
GSTAT08  CLC   KEY(2),KEYSAVE                                                   
         BNE   GSTATX                                                           
*                                                                               
         MVC   FILENAME,=CL8'UNTFIL'                                            
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    GSTAT20                                                          
         DC    H'0'                                                             
*                                                                               
         USING STADATEL,R6                                                      
*                                                                               
GSTAT10  B     GSTAT05             USE CURRENT ELEM ONLY                        
         BRAS  RE,NEXTEL           *** NOP                                      
         BNE   GSTATX              *** NOP                                      
*                                                                               
GSTAT20  CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    GSTAT30              NO                                          
*                                                                               
         MVC   IDATE,STAIDATE                                                   
         XC    IDATE,=X'FFFFFF'    INVERT INACTIVE DATE                         
*                                                                               
         CLC   IDATE,SVRLSE        IF INACTIVE IS BEFORE RELEASE                
         BL    GSTAT10             DONE WITH THIS ELEM                          
*                                                                               
GSTAT30  DS    0H                                                               
         CLC   STAADATE,SVRCL      ACTIVE DATE BEFORE RECALL DATE?              
         BH    GSTAT10             YES                                          
         DROP  R6                                                               
*                                                                               
         MVC   8(4,R2),STATKNET    MOVE NETWORK TO SCREEN                       
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO (Y/N) FIELD                          
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STACODEL,R6                                                      
*                                                                               
         MVC   SVCODE,STACODE      SAVE STATION CODE                            
*                                                                               
         MVI   8(R2),C'Y'          PRESET NET APPROVAL TO YES                   
         OI    6(R2),X'80'                                                      
*                                                                               
         ZICM  RE,SVANET,4                                                      
         ZICM  RF,SVANET+4,4                                                    
         N     RE,SVCODE           TURN OFF ALL BITS EXCEPT THIS ONE            
         N     RF,SVCODE+4                                                      
         STM   RE,RF,WORK                                                       
         XC    SVCODE,WORK                                                      
         BZ    *+8                 IF ZERO THEN APPORVED                        
         MVI   8(R2),C'N'                                                       
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT NET FIELD                       
         AR    R2,RF                                                            
*                                                                               
         LA    R0,TRLTAGH          END OF SCREEN                                
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    GSTAT05                                                          
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
GSTATX   XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         GOTO1 HIGH                DUMMY READ HI FOR SEQ                        
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XIT1                                                                   
         EJECT                                                                  
* DISPLAY COMMENTS (IF ANY)                                                     
* (ON ENTRY R2=1ST COMMENT FIELD, R0=LAST COMMENT FIELD)                        
*                                                                               
DCOMMENT NTR1                                                                   
         BNE   DCOM30              NO MORE ELEMENTS                             
*                                                                               
DCOM05   ZIC   RE,1(R6)            GET ELEM LEN                                 
*                                                                               
DCOM10   AHI   RE,-4               GET COMMENT LENGTH -1                        
*                                                                               
         ZIC   RF,0(R2)            GET FIELD LENGTH                             
         LR    R1,RF                                                            
         AHI   RF,-9               GET FIELD LENGTH -1                          
*                                                                               
         EX    RF,DRLCLR           CLEAR OUTPUT FLD                             
*                                                                               
         CR    RF,RE               COMPARE FIELD LEN TO COMMENT LEN             
         BNL   *+6                                                              
         DC    H'0'                WHAT? COMMENT IS LONGER THAN FIELD           
*                                                                               
         EX    RE,DRLMVC           MOVE COMMENT TO FIELD                        
*                                                                               
         OI    6(R2),X'80'         SET ON TRANSMIT BIT                          
*                                                                               
         AR    R2,R1               POINT TO NEXT FIELD                          
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DCOM30                                                           
*                                                                               
         CR    R2,R0               PAST LAST LINE OF COMMENT?                   
         BNH   DCOM05                                                           
         DC    H'0'                WHAT MORE THAN 5 LINES OF COMMENT?           
*                                                                               
DCOM30   ZIC   RF,0(R2)            GET FIELD LENGTH                             
         LR    R1,RF                                                            
         AHI   RF,-9               GET FIELD LENGTH -1                          
*                                                                               
         CR    R2,R0               PAST DESIRED END?                            
         BH    DCOMX                                                            
*                                                                               
         EX    RF,DRLCLR           CLEAR FIELD                                  
         OI    6(R2),X'80'         SET ON TRANSMIT BIT                          
         AR    R2,R1               POINT TO NEXT FIELD                          
         B     DCOM30                                                           
*                                                                               
DCOMX    XIT1                                                                   
*                                                                               
* CLEAR FIELDS ON SCREEN                                                        
* (ON ENTRY R2=1ST FIELD TO CLEAR                                               
*           R0=LAST FIELD ON SCREEN)                                            
*                                                                               
CLRSCR   NTR1                                                                   
*                                                                               
CLR10    LLC   RF,0(R2)            GET FIELD LENGTH                             
         LTR   RF,RF               ANY ENTRY                                    
         BZ    CLRX                NO, DONE                                     
*                                                                               
         LR    R1,RF                                                            
         AHI   RF,-9               GET FIELD LENGTH -1                          
         LTR   RF,RF                                                            
         BNZ   *+14                                                             
         XC    8(1,R2),8(R2)                                                    
         B     *+8                                                              
         EX    RF,DRLCLR           CLEAR FIELD                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         AR    R2,R1               NEXT FIELD ((UN)PROTECTED)                   
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    CLR10                NO, CONTINUE                                
*                                                                               
CLRX     XIT1                                                                   
*                                                                               
DRLMVC   MVC   8(0,R2),3(R6)       COMMENT                                      
DRLCLR   XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE RECORD (BROADCAST BUSINESS SCREEN)                                   
*                                                                               
VRBROAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,TRBBAGYH         BRAND AGY (LEO B. - Y/N)                     
         GOTO1 ANY                                                              
*                                                                               
         MVI   SVATOAIR,0          INIT APPROVED TO AIR                         
*                                                                               
         LA    R2,TRBREFH                                                       
*                                                                               
         CLI   TRBBAPP,C'Y'        BROADCST BUSINESS APPROVAL ?                 
         BE    VRB02                YES                                         
         CLI   5(R2),0             ANY REFERENCE #                              
         BE    VRB05                                                            
         B     REFNOERR            REFERENCE SHOULD BE BLANK                    
*                                                                               
VRB02    GOTO1 ANY                 REFERENCE IS REQUIRED                        
*                                                                               
         BAS   RE,FREFCML          FIND REFERENCED (CML)                        
         BNE   CMLERR              ERROR, NO SUCH CML                           
*                                                                               
VRB05    L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BNE   VRB10               DISPLAY DEFAULTS                             
*                                                                               
         USING CMLBBEL,R6                                                       
         MVC   SVATOAIR,CMLATAIR   SAVE APPROVED TO AIR                         
         MVC   SVANET,CMLANET       AND NETWORK APPROVALS                       
*                                                                               
*                                                                               
VRB10    L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   SVRLSE,CMLRLSE      SAVE CML RELEASE                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING CMLBBEL,R6                                                       
*                                                                               
         MVI   CMLBBEL,X'90'       BROADCAST BUSINESS ELEMENT                   
         MVI   CMLBBLN,CMLBBX-CMLBBEL   ELEMENT LENGTH FOR ADD                  
*                                                                               
         MVC   CMLBBBCP,TRBPCML    BASIC CODE (PARENT CML)                      
         MVC   CMLBBBCM,TRBBCDE    BASIC CODE (MUSIC)                           
         MVC   CMLBBBAG,TRBBAGY    BRAND AGY (LEO B. - Y/N)                     
         MVC   CMLBBJOB,TRBJOBN    JOB NUMBER                                   
         MVC   CMLBBAPR,TRBBAPP    BROADCST BUSINESS APPROVAL                   
         MVC   CMLBBREF,TRBREF     REFERENCE #                                  
         MVC   CMLATAIR,SVATOAIR   APPROVED TO AIR                              
         MVC   CMLANET,SVANET      NETWORK APPROVALS                            
*                                                                               
         XC    CMLBBPDT,CMLBBPDT                                                
         LA    R2,TRBPRDTH                                                      
         CLI   5(R2),0             PRODUCTION DATE ENTERED ?                    
         BE    VRB15                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,TRBPRDT),DATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRB                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DATE),(3,CMLBBPDT)                                
*                                                                               
VRB15    XC    CMLBBCAD,CMLBBCAD                                                
         LA    R2,TRBCADTH            CLIENT APPROVAL DATE                      
         CLI   5(R2),0                                                          
         BE    VRB20                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,TRBCADT),DATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRB                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DATE),(3,CMLBBCAD)                                
*                                                                               
VRB20    DS    0H                                                               
         XC    CMLBBMXD,CMLBBMXD                                                
         LA    R2,TRBMDTEH                                                      
*                                                                               
         CLI   5(R2),0             DATE ENTERED                                 
         BNE   *+12                                                             
         CLI   TRBBAGY,C'Y'        IS BRAND AGY=Y                               
         BNE   VRB25                NO                                          
*                                                                               
         GOTO1 ANY                 MAX USE DATE ENTERED ?                       
*                                                                               
         GOTO1 DATVAL,DMCB,(0,TRBMDTE),DATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRB                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DATE),(3,CMLBBMXD)                                
*                                                                               
         CLC   SVRLSE,CMLBBMXD     CML RELEASE DTE BEFORE MAX USE DTE           
         BH    DATERRB                                                          
*                                                                               
VRB25    GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'92'        INACTIVE DATA ELEM                           
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
*                                                                               
         USING CMLINAEL,R6                                                      
*                                                                               
         MVI   CMLINAEL,X'92'      INACTIVE DATA ELEMENT                        
*                                                                               
         CLI   TRBIRSNH+5,0        INACTIVE RSN ENTERED ?                       
         BE    VRB50                                                            
*                                                                               
         LA    R2,TRBIRSNH         RSN INACTIVE                                 
         GOTO1 ANY                                                              
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         LR    RE,R1                                                            
         AHI   R1,-1               MINUS 1                                      
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMLINRSN(0),WORK                                                 
*                                                                               
         AHI   RE,2                INPUT LEN (LEN/ELCODE)                       
         STC   RE,CMLINLN          TOTAL ELEMENT LENGTH                         
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VRB50    LA    R2,TRBRSNH                                                       
*                                                                               
         CLI   5(R2),0             REASON ?                                     
         BE    VRBX                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,SVTODAY)  TODAY'S DATE                      
         XC    SVTODAY(3),=X'FFFFFF'  XC FOR LIFO                               
*                                                                               
         MVI   BYTE,X'FF'          INIT REASON NUMBER                           
*                                                                               
         L     R6,AIO                                                           
         USING CMLRSNEL,R6                                                      
*                                                                               
         MVI   ELCODE,X'95'        REASON DATA ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   VRB60                                                            
*                                                                               
         CLC   CMLRSNDT,SVTODAY    COMPARE DATE IN ELEM TO TODAY                
         BNE   VRB60               NO, DONE                                     
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         ZIC   R1,CMLRSNN          LAST  REASON NUMBER USED                     
         AHI   R1,-1               NEXT NUMBER TO USE                           
         STC   R1,BYTE                                                          
*                                                                               
         ZIC   R1,1(R6)            ELEM LEN                                     
         AHI   R1,-6               MINUS OVERHEAD                               
*                                                                               
         ZIC   R0,5(R2)            LEN OF THIS COMMENT                          
         CR    R1,R0                                                            
         BNL   *+6                                                              
         LR    R1,R0               USE BIGGER OF THE TWO                        
*                                                                               
         BCTR  R1,0                EXECUTED LENGTH                              
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CMLRSN(0),WORK      SAME COMMENT ?                               
         BE    VRBX                                                             
*                                                                               
VRB60    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         MVI   CMLRSNEL,X'95'      REASON DATA ELEMENT                          
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         LR    RE,R1                                                            
         AHI   R1,-1               MINUS 1                                      
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMLRSN(0),WORK                                                   
*                                                                               
         AHI   RE,6          INPUT LEN+3+1+1+1 (DTE/NUM/LEN/ELCODE)             
         STC   RE,CMLRSNLN         TOTAL ELEMENT LENGTH                         
*                                                                               
         MVC   CMLRSNDT,SVTODAY                                                 
*                                                                               
         MVC   CMLRSNN,BYTE        REASON NUMBER                                
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VRBX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* FIND REFERENCED COMMERCIAL                                                    
*                                                                               
FREFCML  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         MVC   CMLKCML,TRBREF                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SET CC CODE                                  
         XIT1                                                                   
*                                                                               
CMLERR   MVI   ERROR,INVCOMM       INVALID CML                                  
         B     ERRX                                                             
*                                                                               
ADDBERR  MVI   ERROR,INVACT        ADD BROADCAST RECORD IS NOT VALID            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     ERRX                                                             
*                                                                               
DATERRB  MVI   ERROR,INVDATE                                                    
ERRX     GOTO1 ERREX                                                            
                                                                                
REFNOERR MVC   GERROR,=Y(REFNOMSG)                                              
         GOTO1 VTRAERR                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE RECORD (LEGAL SCREEN)                                                
*                                                                               
VRLEGAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ELCODE,X'70'        LEGAL COMMENT ELEM                           
VRL10    XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   ELCODE,X'80'                                                     
         BE    *+12                                                             
         MVI   ELCODE,X'80'        RESTRICTION COMMENT                          
         B     VRL10                                                            
*                                                                               
         LA    R2,TRLLCMTH         LEGAL COMMENT                                
         MVI   ELCODE,X'70'        LEGAL COMMENT ELEM                           
*                                                                               
* USE THIS CODE FOR BOTH LEGAL COMMENTS & SCHEDULE RESTRICTION COMMENT          
*                                                                               
VRL20    LA    R5,1                LINE COUNTER                                 
*                                                                               
VRL30    XC    ELEM,ELEM                                                        
*                                                                               
         LA    R6,ELEM                                                          
         USING CMLLCTEL,R6         USE IT FOR BOTH LEGAL/SCHEDULE COMT          
*                                                                               
         MVC   CMLLCTEL,ELCODE                                                  
         STC   R5,CMLLCTNO         LINE NUMBER                                  
*                                                                               
         ZIC   R1,5(R2)            GET INPUT LENGTH                             
         LTR   R1,R1               ANY COMMENT                                  
         BZ    VRL35                                                            
*                                                                               
         BCTR  R1,0                INPUT LENGTH -1                              
         EX    R1,VRLMVC                                                        
*                                                                               
         AHI   R1,4                TOTAL ELEMENT LENGTH                         
         STC   R1,CMLLCTLN         ELEM LENGTH                                  
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R5,1(R5)            INCREMENT COUNTER                            
*                                                                               
VRL35    ZIC   R1,0(R2)            GET FIELD LENGTH                             
         AR    R2,R1               BUMP TO NEXT FIELD                           
*                                                                               
         LA    R0,TRLLCMXH         LAST LEGAL COMMENT                           
         CLI   ELCODE,X'70'        ARE WE DOING LEGAL COMMENTS                  
         BE    *+8                                                              
         LA    R0,TRLSCMXH         NO, SCHEDULING COMMENTS                      
         CR    R2,R0                                                            
         BNH   VRL30                                                            
*                                                                               
         CLI   ELCODE,X'80'        DID WE DO SCHEDULING CMT YET                 
         BE    VRL50               YES, DONE                                    
*                                                                               
         MVI   ELCODE,X'80'                                                     
         LA    R2,TRLSCMTH         SCHEDULING COMMENT                           
         B     VRL20                                                            
*                                                                               
VRL50    L     R6,AIO                                                           
         LA    R2,TRLNET1H         NETWORKS                                     
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BE    VRL95                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         MVI   CMLBBEL,X'90'       BROADCAST BUSINESS ELEMENT                   
         MVI   CMLBBLN,CMLBBX-CMLBBEL   ELEMENT LENGTH FOR ADD                  
         MVC   CMLATAIR,TRLATA     APPROVED TO AIR                              
*                                                                               
VRL65    ZIC   RF,0(R2)                                                         
         LTR   RF,RF               ANY NETWORK ON SCREEN                        
         BZ    VRL90                NO, DONE                                    
*                                                                               
         AR    RF,R2                                                            
         CLI   8(RF),C'Y'          SEE IF THIS NETWORK IS APPROVED              
         BE    VRL70                                                            
         CLI   8(RF),C'N'                                                       
         BNE   VRL85                                                            
         OI    FLAGS,NOTAPR        TURN NOT APPROVED TO AIR FLAG                
         B     VRL70                                                            
         LR    R2,RF                NO                                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT NETWORK                         
         LA    R0,TRLTAGH          END OF SCREEN                                
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    VRL65                                                            
         B     VRL90                                                            
*                                                                               
VRL70    BAS   RE,NETAPR           TURN ON NETWORK APPROVAL BITS                
*                                                                               
         ZICM  RE,CMLANET,4                                                     
         ZICM  RF,CMLANET+4,4                                                   
         TM    FLAGS,NOTAPR                                                     
         BO    *+16                                                             
         O     RE,SVCODE           TURN ON APPROVAL BITS                        
         O     RF,SVCODE+4         TURN ON APPROVAL BITS                        
         B     VRL80                                                            
*                                                                               
* NETWORK APPROVAL IS SET TO 'N' SEE IF IT WAS SET TO 'Y' PREVIOUSLY            
*                                                                               
         NI    FLAGS,X'FF'-NOTAPR                                               
         N     RE,SVCODE           TURN OFF ALL BITS EXCEPT THIS ONE            
         N     RF,SVCODE+4                                                      
         STM   RE,RF,WORK                                                       
         MVC   WORK+8(L'SVCODE),SVCODE                                          
         XC    WORK+8(L'SVCODE),WORK                                            
         BNZ   VRL85               NOT ZERO THAN NOTHING TO TURN OFF            
         ZICM  RE,CMLANET,4                                                     
         ZICM  RF,CMLANET+4,4                                                   
         X     RE,SVCODE           TURN OFF APPROVAL BITS                       
         X     RF,SVCODE+4         TURN OFF APPROVAL BITS                       
VRL80    STM   RE,RF,SVANET                                                     
         MVC   CMLANET,SVANET                                                   
*                                                                               
VRL85    ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NETWORK(Y/N) FIELD                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NETWORK FIELD                        
         LA    R0,TRLTAGH          END OF SCREEN                                
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    VRL65                                                            
*                                                                               
VRL90    ZIC   R1,CMLBBLN                                                       
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    CMLBBBCP(0),CMLBBBCP ANYTHING IN THE ELEMENT                     
         BZ    VRLX                                                             
*                                                                               
         GOTO1 ADDELEM                                                          
         B     VRLX                                                             
*                                                                               
VRL95    DS    0H                                                               
         MVC   CMLATAIR,TRLATA     APPROVED TO AIR                              
         ZIC   RF,0(R2)                                                         
         LTR   RF,RF               ANY NETWORK ON SCREEN                        
         BZ    VRLX                 NO, DONE                                    
*                                                                               
         AR    RF,R2                                                            
         CLI   8(RF),C'Y'          SEE IF THIS NETWORK IS APPROVED              
         BE    VRL100                                                           
         CLI   8(RF),C'N'                                                       
         BNE   VRL125                                                           
         OI    FLAGS,NOTAPR        TURN NOT APPROVED TO AIR FLAG                
         B     VRL100                                                           
         LR    R2,RF                NO                                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT NETWORK                         
         LA    R0,TRLTAGH          END OF SCREEN                                
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    VRL95                                                            
         B     VRLX                                                             
*                                                                               
VRL100   BAS   RE,NETAPR           TURN ON NETWORK APPROVAL BITS                
*                                                                               
         ZICM  RE,CMLANET,4                                                     
         ZICM  RF,CMLANET+4,4                                                   
         TM    FLAGS,NOTAPR                                                     
         BO    *+16                                                             
         O     RE,SVCODE           TURN ON APPROVAL BITS                        
         O     RF,SVCODE+4         TURN ON APPROVAL BITS                        
         B     VRL120                                                           
*                                                                               
* NETWORK APPROVAL IS SET TO 'N' SEE IF IT WAS SET TO 'Y' PREVIOUSLY            
*                                                                               
         NI    FLAGS,X'FF'-NOTAPR                                               
         N     RE,SVCODE           TURN OFF ALL BITS EXCEPT THIS ONE            
         N     RF,SVCODE+4                                                      
         STM   RE,RF,WORK                                                       
         MVC   WORK+8(L'SVCODE),SVCODE                                          
         XC    WORK+8(L'SVCODE),WORK                                            
         BNZ   VRL125              NOT ZERO THAN NOTHING TO TURN OFF            
         ZICM  RE,CMLANET,4                                                     
         ZICM  RF,CMLANET+4,4                                                   
         X     RE,SVCODE           TURN OFF APPROVAL BITS                       
         X     RF,SVCODE+4         TURN OFF APPROVAL BITS                       
*                                                                               
VRL120   STM   RE,RF,SVANET                                                     
         MVC   CMLANET,SVANET                                                   
*                                                                               
VRL125   ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NETWORK(Y/N) FIELD                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NETWORK FIELD                        
         LA    R0,TRLTAGH          END OF SCREEN                                
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    VRL95                                                            
*                                                                               
VRLX     XIT1                                                                   
*                                                                               
VRLMVC   MVC   3(0,R6),8(R2)                                                    
*                                                                               
* FIND CORRESPONDING STATION CODE AND TURN ON                                   
* NETWORK APPROVAL BITS                                                         
*                                                                               
NETAPR   NTR1                                                                   
*                                                                               
         XC    SVCODE,SVCODE       INIT STATION CODE                            
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         MVC   FILENAME,=CL8'UNTDIR'                                            
         MVI   LKEY+1,20                                                        
         MVI   DATADISP+1,27                                                    
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
*                                                                               
* READ STATION RECORD TO GET STATION CODE                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVC   STATKID,=X'29'      RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
         MVC   STATKNET,8(R2)      NETWORK                                      
         OC    STATKNET,SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     NETAP20                                                          
*                                                                               
NETAP10  MVC   FILENAME,=CL8'UNTDIR'                                            
         GOTO1 SEQ                                                              
*                                                                               
NETAP20  CLC   KEY(2),KEYSAVE                                                   
         BNE   NETAPX                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BNE   NETAP10                                                          
*                                                                               
         MVC   FILENAME,=CL8'UNTFIL'                                            
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'20'        STATION CODE ELEMENT                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STACODEL,R6                                                      
*                                                                               
         MVC   SVCODE,STACODE      SAVE STATION CODE                            
*                                                                               
         DROP  R6                                                               
NETAPX   XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         GOTO1 HIGH                DUMMY READ HI FOR SEQ                        
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XIT1                                                                   
*                                                                               
ADDLERR  MVI   ERROR,INVACT        ADD LEGAL RECORD IS NOT VALID                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY BROADCAST BUSINESS SCREEN                                             
*                                                                               
DRBROAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWASCR,X'6F'        IS BRDCST SCREEN LOADED ALREADY              
         BE    DRB40                YES                                         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,CONTAGH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'6F'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TWASCR,X'6F'        LOADING BROADCAST SCREEN                     
         MVC   CONREC(6),=C'BCOMML'                                             
         MVC   CONACT(7),=C'DISPLAY'                                            
                                                                                
         BRAS  RE,SETPFK                                                        
                                                                                
         LA    R2,CONHEADH                                                      
*                                                                               
DRB20    OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP                                         
*                                                                               
         CLI   0(R2),0                                                          
         BNE   DRB20                                                            
*                                                                               
         MVC   1(2,R2),=X'0101'    POP IN INDICATORS                            
*                                                                               
* RE-DISPLAY MEDIA/CLIENT/COMMERCIAL                                            
*                                                                               
         MVC   TRAMED(L'QMED),QMED                                              
         OI    TRAMEDH+6,X'80'                                                  
*                                                                               
         MVC   TRACLT(L'QCLT),QCLT                                              
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
         MVC   TRACML(L'HOLDCML),HOLDCML                                        
         OI    TRACMLH+6,X'80'                                                  
*                                                                               
         OI    TRBPCMLH+6,X'40'    CURSOR POSITION                              
*                                                                               
* CLEAR FIELDS ON THE SCREEN                                                    
*                                                                               
DRB40    DS    0H                                                               
         LA    R2,TRBBCDEH                                                      
         LA    R0,TRBTAGH                                                       
*                                                                               
DRB42    ZIC   RF,0(R2)            GET FIELD LENGTH                             
         LR    R1,RF                                                            
         AHI   RF,-9               GET FIELD LENGTH -1                          
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         AR    R2,R1               PROTECTED TITLE FIELD                        
         ZIC   RF,0(R2)            GET FLD LEN                                  
         AR    R2,RF               NEXT INPUT FIELD                             
*                                                                               
         CR    R2,R0               CK IF END OF SCREEN                          
         BL    DRB42                NO, CONTINUE                                
*                                                                               
         MVI   TRBBAGY,C'Y'        SET DEFAULT (BRAND AGY=Y)                    
         CLI   SVT1PROF+12,C'Y'    BRAND AGY=Y                                  
         BE    *+8                                                              
         MVI   TRBBAGY,C'N'                                                     
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    DRB44                                                            
*                                                                               
* CHECK BRAND LEVEL SECURITY                                                    
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,PPRD                                                          
         CLI   ERROR,SECLOCK                                                    
         BE    TRAPERB                                                          
*                                                                               
DRB44    L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BNE   DRB55               DISPLAY DEFAULTS                             
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         MVC   TRBPCML,CMLBBBCP    BASIC CODE (PARENT CML)                      
         MVC   TRBBCDE,CMLBBBCM    BASIC CODE (MUSIC)                           
*                                                                               
         CLI   CMLBBBAG,0                                                       
         BE    *+20                                                             
         CLC   CMLBBBAG,SVT1PROF+12 IS BRAND AGY SAME AS IN PROFILE             
         BE    *+10                 YES                                         
         MVC   TRBBAGY(L'CMLBBBAG),CMLBBBAG  BRAND AGY (LEO B. - Y/N)           
*                                                                               
         MVC   TRBJOBN,CMLBBJOB    JOB NUMBER                                   
         MVC   TRBBAPP(L'CMLBBAPR),CMLBBAPR BROADCST BUSINESS APPROVAL          
         MVC   TRBREF,CMLBBREF     REFERENCE #                                  
*                                                                               
         OC    CMLBBPDT,CMLBBPDT   ANY PRODUCTION DATE                          
         BZ    DRB48                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLBBPDT),(8,TRBPRDT) PRODUCTION DATE             
*                                                                               
DRB48    OC    CMLBBCAD,CMLBBCAD   CLIENT APPROVAL DATE                         
         BZ    DRB50                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLBBCAD),(8,TRBCADT) CLIENT APPROV DTE           
*                                                                               
DRB50    OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    DRB55                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLBBMXD),(8,TRBMDTE) MAX USE DATE                
*                                                                               
DRB55    L     R6,AIO                                                           
         MVI   ELCODE,X'92'        INACTIVE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   DRB60                                                            
*                                                                               
         USING CMLINAEL,R6                                                      
*                                                                               
         ZIC   R1,1(R6)            GET ELEM LEN                                 
         AHI   R1,-3               GET DATA LEN-1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRBIRSN(0),CMLINRSN RSN INACTIVE                                 
*                                                                               
DRB60    L     R6,AIO                                                           
         MVI   ELCODE,X'95'        REASON ELEMENT                               
         BRAS  RE,GETEL                                                         
         BNE   DRB70                                                            
*                                                                               
         USING CMLRSNEL,R6                                                      
*                                                                               
         ZIC   R1,1(R6)            GET ELEM LEN                                 
         AHI   R1,-7               GET DATA LEN-1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRBRSN(0),CMLRSN    REASON FOR CHANGE                            
*                                                                               
DRB70    L     R6,AIO                                                           
         MVI   ELCODE,X'10'        CMML DATA ELEMENT                            
         BRAS  RE,GETEL                                                         
         BNE   DRB80                                                            
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(8,TRBACDT) ACTIVE DATE                  
*                                                                               
         CLC   CMLRCL,=X'FFFFFF'                                                
         BNE   *+14                                                             
         MVC   TRBIDTE(3),=CL3'UFN'                                             
         B     DRB80                                                            
*                                                                               
         GOTO1 (RF),(R1),(3,CMLRCL),(5,TRBIDTE) INACTIVE DATE                   
*                                                                               
DRB80    L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   DRBX                                                             
         USING ACTVD,R6                                                         
*                                                                               
         LA    R3,ACTVADDT         DATE CML RECORD ADDED                        
         GOTO1 DATCON,DMCB,(3,(R3)),(8,TRBADTE)                                 
         OI    TRBADTEH+6,X'80'                                                 
*                                                                               
DRBX     OI    TRBPCMLH+1,X'01'    MODIFIED                                     
         OI    TRBPCMLH+6,X'80'    TRANSMIT                                     
         XIT1                                                                   
*                                                                               
TRAPERB  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
FCLT10   DS    0H                                                               
         LA    R4,KEY                                                           
         CLI   KEY,X'0A'                                                        
         BNE   FCLTNE                                                           
         CLI   KEY+1,X'C1'                                                      
         BE    *+12                                                             
         CLI   KEY+1,X'21'                                                      
         BNE   FCLTNE                                                           
         CLC   KEY+2(1),BAGYMD                                                  
         BNE   FCLTNE                                                           
         USING CMLKEY,R4                                                        
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    *+12                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   FCLT12                                                           
*                                                                               
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    FCLT14                                                           
*                                                                               
         ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,*+8              SEE IF PAST KEY                              
         B     *+10                                                             
         CLC   COMPKEY(1),KEY                                                   
         BNE   FCLTNE              YES, ALL DONE                                
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
FCLT12   MVC   BCLT,CMLKCLT                                                     
         DROP  R4                                                               
*                                                                               
FCLT14   MVC   SVKEY,KEY                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,BCLT,QCLT WILL FIX ITSELF IN VALICT FOR AAN          
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
*                                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
*                                                                               
         L     R6,AIO1                                                          
         MVC   SVCLTINT,CCLTIFC-CLTHDR(R6)                                      
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   FCLT20                                                           
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT60               NO                                          
         B     FCLT30                                                           
*                                                                               
FCLT20   CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    FCLT45                                                           
         DC    H'0'                                                             
*                                                                               
* CHECK OUT BRAND LEVEL SECURITY                                                
*                                                                               
FCLT30   MVC   KEY(L'SVKEY),SVKEY  COMMERCIAL KEY                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              COMMERCIAL RECORD                            
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,PPRD             RETURNS 3 CHAR PRDS IN BLOCK                 
         CLI   ERROR,SECLOCK                                                    
         BE    FCLT50                                                           
         B     FCLT60                                                           
*                                                                               
FCLT45   DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
*                                                                               
FCLT47   CLC   KEY(3),KEYSAVE                                                   
         BNE   FCLTNE                                                           
*                                                                               
         OC    KEY+5(8),KEY+5      IF ALL BIN ZEROS                             
         BZ    FCLT48              THEN CMML SEQ # REC, BYPASS                  
         CLC   KEY+5(8),=8C'9'     IF ALL 9'S BYPASS PROD HOUSE                 
         BNE   FCLT10               NO, USE IT                                  
*                                                                               
FCLT48   GOTO1 SEQ                                                              
         B     FCLT47                                                           
*                                                                               
FCLT50   DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(3),KEYSAVE                                                   
         BNE   FCLTNE                                                           
*                                                                               
         OC    KEY+5(8),KEY+5      IF ALL BIN ZEROS                             
         BZ    FCLT55              THEN CMML SEQ # REC, BYPASS                  
         CLC   KEY+5(8),=8C'9'     IF ALL 9'S BYPASS PROD HOUSE                 
         BNE   FCLT10               NO, USE IT                                  
*                                                                               
FCLT55   GOTO1 SEQ                                                              
         B     FCLT10                                                           
*                                                                               
FCLT60   DS    0H                                                               
         BRAS  RE,GETPRF           GET PROFILES                                 
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         CR    RB,RB                                                            
         B     FCLTX                                                            
FCLTNE   LTR   RB,RB                                                            
FCLTX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CHECK FOR DUPLICATE NETWORK ENTRIES                                           
*                                                                               
         DS    0H                                                               
CHKDUP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,MAXNETS          MAX CML NETWORKS                             
         LA    R1,TRANET1H                                                      
CKDUP10  OC    8(4,R1),8(R1)       EMPTY ?                                      
         BZ    CKDUP20              YES                                         
         CR    R1,R2               IS THIS IT                                   
         BE    CKDUP20              YES, IGNORE                                 
         CLC   8(4,R1),8(R2)       DUPLICATE?                                   
         BE    DUPNERR              YES                                         
CKDUP20  ZIC   R3,0(R1)            GET LENGTH                                   
         AR    R1,R3               BUMP TO NEXT NET SUB-FIELD                   
         BCT   R0,CKDUP10                                                       
         XIT1                                                                   
DUPNERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPNETER),DUPNETER                                     
         GOTO1 ERREX2                                                           
         LTORG                                                                  
*                                                                               
DUPNETER DC    C'* ERROR * DUPLICATE NETWORK ENTERED *'                         
         EJECT                                                                  
* VALIDATE COMMERCIAL TYPE                                                      
*                                                                               
         DS    0H                                                               
VTYP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         CLI   SVTN1PR6,C'S'       BLK CML TYPE FIELD ON NINS GEN               
         BE    VTYP01                                                           
*                                                                               
         CLI   SVTN1PR6,C'X'       BLK CML TYPE FIELD ON NINS GEN               
         BNE   VTYP02                                                           
*                                                                               
VTYP01   CLI   5(R2),0             ANY ENTRY                                    
         BE    VTYPXIT                                                          
*                                                                               
VTYP02   DS    0H                                                               
         GOTO1 ANY                                                              
         CLI   WORK,C'?'                                                        
         BNE   VTYP06                                                           
*                                                                               
* LIST VALID TYPES ON 2 BOTTOM SCREEN LINES                                     
         OI    TRATYPLH+6,X'80'    SET XMIT                                     
         LA    R1,TRATYPL                                                       
         L     RF,=A(CTYPTABN)                                                  
         A     RF,SPTR22RR                                                      
         LA    R0,19               MAX 3 CHAR ENTRIES PER LINE                  
VTYP04   DS    0H                                                               
         CLI   0(RF),X'00'         END OF TABLE?                                
         BE    VTYPHX                                                           
         MVC   0(3,R1),0(RF)                                                    
         MVI   3(R1),C' '                                                       
         LA    R1,4(R1)            NEXT SCREEN POSN                             
         LA    RF,3(RF)            NEXT VALID TYPE                              
         BCT   R0,VTYP04                                                        
*                                                                               
         LA    R1,TRATYPLH         GET TO SECOND HELP LINE                      
         ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         OI    6(R1),X'80'         SET XMIT                                     
         LA    R1,8(R1)                                                         
         OC    0(79,R1),0(R1)      ALREADY BEEN HERE?                           
         BNZ   VTYPHX              GET OUT                                      
         LA    R0,20               FOR BCT                                      
         B     VTYP04                                                           
*                                                                               
VTYPHX   MVC   GERROR,=Y(VTYPDIS)                                               
         MVI   GMSGTYPE,C'I'                                                    
         LA    R2,TRATYPEH                                                      
         GOTO1 VTRAERR             GETTXT CALL                                  
*                                                                               
VTYP06   CLI   WORK+3,C' '                                                      
         BH    VTYPER                                                           
         LA    R0,CTYPTBCN                                                      
         L     R1,=A(CTYPTABN)                                                  
         A     R1,SPTR22RR                                                      
*                                                                               
VTYP10   CLC   WORK(3),0(R1)                                                    
         BE    VTYP30                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,VTYP10                                                        
         B     VTYPER                                                           
*                                                                               
VTYP30   TM    TRATYPEH+4,X'80'    INPUT THIS TIME                              
         BZ    VTYPXIT              NO, DONE                                    
*                                                                               
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VTYPXIT              NO                                          
*                                                                               
         MVI   BYTE,1              GENERATE PATTERN T/A REQ                     
*                                                                               
VTYPXIT  XIT1                                                                   
*                                                                               
VTYPER   MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PRINT OVERRIDE *  (EXPECTS R6 = A(CMLDTAEL))                         
*                                                                               
         DS    0H                                                               
VOVRD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CMLDTAEL,R6                                                      
         XC    CMLOVRD1(2),CMLOVRD1  CLEAR OUT CMLOVRD1 & CMLOVRD2              
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VOVRDX               NO                                          
*                                                                               
         CLI   CMLSLN,X'FF'        PRINT OVERRIDE INVALID FOR LEN 'ALL'         
         BNE   *+14                                                             
         MVC   GERROR,=Y(NOOVRD)                                                
         B     VTRAERRX                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=/='                              
         CLI   DMCB+4,2            MUST BE 2 BLOCKS                             
         BE    *+14                                                             
         MVC   GERROR,=Y(BADOVRD)                                               
         B     VTRAERRX                                                         
*                                                                               
         LA    R1,BLOCK                                                         
         TM    2(R1),X'80'                                                      
         BZ    NUMERR1             MUST BE NUMERIC                              
         L     RE,4(R1)                                                         
         STC   RE,CMLOVRD1                                                      
*                                                                               
         LA    R1,32(R1)           NEXT SCANNER BLOCK                           
         TM    2(R1),X'80'                                                      
         BZ    NUMERR1             MUST BE NUMERIC                              
         L     RF,4(R1)                                                         
         STC   RF,CMLOVRD2                                                      
*                                                                               
* MAKE SURE (OVRD1 + OVRD2 = SLN)                                               
         AR    RE,RF                                                            
         IC    RF,CMLSLN                                                        
         CR    RE,RF                                                            
         BNE   OVRDERR                                                          
*                                                                               
VOVRDX   XIT1                                                                   
NUMERR1  MVI   ERROR,NOTNUM                                                     
         GOTO1 ERREX                                                            
OVRDERR  MVC   GERROR,=Y(INVOVRD)                                               
VTRAERRX GOTO1 VTRAERR             GETTXT CALL                                  
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE CLASS                                                                
*                                                                               
         DS    0H                                                               
VCLS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LR    R3,R4                                                            
         LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,BAGYMD                                                    
         MVC   CLSKCLAS,8(R2)                                                   
         OC    CLSKCLAS,SPACES                                                  
         MVC   CLSKCLT,BCLT                                                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET PRODUCT ELEM                             
*                                                                               
         TM    SECFLAG,NEMORPRD    IS THIS MORE PRODUCTS USER                   
         BZ    *+8                                                              
         MVI   ELCODE,X'29'                                                     
         TM    SECFLAG,NECONPRD    IS THIS MORE PRODUCTS USER                   
         BZ    *+8                                                              
         MVI   ELCODE,X'29'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'FF'         THIS ALL PRODS                               
         BE    VCLS30                                                           
*                                                                               
         ZIC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
*                                                                               
         TM    SECFLAG,NEMORPRD+NECONPRD  MORE BRANDS PU(5)=Y,C                 
         BZ    VCLS06                                                           
         LR    R1,R6                                                            
         SR    R0,R0                                                            
VCLS02   DS    0H                  SET R3 TO # OF 2 CHAR PRODS                  
         AHI   R0,1                                                             
         SHI   R3,3                                                             
         LTR   R3,R3                                                            
         BNZ   VCLS02                                                           
         LR    R3,R0                                                            
         B     VCLS24                                                           
VCLS06   DS    0H                                                               
         LA    R5,2(,R6)                                                        
VCLS10   LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
VCLS20   CLC   0(1,R5),3(R1)                                                    
         BE    VCLS24                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VCLS20                                                        
         DC    H'0'                                                             
*VCLS24   MVC   CLSKPROD,0(R1)                                                  
VCLS24   DS    0H                                                               
         LA    R1,2(R1)                                                         
VCLS26   MVC   CLSKPROD,0(R1)                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCLS40                                                           
         MVC   KEY,KEYSAVE                                                      
*        LA    R5,1(,R5)           POINT TO NEXT PRODUCT IN LIST                
*        TM    SECFLAG,NEMORPRD    IS THIS MORE PRODUCTS USER                   
*        BZ    *+8                                                              
*        LA    R5,2(,R5)           POINT TO NEXT PRODUCT IN LIST                
         LA    R1,3(,R1)           POINT TO NEXT PRODUCT IN LIST                
         BCT   R3,VCLS26                                                        
         XC    CLSKPROD,CLSKPROD                                                
*                                                                               
VCLS30   GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCLS40                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    CLSKCLT,CLSKCLT                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVCLASS                                                         
*                                                                               
VCLS40   MVC   KEY,SVKEY                                                        
         CLI   ACTNUM,ACTADD       UNLESS ADD                                   
         BE    VCLSX                                                            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCLSX                                                            
         DC    H'0'                                                             
VCLSX    XIT1                                                                   
*                                                                               
INVCLASS XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVCLSMS),INVCLSMS                                     
         GOTO1 ERREX2                                                           
INVCLSMS DC    C'* ERROR * COMMERCIAL CLASS NOT FOUND *'                        
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PRODUCTION HOUSE                                                     
*                                                                               
         DS    0H                                                               
VHSE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CMLDTAEL,R6                                                      
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S                               
         BNE   VHSEXIT             NO,                                          
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRHKEY,R4                                                        
         MVC   PRHKID,=X'0A29'                                                  
         MVC   PRHKAM,BAGYMD                                                    
         MVC   PRHKPRH,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VHSE05                                                           
         MVI   ERROR,INVPRHSE      NO PROD HOUSE ON FILE                        
         GOTO1 ERREX                                                            
*                                                                               
VHSE05   MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   CMLTITLE+6(9),=CL9'=SHIP HSE'                                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    VHSE10              NO GETREC NEEDED                             
         GOTO1 GETREC                                                           
VHSE10   MVC   AIO,AIO1                                                         
VHSEXIT  XIT1                                                                   
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
* VALIDATE COMMERCIAL LENGTH *                                                  
*                                                                               
         DS    0H                                                               
VSLN     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CMLDTAEL,R6                                                      
         CLC   =CL3'ALL',8(R2)     ALL LENGTHS                                  
         BE    VSLN10                                                           
         GOTO1 VALISLN             COMMERCIAL LENGTH - REQUIRED                 
                                                                                
         CLC   CMLSLN,WORK                                                      
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SLN                                               
                                                                                
         MVC   CMLSLN,WORK                                                      
         B     VSLN50                                                           
*                                                                               
VSLN10   DS    0H                                                               
         L     RF,=A(CTYPTABN)                                                  
         A     RF,SPTR22RR                                                      
         USING CTYPTABN,RF                                                      
*                                                                               
         CLC   CTYPCSL,CMLTYPE     COLOR SLIDE                                  
         BE    VSLN20                                                           
         CLC   CTYPACD,CMLTYPE     ART CARD                                     
         BE    VSLN20                                                           
         CLC   CTYPCSS,CMLTYPE     COLOR SUPER SLIDE                            
         BE    VSLN20                                                           
         CLC   CTYPSSL,CMLTYPE     SUPER SLIDE                                  
         BE    VSLN20                                                           
         CLC   CTYPFBS,CMLTYPE     FULL SCREEN B/W SLIDE                        
         BE    VSLN20                                                           
         CLC   CTYPFCS,CMLTYPE     FULL SCREEN COLOR SLIDE                      
         BE    VSLN20                                                           
         DROP  RF                                                               
*                                                                               
         MVC   GERROR,=Y(SLNALL)                                                
         GOTO1 VTRAERR                                                          
VSLN20   MVI   CMLSLN,X'FF'                                                     
*                                                                               
VSLN50   TM    TRASLNH+4,X'80'     INPUT THIS TIME                              
         BZ    VSLXIT               NO, DONE                                    
*                                                                               
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VSLXIT               NO, DONE                                    
*                                                                               
         MVI   BYTE,1            GENERATE PATTERN T/A REQ                       
*                                                                               
VSLXIT   XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
         DS    0H                                                               
VNET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 ANY                                                              
         MVC   8(5,R2),WORK        MOVE SPACE FILLED FIELD OVER INPUT           
*                                                                               
         MVI   WORK+4,C' '         CLEAR JUST IN CASE                           
*                                                                               
         CLI   WORK+3,C'-'                                                      
         BNE   *+12                                                             
         MVI   WORK+3,C' '                                                      
         B     *+16                                                             
         CLI   WORK+3,C'*'                                                      
         BNE   *+8                                                              
         MVI   WORK+3,C' '                                                      
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
         CLI   8(R1),0                                                          
         BE    VNET10                                                           
         MVC   GERROR,=Y(NONET)                                                 
         GOTO1 VTRAERR                                                          
*                                                                               
VNET10   TM    4(R2),X'80'        INPUT THIS TIME                               
         BZ    VNETX                NO, DONE                                    
*                                                                               
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VNETX                NO, DONE                                    
*                                                                               
         MVI   BYTE,1              GENERATE PATTERN T/A REQ                     
*                                                                               
VNETX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         GOTO1 CLUNPK,DMCB,(SVCPROF6,BCLT),QCLT                                 
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   HDHK10                 NO                                        
         MVC   H3+42(11),=C'TURN-AROUND'                                        
*                                                                               
* PRINT OUT ANY FILTERS                                                         
*                                                                               
HDHK10   OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    HDHKX                NO                                          
         LA    R3,FLD              OUTPUT AREA                                  
         LR    R4,R3               COMPARAND                                    
         XC    FLD,FLD                                                          
         OC    RLDTFTR,RLDTFTR     RELEASE DATE FILTER                          
         BZ    HDHK20                                                           
         MVC   0(3,R3),=C'REL'                                                  
         CLI   RLDTSFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   3(1,R3),RLDTSFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   3(R3),C'='                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RLDTFTR),(5,4(R3))                                
         LA    R3,12(,R3)                                                       
HDHK20   OC    RCDTFTR,RCDTFTR     RECALL DATE FILTER                           
         BZ    HDHK30                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'RCL'                                                  
         CLI   RCDTSFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   3(1,R3),RCDTSFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   3(R3),C'='                                                       
*                                                                               
         CLC   RCDTFTR,=XL3'FFFFFF'                                             
         BNE   HDHK26                                                           
         MVC   4(3,R3),=C'UFN'                                                  
         LA    R3,7(,R3)                                                        
         B     HDHK30                                                           
HDHK26   GOTO1 DATCON,DMCB,(3,RCDTFTR),(5,4(R3))                                
         LA    R3,12(,R3)                                                       
HDHK30   TM    FTRFLAG,DELFTR      SHOW DELETED CML'S ONLY                      
         BZ    HDHK40                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'DEL'                                                  
         LA    R3,4(,R3)                                                        
         EJECT                                                                  
HDHK40   OC    PRODFTR,PRODFTR     PRODUCT FILTER                               
         BZ    HDHK50                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'P'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),PRODFTR                                                  
         CLI   4(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,5(,R3)                                                        
HDHK50   OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    HDHK60                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'T'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),TYPEFTR                                                  
         CLI   4(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,5(,R3)                                                        
HDHK60   OC    SLNFTR,SLNFTR       CML LENGTH FILTER                            
         BZ    HDHK70                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'L'                                                       
         MVI   1(R3),C'='                                                       
         LA    R3,2(,R3)                                                        
         CLI   SLNFTR,255                                                       
         BNE   *+14                                                             
         MVC   0(3,R3),=C'ALL'                                                  
         B     HDHK70                                                           
         EDIT  (B1,SLNFTR),(3,(R3)),ALIGN=LEFT                                  
*                                                                               
HDHK70   MVC   H5+34(34),FLD                                                    
*                                                                               
HDHKX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*                                                                               
         DS    0H                                                               
PUT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         MVC   AIO,AIO3                                                         
         L     R2,AIO1                                                          
         MVC   KEY,0(R2)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO3                                                          
*                                                                               
         CLI   TWASCR,X'F4'        IS THIS REGULAR CML SCREEN                   
         BNE   PUT05                NO                                          
*                                                                               
* SEE IF ANY NETWORK SUB-FIELDS WERE CHANGED                                    
*                                                                               
         LA    R1,TRANET1H                                                      
         MVI   ELCODE,X'22'        CML NETWORK ELEMENT                          
PUT02    LR    R6,R4                                                            
         BRAS  RE,GETEL                                                         
         BNE   PUT05                                                            
*                                                                               
         USING CMLNETEL,R6                                                      
*                                                                               
         LA    R2,1                COUNT NETWORKS IN ELEM                       
         MVC   WORK,SPACES                                                      
PUT02C   MVC   WORK(4),CMLNET      NETWORK                                      
*                                                                               
         CLI   CMLNETLN,6          OLD RECORD?                                  
         BE    PUT02F               YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    PUT02F               NO                                          
         CLI   WORK+3,C' '                                                      
         BNE   *+12                                                             
         MVI   WORK+3,C'-'                                                      
         B     *+8                                                              
         MVI   WORK+4,C'-'                                                      
PUT02F   OC    8(5,R1),SPACES      MAKE SURE NETWORK SPACE FILELD               
         CLC   WORK(5),8(R1)                                                    
         BE    PUT04                                                            
*                                                                               
PUT03    LLC   RF,0(R1)                                                         
         AR    R1,RF                                                            
         LA    RF,TRANET4H         LAST NET SUB-FIELD                           
         CR    R1,RF                                                            
         BNH   PUT02F                                                           
*                                                                               
* NETWORK IN ELEM DOES NOT MATCH TO NETWORK ON SCREEN - SEE IF DELETED          
*                                                                               
         L     R3,AIO1                                                          
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETELR3          R3 WILL BUMP THRU ELEMS                      
         BNE   PUTERR                                                           
*                                                                               
PUT03F   CLC   CMLNET,2(R3)                                                     
         BNE   *+12                                                             
         MVI   ELCODE,X'23'                                                     
         B     PUT04                                                            
*                                                                               
         BRAS  RE,NEXTELR3         BUMP R3                                      
         BNE   PUTERR                                                           
         B     PUT03F                                                           
*                                                                               
PUT04    BRAS  RE,NEXTEL                                                        
         BNE   PUT05                                                            
         LA    R2,1(R2)                                                         
         LA    R1,TRANET1H                                                      
         B     PUT02C                                                           
*                                                                               
PUT05    L     R2,AIO1                                                          
         SR    R3,R3                                                            
         ICM   R3,3,13(R2)                                                      
         L     R4,AIO3                                                          
         LR    R5,R3                                                            
         CLCL  R2,R4                                                            
         BNE   PUT10                                                            
*                                                                               
PUT06    MVI   IOOPT,C'Y'                                                       
*                                                                               
PUT10    MVC   AIO,AIO1                                                         
         XIT1                                                                   
*                                                                               
PUTERR   DS    0H                                                               
*                                                                               
* RE-DISPLAY NET FIELDS                                                         
*                                                                               
         LA    R2,TRANET1H                                                      
         LA    R0,MAXNETS                                                       
*                                                                               
PUTERR1  XC    8(L'TRANET1,R2),8(R2)                                            
         OI    6(R2),X'81'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,PUTERR1                                                       
*                                                                               
         LA    R2,TRANET1H                                                      
         LA    R0,MAXNETS                                                       
         L     R6,AIO3                                                          
         MVI   ELCODE,X'22'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PUTERR2  BRAS  RE,NEXTEL                                                        
         BNE   PUTERR4                                                          
         MVC   8(4,R2),2(R6)                                                    
*                                                                               
         CLI   CMLNETLN,6          OLD RECORD?                                  
         BE    PUTERR3              YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    PUTERR3              NO                                          
         LA    RF,11(R2)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'                                                       
*                                                                               
PUTERR3  OI    6(R2),X'81'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,PUTERR2                                                       
*                                                                               
PUTERR4  LA    R2,TRANET1H                                                      
         MVC   GERROR,=Y(NETCHERR) CAN'T CHANGE NET FIELD                       
         GOTO1 VTRAERR             GETTXT CALL                                  
*                                                                               
* THIS ROUTINE USES R3 TO BUMP THROUGH ELEMENTS                                 
GETELR3  AH    R3,DATADISP                                                      
*                                                                               
FRSTELR3 CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
NEXTELR3 SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R3),1                                                          
         BR    RE                                                               
         AR    R3,RF                                                            
         B     FRSTELR3                                                         
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* BEFORE PUTREC *                                                               
*                                                                               
         DS    0H                                                               
NPAR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD   & BCLT                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF CMML SEQ REC FOUND                        
         BE    PAR10               GO SAVE CMML SEQ NUMBER                      
*                                                                               
         DROP  R4                                                               
*                                                                               
* NOW MUST ADD CMML SEQ REC FOR AGENCY/MEDIA/CLT-ONCE FOR EACH A/M/CLT          
*                                                                               
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         L     R6,AIO                                                           
         USING CMLKEY,R6                                                        
         XC    CMLRECD(256),CMLRECD                                             
         MVC   CMLKEY,KEY                                                       
         XC    ELEM+2(CMLDTAX-CMLDTAEL),ELEM+2                                  
         LA    R6,ELEM                                                          
         USING CMLDTAEL,R6                                                      
         MVI   CMLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL                                        
         MVC   CMLSEQ,=XL3'000001' START SEQ NUMBER ONE IN MASTER               
         MVC   HOLDSEQ,CMLSEQ      START SEQ NUMBER ONE IN DSECT                
         MVC   CMLTITLE,=CL15'CMML SEQ RECORD'                                  
         GOTO1 ADDELEM             ADD CMML DATA ELEMENT                        
         GOTO1 ADDREC                                                           
         B     PAR20                                                            
PAR10    GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   HOLDSEQ,CMLSEQ                                                   
PAR20    MVC   KEY(L'SVKEY),SVKEY       RESTORE KEY AND AIO                     
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        NOW GET DATA ELEM                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   CMLSEQ,HOLDSEQ      AND PUT COMMERCIAL SEQ # IN IT               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* AFTER PUT REC - CHECK IF ACTUAL CMMLS NEED UPDATE *                           
*                                                                               
         DS    0H                                                               
APUT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWASCR,X'F4'        IS THIS REGULAR CML SCREEN                   
         BNE   NPX                  NO, DONE                                    
*                                                                               
         LA    R0,1                                                             
         TM    FLAGS,SOFTDEL                                                    
         BZ    *+10                                                             
         LCR   R0,R0   '                                                        
         B     *+12                                                             
         TM    FLAGS,SOFTREST                                                   
         BZ    NPUT                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
AP10     BRAS  RE,NEXTEL                                                        
         BNE   NPX                                                              
*                                                                               
         LA    R1,2(R6)                                                         
         USING CMLACTEL,R6                                                      
         CLI   CMLACTLN,CMLACTL1              OLD ELEM                          
         BE    AP12                                                             
         CLI   8(R1),C' '                     TEST ISCI                         
         BNH   AP12                           YES                               
         GOTO1 VTRPACK,DMCB,(C'P',2(R6)),DUB   PACK 12 CHAR ADID                
         LA    R1,DUB                         AND POINT TO IT                   
*                                                                               
AP12     BRAS  RE,UPDACT                                                        
         B     AP10                                                             
         EJECT                                                                  
* UPDATE CMMLS IF NEEDED - CHANGE WAS OTHER THAN DELETE OR RESTORE *            
*                                                                               
NPUT     DS    0H                                                               
*                                                                               
* FOR ANY ENTRY THAT IS IN OLDACTS AND NOT IN NEWACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'-1'                                                  
         LHI   R0,-1                                                            
         LA    R2,NUMACTS                                                       
         LA    R3,OLDACTS                                                       
         OC    0(8,R3),0(R3)                                                    
         BZ    NP20                                                             
*                                                                               
NPOUT1   LA    R4,NUMACTS                                                       
         LA    R5,NEWACTS                                                       
*                                                                               
NPIN1    CLC   0(12,R3),0(R5)                                                   
         BE    NP10                                                             
         LA    R5,12(R5)                                                        
         BCT   R4,NPIN1                                                         
*                                                                               
         LR    R1,R3                                                            
         CLI   8(R3),C' '                     TEST ISCI                         
         BNH   NP8                            YES                               
         GOTO1 VTRPACK,DMCB,(C'P',(R3)),DUB   PACK 12 CHAR ADID                 
         LA    R1,DUB                         AND POINT TO IT                   
*                                                                               
NP8      BRAS  RE,UPDACT                                                        
*                                                                               
NP10     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         BZ    NP20                                                             
         BCT   R2,NPOUT1                                                        
*                                                                               
* FOR ANY ENTRY THAT IS IN NEWACTS AND NOT IN OLDACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'1'                                                   
*                                                                               
NP20     DS    0H                                                               
         TM    FLAGS,UNCOVER                                                    
         BNZ   NPX                 NO NEWACTS                                   
         LA    R0,1                                                             
         LA    R2,NUMACTS                                                       
         LA    R3,NEWACTS                                                       
         OC    0(12,R3),0(R3)                                                   
         BZ    NPX                                                              
*                                                                               
NPOUT2   LA    R4,NUMACTS                                                       
         LA    R5,OLDACTS                                                       
*                                                                               
NPIN2    CLC   0(12,R3),0(R5)                                                   
         BE    NP30                                                             
         LA    R5,12(R5)                                                        
         BCT   R4,NPIN2                                                         
*                                                                               
         LR    R1,R3                                                            
         CLI   8(R1),C' '                     TEST ISCI                         
         BNH   NP22                           YES                               
         GOTO1 VTRPACK,DMCB,(C'P',(R3)),DUB   PACK 12 CHAR ADID                 
         LA    R1,DUB                         AND POINT TO IT                   
*                                                                               
NP22     BRAS  RE,UPDACT                                                        
*                                                                               
NP30     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         BZ    NPX                                                              
         BCT   R2,NPOUT2                                                        
*                                                                               
NPX      DS    0H                                                               
         OC    SVADADDK,SVADADDK                                                
         BZ    NP35                                                             
         MVC   KEY(L'SVADADDK),SVADADDK                                         
         MVC   KEY+14(4),SVDSKADR                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         XC    SVADADDK,SVADADDK                                                
*                                                                               
NP35     DS    0H                                                               
         MVC   SVDSKAD,SVDSKADR                                                 
         XC    KEY,KEY                                                          
         OC    SVHDDELK,SVHDDELK                                                
         BZ    NP40                                                             
         CLC   SVHDDELK(2),=X'0AC2'                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDDELK),SVHDDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
                                                                                
NP40     XC    KEY,KEY                                                          
         OC    SVCCDELK,SVCCDELK                                                
         BZ    NP45                                                             
         CLC   SVCCDELK(2),=X'0AC3'                                             
*        BNE   NP45                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCDELK),SVCCDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVCCDELK,SVCCDELK                                                
                                                                                
NP45     XC    KEY,KEY                                                          
         OC    SVHDADDK,SVHDADDK                                                
         BZ    NP50                                                             
         CLC   SVHDADDK(2),=X'0AC2'                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDADDK),SVHDADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
                                                                                
NP50     XC    KEY,KEY                                                          
         OC    SVCCADDK,SVCCADDK                                                
         BZ    NP55                                                             
         CLC   SVCCADDK(2),=X'0AC3'                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCADDK),SVCCADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVCCADDK,SVCCADDK                                                
                                                                                
NP55     DS    0H                                                               
*                                                                               
NPXXIT   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* UPDATE ACTUAL CMML *                                                          
* EXPECTS:                                                                      
*  R0 = H'1' FOR INC, H'-1' FOR DEC                                             
*  R1 = A(CMML ID)                                                              
*                                                                               
         DS    0H                                                               
UPDACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD  & BCLT                                         
         MVC   CMLKCML,0(R1)                                                    
*                                                                               
UPDACT2  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    UPDACT4                                                          
         MVC   KEY,KEYSAVE         RESTORE                                      
         MVC   CMLKID,=X'0AC1'     TRY FOR ADID                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
UPDACT4  MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   BYTE,ELCODE         SAVE ELCODE                                  
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
*                                                                               
         USING CMLDTAEL,R6                                                      
         SR    R1,R1                                                            
         ICM   R1,3,CMLCOVCT                                                    
         AR    R1,R0                                                            
         STCM  R1,3,CMLCOVCT                                                    
*                                                                               
* DO NOT LET GENCON UPDATE ACTIVITY (X'F1') ELEM - SCREWS UP ELCODE             
*                                                                               
         MVI   ACTELOPT,C'N'                                                    
         GOTO1 PUTREC                                                           
         MVI   ACTELOPT,C'Y'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
UAXIT    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* PRINT COVER CROSS REFERENCE *                                                 
* EXPECTS:                                                                      
*  R3 = A(1'ST UNUSED ENTRY)                                                    
*                                                                               
PXREF    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R3               R4 WILL BE # OF ENTRIES                      
         S     R4,ASORTTAB         ...R4=LENGTH OF ALL ENTRIES                  
         LTR   R4,R4               MAKE SURE SOME ENTRIES                       
         BZ    PXXIT                                                            
*                                                                               
         MVI   RCSUBPRG,1                                                       
         SRA   R4,4                ...DIVIDED BY ENTRY LENGTH (16)              
         GOTO1 VQSORT,DMCB,(0,ASORTTAB),(R4),16,16,0                            
         L     R2,ASORTTAB         R2 = A(CURRENT COVER)                        
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTACT,LASTACT                                                  
*                                                                               
PX10     CLC   LASTACT,0(R2)       SAME ACTUAL?                                 
         BE    PX20                 YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CR    R2,R3               PAST LAST ENTRY?                             
         BNL   PXXIT                                                            
         MVC   LASTACT,0(R2)                                                    
         MVC   P+2(8),0(R2)        PRINT ACTUAL                                 
PX15     LA    R4,P+14                                                          
         LA    R0,10               COLS ACROSS                                  
         LA    R1,4                # OF PRINTLINES                              
         LA    R5,P                                                             
PX20     MVC   0(8,R4),8(R2)       PRINT COVER                                  
         LA    R2,16(R2)           NEXT COV                                     
         LA    R4,10(R4)           NEXT COL                                     
         BCT   R0,PX10                                                          
         LA    R4,132+14(R5)       NEXT PRINT LINE                              
         LA    R5,132(R5)                                                       
         LA    R0,10                                                            
         BCT   R1,PX10                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,P+14                                                          
         LA    R0,10               COLS ACROSS                                  
         LA    R1,4                # OF PRINTLINES                              
         LA    R5,P                                                             
         B     PX10                                                             
PXXIT    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PRODUCT LIST AND BUILD PROD LIST ELEMENT                             
*                                                                               
VPRDL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VPMISS              NO                                           
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         USING CMLPRDEL,R6                                                      
         MVI   CMLPRDEL,X'20'      ELEM CODE                                    
         MVI   CMLPRDLN,2          ELEM LENGTH EMPTY                            
*                                                                               
         XC    MORPRDEL,MORPRDEL   CLEAR FOR 3 CHAR PROD LIST                   
         MVI   MORPRDEL,X'29'                                                   
         MVI   MORPRDEL+1,02       ELEM LENGTH EMPTY                            
         LA    R1,MORPRDEL+2          STARTING ADDR FOR 3 CHAR PRODS            
         ST    R1,SVR1                                                          
*                                                                               
         GOTO1 ANY                                                              
         CLC   =CL7'PRD=ALL',WORK                                               
         BNE   VPRDL06                                                          
*                                                                               
         CLI   SVTN2PR9,C'Y'       LIMIT ONE PRD PER ISCII                      
         BE    PRDSERR              YES ERROR                                   
*                                                                               
VPRDL04  DS    0H                                                               
         MVI   CMLPRDLN,3                                                       
         MVI   CMLPRDS,X'FF'                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   MORPRDEL+1,03                                                    
         MVI   MORPRDEL+2,X'FF'                                                 
*                                                                               
         MVC   ELEM,MORPRDEL                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         B     VPRDLX                                                           
*                                                                               
VPRDL06  DS   0H                   ELEM LENGTH EMPTY                            
         MVC   FLDH,TRAPLSTH       SAVE HEADER                                  
         MVC   FLD(L'TRAPLST),TRAPLST                                           
*                                                                               
VPRDL08  GOTO1 SCANNER,DMCB,FLDH,(20,AIO3),0                                    
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3                                                            
         BZ    VPMISS                                                           
         CLI   DMCB+4,1                                                         
         BE    *+12                                                             
         CLI   SVTN2PR9,C'Y'       LIMIT ONE PRD PER ISCII                      
         BE    PRDSERR              YES ERROR                                   
*                                                                               
         L     R4,AIO3             ADDRESS OF FIRST BLOCK                       
*                                                                               
         CLC   FLD(L'TRAPLST),TRAPLST  1ST PROD LINE                            
         BNE   *+8                  NO, DO NOT RESET ELEM                       
         LA    R5,ELEM+2           SAVE PRODUCT CODE HERE                       
*                                                                               
VPRDL10  DS   0H                                                                
         CLC   12(3,R4),=C'POL'    THIS ILLEGAL                                 
         BE    PRDINV                                                           
         CLC   12(3,R4),=C'AAA'    THIS ILLEGAL                                 
         BE    PRDINV                                                           
*                                                                               
* THE 2 PLST FIELDS OF 58 BYTES EACH WILL HOLD A MAX OF 19 2 BYTE               
* PRODUCT CODES FOR A TOTAL POSSIBLE PRODUCT LIST ELEM OF 38 ENTRIES            
* OR AN ELEMENT LENGTH OF 116 BYTES                                             
*                                                                               
* USE FLDAH & FLDA FOR FAKE FLDH & FLD FOR VALIPRD                              
*                                                                               
         XC    FLDA,FLDA                                                        
         MVC   FLDAH,TRAPLSTH                                                   
         MVI   FLDAH+5,03                                                       
         MVC   FLDA(3),12(R4)     MOVE IN PRODUCT CODE                          
*                                                                               
* SAVE CMML IN AIO1                                                             
*                                                                               
         L     RE,AIO1                                                          
         L     R0,AIO2                                                          
         LHI   R1,2000                                                          
*        ICM   R1,3,13(RE)                                                      
         LR    RF,R1                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LR    R0,R2                                                            
         LA    R2,FLDAH                                                         
*                                                                               
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
         LR    R2,R0                                                            
*                                                                               
         CLI   ERROR,0             WAS THERE AN ERROR                           
         BNE   VPTRAPX              YES                                         
*                                                                               
* RESTORE CMML TO AIO1                                                          
*                                                                               
         L     RE,AIO2                                                          
         L     R0,AIO1                                                          
         LHI   R1,2000                                                          
*        ICM   R1,3,13(RE)                                                      
         LR    RF,R1                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         LA    RF,MORPRDEL+2       ADDRESS OF FIRST ENTRY                       
         L     R1,SVR1             ADDRESS OF NEXT                              
*                                                                               
VPRDL16  CR    R1,RF               THIS FIRST/CURR ENTRY                        
         BE    VPRDL18             YES, NO DUPES                                
         CLC   0(3,RF),WORK        THIS A DUPE                                  
         BE    DUPRDER                                                          
         LA    RF,3(,RF)                                                        
         B     VPRDL16                                                          
VPRDL18  MVC   0(1,R5),WORK+3      SAVE BINARY PRODUCT CODE                     
*                                                                               
         MVC   0(3,R1),WORK                                                     
         AHI   R1,3                                                             
         ST    R1,SVR1                                                          
         IC    RE,MORPRDEL+1                                                    
         LA    RE,3(,RE)                                                        
         STC   RE,MORPRDEL+1                                                    
*                                                                               
         LA    R4,32(,R4)          NEXT SCANNER BLOCK                           
         LA    R5,1(,R5)           BUMP ELEMENT POINTER                         
         IC    RE,ELEM+1           GET ELEM LENGTH                              
         LA    RE,1(,RE)                                                        
         STC   RE,ELEM+1                                                        
*                                                                               
         CLI   SVTN2PR5,C'1'       IF NET TALENT USER, NOT ALLOWED              
         BE    *+12                                                             
         CLI   SVTN2PR5,C'Y'       IF NET TALENT USER, NOT ALLOWED              
         BNE   *+8                                                              
         BAS   RE,CKTAL            SEE IF PROD HAS TAL AGENCY                   
*                                                                               
         BCT   R3,VPRDL10                                                       
*                                                                               
         CLC   FLD(L'TRAPLST),TRAPLST  1ST PROD LINE                            
         BNE   VPRDL30             BOTH LINES OF PRODS PROCESSED                
*                                                                               
         ZIC   R0,0(R2)            NEXT PRD LINE                                
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             ANY PROD ON LINE 2                           
         BE    VPRDL30              NO DONE                                     
*                                                                               
         MVC   FLDH,0(R2)                                                       
         MVC   FLD(L'TRAPLST),8(R2)                                             
         B     VPRDL08                                                          
*                                                                               
VPRDL30  CLI   CMLPRDLN,2          WERE ANY PROD CODES FOUND                    
         BE    VPMISS              NO                                           
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   ELEM,MORPRDEL                                                    
         GOTO1 ADDELEM                                                          
         CLI   SVTN2PR5,C'1'       IF NET TALENT USER, MUST DO                  
         BE    *+12                                                             
         CLI   SVTN2PR5,C'Y'       NET TALENT USER?                             
         BNE   VPRDL50              NO, NOT NEEDED                              
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADD, BYPASS REPOSITION                    
         BE    VPRDLX                                                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VPRDL50  TM    TRAPLSTH+4,X'80'    FIELD INPUT THIS TIME                        
         BZ    VPRDLX               NO, DONE                                    
*                                                                               
         CLI   ACTNUM,ACTSEL       ACTION CHANGE                                
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BNE   VPRDLX               NO                                          
*                                                                               
         MVI   BYTE,1              GENERATE PATTERN T/A REQ                     
*                                                                               
VPRDLX   DS   0H                                                                
         XIT1                                                                   
*                                                                               
* CHECK IF PRODUCT HAS A TALENT AGENCY *                                        
*                                                                               
CKTAL    NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+PKEYAM-PKEY(3),BAGYMD                                        
         MVC   KEY+PKEYPRD-PKEY(3),=C'POL'  IF POL HAS ONE, THEN OK             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         GOTO1 GETREC                                                           
*                                                                               
         OC    PTALAGY,PTALAGY                                                  
         BZ    *+14                                                             
         CLC   PTALAGY,SPACES                                                   
         BNE   CKTAL10                                                          
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+PKEYAM-PKEY(3),BAGYMD                                        
         MVC   KEY+PKEYPRD-PKEY(3),WORK                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         GOTO1 GETREC                                                           
         CLI   PFKEY,1                                                          
         BE    CKTAL10                                                          
*                                                                               
         OC    PTALAGY,PTALAGY                                                  
         BZ    NOTALER                                                          
         CLC   PTALAGY,SPACES                                                   
         BE    NOTALER                                                          
CKTAL10  MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1                                                         
         B     VPRDLX                                                           
         DROP  R6                                                               
*                                                                               
DUPRDER  MVC   GERROR,=Y(DUPPRD)                                                
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,4              L'GASUBST + 1                                
         MVC   ELEM+1(3),WORK                                                   
VTRPER   GOTO1 VTRAERR                                                          
*                                                                               
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
         B     VPTRAPX                                                          
VPMISS   MVI   ERROR,MISSING                                                    
         B     VPTRAPX                                                          
*                                                                               
VPTRAP   DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    *+8                                                              
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
VPTRAPX  GOTO1 ERREX                                                            
                                                                                
PRDSERR  MVC   GERROR,=Y(PRDSMSG)                                               
         B     VTRPER                                                           
                                                                                
ERRXIT   GOTO1 ERREX2                                                           
                                                                                
NOTALER  MVC   GERROR,=Y(NOTALMS)  SET MODIFIED                                 
         OI    TRAPLSTH+1,X'01'    SET MODIFIED                                 
         OI    TRAPLSTH+6,X'80'    TRANSMIT                                     
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         STCM  R1,7,GASUBST                                                     
         MVI   DUB,4               L'SUBST TEXT + 1                             
         MVC   DUB+1(3),KEY+4                                                   
         B     VTRPER                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - DATE, PROD, TYPE, LEN *                            
*                                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FILTERS,FILTERS                                                  
         XC    ADIDFLT,ADIDFLT     FILTER ON SPECIFIC ADID                      
         XC    CMLFLT,CMLFLT       CML= (HIDEF,CNTRCUT,ADID)                    
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFEXIT              NO                                           
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
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,(12,TRAFLTRH),(5,BLOCKA)                            
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    INVALER                                                          
         LA    R4,BLOCKA           ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    *+12                YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   *+12                NO, NETHER                                   
         MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
* GET ADDRESS OF FILTER VALIDATION RTN                                          
*                                                                               
         LA    RF,FLTTABLE                                                      
         EX    R1,FLTRCLC                                                       
         BE    FLTRGO                                                           
         LA    RF,L'FLTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VFTR80                                                           
*                                                                               
FLTRCLC  CLC   12(0,R4),0(RF)                                                   
FLTRGO   L     RE,10(RF)                                                        
         A     RE,SPTR22RR                                                      
         BR    RE                                                               
         EJECT                                                                  
FLTREL   LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BNZ   *+12                 YES                                         
         MVI   ERROR,INVDATE                                                    
         B     VFTTRAP                                                          
         GOTO1 DATCON,DMCB,(0,DATE),(3,RLDTFTR)                                 
         MVC   RLDTSFTR,HOLDSIGN                                                
         B     VFTR70                                                           
*                                                                               
FLTRCL   LA    R5,22(,R4)                                                       
*                                                                               
         CLI   1(R4),3                                                          
         BH    FLTRCL10                                                         
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,FLTRCLCL                                                      
         BNE   FLTRCL10                                                         
         MVC   RCDTFTR,=XL3'FFFFFF'                                             
         MVI   RCDTSFTR,0                                                       
         B     VFTR70                                                           
*                                                                               
FLTRCLCL CLC   22(0,R4),=C'UFN'                                                 
*                                                                               
FLTRCL10 GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BNZ   *+12                 YES                                         
         MVI   ERROR,INVDATE                                                    
         B     VFTTRAP                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DATE),(3,RCDTFTR)                                 
         MVC   RCDTSFTR,HOLDSIGN                                                
         B     VFTR70                                                           
*                                                                               
* FILTER ON PGROUP                                                              
*                                                                               
FLTPGRP  OC    BCLT,BCLT           CLIENT MUST HAVE BEEN ENTERED                
         BZ    MISSCLT                                                          
*                                                                               
         CLI   PRDFTR,0            ANY PROD FILTER                              
         BNE   PRDPGRER            YES, ERROR                                   
*                                                                               
         BAS   RE,VPGRP            VALIDATE PGROUP                              
         B     VFTR70                                                           
*                                                                               
FLTPROD  OC    BCLT,BCLT           CLIENT MUST HAVE BEEN ENTERED                
         BZ    MISSCLT                                                          
*                                                                               
         OC    PGRPFTR,PGRPFTR     ANY PGROUP FILTER                            
         BNE   PRDPGRER            YES, ERROR                                   
*                                                                               
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    VFTR28              YES, SAVE IT                                 
         CLI   1(R4),3                                                          
         BH    INVALER                                                          
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVC   FLDH+5(1),1(R4)     SAVE LENGTH                                  
         MVC   FLD(3),22(R4)                                                    
         MVI   ERROPT,C'Y'         RETURN IF ERROR                              
         LR    R0,R2                                                            
         LA    R2,FLDH                                                          
         GOTO1 VALIPRD                                                          
         LR    R2,R0               RESTORE TWA POINTER                          
                                                                                
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    VFTR26                                                           
         MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     VFTTRAP                                                          
                                                                                
VFTR26   MVC   PRODFTR(4),WORK     PROD AND PRD                                 
         B     VFTR70                                                           
*                                                                               
VFTR28   MVC   PRODFTR(3),=C'ALL'  MOVE IN ALL, LEAVE PRD ZERO                  
         B     VFTR70                                                           
*                                                                               
FLTTYPE  MVC   WORK(12),22(R4)                                                  
         BAS   RE,VTYPF                                                         
         MVC   TYPEFTR,WORK                                                     
         B     VFTR70                                                           
*                                                                               
FLTLEN   CLI   1(R4),3             LEN 3                                        
         BNE   VFTR44                                                           
         CLC   =C'ALL',22(R4)      LEN ALL                                      
         BNE   VFTR44                                                           
         MVI   SLNFTR,X'FF'                                                     
         B     VFTR70                                                           
VFTR44   TM    3(R4),X'80'         WAS CML LEN NUMERIC                          
         BNZ   *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     VFTTRAP                                                          
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      CML LEN                                      
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   VFTTRAP             GO PRINT ERROR                               
         MVC   SLNFTR,WORK                                                      
         B     VFTR70                                                           
*                                                                               
FLTDEL   OI    FTRFLAG,DELFTR                                                   
         B     VFTR70                                                           
*                                                                               
FLTFIL   DS   0H                                                                
         TM    WHEN,X'A0'          MUST BE RUN NOW, OV, OR DDS                  
         BNZ   IMMEDERR             NOT IMMED OR SOON                           
*                                                                               
         CLI   OFFLINE,C'Y'        THIS OFFLINE?                                
         BE    FLTFIL20             VALIDATED ONLINE, MUST BE OKAY              
                                                                                
         CLC   =C'SJ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    FLTFIL20                                                         
         CLC   =C'GZ',AGENCY       AGENCY IS GM MEDIAWORKS                      
         BE    FLTFIL20                                                         
         CLC   =C'H9',AGENCY       STARCOM ?                                    
         BNE   VFTR80                                                           
*                                                                               
FLTFIL20 DS   0H                                                                
         OI    FTRFLAG,FILFTR                                                   
         MVI   PQSW,1              SUPPRESS AUTO PRTQUE OPEN                    
*                                                                               
         ZAP   CMLFILCT,=P'0'                                                   
         ZAP   CMLPBCT,=P'0'                                                    
*                                                                               
         L     R0,AIO3                                                          
         L     R1,SIZEIO                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)           CLEAR AIO3 FOR P/B COMMLS TABLE              
         B     VFTR70                                                           
*                                                                               
FLTSEQ   CLI   OFFLINE,C'Y'        IF OFFLINE, NO CHECK                         
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL?                         
         BNE   VFTR80                                                           
*                                                                               
         OI    FTRFLAG,SEQFTR      SHOW COMML SEQ NO.                           
         B     VFTR70                                                           
*                                                                               
FLTADID  DS   0H                                                                
         OI    FTRFLAG,ADIDFTR     SHOW ONLY ADID COMMLS                        
*                                                                               
         CLI   1(R4),0             FILTER ON SPECIFIC ADID                      
         BE    VFTR70              NO                                           
*                                                                               
         CLI   1(R4),9             ADID = 12 ALPHA NUMERICS                     
         BL    BDCML               BAD ADID                                     
*                                                                               
         MVC   ADIDFLT,22(R4)      ADID FILTER                                  
         OC    ADIDFLT,SPACES      ADID FILTER                                  
         B     VFTR70                                                           
*                                                                               
FLTHDEF  DS   0H                                                                
         OI    FTRFLAG,HDEFFTR     SHOW HD CMML INSTEAD OF TITLE                
         B     VFTR70                                                           
*                                                                               
FLTCML   DS   0H                                                                
         OI    FTRFLAG,CMLFTR      SHOW SPECIFIC CML (HIDEF,CNTR,ADID)          
*                                                                               
         CLI   1(R4),0             FILTER ON SPECIFIC CML                       
         BE    VFTR80              NO, ERROR MUST ENTER CML                     
*                                                                               
         CLI   1(R4),9             MUST BE 9-12 ALPHA NUMERIC                   
         BL    BDCML               BAD ADID                                     
*                                                                               
         MVC   CMLFLT,22(R4)       ADID FILTER                                  
         OC    CMLFLT,SPACES       ADID FILTER                                  
         B     VFTR70                                                           
*                                                                               
FLTCCUT  DS   0H                                                                
         OI    FTRFLAG,CCUTFTR     SHOW ONLY CMLS W/CENTRCUT                    
         CLI   1(R4),0             SPECIFIC CML IS NOT VALID HERE               
         BNE   VFTR80              NO                                           
         B     VFTR70                                                           
*                                                                               
VFTR70   LA    R4,34(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     VFEXIT                                                           
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELP),FTRMSG                               
         B     ERREXIT                                                          
*                                                                               
BDCML    MVC   CONHEAD(L'BDCMLMS),BDCMLMS                                       
         GOTO1 ERREX2                                                           
*                                                                               
VTYPF    NTR1                                                                   
         CLI   WORK+3,C' '                                                      
         BH    VTYPER2                                                          
         LA    R1,VTYPTABN                                                      
         LA    R0,VTYPTBCN                                                      
                                                                                
VTYPF10  CLC   WORK(3),0(R1)                                                    
         BE    VTYPF20                                                          
         LA    R1,3(,R1)                                                        
         BCT   R0,VTYPF10                                                       
VTYPER2  MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         B     VFTTRAP                                                          
VTYPF20  B     VFEXIT                                                           
                                                                                
VTYPTABN DC    CL3'ABB'            AUDIO BILLBOARD                              
         DC    CL3'BET'                                                         
         DC    CL3'D2 '            DIGITAL FORMAT                               
         DC    CL3'ACD'            ART CARD                                     
         DC    CL3'AT '            AUDIO TAPES                                  
         DC    CL3'CLR'            CLR/VTR                                      
         DC    CL3'CPY'            COPY                                         
         DC    CL3'CSL'            COLOR SLIDE                                  
         DC    CL3'CSS'            COLOR SUPERSLIDE                             
         DC    CL3'EPS'            ENCAPSULATED POSTSCRIPT                      
         DC    CL3'FBB'            FILM BILLBOARD                               
         DC    CL3'FBS'            FULL SCREEN B/W SLIDE                        
         DC    CL3'FCS'            FULL SCREEN COLOR SLIDE                      
         DC    CL3'HDT'            HDVT HIGH DEFINITION TV                      
         DC    CL3'HV1'            HIGH BAND 1                                  
         DC    CL3'GIF'            GRAPHIC FORMAT                               
         DC    CL3'JPG'            .JPEG GRAPHIC FORMAT                         
         DC    CL3'SC '            SLIDE COPY                                   
         DC    CL3'SSL'            SUPERSLIDE                                   
         DC    CL3'VTR'            VIDEO TAPE                                   
VTYPTBCN EQU   (*-VTYPTABN)/3                                                   
         DC    X'00'               END OF TABLE MARKER                          
         EJECT                                                                  
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
VPGRP    NTR1                                                                   
         CLI   1(R4),1             MUST BE SECOND ENTRY                         
         BL    PGRLENER             NO                                          
         CLI   1(R4),4                                                          
         BH    PGRLENER                                                         
         CLI   22(R4),C'A'         MUST BE A                                    
         BL    VPGR02                                                           
         CLI   22(R4),C'Z'           TO Z                                       
         BH    VPGR02                                                           
         MVC   PGRPFTR,22(R4)      SAVE PROD GROUP SCHEME                       
         B     VPGR05                                                           
*                                                                               
VPGR02   CLI   SVTN2PR1,C'A'       SEE IF                                       
         BL    PGRFORER              PROFILE                                    
         CLI   SVTN2PR1,C'Z'           IS SET FOR                               
         BH    PGRFORER                      PGROUP                             
*                                                                               
         MVC   PGRPFTR(1),SVTN2PR1  SAVE PROD GROUP SCHEME                      
         ZIC   R1,1(R4)                                                         
         AHI   R1,-2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PGRPFTR+1(0),22(R4) AND PROD GROUP NUMBER                        
*                                                                               
VPGR05   ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         LA    R1,PGRPFTR+1        POINT PAST SCHEME                            
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VPGR10   CLI   0(R1),C'0'                                                       
         BL    PGRFORER                                                         
         CLI   0(R1),C'9'                                                       
         BH    PGRFORER                                                         
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R0,VPGR10                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   PRGRFTR,WORK                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(1),PGRPFTR                                                 
         MVC   KEY+6(2),PRGRFTR                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(8),KEYSAVE      FIND A PRODUCT GROUP                         
         BE    VPGR20                                                           
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLTS                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(8),KEYSAVE      FIND A PRODUCT GROUP                         
         BE    VPGR20                                                           
*                                                                               
         MVC   GERROR,=Y(PRDGRPNF)                                              
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         STCM  R1,7,GASUBST                                                     
         MVI   DUB,6               L'SUBST TEXT + 1                             
         MVC   DUB+1(1),PGRPFTR                                                 
         UNPK  WORK+3(5),WORK(3)                                                
         MVC   DUB+2(4),WORK+3                                                  
VTRAPERR GOTO1 VTRAERR                                                          
*                                                                               
VPGR20   LA    R0,L'PGRLFTR                                                     
         LA    R5,PGRLFTR                                                       
         XC    PGRLFTR,PGRLFTR                                                  
*                                                                               
VPGR30   LA    RE,NCLSTSIZ                                                      
         L     RF,ASVNCLST                                                      
VPGR34   CLC   0(3,RF),KEY+8                                                    
         BE    VPGR40                                                           
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,VPGR34                                                        
         MVI   ERROR,SECLOCK                                                    
         B     VFTTRAP                                                          
*                                                                               
VPGR40   MVC   0(1,R5),3(RF)                                                    
         LA    R5,1(,R5)                                                        
         BCT   R0,VPGR44                                                        
         DC    H'0'                TOO MANY PRODUCTS FOR TABLE                  
VPGR44   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(8),KEYSAVE                                                   
         BE    VPGR30                                                           
         B     VFEXIT                                                           
*                                                                               
PGRFORER MVC   GERROR,=Y(BADPRGR)                                               
         B     VTRAPERR                                                         
*                                                                               
PGRLENER MVC   GERROR,=Y(BDPGRPLN)   1 TO 3 DIGIT                               
         B     VTRAPERR                                                         
*                                                                               
PRDPGRER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDPGRMS),PRDPGRMS                                     
         B     ERREXIT                                                          
IMMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG),FTRMSG                                         
         MVC   CONHEAD+L'FTRMSG+1(L'IMMEDMS),IMMEDMS                            
         B     ERREXIT                                                          
         EJECT                                                                  
                                                                                
INVALER  MVI   ERROR,INVALID                                                    
         B     VFTTRAP                                                          
                                                                                
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR2 MVI   ERROR,MISSING                                                    
VFTTRAP  GOTO1 ERREX                                                            
VFEXIT   XIT1                                                                   
ERREXIT  GOTO1 ERREX2                                                           
PRDPGRMS DC    C'* EITHER PRODUCT OR PRODUCT GROUP BUT NOT BOTH *'              
IMMEDMS  DC    C'FILE OPT MUST BE RUN NOW, OR OVERNIGHT *'                      
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC  C'VALID FLTRS: DEL/REL/RCL/PRD/LEN=/CNTR/HIDEF/CML='               
BDCMLMS  DC  C'* ERROR * COMMERCIAL MUST BE 9-12 ALPAH/NUM *'                   
*                                                                               
FLTTABLE DS    0CL14                                                            
         DC    CL10'REL       ',AL4(FLTREL)                                     
         DC    CL10'RCL       ',AL4(FLTRCL)                                     
         DC    CL10'PRD       ',AL4(FLTPROD)                                    
         DC    CL10'PGROUP    ',AL4(FLTPGRP)                                    
         DC    CL10'PROD      ',AL4(FLTPROD)                                    
         DC    CL10'TYPE      ',AL4(FLTTYPE)                                    
         DC    CL10'LEN       ',AL4(FLTLEN)                                     
         DC    CL10'DELETED   ',AL4(FLTDEL)                                     
         DC    CL10'SEQ       ',AL4(FLTSEQ)                                     
         DC    CL10'FILE      ',AL4(FLTFIL)                                     
         DC    CL10'ADID      ',AL4(FLTADID)                                    
         DC    CL10'AD-ID     ',AL4(FLTADID)                                    
         DC    CL10'HIDEF     ',AL4(FLTHDEF)                                    
         DC    CL10'HDEF      ',AL4(FLTHDEF)                                    
         DC    CL10'CML       ',AL4(FLTCML)                                     
         DC    CL10'CNTRCUT   ',AL4(FLTCCUT)                                    
         DC    X'FF'                                                            
                                                                                
BLOCKA   DS    CL(L'BLOCK)                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
* DISPLAY CHANGE ELEMENTS USING DF SCREEN                                       
*----------------------------------------------------------------*              
DSPCHG   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    DMCB(24),DMCB                                                    
         LA    RE,TRAFLTRH                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,X'DF'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TWASCR,X'DF'        SET DF SCREEN LOADED                         
                                                                                
         LA    R2,TRAFLTRH         FIRST DISPLAY LINE                           
         USING ACDD,R2                                                          
                                                                                
         LA    R3,8(R2)                                                         
         MVC   0(5,R3),=C'ADDED'                                                
         XC    1(4,R3),SPACES      MAKE LOWERCASE                               
         MVI   8(R3),C'?'                                                       
         LA    R3,8(R3)                                                         
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   DSPCHG0                                                          
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(8,(R3))                                
                                                                                
DSPCHG0  LA    R2,ACDNEXT                                                       
                                                                                
         MVI   ELCODE,X'C0'                                                     
         L     R6,AIO              FIND MY CHANGE ELEMENTS                      
         USING CMLCHEL,R6                                                       
                                                                                
         SR    R4,R4                                                            
         BRAS  RE,GETEL                                                         
         BE    DSPCHG1                                                          
         MVC   ACDDATA(28),=C'CHANGE HISTORY NOT AVAILABLE'                     
         B     DSPCHGX4                                                         
                                                                                
DSPCHG1  MVC   CHGELNUM,SVNXTCHG   NOTE STARTING POINT                          
         CLI   SVNXTCHG,0                                                       
         BE    DSPCHG2                                                          
                                                                                
DSPCHG1A LA    R4,1(R4)                                                         
         CLM   R4,1,SVNXTCHG                                                    
         BE    DSPCHG2                                                          
         BRAS  RE,NEXTEL                                                        
         BE    DSPCHG1A                                                         
         DC    H'0'                                                             
                                                                                
DSPCHG2  OI    1(R2),X'08'            SET HIGH INTENSITY                        
         MVC   ACDDATA(7),=C'CHANGED'                                           
         XC    ACDDATA+1(6),SPACES    MAKE LOWERCASE                            
         LA    R4,ACDDATA+8                                                     
         GOTO1 DATCON,DMCB,(3,CMLCHDAT),(8,(R4))                                
         LA    R4,9(R4)                                                         
                                                                                
         MVC   0(12,R4),=C'AT HH.MM BY '                                        
         XC    0(12,R4),SPACES                                                  
         MVI   5(R4),C'.'                                                       
                                                                                
         LA    R4,3(R4)            POINT TO HH                                  
         LA    R1,CMLCHTIM                                                      
         LA    R0,2                                                             
                                                                                
DSPCHG4  SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
                                                                                
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DSPCHG4                                                       
                                                                                
         LA    R4,3(R4)            NEXT OUTPUT POSITION                         
         MVC   0(8,R4),CMLCHWHO                                                 
                                                                                
         LA    R2,ACDNEXT          NEXT DISPLAY LINE                            
         CLI   0(R2),0             TEST EOS                                     
         BE    DSPCHGX                                                          
                                                                                
         LA    R3,12(R2)           FIRST OUTPUT POSITION                        
         LA    R4,CHGTAB1                                                       
         LA    R5,CHGTAB1N                                                      
         LA    R1,CMLCHDT1         BYTE TO BE TESTED                            
         BAS   RE,TESTIT                                                        
                                                                                
         LA    R4,CHGTAB2                                                       
         LA    R5,CHGTAB2N                                                      
         LA    R1,CMLCHDT2                                                      
         BAS   RE,TESTIT                                                        
                                                                                
         LA    R4,CHGTAB3                                                       
         LA    R5,CHGTAB3N                                                      
         LA    R1,CMLCHDT3                                                      
         BAS   RE,TESTIT                                                        
                                                                                
         BCTR  R3,0                BACK UP TO END OF LAST FIELD                 
         CLI   0(R3),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '          AND GET RID OF /                             
                                                                                
         CLI   1(R6),20            TEST FOR OLD CHGEL LENGTH                    
         BNH   DSPCHG40                                                         
                                                                                
         LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)           START OF OUTPUT DATA                         
                                                                                
*----------------------------------------------------------------*              
* FOR MOST RECENT CHANGE, NEW DATA IS IN RECORD                                 
* FOR PREVIOUS CHANGES, IT IS IN PREVIOUS CHGEL                                 
*----------------------------------------------------------------*              
                                                                                
         LR    R7,R6               SAVE CHANGE ELEMENT ADDRESS                  
         XC    WORK,WORK           EXTRACT NEW DATA INTO WORK                   
                                                                                
         CLI   CHGELNUM,0          IF THIS IS FIRST CHGEL                       
         BNE   DSPCHG6                                                          
         BRAS  RE,GETCUR                                                        
         B     DSPCHG8                                                          
                                                                                
DSPCHG6  LLC   R5,CHGELNUM         GET CURRENT CHGEL NUMBER                     
                                                                                
         L     R6,AIO              FIND MY CHANGE ELEMENTS                      
         USING CMLCHEL,R6                                                       
         BRAS  RE,GETEL                                                         
         B     DSPCHG7A                                                         
                                                                                
DSPCHG7  BRAS  RE,NEXTEL           AND GET THE ONE BEFORE IT                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DSPCHG7A BCT   R5,DSPCHG7                                                       
                                                                                
         MVC   WORK(23),CMLCHPRD   SAVE AS THE CURRENT 'NEW'                    
                                                                                
DSPCHG8  LLC   R0,CHGELNUM                                                      
         AHI   R0,1                                                             
         STC   R0,CHGELNUM                                                      
                                                                                
         MVI   ELCODE,X'C0'        RESTORE ELCODE                               
         LR    R6,R7               AND CHGEL POINTER                            
                                                                                
* TEST FOR A CHANGE TO PRODUCTS/START DATE/END DATE/SLN                         
                                                                                
         TM    CMLCHDT1,CMLCH_PRDL+CMLCH_SDAT+CMLCH_EDAT+CMLCH_SLN              
         BNZ   DSPCHG10                                                         
         TM    CMLCHDT2,CMLCH_TIME                                              
         BNZ   DSPCHG20                                                         
         B     DSPCHG30                                                         
                                                                                
DSPCHG10 TM    CMLCHDT1,CMLCH_SDAT+CMLCH_EDAT                                   
         BZ    DSPCHG14                                                         
                                                                                
         MVC   0(11,R4),=C'DATES FROM '                                         
         XC    1(9,R4),SPACES                                                   
         LA    R4,11(R4)                                                        
         GOTO1 DATCON,DMCB,(3,CMLCHSDT),(8,0(R4))                               
         MVI   8(R4),C'-'                                                       
         MVC   9(3,R4),=C'UFN'                                                  
         CLC   =X'FFFFFF',CMLCHEDT                                              
         BE    DSPCHG11                                                         
         GOTO1 (RF),(R1),(3,CMLCHEDT),(8,9(R4))                                 
DSPCHG11 LA    R4,18(R4)                                                        
                                                                                
         MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
         LA    R4,3(R4)                                                         
         GOTO1 DATCON,DMCB,(3,WORK+8),(8,0(R4))                                 
         MVI   8(R4),C'-'                                                       
         MVC   9(3,R4),=C'UFN'                                                  
         CLC   =X'FFFFFF',WORK+11                                               
         BE    DSPCHG12                                                         
         GOTO1 (RF),(R1),(3,WORK+11),(8,9(R4))                                  
                                                                                
DSPCHG12 LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)                                                        
                                                                                
DSPCHG14 TM    CMLCHDT1,CMLCH_SLN                                               
         BZ    DSPCHG20                                                         
                                                                                
DSPCHG16 MVC   0(8,R4),=C'SLN FROM'                                             
         XC    1(7,R4),SPACES                                                   
         LA    R4,9(R4)                                                         
                                                                                
         SR    R0,R0                                                            
         IC    R0,CMLCHSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
                                                                                
         MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
         IC    R0,WORK+14                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(3,R4),DUB                                                      
                                                                                
         LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)                                                        
                                                                                
DSPCHG20 TM    CMLCHDT2,CMLCH_TIME                                              
         BZ    DSPCHG30                                                         
                                                                                
         MVC   0(20,R4),=C'MATCHING TIMES FROM '                                
         XC    1(19,R4),SPACES                                                  
                                                                                
         OC    CMLCHSTM(4),CMLCHSTM                                             
         BZ    DSPCHG21                                                         
         CLC   CMLCHSTM(4),=X'0000FFFF'                                         
         BNE   DSPCHG22                                                         
DSPCHG21 MVC   20(4,R4),=C'NONE'                                                
         LA    R4,25(R4)                                                        
         B     DSPCHG24                                                         
                                                                                
DSPCHG22 GOTO1 UNTIME,DMCB,CMLCHSTM,20(R4)                                      
         LA    R4,32(R4)                                                        
         CLI   0(R4),C' '          FIND LAST CHAR OF TIME                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
                                                                                
DSPCHG24 MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
                                                                                
         MVC   3(4,R4),=C'NONE'                                                 
         CLC   WORK+15(4),=X'0000FFFF'                                          
         BE    DSPCHG26                                                         
         GOTO1 UNTIME,DMCB,WORK+15,3(R4)                                        
                                                                                
DSPCHG26 LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)                                                        
                                                                                
DSPCHG30 TM    CMLCHDT1,CMLCH_PRDL                                              
         BZ    DSPCHG42                                                         
                                                                                
         MVC   0(9,R4),=C'PRDS FROM'                                            
         XC    1(8,R4),SPACES                                                   
         LA    R4,10(R4)                                                        
                                                                                
         LA    R1,CMLCHPRD                                                      
         LA    R0,8                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   DSPCHG32                                                         
         LA    R4,4(R4)                                                         
         B     DSPCHG34                                                         
                                                                                
DSPCHG32 BAS   RE,GETPRD                                                        
                                                                                
         MVC   0(3,R4),0(RF)                                                    
         LA    R1,1(R1)                                                         
                                                                                
         LA    R4,2(R4)            POINT TO 3RD CHAR                            
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
                                                                                
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,DSPCHG32                                                      
                                                                                
         BCTR  R4,0                                                             
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
                                                                                
DSPCHG34 MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
         LA    R4,3(R4)                                                         
                                                                                
         LA    R1,WORK                                                          
         LA    R0,8                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         CLI   0(R1),X'FF'                                                      
         BE    DSPCHG40                                                         
                                                                                
DSPCHG36 BAS   RE,GETPRD                                                        
                                                                                
         MVC   0(3,R4),0(RF)                                                    
                                                                                
         LA    R4,2(R4)            POINT TO 3RD CHAR                            
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
                                                                                
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,DSPCHG36                                                      
                                                                                
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
                                                                                
DSPCHG40 LA    R2,ACDNEXT                                                       
         CLI   0(R2),0             TEST REACHED EOS                             
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
                                                                                
DSPCHG42 MVI   ELCODE,X'C0'        RESTORE ELCODE                               
         BRAS  RE,NEXTEL           GET NEXT CHANGE ELEMENT                      
         BE    DSPCHG2             GO BACK TO DISPLAY                           
         B     DSPCHGX4                                                         
                                                                                
DSPCHGX  CLI   0(R2),0             DID WE GET HERE BECAUSE OF EOS               
         BNE   DSPCHGX4            NO                                           
                                                                                
         LR    R7,R6               SAVE CURRENT CHGEL ADDR                      
         L     R6,AIO              COUNT TO THIS CHGEL NUMBER                   
         LA    R6,24(R6)           FIRST ELEMENT                                
         SR    R4,R4                                                            
                                                                                
DSPCHGX2 LA    R4,1(R4)            FIRST ELNUM IS 1                             
         STC   R4,SVNXTCHG         SAVE ELNUM FOR NEXT DISPLAY                  
         BRAS  RE,NEXTEL                                                        
         BNE   DSPCHGX4                                                         
         CR    R7,R6                                                            
         BNE   DSPCHGX2                                                         
         B     DSPCHGX6                                                         
                                                                                
DSPCHGX4 MVI   SVNXTCHG,0          INDICATE NO MORE CHGELS                      
                                                                                
DSPCHGX6 CLI   0(R2),0             FIND EOS                                     
         BE    *+12                                                             
         LA    R2,ACDNEXT                                                       
         B     DSPCHGX                                                          
                                                                                
         MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
         BRAS  RE,SETPFK                                                        
                                                                                
         OI    CONSERVH+1,X'01'     FORCE MODIFIED                              
         OI    CONSERVH+6,X'80'     AND XMT                                     
         J     EXIT                                                             
                                                                                
GETPRD   L     RF,ASVCLIST                                                      
                                                                                
GETPRD2  CLC   3(1,RF),0(R1)                                                    
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   GETPRD2                                                          
         LA    RF,=C'***'                                                       
         BR    RE                                                               
                                                                                
TESTIT   SR    RF,RF                                                            
         IC    RF,0(R4)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    TESTIT2                                                          
         MVC   0(10,R3),1(R4)                                                   
         XC    0(10,R3),=X'00404040404040404040'  MAKE LOWERCASE                
                                                                                
         LA    R3,11(R3)           PUT A / AFTER LAST CHAR                      
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'/'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         LA    R0,60(R2)           CAN'T START A DESC AFTER HERE                
         CR    R3,R0                                                            
         BL    TESTIT2                                                          
         BCTR  R3,0                                                             
         MVI   0(R3),C' '          GET RID OF LAST /                            
                                                                                
         LA    R2,ACDNEXT          CONTINUE ON NEXT LINE                        
         CLI   0(R2),0             TEST REACHED EOS                             
         BE    DSPCHGX             YES - CAN'T DISPLAY ANY MORE                 
         XC    ACDDATA,ACDDATA                                                  
         LA    R3,12(R2)           FIRST OUTPUT POSN                            
                                                                                
TESTIT2  LA    R4,L'CHGTAB1(R4)                                                 
         BCT   R5,TESTIT                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------*              
* BUILD SAVE AREA IN WORK OF USEFUL CURRENT DATA IN RECORD                      
*                                                                               
* WORK(8)     PRODUCT LIST                                                      
* WORK+8(6)   START/END DATES                                                   
* WORK+14(1)  SLN                                                               
* WORK+16(4)  MATCH START/END TIMES                                             
*----------------------------------------------------------------*              
GETCUR   NTR1                                                                   
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CMLDTAEL,R6                                                      
         MVC   WORK+8(6),CMLRLSE   START/END DATES                              
         MVC   WORK+14(1),CMLSLN                                                
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         CHI   RF,8                                                             
         BNH   *+8                                                              
         LHI   RF,8                                                             
         AHI   RF,-3                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R6)                                                    
                                                                                
         MVI   ELCODE,X'B0'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING CMLMATEL,R6                                                      
         MVC   WORK+15(4),CMLMSTIM    SAVE START/END TIME                       
         J     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------*              
* SET PFKEYS IN LAST LINE ON SCREEN                                             
*----------------------------------------------------------------*              
SETPFK   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,TRAFLTRH         FIND EOS                                     
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *-10                                                             
         SR    R2,R0               BACK UP TO LAST FIELD                        
         LA    R1,8(R2)            SET FIRST OUTPUT POSN                        
                                                                                
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AHI   RE,-9                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
                                                                                
         CLI   TWASCR,X'DF'        TEST CHGHIST ACTIVE                          
         BNE   SETPFK2                                                          
         BAS   RE,CLRPFK                                                        
         MVC   0(9,R1),=C'PF4=COMML'                                            
         XC    5(4,R1),SPACES      MAKE LOWERCASE                               
         LA    R1,10(R1)                                                        
         CLI   SVNXTCHG,0          TEST ANY MORE CHGELS                         
         BE    SETPFKX                                                          
         MVC   0(10,R1),=C'5=CHGHIST+'                                          
         XC    3(6,R1),SPACES                                                   
*                                                                               
         LA    R1,10(R1)                                                        
         MVC   0(9,R1),=C'6=COMTEXT'                                            
         XC    3(6,R1),SPACES                                                   
         B     SETPFKX                                                          
                                                                                
SETPFK2  BRAS  RE,CLRPFK                                                        
         LA    R1,8(R2)                                                         
         MVC   0(2,R1),=C'PF'                                                   
         LA    R1,2(R1)                                                         
         B     SETPFK8    <<<< NOP LEGAL/BROAD SCREENS >>>                      
                                                                                
*----------------------------------------------------------------*              
* BCOM AND LCOM ONLY FOR GM/H9/SJ                                               
*----------------------------------------------------------------*              
                                                                                
*        L     R6,AIO                                                           
*        CLC   =C'SJ=NO',CMLTITLE-CMLRECD(R6)                                   
*        BE    *+14                                                             
*        CLC   =C'SJ',AGENCY       AGENCY IS SJR                                
*        BE    SETPFK4                                                          
*        CLC   =C'MC',AGENCY       AGENCY IS GM MCCANN                          
*        BE    SETPFK4                                                          
*        CLC   =C'H9',AGENCY       OR STARCOM                                   
*        BE    SETPFK4                                                          
*        B     SETPFK10                                                         
*                                                                               
*ETPFK4  CLI   TWASCR,X'9F'        TEST DRLEGAL ACTIVE                          
*        BE    SETPFK6                                                          
*        MVC   0(6,R1),=C'2=LCOM'                                               
*        XC    3(3,R1),SPACES      MAKE LOWERCASE                               
*        LA    R1,7(R1)                                                         
*                                                                               
*ETPFK6  CLI   TWASCR,X'6F'        TEST DRBROAD ACTIVE                          
*        BE    SETPFK8                                                          
*        MVC   0(6,R1),=C'3=BCOM'                                               
*        XC    3(3,R1),SPACES                                                   
*        LA    R1,7(R1)                                                         
                                                                                
SETPFK8  CLI   TWASCR,X'F4'        TEST COMML SCREEN LOADED                     
         BE    SETPFK10                                                         
         MVC   0(7,R1),=C'4=COMML'                                              
         XC    3(4,R1),SPACES                                                   
         LA    R1,8(R1)                                                         
                                                                                
SETPFK10 MVC   0(9,R1),=C'5=CHGHIST'                                            
         XC    3(6,R1),SPACES                                                   
                                                                                
         LA    R1,10(R1)                                                        
         MVC   0(9,R1),=C'6=COMTEXT'                                            
         XC    3(6,R1),SPACES                                                   
                                                                                
         B     SETPFKX                                                          
                                                                                
SETPFKX  OI    6(R2),X'80'         TRANSMIT                                     
         NI    1(R2),X'FF'-X'04'   CHANGE INTENSITY                             
         J     EXIT                                                             
                                                                                
CLRPFK   SR    RF,RF               CLEAR PFKEY FIELD                            
         IC    RF,0(R2)                                                         
         AHI   RF,-9                                                            
         EX    RF,*+6                                                           
         BR    RE                                                               
         XC    8(0,R2),8(R2)                                                    
         DROP  R6                                                               
         LTORG                                                                  
*----------------------------------------------------------------*              
*                                                                               
*----------------------------------------------------------------*              
CHGTAB1  DS    0CL11                                                            
         DC    X'80',CL10'TITLE'                                                
         DC    X'40',CL10'PRDLIST'                                              
         DC    X'20',CL10'START DATE'                                           
         DC    X'10',CL10'END DATE  '                                           
         DC    X'08',CL10'UNIT LEN'                                             
         DC    X'04',CL10'CLASS'                                                
         DC    X'02',CL10'ADID'                                                 
         DC    X'01',CL10'TALENT OPT'                                           
CHGTAB1X EQU   *                                                                
CHGTAB1N EQU   (CHGTAB1X-CHGTAB1)/L'CHGTAB1                                     
                                                                                
CHGTAB2  DS    0CL11                                                            
         DC    X'80',CL10'MATCH TIME '                                          
         DC    X'40',CL10'MATCH DLY'                                            
         DC    X'20',CL10'MATCH DATE'                                           
         DC    X'10',CL10'TYPE       '                                          
         DC    X'08',CL10'CLT CMML'                                             
         DC    X'04',CL10'CMML COST'                                            
         DC    X'02',CL10'ACTUALS'                                              
         DC    X'01',CL10'       '                                              
CHGTAB2X EQU   *                                                                
CHGTAB2N EQU   (CHGTAB2X-CHGTAB2)/L'CHGTAB2                                     
                                                                                
CHGTAB3  DS    0CL11                                                            
         DC    X'80',CL10'PRNT CMML'                                            
         DC    X'40',CL10'HDEF CMML'                                            
         DC    X'20',CL10'CNTR CMML'                                            
         DC    X'10',CL10'PROD HSE '                                            
         DC    X'01',CL10'OTHER'                                                
CHGTAB3X EQU   *                                                                
CHGTAB3N EQU   (CHGTAB3X-CHGTAB3)/L'CHGTAB3                                     
                                                                                
ACDD     DSECT                     ACTIVITY DISPLAY SCREEN LINE                 
ACDHDR   DS    XL8                 FIELD HEADER                                 
ACDDATA  DS    CL70                                                             
ACDNEXT  EQU   *                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMLCLS                                                     
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAF4D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA9FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA6FD                                                       
         PRINT OFF                                                              
*        INCLUDE SPTRADFD                                                       
*        PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR22RR DS    F                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
FLDAH    DS    CL8                                                              
FLDA     DS    CL64                                                             
*                                                                               
SVDSKAD  DS    XL4                                                              
*                                                                               
PRDPTR   DS    F                                                                
PRDCTR   DS    CL1                                                              
PRDTYP   DS    CL1                                                              
RECCT    DS    H                                                                
*                                                                               
ADIDNO   DS    CL12                AD-ID NO                                     
ADIDNOP  DS    XL8                 AD-ID NO PACKED                              
ADIDYTM  DS    XL6                 DATE/TIME YMDHMS                             
OADIDNO  DS    CL12                PREVIOUS AD-ID NO                            
OADIDNOP DS    XL8                 PREVIOUS AD-ID NO PACKED                     
OADIDYTM DS    XL6                 DATE/TIME YMDHMS                             
CMLFLT   DS    CL12                SHOW SPECIFIC CML FILTER(HD,CC,ADID)         
ADIDFLT  DS    CL12                AD-ID FILTER                                 
ADIDXP   DS    XL8                 AD-ID NO PACKED                              
ADIDXLEN DS    XL1                 AD-ID LEN                                    
*                                                                               
SCANCT   DS    XL1                                                              
DATE     DS    CL6                                                              
HOLDSEQ  DS    XL3                                                              
HOLDCML  DS    CL8                                                              
HOLDCM12 DS    CL12                                                             
COMPKEY  DS    CL18                COMPARE KEY FOR ONLINE LIST                  
SAVEKEY  DS    XL18                                                             
COMPKEYL DS    CL1                                                              
SVATOAIR DS    CL1                 APPROVE TO AIR                               
CMLFILCT DS    PL3                                                              
CMLPBCT  DS    PL3                                                              
*                                                                               
SVCLTINT DS    CL2                 SAVED CCLTIFC FROM CLIENT HEADER             
*                                  FOR STARCOM FILE                             
SVNEXT   DS    F                                                                
*                                                                               
SVNETS   DS    CL53                4 NETWORK ELEMENTS+1 FOR END OF LIST         
*                                                                               
FILTERS  DS    0CL(HOLDSIGN+1-PRODFTR)                                          
PRODFTR  DS    CL3                 PRODFTR MUST BE FOLLOWED BY PRDFTR           
PRDFTR   DS    XL1                                                              
TYPEFTR  DS    CL4                                                              
SLNFTR   DS    CL1                                                              
RLDTFTR  DS    CL3                 RELEASE DATE FILTER                          
RLDTSFTR DS    CL1                                                              
RCDTFTR  DS    CL3                 RECALL DATE FILTER                           
RCDTSFTR DS    CL1                                                              
PGRPFTR  DS    CL4                 PGROUP FILTER FOR DISPLAY                    
PRGRFTR  DS    XL2                 PGROUP NUMBER                                
PGRLFTR  DS    XL120               PGROUP LIST                                  
FTRFLAG  DS    XL1                                                              
DELFTR   EQU   X'80'               NON-ZERO SHOW ONLY DELETED CML'S             
FILFTR   EQU   X'40'               CREATE FILE FROM CMMLS - STAR-COM            
SEQFTR   EQU   X'10'               SHOW COMML SEQ NO                            
CMLFTR   EQU   X'08'               SHOW SPECIFIC CML (HDEF/CNTR,ADID)           
ADIDFTR  EQU   X'04'               SHOW COMMLS WITH ADID                        
CCUTFTR  EQU   X'02'               SHOW COMMLS WITH CENTERCUT                   
HDEFFTR  EQU   X'01'               SHOW HIDEF IN LIST OVER TITLE                
*                                                                               
HOLDSIGN DS    CL1                                                              
*                                                                               
SVPRDLST DS    CL256                                                            
ACTCT    DS    X                   COUNT OF ACTUAL CMMLS USED                   
ACTSLN   DS    CL1                                                              
OLDACTS  DS    (NUMACTS)CL12                                                    
NEWACTS  DS    (NUMACTS)CL12                                                    
NUMACTS  EQU   4                   NUMBER OF ACTUALS ALLOWED FOR COVER          
MAXNETS  EQU   4                   MAX CML NETWORKS                             
CHGFLAGS DS    0XL3                FLAGS FOR CHANGES HISTORY                    
CHGFLAG1 DS    XL1                                                              
CHGFLAG2 DS    XL1                                                              
CHGFLAG3 DS    XL1                                                              
CHKCMML  DS    XL1                 Y/N FLAG FOR ROUTINE                         
DSTRYDAT DS    XL3                 SAVE DESTROY DATE                            
DSTRYTIM DS    XL2                 SAVE DESTROY TIME                            
SVCMLSTR DS    XL3                 SAVE CML START DATE                          
SVDSKADR DS    XL4                 SAVE DISK ADDR OF COMML REC                  
SVHIDEF  DS    CL12                ORIGINAL HIDEF CHAR                          
SVHIDEFX DS    XL8                 ORIGINAL HIDEF HEX                           
NWHIDEFX DS    XL8                 NEW HIDEF HEX                                
SVCNTCT  DS    CL12                ORIGINAL CENTERCUT CHAR                      
SVCNTCTX DS    XL8                 ORIGINAL CENTERCUT HEX                       
NWCNTCTX DS    XL8                 NEW CENTERCUT HEX                            
SVSWAP   DS    CL1                 HIDEF/CENTERCUT SWAP INDICATOR               
*                                                                               
SVPPKEY  DS    0C                                                               
SVHDDELK DS    XL13                                                             
SVHDADDK DS    XL13                                                             
SVCCDELK DS    XL13                                                             
SVCCADDK DS    XL13                                                             
SVADADDK DS    XL13                                                             
SVPPKEYL EQU   *-SVPPKEY                                                        
ELCODE2  DS    XL1                                                              
CHGELNUM EQU   ELCODE2                                                          
*                                                                               
FLAGS    DS    X                   VARIOUS FLAGS                                
SOFTDEL  EQU   X'80'               JUST DID A SOFT DELETE                       
SOFTREST EQU   X'40'               JUST DID A SOFT RESTORE                      
ISCOVCML EQU   X'20'               THIS IS A COVER CMML                         
ISCOVERD EQU   X'10'               THIS CMML IS COVERED                         
UNCOVER  EQU   X'08'               ACTION UNCOVER                               
NETWORKX EQU   X'04'               EXCLUDED NETWORKS                            
NETWSPEC EQU   X'02'               NETWORK SPECIFIC CML                         
NOTAPR   EQU   X'01'               CML NOT APPROVED FOR AIR                     
*                                                                               
FLAGS2   DS    X                   VARIOUS FLAGS                                
ADIDELP  EQU   X'80'               FOUND DELETED 0AC1 AD-ID NO                  
MATELEM  EQU   X'40'               ADDING MATCH ELEMENT (X'B0')                 
ALLCLTR  EQU   X'20'               ALL CLIENT REQUEST                           
*        EQU   X'10'                                                            
*        EQU   X'08'                                                            
*        EQU   X'04'                                                            
*        EQU   X'02'                                                            
*        EQU   X'01'                                                            
*                                                                               
PERVALST DS    XL56                                                             
*                                                                               
VTRPACK  DS    A                                                                
VQSORT   DS    V                                                                
ASORTTAB DS    A                                                                
ASORTEND DS    A                                                                
SVR1     DS    F                   SAVE R1                                      
LASTACT  DS    CL8                 LAST COVERED CMML                            
SVRLSE   DS    XL3                 SAVE RELEASE DATE                            
SVRCL    DS    XL3                 AND RECALL DATE                              
SVTODAY  DS    XL3                 SAVE TODAY'S DATE                            
IDATE    DS    XL3                 INACTIVE DATE                                
SVCODE   DS    D                   STATTION CODE                                
SVANET   DS    XL8                 SAVE APPROVED NETWORKS BITS                  
MYKEY    DS    XL13                                                             
*                                                                               
MORPRDEL DS    CL256                                                            
*                                                                               
TMPPSSV  DS    XL8                 SAVE HIDEF/CENTERCUT TO BE ADDED             
TMPKTYP  DS    XL2                 PASSIVE KEY TYPE TO BE ADDED                 
CMLDSKAD DS    XL4                                                              
         DS    0D                                                               
SORTTAB  DS    CL5000                                                           
         DS    0H                  BIG TROUBLE IF GREATER THAN 2F10             
*                                                                               
         EJECT                                                                  
* OFFLINE REPORT                                                                
*                                                                               
PRTLINE  DSECT                                                                  
PCML     DS    CL13                                                             
         DS    CL1                                                              
PSLN     DS    CL7                                                              
         DS    CL2                                                              
PTITLE   DS    CL15                                                             
         DS    CL2                                                              
PSOLO    DS    CL1                                                              
         DS    CL1                                                              
PRELSE   DS    CL8                                                              
         DS    CL1                                                              
PRECALL  DS    CL8                                                              
         DS    CL1                                                              
PTYPE    DS    CL4                                                              
         DS    CL1                                                              
PLIST    DS    CL24                                                             
         DS    CL1                                                              
PMISC    DS    CL26                                                             
*                                                                               
* ONLINE LIST                                                                   
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    C                                                                
LCML     DS    CL12                                                             
         DS    C                                                                
LLEN     DS    CL6                                                              
         DS    C                                                                
LTITLE   DS    CL15                                                             
         DS    C                                                                
LRELSE   DS    CL8                                                              
         DS    C                                                                
LRECALL  DS    CL8                                                              
         DS    CL2                                                              
LTYPE    DS    CL3                                                              
         DS    CL2                                                              
LCLASS   DS    CL4                                                              
         DS    CL1                                                              
LCOV     DS    CL1                 C=COVER, A=ACTUAL                            
         DS    CL1                                                              
LCOMTXT  DS    CL8                 COMTEXT                                      
         EJECT                                                                  
* DUMP FILE TABLE DSECT                                                         
*                                                                               
FILTSECT DSECT                                                                  
FILTFROM DS    A                   FIELD DISP FROM START OF EL OR KEY           
FILTTO   DS    A                   DISP OF OUTPUT IN RECORD                     
FILTRTN  DS    A                   ADDRESS OF ROUTINE TO MOVE DATA              
FILTEL   DS    X                   ELEM WITH DATA (ZERO = KEY)                  
FILFRLN  DS    X                   LENGTH OF FROM DATA-1                        
FILTOLN  DS    X                   LENGTH OF TO FIELD                           
         DS    X                   SPARE                                        
FILTNXT  EQU   *                                                                
*                                                                               
* OFFLINE DUMP TO FILE                                                          
*                                                                               
CMLFILE  DSECT                                                                  
FCML     DS    CL16   1- 16                                                     
FCLTCD   DS    CL2   17- 18                                                     
FCMLTITL DS    CL50  19- 68        ONLY 1ST 15 OR 24 USED                       
FCMLEN   DS    CL5   69- 73                                                     
FPRODNO  DS    CL6   74- 79                                                     
FPRODCD  DS    CL3   80- 82                                                     
FPRODSC  DS    CL15  83- 97                                                     
FSAGISCI DS    CL16  97-113                                                     
FAFMISCI DS    CL16 114-129                                                     
FCMLMED  DS    CL1  130                                                         
FPRODFMT DS    CL3  131-133        NOT AVAILABLE                                
FFRSTDTE DS    CL10 134-143        MM/DD/YYYY    FIRST FILM DATE                
FAPRVDTE DS    CL10 144-153        CLIENT APPROVAL DATE                         
FMPUDATE DS    CL10 154-163        MAX USE DATE                                 
FACTDATE DS    CL10 164-173        ACTIVE FILM DATE                             
FINACTDT DS    CL10 174-183        INACTIVE DATE                                
FINACTRS DS    CL25 184-208        INACTIVE REASON                              
FBRDAGY  DS    CL3  209-211        BRAND AGENCY                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'126SPTRA22   10/16/19'                                      
         END                                                                    
