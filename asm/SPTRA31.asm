*          DATA SET SPTRA31    AT LEVEL 036 AS OF 01/21/16                      
*PHASE T21631A                                                                  
         TITLE 'T21631-TRAFFIC CMML TXT REC. ADD,DEL,CHA,DIS,LIST PGM'          
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ CML SEQ RECORD FOR ADDS (IN PAR RTN)                  
*                    READ IN PRDHDR FOR PROD NAMES IN OFFLINE LIST              
*             AIO3 - CMML REC READ IN TO SET CMLSTAT FLAG                       
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 -                                                                   
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 -                                                                   
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
***********************************************************************         
*                                                                               
*  LEV  7-9  OCT07/87 FIX DELETING COMMLS BUG                                   
*  LEV 10    OCT12/87 FIX SET OFF FLAG IN COMML REC BUG                         
*  LEV 11-12 DEC14/87 UPDATE ALL AFFECTED PATTERNS                              
*  LEV 13-14 FEB11/88 SET PATTERN FLAG AND INSTR RECAP FLAG                     
*  LEV 15    FEB17/88 CHANGE SCREEN TO COMBINE MKT/STA                          
*  LEV 16    APR07/92 ADD PF KEY SUPPORT FOR ADD/DELETE LINE                    
*  LEV 17    AUG27/92 FIX PATTERN UPDATE BUG                          *         
*  LEV 18    DEC02/92 CHANGE FOR MSUNPK                               *         
*  LEV 19    FEB18/93 FIX DISPLAY KEY BUG                             *         
*  LEV 20    JUL28/93 ALLOW FOR NO INSTR RECAPS                       *         
*  LEV 21    OCT15/93 STOP ENTRY OF COMML CODE OF ALL 9'S             *         
*  LEV 22    NOV23/94 CHANGE PTNTABL SIZE FROM 100 TO 200             *         
*  LEV 23 SMUR APR15/98 FIX DEL CMML TEXT                             *         
*  LEV 24 BGRI OCT28/02 NEW INST RECAP RECS                           *         
*  LEV 26 SMUR JUL28/04 SOX                                           *         
*  LEV 29 SMUR JUL08/08 COMTEXT FOR NET                               *         
*  LEV 30 SMUR SEP18/08 COMTEXT FOR NET ON XSPOT FILE                 *         
*  LEV 31 SMUR DEC04/08 BYPASS INCOMPLETE PATTERN RECORDS             *         
*      34 MHER MAY/09 - ADID SUPPORT                                  *         
*  LEV 36 JBAS JAN03/16 ENABLE GLOBBER CALL FOR COMTEXT               *         
***********************************************************************         
         EJECT                                                                  
         PRINT NOGEN                                                            
T21631   CSECT                                                                  
         NMOD1 0,**1631**,R7                                                    
*                                                                               
         L     RC,0(R1)            RETRV GENCON WORK POINTER                    
         USING GEND,RC             ESTAB.ADDR'BLTY TO GENCON WORK               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8           ESTAB.ADDR'BLTY TO SPOOL PRINT AREAS         
         L     RA,ATWA             RETRV TWA ADDRESS                            
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD            RETRV ADDR.OF USER SYSTEM VARIABLES          
         USING SYSD,R9             ESTAB.ADDR'BLTY TO THEM                      
*                                                                               
         OI    CONSERVH+1,X'01'     FORCE MODIFIED SO PFKEYS WORK               
         OI    CONSERVH+6,X'80'     AND XMT                                     
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         TM    GENSTAT6,GES$UPLD   IF NOT UPLOADING THROUGH DDLINK              
         BO    MAIN00                                                           
         CLI   TWASCR,X'E1'        AND ON MAINTENANCE SCREEN                    
         BNE   MAIN00                                                           
         OI    TRACKSH+1,X'2C'     MAKE CHECKSUM FIELD PROTECTED,               
         OI    TRACKSH+6,X'80'     INVISIBLE AND TRANSMITTED                    
*                                                                               
MAIN00   CLI   MODE,VALKEY         VALIDATE RECORD KEY ?                        
         BE    VK                  YES-PROCESS KEY VALIDATION                   
         CLI   MODE,VALREC         VALIDATE RECORD DETAIL ?                     
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD KEY ?                         
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD DETAIL                        
         BE    DR                                                               
         CLI   MODE,XRECADD        AFTER ADD REC                                
         BE    AAR                                                              
         CLI   MODE,XRECDEL        AFTER DEL REC                                
         BE    ADR                                                              
         CLI   MODE,XRECREST       AFTER RESTORE REC                            
         BE    AAR                                                              
         CLI   MODE,RECPUT         AT PUT REC TIME (AFTER VALIDATE REC)         
         BE    PUT                                                              
         CLI   MODE,XRECPUT        AFTER PUT REC                                
         BE    APR                                                              
         CLI   MODE,LISTRECS       LIST RECORDS ?                               
         BE    LREC                                                             
         CLI   MODE,PRINTREP       PRINT OFFLINE REPORT ?                       
         BE    LREC                                                             
         CLI   MODE,SETFILE        SET FILE FOR XFILE                           
         BNE   MAIN10                                                           
         CLI   SPOTNETF,C'N'       FOR NET ONLY                                 
         BNE   EXIT                                                             
         BRAS  RE,SFX                                                           
         B     EXIT                                                             
*                                                                               
MAIN10   CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
EXIT     EQU   *                                                                
         XIT1                                                                   
*-------------------------------                                                
*  SET FILE TO XSPOT FILE                                                       
*-------------------------------                                                
SFX      DS    0H                                                               
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=CL3'XSP'                                              
         MVC   SYSFIL(3),=CL3'XSP'                                              
         BR    RE                                                               
*-------------------------------                                                
*  SET FILE TO TRAFFIC FILE                                                     
*-------------------------------                                                
SFT      DS    0H                                                               
         MVI   DATADISP+1,24                                                    
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         MVC   SYSDIR(3),=CL3'TRF'                                              
         MVC   SYSFIL(3),=CL3'TRF'                                              
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE KEY ROUTINE *                                                        
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
*                                                                               
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          POINT TO MEDIA HEADER FIELD                  
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          POINT TO CLIENT HEADER FIELD                 
         GOTO1 VALICLT                                                          
*                                                                               
         MVI   BPRD,0              INITIALIZE BINARY PRODUCT CODE               
         XC    QPRD,QPRD           INITIALIZE PRODUCT MNEMONIC                  
         LA    R2,TRAPRDH          POINT TO PRODUCT HEADER FIELD                
         CLI   5(R2),0             WAS PRODUCT ENTERED ?                        
         BE    VPRDX               EXIT PRODUCT PROCESSING                      
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
*                                                                               
VPRDX    EQU   *                                                                
*                                                                               
         CLI   SPOTNETF,C'N'       IS THIS NET                                  
         BNE   VK04                                                             
         XC    NETWRK,NETWRK                                                    
         LA    R2,TRAMSH                                                        
         CLI   5(R2),0             WAS NETWORK ENTERED ?                        
         BE    VK12                                                             
         BAS   RE,VNET                                                          
         B     VK12                                                             
*                                                                               
VK04     XC    BMKTSTA,BMKTSTA                                                  
         XC    QMKT,QMKT                                                        
         XC    QSTA,QSTA                                                        
         LA    R2,TRAMSH                                                        
         CLI   5(R2),0             WAS MKT/STA ENTERED ?                        
         BE    VK12                EXIT MKT/STA PROCESSING                      
         CLI   5(R2),4             MORE THAN 4 MUST BE STA                      
         BH    VK10                                                             
         MVC   WORK(4),=C'0000'                                                 
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVN   WORK(0),8(R2)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),8(R2)                                                    
         BNE   VK10                                                             
         GOTO1 VALIMKT                                                          
         B     VK12                                                             
*                                                                               
VK10     GOTO1 VALISTA                                                          
*                                                                               
VK12     EQU   *                                                                
*                                                                               
VK20     EQU   *                   (REQUIRED)                                   
         LA    R2,TRACMLH          POINT TO CML TXT HEADER FIELD                
         CLI   5(R2),0             WAS FIELD ENTERED ?                          
         BNE   VK22                YES-READ FOR CMML PROFILE                    
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VK50                                                             
         B     MISSERR             MUST INPUT CMML                              
                                                                                
* READ FOR CML PROF RECORD                                                      
                                                                                
VK22     BRAS  RE,VCML                                                          
*                                                                               
         BAS   RE,SFT              SET FILE TO SPOT                             
*                                                                               
         LA    R4,KEY                                                           
         USING CMLRECD,R4                                                       
*                                                                               
         XC    CMLKEY,CMLKEY       CLEAR WORK KEY                               
         MVC   CMLKID,=X'0A21'     CML PROF RECORD ID                           
         MVC   CMLKAM,BAGYMD       BINARY AGENCY/MEDIA                          
         MVC   CMLKCLT,BCLT        BINARY CLIENT CODE                           
         MVC   CMLKCML,SVCMLP      PACKED ADID OR ISCI                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   CMLKEY,KEYSAVE      DESIRED KEY FOUND ?                          
         BNE   NOCMLERR             NO COMMERCIAL                               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK30                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED COMML                                
         BO    DELCMLER                                                         
                                                                                
* VALIDATE PROD IN COMMERCIAL IF ENTERED *                                      
                                                                                
VK30     CLI   BPRD,0              WAS PRODUCT ENTERED ?                        
         BE    VK40                 NO-PROCESS CML DATA ELEM                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6         ESTAB.ADDR'BLTY TO PRD LIST ELM              
         CLI   CMLPRDS,X'FF'       ALL PRODUCTS                                 
         BE    VK40                                                             
         SR    RE,RE                                                            
         IC    RE,CMLPRDLN         ELEMENT LENGTH                               
         LA    RF,0(R6,RE)         GET END ADDRESS                              
         LA    RE,1                                                             
         LA    R6,2(,R6)                                                        
         BCTR  RF,0                LESS 1 FOR BXLE                              
*                                                                               
VK32     CLC   BPRD,0(R6)          DESIRED PRODUCT FOUND ?                      
         BE    VK40                 YES-PROCESS CML DATA ELEM                   
         BXLE  R6,RE,VK32          BXLE THRU LIST 'TILL DONE                    
         B     PRDNCML             ERROR,PRODUCT NOT IN CML REC                 
*                                                                               
VK40     L     R6,AIO                                                           
         MVI   ADIDFLAG,C'Y'                                                    
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   BSEQ,CMLSEQ+(L'CMLSEQ-L'BSEQ) SAVE CML SEQ CD FOR GENCON         
         MVC   SVCMLT,CMLTITLE     COMMERCIAL TITLE                             
         MVC   SVCMLEN,CMLSLN      COMMERCIAL LENGTH                            
*                                                                               
         MVI   ELCODE,X'20'                                                     
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   *+8                                                              
         MVI   ELCODE,X'29'                                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLPRDEL,R6         ESTAB.ADDR'BLTY TO PRD LIST ELM              
         MVC   SVK30D,1(R6)                                                     
         ZIC   R0,SVK30D                                                        
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         STC   R0,SVK30D                                                        
*                                                                               
VK50     EQU   *                                                                
         DROP  R4,R6                                                            
*                                                                               
         LA    R2,TRAFLTH          POINT TO FILTERS HEADER FIELD                
         BAS   RE,VFTR             VALIDATE FILTERS                             
*                                                                               
         LA    R4,KEY              POINT TOP KEY WORK                           
         USING CMTRECD,R4                                                       
         XC    CMTKEY,CMTKEY       CLEAR KEY WORK                               
         MVC   CMTKID,=X'0A35'     RECORD ID                                    
         MVC   CMTKAM,BAGYMD       BINARY AGY/MED                               
         MVC   CMTKCLT,BCLT        CLIENT CODE                                  
         CLI   SPOTNETF,C'N'       NET TRAFFIC?                                 
         BE    VK52                                                             
         MVC   CMTKPRD,BPRD        BINARY PRODUCT CODE                          
         MVC   CMTKMKT,BMKT        BINARY MARKEY NMBR                           
         MVC   CMTKSTA,BSTA        BINARY STATION                               
         MVC   CMTKSEQ,BSEQ        BINARY CML SEQUENCE NMBR                     
         B     VK54                                                             
*                                                                               
VK52     DS    0H                                                               
         MVC   CMXKPROD,QPRD                                                    
         MVC   CMXKNET,NETWRK                                                   
         MVC   CMXKCML,SVCMLP      COMMERCIAL                                   
*                                                                               
         BAS   RE,SFX              SET TO XSPOT FILE                            
*                                                                               
         MVC   COMPKEYN,KEY        COMTEXT FOR NET KEY                          
         B     *+10                                                             
VK54     MVC   COMPKEY,KEY                                                      
         B     EXIT                RETURN TO GENCON                             
         DROP  R4                  RELEASE R4                                   
         EJECT                                                                  
*===========================================                                    
* VALIDATE RECORD ROUTINE *                                                     
*===========================================                                    
                                                                                
VR       DS    0H                  FIRST VALIDATE PF KEY HIT                    
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VRS01    DS    0H                                                               
*                                                                               
VRPFK    CLI   PFKEY,3             ERASE LINE?                                  
         BE    VRPFK5                                                           
         CLI   PFKEY,4             ADD LINE?                                    
         BE    VRPFK5                                                           
         CLI   PFKEY,0             ANY OTHER PFKEY IS INVALID                   
         BNE   INVPFKY                                                          
         B     VR06                ENTER KEY HIT, GO VALIDATE INPUT             
*                                                                               
VRPFK5   L     R5,ATIOB            A(TIOB)                                      
         USING TIOBD,R5                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R5                                                               
*                                                                               
         LA    R2,TRAL01H          1ST FIELD WHICH COULD CONTAIN CURSOR         
VRPFK10  SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               IS CURSOR ON 1ST TEXT LINE?                  
         BH    INVPOSN             NO -CURSOR IS ABOVE 1ST TEXT LINE            
         CR    RE,R1               END OF LINE ADDR, CURSOR ADDR                
         BNL   VRPFK20             BR IF NCURSOR IS BEFORE LINE END             
*                                                                               
         LA    RF,TRAL02H-TRAL01H  BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,TRAL09H                                                       
         CR    R2,RF               END OF SCREEN?                               
         BH    INVPOSN             YES- INVALID POSITION FOR PF3/4              
         B     VRPFK10                                                          
*                                                                               
VRPFK20  LA    RF,TRAL09H          A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VRPFK50             NO                                           
*                                                                               
         LA    R0,TRAL01H                                                       
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BL    VR06                YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURFORC         KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VRPFK30  CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VRPFK40             YES                                          
         LA    R0,TRAL02H-TRAL01H  LENGTH OF FIELD                              
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         MVC   1(1,R2),1(R3)       MOVE ATTRIBUTE BYTE                          
         XC    4(2,R3),4(R3)       CLEAR LINE BELOW                             
         NI    1(R3),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         OI    6(R2),X'80'         TRANSMIT                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST TEXT FIELD                        
         LR    R2,R3                                                            
         B     VRPFK30                                                          
*                                                                               
VRPFK40  XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         NI    1(R2),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR06                                                             
*                                                                               
VRPFK50  CLI   PFKEY,4             ADD LINE?                                    
         BNE   VRPFK80             NO                                           
*                                                                               
         CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR06                YES                                          
*                                                                               
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,TRAL09H          LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VRPFK60  LA    R0,TRAL02H-TRAL01H  DISTANCE TO NEXT LINE                        
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VRPFK70             YES                                          
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         OI    6(R3),X'80'         TRANSMIT                                     
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         MVC   1(1,R3),1(R2)       MOVE ATTRIBUTE BYTE                          
         LR    R3,R2                                                            
         B     VRPFK60                                                          
*                                                                               
VRPFK70  XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         NI    1(R3),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURFORC         KEEP CURSOR IN PLACE                         
         B     VR06                                                             
*                                                                               
VRPFK80  TM    1(R2),X'08'         IS LINE CURRENTLY HIGH INTENSITY?            
         BZ    *+12                                                             
         NI    1(R2),X'FF'-X'0C'   YES -- FORCE NORMAL INTENSITY                
         B     *+8                                                              
         OI    1(R2),X'08'         NO -- FORCE HIGH INTENSITY                   
         EJECT                                                                  
*                                                                               
* ----HERE BEGINS THE NON-FUNCTION KEY VALIDATION FOR RECORD----                
* 1                                                                             
VR06     L     R4,AIO                                                           
*                                                                               
         USING CMXKEY,R4                                                        
         TM    GENSTAT6,GES$UPLD   IF UPLOADING THROUGH DDLINK                  
         BZ    VR07                                                             
         CLI   SPOTNETF,C'N'       AND IN NET TRAFFIC                           
         BNE   VR07                                                             
         CLI   ACTNUM,ACTADD       AND ACTION IS NOT ADD                        
         BE    VR07                                                             
         CLI   TRACKSH+5,0         CHECK SUM IS REQUIRED                        
         BE    VR06A                                                            
         GOTO1 HEXIN,DMCB,TRACKS,FULL,L'TRACKS                                  
         LR    RE,R4                                                            
         ZICM  RF,CMXKEY+L'CMXKEY,2                                             
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *+4                                                              
         C     R0,FULL             AND MUST MATCH RECORD                        
         JE    VR07                                                             
VR06A    LHI   R0,CMLCHKSUM                                                     
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
         DROP  R4                                                               
*                                                                               
VR07     MVI   UPDRECSW,C'N'       SET OFF SW TO UPDATE PATTERN RECORDS         
         USING CMTKEY,R4                                                        
         CLC   BAGYMD,CMTKAM                                                    
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLC   BCLT,CMTKCLT        IS CLIENT SAME                               
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLI   SPOTNETF,C'N'                                                    
         BNE   *+14                                                             
         MVC   CMXAGYA,AGENCY                                                   
         B     *+10                                                             
         MVC   CMTAGYA,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
* GET ANY TEXT LINES                                                            
*                                                                               
         MVI   ELCODE,X'40'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R3,1                SET TEXT LINE NUMBER                         
         LR    R5,R3               SET BLK LINE CTR                             
         LA    R6,ELEM                                                          
         USING CMTTXTEL,R6                                                      
         MVI   CMTTXTEL,X'40'                                                   
         LA    R2,TRAL01H          FIRST TEXT LINE                              
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
VR10     CLI   0(R1),C' '          IF BLANK                                     
         BH    VR14                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,VR10             CK ENTIRE FLD                                
VR14     STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
*                                                                               
VR16     GOTO1 ANY                                                              
*                                                                               
         CLI   5(R2),58            MAX LENGTH                                   
         BH    CMTLENER            TOO LONG                                     
         ZIC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)           TOTAL ELEMENT LENGTH                         
         STC   R0,CMTTXTLN         AND LENGTH                                   
         STC   R3,CMTLNNUM         TEXT LINE NUMBER                             
         BCTR  R1,0                                                             
         EX    R1,VRMVC                                                         
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VR20     LA    R5,1(,R5)           INCREMENT BLANK LINE CTR                     
         ZIC   R1,0(R2)            GET THIS SCREEN FIELD LENGTH                 
         AR    R2,R1               ADD TO FIELD POINTER                         
         ZIC   R1,0(R2)            ALSO BYPASS PROTECTED FIELD                  
         AR    R2,R1               ADD TO FIELD POINTER                         
         LA    R1,TRATAGH          END OF SCREEN                                
         CR    R1,R2               IF THERE                                     
         BNH   VR30                GET OUT                                      
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
VR22     CLI   0(R1),C' '          IF BLANK                                     
         BH    VR24                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,VR22             CK ENTIRE FLD                                
VR24     STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR20                NO                                           
VR26     CR    R3,R5               CK LAST LINE VS THIS LINE                    
         BE    VR16                NO INTERVENING BLANK LINES                   
         MVI   CMTTXTLN,4          ONLY 1 BLANK NEEDED FOR BLK LINE             
         STC   R3,CMTLNNUM         STORE TEXT LINE NUMBER                       
         MVI   CMTTXT,C' '         AND HERE IT IS                               
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           ADD TO TEXT LINE CT                          
         B     VR26                BUILD ALL BLK LINES NEEDED                   
VR30     EQU   *                                                                
         CH    R3,=H'1'                                                         
         BH    DR                                                               
         LA    R2,TRAL01H                                                       
         B     MISSERR             NOW DISPLAY VALIDATED RECORD                 
VRMVC    MVC   CMTTXT,WORK                                                      
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD *                                                              
*                                                                               
DR       LA    R2,TRAL01H                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING CMTTXTEL,R6                                                      
DR10     ZIC   R1,CMTTXTLN                                                      
         SH    R1,=H'4'            GET TXT LEN-1                                
         MVC   WORK(L'TRAL01),SPACES                                            
         EX    R1,DRMVC                                                         
         CLC   8(L'TRAL01,R2),WORK TEXT LINE                                    
         BE    *+14                                                             
         MVC   8(L'TRAL01,R2),WORK                                              
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
*                                                                               
         LA    R1,TRATAG                                                        
         CR    R2,R1                                                            
         BNL   EXIT                                                             
*                                                                               
* BLANK OUT REST OF SCREEN *                                                    
*                                                                               
         MVC   WORK(L'TRAL01),SPACES                                            
DR20     CLC   8(L'TRAL01,R2),WORK TEXT LINE                                    
         BE    *+14                                                             
         MVC   8(L'TRAL01,R2),WORK                                              
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R1,TRATAG                                                        
         CR    R2,R1                                                            
         BL    DR20                                                             
*                                                                               
         B     EXIT                                                             
DRMVC    MVC   WORK(0),CMTTXT                                                   
         EJECT                                                                  
*  DISPLAY KEY ROUTINE *                                                        
*                                                                               
DK       LA    R4,KEY                                                           
         USING CMTRECD,R4          ESTAB.ADDR'BLTY TO CML TXT REC               
                                                                                
         CLI   SPOTNETF,C'N'       NET TRAFFIC?                                 
         BE    DK02                                                             
                                                                                
         MVC   BPRD,CMTKPRD                                                     
         MVC   BMKTSTA,CMTKMKT                                                  
DK02     XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         MVC   TRAMED,WORK                                                      
         OI    TRAMEDH+6,X'80'                                                  
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
         XC    WORK(L'TRAPRD),WORK                                              
                                                                                
         CLI   CMTKPRD,0           IS PRODUCT PRESENT ?                         
         BE    DK20                 NO                                          
*                                                                               
         CLI   SPOTNETF,C'N'       NET TRAFFIC?                                 
         BNE   *+14                                                             
         MVC   WORK(L'CMXKPROD),CMXKPROD    3 CHAR PROD                         
         B     DK20                                                             
*                                                                               
         L     R1,ASVCLIST         POINT TO CLIST SAVE                          
         USING CLISTDS,R1          ESTAB. ADDR'BLTY TO IT                       
         LA    RE,CLISTENT         GET CLIST ENTRY LENGTH                       
         LA    RF,880-1(R1)        POINT TO SVCLIST END                         
DK10     CLC   CLISBPRD,CMTKPRD    PRODUCT CODES EQ ?                           
         BE    DK16                 YES-PRODUCT FOUND                           
         CLI   CLISQPRD,C' '       END OF LIST ?                                
         BE    *+8                  YES-CRASH                                   
         BXLE  R1,RE,DK10          BXLE THRU LIST 'TILL FOUND                   
         LA    R1,=C'???'          UNKNOWN PRODUCT                              
         B     *+10                                                             
DK16     MVC   WORK(L'CLISQPRD),0(R1)   PUT PRD MNEMONIC IN SCRN                
*                                                                               
DK20     MVC   TRAPRD,WORK                                                      
         OI    TRAPRDH+6,X'80'     TRANSMIT FIELD                               
         DROP  R1                  RELEASE R1                                   
         EJECT                                                                  
         XC    WORK(L'TRAMS),WORK                                               
         LA    R2,TRAMSH                                                        
         CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
         BE    DK48                                                             
         OC    CMTKMKT,CMTKMKT     IS MKT IN THIS KEY ?                         
         BZ    DK30                 NO, BYPASS MSUNPK                           
*                                                                               
         GOTO1 MSUNPK,DMCB,CMTKMKT,WORK,DUB                                     
         MVC   PRNTSTA,DUB                                                      
         OC    CMTKSTA,CMTKSTA     IS STA IS THIS KEY ?                         
         BNZ   DK40                 NO-EXIT MKT/STA DISPLAY                     
*                                                                               
DK30     CLC   TRAMS,WORK          ARE THEY THE SAME                            
         BE    *+14                                                             
         MVC   TRAMS,WORK          MOV TO SCREEN                                
         OI    TRAMSH+6,X'80'      TRANSMIT FIELD                               
*                                                                               
         B     DK50                                                             
*                                                                               
DK40     BAS   RE,FSTA             FORMAT STATION FOR PRINTING                  
         MVC   WORK(6),PRNTSTA                                                  
*                                                                               
         CLC   TRAMS,WORK          ARE THEY THE SAME                            
         BE    *+14                                                             
         MVC   TRAMS,WORK          MOV TO SCREEN                                
         OI    TRAMSH+6,X'80'                                                   
         B     DK50                                                             
*                                                                               
DK48     MVC   TRAMS(4),CMXKNET    MOVE NETWORK TO SCREEN                       
         OI    TRAMSH+6,X'80'      TRANSMIT FIELD                               
*                                                                               
         MVC   WORK(L'CMXKCML),CMXKCML 8 CHAR ISCII                             
         B     *+10                                                             
DK50     MVC   BSEQ,CMTKSEQ                                                     
*                                                                               
         BAS   RE,SFT              SET FILE TO SPOT                             
         LA    R4,KEY              POINT TOP KEY WORK                           
         MVC   MYSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
         USING CMLRECD,R4                                                       
*                                                                               
         XC    CMLKEY,CMLKEY       CLEAR KEY WORK                               
         MVC   CMLKID,=X'0AA1'     RECORD ID                                    
         MVC   CMLKAM,BAGYMD       BINARY AGY/MED                               
         MVC   CMLKCLT,BCLT        CLIENT CODE                                  
         MVC   CMLKCML+1(2),BSEQ                                                
*                                                                               
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   DK50F                                                            
         MVI   CMLKID+1,X'21'      GET 0A21 RECORD ID                           
         MVC   CMLKCML,WORK        8 CHAR ISCII                                 
*                                                                               
DK50F    GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         XC    TRACML,TRACML                                                    
         MVC   TRACML(8),CMLKCML-CMLKEY(R6)                                     
         OI    TRACMLH+6,X'80'                                                  
*                                                                               
         TM    15(R6),X'01'        TEST CMML PACKED                             
         BZ    DK52                                                             
         GOTO1 VTRPACK,DMCB,(C'U',TRACML),TRACML                                
*                                                                               
DK52     CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   *+12                                                             
         MVI   ELCODE,X'29'                                                     
         B     *+8                                                              
         MVI   ELCODE,X'20'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVK30D,1(R6)        SAVE PRODUCT LIST                            
         ZIC   R0,SVK30D                                                        
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         STC   R0,SVK30D                                                        
*                                                                               
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+8                                                              
         BAS   RE,SFX              SET TO XSPOT FILE                            
*                                                                               
         MVC   KEY,MYSVKEY                                                      
         GOTO1 HIGH                RE-READ COMTEXT RECORD                       
*                                                                               
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+14                                                             
         CLC   KEY(32),KEYSAVE                                                  
         B     *+10                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         B     EXIT                RETURN TO GENCON                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LREC     LA    R4,KEY                                                           
         USING CMTKEY,R4                                                        
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+14                                                             
         OC    KEY(32),KEY                                                      
         B     *+10                                                             
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LREC08              NO, GOTO HIGH                                
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             POINT TO HEADLINE HIIK ROUTINE               
         ST    R1,HEADHOOK         STORE IN DDSPOOLD FOR GENCON                 
                                                                                
* BUILD KEY, AND DO READHI                                                      
                                                                                
         MVC   CMTKID,=X'0A35'     RECORD ID                                    
         MVC   CMTKAM,BAGYMD       A-M                                          
         MVC   CMTKCLT,BCLT        CLT                                          
         MVC   CMTKPRD,BPRD        BPRD                                         
*                                                                               
LREC08   CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   LREC10                                                           
         MVC   CMTKPRD(3),QPRD     3 CHAR PROD                                  
         BAS   RE,SFX                                                           
*                                                                               
LREC10   GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      SAME TY/A-M/CLT                              
         BE    LREC22                                                           
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         MVC   P(32),=CL32'NO COMMERCIAL TEXT RECORDS FOUND'                    
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
*                                                                               
LREC20   CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+8                                                              
         BAS   RE,SFX              SET TO XSPOT FILE                            
         GOTO1 SEQ                 DO READ SEQUENTIAL                           
         LA    R4,KEY                                                           
*                                                                               
LREC22   CLC   KEY(5),COMPKEY      AT END OF THIS A-M/CLT                       
         BNE   EXIT                YES                                          
*                                                                               
LREC24   CLI   COMPKPRD,0                                                       
         BE    *+14                                                             
         CLC   CMTKPRD,COMPKPRD                                                 
         BNE   LREC20                                                           
*                                                                               
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   LREC28                                                           
                                                                                
         OC    COMPKPRO,COMPKPRO                                                
         BZ    *+14                                                             
         CLC   CMXKPROD,COMPKPRO                                                
         BNE   LREC20                                                           
                                                                                
         OC    COMPKNET,COMPKNET                                                
         BZ    LREC26                                                           
         CLC   CMXKNET,COMPKNET                                                 
         BNE   LREC20                                                           
*                                                                               
LREC26   MVC   SVCMLP,CMXKCML       8 CHAR CMML                                 
         MVC   MYSVKEY,KEY                                                      
*                                                                               
         USING CMLRECD,R4                                                       
         XC    CMLKEY,CMLKEY       CLEAR WORK KEY                               
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,SVCMLP                                                   
         B     LREC32                                                           
         DROP  R4                                                               
*                                                                               
         USING CMTRECD,R4                                                       
LREC28   OC    COMPKMKT,COMPKMKT                                                
         BZ    *+14                                                             
         CLC   CMTKMKT,COMPKMKT                                                 
         BNE   LREC20                                                           
*                                                                               
         OC    COMPKSTA,COMPKSTA                                                
         BZ    *+14                                                             
         CLC   CMTKSTA,COMPKSTA                                                 
         BNE   LREC20                                                           
*                                                                               
LREC30   MVC   BSEQ,CMTKSEQ                                                     
         MVC   MYSVKEY,KEY                                                      
*                                                                               
         USING CMLRECD,R4                                                       
         XC    CMLKEY,CMLKEY                                                    
         MVC   CMLKID,=X'0AA1'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML+1(2),BSEQ                                                
*                                                                               
LREC32   BAS   RE,SFT              SET FILE TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   CMLKEY,KEYSAVE      DESIRED KEY FOUND ?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING CMLKEY,R4                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   SVCMLT,CMLTITLE                                                  
         MVC   SVCML12,SPACES                                                   
         MVC   SVCML12(8),CMLKCML                                               
         TM    CMLRSTAT,CMLKSTA_PCKD   TEST CMML PACKED                         
         BZ    LREC34                                                           
         GOTO1 VTRPACK,DMCB,(C'U',CMLKCML),SVCML12                              
*                                                                               
LREC34   MVC   KEY,MYSVKEY                                                      
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+8                                                              
         BAS   RE,SFX              SET TO XSPOT FILE                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         LA    RE,32-1                                                          
         CLI   SPOTNETF,C'N'                                                    
         BE    *+8                                                              
         LA    RE,13-1                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LREC36   GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING CMTKEY,R4                                                        
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMTTXTEL,R6                                                      
*                                                                               
         BAS   RE,FTR                FILTER RECORDS                             
         BNE   LREC20                                                           
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
*                                                                               
LRL      MVC   LISTAR,SPACES                                                    
         MVC   LCLT,QCLT                                                        
*                                                                               
         CLI   CMTKPRD,0                                                        
         BE    LRL40                                                            
                                                                                
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   *+14                                                             
         MVC   LPROD(L'CMXKPROD),CMXKPROD 3 CHAR PROD FOR NET                   
         B     LRL40                                                            
                                                                                
         L     R1,ASVCLIST         POINT TO CLIST SAVE                          
         LA    R0,220              MAX ENTRIES                                  
         USING CLISTDS,R1          ESTAB. ADDR'BLTY TO IT                       
*                                                                               
LRL10    CLC   CLISBPRD,CMTKPRD    PRODUCT CODES EQ ?                           
         BE    LRL20                                                            
         CLI   CLISQPRD,C' '       END OF LIST ?                                
         BNH   *+12                YES-CRASH                                    
         LA    R1,4(R1)                                                         
         BCT   R0,LRL10                                                         
         LA    R1,=C'???'          UNKNOWN PRODUCT                              
*                                                                               
LRL20    MVC   LPROD(L'CLISQPRD),0(R1) PUT PRD MNEMONIC IN SCRN                 
*                                                                               
LRL24    OC    CMTKMKT,CMTKMKT                                                  
         BZ    LRL30                                                            
         SR    R0,R0                                                            
         ICM   R0,3,CMTKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LMKT,DUB                                                         
LRL30    OC    CMTKSTA,CMTKSTA                                                  
         BZ    LRL40                                                            
         GOTO1 MSUNPK,DMCB,CMTKMKT,WORK,DUB                                     
         MVC   PRNTSTA,DUB                                                      
         BAS   RE,FSTA                                                          
         MVC   LSTA,PRNTSTA                                                     
         B     LRL50                                                            
LRL40    EQU   *                                                                
         OC    CMXKNET,CMXKNET                                                  
         BZ    *+10                                                             
         MVC   LNET,CMXKNET                                                     
*                                                                               
LRL50    MVC   LCML,SVCML12                                                     
         SLR   R1,R1                                                            
         IC    R1,CMTTXTLN                                                      
         AHI   R1,-4                                                            
*                                                                               
         CHI   R1,L'LTEXT-1                                                     
         BNH   *+8                                                              
         LHI   R1,L'LTEXT-1                                                     
*                                                                               
         EX    R1,LRLMVC                                                        
*                                                                               
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LREC20                                                           
*                                                                               
LRLMVC   MVC   LTEXT(0),CMTTXT                                                  
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
*                                                                               
LRR      MVC   P,SPACES                                                         
         MVC   PCML,SVCML12                                                     
         MVC   PCMLTI,SVCMLT                                                    
         CLI   SPOTNETF,C'N'       NET TRAFFIC?                                 
         BE    LRR36                                                            
                                                                                
         L     R1,ASVCLIST         POINT TO CLIST SAVE                          
         USING CLISTDS,R1          ESTAB. ADDR'BLTY TO IT                       
         LA    RE,CLISTENT         GET CLIST ENTRY LENGTH                       
         LA    RF,880-1(R1)        POINT TO SVCLIST END                         
LRR10    CLC   CLISBPRD,CMTKPRD    PRODUCT CODES EQ ?                           
         BE    LRR20                                                            
         CLI   CLISQPRD,C' '       END OF LIST ?                                
         BE    *+8                 YES-CRASH                                    
         BXLE  R1,RE,LRR10         BXLE THRU LIST 'TILL FOUND                   
         LA    R1,=C'???'                                                       
*        B     *+10                                                             
LRR20    MVC   PPROD(L'CLISQPRD),0(R1)  PUT PRD MNEMONIC IN SCRN                
*                                                                               
LRR24    OC    CMTKMKT,CMTKMKT                                                  
         BZ    LRR30                                                            
         SR    R0,R0                                                            
         ICM   R0,3,CMTKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
*                                                                               
LRR30    OC    CMTKSTA,CMTKSTA                                                  
         BZ    LRR40                                                            
         GOTO1 MSUNPK,DMCB,CMTKMKT,WORK,DUB                                     
         MVC   PRNTSTA,DUB                                                      
         BAS   RE,FSTA                                                          
         MVC   PSTA,PRNTSTA                                                     
*                                                                               
* COUNT TOTAL LINES NEEDED FOR COMML TEXT *                                     
*                                                                               
         SR    R2,R2                                                            
LRR34    LA    R2,1(,R2)                                                        
         BAS   RE,NEXTEL                                                        
         BE    LRR34                                                            
         STC   R2,ALLOWLIN                                                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     LRR40                                                            
                                                                                
LRR36    MVC   PPROD(L'CMXKPROD),CMXKPROD 3 CHAR PROD                           
         OC    CMXKNET,CMXKNET                                                  
         BZ    LRR40                                                            
         MVC   PNET,CMXKNET                                                     
*                                                                               
LRR40    SLR   R1,R1                                                            
         IC    R1,CMTTXTLN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,LRMVC                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRT TWX OR BLK LINE                          
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    LRR40                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRT TWX OR BLK LINE                          
*                                                                               
         B     LREC20                                                           
LRMVC    MVC   PTEXT(0),CMTTXT                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
* AFTER ADD COMML TEXT REC, SET ON FLAG IN COMML REC *                          
*                                                                               
AAR      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLRECD,R4                                                       
         MVC   CMLKID,=X'0AA1'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         MVC   CMLKCML+1(2),BSEQ                                                
         DROP  R4                                                               
         BAS   RE,SFT              SET FILE TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      DESIRED KEY FOUND ?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         OI    CMLSTAT,X'40'                                                    
         GOTO1 PUTREC                                                           
*                                                                               
* GO UPDATE ANY PATTERN RECS AFFECTED BY THIS COMMERCIAL TEXT *                 
*                                                                               
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BE    EXIT                BYPASS PATTERN UPDATE                        
*                                                                               
         BAS   RE,UPR                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* AFTER DEL COMML TEXT REC, SEE IF ANY OTHER COM TEXT FOR THIS COMML *          
*                                                                               
ADR      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMTRECD,R4                                                       
         MVC   CMTKID,=X'0A35'                                                  
         MVC   CMTKAM(3),BAGYMD & BCLT                                          
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+8                                                              
         BAS   RE,SFX              SET TO XSPOT FILE                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
ADR10    CLC   KEY(5),KEYSAVE                                                   
         BNE   ADR20                                                            
         TM    KEY+13,X'80'        DELETED?                                     
         BO    ADR15                                                            
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+18                                                             
         CLC   CMXKCML,SVCMLP                                                   
         BE    EXIT                                                             
         B     ADR15                                                            
*                                                                               
         CLC   CMTKSEQ,BSEQ        IF ANY OTHER TEXT FOR THIS COMML             
         BE    EXIT                 DON'T TURN OFF STATUS TEXT FLAG             
*                                                                               
ADR15    GOTO1 SEQ                                                              
         B     ADR10                                                            
         DROP  R4                                                               
*                                                                               
* NO OTHER COMML TEXT REC FOR THIS COMML, SET OFF FLAG IN COMML REC *           
*                                                                               
ADR20    XC    KEY,KEY                                                          
         USING CMLRECD,R4                                                       
         CLI   SPOTNETF,C'N'       NET TRAF                                     
         BNE   ADR21                                                            
                                                                                
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         MVC   CMLKCML,SVCMLP                                                   
         B     ADR21C                                                           
                                                                                
ADR21    MVC   CMLKID,=X'0AA1'                                                  
         MVC   CMLKAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMLKCML+1(2),BSEQ                                                
                                                                                
         DROP  R4                                                               
ADR21C   DS    0H                                                               
         BAS   RE,SFT              SET FILE TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      DESIRED KEY FOUND ?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         NI    CMLSTAT,X'FF'-X'40'                                              
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* COMPARE THIS RECORD WITH ONE ON FILE FOR CHANGES *                            
*                                                                               
PUT      L     R4,AIO3                                                          
         ST    R4,AIO                                                           
         L     R2,AIO1             GET KEY FROM AIO1                            
         MVC   KEY,0(R2)                                                        
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+8                                                              
         BAS   RE,SFX              SET TO XSPOT FILE                            
         GOTO1 HIGH                                                             
         CLI   SPOTNETF,C'N'       IS THIS NET TRAF                             
         BNE   *+14                                                             
         CLC   KEY(32),KEYSAVE                                                  
         B     *+10                                                             
         CLC   KEY(13),KEYSAVE     MUST HAVE FOUND REC                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLC   0(256,R2),0(R4)     COMPARE START OF 2 RECS FOR CHANGE           
         BNE   PUT10               IF DIFFERENT, SET SW                         
         CLC   =H'256',14(R2)      SEE IF REC LENGTH MORE THAN 256              
         BNL   PUT06               IF NOT, NO CHANGE                            
         LH    R3,14(,R2)          GET LENGTH                                   
         LA    R2,256(,R2)                                                      
         LA    R4,256(,R4)                                                      
         SH    R3,=H'256'                                                       
         LR    R5,R3                                                            
         CLCL  R2,R4               IF ANY DIFFERENCES                           
         BNE   PUT10                SET SW                                      
PUT06    MVI   IOOPT,C'Y'          ELSE, DO NOT UPDATE REC                      
         B     EXIT                                                             
PUT10    MVI   UPDRECSW,C'Y'       SET SW TO UPDATE PATTERN RECORDS             
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
*                                                                               
* AFTER PUT RECORD UPDATE ALL AFFECTED PATTERN RECORDS *                        
*                                                                               
APR      CLI   UPDRECSW,C'Y'       SW SET TO UPDATE PATTERN RECORDS             
         BNE   EXIT                                                             
*                                                                               
* GO UPDATE ANY PATTERN RECS AFFECTED BY THIS COMMERCIAL TEXT *                 
*                                                                               
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BE    EXIT                BYPASS PATTERN UPDATE                        
         BAS   RE,UPR                                                           
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------                                                      
* VALIDATE NETWORK                                                              
*-------------------------                                                      
*                                                                               
VNET     NTR1                                                                   
*                                                                               
         GOTO1 ANY                                                              
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
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   NETERR                                                           
         MVC   NETWRK,WORK                                                      
         XIT1                                                                   
NETERR   MVC   GERROR,=Y(NONET)                                                 
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - CMML TYPE *                                        
*                                                                               
* FILTER CMMLS FOR LIST FUNCTION                                                
*                                                                               
         DS    0H                                                               
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS     INITIALIZE FILTERS WORK AREA                 
         CLI   5(R2),0             FIELD ENTERED ?                              
         BE    EXIT                NO-RETURN TO CALLING ROUTINE                 
         CLI   8(R2),C'?'          DOES USER NEED HELP ?                        
         BE    VFTRHLP             YES-PROCESS HELP                             
         CLI   5(R2),4             FIELD LNGTH LE 4                             
         BNH   VFTR02              YES-USE FIELD LENGTH                         
         LA    R1,4                FORCE MAX LENGTH OF 4                        
         B     VFTR04                                                           
VFTR02   EQU   *                                                                
         SLR   R1,R1               CLEAR R1                                     
         IC    R1,5(R2)            RETRIEVE FIELD LENGTH                        
VFTR04   EQU   *                                                                
         EX    R1,VFTRCLCH         FIELD EQ 'HELP ?                             
         BNE   VFTRSCAN            SCAN FIELD FOR DESIRED                       
VFTRHLP  EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR IST FIELD'S HEADER                     
         MVC   CONHEAD(L'FTRHELP),FTRHELP PUT FILTER HELP INTO SCREEN           
         B     ERREXIT             PUT SCREEN OUT                               
VFTRCLCH CLC   8(0,R2),=CL4'HELP'                                               
VFTRSCAN EQU   *                                                                
         GOTO1 SCANNER,DMCB,TRAFLTH,(5,BLOCK)                                   
         SLR   R3,R3               CLEAR R3                                     
         IC    R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   EQU   *                                                                
         SLR   R1,R1               CLEAR R1                                     
         IC    R1,0(R4)            RETRIEVE SCANNED LENGTH                      
         BCTR  R1,0                GET MACHINE LENGTH                           
*                                                                               
         B     VFTR90                                                           
*                                                                               
VFTR80   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
VFTR90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(FTRMSGL),FTRMSG                                          
         B     ERREXIT                                                          
         EJECT                                                                  
* READ PATTERN RECORDS AND UPDATE ANY AFFECTED BY THIS TEXT *                   
*                                                                               
         DS    0H                                                               
UPR      NTR1                                                                   
         BC    0,EXIT                                                           
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
         LA    R0,PTNTABLN                                                      
         LA    R1,PTNTABL                                                       
         XC    0(100,R1),0(R1)                                                  
         LA    R1,100(,R1)                                                      
         BCT   R0,*-14                                                          
*                                                                               
* READ PATTERN RECORDS, CHECKING FOR THIS COMMERCIAL *                          
*                                                                               
UPR010   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(3),BAGYMD                                                 
*        MVC   PATKCLT,BCLT                                                     
         GOTO1 HIGH                                                             
*                                                                               
UPR020   CLC   KEY(5),KEYSAVE      ONLY CK THIS TYPE, A/M, CLT                  
         BNE   UPR090                                                           
*                                                                               
         TM    KEY+13,X'02'        INCOMPLETE RECORD ?                          
         BO    UPR060                NO CMLS ON RECORD, BYPASS                  
*                                                                               
         CLI   BPRD,0              ONLY 1 PROD                                  
         BE    UPR030               NOPE                                        
*                                                                               
* COM TEXT PROD SPECIFIC, ONLY THIS PROD *                                      
*                                                                               
         CLI   SVCMLEN,X'FF'       ALL LENGTHS                                  
         BE    UPR022                                                           
         CLC   PATKSLN,SVCMLEN                                                  
         BNE   UPR024                                                           
UPR022   CLC   PATKPRD,BPRD                                                     
         BE    UPR040                                                           
         B     UPR060                                                           
UPR024   CLI   SVCMLEN,X'FF'       ALL LENGTHS                                  
         BE    UPR026                                                           
         CLC   PATKSLN2,SVCMLEN                                                 
         BNE   UPR060                                                           
UPR026   CLC   PATKPRD2,BPRD                                                    
         BE    UPR060                                                           
         B     UPR060                                                           
*                                                                               
* COM TEXT NON PROD SPECIFIC, CK ALL PRODS THIS COMMERCIAL *                    
*                                                                               
UPR030   CLI   SVK30D+1,X'FF'      ALL PRODUCTS                                 
         BE    UPR040                                                           
         ZIC   RE,SVK30D                                                        
         LA    RF,SVK30D(RE)                                                    
         LR    R0,RE                                                            
         LR    R1,RF                                                            
         CLI   SVCMLEN,X'FF'       ALL LENGTHS                                  
         BE    UPR032                                                           
         CLC   PATKSLN,SVCMLEN                                                  
         BNE   UPR060                                                           
UPR032   CLC   PATKPRD,0(RF)                                                    
         BE    UPR040                                                           
         BCTR  RF,0                                                             
         BCT   RE,UPR032                                                        
UPR034   CLI   SVCMLEN,X'FF'       ALL LENGTHS                                  
         BE    UPR036                                                           
         CLC   PATKSLN2,SVCMLEN                                                 
         BNE   UPR060                                                           
UPR036   CLC   PATKPRD2,0(R1)                                                   
         BE    UPR040                                                           
         BCTR  R1,0                                                             
         BCT   R0,UPR036                                                        
         B     UPR060                                                           
*                                                                               
* NOW GET RECORD AND CK PATTERN USED/COMMERCIAL IN CMML LIST *                  
*                                                                               
UPR040   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
* NOW MUST SEARCH PATTERN COMMERCIAL LIST FOR THIS COMMERCIAL *                 
*                                                                               
         MVI   ELCODE,X'30'        CML LIST ELEM                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         ZIC   R0,PATCMLLN                                                      
         SRL   R0,4                DIVIDE BY 16 (DROPPING ODD)                  
         LA    R1,PATCML                                                        
UPR050   CLC   SVCMLP,0(R1)                                                     
         BE    UPR070                                                           
         CLC   SVCMLP,8(R1)                                                     
         BE    UPR070                                                           
         LA    R1,16(,R1)                                                       
         BCT   R0,UPR050                                                        
         EJECT                                                                  
* THIS PATTERN DOES NOT APPLY, GO TO NEXT *                                     
*                                                                               
UPR060   LA    R4,KEY                                                           
         MVC   MYSVKEY,KEY                                                      
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         SH    R1,=H'1'            BUILD NEXT KEY                               
         BP    UPR066              IF ZERO, JUST GO SEQ                         
*                                                                               
         ZIC   R1,PATKCODE                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,PATKCODE                                                      
         CLI   PATKCODE,0          IF OVER 255, BUMP SPOT LEN                   
         BNE   UPR064                                                           
*                                                                               
         ZIC   R1,PATKSLN2                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,PATKSLN2                                                      
*                                                                               
UPR064   SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
UPR066   X     R1,=XL4'00003FFF'             RESET REF TO 1'S COMPL             
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,PATKREF                                                     
         GOTO1 HIGH                                                             
         B     UPR020                                                           
         DROP  R4                                                               
*                                                                               
* UPDATE THIS PATTERN SUBLINE IF PATTERN HAS BEEN USED *                        
*                                                                               
UPR070   L     R4,AIO2                                                          
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         TM    PATSTAT,X'80'       DELETED PATTERN                              
         BO    UPR060                                                           
         OC    PATUSED,PATUSED     WAS PATTERN USED IN INSTRUCTIONS             
         BZ    UPR060               NO, NO UPDATE                               
         CLC   TODAY,PATEND        IF PAT END DATE BEFORE TODAY                 
         BH    UPR060               IGNORE                                      
*                                                                               
         OC    BMKTSTA,BMKTSTA     IF COMML TEXT REC MKT/STA SPECIFIC           
         BNZ   UPR080               ONLY UPDATE INS RECAPS                      
*                                                                               
         XC    PATUSED,PATUSED     CLEAR USED DATE FLD                          
         OI    PATSTAT,X'08'       SET ON CMML TEXT FORCED UPDATE               
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         BCTR  R1,0                SUB 1 FROM SUBLINE                           
         STCM  R1,7,PATKREF                                                     
         MVC   MYSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(13),PATKEY                                                   
         ST    R4,AIO                                                           
         GOTO1 ADDREC              NOW ADD REC WITH NEW SUBLINE                 
         MVC   KEY,MYSVKEY                                                      
         GOTO1 HIGH                                                             
         B     UPR060                                                           
*                                                                               
* COMML TEXT IS MKT/STA SPECIFIC, UPDATE INSTR RECAP RECS *                     
*                                                                               
UPR080   LA    R0,PTNTABLN                                                      
         LA    R2,PTNTABL                                                       
         USING PTNLIST,R2                                                       
UPR082   CLI   0(R2),0             END OF TABLE                                 
         BE    UPR086                                                           
         CLC   PTNLPRD,PATKPRD     BUG IF IN TABLE                              
         BNE   UPR084                                                           
         CLC   PTNLCPY,PATKCODE    BUG IF IN TABLE                              
         BNE   UPR084                                                           
         CLC   PTNLSLN(6),PATKSLN     BUG IF IN TABLE                           
         BE    UPRBUG                                                           
UPR084   LA    R2,PTNLEN(,R2)                                                   
         BCT   R0,UPR082                                                        
UPRBUG   DC    H'0'                                                             
UPR086   MVC   PTNLPRD,PATKPRD                                                  
         MVC   PTNLCPY,PATKCODE                                                 
         MVC   PTNLSLN(3),PATKSLN                                               
         MVC   PTNLREFS,PATKREF                                                 
         GOTO1 DATCON,DMCB,(3,PATSTART),(2,PTNLSDT)                             
         GOTO1 (RF),(R1),(3,PATEND),(2,PTNLEDT)                                 
*        MVC   PTNLSDT,PATSTART                                                 
*        MVC   PTNLEDT,PATEND                                                   
         B     UPR060                                                           
         DROP  R2                                                               
*                                                                               
UPR090   OC    PTNTABL,PTNTABL                                                  
         BZ    EXIT                                                             
         OC    BMKTSTA,BMKTSTA     MUST BE MKT/STA SPECIFIC                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,PTNTABLN                                                      
         LA    R2,PTNTABL                                                       
         USING PTNLIST,R2                                                       
         LR    RF,R2                                                            
         SR    R0,R0                                                            
         CLI   0(RF),0                                                          
         BE    *+14                                                             
         BCTR  R0,0                                                             
         LA    RF,PTNLEN(,RF)                                                   
         BCT   RE,*-14                                                          
*                                                                               
         LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,(R2),(R0),PTNLEN,PTNLEN,0                             
*                                                                               
UPR091   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(3),BAGYMD & CLT                                           
         MVC   INSKPRD,PTNLPRD                                                  
         MVC   INSKMKT(5),BMKTSTA                                               
         MVC   INSKCOPY,PTNLCPY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   UPR116                                                           
         OC    BSTA,BSTA           THIS STATION SPECIFIC                        
         BZ    UPR092                                                           
         CLC   INSKSTA,BSTA                                                     
         BNE   UPR116                                                           
         CLI   PTNLCPY,0                                                        
         BE    UPR092                                                           
         CLC   INSKCOPY,PTNLCPY                                                 
         BNE   UPR110                                                           
UPR092   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   UPDRECSW,C'N'       SET OFF SW TO UPDATE RECORD                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   UPR110                                                           
         USING INSDTAEL,R6                                                      
UPR094   CLC   INSPRD1,PTNLPRD                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   INSSLN1(3),PTNLSLN                                               
         BE    UPR098                                                           
UPR096   BAS   RE,NEXTEL                                                        
         BE    UPR094                                                           
         CLI   UPDRECSW,C'Y'       SET ON SW TO UPDATE RECORD                   
         BNE   UPR110                                                           
         GOTO1 PUTREC                                                           
         B     UPR110                                                           
UPR098   SR    RE,RE                                                            
         ZIC   RF,INSDTALN                                                      
         AHI   RF,-INSBSCEL                                                     
         D     RE,=A(INSSUBEL)                                                  
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R1,INSPTTN                                                       
UPR100   CLC   PTNLREFS,0(R1)                                                   
         BNE   UPR104                                                           
         CLC   PTNLSDT,5(R1)       PAT START TO THIS END                        
         BH    UPR104                                                           
         CLC   PTNLEDT,3(R1)       PAT END TO THIS START                        
         BL    UPR104                                                           
         CLC   PTNLSDT,3(R1)       PAT START TO THIS START                      
         BH    UPR104                                                           
         CLC   PTNLEDT,5(R1)       PAT END TO THIS END                          
         BNL   UPR106                                                           
UPR104   LA    R1,9(,R1)                                                        
         BCT   RF,UPR100                                                        
         B     UPR096                                                           
UPR106   MVI   UPDRECSW,C'Y'       SET ON SW TO UPDATE RECORD                   
         OI    INSFLAG,X'08'                                                    
         B     UPR096                                                           
UPR110   GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   UPR116                                                           
         OC    BSTA,BSTA           THIS STATION SPECIFIC                        
         BZ    UPR114                                                           
         CLC   INSKSTA,BSTA                                                     
         BE    UPR092                                                           
UPR114   CLI   PTNLCPY,0                                                        
         BE    UPR092                                                           
         CLC   INSKCOPY,PTNLCPY                                                 
         BNE   UPR110                                                           
         B     UPR092                                                           
UPR116   LA    R2,PTNLEN(,R2)                                                   
         CLI   PTNLPRD,0                                                        
         BNE   UPR091                                                           
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT STATION FOR PRINTING *                                                 
*                                                                               
         DS    0H                                                               
FSTA     NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(4),PRNTSTA                                                  
         LA    R1,WORK+4                                                        
         CLI   WORK+3,C' '                                                      
         BH    *+6                                                              
         BCTR  R1,0                                                             
FSTA10   MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),PRNTSTA+4                                                
         CLI   1(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   1(R1),C'T'                                                       
         MVC   PRNTSTA,WORK '                                                   
         B     EXIT                                                             
*                                                                               
* FILTER ROUTINE WHEN NEEDED *                                                  
*                                                                               
FTR      NTR1                                                                   
*                                                                               
         OC    FILTERS,FILTERS                                                  
         BZ    FTREQ                                                            
*                                                                               
FTREQ    CR    R1,R1               SET COND CODE FILTERED OK                    
         B     EXIT                                                             
FTRNO    CR    RB,RD               SET COND CODE NO FILTER                      
         B     EXIT                                                             
*                                                                               
HDHK     NTR1                                                                   
*                                                                               
         MVC    H5+3(L'QCLT),QCLT  PUT CLIENT MNEMONIC INTO HL                  
         MVC    H5+7(L'CLTNM),CLTNM PUT CLIENT NAME AFTER IT                    
*                                                                               
         CLI   SPOTNETF,C'N'       NET TRAFFIC                                  
         BNE   HDHKX                                                            
         MVC   H8+21(7),=C'NETWORK'                                             
         MVI   H9+27,C'-'                                                       
         MVC   H8+28(7),SPACES                                                  
         MVC   H9+28(7),SPACES                                                  
*                                                                               
HDHKX    B     EXIT                RETURN TO GENCON                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
*=====================================                                          
*        ERROR ROUTINES                                                         
*=====================================                                          
                                                                                
INVPOSN  MVC   CONHEAD,ERRPOSN                                                  
         B     ERREXIT                                                          
*                                                                               
INVPFKY  MVC   CONHEAD,ERRPFKY                                                  
         B     ERREXIT                                                          
*                                                                               
DELCMLER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DELCMLMS),DELCMLMS LINE LENGTH ERROR                   
         B     ERREXIT                                                          
LINLENER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'LINLENMS),LINLENMS LINE LENGTH ERROR                   
         B     ERREXIT                                                          
PRDNCML  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDNOCML),PRDNOCML NO PRODUCT COMMERCIAL               
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
BADCOMML MVI   ERROR,INVCOMM       INVALID COMMERCIAL CODE                      
         B     TRAPERR             PROCESS ERROR                                
CMTLENER MVI   ERROR,INVTXTLN      TEXT LINE TOO LONG                           
         B     TRAPERR             PROCESS ERROR                                
NOCMLERR MVI   ERROR,INVCOMM       INDICATE COMMERCIAL INVALID                  
         B     TRAPERR             PROCESS ERROR                                
MISSERR  MVI   ERROR,MISSING                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
INSBSCEL EQU   15                  BASIC ELEM LENGTH                            
INSSUBEL EQU   7                   SUB-EL LENGTH                                
*                                                                               
ERRPFKY  DC    CL60'* ERROR * UNDEFINED FUNCTION KEY *'                         
ERRPOSN  DC   CL60'* ERROR * CURSOR MUST BE ON A TEXT LINE FOR PF3/4 *'         
DELCMLMS DC    C'** ERROR ** COMMERCIAL IS DELETED *'                           
LINLENMS DC    C'** ERROR ** LINE CAN NOT BE MORE THAN 58 CHARACTERS *'         
PRDNOCML DC    C'** ERROR ** DESIRED PRODUCT NOT IN COMMERCIAL **'              
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'VALID FILTERS - NONE *'                                        
FTRHELPL EQU   *-FTRHELP                                                        
FTRMSGL  EQU   *-FTRMSG                                                         
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'COMMERCIAL TEXT LIST'                                    
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'--------------'                                           
         SSPEC H2,35,C'--------------------'                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H6,85,PAGE                                                       
         SSPEC H8,3,C'COMMERCIAL'                                               
         SSPEC H9,3,C'----------'                                               
         SSPEC H8,17,C'PROD'                                                    
         SSPEC H9,17,C'----'                                                    
         SSPEC H8,23,C'MARKET'                                                  
         SSPEC H9,23,C'------'                                                  
         SSPEC H8,30,C'STATION'                                                 
         SSPEC H9,30,C'-------'                                                 
         SSPEC H8,38,C'COMMERCIAL TITLE'                                        
         SSPEC H9,38,C'----------------'                                        
         SSPEC H8,56,C'COMMERCIAL TEXT'                                         
         SSPEC H9,56,C'---------------'                                         
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
*====================================================================           
* VALIDATE CHARACTER COMMERCIAL CODE                                            
* EXCEPT FOR LIST ACTION,                                                       
* FIRST 4 CHARS MUST BE ALPHA, 5TH NUMERIC OR -, LAST 3 NUMERIC *               
* ON EXIT, 8 BYTE CMML WILL BE IN SVCMLP                                        
*====================================================================           
                                                                                
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVCMLP,SVCMLP                                                    
         CLI   5(R2),0                                                          
         BNE   VCML2                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    VCMLX                                                            
*                                                                               
VCML2    GOTO1 ANY                 GET INPUT IN WORK                            
         MVC   SVCMLP,WORK                                                      
*                                                                               
         CLI   5(R2),8             MUST BE AT LEAST 8 CHARS                     
         BH    VCML50              MORE IS ADID                                 
         BL    BADLENER                                                         
*                                                                               
         CLC   TRACML(8),=8C'9'    CML ID ALL 9'S (PROD HSE KEY)                
         BE    CMLINVER                                                         
*                                                                               
VCMLX    XIT1                                                                   
         EJECT                                                                  
*=================================================================              
* SEE IF THIS IS AN AD-ID CODE                                                  
*=================================================================              
                                                                                
VCML50   DS    0H                                                               
         CLI   5(R2),12            ADID MUST BE 9-12                            
         JH    BADLENER                                                         
                                                                                
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SVCMLP                                  
         BNE   CMLINVER                                                         
                                                                                
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BE    VCMLX                                                            
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),SVCMLP                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCML52                                                           
         MVI   ERROR,NOTFOUND                                                   
         J     TRAPERR                                                          
*                                                                               
VCML52   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
                                                                                
         GOTO1 GETREC                                                           
*                                                                               
         XC    TRACML,TRACML                                                    
         MVC   TRACML(8),5(R6)                                                  
         MVC   SVCMLP,TRACML                                                    
                                                                                
         TM    15(R6),X'01'        IS CML PACKED IN KEY                         
         BZ    VCML55                                                           
         GOTO1 VTRPACK,DMCB,(C'U',5(R6)),TRACML                                 
                                                                                
VCML55   OI    TRACMLH+6,X'80'     FORCE TRANSMIT                               
         B     VCMLX                                                            
         CLI   5(R2),0             TEST NO INPUT                                
         BE    VCMLX                                                            
*                                                                               
CMLINVER MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
                                                                                
BADLENER LHI   R0,NOT812                                                        
*                                                                               
TRAPERR2 STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRCMLTXT                                                     
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         PRINT OFF                                                              
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE SPTRAE1D                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
VTRPACK  DS    A                                                                
TODAY    DS    XL3                                                              
FILTERS  DS    0CL4                                                             
CTYPFTR  DS    CL4                                                              
BSEQ     DS    XL2                 COMMERICAL PROFILE REC SEQUENCE CODE         
PRNTSTA  DS    CL6                 STATION SAVE                                 
SVCMLP   DS    CL8                 COMMERCIAL CODE                              
SVCML12  DS    CL12                                                             
SVCMLT   DS    CL15                COMML TITLE FOR LIST                         
SVCMLEN  DS    XL1                 COMML LENGTH                                 
COMPKEY  DS    0XL13                                                            
COMPKEYN DS    0XL20                                                            
COMPKID  DS    XL2                                                              
COMPKAM  DS    XL1                                                              
COMPKCLT DS    XL2                                                              
COMPKPRO DS   0XL3                                                              
COMPKPRD DS    XL1                                                              
COMPKMKT DS    XL2                                                              
COMPKNET DS   0XL4                 FOR NET TRAFFIC                              
COMPKSTA DS    XL3                                                              
COMPKSEQ DS    XL2                                                              
         ORG   COMPKSEQ+1                                                       
COMPKCML DS    CL8                                                              
MYSVKEY  DS    CL(L'KEY)                                                        
                                                                                
UPDRECSW DS    CL1                 UPDATE PATTERN SWITCH                        
NETWRK   DS    CL4                 NETWORK                                      
SVK30D DS      CL256                                                            
*                                                                               
* ENTRY IS PRD/SLN/PRD2/SLN2/REF-SUB                                            
*                                                                               
PTNTABL  DS    200XL12                                                          
PTNTABLN EQU   200                                                              
*                                                                               
***********************************************************************         
*CLISTDS       CLIENT HEADER PRODUCT LIST DSECT                       *         
*        THIS DSECT MAPS OUT THE PRODUCT LIST IN THE CLIENT HDR CLIST *         
***********************************************************************         
*                                                                               
CLISTDS  DSECT                                                                  
CLISQPRD DS    CL3            PRODUCT MNEMONIC                                  
CLISBPRD DS    X              BINARY PRODUCT CODE                               
CLISTENT EQU   *-CLISTDS      PRODUCT ENTRY LENGTH                              
*                                                                               
***********************************************************************         
*PTNLIST       PATTERNS AFFECTED BY COMMERCIAL TEXT CHANGE OR ADD     *         
*              ONLY CREATED FOR MARKET AND STATION SPECIFIC RECS      *         
***********************************************************************         
*                                                                               
PTNLIST  DSECT                                                                  
PTNLPRD  DS    XL1                                                              
PTNLCPY  DS    XL1                                                              
PTNLSLN  DS    XL1                                                              
PTNLPRD2 DS    XL1                                                              
PTNLSLN2 DS    XL1                                                              
PTNLSDT  DS    XL2                                                              
PTNLEDT  DS    XL2                                                              
PTNLREFS DS    XL3                                                              
PTNLEN   EQU   *-PTNLIST              ENTRY LENGTH                              
         EJECT                                                                  
* OFFLINE PRINT LINE DSECT                                                      
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P1                                                               
         DS    CL2                                                              
PCML     DS    CL8                                                              
         DS    CL4                                                              
PPROD    DS    CL3                                                              
         DS    CL4                                                              
PNET     DS   0CL4                                                              
PMKT     DS    CL4                                                              
         DS    CL3                                                              
PSTA     DS    CL6                                                              
         DS    CL2                                                              
PCMLTI   DS    CL15                                                             
         DS    CL2                                                              
PTEXT    DS    CL58                                                             
         DS    C                                                                
*                                                                               
* OFFLINE PRINT LINE DSECT                                                      
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL2                                                              
LPROD    DS    CL3                                                              
         DS    CL2                                                              
LNET     DS   0CL4                                                              
LMKT     DS    CL4                                                              
         DS    CL2                                                              
LSTA     DS    CL6                                                              
         DS    CL2                                                              
LCML     DS    CL12                                                             
         DS    CL2                                                              
LTEXT    DS    CL36                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPTRA31   01/21/16'                                      
         END                                                                    
