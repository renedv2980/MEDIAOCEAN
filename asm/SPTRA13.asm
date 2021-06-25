*          DATA SET SPTRA13    AT LEVEL 060 AS OF 09/28/16                      
*PHASE T21613C                                                                  
*        TITLE 'T21613 DEALER AND SPECIAL TEXT MAINT AND LIST'                  
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - USED TO READ PREV RECS FOR CHAINED STEXT                   
*                  - USED TO READ CLIENT RECS IN OFFLINE LIST                   
*             AIO3 -                                                            
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
*        R7 - SECOND BASE REGISTER                                              
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
* LEV 18 04/30/86 EDIT FOR MKT AND STA FOR SPEC CHAR                            
* LEV 29    APR03/87 ALLOW FOR SPECIAL TEXT EST=000 AND TYPE=X                  
* LEV 30-32 MAY13/87 ALLOW FOR SPECIAL TEXT FOR MARKET GROUP MGR=X0000          
* LEV 33-38 MAR28/88 USE PAGE, NOT NEXT ID FIELD                                
*                    ONLY SHOW SPEC TEXT/DEALER TEXT                            
*                    LIST CHAINED RECS TOGETHER                                 
* LEV 39    APR07/92 ADD PF KEY ADD/DELETE LINE SUPPORT                         
* LEV 40    APR23/92 FIX FIELD LENGTH CHECK FOR HELP FIELDS STD                 
* LEV 41    AUG28/92 ADD CODE FOR LETTER FOR FAX LETTERS                        
* LEV 42    JUN23/93 ADD NEW TRAFFIC SYSTEM                           *         
* LEV 43    JUL20/94 CHANGE TO FILENAME                               *         
* LEV 44    AUG01/94 FIX INACTIVE PATTERN                             *         
* LEV 45    JAN09/95 FIX DEL ACTIVE PATTERN CHECKING                  *         
* LEV 46 SMUR SEP15/97 ALLOW CABLE STATIONS (*9999/)                  *         
* LEV 47 BGRI DEC07/00 SHOW ** UNKNOWN ** FOR MISSING CLIENT          *         
* LEV 48 SMUR JUL02/01 ALLOW ALL CLIENT AND BY OFFICE '*' STEXT RECORD*         
* LEV 49 SMUR OCT23/01 CLIENT SECURITY                                *         
* LEV 50 SMUR JUN24/02 MUST ENTER CLIENT IF CLIENT STRING SECURITY    *         
* LEV 51 SMUR FEB04/04 2 CHAR MARKET GROUP                            *         
* LEV 52 SMUR MAR25/04 BRAND LEVEL SECURITY                           *         
* LEV 53 SMUR JUL26/04 SOX                                            *         
* LEV 54 SMUR SEP02/05 2 CHAR OFFICE CODE                             *         
* LEV 55 MHER MAY12/14 AUTO ADD MEDIA N WHEN ADDING REC FOR MEDIA T   *         
* LEV 58 SMUR FEB23/16 ENABLE GLOBBER CALL FOR OPTICA                 *         
* LEV 59 SMUR JUL18/16 RESTORE X'50' LINK ELEMENT WHEN ACTION RESTORE *         
*                      GENNEW CAN'T HANDLE BLANK LINES SEND <BLANK>   *         
*                      FIX CKSM:LEADING ZEROS REMOVED BY HEXD IN DDLNK*         
* LEV 60 SMUR SEP01/16 FIX DISPLAY OF ALPHA NUMERIC CLIENT CODES      *         
***********************************************************************         
         TITLE 'T21613 DEALER AND SPECIAL TEXT MAINT AND LIST'                  
T21613   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21613**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR13RR                                                      
*                                                                               
         NI    GENSTAT2,X'FF'-USMYOK   RESET USE MY MSG FLAG                    
*                                                                               
         TM    GENSTAT6,GES$UPLD   IF NOT UPLOADING THROUGH DDLINK              
         BO    MAIN00                                                           
         CLI   TWASCR,X'B3'        AND ON MAINTENANCE SCREEN                    
         BNE   MAIN00                                                           
         OI    TRACKSH+1,X'2C'     MAKE CHECKSUM FIELD PROTECTED,               
         OI    TRACKSH+6,X'80'     INVISIBLE AND TRANSMITTED                    
*                                                                               
MAIN00   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,XRECPUT        AFTER PUT REC                                
         BE    APR                                                              
         CLI   MODE,XRECADD        AFTER ADD REC                                
         BE    AAR                                                              
         CLI   MODE,RECREST        BEFORE RESTORE RECORD                        
         BE    BREST                                                            
         CLI   MODE,XRECREST       AFTER RESTORE RECORD                         
         BE    AREST                                                            
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    DEL                                                              
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    DEL                                                              
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK08                 NO                                          
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
VK08     CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VK14                YES, NOT NEEDED                              
         SPACE                                                                  
* ONLY ALLOW TO ADD STEXT BY OFFICE IN NET TRAFFIC                              
         SPACE                                                                  
VK10     DS    0H                                                               
         CLI   8(R2),C'*'          REQUEST BY OFFICE                            
         BE    *+16                                                             
         CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
         BE    BADOFFER             YES, ERROR                                  
         B     VK12                                                             
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
         CLI   CONREC,C'S'         THIS SPECIAL TEXT                            
         BNE   INVCLERR            NO                                           
         CLI   5(R2),3             IS LENGTH 3                                  
         BH    BADOFFER                                                         
         SPACE                                                                  
* VALIDATE OFFICE AND CONVERT TO 1 BYTE IF NEEDED                               
         SPACE                                                                  
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),3             IS LENGTH 3                                  
         BE    *+10                                                             
         OC    OFCOFC2,SPACES                                                   
*                                                                               
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VK10F               JUST CONVERT, DO NOT VALIDATE                
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK10C               NO, VALIDATE AND CONVERT                     
*                                                                               
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK10C               VALIDATE AND CONVERT                         
*                                                                               
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK10C                                                            
*                                  SINGLE CLIENT ACCESS                         
         MVI   LAOFFICE,0          INIT LIMITED ACCESS OFFICE                   
*                                                                               
         BAS   RE,GOFF             GET OFFICE FOR THIS CLIENT                   
         B     VK10F                                                            
*                                                                               
VK10C    MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
*                                                                               
VK10F    XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   INVOFERR            INVALID OFFICE                               
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    VK11                2 CHAR OFFICE IS NOT ON                      
*                                                                               
         MVC   SVOFF,OFCOFC        SAVE 1 BYTE OFFICE CODE                      
         MVI   BCLT,C'*'           MOVE (*) INTO CLT                            
         MVC   BCLT+1(1),OFCOFC                                                 
         B     VK11F                                                            
         SPACE                                                                  
         DROP  R3                                                               
         SPACE                                                                  
VK11     MVC   SVOFF,9(R2)                                                      
         MVC   BCLT,8(R2)          MOVE (*) INTO CLT                            
         SPACE                                                                  
         CLI   LAOFFICE,0          LIMITED ACCESS ?                             
         BE    VK11F                NO                                          
         SPACE                                                                  
         CLC   SVOFF,LAOFFICE      SAME OFFICE                                  
         BE    VK11F                                                            
         SPACE                                                                  
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR                                                          
*                                                                               
VK11F    GOTO1 =A(VOFF),RR=SPTR13RR  VALIDATE OFFICE CODE                       
         BE    VK14                                                             
*                                                                               
         MVC   GERROR,=Y(NOCLTS)                                                
         GOTO1 VTRAERR             USING GETTXT CALL                            
*                                                                               
VK12     GOTO1 VALICLT                                                          
*                                                                               
VK14     XC    WORK,WORK           READ T3 PROFILE                              
         MVC   WORK(4),=C'S0T3'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK15                                                             
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK15                                                             
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK15     GOTO1 GETPROF,DMCB,WORK,SVT3PROF,DATAMGR                               
*                                                                               
         MVI   WORK+6,C'T'         READ MEDIA T PROF+6                          
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF+6(1),ELEM+6                                             
*                                                                               
VK16     LA    R2,TRAIDH           TEXT ID                                      
         XC    ID,ID                                                            
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK20                                                             
         CLI   CONREC,C'S'         THIS SPECIAL TEXT                            
         BNE   *+12                                                             
         CLI   BCLT,C'*'           BY OFFICE                                    
         BE    VK30                YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST                                         
         BNE   MISSERR             NO, ERROR                                    
         B     VK30                YES, NOT NEEDED                              
*                                                                               
VK20     CLI   BCLT,C'*'           BY OFFICE                                    
         BE    NOIDERR             YES, FIELD SHOULD BE BLANK                   
         SPACE                                                                  
         BAS   RE,VID             VALIDATE ID                                   
         SPACE                                                                  
VK30     LA    R2,TRAPGH           VALIDATE PAGE                                
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK34                 YES                                         
         MVI   PAGNO,C'L'          SET TO PAGE 1                                
         MVI   TRAPG,C'1'                                                       
         OI    TRAPGH+6,X'80'                                                   
         B     VK40                                                             
*                                                                               
VK34     BAS   RE,VPG              GO VALIDATE PAGE NUMBER                      
*                                                                               
VK40     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING DTXKEY,R4                                                        
         MVC   DTXKID,=XL2'0A2D'                                                
         MVC   DTXKAM,BAGYMD                                                    
         MVC   DTXKCLT,BCLT        MOVE IN CLIENT                               
         MVC   DTXKDESC,ID         MOVE TEXT DESC                               
         MVC   DTXKTYP,PAGNO                                                    
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKX                                                              
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
         BE    VK42                                                             
         LA    R2,TRAMEDH                                                       
         LHI   R0,NOTVCMML                                                      
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
VK42     MVC   KEY(13),WORK        RESTORE KEY                                  
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
* *******************************************************************           
* VALIDATE RECORD ROUTINE                                                       
* *******************************************************************           
         SPACE                                                                  
VR       DS    0H                  FIRST VALIDATE PF KEY HIT                    
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VR01     DS    0H                                                               
         CLI   PFKEY,3             ERASE LINE?                                  
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
         LA    RF,TRAL16H                                                       
         CR    R2,RF               END OF SCREEN?                               
         BH    INVPOSN             YES- INVALID POSITION FOR PF3/4              
         B     VRPFK10                                                          
*                                                                               
VRPFK20  LA    RF,TRAL16H          A(LAST TEXT FIELD)                           
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
         LA    R3,TRAL16H          LAST LINE OF TEXT                            
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
         SPACE                                                                  
VR06     L     R4,AIO                                                           
         TM    GENSTAT6,GES$UPLD   IF UPLOADING THROUGH DDLINK                  
         BZ    VR07                                                             
         CLI   ACTNUM,ACTADD       AND ACTION IS NOT ADD                        
         BE    VR07                                                             
         CLI   TRACKSH+5,0         CHECK SUM IS REQUIRED                        
         BE    VR06F                                                            
*                                                                               
         MVC   WORK(L'TRACKS),TRACKS                                            
         CLI   TRACKSH+5,8         CKSM < 8 IS LEFT JUSTIFIED                   
         BE    VR06B                                                            
                                                                                
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(7),WORK      PREFILL WITH ZEROS                           
         LLC   R1,TRACKSH+5        INPUT LEN                                    
         SHI   R1,8                                                             
         LPR   R1,R1               NUMBER OF ZERO PADDING NEEDED                
         LA    R1,WORK(R1)                                                      
         LLC   RE,TRACKSH+5        INPUT LEN                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TRACKS                                                   
VR06B    GOTO1 HEXIN,DMCB,WORK,FULL,L'TRACKS                                    
*                                                                               
         L     RE,AIO                                                           
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         C     R0,FULL             AND MUST MATCH RECORD                        
         JE    VR07                                                             
*                                                                               
VR06F    LHI   R0,CMLCHKSUM                                                     
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
VR07     L     R4,AIO                                                           
         USING DTXKEY,R4                                                        
         MVC   DTXAGYA,AGENCY                                                   
         MVC   BAGYMD,DTXKAM                                                    
         CLC   BCLT,DTXKCLT        IS CLIENT SAME                               
         BE    VR10                 YES                                         
         MVC   BCLT,DTXKCLT                                                     
         GOTO1 CLUNPK,DMCB,(SVCPROF6,DTXKCLT),QCLT                              
VR10     MVC   ID,DTXKDESC                                                      
         DROP  R4                                                               
         SPACE                                                                  
* GET ANY TEXT TITLE                                                            
         SPACE                                                                  
         MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM             WILL REMOVE TITLE ELEMENT                    
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DTXTLEEL,R6                                                      
         MVI   DTXTLEEL,X'20'                                                   
         MVI   DTXTLELN,26         LENGTH                                       
         LA    R2,TRATLEH          TITLE LINE                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 ANY                                                              
         MVC   DTXTITLE,WORK                                                    
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
* GET ANY TEXT LINES                                                            
         SPACE                                                                  
VR30     MVI   ELCODE,X'40'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R3,1                SET TEXT LINE NUMBER                         
         LR    R5,R3               SET BLK LINE CTR                             
         LA    R6,ELEM                                                          
         USING DTXTXTEL,R6                                                      
         MVI   DTXTXTEL,X'40'                                                   
         LA    R2,TRAL01H          FIRST TEXT LINE                              
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
         SPACE                                                                  
VR40     CLI   0(R1),C' '          IF BLANK                                     
         BH    VR44                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,VR40             CK ENTIRE FLD                                
VR44     STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
         SPACE                                                                  
VR50     GOTO1 ANY                                                              
         SPACE                                                                  
         CLI   5(R2),58            MAX LENGTH                                   
         BH    DTXLENER            TOO LONG                                     
         ZIC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)           TOTAL ELEMENT LENGTH                         
         STC   R0,DTXTXTLN         AND LENGTH                                   
         STC   R3,DTXLNNUM         TEXT LINE NUMBER                             
         BCTR  R1,0                                                             
         EX    R1,VRMVC                                                         
         GOTO1 ADDELEM                                                          
         MVI   DTXTXTEL,X'40'                                                   
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VR60     LA    R5,1(,R5)           INCREMENT BLANK LINE CTR                     
         ZIC   R1,0(R2)            GET THIS SCREEN FIELD LENGTH                 
         AR    R2,R1               ADD TO FIELD POINTER                         
         ZIC   R1,0(R2)            ALSO BYPASS PROTECTED FIELD                  
         AR    R2,R1               ADD TO FIELD POINTER                         
         LA    R1,TRATAGH          END OF SCREEN                                
         CR    R1,R2               IF THERE                                     
         BNH   VR80                GET OUT                                      
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
VR64     DS    0H                                                               
         TM    GENSTAT6,GES$UPLD   IF UPLOADING THROUGH DDLINK                  
         BZ    VR66                                                             
         CLI   5(R2),7             INPUT LEN=7 ?                                
         BNE   VR66                                                             
         CLC   =C'<BLANK>',8(R2)    BLANK LINE ?                                
         BNE   VR66                                                             
         MVI   5(R2),0             INPUT LEN                                    
         MVI   4(R2),0             INPUT INDICATOR                              
         XC    8(L'TRAL01,R2),8(R2)                                             
         B     VR70                                                             
                                                                                
VR66     CLI   0(R1),C' '          IF BLANK                                     
         BH    VR70                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,VR64             CK ENTIRE FLD                                
                                                                                
VR70     STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR60                NO                                           
VR74     CR    R3,R5               CK LAST LINE VS THIS LINE                    
         BE    VR50                NO INTERVENING BLANK LINES                   
         MVI   DTXTXTLN,4          ONLY 1 BLANK NEEDED FOR BLK LINE             
         STC   R3,DTXLNNUM         STORE TEXT LINE NUMBER                       
         MVI   DTXTXT,C' '         AND HERE IT IS                               
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           ADD TO TEXT LINE CT                          
         B     VR74                BUILD ALL BLK LINES NEEDED                   
VR80     CLI   ACTNUM,ACTADD       UNLESS ADD, ALL DONE                         
         BNE   DR                                                               
         CLI   PAGNO,C'L'          PAGE 1 - NO PREV LINK NEEDED                 
         BE    DR                                                               
         LA    R4,KEY                                                           
         L     R6,AIO1                                                          
         USING DTXKEY,R4                                                        
         MVC   DTXKEY,0(R6)  '                                                  
         ZIC   RE,DTXKTYP                                                       
         BCTR  RE,0                                                             
         STC   RE,DTXKTYP                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         SPACE                                                                  
* BUILD NEXT (LINK) ELEM IN PREV REC *                                          
         SPACE                                                                  
         MVI   ELCODE,X'50'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE ALL X'50' ELEMENTS               
         LA    R6,ELEM                                                          
         USING DTXNXTEL,R6                                                      
         MVI   DTXNXTEL,X'50'                                                   
         MVI   DTXNXTLN,9                                                       
         MVC   DTXNEXT,ID                                                       
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         B     DR                  NOW DISPLAY VALIDATED RECORD                 
VRMVC    MVC   DTXTXT-DTXTXTEL(0,R6),WORK                                       
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE TITLE ELEMENT                      
         USING DTXTLEEL,R6                                                      
         CLC   TRATLE,DTXTITLE                                                  
         BE    *+14                                                             
         MVC   TRATLE,DTXTITLE                                                  
         OI    TRATLEH+6,X'80'                                                  
DR40     L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE 1 TEXT ELEMENT                     
         LA    R2,TRAL01H                                                       
         LA    R4,8(,R2)                                                        
         USING DTXTXTEL,R6                                                      
DR50     LLC   R1,DTXTXTLN         GET TEXT ELEMENT LENGTH                      
         AHI   R1,-(3+1)           GET TEXT LENGTH -1                           
         LLC   RF,0(R2)            GET FIELD LENGTH                             
         LR    RE,RF                                                            
         TM    1(R2),FATBXHDR      Have extended field header?                  
         JZ    *+8                                                              
         AHI   RF,-(FLDHDRL)       Do not clear extended field header           
         AHI   RF,-(FLDHDRL+1)     1 for EX, length of field to clear           
         EX    RF,DRXC             CLEAR OUTPUT FLD                             
         CR    RF,R1               SEE IF ENOUGH ROOM IN FIELD                  
         BNL   DR60                                                             
         DC    H'0'                NOT ENOUGH ROOM IN FLD FOR TEXT LINE         
DR60     EX    R1,DRMVC                                                         
         OI    6(R2),X'80'         SET ON TRANSMIT BIT                          
         AR    R2,RE               POINT TO NEXT FIELD (TITLE)                  
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD (INPUT)                  
         BAS   RE,NEXTEL                                                        
         BNE   DR70                                                             
         LA    R0,TRATAGH                                                       
         CR    R0,R2               CK IF END OF SCREEN                          
         BH    DR50                NO                                           
         DC    H'0'                MORE TEXT LINES THAN SCREEN SPACE            
DR70     LA    R0,TRATAGH                                                       
         CR    R0,R2               CK IF END OF SCREEN                          
         BNH   DR80                NO                                           
         OC    8(L'TRAL01,R2),8(R2)                                             
         BZ    DR74                                                             
         XC    8(L'TRAL01,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
DR74     ZIC   R1,0(R2)            GET FLD LEN                                  
         AR    R2,R1               NOW AT TITLE                                 
         ZIC   R1,0(R2)            GET FLD LEN                                  
         AR    R2,R1               NOW AT NEXT INPUT FIELD                      
         B     DR70                                                             
DR80     B     EXIT                                                             
DRMVC    MVC   8(0,R2),DTXTXT-DTXTXTEL(R6)                                      
DRXC     XC    8(0,R2),8(R2)                                                    
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       L     R4,AIO                                                           
         USING DTXKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         XC    WORK(L'TRACLT),WORK                                              
         MVC   BCLT,DTXKCLT                                                     
         CLI   BCLT,C'*'           IF BY OFFICE                                 
         BNE   DK05                                                             
         BAS   RE,CNVOFF                                                        
         BNE   DK08                                                             
         B     DK09                                                             
         SPACE                                                                  
DK05     BAS   RE,FCLT             FIND CLIENT                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
DK08     GOTO1 CLUNPK,DMCB,(SVCPROF6,DTXKCLT),QCLT                              
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
DK09     CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         XC    WORK(L'TRAID),WORK                                               
         MVC   WORK(L'DTXKDESC),DTXKDESC                                        
         MVC   ID,DTXKDESC                                                      
         SPACE                                                                  
* TEST IF ANY SPECIAL CHARS SPECIAL TEXT *                                      
         SPACE                                                                  
         LA    R1,IDCHARS                                                       
DK10     CLC   DTXKDESC(1),0(R1)                                                
         BE    DK20                                                             
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         BNE   DK10                                                             
         B     DK30                                                             
DK20     CLC   DTXKDESC+1(3),=C'MG='                                            
         BNE   DK30                                                             
         MVC   WORK+1(3),DTXKDESC+1 MOVE MG=                                    
         SPACE                                                                  
* LOOK UP 1 CHAR MARKET GROUP IN THE CONVERT TABLE                              
         SPACE                                                                  
         L     R1,=A(SPMGRTAB)                                                  
         A     R1,SPTR13RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
         SPACE                                                                  
DK22     CLC   DTXKDESC+4(1),2(R1) IS THIS IT                                   
         BE    DK25                                                             
         LA    R1,3(R1)                                                         
         BCT   RF,DK22                                                          
         SPACE                                                                  
         DC    H'0'                SHOULD NOT BE (NOT IN TABLE)???              
         SPACE                                                                  
DK25     MVC   WORK+4(2),0(R1)     MOVE IN SCHEME                               
         SPACE                                                                  
         UNPK  DUB(5),DTXKDESC+5(3)                                             
         SPACE                                                                  
         CLI   WORK+5,X'40'        SEE IF SECOND CHAR IS BLANK                  
         BNH   DK28                                                             
         SPACE                                                                  
         MVC   WORK+6(4),DUB        MOVE IN THE NUMBER                          
         B     DK30                                                             
         SPACE                                                                  
DK28     MVC   WORK+5(4),DUB        MOVE IN THE NUMBER                          
         SPACE                                                                  
DK30     CLC   TRAID,WORK                                                       
         BE    *+14                                                             
         MVC   TRAID,WORK          MOVE IN TEXT ID                              
         OI    TRAIDH+6,X'80'      SET ON TRANSMIT BIT                          
         SPACE                                                                  
         ZIC   RE,DTXKTYP                                                       
         LA    RF,C'L'-1                                                        
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         XC    WORK(L'TRAPG),WORK                                               
         UNPK  WORK(2),DUB                                                      
         CLI   WORK,C'0'                                                        
         BNE   DK34                                                             
         MVC   WORK(1),WORK+1                                                   
         MVI   WORK+1,C' '                                                      
         SPACE                                                                  
DK34     CLC   TRAPG,WORK                                                       
         BE    *+14                                                             
         MVC   TRAPG,WORK          MOVE IN TEXT ID                              
         OI    TRAPGH+6,X'80'      SET ON TRANSMIT BIT                          
         SPACE                                                                  
         MVC   BAGYMD,DTXKAM                                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE 3                                                                
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR22                NO, CONTINUE WITH READ SEQUENTIAL            
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    RECCT,RECCT                                                      
         SPACE                                                                  
* BUILD KEY, AND DO READHI                                                      
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING DTXKEY,R4                                                        
         MVC   DTXKID(2),=XL2'0A2D'                                             
         MVC   DTXKAM,BAGYMD                                                    
         MVC   DTXKCLT,BCLT                                                     
         SPACE                                                                  
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
         SPACE                                                                  
         LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF CMLKCML                      
LR02     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   LR04                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,LR02                                                          
LR04     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         B     LR100               GO SEND NO SEL MSG                           
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         SPACE                                                                  
LR22     LA    R4,KEY                                                           
         CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LR100                YES                                         
         CLC   KEY(2),=XL2'0A2D'   TO INSURE                                    
         BL    LR20                                                             
         BH    LR100                                                            
         CLC   BAGYMD,KEY+2      ONLY WANTED KEYS ARE PASSED                    
         BL    LR20                                                             
         BH    LR100                                                            
         SPACE                                                                  
* ONLY LIST SPECIAL OR DEALER TEXT, NOT BOTH *                                  
         SPACE                                                                  
LR30     LA    R1,IDCHARS          SPECIAL TEXT CHAR TABLE                      
         SPACE                                                                  
LR34     CLC   0(1,R1),DTXKDESC    THIS SPECIAL TEXT                            
         BE    LR36                 YES                                         
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         BNE   LR34                                                             
         CLI   CONREC,C'D'         THIS DEALER TEXT                             
         BE    LR40                 YES                                         
         SPACE                                                                  
         CLI   DTXKCLT,C'*'        OFFICE RECORD                                
         BNE   LR20                                                             
         CLI   CONREC,C'S'         STEXT RECORD                                 
         BNE   LR20                                                             
         SPACE                                                                  
LR36     CLI   CONREC,C'S'         THIS SPECIAL TEXT                            
         BNE   LR20                 NO, BYPASS                                  
LR40     ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   LR100               YES, ALL DONE                                
         OC    ID,ID               WAS ID FILTER ENTERED                        
         BZ    LR60                                                             
         ZIC   R1,CMTIDLEN         GET LEN FOR FILTER                           
         EX    R1,LRCLCA           SEE IF THIS REQ CMTID                        
         BNE   LR20                NO                                           
         SPACE                                                                  
LR60     LA    R4,KEY                                                           
         USING DTXKEY,R4                                                        
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   LR62                                                             
         CLI   BCLT,C'*'           IF BY OFFICE                                 
         BNE   LR62                                                             
         CLC   BCLT,DTXKCLT                                                     
         BE    LR62                                                             
         MVI   FORCEHED,C'Y'       PAGE BREAK ON OFFICE                         
         SPACE                                                                  
LR62     MVC   BCLT,DTXKCLT                                                     
         CLI   BCLT,C'*'           IF BY OFFICE                                 
         BE    LR63                                                             
         BAS   RE,FCLT             FIND CLIENT                                  
         BNE   LR20                                                             
         B     LR85                                                             
         SPACE                                                                  
LR63     DS    0H                                                               
         CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
         BE    LR70                YES, NET IS BY OFFICE ONLY                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING                                
         BNZ   LR20                                                             
         SPACE                                                                  
         MVI   ERROR,0                                                          
         SPACE                                                                  
LR70     DS    0H                                                               
         CLI   DTXKCLT,C'*'        OFFICE                                       
         BNE   LR85                                                             
         SPACE                                                                  
         MVC   SVOFF,DTXKCLT+1                                                  
         BAS   RE,VOFF                                                          
         BNE   LR20                                                             
         SPACE                                                                  
LR85     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         SPACE                                                                  
* CHECK FILTERS HERE - IF NEEDED                                                
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
LRCLC    CLC   COMPKEY(0),KEY                                                   
LRCLCA   CLC   ID(0),KEY+5                                                      
LR100    OC    RECCT,RECCT                                                      
         BNZ   EXIT                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    EXIT                GO FORMAT FOR OFFLINE REPORT                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* FORMAT ONLINE LIST                                                            
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES                                                    
         MVC   BCLT,DTXKCLT                                                     
         CLI   BCLT,C'*'           IF BY OFFICE                                 
         BNE   LRL05                                                            
         BAS   RE,CNVOFF                                                        
         BNE   LRL05                                                            
         MVC   LCLT(3),WORK                                                     
         B     LRL08                                                            
         SPACE                                                                  
LRL05    GOTO1 CLUNPK,DMCB,(SVCPROF6,DTXKCLT),LCLT                              
         SPACE                                                                  
LRL08    MVC   LID(7),DTXKDESC                                                  
         SPACE                                                                  
* TEST IF ANY SPECIAL CHARS SPECIAL TEXT *                                      
         SPACE                                                                  
         LA    R1,IDCHARS                                                       
LRL10    CLC   DTXKDESC(1),0(R1)                                                
         BE    LRL20                                                            
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         BNE   LRL10                                                            
         B     LRL30                                                            
LRL20    CLC   DTXKDESC+1(3),=C'MG='                                            
         BNE   LRL30                                                            
         MVC   LID+1(3),DTXKDESC+1                                              
         SPACE                                                                  
* LOOK UP 1 CHAR MARKET GROUP IN THE CONVERT TABLE                              
         SPACE                                                                  
         L     R1,=A(SPMGRTAB)                                                  
         A     R1,SPTR13RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
         SPACE                                                                  
LRL24    CLC   DTXKDESC+4(1),2(R1) IS THIS IT                                   
         BE    LRL25                                                            
         LA    R1,3(R1)                                                         
         BCT   RF,LRL24                                                         
         SPACE                                                                  
         DC    H'0'                SHOULD NOT BE (NOT IN TABLE)???              
         SPACE                                                                  
LRL25    MVC   LID+4(2),0(R1)      MOVE IN SCHEME                               
         SPACE                                                                  
         UNPK  DUB(5),DTXKDESC+5(3)                                             
         SPACE                                                                  
         CLI   LID+5,X'40'         SEE IF SECOND CHAR IS BLANK                  
         BNH   LRL26                                                            
         SPACE                                                                  
         MVC   LID+6(4),DUB         MOVE IN THE NUMBER                          
         B     LRL30                                                            
         SPACE                                                                  
LRL26    MVC   LID+5(4),DUB         MOVE IN THE NUMBER                          
         SPACE                                                                  
*NOP     LA    R1,LID+9                                                         
*        CLI   0(R1),C'0'                                                       
*        BNE   LRL30                                                            
*        MVI   0(R1),C' '                                                       
******** BCT   R1,*-12                                                          
         SPACE                                                                  
LRL30    ZIC   RE,DTXKTYP                                                       
         LA    RF,C'L'-1                                                        
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         UNPK  LPG(2),DUB                                                       
         CLI   LPG,C'0'                                                         
         BNE   *+8                                                              
         MVI   LPG,C' '                                                         
         SPACE                                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DTXTLEEL,R6                                                      
         MVC   LTITLE,DTXTITLE                                                  
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DTXTXTEL,R6                                                      
         ZIC   R1,DTXTXTLN                                                      
         SH    R1,=H'4'            GET TEXT LEN-1                               
         CH    R1,=AL2(L'LTEXT-1)                                               
         BNH   *+8                                                              
         LH    R1,=AL2(L'LTEXT-1)                                               
         EX    R1,LRLMVC                                                        
         LH    R1,RECCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,RECCT                                                         
         GOTO1 LISTMON                                                          
         B     LR20                                                             
LRLMVC   MVC   LTEXT(0),DTXTXT-DTXTXTEL(R6)                                     
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
         SPACE                                                                  
LRR      LA    R5,P                                                             
         CLC   BCLT,DTXKCLT                                                     
         BE    LRR06                                                            
         XC    KEYCHAIN,KEYCHAIN      CLEAR NEXT LIST                           
         MVC   BCLT,DTXKCLT                                                     
         GOTO1 CLUNPK,DMCB,(SVCPROF6,DTXKCLT),QCLT                              
         MVC   LASTKEY,KEY                                                      
         MVC   AIO,AIO2                                                         
         SPACE                                                                  
         LA    R1,KEY                                                           
         USING CLTHDR,R1                                                        
         XC    KEY,KEY                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT SYSTEM                      
         MVC   CLTNM,=CL20'** UNKNOWN **'                                       
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   LRR04                                                            
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT SYSTEM                      
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING CLTHDR,R1                                                        
         MVC   CLTNM,CNAME                                                      
         DROP  R1                                                               
         SPACE                                                                  
LRR04    DS   0H                                                                
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY(L'LASTKEY),LASTKEY  RESTORE KEY AND DISK ADDR                
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         MVI   FORCEHED,C'Y'                                                    
LRR06    MVC   PID(7),DTXKDESC                                                  
         SPACE                                                                  
* TEST IF ANY SPECIAL CHARS SPECIAL TEXT *                                      
         SPACE                                                                  
         LA    R1,IDCHARS                                                       
LRR10    CLC   DTXKDESC(1),0(R1)                                                
         BE    LRR12                                                            
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         BNE   LRR10                                                            
         B     LRR18                                                            
LRR12    CLC   DTXKDESC+1(3),=C'MG='                                            
         BNE   LRR18                                                            
         MVC   PID+1(3),DTXKDESC+1                                              
         SPACE                                                                  
* LOOK UP 1 CHAR MARKET GROUP IN THE CONVERT TABLE                              
         SPACE                                                                  
         L     R1,=A(SPMGRTAB)                                                  
         A     R1,SPTR13RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
         SPACE                                                                  
LRR14    CLC   DTXKDESC+4(1),2(R1) IS THIS IT                                   
         BE    LRR15                                                            
         LA    R1,3(R1)                                                         
         BCT   RF,LRR14                                                         
         SPACE                                                                  
         DC    H'0'                SHOULD NOT BE (NOT IN TABLE)???              
         SPACE                                                                  
LRR15    MVC   PID+4(2),0(R1)      MOVE IN SCHEME                               
         SPACE                                                                  
         UNPK  DUB(5),DTXKDESC+5(3)                                             
         SPACE                                                                  
         CLI   PID+5,X'40'         SEE IF SECOND CHAR IS BLANK                  
         BNH   LRR16                                                            
         SPACE                                                                  
         MVC   PID+6(4),DUB         MOVE IN THE NUMBER                          
         B     LRR18                                                            
         SPACE                                                                  
LRR16    MVC   PID+5(4),DUB         MOVE IN THE NUMBER                          
         SPACE                                                                  
LRR18    L     R6,AIO                                                           
         SPACE                                                                  
         ZIC   RE,DTXKTYP                                                       
         LA    RF,C'L'-1                                                        
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         UNPK  PPAGE(2),DUB                                                     
         CLI   PPAGE,C'0'                                                       
         BNE   *+8                                                              
         MVI   PPAGE,C' '                                                       
         SPACE                                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DTXTLEEL,R6                                                      
         MVC   PTITLE,DTXTITLE                                                  
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         USING DTXTXTEL,R6                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
LRR20    LA    R0,4                                                             
         LA    R5,P                                                             
LRR22    ZIC   R1,DTXTXTLN                                                      
         SH    R1,=H'4'            GET TEXT LEN-1                               
         EX    R1,LRRMVC                                                        
         CLC   0(132,R5),SPACES    IF STILL BLANK LINE                          
         BNE   *+8                                                              
         MVI   0(R5),0             FORCE LINE TO PRINT                          
         BAS   RE,NEXTEL                                                        
         BNE   LRR30                                                            
         LA    R5,132(,R5)                                                      
         BCT   R0,LRR22                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR20                                                            
LRR30    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
LRRMVC   MVC   PTEXT-P(0,R5),DTXTXT                                             
         EJECT                                                                  
* DELETE RECORD - CHECK IF ANY PAGES AFTER THIS ONE *                           
         SPACE                                                                  
DEL      L     R4,AIO                                                           
         LR    R6,R4                                                            
         SPACE                                                                  
         CLI   ID,C'-'             THIS A PATTERN TEXT REC                      
         BNE   DEL10                                                            
         MVI   ELCODE,X'70'        GET ANY PATTERN LINK ELEM                    
         BAS   RE,GETEL                                                         
         BNE   DEL06                NONE, GET ON WITH IT                        
         USING DTXPATEL,R6                                                      
DEL00    CLI   DTXPATST,01         IS THIS PATTERN ACTIVE                       
         BE    PATXTER             CAN'T DELETE IF LINKED                       
         BAS   RE,NEXTEL                                                        
         BE    DEL00                                                            
DEL06    LR    R6,R4                                                            
         SPACE                                                                  
DEL10    MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL            ANY LINK FOR NEXT REC                        
         BE    NXTRECER                                                         
         USING DTXKEY,R4                                                        
         CLI   DTXKTYP,C'L'        THIS PAGE 1                                  
         BE    EXIT                                                             
         MVC   KEY(13),DTXKEY                                                   
         LA    R4,KEY                                                           
         ZIC   RE,DTXKTYP                                                       
         BCTR  RE,0                                                             
         STC   RE,DTXKTYP                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         SPACE                                                                  
* DELETE NEXT (LINK) ELEM IN PREV REC *                                         
         SPACE                                                                  
         MVI   ELCODE,X'50'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'50' ELEMENTS               
         SPACE                                                                  
         GOTO1 PUTREC                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   DTXKEY,0(R6)                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
*================================================================               
* RESTORE RECORD- CHECK IF ANY PAGES BEFORE THIS ONE ARE INACTIVE               
*================================================================               
         SPACE                                                                  
BREST    L     R4,AIO                                                           
         LR    R6,R4                                                            
         TM    15(R6),X'80'        RECORD DELETED?                              
         BZ    EXIT                                                             
         USING DTXKEY,R4                                                        
         CLI   DTXKTYP,C'L'        THIS PAGE 1                                  
         BE    EXIT                                                             
         MVC   KEY(13),DTXKEY                                                   
         LA    R4,KEY                                                           
         ZIC   RE,DTXKTYP          CURRENT PAGE                                 
         BCTR  RE,0                MINUS 1                                      
         STC   RE,DTXKTYP          IS IT ACTIVE ?                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RESTERR             RESTORE PREVOIUS PAGE                        
         CLI   KEY+13,X'80'                                                     
         BE    RESTERR             RESTORE PREVOIUS PAGE                        
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   DTXKEY,0(R6)                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVI   RDUPDATE,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================          
* AFTER RESTORE RECORD ADD LINK ELEMENT TO THE PREVIOUS RECORD IF ANY           
*=====================================================================          
         SPACE                                                                  
AREST    L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING DTXKEY,R4                                                        
         CLI   DTXKTYP,C'L'        THIS PAGE 1                                  
         BE    EXIT                                                             
         MVC   KEY(13),DTXKEY                                                   
         LA    R4,KEY                                                           
         ZIC   RE,DTXKTYP          CURRENT PAGE                                 
         BCTR  RE,0                MINUS 1                                      
         STC   RE,DTXKTYP          IS IT ACTIVE ?                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                 RESTORE PREVOIUS PAGE                        
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         SPACE                                                                  
* ADD (LINK) ELEM IN PREV REC *                                                 
         SPACE                                                                  
         MVI   ELCODE,X'50'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE ALL X'50' ELEMENTS               
         LA    R6,ELEM                                                          
         USING DTXNXTEL,R6                                                      
         MVI   DTXNXTEL,X'50'                                                   
         MVI   DTXNXTLN,9                                                       
         MVC   DTXNEXT,ID                                                       
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         GOTO1 PUTREC                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   DTXKEY,0(R6)                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVI   RDUPDATE,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,(SVCPROF6,BCLT),QCLT                                 
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
FCLT20   DS    0H                                                               
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   ERROR,0             SET CC FOR RETURN                            
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE ID, AND IF 1ST CHAR SPEC CHAR, MARKET/STATION                        
         SPACE                                                                  
VID      NTR1                                                                   
         SPACE                                                                  
* ONLY ALLOW SPECIAL OR DEALER TEXT, NOT BOTH *                                 
         SPACE                                                                  
         LA    R1,IDCHARS          SPECIAL TEXT CHAR TABLE                      
VID02    CLC   0(1,R1),TRAID       THIS SPECIAL TEXT                            
         BE    VID04               YES, ALL DONE                                
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         BNE   VID02                                                            
         SPACE                                                                  
         CLI   CONREC,C'D'         THIS DEALER TEXT                             
         BE    VID06                YES                                         
         B     SPLTXTER                                                         
         SPACE                                                                  
VID04    CLI   CONREC,C'S'         THIS SPECIAL TEXT                            
         BNE   DLRTXTER             NO, ERROR                                   
VID06    ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VIDMVC           MOVE MGROUP TO ID                            
         STC   R1,CMTIDLEN         STORE LEN FOR FILTER IN LIST                 
         CLI   5(R2),1            IF ONLY 1 CHAR, IGNORE                        
         BE    EXIT                                                             
         SPACE                                                                  
* TEST IF ANY SPECIAL CHARS SPECIAL TEXT *                                      
         SPACE                                                                  
         LA    R1,IDCHARS                                                       
VID10    CLC   ID(1),0(R1)                                                      
         BE    VID20                                                            
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         BNE   VID10                                                            
         B     EXIT                                                             
         SPACE                                                                  
* NOW SEE IF MARKET OR STATION SPECIAL TEXT *                                   
         SPACE                                                                  
VID20    CLC   ID+1(3),=C'ES='                                                  
         BE    VID60                                                            
         CLC   ID+1(5),=C'TYPE='                                                
         BE    VID70                                                            
         CLC   ID+1(3),=C'MG='                                                  
         BE    VID80                                                            
         CLI   ID,C'='             FAX LETTER ID                                
         BE    *+12                                                             
         CLI   ID,C'-'             PATTERN SPECIAL TEXT                         
         BNE   VID22                                                            
         OC    ID,SPACES                                                        
         B     EXIT                                                             
         SPACE                                                                  
VID22    MVC   FLDH,0(R2)                                                       
         MVC   FLDH+5(1),CMTIDLEN  DATA LENGTH                                  
         MVI   ERROPT,C'Y'         SET ERROPT FOR RETURN HERE                   
         XC    ID+1(6),ID+1                                                     
         MVC   FLD(L'TRAID-1),9(R2) MOVE IN MARKET OR STA                       
         LA    R2,FLDH                                                          
         LA    RE,8(,R2)          1ST CHAR OF MKT/STA                           
         ZIC   RF,CMTIDLEN        MAX CHARS                                     
         SPACE                                                                  
VID24    CLI   0(RE),C'0'                                                       
         BL    VID40                                                            
         CLI   0(RE),C'9'                                                       
         BH    VID40                                                            
         LA    RE,1(,RE)                                                        
         BCT   RF,VID24                                                         
         SPACE                                                                  
* MUST BE MARKET, ALL NUMERIC, CK LEN *                                         
         SPACE                                                                  
VID30    CLI   5(R2),4                                                          
         BH    MKTSIZER                                                         
         MVI   FLDH+4,X'08'        SET ON NUMERIC                               
         GOTO1 VALIMKT                                                          
         MVC   ID+1(4),QMKT                                                     
         B     VID50                                                            
         SPACE                                                                  
VID40    DS    0H                                                               
         CLI   0(RE),C'/'          IF LAST CHAR IS A /                          
         BNE   VID42                                                            
         CLI   CMTIDLEN,5          AND FIELD LENGTH IS 5                        
         BNE   VID42                                                            
         BCTR  RF,0                                                             
         LTR   RF,RF               THEN MUST BE CABLE STATION                   
         BNZ   VID42                                                            
         MVC   ID+1(5),8(R2)                                                    
         MVI   ERROPT,0            RESET ERROPT                                 
         B     VID56                                                            
         EJECT                                                                  
* MUST BE STATION, NOT ALL NUMERIC *                                            
         SPACE                                                                  
VID42    MVI   FLDH+4,X'04'        SET TO ALPHA                                 
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALISTA                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   INVERR              INVALID ENTRY ERROR                          
         SPACE                                                                  
         CLI   QSTA+4,C'T'                                                      
         BNE   VID44                                                            
         MVI   QSTA+4,0                                                         
         CLI   QSTA+3,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+3,0                                                         
VID44    MVC   ID+1(5),QSTA                                                     
         SPACE                                                                  
VID50    LA    R2,TRAIDH                                                        
         MVI   ERROPT,0            RESET ERROPT                                 
         CLI   ERROR,0             IF NON-ZERO, ERROR                           
         BNE   TRAPERR             BAD MARKET                                   
VID56    XC    TRAID,TRAID                                                      
         MVC   TRAID(7),ID                                                      
         OI    TRAIDH+6,X'80'                                                   
         B     EXIT                                                             
         SPACE                                                                  
* VALIDATE SPECIAL TEXT BY ESTIMATE *                                           
         SPACE                                                                  
VID60    CLI   5(R2),7                                                          
         BH    IDESTERR                                                         
         LA    RE,ID+6                                                          
         LA    RF,3                                                             
         LA    R1,DUB+2                                                         
         MVC   DUB(3),=3C'0'                                                    
VID62    CLI   0(RE),0                                                          
         BE    VID64                                                            
         CLI   0(RE),C' '                                                       
         BE    VID64                                                            
         CLI   0(RE),C'0'                                                       
         BL    IDESTERR                                                         
         CLI   0(RE),C'9'                                                       
         BH    IDESTERR                                                         
         MVC   0(1,R1),0(RE)                                                    
         BCTR  R1,0                                                             
VID64    BCTR  RE,0                                                             
         BCT   RF,VID62                                                         
         CLC   DUB(3),=3C'0'                                                    
         BE    IDESTERR                                                         
         MVC   ID+4(3),DUB                                                      
         B     VID56                                                            
         EJECT                                                                  
* VALIDATE SPECIAL TEXT BY STATION TYPE *                                       
         SPACE                                                                  
VID70    CLI   5(R2),7                                                          
         BH    STYPERR                                                          
         B     EXIT                                                             
         SPACE                                                                  
* VALIDATE SPECIAL TEXT BY MARKET GROUP *                                       
         SPACE                                                                  
VID80    CLI   5(R2),10                                                         
         BH    MGRPERR                                                          
         CLI   5(R2),6                                                          
         BL    MGRPERR                                                          
         SPACE                                                                  
         CLI   ID+4,C'A'                                                        
         BL    MGRPERR                                                          
         CLI   ID+4,C'Z'                                                        
         BH    MGRPERR                                                          
         SPACE                                                                  
         MVC   MKTGID,ID+4         MARKET GROUP ID                              
         SPACE                                                                  
         ZIC   RF,5(R2)            GET INPUT LENGTH                             
         SHI   RF,5                MINUS 5 (*MG=X)                              
         SPACE                                                                  
         LA    R1,13(,R2)          POINT TO 2ND CHAR OR NUMBER                  
         SPACE                                                                  
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
         SPACE                                                                  
         CLI   0(R1),C'A'          SEE IF 2 CHAR MARKET GROUP                   
         BL    VID84                                                            
         CLI   0(R1),C'Z'                                                       
         BH    VID84                                                            
         LA    R1,1(R1)                                                         
         SPACE                                                                  
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
         SPACE                                                                  
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,SPTR13RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
         SPACE                                                                  
VID81    CLC   12(2,R2),0(RE)      IS THIS IT                                   
         BE    VID82                                                            
         LA    RE,3(RE)                                                         
         BCT   RF,VID81                                                         
         SPACE                                                                  
         B     MGRPERR                                                          
         SPACE                                                                  
VID82    MVC   MKTGID,2(RE)        MOVE HEX VALUE FROM TABLE                    
         SPACE                                                                  
         LA    RE,DUB                                                           
         SPACE                                                                  
         ZIC   RF,5(R2)            GET INPUT LENGTH                             
         SHI   RF,6                MINUS 5 (*MG=XX)                             
         SPACE                                                                  
VID84    CLI   0(R1),C'0'                                                       
         BL    MGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    MGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VID84                                                         
         SPACE                                                                  
         PACK  WORK(3),DUB(5)                                                   
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(1),MKTGID                                                  
         MVC   KEY+9(2),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         SPACE                                                                  
         MVC   FILENAME,=CL8'SPTDIR'                                            
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    VID86                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   BDMGRPER                                                         
         SPACE                                                                  
VID86    MVC   ID+5(2),WORK        MGROUP PWOS                                  
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
VIDMVC   MVC   ID(0),8(R2)         SAVE TEXT ID                                 
         SPACE                                                                  
INVERR   MVI   ERROR,INVALID                                                    
         LA    R2,TRAIDH                                                        
         B     TRAPERR                                                          
         EJECT                                                                  
* VALIDATE PAGE NUMBER - AND ALL PREVIOUS RECORDS TO EXIST *                    
         SPACE                                                                  
VPG      NTR1                                                                   
         GOTO1 VALINUM  VALIDATE TO BE NUMERIC                                  
         ZIC   RE,ACTUAL                                                        
         ZIC   RF,=C'L'                                                         
         LTR   RE,RE               IF ZERO, DON'T SUBTRACT                      
         BZ    VPG10                                                            
         BCTR  RE,0                                                             
VPG10    AR    RE,RF                                                            
         STC   RE,PAGNO                                                         
         SPACE                                                                  
* IF AN ADD, AND NOT PAGE 0/1, TEST IF PREV RECORD EXISTS *                     
         SPACE                                                                  
         CLI   PAGNO,C'L'          IF PAGE 1, NO PREVIOUS                       
         BE    EXIT                                                             
         CLI   ACTNUM,ACTADD       ONLY CK IF ADD                               
         BNE   EXIT                                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DTXKEY,R4                                                        
         MVC   DTXKID,=XL2'0A2D'                                                
         MVC   DTXKAM,BAGYMD                                                    
         MVC   DTXKCLT,BCLT                                                     
         MVC   DTXKDESC,ID                                                      
         BCTR  RE,0                                                             
         STC   RE,DTXKTYP                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TXTPGERR                                                         
         B     EXIT                                                             
         PRINT GEN                                                              
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* NOTE - SINCE NO MODE CHECK FOR XRECADD BEFORE I PUT IN THE CODE TO            
* ADD MEDIA N REC WHEN ADDING MEDIA T FOR CANADA, NOP GENR CALL                 
                                                                                
AAR      CLI   SPOTCAN,C'C'        TEST CANADA                                  
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
AARX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* AFTER PUT ROUTINE, MARK LINKED PATTERN RECS AS UPDATED *                      
         SPACE                                                                  
APR      NTR1                                                                   
         L     R6,AIO1                                                          
         MVI   ELCODE,X'70'                                                     
         BAS   RE,GETEL                                                         
         BNE   APRX                                                             
         SPACE                                                                  
         USING DTXPATEL,R6                                                      
         MVC   SVKEY,KEY           SAVE KEY                                     
         SPACE                                                                  
APR10    CLI   9(R6),00            THIS AN INACTIVE ELEMENT                     
         BE    APR50                YES                                         
         CLI   9(R6),01            THIS AN ACTIVE ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R6               SAVE ELEM ADDRESS                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4           GO GET LINKED PATTERN                        
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(3),BAGYMD     & BCLT                                      
         MVC   PATKPRD(5),DTXPATP1 & SLN1, PRD2, SLN2, & COPY                   
         SR    R0,R0                                                            
         ICM   R0,3,DTXPATRF                                                    
         DROP  R6                                                               
         X     R0,=X'00003FFF'     GET COMPLEMENT                               
         SLL   R0,10                                                            
         STCM  R0,7,PATKREF        WILL READ MOST RECENT PATTERN                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     SAME TYPE,A/M, CLT, PRD/SLN, CODE            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,PATKREF                                                     
         ICM   RF,7,PATKREF-PATKEY+KEYSAVE                                      
         SRL   RE,10                                                            
         SRL   RF,10                                                            
         CR    RE,RF               ONLY CARE ABOUT REF, NOT SUBLINE             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ID,2(R6)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'        UPDATE PATTERN DATA ELEM                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         OI    PATSTAT,X'01'       SET ON UPDATED PAT TEXT                      
         GOTO1 PUTREC                                                           
         SPACE                                                                  
         MVI   ELCODE,X'70'                                                     
         LR    R6,R2               RESTORE                                      
APR50    BAS   RE,NEXTEL                                                        
         BE    APR10               UPDATE NEXT LINKED PATTERN REC               
         MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1                                                         
APRX     B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         CLI   CONREC,C'D'         THIS DEALER TEXT                             
         BE    HDHK10               YES                                         
         MVC   H1+40(7),=C'SPECIAL'                                             
         MVI   H2+40,C'-'                                                       
HDHK10   MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         CLI   BCLT,C'*'           BY OFFICE                                    
         BNE   HDHK20                                                           
         SPACE                                                                  
         BAS   RE,CNVOFF           CONVERT ONE BYTE OFFICE CODE                 
         BNE   HDHK20                                                           
         MVC   H4+10(3),WORK                                                    
         B     EXIT                                                             
         SPACE                                                                  
HDHK20   GOTO1 CLUNPK,DMCB,(SVCPROF6,BCLT),QCLT                                 
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         B     EXIT                                                             
         SPACE 3                                                                
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
         SPACE                                                                  
GOFF     NTR1                                                                   
         SPACE                                                                  
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
         SPACE                                                                  
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
         SPACE                                                                  
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
         SPACE                                                                  
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         SPACE                                                                  
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
         MVC   LAOFFICE,COFFICE    USE MEDIA OFFICE                             
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         XIT1                                                                   
         SPACE 3                                                                
* CONVERT 1 BYTE OFFICE CODE AND PRINT 2 CHAR CODE                              
         SPACE                                                                  
CNVOFF   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,BCLT+1                                                    
         SPACE                                                                  
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   CNV20               JUST CONVERT, DO NOT VALIDATE                
         SPACE                                                                  
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    CNV10               NO, VALIDATE AND CONVERT                     
         SPACE                                                                  
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    CNV10               VALIDATE AND CONVERT                         
         SPACE                                                                  
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BNE   CNV20               JUST CONVERT                                 
         SPACE                                                                  
CNV10    MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
         SPACE                                                                  
CNV20    L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CNVOFFX                                                          
         SPACE                                                                  
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    CNVOFFX             2 CHAR OFFICE IS NOT ON                      
         SPACE                                                                  
         MVI   WORK,C'*'                                                        
         MVC   WORK+1(2),OFCOFC2                                                
         SPACE                                                                  
         CR    RB,RB               SET EQ CC                                    
         SPACE                                                                  
CNVOFFX  XIT1                                                                   
         SPACE                                                                  
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* *********************************                                             
*        ERROR ROUTINES                                                         
* *********************************                                             
INVCLERR MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
         SPACE                                                                  
BADOFFER MVC   GERROR,=Y(BADOFF)                                                
         GOTO1 VTRAERR             USING GETTXT CALL                            
         SPACE                                                                  
INVPOSN  MVC   CONHEAD,ERRPOSN                                                  
         B     ERREXIT                                                          
INVPFKY  MVC   CONHEAD,ERRPFKY                                                  
         B     ERREXIT                                                          
IDESTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'IDESTMS),IDESTMS                                       
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
STYPERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'STYPMS),STYPMS                                         
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
MGRPERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MGRPMS),MGRPMS                                         
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
BDMGRPER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMGRPMS),BDMGRPMS                                     
         MVC   CONHEAD+26(5),13(R2)                                             
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
MKTSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MKTSIZMS),MKTSIZMS                                     
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
TXTPGERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TXPGERMS),TXPGERMS                                     
         B     ERREXIT                                                          
DTXTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DTXTERMS),DTXTERMS                                     
         B     ERREXIT                                                          
DLRTXTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DLRTXTMS),DLRTXTMS                                     
         B     ERREXIT                                                          
SPLTXTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SPLTXTMS),SPLTXTMS                                     
         B     ERREXIT                                                          
NXTRECER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NXTRECMS),NXTRECMS                                     
         B     ERREXIT                                                          
PATXTER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PATXTMS),PATXTMS                                       
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
RESTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RESTMSG),RESTMSG                                       
         LA    R2,TRAIDH                                                        
         B     ERREXIT                                                          
         SPACE                                                                  
NOIDERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOIDMSG),NOIDMSG                                       
         B     ERREXIT                                                          
NOIDMSG  DC    C'* ERROR * FIELD SHOULD BE BLANK *'                             
         SPACE                                                                  
INVOFERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOFMSG),INVOFMSG                                     
         B     ERREXIT                                                          
INVOFMSG DC    C'* ERROR * INVALID OFFICE *'                                    
         SPACE                                                                  
HDNTER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'* ERROR * MORE THAN 7 LINES && BOX=Y *'           
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
DTXLENER MVI   ERROR,INVTXTLN      TEXT LINE TOO LONG                           
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING       NO DATA ENTERED, REQUIRED                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
ERRPFKY  DC    CL60'* ERROR * UNDEFINED FUNCTION KEY *'                         
ERRPOSN  DC   CL60'* ERROR * CURSOR MUST BE ON A TEXT LINE FOR PF3/4 *'         
IDESTMS  DC    CL38'* ERROR * SPECIAL TEXT BY EST ES=000 *'                     
STYPMS   DC    CL38'* ERROR * SPECIAL TEXT BY STA TYPE=X *'                     
MGRPMS   DC    C'* ERROR * MKT GROUP MUST BE 1-2 CHAR && 1-4 DIGITS *'          
BDMGRPMS DC    CL39'* ERROR * NO MARKET GROUP X0000 FOUND *'                    
TXPGERMS DC    C'* ERROR * NO PREVIOUS PAGE TEXT EXISTS *'                      
DLRTXTMS DC    C'* ERROR * DEALER TEXT CAN''T START WITH *$&&@#/ *'             
SPLTXTMS DC    C'* ERROR * SPECIAL TEXT MUST START WITH *$&&@#/ *'              
MKTSIZMS DC    CL60'* ERROR * SPECIAL TEXT = 1 SPECIAL CHAR, 1-4 DIGITSC        
                OF MKT *'                                                       
DTXTERMS DC    C'* ERROR * NO DEALER TEXT RECORD FOUND *'                       
NXTRECMS DC    C'* ERROR * DELETE LAST PAGE FIRST *'                            
PATXTMS  DC    C'* ERROR * PATTERN(S) STILL USING TEXT *'                       
RESTMSG  DC    C'* ERROR * RESTORE PREVIOUS PAGE *'                             
         SPACE                                                                  
* 1ST 6 CHARACTERS ARE FOR SPECIAL TEXT ON INSTRUCTIONS,                        
* = IS USED FOR FAX LETTERS, - IS FOR PATTEN SPECIAL TEXT                       
         SPACE                                                                  
IDCHARS  DC    C'*$&&@#/=-'                                                     
         DC    X'FF'                                                            
         SPACE                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,42,C'DEALER TEXT LIST'                                        
         SSPEC H2,42,C'----------------'                                        
         SSPEC H1,71,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,71,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,83,RUN                                                        
         SSPEC H4,71,REPORT                                                     
         SSPEC H5,71,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H7,3,C'CMT-ID '                                                  
         SSPEC H8,3,C'-------'                                                  
         SSPEC H7,16,C'PAGE'                                                    
         SSPEC H8,16,C'----'                                                    
         SSPEC H7,28,C'TITLE'                                                   
         SSPEC H8,28,C'------------------------'                                
         SSPEC H7,54,C'TEXT'                                                    
         SSPEC H8,54,C'-------------------------------------------'             
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
* VALIDATE OFFICE CODE                                                          
         SPACE                                                                  
         DS    0H                                                               
VOFF     NMOD1 0,**VOFF**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         MVI   BYTE,0              INIT CC                                      
         SPACE                                                                  
         MVC   SVKEY(13),KEY           SAVE KEY                                 
         SPACE                                                                  
         CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
*NOP     BNE   VOFF05                                                           
         B     VOFF05                                                           
         SPACE                                                                  
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BZ    *+16                                                             
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BE    VOFFX               BYPASS, NOT WORKING YET                      
         B     VOFF50              CLIENT STRING SECURITY                       
         SPACE                                                                  
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VOFF05                                                           
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BNE   VOFF02                                                           
         SPACE                                                                  
         CLC   T216FFD+7(1),SVOFF     MATCH OFFICE CODE                         
         B     VOFFX                                                            
         SPACE                                                                  
VOFF02   CLI   T216FFD+6,C'$'          TEST OFFICE LIST                         
         BE    VOFF50                                                           
         B     VOFFX               SINGLE CLIENT ACCESS                         
         SPACE                                                                  
VOFF05   XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         SPACE                                                                  
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
VOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+12                                                             
         MVI   BYTE,X'FF'                                                       
         B     VOFF100                                                          
         SPACE                                                                  
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+12                                                             
         MVI   BYTE,X'FF'                                                       
         B     VOFF100                                                          
         SPACE                                                                  
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
         SPACE                                                                  
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
         SPACE                                                                  
VOFF30   L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         SPACE                                                                  
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   VOFF40                                                           
         SPACE                                                                  
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
         SPACE                                                                  
VOFF40   CLI   LAOFFICE,0          IS THIS CLT LIMITED ACCESS                   
         BE    VOFF50               NO                                          
         CLC   SVCLTOFF,LAOFFICE   SAME OFFICE                                  
         BNE   VOFF20                                                           
         B     VOFF70                                                           
         SPACE                                                                  
VOFF50   BRAS  RE,COFF             CHK OFFICE VALIDITY                          
         BE    VOFF70                                                           
         SPACE                                                                  
         CLC   SVOFF,SVCLTOFF                                                   
         BNE   VOFF20                                                           
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BE    VOFF20                                                           
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VOFF20                                                           
         SPACE                                                                  
         MVI   BYTE,X'FF'          SET CC                                       
         B     VOFF100                                                          
         SPACE                                                                  
VOFF70   DS    0H                                                               
         CLC   SVOFF,SVCLTOFF      IS THIS THE RIGHT OFFICE                     
         BNE   VOFF20                                                           
*                                                                               
*********************************************************                       
*        OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
*        BZ    VOFF100                                                          
*        SPACE                                                                  
*        CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
*        BE    VOFF55                                                           
*        SPACE                                                                  
*        CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
*        BE    VOFF60                                                           
*        SPACE                                                                  
*        CLI   T216FFD+6,C'$'          TEST OFFICE LIST                         
*        BE    VOFF100                                                          
*        SPACE                                                                  
*        OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
*        BZ    *+16                                                             
*        CLI   OFFLINE,C'Y'        OFFLINE                                      
*        BE    VOFF100             BYPASS, NOT WORKING YET                      
*        B     VOFF70              CLIENT STRING SECURITY                       
*        SPACE                                                                  
*OFF55   CLC   T216FFD+6(2),CKEYCLT    SINGLE CLIENT ACCESS                     
*        BE    VOFF100                                                          
*        SPACE                                                                  
*OFF60   CLC   T216FFD+7(1),SVOFF     MATCH OFFICE CODE                         
*        BNE   VOFF20                                                           
*        B     VOFF100                                                          
*****    SPACE                                                                  
*OFF70   DS    0H                                                               
****************************                                                    
         SPACE                                                                  
VOFF100  XC    FILENAME,FILENAME                                                
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       RESTORE KEY                                  
         GOTO1 HIGH                DUMMY HIGH FOR GETREC                        
         SPACE                                                                  
         CLI   BYTE,0              SET CC                                       
         SPACE                                                                  
VOFFX    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XIT1                                                                   
         EJECT                                                                  
* CHECK OFFICE TO BE VALID *                                                    
         SPACE                                                                  
COFF     NMOD1 0,**COFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
         BE    COFF10              BY OFFICE ONLY                               
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,(SVCPROF6,KEY+2),QCLT                                
         SPACE                                                                  
         USING CLTHDRD,R6                                                       
         SPACE                                                                  
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
         SPACE                                                                  
COFF10   XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFF                                                     
         CLI   SPOTNETF,C'N'       IS THIS NET TRAFFIC                          
*NOP     BE    COFF20              BY OFFICE ONLY                               
         SPACE                                                                  
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         SPACE                                                                  
COFF20   MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
         SPACE                                                                  
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
       ++INCLUDE SPTRDTXT                                                       
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE DDFLDHDR                                                       
FLDHDRL  EQU   FLDDATA-FLDHDRD                                                  
FLDLEND  EQU   FLDLEN-FLDHDRD                                                   
FLDATBD  EQU   FLDATB-FLDHDRD                                                   
FLDILEND EQU   FLDILEN-FLDHDRD                                                  
FLDIINDD EQU   FLDIIND-FLDHDRD                                                  
FLDOINDD EQU   FLDOIND-FLDHDRD                                                  
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
SPTR13RR DS    A                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
RECCT    DS    H                                                                
LASTKEY  DS    CL18                                                             
CMTIDLEN DS    XL1                                                              
ID       DS    CL7                                                              
MKTGID   DS    CL1                 MARKET GROUP ID                              
PAGNO    DS    XL1                                                              
SVOFF    DS    XL1                 SAVE OFFICE                                  
COMPKEY  DS    CL13                COMPARE KEY FOR ONLINE LIST                  
COMPKEYL DS    CL1                                                              
KEYCHAIN DS    CL140                                                            
LAOFFICE DS    XL1                 LIMITED ACCESS OFFICE FOR CLT                
         SPACE 3                                                                
* OFFLINE PRINT DSECT                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PID      DS    CL10                                                             
         DS    CL4                                                              
PPAGE    DS    CL3                                                              
         DS    CL8                                                              
PTITLE   DS    CL24                                                             
         DS    CL2                                                              
PTEXT    DS    CL60                                                             
         SPACE 3                                                                
* ONLINE SCREEN LINE DSECT                                                      
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LID      DS    CL10                                                             
         DS    CL1                                                              
LPG      DS    CL2                                                              
         DS    CL1                                                              
LTITLE   DS    CL24                                                             
         DS    CL1                                                              
LTEXT    DS    CL33                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPTRA13   09/28/16'                                      
         END                                                                    
