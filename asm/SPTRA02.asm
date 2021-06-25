*          DATA SET SPTRA02    AT LEVEL 127 AS OF 05/12/20                      
*PHASE T21602C                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T21602- SPOT TRAFFIC COMMERCIAL MAINT/LIST'                     
*===================================================================            
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ CML SEQ RECORD FOR ADDS (IN PAR RTN)                  
*                    READ IN PRDHDR FOR PROD NAMES IN OFFLINE LIST              
*                    READ IN ACTUAL ID REC IN VID                               
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
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*           - THIRD BASE REG                                                    
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
* LEV 80 MHER JAN/08   NEW COMMERCIAL FIELDS/NEW SCREEN               *         
* LEV 82 SMUR MAR04/08 FIX BAD BRANCH IN CML RESTORE LOGIC            *         
* LEV 83 MNAS MAR20/08 ACCEPT 9-12 CHAR ADID                          *         
* LEV 84 MNAS APR30/08 CREATE PASSIVE PTRS FOR HIDEF AND CENTERCUT    *         
* LEV 85 MNAS MAY23/08 ADD 9-12 ADID FUNCTIONALITY                    *         
* LEV 86 MNAS JUN06/08 FIX ADID FILTERING TO WORK WITH 9-12 SCHEME    *         
* LEV 87 MNAS JUN12/08 BUG FIX OF BAD BRANCH WHEN NO MATCHING DATA    *         
* LEV 89 MNAS JUN17/08 BUG FIX CREATING X'00' POINTERS - SHOULDN'T BE *         
* LEV 90 MNAS JUN26/08 NEW LOGIC FOR PASSIVE POINTER CREATION FOR     *         
*                      HIDEF AND CENTERCUT                            *         
* LEV 100 SMUR OCT31/08 FIX HD, CC BAD DISK ADDRESS ON ADD            *         
* LEV 101 MNAS MAR13/09 ADD HIDEF/CC/ SWAP FIELD                      *         
* LEV 102 MNAS MAR18/09 RELINK:HIDEF/CC SWAP FIELD PROTECTED/ZERO INT *         
* LEV 107 MNAS NOV04/10 FIX:PACKED BIT NOT ON IN CML KEYS FOR ADID'S  *         
* LEV 108 MNAS DEC23/10 FIX:MISSING DISK ADR ON HD/CC PASSIVES WHEN   *         
*                       ADDED DURING ACTION CHANGE FROM LIST          *         
* LEV 109 SMUR DEC20/10 FIX:DELETE/RESTORE LOGIC,CREATES DUP X'20'ELEM*         
* LEV 110 MNAS   APR/11 TELECASTER EXPANSION                          *         
* LEV 111 SMUR MAY17/11 TELECASTER EXPANSION BUG FIX                  *         
* LEV 117 MNAS FEB08/12 FIX TALENT AGENCY PFKEY BUG CAUSING DUMPS     *         
* LEV 120 MNAS JUL23/12 FIX PASSIVE DUPLICATE CHECKING FOR HIDEFS     *         
*              ADD CODE TO FIX CHANGE HISTORY DISPLAY                 *         
* LEV 122 SMUR MAY17/13 CHANGE =PROD TO =SHIP ON 9999 RECORD          *         
* LEV 124 SMUR SEP30/14 FIX TAL TRANSFER EXCL DISPLAY                 *         
*                       TURN OFF CML CHANGED IN OPTICA FLAG           *         
* LEV 125 SMUR MAY18/15 READ PROD POL RECORD FOR TALENT USER          *         
* SPEC-34390  SMUR OFFLINE REPORT FOR ALL CLIENTS WITH REL/RCL FILTERS*         
* SPEC-42467  SMUR BYPASS CML SEQ REC & X'01' WHEN PROCESSING ISCII   *         
*=====================================================================*         
         EJECT                                                                  
T21602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21602*,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR02RR                                                      
*                                                                               
         L     R0,=V(DLFLD)                                                     
         AR    R0,R2                                                            
         ST    R0,VDLFLD                                                        
*                                                                               
D        USING DLCBD,DLCB                                                       
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
                                                                                
*        DO NOT REMOVE THE CODE BELOW - IT DOESN'T HURT ANYTHING!               
                                                                                
         MVC   TRACEIT+6(1),MODE                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEIT                        
*                                                                               
         CLI   SVTTPR3,C'Y'        IF SPOTTAL USER,                             
         BNE   MAIN01                                                           
         CLC   CONACT(3),=C'ADD'                                                
         BNE   MAIN01                                                           
         CLC   TRAPTTL(3),=X'D79996'                                            
         BNE   MAIN01                                                           
*MNUPLOAD                                                                       
*        GOTO1 GETFACT,DMCB,0      GET A(SYSTEM INFO BLOCK)                     
*        L     R1,DMCB                                                          
*        USING FACTSD,R1                                                        
*        TM    FATSTAT6,X'40'                                                   
*        BO    MAIN01                                                           
*        DROP  R1                                                               
*MNUPLOAD                                                                       
         CLI   PFKEY,4                                                          
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
*        CLI   PFKEY,7                                                          
*        BNE   *+8                                                              
*        MVI   PFKEY,0                                                          
         CLI   PFKEY,8                                                          
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
MAIN01   DS    0H                                                               
         NI    GENSTAT2,X'FF'-USMYOK   RESET 'USE MY MESSAGE'                   
         CLI   TRLSTPGM,X'02'                                                   
         BE    *+12                                                             
         MVI   MYSCREEN,0                                                       
         MVI   TRLSTPGM,X'02'                                                   
                                                                                
         CLI   MODE,PROCPFK        BEFORE DELETE RECORD (INVALID CML)           
         BNE   MAIN05                                                           
*                                                                               
*                                                                               
         CLI   PFKEY,5                                                          
         BE    MAIN04                                                           
         CLI   PFKEY,7                                                          
         BE    MAIN03                                                           
         CLI   PFKEY,8                                                          
         BE    MAIN02                                                           
         B     MAIN05                                                           
*                                                                               
*        LR    RE,RA                  ONLY GET PROCPFK FROM SELECT              
*        AHI   RE,THISLSEL-T216FFD    FROM LIST                                 
*        CLI   0(RE),C'C'                                                       
*        BE    MAIN05                                                           
*                                                                               
MAIN02   DS    0H                                                               
         BRAS  RE,GETF6            LOAD F6 SCREEN                               
         BRAS  RE,DRPRSUB          DISPLAY                                      
         MVI   GENPFOPT,C'Y'                                                    
         MVI   MYSCREEN,X'F6'                                                   
         B     EXIT                                                             
*                                                                               
MAIN03   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         BRAS  RE,DRTELE                                                        
         MVI   GENPFOPT,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
MAIN04   BRAS  RE,DSPCHG           DISPLAY CHANGE HISTORY                       
         OI    TRTMEDH+6,X'40'      POSITION CURSOR                             
         MVI   MYSCREEN,X'DF'                                                   
         MVI   TWASCR,X'DF'                                                     
         MVI   GENPFOPT,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
MAIN05   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   MAIN10                                                           
                                                                                
         BRAS  RE,DR                                                            
         B     EXIT                                                             
                                                                                
TRACEIT  DC    X'07',C'MODE=X'                                                  
                                                                                
MAIN10   CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECADD         PRIOR TO ADD RECORD                          
         BE    PAR                                                              
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    AAR                                                              
         CLI   MODE,RECPUT         BEFORE REWRITING, CK IF REQ NEEDED           
         BE    PUT                                                              
         CLI   MODE,XRECPUT        AFTER REWRITING                              
         BE    ACH                                                              
         CLI   MODE,RECDEL         BEFORE DELETE RECORD (INVALID CML)           
         BE    DELREC                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===================================================================            
*     VALIDATE KEY ROUTINE                                                      
*===================================================================            
                                                                                
VK       LA    R2,TRACLTH                                                       
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BNE   VK05                                                             
                                                                                
         CLI   MYSCREEN,X'B2'      TELECASTER SCREEN                            
         BNE   VK03                                                             
                                                                                
         MVI   ERROR,INVACT        ADD ACTION IS INVALID                        
         LA    R2,CONACTH                                                       
         J     TRAPERR                                                          
                                                                                
VK03     BRAS  RE,CLRHLP           CLEAR TYPE HELP FIELD                        
*                                                                               
         CLI   PFKEY,7                                                          
         BNE   VK04                                                             
         CLC   CONACT(3),=C'ADD'                                                
         BNE   VK05                                                             
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(6),=C'CHANGE'                                             
         MVI   MYSCREEN,X'F2'      MAKE SURE B2 SCREEN GETS LOADED              
         MVI   ACTNUM,ACTCHA                                                    
         MVI   ACTEQU,ACTCHA                                                    
         BRAS  RE,DRTELE                                                        
*                                                                               
VK04     CLI   PFKEY,8                                                          
         BNE   VK05                                                             
         CLC   CONACT(3),=C'ADD'                                                
         BNE   VK05                                                             
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(6),=C'CHANGE'                                             
         BRAS  RE,GETF6            LOAD F6 SCREEN                               
         BRAS  RE,DRPRSUB          DISPLAY                                      
         MVI   ACTNUM,ACTCHA                                                    
         MVI   ACTEQU,ACTCHA                                                    
                                                                                
VK05     CLI   ACTNUM,ACTDEL       DELETE IS INVALID                            
         BE    DELREC                                                           
                                                                                
         CLI   PFKEY,6             RETURNING FROM PF7/PF6/PF6                   
         BNE   VK06                                                             
         CLI   MYSCREEN,X'B2'      TELECASTER SCREEN                            
         BNE   VK06                                                             
         MVI   MYSCREEN,X'F2'      SET F2 SCREEN LOADED                         
VK06     DS    0H                                                               
                                                                                
         BRAS  RE,VKEY                                                          
         B     EXIT                                                             
*====================================================================           
* VALIDATE RECORD ROUTINE                                                       
*====================================================================           
                                                                                
VR       DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         XC    CHGFLAGS,CHGFLAGS                                                
         MVC   SVDSKADR,DMDSKADD                                                
         XC    SVPPKEY(SVPPKEYL),SVPPKEY                                        
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+8                                                              
         OI    GENSTAT2,RETEQSEL                                                
         BRAS  RE,SETPFK                                                        
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VR01                                                             
*                                                                               
         L     RE,AIO1             SAVE ORIGINAL RECORD IN AIO3                 
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIO3                                                          
         LA    R1,2(RF)                                                         
         MVCL  R0,RE                                                            
                                                                                
VR01     TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR02                                                             
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR02                                                             
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
VR02     TM    FLAGS,CONTINUE      ARE WE IN THE MIDST OF CML COPIES            
         BO    EXIT                 YES, NO NEED TO RE-VALIDATE RECORD          
*                                                                               
         CLI   PFKEY,4                                                          
         BNE   VR05                                                             
         CLI   MYSCREEN,X'F2'      IS COMML SCREEN LOADED ALREADY               
         BE    VR06                                                             
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE KEY FIELDS                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,TRAKEYH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'F2'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*MNPFK   MVI   MYSCREEN,X'B2'    SET B2 SCREEN LOADED                           
         MVI   MYSCREEN,X'F2'    SET F2 SCREEN LOADED                           
         BRAS  RE,SETPFK                                                        
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE KEY FIELDS                           
                                                                                
         MVI   MYSCREEN,X'F2'                                                   
         LA    R2,CONHEADH                                                      
         OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
*                                                                               
         BRAS  RE,DR                                                            
         B     VR08                                                             
*                                                                               
*                                                                               
VR05     CLI   MYSCREEN,X'B2'      TELECASTER SCREEN?                           
         BNE   VR06                                                             
         BRAS  RE,VRTELE                                                        
         J     EXIT                                                             
*                                                                               
VR06     CLI   MYSCREEN,X'F6'      PRODUCT SUBST SCREEN                         
         BNE   VR08                                                             
         BRAS  RE,VRPRSUB                                                       
         BRAS  RE,DRPRSUB                                                       
         J     EXIT                                                             
*                                                                               
VR08     BAS   RE,CLRHLP           CLEAR TYPE HELP FIELD                        
                                                                                
         L     R4,AIO              SAVE KEY CONTENTS                            
         MVC   20(2,R4),AGENCY                                                  
*                                                                               
         USING CMLKEY,R4                                                        
VR14     MVC   BAGYMD,CMLKAM                                                    
         CLC   BCLT,CMLKCLT        IS CLIENT SAME                               
         BE    VR16                YES                                          
                                                                                
         BRAS  RE,FCLT             GO GET CLIENT CLIST                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR16     MVC   HOLDCML,CMLKCML                                                  
         DROP  R4                                                               
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         OI    FLAGS,ISCOVCML      SET IS COVER CMML                            
                                                                                
         LA    R2,TRAPLSTH         PRODUCT LIST                                 
         CLC   HOLDCML,=8C'9'       CML ID ALL 9'S (PROD HSE KEY)               
         BE    VR20                 YES, ONLY VAL TITLE (PROD HOUSE)            
                                                                                
         BRAS  RE,VPRDL             VALIDATE PROD LIST & BUILD ELEM             
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR18                                                             
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'        PROD LIST ELEM CODE                          
         BRAS  RE,GETEL            DELETE ELEMENT PRESERVING ELEM               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,AIO1,(R6)                                            
                                                                                
VR18     GOTO1 ADDELEM                                                          
                                                                                
*===========================================================                    
* COMMERCIAL TITLES                                                             
*===========================================================                    
                                                                                
VR20     L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    VR22                                                             
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'10'                                                       
         MVI   ELEM+1,CMLDTAX-CMLDTAEL                                          
         GOTO1 ADDELEM                                                          
         B     VR20                GO FIND ELEM JUST INSERTED                   
                                                                                
         USING CMLDTAEL,R6                                                      
VR22     CLC   HOLDCML,=8C'9'       CML ID ALL 9'S (PROD HSE KEY)               
         BE    VR27                 YES, ONLY VAL TITLE (PROD HOUSE)            
*                                                                               
         OC    CMLCOVCT,CMLCOVCT   IS THIS A COVD (ACT) CML?                    
         BZ    *+8                                                              
         OI    FLAGS,ISCOVERD                                                   
*                                                                               
         LA    R2,TRADSC1H                                                      
         CLC   8(6,R2),=C'DELETE'  SOFT DELETE THIS CML                         
         BNE   VR24                NO                                           
                                                                                
*MNADDDEL                                                                       
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BNE   VR23                                                             
         LHI   R0,NODELADD         CAN'T ADD A SOFT DELETED RECORD              
         J     TRAPERR2                                                         
*MNADDDEL                                                                       
                                                                                
VR23     TM    FLAGS,ISCOVERD      IS CMML COVERED?                             
         BZ    *+12                 NO                                          
         LHI   R0,CMLCOVRD                                                      
         J     TRAPERR2                                                         
                                                                                
         OI    CMLSTAT,X'80'       SOFT DELETE RECORD                           
         OI    FLAGS,SOFTDEL                                                    
*        B     VR30                DO NOT OVERLAY TITLE                         
         B     VR70F                                                            
                                                                                
VR24     CLC   8(6,R2),=C'RESTORE' RESTORE SOFT DELETE?                         
         BNE   VR25                NO                                           
         LHI   R0,NORESTOR                                                      
         TM    CMLSTAT,X'80'       WAS CML SOFT DELETED                         
         JZ    TRAPERR2            CAN'T RESTORE                                
         NI    CMLSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         OI    FLAGS,SOFTREST                                                   
*        B     VR30                                                             
         B     VR70F                                                            
*                                                                               
VR25     BRAS  RE,VALTTL                                                        
*                                                                               
VR27     L     R6,AIO              MOVE X'10' ELEM OUT OF RECORD                
         MVI   ELCODE,X'10'        IT NOW CONTAINS NEW TITLE1                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         EJECT                                                                  
*==================================================================             
* VALIDATE PRODUCTION HOUSE (FOR 99999999 RECORD ONLY)                          
*==================================================================             
                                                                                
VR30     CLC   HOLDCML,=8C'9'      CML ID ALL 9'S                               
         BNE   VR32                NO                                           
         LA    R2,TRADSC1H         POINT TO FIRST DESC LINE                     
         BAS   RE,VHSE             VALIDATE PROD HOUSE                          
         MVC   CMLTITLE(6),8(R2)                                                
         OC    CMLTITLE(6),SPACES                                               
         MVC   CMLTITLE+6(9),=CL9'=SHIP HSE'                                    
         GOTO1 ADDELEM             INSERT X'10' ELEM IN RECORD                  
         B     VR80                                                             
                                                                                
*==================================================================             
* VALIDATE COMMERCIAL LEN                                                       
*==================================================================             
                                                                                
VR32     LA    R2,TRASLNH                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         JE    MISSERR             NO, ERROR                                    
         GOTO1 VALISLN             COMMERCIAL LENGTH - REQUIRED                 
*                                                                               
         CLC   CMLSLN,WORK                                                      
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SLN                                               
         MVC   CMLSLN,WORK                                                      
*                                                                               
         NI    CMLOPFLG,X'FF'-CMLOPCHG  TURN OFF CML CHANGED IN OPTICA          
*                                                                               
         LA    R2,TRAOVRDH                                                      
         BAS   RE,VOVRD            GO GET PRINTABLE OVERRIDE SPOT LENS          
                                                                                
*==================================================================             
* VALIDATE START (RELEASE) AND END (RECALL) DATES                               
*==================================================================             
                                                                                
         LA    R2,TRARLSEH         RELEASE DATE - REQUIRED                      
         GOTO1 DATVAL,DMCB,(0,TRARLSE),DATE                                     
         OC    DMCB(4),DMCB                                                     
         JZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,WORK)                                    
         CLC   CMLRLSE,WORK                                                     
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SDAT                                              
         MVC   CMLRLSE,WORK                                                     
*                                                                               
         LA    R2,TRARCLH          RECALL DATE                                  
         CLC   =CL3'UFN',TRARCL    UNTIL FURTHUR NOTICE                         
         BNE   VR34                NO, PROCESS IT                               
         MVC   WORK(3),=X'FFFFFF'  FORCE IT                                     
         B     VR36                                                             
*                                                                               
VR34     GOTO1 DATVAL,(R1),(0,TRARCL),DATE                                      
         OC    DMCB(4),DMCB                                                     
         JZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,WORK)                                    
*                                                                               
VR36     CLC   CMLRCL,WORK                                                      
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_EDAT                                              
         MVC   CMLRCL,WORK                                                      
*                                                                               
         CLC   CMLRLSE,CMLRCL      CAN'T RECALL BEFORE RELEASE                  
         JH    DATERR                                                           
*                                                                               
         MVC   WORK(3),CMLRLSE                                                  
         MVC   WORK+3(3),CMLRCL                                                 
*                                                                               
         BRAS  RE,CHKPRSOV         SEE IF PRD SUB ELEMS IN PERIOD               
                                                                                
*==================================================================             
* VALIDATE TYPE (OPTIONAL)                                                      
*==================================================================             
                                                                                
VR40     LA    R2,TRATYPEH         TYPE                                         
         XC    WORK,WORK           CLEAR TYPE                                   
         CLI   5(R2),0             TYPE IS NOW OPTIONAL                         
         BE    VR42                SO IGNORE IF NOT INPUT                       
         MVI   BYTE,0              SET FOR CALL TO ANY                          
         BRAS  RE,VTYP                                                          
*                                                                               
VR42     CLC   CMLTYPE,WORK                                                     
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_TYPE                                              
         MVC   CMLTYPE,WORK        STORE TYPE                                   
*                                                                               
         XC    CMLLMTCD(11),CMLLMTCD CLEAR RUN LIMIT CODE                       
                                                                                
*============================================================                   
* CLIENT COMMERCIAL NUMBER                                                      
*============================================================                   
                                                                                
VR50     LA    R2,TRACLTNH         CLIENT COMMERCIAL NUMBER                     
         XC    WORK,WORK                                                        
         CLI   5(R2),0                                                          
         BE    VR50X                                                            
         GOTO1 ANY                                                              
*                                                                               
VR50X    CLC   CMLCLTNO,WORK                                                    
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_CLNO                                              
         MVC   CMLCLTNO,WORK                                                    
                                                                                
*===================================================================            
* VALIDATE P/B-SOLO OPTION                                                      
*===================================================================            
                                                                                
         LA    R2,TRASOLOH         PIGGYBACK/SOLO                               
         MVI   WORK,0                                                           
         CLI   5(R2),0                                                          
         BE    VR52                                                             
         MVC   WORK(1),8(R2)                                                    
         CLI   8(R2),C'S'                                                       
         BE    VR52                                                             
         CLI   8(R2),C'P'                                                       
         JNE   SOLOER                                                           
*                                                                               
VR52     CLC   CMLSOLO,WORK                                                     
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_OTHR                                              
         MVC   CMLSOLO,8(R2)                                                    
                                                                                
*==================================================================             
* TALENT TRANSFER EXCLUDE OPTION                                                
*==================================================================             
                                                                                
         LA    R2,TRATTEXH         TALENT TRANSFER EXCLUDE                      
         MVI   WORK,0                                                           
         CLI   5(R2),0                                                          
         BE    VR55                                                             
         MVC   WORK(1),8(R2)       MOVE TO WORK                                 
*                                                                               
         CLI   8(R2),C'Y'          YES, EXCLUDE FROM TALENT TRANSFER            
         BE    VR55                                                             
         CLI   8(R2),C'N'                                                       
         JNE   TALTRNER                                                         
                                                                                
         CLI   SVTTPR3,C'Y'        IF SPOT TALENT USER, NOT ALLOWED             
         BNE   VR55                                                             
                                                                                
* MAKE SURE THAT THIS IS NOT PRD=ALL                                            
                                                                                
         LR    R5,R6                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'FF'         THIS ALL PRODUCTS                            
         JE    PRDALLRR                                                         
         LR    R6,R5                                                            
                                                                                
VR55     CLC   CMLTALEX,WORK                                                    
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_TAL                                               
         MVC   CMLTALEX,WORK                                                    
         EJECT                                                                  
VR60     XC    CMLCLASS,CMLCLASS  CLEAR FOR NOW                                 
*                                                                               
         GOTO1 ADDELEM             INSERT X'10' ELEMENT NOW!                    
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR62                                                             
         BRAS  RE,DUPCHK           MAKE SURE THIS CML DOES NOT EXIST            
         CLI   ADIDFLAG,C'Y'       TEST CMML IS AN ADID                         
         BNE   *+8                 NO                                           
         BRAS  RE,AADIDEL          ELSE ADD ADID ELEM FOR CMML                  
                                                                                
*==================================================================             
* VALIDATE MATCH DATES, TIMES                                                   
*==================================================================             
                                                                                
VR62     BRAS  RE,VALMAT                                                        
                                                                                
*=================================================================              
* VALIDATE HIDEF COMMERCIAL                                                     
* PER RAMUNE 08FEB08, JUST ACCEPT ANY COMMERCIAL-LIKE INPUT                     
* PER RAMUNE 21FEB08, DO NOT ALLOW ANY SPECIAL CHARACTERS                       
*=================================================================              
                                                                                
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
         CLC   SVHIDEF,8(R2)       IF NO CHANGE THEN ALREADY VALID              
         BE    VRPC                AND PASSIVES ALREADY MAINTAINED              
                                                                                
VRHD010  XC    WORK,WORK                                                        
         XC    NWHIDEFX,NWHIDEFX   INITIALIZE NEW HIDEF CODE                    
         LA    R2,TRAHDEFH                                                      
         CLI   5(R2),0                                                          
         BE    VRHD050                                                          
         LHI   R0,CMML912                                                       
         CLI   5(R2),9                                                          
         JL    TRAPERR2                                                         
*                                                                               
         LLC   R0,5(R2)            MAKE SURE NO SPCL CHARS                      
         LA    R1,8(R2)                                                         
*                                                                               
VRHD020  CLI   0(R1),C'A'                                                       
         BL    VRHDERR                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VRHD030                                                          
         CLI   0(R1),C'0'                                                       
         BL    VRHDERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    VRHDERR                                                          
*                                                                               
VRHD030  LA    R1,1(R1)                                                         
         BCT   R0,VRHD020                                                       
         L     RF,VTRPACK                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(12),8(R2)                                                   
         OC    WORK(12),SPACES                                                  
         GOTO1 (RF),DMCB,(C'P',WORK),WORK+12   WORK+12 PASSED TO REDUPS         
         BNE   VRHDERR                                                          
                                                                                
         MVC   NWHIDEFX,WORK+12                                                 
                                                                                
         OC    TRACML,SPACES                                                    
         OC    TRAADID,SPACES                                                   
         OC    TRAHDEF,SPACES                                                   
         OC    TRACNTR,SPACES                                                   
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
*                                                                               
VRHDERR  LHI   R0,CMLBADMS                                                      
         J     TRAPERR2                                                         
                                                                                
VRHDDUP  LHI   R0,DUPHIDEF                                                      
         J     TRAPERR2                                                         
                                                                                
VRHDSCR  LHI   R0,DUPCMSCR                                                      
         J     TRAPERR2                                                         
                                                                                
*=================================================================              
* VALIDATE PARENT COMMERCIAL                                                    
*=================================================================              
                                                                                
VRPC     DS    0H                                                               
         MVI   CHKCMML,C'Y'                                                     
         LA    R2,TRAPRNTH                                                      
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BRAS  RE,VALCMML                                                       
                                                                                
*=================================================================              
* VALIDATE CENTERCUT COMMERCIAL                                                 
*=================================================================              
                                                                                
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
         LHI   R0,CMML912                                                       
         CLI   5(R2),9                                                          
         JL    TRAPERR2                                                         
         BRAS  RE,VALCMML                                                       
                                                                                
         TM    KEY+13,X'80'                                                     
         BO    VRCCERR                                                          
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(12),8(R2)                                                   
         OC    WORK(12),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12 WORK+12 PASSED->REDUPS          
         BNE   VRCCERR                                                          
                                                                                
         MVC   NWCNTCTX,WORK+12                                                 
                                                                                
         OC    TRAADID,SPACES                                                   
         OC    TRACML,SPACES                                                    
         OC    TRAHDEF,SPACES                                                   
         OC    TRACNTR,SPACES                                                   
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
                                                                                
VRCCERR  LHI   R0,CMLBADMS                                                      
         J     TRAPERR2                                                         
                                                                                
VRCCDUP  LHI   R0,DUPCNTRC                                                      
         J     TRAPERR2                                                         
                                                                                
VRCCSCR  LHI   R0,DUPCMSCR                                                      
         J     TRAPERR2                                                         
                                                                                
                                                                                
*======================================================================         
* VALIDATE TELECASTER NUMBER (CANADA ONLY)                                      
*======================================================================         
                                                                                
VRSWAP   DS    0H                                                               
         MVI   SVSWAP,0                                                         
         LA    R2,TRASWAPH                                                      
         CLI   5(R2),0                                                          
         BE    VRTLCST                                                          
         MVI   SVSWAP,C'N'                                                      
         CLI   TRASWAP,C'N'                                                     
         BE    VRTLCST                                                          
         MVI   SVSWAP,C'Y'                                                      
         CLI   TRASWAP,C'Y'                                                     
         BE    VRTLCST                                                          
                                                                                
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
                                                                                
*======================================================================         
* VALIDATE TELECASTER NUMBER (CANADA ONLY)                                      
*======================================================================         
                                                                                
VRTLCST  DS    0H                                                               
                                                                                
*======================================================================         
* VALIDATE CLASS                                                                
*======================================================================         
                                                                                
         BRAS  RE,VCLASS                                                        
                                                                                
*======================================================================         
* VALIDATE PRODUCTION HOUSE                                                     
*======================================================================         
                                                                                
         LA    R2,TRAPRHSH                                                      
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BRAS  RE,VHSE                                                          
                                                                                
*======================================================================         
* VALIDATE DESTROY DATE AND TIME                                                
*======================================================================         
                                                                                
         XC    DSTRYDAT(5),DSTRYDAT   CLEAR DATE/TIME SAVE AREA                 
                                                                                
         LA    R2,TRADSDTH                                                      
         CLI   5(R2),0                                                          
         BE    VR70                                                             
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATE                                       
         OC    DMCB(4),DMCB                                                     
         JZ    DATERRB                                                          
         GOTO1 DATCON,DMCB,(0,DATE),(3,DSTRYDAT)                                
*                                                                               
         LA    R2,TRADSTMH                                                      
         CLI   5(R2),0                                                          
         BE    VR70                                                             
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                 
*                                                                               
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
*                                                                               
VR70     MVI   ELCODE,X'24'        UPDATE EXTENDED DATA ELEM                    
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING CMLXDTEL,R6                                                      
*                                                                               
         CLC   TRAHDEF,SPACES                                                   
         BNE   *+10                                                             
         XC    TRAHDEF,TRAHDEF                                                  
         CLC   TRACNTR,SPACES                                                   
         BNE   *+10                                                             
         XC    TRACNTR,TRACNTR                                                  
*                                                                               
         CLC   CMLXPRNT,TRAPRNT                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_PRNT                                              
*                                                                               
         CLC   CMLXHDEF,TRAHDEF                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_HDEF                                              
*                                                                               
         CLC   CMLXCNTR,TRACNTR                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_CNTR                                              
*                                                                               
         CLC   CMLXPRHS,TRAPRHS                                                 
         BE    *+8                                                              
         OI    CHGFLAG3,CMLCH_PRHS                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   CMLXDTEL,X'24'                                                   
         MVI   CMLXDTLN,CMLXDTX-CMLXDTEL                                        
         MVC   CMLXPRHS,TRAPRHS                                                 
         MVC   CMLXHDEF,TRAHDEF                                                 
         MVC   CMLXCNTR,TRACNTR                                                 
         MVC   CMLXPRNT,TRAPRNT                                                 
         MVC   CMLXDSDT,DSTRYDAT                                                
         MVC   CMLXDSTM,DSTRYTIM                                                
         MVC   CMLXSWAP,SVSWAP                                                  
         MVC   CMLXHDPK,NWHIDEFX                                                
         MVC   CMLXCCPK,NWCNTCTX                                                
                                                                                
         LLC   R1,CMLXDTLN                                                      
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    ELEM+2(0),ELEM+2                                                 
         BZ    VR70F               DO NOT ADD AN EMPTY ELEMENT                  
                                                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         EJECT                                                                  
*======================================================================         
* VALIDATE ACTUAL COMMERCIALS                                                   
* IF DELETE, LEAVE TITLES ALONE                                                 
* IF RESTORE, VALIDATE ACT CMLS IF COV                                          
*======================================================================         
                                                                                
VR70F    TM    FLAGS,SOFTDEL       SOFT DELETE THIS CML                         
         BNZ   VR80                YES - NO OTHER CHANGES                       
         TM    FLAGS,SOFTREST      RESTORE SOFT DELETE?                         
         BZ    VR72                NO, CONTINUE                                 
         TM    FLAGS,ISCOVCML      IS THIS A COVER?                             
         BNZ   VR74                YES - MAKE SURE ACTUALS STILL VALID          
         B     VR80                NO - DISALLOW OTHER CHANGES                  
                                                                                
* ACCEPT NO CHANGES ON A DELETED CMML                                           
                                                                                
VR72     L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CMLSTAT-CMLDTAEL(R6),X'80'                                       
         BZ    VR74                                                             
         LHI   R0,CMLISDEL                                                      
         LA    R2,TRACMLH                                                       
         J     TRAPERR2                                                         
*                                                                               
VR74     DS    0H                                                               
         MVC   TEMPADID,ADIDFLAG                                                
         BRAS  RE,VALACT                                                        
         MVC   ADIDFLAG,TEMPADID                                                
*                                                                               
VR80     MVC   KEY,SVKEY                                                        
                                                                                
*===============================================================                
* ADD CHANGE ELEMENT TO RECORD                                                  
*===============================================================                
                                                                                
VR110    BRAS  RE,SETCHGEL                                                      
                                                                                
         L     R6,AIO                                                           
         CLI   ADIDFLAG,C'Y'       TEST CMML PACKED                             
         BNE   *+12                NO                                           
         OI    15(R6),X'01'        TURN ON STATUS IN RECORD                     
         OI    KEY+13,X'01'        AND IN KEY FOR PACKED CML                    
                                                                                
         BRAS  RE,DR                                                            
         B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
*====================================================================           
* AFTER RE-WRITING RECORD - CHECK IF ACTUAL CMMLS NEED REWRITING                
*====================================================================           
                                                                                
ACH      LHI   R0,1                                                             
         TM    FLAGS,SOFTDEL                                                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         B     *+12                                                             
         TM    FLAGS,SOFTREST                                                   
         BZ    ACH20                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
ACH10    BRAS  RE,NEXTEL                                                        
         BNE   ACH30                                                            
         LA    R3,2(R6)                                                         
         LA    R0,1                                                             
         LR    R1,R3               POINT TO CMML                                
                                                                                
         USING CMLACTEL,R6                                                      
         MVI   ADIDFLAG,C'N'                                                    
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         BE    ACH15                                                            
         CLI   8(R3),C' '          TEST ISCI                                    
         BNH   ACH15                                                            
*                                                                               
         OC    0(12,R3),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         JNE   BADLENER                                                         
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
                                                                                
ACH15    BRAS  RE,UPDACT                                                        
         B     ACH10                                                            
                                                                                
ACH20    BRAS  RE,NPUT                                                          
                                                                                
ACH30    DS    0H                                                               
         MVC   SVDSKAD,SVDSKADR                                                 
         XC    KEY,KEY                                                          
         OC    SVHDDELK,SVHDDELK                                                
         BZ    ACH35                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
*        BNE   ACH35                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDDELK),SVHDDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
                                                                                
ACH35    XC    KEY,KEY                                                          
         OC    SVCCDELK,SVCCDELK                                                
         BZ    ACH40                                                            
         CLC   SVCCDELK(2),=X'0AC3'                                             
*        BNE   ACH40                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCDELK),SVCCDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVCCDELK,SVCCDELK                                                
                                                                                
ACH40    XC    KEY,KEY                                                          
         OC    SVHDADDK,SVHDADDK                                                
         BZ    ACH45                                                            
         CLC   SVHDADDK(2),=X'0AC2'                                             
*        BNE   ACH45                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDADDK),SVHDADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
                                                                                
ACH45    XC    KEY,KEY                                                          
         OC    SVCCADDK,SVCCADDK                                                
         BZ    ACH50                                                            
         CLC   SVCCADDK(2),=X'0AC3'                                             
*        BNE   ACH50                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCADDK),SVCCADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVCCADDK,SVCCADDK                                                
                                                                                
ACH50    DS    0H                                                               
*                                                                               
ACHX     B     EXIT                                                             
         EJECT                                                                  
*======================================================================         
* UPDATE SEQUENCE NUMBER IN MASTER RECORD FOR LAST ADD, ADD PASSIVE PTR         
*======================================================================         
                                                                                
AAR      DS    0H                                                               
         BRAS  RE,AAREC                                                         
*                                                                               
         CLI   SPOTCAN,C'C'        TEST CANADA                                  
         JNE   EXIT                                                             
         CLI   SVT3PROF+6,C'Y'     TEST AUTO ADD MEDIA N                        
         JNE   EXIT                                                             
         CLI   TRAMED,C'T'         TEST SEL TV                                  
         JE    AAR2                                                             
         CLI   TRAMED,C'N'         TEST DOING N DIRECTLY                        
         JNE   EXIT                                                             
         MVC   CONHEAD(40),=C'Media N REC added. Media T already there'         
         LA    R2,TRAMEDH                                                       
         OI    GENSTAT2,USMYOK     TELL GENCON USE MY MESSAGE                   
         J     EXIT                                                             
*                                                                               
AAR2     MVC   KEY+14(4),SVDSKAD   REREAD SEL TV CMML                           
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         NI    BAGYMD,X'F0'                                                     
         OI    BAGYMD,X'03'        SET MEDIA TO NETWORK                         
         L     RE,AIO                                                           
         MVC   2(1,RE),BAGYMD      CHANGE MEDIA IN RECORD                       
         MVC   KEY(13),0(RE)       AND SAVE THE KEY                             
                                                                                
* NOW ADD THE NETWORK RECORD IF IT'S NOT ALREADY THERE                          
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST ALREADY THERE                           
         BNE   AAR10                                                            
         LA    R2,TRAMEDH                                                       
         LHI   R0,NETTHERE         TELL THEM NET CMML ALREADY THERE             
         J     TRAPERR2                                                         
                                                                                
AAR10    BRAS  RE,PAREC                                                         
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         BRAS  RE,AAREC                                                         
*                                                                               
         MVC   KEY+14(4),SVDSKAD   REREAD NETWORK CMML                          
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,TRAMEDH                                                       
         MVC   CONHEAD(31),=C'Records added for media T and N'                  
         OI    GENSTAT2,USMYOK     TELL GENCON USE MY MESSAGE                   
*                                                                               
         J     EXIT                                                             
                                                                                
PAR      DS    0H                                                               
         BRAS  RE,PAREC                                                         
         B     EXIT                                                             
         EJECT                                                                  
*======================================================================         
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*======================================================================         
                                                                                
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
                                                                                
PUT10    BRAS  RE,GENR             GO GENERATE AUTO-TURNAROUND REQ              
                                                                                
PUT12    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
                                                                                
* DELETE RECORD INVALID FOR COMMERCIALS                                         
                                                                                
DELREC   MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
         EJECT                                                                  
*======================================================================         
* DISPLAY KEY                                                                   
*======================================================================         
                                                                                
DK       XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    DK10                                                             
         MVC   TRAMED,WORK                                                      
         OI    TRAMEDH+6,X'80'                                                  
                                                                                
DK10     L     R4,AIO                                                           
         USING CMLKEY,R4                                                        
                                                                                
         GOTO1 CLUNPK,DMCB,(SVCPROF6,CMLKCLT),QCLT                              
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    DK12                                                             
         MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
                                                                                
DK12     XC    TRACML,TRACML                                                    
         MVC   TRACML(L'CMLKCML),CMLKCML                                        
         TM    KEY+13,X'01'        IS CML PACKED IN KEY                         
         BZ    DK12C                                                            
                                                                                
         GOTO1 VTRPACK,DMCB,(C'U',CMLKCML),TRACML                               
                                                                                
DK12C    OI    TRACMLH+6,X'80'                                                  
                                                                                
         MVC   BAGYMD,CMLKAM                                                    
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    DK20                YES                                          
                                                                                
         BRAS  RE,FCLT             GO GET CLIENT CLIST                          
         BNE   EXIT                                                             
                                                                                
DK20     MVC   HOLDCML,CMLKCML                                                  
         DROP  R4                                                               
                                                                                
* PRINT OUT ANY FILTERS                                                         
                                                                                
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
                                                                                
         CLC   RCDTFTR,=X'FFFFFF'                                               
         BNE   DK26                                                             
         MVC   4(3,R3),=C'UFN'                                                  
         LA    R3,7(,R3)                                                        
         B     DK28                                                             
                                                                                
DK26     GOTO1 DATCON,DMCB,(3,RCDTFTR),(5,4(R3))                                
         LA    R3,12(,R3)                                                       
                                                                                
DK28     TM    FTRFLAG,DELFTR      SHOW DELETED CML'S ONLY                      
         BZ    DK30                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),DELET                                                    
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
DK34     OC    SLNFTR,SLNFTR       SPOT LENGTH FILTER                           
         BZ    DK36                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'L'                                                       
         MVI   1(R3),C'='                                                       
         LA    R3,2(,R3)                                                        
         EDIT  (B1,SLNFTR),(3,(R3)),ALIGN=LEFT                                  
         CLI   2(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,3(,R3)                                                        
DK36     OC    CLSFTR,CLSFTR       CLASS FILTER                                 
         BZ    DK38                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'R'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(4,R3),CLSFTR                                                   
DK38     MVC   TRAFLTR,FLD                                                      
         OI    TRAFLTRH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* CLEAR VALID TYPE HELP FROM SCREEN  (R0 & R2 NOT PRESERVED)                    
*============================================================                   
                                                                                
CLRHLP   DS    0H                                                               
         LA    R2,TRATYPLH                                                      
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         ZIC   R0,0(R2)            TO NEXT HELP LINE                            
         AR    R2,R0                                                            
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         BR    RE                                                               
         EJECT                                                                  
*===============================================================                
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*===============================================================                
                                                                                
LR       DS    0H                                                               
         CLC   =C'DOWN',CONOUT                                                  
         BNE   LR04                                                             
         CLC   =C'NOW',CONWHEN     ONLY PRINT THIS FOR NOW REQUEST              
         BNE   LR02                                                             
         MVC   P(23),=C'SPOT COMMERCIAL DOWLOAD'                                
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,99                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR02     XC    D.DLCBD(DLCBL),D.DLCBD                                           
         MVI   D.DLCBACT,DLCBINIT         DOWNLOAD ACTION IS START              
         LARL  RF,DLHOOK                  DUMMY HOOK                            
         ST    RF,D.DLCBAPR                                                     
         LA    RF,P                                                             
         ST    RF,D.DLCBAPL                                                     
         MVI   D.DLCXMAXL+1,L'P                                                 
         MVI   D.DLCXDELC,C' '            DELIMITER                             
         MVI   D.DLCXEOTC,C'"'            TEXT DELIMITER                        
         MVI   D.DLCXEOLC,X'5E'           SEMI-COLON, END-OF-LINE               
         MVI   D.DLCXEORC,C':'            END-OF-REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         MVC   D.DLCBFLD,SPACES           MUST CLEAR FIRST TIME                 
         MVI   D.DLCBFLX,C' '                                                   
         MVC   D.DLCBFLX+1(L'DLCBFLX-1),D.DLCBFLX                               
*                                                                               
LR04     LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVI   MYSCREEN,0                                                       
*                                                                               
* NEED TO RESET ADIDFLAG IF NOT FIRST TIME - NOT IN SAVED STORAGE               
                                                                                
         CLI   KEY,0               TEST FIRST TIME                              
         BE    LR06                YES                                          
*                                                                               
         MVC   KEY,SVLSTKEY        RESTORE LAST KEY TO CONTROLLER               
         MVI   ADIDFLAG,C'A'                                                    
         CLI   KEY+1,X'C1'                                                      
         BE    *+8                                                              
         MVI   ADIDFLAG,C'I'                                                    
         B     LR10                                                             
*                                                                               
LR06     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0AC1'                                                  
         CLI   ADIDFLAG,C'A'       TEST TO READ ADIDS                           
         BE    *+10                                                             
LR08     MVC   CMLKID,=X'0A21'     ELSE JUST DO ISCI'S NOW                      
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,HOLDCML     (NOTE MAY BE PACKED CMML)                    
*                                                                               
         XC    RECCT,RECCT         ZERO RECORD CT                               
         LM    R0,R1,=A(HEADING,HDHK)      HEADING LINE FOR REPORT              
         A     R0,SPTR02RR                                                      
         ST    R0,SPECS                                                         
         A     R1,SPTR02RR         HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK                                                      
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      ARE THERE ANY RECS FOR THIS AGENCY           
         BNE   LR28                                                             
         B     LR22                                                             
                                                                                
* CHECK MAX I/O                                                                 
                                                                                
LR20     DS    0H                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         JNE   TOOBIGER             YES, TOO BIG FOR ONLINE                     
                                                                                
* ARE WE ABOUT TO EXCEED MAX IO COUNT                                           
                                                                                
         GOTO1 GETFACT,DMCB,0      GET A(SYSTEM INFO BLOCK)                     
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         AHI   R3,-1000                                                         
         CLM   R3,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    *+12                NO-STILL HAVE AT LEAST 1000 IOS              
         MVI   ERROR,1             DUMMY ERROR MSG                              
         J     TOOBIGER            TOO BIG FOR ONLINE                           
                                                                                
         GOTO1 SEQ                 DO READ SEQUENTIAL                           
                                                                                
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
         BE    LRANY                                                            
*                                                                               
         MVI   ADIDFLAG,C'I'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         TM    FLAGS2,ALLCLTR      ALL CLIENTS REQUEST?                         
         BZ    *+10                 NO                                          
         XC    KEY+3(2),KEY+3      CLEAR CLT                                    
         SR    RE,RE                                                            
         ICM   RE,1,TRACMLH+5                                                   
         BZ    LR10                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+5(0),TRACML     USE EBCDIC COMMERCIAL                        
         B     LR10                                                             
*                                                                               
LR30     DS    0H                                                               
*MNTEMP                                                                         
*        OI    GENSTAT1,RDUPAPPL   TEMPORARY CODE TO HANDLE ISSUE WITH          
*        OI    GENSTAT4,NODUPDIE   PURGE WHERE OLD ADID STYLE 0AC1              
*        MVI   ERROPT,C'Y'         POINTERS WERE LEFT ON THE FILE WHEN          
*MNTEMP                            THE COMML REC HAD BEEN PURGED                
         GOTO1 GETREC                                                           
*MNTEMP                                                                         
*        TM    DMCB+8,X'10'        THE GETREC CALL WAS DUMPING                  
*        BO    LR20                                                             
*        TM    DMCB+8,X'02'                                                     
*        BO    LR20                                                             
*MNTEMP                                                                         
                                                                                
*                                                                               
         L     R4,AIO                                                           
         USING CMLKEY,R4                                                        
         LR    R6,R4                                                            
         BRAS  RE,FTR              GO FILTER RECS                               
         BNE   LR20                GOT FILTERED OUT                             
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE DATA EL                            
*                                                                               
         USING CMLDTAEL,R6                                                      
         LH    R1,RECCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,RECCT                                                         
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   LR35                                                             
*                                                                               
         CLC   =C'DOWN',CONOUT     TEST DOWNLOAD                                
         BNE   LR32                                                             
         CLI   KEY+1,X'C1'         TEST READING ADIDS                           
         BNE   LR31                NO                                           
         L     RE,AIO                                                           
         TM    15(RE),X'01'        TEST THIS IS NATIVE ADID                     
         BZ    *+8                 NO-SKIP FOR NOW                              
LR31     BRAS  RE,DL               OUTPUT DOWNLOAD FORMAT                       
         B     LR20                                                             
*                                                                               
LR32     BRAS  RE,LRR              GO FORMAT FOR OFFLINE REPORT                 
         B     LR20                                                             
                                                                                
LR35     CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
                                                                                
         USING CMLKEY,R4                                                        
         USING CMLDTAEL,R6                                                      
LRL      DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    *+12                                                             
         BRAS  RE,FCLT             GET CLIENT CLIST                             
         BNE   LR20                                                             
                                                                                
         GOTO1 CLUNPK,DMCB,(SVCPROF6,CMLKCLT),LCLT                              
         TM    CMLSTAT,X'80'       SOFT DELETE                                  
         BZ    *+8                 NO                                           
         MVI   LCLT+3,C'*'                                                      
                                                                                
         MVC   LCML(L'CMLKCML),CMLKCML                                          
         MVC   LCML+8(4),SPACES                                                 
         CLI   KEY+1,X'C1'        TEST READING ADID'S                           
         BNE   LRL02C                                                           
         LA    R0,CMLKCML-CMLKEY+KEY     PACKED ADID IS IN KEY                  
         GOTO1 VTRPACK,DMCB,(C'U',(R0)),LCML                                    
*                                                                               
         CLC   CMLKCML-CMLKEY+KEY(8),CMLKCML   KEY/REC CMML MATCH               
         BE    LRL02C                                                           
*                                                                               
         LA    R1,LCML+L'LCML-1    SET FLAG TO SHOW IT IS ISCI                  
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'!'                                                       
                                                                                
LRL02C   EDIT  (1,CMLSLN),(3,LLEN),ZERO=BLANK                                   
                                                                                
         OC    CMLOVRD1(2),CMLOVRD1  ANY PRINT OVERRIDE?                        
         BZ    LRL04                  NO                                        
         EDIT  (1,CMLOVRD1),(3,LLEN),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R1,LLEN                                                          
         AR    R1,R0               NEXT BLANK SPACE                             
         MVI   0(R1),C'/'                                                       
         EDIT  (1,CMLOVRD2),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                   
                                                                                
LRL04    MVC   LTITLE,CMLTITLE                                                  
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,LRELSE)                               
         CLC   CMLRCL,=X'FFFFFF'                                                
         BNE   LRL10                                                            
         MVC   LRECALL(3),=CL3'UFN'                                             
         B     LRL12                                                            
*                                                                               
LRL10    GOTO1 (RF),(R1),(3,CMLRCL),(5,LRECALL)                                 
*                                                                               
LRL12    MVC   LTYPE,CMLTYPE                                                    
         MVC   LCLASS,CMLCLASS                                                  
         TM    CMLSTAT,X'40'       COMML TEXT REC FOR THIS COMML                
         BZ    LRL16                                                            
         MVC   LCOMTXT,=C'COM TEXT'                                             
                                                                                
LRL16    DS    0H                                                               
         TM    FTRFLAG,SEQFTR      DO THEY WANT COMML SEQ NO                    
         BZ    LRL18                                                            
                                                                                
         MVC   LCOMTXT,SPACES                                                   
         EDIT  CMLSEQ,(7,LCOMTXT)                                               
                                                                                
LRL18    DS    0H                                                               
         CLI   CMLCLASS,0          WAS COMML CLASS PRESENT                      
         BNE   LRL20                YES                                         
         L     R6,AIO1                                                          
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRL20                                                            
         MVI   LCLASS+2,C'%'                                                    
                                                                                
*                                                                               
LRL20    TM    FTRFLAG,HDEFFTR     SHOW HDEF CMML INSTEAD OF TITLE              
         BZ    LRL22                                                            
         MVC   LTITLE,SPACES       DON'T SHOW TITLE THEN                        
         MVI   ELCODE,X'24'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LRL22                                                            
         USING CMLXDTEL,R6                                                      
         MVC   LTITLE(12),CMLXHDEF                                              
*                                                                               
LRL22    MVC   SVLSTKEY,KEY        SAVE LAST KEY TO CONTROLLER                  
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         DROP  R4,R6                                                            
                                                                                
LRANY    DS   0H                                                                
         OC    RECCT,RECCT         WERE ANY RECS SELECTED                       
         BZ    LRNONE                                                           
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
         JNE   EXIT                                                             
*                                                                               
         OC    CONWHEN,CONWHEN     IMMEDIATE REQUEST                            
         JZ    EXIT                YES, NOT SUPPORTED                           
*                                                                               
         MVI   D.DLCBACT,DLCBEOR          END OF REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
         J     EXIT                                                             
*                                                                               
LRNONE   CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   LRNONEA                                                          
         MVC   P(19),=CL19'NO RECORDS SELECTED'                                 
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
*                                                                               
LRNONEA  LHI   R0,NORECSEL         NO RECORDS SELECTED MESSAGE                  
         MVI   GMSGTYPE,C'I'                                                    
         LA    R2,TRAMEDH                                                       
         J     TRAPERR2                                                         
         LTORG                                                                  
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
                                                                                
ASTDEL   DS    0CL9                                                             
         DC    C'*'                                                             
DELET    DS    0CL6                                                             
DELETD   DC    C'DELETED'                                                       
         DS    C'*'                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* VALIDATE DATA IN COMMERCIAL FIELD AT 0(R2)                                    
* FOR NOW, 8 CHARS=ISCI, 9-12=ADID                                              
*============================================================                   
                                                                                
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
VALC10   DS   0H                                                                
         CLI   5(R2),9             MUST BE 12 CHAR                              
         JL    BADLENER                                                         
                                                                                
         LLC   R0,5(R2)                                                         
         LA    R1,8(,R2)                                                        
*                                                                               
VALC14   CLI   0(R1),C'A'                                                       
         JL    BADLENER                                                         
         CLI   0(R1),C'Z'                                                       
         BNH   VALC16                                                           
                                                                                
         CLI   0(R1),C'0'                                                       
         JL    BADLENER                                                         
         CLI   0(R1),C'9'                                                       
         JH    BADLENER                                                         
*                                                                               
VALC16   LA    R1,1(R1)                                                         
         BCT   R0,VALC14                                                        
*                                                                               
         OC    8(12,R2),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',8(R2)),DUB                                    
         JNE   BADLENER                                                         
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
         LHI   R0,CMLISDEL                                                      
         L     RE,AIO2                                                          
         TM    CMLSTAT-CMLRECD(RE),X'80'   TEST SOFT DELETED                    
         JO    TRAPERR2                                                         
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*======================================================================         
* VALIDATE ACTUAL COMMERCIALS AND INSERT ELEMENTS IN RECORD                     
*======================================================================         
                                                                                
VALACT   NTR1  BASE=*,LABEL=*                                                   
         XC    OLDACTS(L'OLDACTS*NUMACTS),OLDACTS                               
         XC    NEWACTS(L'NEWACTS*NUMACTS),NEWACTS                               
         XC    ACTSLN,ACTSLN                                                    
                                                                                
* SAVE OFF OLD ACTUALS                                                          
                                                                                
         LA    R0,NUMACTS                                                       
         LA    R1,OLDACTS                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         USING CMLACTEL,R6                                                      
         BRAS  RE,GETEL                                                         
         BNE   VACT5                                                            
         B     VACT3                                                            
*                                                                               
VACT2    BRAS  RE,NEXTEL                                                        
         BNE   VACT4                                                            
VACT3    MVC   0(L'CMLACTID,R1),CMLACTID  8 CHAR CML                            
         CLI   CMLACTLN,CMLACTL1          OLD ELEM                              
         BE    *+10                                                             
         MVC   0(L'CMLACTCM,R1),CMLACTCM   NO, MOVE IN 12 CHAR CML              
         LA    R1,L'OLDACTS(R1)                                                 
         BCT   R0,VACT2                                                         
*                                                                               
VACT4    GOTO1 REMELEM             A-DING-DONG                                  
                                                                                
* VALIDATE ID'S ON SCREEN & BUILD TABLE OF NEW ACTUAL ID'S                      
                                                                                
VACT5    XC    ACTCT,ACTCT         COUNT OF ACTUAL CMMLS                        
         OC    TRAACT1,TRAACT1                                                  
         BNZ   VACT5C                                                           
         OC    TRAACT2,TRAACT2                                                  
         BNZ   VACT5C                                                           
         OC    TRAACT3,TRAACT3                                                  
         BNZ   VACT5C                                                           
         OC    TRAACT4,TRAACT4                                                  
         BNZ   VACT5C                                                           
         J     EXIT                                                             
*                                                                               
VACT5C   DS    0H                                                               
         CLC   TRAACT1(7),=C'UNCOVER'                                           
         BNE   *+12                                                             
         OI    FLAGS,UNCOVER                                                    
         B     VACT24                                                           
*                                                                               
         LA    R2,TRAACT1H                                                      
         LA    R0,NUMACTS                                                       
         LA    R5,NEWACTS                                                       
         SR    RF,RF                                                            
*                                                                               
VACT6    CLI   5(R2),0             ANY INPUT?                                   
         BE    VACT10               NO                                          
         TM    FLAGS,ISCOVERD      IS THIS CMML COVERED?                        
         BZ    *+12                 NO                                          
         LHI   R0,NOCOVCOV                                                      
         J     TRAPERR2                                                         
                                                                                
         IC    RF,ACTCT            INC ACTUAL COUNT                             
         LA    RF,1(RF)                                                         
         STC   RF,ACTCT                                                         
*                                                                               
         BRAS  RE,VID                                                           
*                                                                               
         MVC   0(L'NEWACTS,R5),8(R2)                                            
         OC    0(L'NEWACTS,R5),SPACES                                           
         LA    R5,L'NEWACTS(R5)                                                 
*                                                                               
VACT10   ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,VACT6                                                         
                                                                                
* MAKE SURE 2+ ACTUALS USED                                                     
                                                                                
         CLI   ACTCT,0              ANY ACTUALS?                                
         BNE   VACT12               YES                                         
         TM    FLAGS,ISCOVCML      WERE THERE ACTUALS BEFORE?                   
         BNZ   VACT14               YES                                         
         B     VACT22                                                           
*                                                                               
VACT12   CLI   ACTCT,2                                                          
         BNL   VACT16                                                           
*                                                                               
VACT14   LA    R2,TRAACT1H                                                      
         LHI   R0,COVREQ2                                                       
         J     TRAPERR2                                                         
                                                                                
* CHECK THAT ACCUM SLN'S = COV SLN                                              
                                                                                
VACT16   L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACTSLN,CMLSLN-CMLDTAEL(R6)                                       
         BE    VACT20                                                           
         LA    R2,TRAACT1H                                                      
         LHI   R0,SLNNEQ                                                        
         J     TRAPERR2                                                         
                                                                                
* ADD X'60' ELS                                                                 
                                                                                
VACT20   XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLACTEL,R6                                                      
                                                                                
         ZIC   R0,ACTCT            COUNT OF ACT CMMLS                           
         LA    R4,NEWACTS                                                       
*                                                                               
VACT22   MVI   CMLACTEL,X'60'                                                   
         MVI   CMLACTLN,CMLACTL2                                                
         MVC   CMLACTCM,0(R4)                                                   
         OC    CMLACTCM,SPACES                                                  
         GOTO1 ADDELEM                                                          
         LA    R4,L'NEWACTS(R4)                                                 
         BCT   R0,VACT22                                                        
*                                                                               
VACT24   SR    R1,R1                                                            
         LA    R0,NUMACTS                                                       
*                                                                               
VACT26   LA    RE,OLDACTS(R1)                                                   
         LA    RF,NEWACTS(R1)                                                   
         CLC   0(L'NEWACTS,RE),0(RF)  NEW MATCH OLD                             
         BE    *+8                                                              
         OI    CHGFLAG2,CMLCH_ACTS   SET CHANGE FLAG - NOT IN OLD LIST          
         LA    R1,L'OLDACTS(R1)                                                 
         BCT   R0,VACT26                                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* VALIDATE TITLE FIELDS - NOTE FIRST FIELD IS ONLY 15 BYTES                     
* NOTE THAT ON ENTRY R6 POINTS TO X'10' ELEMENT IN RECORD                       
*============================================================                   
                                                                                
         USING CMLDTAEL,R6                                                      
VALTTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,TRADSC1H         COMMERCIAL TITLE 1                           
         GOTO1 ANY                                                              
*                                                                               
         MVC   WORK+20(20),TRADSC2                                              
         OC    WORK+20(20),SPACES                                               
         MVC   WORK+40(20),TRADSC3                                              
         OC    WORK+40(20),SPACES                                               
*                                                                               
         CLI   TRADSC2H+5,0        ANY TITLE 2 ENTERED                          
         BNE   VALTTL2             YES                                          
         CLI   TRADSC3H+5,0        IF NOT, SHOULD BE NO  TITLE 3                
         BE    VALTTL2                                                          
         LA    R2,TRADSC2H                                                      
         MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
VALTTL2  CLC   CMLTITLE,WORK       UPDATE TITLE IN CMLDTAEL                     
         BE    *+8                 BUT DO NOT INSERT IN RECORD YET              
         OI    CHGFLAG1,CMLCH_DESC                                              
         MVC   CMLTITLE,WORK                                                    
*                                                                               
         L     R6,AIO              TEST FOR CHANGES IT TITLE2/3                 
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VALTTL10                                                         
         OC    3(20,R6),SPACES                                                  
         CLC   WORK+20(20),3(R6)                                                
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   VALTTL10                                                         
         OC    3(20,R6),SPACES                                                  
         CLC   WORK+40(20),3(R6)                                                
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
*                                                                               
VALTTL10 GOTO1 REMELEM             DELETE X'30' ELEMENTS                        
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLDSCEL,R6                                                      
*                                                                               
         MVI   ELEM,X'30'          BUILD NEW X'30' ELEMS AND INSERT             
         MVI   ELEM+1,27                                                        
         MVI   ELEM+2,1            SEQUENCE NUMBER                              
         MVC   ELEM+3(L'TRADSC2),WORK+20                                        
         OC    ELEM+3(24),SPACES                                                
                                                                                
         LLC   R1,CMLDSCLN         ELEM LEN                                     
         SHI   R1,4                ADJ FOR EX (ELCODE/LEN/LIN#)                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ELEM+3(0),SPACES                                                 
         JE    EXIT                DO NOT ADD AN EMPTY ELEMENTS                 
                                                                                
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELEM+2,2                                                         
         MVC   ELEM+3(L'TRADSC3),WORK+40                                        
         OC    ELEM+3(24),SPACES                                                
                                                                                
         LLC   R1,CMLDSCLN         ELEM LEN                                     
         SHI   R1,4                ADJ FOR EX (ELCODE/LEN/LIN#)                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ELEM+3(0),SPACES                                                 
         JE    EXIT                DO NOT ADD AN EMPTY ELEMENTS                 
                                                                                
         GOTO1 ADDELEM                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* VALIDATE PRODUCTION HOUSE                                                     
*============================================================                   
                                                                                
         USING CMLDTAEL,R6                                                      
         DS    0H                                                               
VHSE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRHKEY,R4                                                        
         MVC   PRHKID,=X'0A29'                                                  
         MVC   PRHKAM,BAGYMD                                                    
         MVC   PRHKPRH,8(R2)                                                    
         OC    PRHKPRH,SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   PRDHSER                                                          
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
*============================================================                   
* VALIDATE PRINT OVERRIDE                                                       
*============================================================                   
                                                                                
         USING CMLDTAEL,R6                                                      
VOVRD    NTR1  BASE=*,LABEL=*                                                   
         XC    CMLOVRD1(2),CMLOVRD1                                             
                                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VOVRDX               NO                                          
                                                                                
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=/='                              
         CLI   DMCB+4,2            MUST BE 2 BLOCKS                             
         BNE   OVDRERR                                                          
                                                                                
         LA    R1,BLOCK                                                         
         TM    2(R1),X'80'                                                      
         BZ    NUMERR              MUST BE NUMERIC                              
         L     RE,4(R1)                                                         
         STC   RE,CMLOVRD1                                                      
                                                                                
         LA    R1,32(R1)           NEXT SCANNER BLOCK                           
         TM    2(R1),X'80'                                                      
         BZ    NUMERR              MUST BE NUMERIC                              
         L     RF,4(R1)                                                         
         STC   RF,CMLOVRD2                                                      
                                                                                
* MAKE SURE (OVRD1 + OVRD2 = SLN)                                               
                                                                                
         AR    RE,RF                                                            
         IC    RF,CMLSLN                                                        
         CR    RE,RF                                                            
         BE    VOVRDX                                                           
         LHI   R0,INVOVRD                                                       
         J     TRAPERR2                                                         
*                                                                               
VOVRDX   J     EXIT                                                             
         DROP  R6                                                               
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
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C2'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDDXIT                                                           
*                                                                               
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
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'TRFDIR',KEY,KEYSAVE,0         
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DELPXIT                                                          
                                                                                
         MVC   KEY,KEYSAVE                                                      
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TRFDIR',KEY,KEY                       
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
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'TRFDIR',KEY,KEYSAVE,0         
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADDPSS                                                           
                                                                                
WRTPSS   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         NI    KEY+13,X'FF'-X'80'                                               
         MVC   KEY+14(4),SVDSKADR                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   KEY+14(4),SVDADRA   USE THIS FOR ACTION ADD                      
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TRFDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    ADDPXIT                                                          
         BNE   ADDPERRX                                                         
                                                                                
ADDPSS   DS    0H                                                               
         MVC   KEY+14(4),SVDSKADR                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   KEY+14(4),SVDADRA   USE THIS FOR ACTION ADD                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD ',=C'TRFDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BNE   ADDPERRX                                                         
                                                                                
ADDPXIT  SR    RB,RB                                                            
ADDPERRX LTR   RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* CLEAR VALID TYPE HELP FROM SCREEN  (R0 & R2 NOT PRESERVED)                    
*============================================================                   
*                                                                               
*CLRHLP   DS    0H                                                              
*         LA    R2,TRATYPLH                                                     
*         XC    8(L'TRATYPL,R2),8(R2)                                           
*         OI    6(R2),X'80'         XMIT FLD                                    
*         ZIC   R0,0(R2)            TO NEXT HELP LINE                           
*         AR    R2,R0                                                           
*         XC    8(L'TRATYPL,R2),8(R2)                                           
*         OI    6(R2),X'80'         XMIT FLD                                    
*         BR    RE                                                              
         EJECT                                                                  
*================================================================               
* ERROR ROUTINES                                                                
*================================================================               
                                                                                
TOOBIGER LA    R2,CONWHENH                                                      
         LHI   R0,TOOBIG                                                        
         J     TRAPERR2                                                         
                                                                                
CANMEDER LHI   R0,CANMEDMS                                                      
         J     TRAPERR2                                                         
*                                                                               
PRCNTER  LHI   R0,PRCNTMS                                                       
         J     TRAPERR2                                                         
                                                                                
BADTELNO LHI   R0,BADTELMS                                                      
         J     TRAPERR2                                                         
                                                                                
DUPTELNO LHI   R0,DUPTELMS                                                      
         J     TRAPERR2                                                         
                                                                                
PRDHSER  MVI   ERROR,INVPRHSE      NO PROD HOUSE ON FILE                        
         J     TRAPERR                                                          
                                                                                
MISSERRC LA    R2,TRACLTH          ERROR                                        
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
                                                                                
DATERR   MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
                                                                                
NUMERR   MVI   ERROR,NOTNUM                                                     
                                                                                
TRAPERR  GOTO1 ERREX                                                            
                                                                                
TRAPERR2 STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
                                                                                
ENTCMLER LHI   R0,ENTCMLMS      AUTO CML ASSIGN, NO ENTRY ALLOWED               
         J     TRAPERR2                                                         
                                                                                
CLASIZER LHI   R0,BADCLS                                                        
         J     TRAPERR2                                                         
                                                                                
SOLOER   LHI   R0,SOLOERR                                                       
         J     TRAPERR2                                                         
                                                                                
OVDRERR  LHI   R0,BADOVRD                                                       
         J     TRAPERR2                                                         
                                                                                
PRDALLRR LHI   R0,PRDALLMS                                                      
         J     TRAPERR2                                                         
*                                                                               
TALTRNER LHI   R0,TALTRNMS                                                      
         J     TRAPERR2                                                         
         EJECT                                                                  
         LTORG                                                                  
**** CMLTXTMS DC    C'COMMERCIAL TEXT PRESENT'                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*=======================================================                        
* VALIDATE STATION TELECASTER NO.                                               
*=======================================================                        
*                                                                               
*VTLCSTR  NTR1  BASE=*,LABEL=*                                                  
*                                                                               
*        XC    OTELNO,OTELNO                                                    
*        L     R6,AIO                                                           
*        MVI   ELCODE,X'40'        STATION TELECASTER NO                        
*        BRAS  RE,GETEL                                                         
*        BNE   VTLC1                                                            
*        CLC   TRATEL,2(R6)        HAS DATA CHANGED                             
*        BE    VTLCX                                                            
*        MVC   OTELNO,TRATEL       SAVE OLD TLCSTR NUMBER                       
*                                                                               
*VTLC1    OI    CHGFLAG3,CMLCH_OTHR  SET CHANGED FLAG                           
*        NI    FLAGS,X'FF'-DELTELP                                              
*                                                                               
*        GOTO1 REMELEM                                                          
*        XC    ELEM,ELEM                                                        
*                                                                               
*        LA    R2,TRATELH                                                       
*                                                                               
*        CLI   5(R2),0             TEST NO ENTRY                                
*        BE    VTLC4                                                            
*        CLI   5(R2),8             MUST BE 8 CHAR                               
*        JNE   BADTELNO                                                         
*                                                                               
*                                                                               
*        XC    KEY,KEY             READ REC FOR NEW NUMBER                      
*        MVC   KEY(2),=X'0AE1'                                                  
*        MVC   KEY+2(3),BAGYMD      A-M/CLT                                     
*        MVC   KEY+5(8),TRATEL                                                  
*        OI    DMINBTS,X'08'                                                    
*        GOTO1 HIGH                                                             
*        NI    DMINBTS,X'F7'                                                    
*        CLC   KEY(13),KEYSAVE                                                  
*        BNE   VTLC2                                                            
*                                                                               
*        TM    KEY+13,X'80'        DELETED RECORD                               
*        JZ    DUPTELNO                                                         
*        OI    FLAGS,DELTELP       SET ON DEL PASSIVE PTR FOUND                 
*                                                                               
*VTLC2    CLC   TRATEL,SPACES       TEST ANY NEW TELECASTER                     
*        BNH   VTLC4               NO                                           
*                                                                               
*        MVI   ELEM,X'40'          YES- ADD ELEMENT FOR NEW                     
*        MVI   ELEM+1,10                                                        
*        MVC   ELEM+2(8),TRATEL                                                 
*        GOTO1 ADDELEM                                                          
*                                                                               
*VTLC4    CLC   OTELNO,SPACES       WAS THERE AN OLD TELECASTER NO              
*        BNH   VTLC10               NO                                          
*                                                                               
*        CLC   OTELNO,TRATEL       ARE OLD/NEW TEL SAME                         
*        BE    VTLCX                YES, DON'T CHANGE PASSIVE KEY               
*                                                                               
*        OC    OTELNO,OTELNO       TEST ANY OLD                                 
*        BZ    VTLCX                                                            
*        XC    KEY,KEY             DELETE OLD                                   
*        MVC   KEY(2),=X'0AE1'                                                  
*        MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
*        MVC   KEY+5(8),OTELNO                                                  
*        GOTO1 HIGH                                                             
*        CLC   KEY(13),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        OI    KEY+13,X'80'                                                     
*        GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
*        CLI   DMCB+8,0                                                         
*        BE    VTLC10                                                           
*        DC    H'0'                                                             
*                                                                               
* ADD NEW TELECASTER NUMBER *                                                   
*                                                                               
*VTLC10   CLI   ACTNUM,ACTADD       IF ADD CAN'T DO IT NOW                      
*        BE    VTLCX                YES                                         
*                                                                               
*        CLI   TRATELH+5,0         ANY TELECASTER #                             
*        BE    VTLCX                NO                                          
*                                                                               
*        TM    FLAGS,DELTELP       TEST IF DEL PASSIVE PTR FOUND                
*        BO    VTLC20                                                           
*                                                                               
*        XC    KEY,KEY                                                          
*        MVC   KEY(2),=X'0AE1'                                                  
*        MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
*        MVC   KEY+5(8),TRATEL                                                  
*                                                                               
*        MVC   KEY+14(4),SVKEY+14  MOVE IN SAVED DISK ADDR                      
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
*                                                                               
*        CLI   DMCB+8,0                                                         
*        BE    VTLCX                                                            
*        DC    H'0'                                                             
*                                                                               
*VTLC20   EQU   *                                                               
*        NI    FLAGS,X'FF'-DELTELP                                              
*                                                                               
*        XC    KEY,KEY                                                          
*        MVC   KEY(2),=X'0AE1'                                                  
*        MVC   KEY+2(3),BAGYMD          AG/CLT                                  
*        MVC   KEY+5(8),TRATEL                                                  
*                                                                               
*        OI    DMINBTS,X'08'                                                    
*        GOTO1 HIGH                                                             
*        NI    DMINBTS,X'F7'                                                    
*        CLC   KEY(13),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        TM    KEY+13,X'80'        DELETED RECORD                               
*        BO    *+6                                                              
*        DC    H'0'                                                             
*        NI    KEY+13,X'7F'                                                     
*        MVC   KEY+14(4),SVKEY+14  MOVE IN DISK ADDR                            
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
*        CLI   DMCB+8,0                                                         
*        BE    VTLCX                                                            
*        DC    H'0'                                                             
*                                                                               
*VTLCX    XIT1                                                                  
*        LTORG                                                                  
         EJECT                                                                  
*                                                                               
*==============================                                                 
* VALIDATE KEY                                                                  
*==============================                                                 
                                                                                
VKEY     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   FLAGS,0                                                          
         XC    OLDACTS(L'OLDACTS*NUMACTS),OLDACTS                               
         XC    NEWACTS(L'NEWACTS*NUMACTS),NEWACTS                               
         XC    ACTSLN,ACTSLN                                                    
                                                                                
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
                                                                                
         LA    R2,TRACLTH          CLIENT                                       
         NI    FLAGS2,X'FF'-ALLCLTR INIT ALL CLIENT REQUEST                     
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BNE   VK12                                                             
                                                                                
         OI    FLAGS2,ALLCLTR      ALL CLIENT REQUEST                           
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK07                                                             
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         JNZ   TRAPERR                                                          
         DROP  RF                                                               
                                                                                
         MVI   ERROR,0                                                          
                                                                                
VK07     CLC   =C'BESAT',AGYORIG   STOP THESE AGENCIES                          
         JE    MISSERR              FROM LISTING ALL CLTS                       
         CLC   =C'LHNC',AGYORIG                                                 
         JE    MISSERR                                                          
         CLC   =C'MNAT',AGYORIG                                                 
         JE    MISSERR                                                          
         CLC   =C'MNNY',AGYORIG                                                 
         JE    MISSERR                                                          
         CLC   =C'WWAT',AGYORIG                                                 
         JE    MISSERR                                                          
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         J     MISSERR                                                          
*                                                                               
VK12     GOTO1 VALICLT                                                          
                                                                                
         L     R6,AIO1                                                          
         MVC   SVCLTINT,CCLTIFC-CLTHDR(R6)                                      
         MVC   SVCXTRA8,CEXTRA+8-CLTHDR(R6)   SAVE P&G FLAG                     
                                                                                
         BRAS  RE,RPROFILE         GO READ PROFILE                              
                                                                                
VK20     LA    R2,TRACMLH          CMML                                         
                                                                                
         XC    HOLDCML,HOLDCML                                                  
                                                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK22                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK24                                                             
                                                                                
         CLI   SVT2PR7,C'Y'        THIS AN AUTO NUMBER CMMLS AGY                
         JNE   MISSERR                                                          
                                                                                
         CLI   QMED,C'R'           ALSO MISSING FOR MEDIA R                     
*        JE    MISSERR                                                          
                                                                                
         CLI   QMED,C'X'           ALSO MISSING FOR MEDIA X                     
*        JE    MISSERR                                                          
                                                                                
         CLI   ACTNUM,ACTADD       THIS AN ADD                                  
         BNE   *+12                 NO                                          
                                                                                
         CLI   QMED,C'N'           NO  ADDS FOR MEDIA N                         
         JE    CANMEDER                                                         
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(1),BAGYMD                                                 
         NI    CMLKAM,X'F0'        SET OFF MEDIA                                
         OI    CMLKAM,X'01'        FORCE TO TV                                  
                                                                                
         XC    CODESEQ,CODESEQ                                                  
         MVI   CODESEQ+2,1                                                      
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF CMML SEQ REC FOUND                        
         BNE   VK21                 NO, USE 1                                   
         DROP  R4                                                               
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   CODESEQ,CMLSEQ                                                   
                                                                                
VK21     MVC   HOLDCML(2),AGENCY                                                
         SR    R0,R0                                                            
         ICM   R0,7,CODESEQ                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HOLDCML+2(6),DUB                                                 
         B     VK30                                                             
*                                                                               
* CML ENTERED - CHK FOR CLIENT                                                  
VK22     TM    FLAGS2,ALLCLTR      ALL CLIENT REQUEST                           
         JO    MISSERRC            MUST ENTER CLIENT FOR CML                    
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADD FULLY EDIT CML                        
         BNE   VK24                                                             
                                                                                
         CLI   SVT2PR7,C'Y'        THIS AN AUTO NUMBER CMMLS AGY                
         JE    ENTCMLER             NO CODE ENTRY ALLOWED                       
*                                                                               
VK24     BRAS  RE,VCML             GO VALIDATE COMMERCIAL                       
         MVC   HOLDCML,WORK        SAVE 8 CHAR CMML                             
*                                                                               
VK30     LA    R2,TRAFLTRH         VALIDATE ANY FILTERS                         
         BRAS  RE,VFTR                                                          
* BUILD KEY                                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,HOLDCML                                                  
         MVC   MYKEY,KEY           SAVE RECORD KEY                              
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK32                                                             
         MVC   CMLKID,=X'0AC1'     MAKE SURE ADID NOT ON FILE YET               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     EXITING HERE WILL GIVE ERROR                 
         BE    VK32                                                             
         MVC   KEY,MYKEY           ELSE RESTORE RECORD KEY                      
VK32     J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* READ PROFILES                                                                 
                                                                                
RPROFILE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK           READ T0 PROFILE                              
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'1'         READ T1 PROFILE                              
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVI   WORK+3,C'2'         READ T2 PROFILE                              
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT2PR6,ELEM+5                                                   
         MVC   SVT2PR8,ELEM+7      1 PRODUCT PER COMML                          
         MVC   SVT2PR9,ELEM+8      BRAND AGENCY                                 
         MVI   SVT2PR7,0                                                        
*                                                                               
         MVI   WORK+3,C'3'         READ T3 PROFILE                              
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF,ELEM                                                    
*                                                                               
         MVI   WORK+3,C'T'         READ TT PROFILE                              
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVTTPR3,ELEM+3      SPOTTAL USER Y/N                             
*                                                                               
         CLI   SPOTCAN,C'C'        THIS CANADA                                  
         BNE   RPROFX                                                           
*                                                                               
         MVI   WORK+3,C'2'         READ T2 PROFILE BY AGY/MEDIA                 
         XC    WORK+7(5),WORK+7    CLEAR CLT/OFF                                
*                                                                               
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT2PR7,ELEM+6                                                   
*                                                                               
         MVI   WORK+3,C'3'         READ T3PROF+6 FOR MEDIA T                    
         MVI   WORK+6,C'T'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF+6(1),ELEM+6                                             
*                                                                               
RPROFX   XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* MAKE SURE THIS ADID DOES NOT EXIST AS ADID OR HIDEF ON ANOTHER REC            
*---------------------------------------------------------------------          
*                                                                               
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
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C2'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   EXIT                                                             
                                                                                
DUPADINO MVI   ERROR,RECEXIST                                                   
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
* FOR ACTION ADD, ADD AD-ID ELEMENT FOR PACKED CML                              
* SO ALWAYS HAVE PACKED AND UNPACKED ADID IN A0 ELEMENT FOR KATZ                
*--------------------------------------------------------------                 
*                                                                               
AADIDEL  NTR1  BASE=*,LABEL=*                                                   
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
*                                                                               
DUPADIN2 LHI   R0,DUPCMSCR                                                      
         J     TRAPERR2                                                         
                                                                                
DUPADIN3 LHI   R0,DUPCMFIL                                                      
         J     TRAPERR2                                                         
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY RECORD                                                                
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         BRAS  RE,SETPFK                                                        
                                                                                
         CLI   PFKEY,5             PF5 - CHGHIST                                
         BE    DRC                                                              
*NOP THIS FOR SCRIPT UPLOAD TO WORK                                             
*        CLI   SPOTCAN,C'C'        THIS CANADA                                  
*SM->    BNE   DR02                                                             
         OI    CONSERVH+1,X'01'     FORCE MODIFIED                              
         OI    CONSERVH+6,X'80'     AND XMT                                     
                                                                                
         CLI   PFKEY,4             PF4 - RETURN TO COMMERCIAL                   
         BE    DR02                                                             
         CLI   PFKEY,7             PF7 - TELECASTER SCREEN                      
         BE    DRT                                                              
         CLI   PFKEY,8             PF8 - SUBSTITUTE PRD                         
         BE    DRPRS                                                            
         CLI   MYSCREEN,X'B2'                                                   
         BE    DRT                                                              
DR02     DS    0H                                                               
         MVC   SVF2ACT,CONACT                                                   
*                                                                               
         MVI   SVNXTCHG,0          CLEAR NEXT CHANGE ELEM                       
         CLI   PFKEY,4             PF4 - RETURN TO COMMERCIAL                   
         BE    DR03                                                             
                                                                                
         ZIC   RE,CONRECH+5        LENGTH OF INPUT (RECORD TYPE)                
         BCTR  RE,0                                                             
*                                                                               
DR03     DS    0H                                                               
         CLI   MYSCREEN,X'F2'      IS COMML SCREEN LOADED ALREADY               
         BE    DR08                 YES                                         
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE KEY FIELDS                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,TRAKEYH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'F2'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*MNPFK   MVI   MYSCREEN,X'B2'    SET B2 SCREEN LOADED                           
         MVI   MYSCREEN,X'F2'    SET F2 SCREEN LOADED                           
         BRAS  RE,SETPFK                                                        
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE KEY FIELDS                           
                                                                                
         MVI   MYSCREEN,X'F2'                                                   
         LA    R2,CONHEADH                                                      
         OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
*                                                                               
         SR    R0,R0                                                            
DR05     IC    R0,0(R2)                                                         
         AR    R2,R0               BUMP                                         
         CLI   0(R2),0                                                          
         BNE   DR05                                                             
         MVC   1(2,R2),=X'0101'    POP IN INDICATORS                            
*                                                                               
DR08     LA    R2,TRATYPLH                                                      
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
         ZIC   R0,0(R2)            TO NEXT HELP LINE                            
         AR    R2,R0                                                            
         XC    8(L'TRATYPL,R2),8(R2)                                            
         OI    6(R2),X'80'         XMIT FLD                                     
                                                                                
         L     R6,AIO                                                           
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    DR60                YES, ONLY DISPL TITLE (PROD HOUSE)           
*                                                                               
         BAS   RE,PPRD             GO PRINT PRODUCT LIST                        
         CLC   TRAPLST,WORK                                                     
         BE    *+14                                                             
         MVC   TRAPLST,WORK                                                     
         OI    TRAPLSTH+6,X'80'                                                 
*                                                                               
DR10     MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   WORK(L'CMLTITLE),CMLTITLE                                        
*                                                                               
         CLC   TRADSC1,WORK         TITLE                                       
         BE    *+14                                                             
         MVC   TRADSC1,WORK                                                     
         OI    TRADSC1H+6,X'80'                                                 
                                                                                
         TM    CMLSTAT,X'80'       IS RECORD SOFT DELETED                       
         BZ    DR20                NO                                           
         MVC   TRADEL,ASTDEL                                                    
         B     DR22                                                             
*                                                                               
DR20     OC    TRADEL,TRADEL                                                    
         BZ    DR24                                                             
         XC    TRADEL,TRADEL                                                    
*                                                                               
DR22     OI    TRADELH+6,X'80'                                                  
                                                                                
DR24     DS    0H                                                               
         TM    FTRFLAG,SEQFTR      SHOW COMML SEQ NO                            
         BZ    DR26                                                             
                                                                                
         EDIT  CMLSEQ,(6,TRADEL+3)                                              
         OI    TRADELH+6,X'80'                                                  
                                                                                
DR26     XC    FLD(24),FLD                                                      
         EDIT  (1,CMLSLN),(5,TRASLN),ZERO=BLANK,ALIGN=LEFT                      
         OI    TRASLNH+6,X'80'                                                  
                                                                                
         XC    TRAOVRD,TRAOVRD                                                  
         OI    TRAOVRDH+6,X'80'                                                 
         OC    CMLOVRD1(2),CMLOVRD1    ANY PRINT OVERRIDE?                      
         BZ    DR30                                                             
         XC    FLD(8),FLD                                                       
         EDIT  (1,CMLOVRD1),(3,FLD),ALIGN=LEFT,ZERO=NOBLANK                     
         LA    R1,FLD                                                           
         AR    R1,R0                   NEXT BLANK SPACE                         
         MVI   0(R1),C'/'                                                       
         EDIT  (1,CMLOVRD2),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   TRAOVRD,FLD                                                      
                                                                                
DR30     GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,TRARLSE)                              
         OI    TRARLSEH+6,X'80'                                                 
                                                                                
         XC    TRARCL,TRARCL                                                    
         MVC   TRARCL(3),=CL3'UFN'                                              
         OI    TRARCLH+6,X'80'                                                  
         CLC   CMLRCL,=X'FFFFFF'                                                
         BE    DR32                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRCL),(5,TRARCL)                                
*                                                                               
DR32     MVC   TRATYPE,CMLTYPE                                                  
         OI    TRATYPEH+6,X'80'                                                 
                                                                                
         XC    TRACTXT,TRACTXT                                                  
         TM    CMLSTAT,X'40'       COMML TEXT REC FOR THIS COMML                
         BZ    *+10                                                             
         MVC   TRACTXT(23),CMLTXTMS                                             
         XC    TRACTXT+1(22),SPACES    LOWERCASE                                
         OI    TRACTXTH+6,X'80'                                                 
                                                                                
         MVC   TRACLTN,CMLCLTNO                                                 
         OI    TRACLTNH+6,X'80'                                                 
*                                                                               
         MVC   TRASOLO(1),CMLSOLO                                               
         MVI   TRASOLO+1,X'40'                                                  
         OI    TRASOLOH+6,X'80'                                                 
                                                                                
         MVC   TRATTEX(L'CMLTALEX),CMLTALEX                                     
         OI    TRATTEXH+6,X'80'                                                 
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(4),CMLCLASS                                                 
                                                                                
         MVI   ELCODE,X'21'                                                     
         LR    R3,R6                                                            
         L     R6,AIO                                                           
         LA    R4,ELEM                                                          
         BRAS  RE,GETEL                                                         
         BNE   DR50                                                             
         B     DR44                                                             
*                                                                               
DR43     BRAS  RE,NEXTEL                                                        
         BNE   DR50                                                             
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
*                                                                               
         USING CMLCLSEL,R6                                                      
DR44     MVC   0(4,R4),CMLCLS                                                   
         LA    R4,4(R4)                                                         
         LA    RE,CMLCLS+3                                                      
*                                                                               
DR45     CLI   0(RE),C' '                                                       
         BH    DR46                                                             
         BCTR  R4,0                                                             
         BCT   RE,DR45                                                          
*                                                                               
DR46     MVI   0(R4),C'='                                                       
         EDIT  (B2,CMLCLSPC),(6,1(R4)),2,ALIGN=LEFT                             
         AR    R4,R0                                                            
         B     DR43                                                             
                                                                                
DR50     LR    R6,R3               RESTORE R6                                   
         DROP  R6                                                               
         CLC   TRACLS,ELEM                                                      
         BE    *+14                                                             
         MVC   TRACLS,ELEM                                                      
         OI    TRACLSH+6,X'80'                                                  
                                                                                
         XC    TRADSC2,TRADSC2     TITLE 2                                      
         OI    TRADSC2H+6,X'80'                                                 
         XC    TRADSC3,TRADSC3     TITLE 3                                      
         OI    TRADSC3H+6,X'80'                                                 
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   DR52                                                             
         MVC   TRADSC2(20),3(R6)   TITLE 2                                      
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   DR52                                                             
         MVC   TRADSC3(20),3(R6)   TITLE 3                                      
                                                                                
*        DISPLAY ACTUAL CMMLS                                                   
DR52     DS    0H                                                               
         LA    R2,TRAACT1H                                                      
         LA    R0,NUMACTS                                                       
         SR    RF,RF                                                            
         L     R6,AIO                                                           
         USING CMLACTEL,R6                                                      
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR53     BRAS  RE,NEXTEL                                                        
         BNE   DR55                                                             
                                                                                
         MVC   8(L'CMLACTID,R2),CMLACTID  DISP 8 CHAR CML                       
         CLI   CMLACTLN,CMLACTL1          OLD ELEM                              
         BE    *+10                                                             
         MVC   8(L'CMLACTCM,R2),CMLACTCM   NO, DISP 12 CHAR CML                 
                                                                                
         OI    6(R2),X'80'         XMIT                                         
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR53                                                          
         B     DR56                                                             
*                                                                               
DR55     XC    8(12,R2),8(R2)                                                   
         OI    6(R2),X'80'         XMIT                                         
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,DR55                                                          
*                                                                               
* DISPLAY AD-ID NO                                                              
*                                                                               
DR56     XC    TRAADID,TRAADID                                                  
         OI    TRAADIDH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                                                           
         TM    15(RE),CMLKSTA_PCKD IF CMML NOT PACKED                           
         BZ    DR57                DISPLAY ADID IF THERE IS ONE                 
*                                                                               
         L     R6,AIO                                                           
         GOTO1 VTRPACK,DMCB,(C'U',5(R6)),WORK                                   
         CLI   WORK+8,C' '         IF CMML IS AN ADID                           
         BH    DR57X               DON'T DISPLAY ADID                           
*                                                                               
DR57     L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR57X                                                            
*                                                                               
         USING CMLADIEL,R6                                                      
         CLI   CMLADID+8,C' '      BUT DO NOT DISPLAY IF NOT                    
         BNH   DR57X               A REAL ADID                                  
         MVC   TRAADID(L'CMLADID),CMLADID                                       
         OI    TRAADIDH+6,X'80'          FORCE TRANSMIT                         
*                                                                               
DR57X    B     DR65                                                             
         DROP  R6                                                               
         EJECT                                                                  
*======================================================================         
* DISPLAY PROD HOUSE KEY ONLY (ALL 9'S CML ID) AND CLEAR ALL OTHER FLDS         
*======================================================================         
                                                                                
DR60     L     R6,AIO                                                           
         XC    WORK,WORK                                                        
         LA    R0,8                NUMBER OF FLDS TO CLEAR                      
         LA    R1,TRADELH          1ST FLD                                      
DR62     ZIC   RE,0(R1)            GET FLD LEN                                  
         AHI   RE,-9                                                            
         EX    RE,DRCLC                                                         
         BE    *+12                                                             
         EX    RE,DRMVC                                                         
         OI    6(R1),X'80'                                                      
         LA    R1,9(RE,R1)                                                      
         ZIC   RE,0(,R1)                                                        
         AR    R1,RE                                                            
         BCT   R0,DR62                                                          
         LA    R0,4                NUMBER OF FLDS TO CLEAR                      
         LA    R1,TRACTXTH         1ST FLD                                      
DR64     ZIC   RE,0(R1)            GET FLD LEN                                  
         SH    RE,=H'9'                                                         
         EX    RE,DRCLC                                                         
         BE    *+12                                                             
         EX    RE,DRMVC                                                         
         OI    6(R1),X'80'                                                      
         LA    R1,9(RE,R1)                                                      
         ZIC   RE,0(,R1)                                                        
         AR    R1,RE                                                            
         BCT   R0,DR64                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
         USING CMLDTAEL,R6                                                      
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(L'CMLTITLE),CMLTITLE                                        
         CLC   WORK+6(9),=C'=PROD HSE'  IS THIS PROD HOUSE RECORD               
         BNE   *+10                                                             
         MVC   WORK+7(4),=C'SHIP'   DISPLAY AS SHIP HOUSE                       
*                                                                               
         MVC   TRADSC1,WORK        TITLE 1                                      
         OI    TRADSC1H+6,X'80'                                                 
         XC    TRADSC2,TRADSC2     TITLE 2                                      
         OI    TRADSC2H+6,X'80'                                                 
         XC    TRADSC3,TRADSC3     TITLE 3                                      
         OI    TRADSC3H+6,X'80'                                                 
         B     DRX                                                              
         EJECT                                                                  
DR65     DS    0H                                                               
         XC    TRASTIM,TRASTIM     CLEAR MATCH DATA                             
         OI    TRASTIMH+6,X'80'                                                 
         XC    TRAETIM,TRAETIM                                                  
         OI    TRAETIMH+6,X'80'                                                 
         MVI   TRADLY,C'N'                                                      
         OI    TRADLYH+6,X'80'                                                  
*                                                                               
         LA    R3,6                                                             
         LA    R2,TRADT1H                                                       
*                                                                               
DR66     XC    8(L'TRADT1,R2),8(R2)       CLEAR IT                              
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R3,DR66                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CMLMATEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   DR105                                                            
         USING CMLMATCH,R6                                                      
                                                                                
* NOTE, THAT THIS LOOP RELIES ON 6 EQUAL LENGTH PERIOD FIELDS                   
* TAKE GREAT CARE WHEN CHANGING THE SCREEN, PLEASE                              
                                                                                
         LHI   R3,6                                                             
         LA    R2,TRADT1H                                                       
         LA    R5,CMLMPER1                                                      
                                                                                
* MATCH DATE RANGE DISPLAY LOOP                                                 
                                                                                
DR70     DS    0H                                                               
         OC    0(L'CMLMPER1,R5),0(R5)  ANYTHING HERE?                           
         BZ    DR80                    NO, GO TO NEXT FIELD                     
*                                                                               
         GOTO1 DATCON,DMCB,(X'12',0(R5)),(5,8(R2))                              
*                                                                               
DR80     DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LA    R5,L'CMLMPER1(R5)                                                
         BCT   R3,DR70                                                          
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET ADDRESS OF UNTIME                 
         L     RF,0(R1)                                                         
*                                                                               
         OC    CMLMSTIM,CMLMSTIM   MATCH START TIME                             
         BZ    DR90                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMSTIM                                                 
         GOTO1 (RF),DMCB,WORK,TRASTIM                                           
         OI    TRASTIMH+6,X'80'                                                 
*                                                                               
DR90     DS    0H                                                               
         OC    CMLMETIM,CMLMETIM   MATCH END TIME                               
         BZ    DR100                                                            
         CLC   CMLMETIM,=X'FFFF'                                                
         BE    DR100                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMETIM                                                 
         GOTO1 (RF),DMCB,WORK,TRAETIM                                           
         OI    TRAETIMH+6,X'80'                                                 
*                                                                               
DR100    DS    0H                  CHECK TIMES DAILY                            
         MVI   TRADLY,C'N'                                                      
         OI    TRADLYH+6,X'80'                                                  
         TM    CMLMFLAG,CMLMFDAY                                                
         BZ    *+8                                                              
         MVI   TRADLY,C'Y'                                                      
*                                                                               
DR105    XC    TRAPRNT,TRAPRNT                                                  
         OI    TRAPRNTH+6,X'80'                                                 
*                                                                               
         XC    TRACNTR,TRACNTR                                                  
         OI    TRACNTRH+6,X'80'                                                 
*                                                                               
         XC    TRAHDEF,TRAHDEF                                                  
         OI    TRAHDEFH+6,X'80'                                                 
*                                                                               
         XC    TRASWAP,TRAHDEF                                                  
         OI    TRASWAPH+6,X'80'                                                 
*                                                                               
         XC    TRAPRHS,TRAPRHS                                                  
         OI    TRAPRHSH+6,X'80'                                                 
*                                                                               
         XC    TRADSDT,TRADSDT                                                  
         OI    TRADSDTH+6,X'80'                                                 
*                                                                               
         XC    TRADSTM,TRADSTM                                                  
         OI    TRADSTMH+6,X'80'                                                 
*                                                                               
         XC    SVHIDEF,SVHIDEF                                                  
         XC    SVCNTCT,SVCNTCT                                                  
         XC    SVHIDEFX,SVHIDEFX                                                
         XC    SVCNTCTX,SVCNTCTX                                                
*                                                                               
         MVI   ELCODE,X'24'        SEARCH FOR EXTENDED DATA ELEMENT             
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         USING CMLXDTEL,R6                                                      
         MVC   TRAPRHS,CMLXPRHS                                                 
         MVC   TRAHDEF,CMLXHDEF                                                 
         MVC   TRACNTR,CMLXCNTR                                                 
         MVC   TRAPRNT,CMLXPRNT                                                 
*                                                                               
         MVC   TRASWAP,CMLXSWAP                                                 
*                                                                               
         MVC   SVHIDEF,CMLXHDEF     NEEDED FOR PASSIVES POINTER MAINT           
         MVC   SVCNTCT,CMLXCNTR                                                 
         MVC   SVHIDEFX,CMLXHDPK                                                
         MVC   SVCNTCTX,CMLXCCPK                                                
*                                                                               
         CLI   CMLXDSDT,0                                                       
         BE    DR110                                                            
         GOTO1 DATCON,DMCB,(3,CMLXDSDT),(8,TRADSDT)                             
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET ADDRESS OF UNTIME                 
         L     RF,0(R1)                                                         
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),CMLXDSTM                                                 
         GOTO1 (RF),DMCB,WORK,TRADSTM                                           
         DROP  R6                                                               
                                                                                
* GET PROD HOUSE NAME TO DISPLAY                                                
                                                                                
DR110    XC    TRAPHNM,TRAPHNM     CLEAR NAME                                   
         OI    TRAPHNMH+6,X'80'                                                 
         LA    R2,TRAPRHSH                                                      
         CLI   8(R2),C' '          IS THERE A PRODUCTION HOUS                   
         BNH   DR120               NO                                           
         BRAS  RE,VHSE             READ THE PROD HOUSE KEY                      
*                                                                               
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         ST    R0,AIO                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRHDTAEL,R6                                                      
         MVC   TRAPHNM,PRHLINE1                                                 
         DROP  R6                                                               
*                                                                               
DR120    B     DRX                                                              
*                                                                               
DRCLC    CLC   8(0,R1),WORK                                                     
DRMVC    MVC   8(0,R1),WORK                                                     
CLCLEGAL CLC   CONRECH+8(0),=C'LCOMML'                                          
CLCBROAD CLC   CONRECH+8(0),=C'BCOMML'                                          
CMLTXTMS DC    C'COMMERCIAL TEXT PRESENT'                                       
                                                                                
*                                                                               
* DISPLAY LEGAL SCREEN                                                          
*                                                                               
*DRL      DS    0H                                                              
*         CLC   =C'SJ',AGENCY       AGENCY IS SJR TEST                          
*         BE    DRL00                                                           
*         CLC   =C'MC',AGENCY       AGENCY IS GM MCCANN                         
*         BE    DRL00                                                           
*         CLC   =C'H9',AGENCY       STARCOM ?                                   
*         BNE   DR03                NO, GO RE-DISPLAY DISPLAY SCREEN            
*                                                                               
*DRL00    DS    0H                                                              
*         BRAS  RE,DRLEGAL          DISPLAY LEGAL SCREEN                        
*         B     DRXALL                                                          
*         EJECT                                                                 
* DISPLAY BROADCAST BUSINESS SCREEN                                             
*                                                                               
*DRB      DS    0H                                                              
*         CLC   =C'SJ',AGENCY       AGENCY IS SJR TEST                          
*         BE    DRB00                                                           
*         CLC   =C'MC',AGENCY       AGENCY IS GM MCCANN                         
*         BE    DRB00                                                           
*         CLC   =C'H9',AGENCY       STARCOM ?                                   
*         BNE   DR03                NO, GO RE-DISPLAY DISPLAY SCREEN            
*                                                                               
*DRB00    DS    0H                                                              
*         BRAS  RE,DRBROAD          DISPLAY BROADCAST SCREEN                    
*         B     DRXALL                                                          
*                                                                               
DRT      DS    0H                                                               
         BRAS  RE,DRTELE            DISPLAY TLCSTR SCREEN                       
         LA    R2,TRAMEDH                                                       
         B     DRXALL                                                           
*                                                                               
DRPRS    BRAS  RE,DRPRSUB          DISPLAY PRD SUBST SCREEN                     
         LA    R2,TRAMEDH                                                       
         B     DRXALL                                                           
*                                                                               
DRC      DS    0H                                                               
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(7),=C'DISPLAY'                                            
         OI    CONACTH+6,X'80'                                                  
         MVI   ACTNUM,ACTDIS                                                    
         MVI   ACTEQU,ACTDIS                                                    
*                                                                               
         BRAS  RE,DSPCHG           DISPLAY CHANGE HISTORY                       
         OI    TRTMEDH+6,X'40'      POSITION CURSOR                             
         MVI   MYSCREEN,X'DF'                                                   
         MVI   TWASCR,X'DF'                                                     
*                                                                               
DRX      DS    0H                                                               
                                                                                
         LR    RE,RA                                                            
         AHI   RE,THISLSEL-T216FFD                                              
         CLI   0(RE),C'C'                                                       
         BNE   *+8                                                              
         OI    TRAPLSTH+1,X'01'     MODIFIED                                    
         OI    TRAPLSTH+6,X'80'     TRANSMIT                                    
*                                                                               
DRXALL   XIT1                                                                   
         EJECT                                                                  
*===================================================================            
* FIND, EDIT, AND PRINT PRODUCT LIST                                            
*===================================================================            
                                                                                
PPRD     NTR1                                                                   
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6                                                      
         MVC   WORK,SPACES                                                      
         CLI   CMLPRDS,X'FF'       IS THIS PRD=ALL                              
         BNE   PPRD06              NO                                           
         MVC   WORK(7),=CL7'PRD=ALL'                                            
         B     DRXALL                                                           
*                                                                               
PPRD06   ZIC   R3,CMLPRDLN         GET PROD LIST ELEM LEN                       
         BCTR  R3,0                                                             
         BCTR  R3,0                NOW # PRODS IN LIST                          
         LA    R4,CMLPRDS          POINT TO START BIN PROD LIST                 
         LA    R5,WORK             OUTPUT AREA                                  
*                                                                               
PPRD10   L     R1,ASVCLIST                                                      
*                                                                               
PPRD12   CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,=C'***'                                                       
         B     PPRD14                                                           
         CLC   0(1,R4),3(R1)                                                    
         BE    PPRD14                                                           
         LA    R1,4(R1)                                                         
         B     PPRD12                                                           
*                                                                               
PPRD14   MVC   0(3,R5),0(R1)       STORE IN WORK AREA                           
         LA    R5,2(,R5)                                                        
*                                                                               
PPRD20   CLI   0(R5),C' '          FIND END OF PROD CODE                        
         BNH   PPRD22                                                           
         LA    R5,1(,R5)                                                        
*                                                                               
PPRD22   MVI   0(R5),C','          SEPARATE PRODUCTS                            
         LA    R4,1(,R4)           PRODUCT LIST POINTER                         
         LA    R5,1(,R5)                                                        
         BCT   R3,PPRD10                                                        
         BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         B     DRXALL                                                           
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
DATERRB  MVI   ERROR,INVDATE                                                    
                                                                                
ERRX     GOTO1 ERREX                                                            
                                                                                
*REFNOERR LHI   R0,REFNOMSG                                                     
*         J     TRAPERR2                                                        
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD (LEGAL SCREEN)                                                
*                                                                               
*VRLEGAL  NTR1  BASE=*,LABEL=*                                                  
*                                                                               
*         MVI   ELCODE,X'70'        LEGAL COMMENT ELEM                          
*VRL10    XC    ELEM,ELEM                                                       
*         GOTO1 REMELEM                                                         
*                                                                               
*         CLI   ELCODE,X'80'                                                    
*         BE    *+12                                                            
*         MVI   ELCODE,X'80'        RESTRICTION COMMENT                         
*         B     VRL10                                                           
*                                                                               
*         LA    R2,TRLLCMTH         LEGAL COMMENT                               
*         MVI   ELCODE,X'70'        LEGAL COMMENT ELEM                          
*                                                                               
** USE THIS CODE FOR BOTH LEGAL COMMENTS & SCHEDULE RESTRICTION COMMENT         
*                                                                               
*VRL20    LA    R5,1                LINE COUNTER                                
*                                                                               
*VRL30    XC    ELEM,ELEM                                                       
*                                                                               
*         LA    R6,ELEM                                                         
*         USING CMLLCTEL,R6         USE IT FOR BOTH LEGAL/SCHEDULE COMT         
*                                                                               
*         MVC   CMLLCTEL,ELCODE                                                 
*         STC   R5,CMLLCTNO         LINE NUMBER                                 
*                                                                               
*         ZIC   R1,5(R2)            GET INPUT LENGTH                            
*         LTR   R1,R1               ANY COMMENT                                 
*         BZ    VRL35                                                           
*                                                                               
*         BCTR  R1,0                INPUT LENGTH -1                             
*         EX    R1,VRLMVC                                                       
*                                                                               
*         AHI   R1,4                TOTAL ELEMENT LENGTH                        
*         STC   R1,CMLLCTLN         ELEM LENGTH                                 
*                                                                               
*         GOTO1 ADDELEM                                                         
*                                                                               
*         LA    R5,1(R5)            INCREMENT COUNTER                           
*                                                                               
*VRL35    ZIC   R1,0(R2)            GET FIELD LENGTH                            
*         AR    R2,R1               BUMP TO NEXT FIELD                          
*                                                                               
*         LA    R0,TRLLCMXH         LAST LEGAL COMMENT                          
*         CLI   ELCODE,X'70'        ARE WE DOING LEGAL COMMENTS                 
*         BE    *+8                                                             
*         LA    R0,TRLSCMXH         NO, SCHEDULING COMMENTS                     
*         CR    R2,R0                                                           
*         BNH   VRL30                                                           
*                                                                               
*         CLI   ELCODE,X'80'        DID WE DO SCHEDULING CMT YET                
*         BE    VRL50               YES, DONE                                   
*                                                                               
*         MVI   ELCODE,X'80'                                                    
*         LA    R2,TRLSCMTH         SCHEDULING COMMENT                          
*         B     VRL20                                                           
*                                                                               
*VRL50    DS    0H                                                              
*         CLI   TRLATAH+5,0         APPROVED TO AIR ENTERED ?                   
*         BE    VRLX                 NO, DONE                                   
*                                                                               
*         L     R6,AIO                                                          
*         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                     
*         BRAS  RE,GETEL                                                        
*         BNE   VRL60                                                           
*                                                                               
*         USING CMLBBEL,R6                                                      
*         MVC   CMLATAIR,TRLATA     APPROVED TO AIR                             
*         B     VRLX                                                            
*                                                                               
*VRL60    XC    ELEM,ELEM                                                       
*         LA    R6,ELEM                                                         
*                                                                               
*         MVI   CMLBBEL,X'90'       BROADCAST BUSINESS ELEMENT                  
*         MVI   CMLBBLN,CMLBBX-CMLBBEL   ELEMENT LENGTH FOR ADD                 
*         MVC   CMLATAIR,TRLATA     APPROVED TO AIR                             
*                                                                               
*         GOTO1 ADDELEM                                                         
*                                                                               
*VRLX     XIT1                                                                  
*                                                                               
*VRLMVC   MVC   3(0,R6),8(R2)                                                   
*                                                                               
*         EJECT                                                                 
*         LTORG                                                                 
*         DROP  RB                                                              
*         EJECT                                                                 
** DISPLAY BROADCAST BUSINESS SCREEN                                            
*                                                                               
*DRBROAD  NTR1  BASE=*,LABEL=*                                                  
*                                                                               
*         CLI   MYSCREEN,X'6F'      IS BRDCST SCREEN LOADED ALREADY             
*         BE    DRB40                YES                                        
*                                                                               
*         XC    DMCB(24),DMCB                                                   
*         LA    RE,CONTAGH                                                      
*         ST    RE,DMCB                                                         
*         MVI   DMCB,X'6F'                                                      
*         GOTO1 CALLOV,DMCB                                                     
*         CLI   4(R1),X'FF'                                                     
*         BNE   *+6                                                             
*         DC    H'0'                                                            
*                                                                               
*         MVI   MYSCREEN,X'6F'      LOADING BROADCAST SCREEN                    
*         MVC   CONREC(6),=C'BCOMML'                                            
*         MVC   CONACT(7),=C'DISPLAY'                                           
*         BRAS  RE,SETPFK                                                       
*                                                                               
*         LA    R2,CONHEADH                                                     
*                                                                               
*DRB20    OI    6(R2),X'80'         TURN ON TRANSMIT BITS                       
*                                                                               
*         ZIC   R1,0(R2)                                                        
*         AR    R2,R1               BUMP                                        
*                                                                               
*         CLI   0(R2),0                                                         
*         BNE   DRB20                                                           
*                                                                               
*         MVC   1(2,R2),=X'0101'    POP IN INDICATORS                           
*                                                                               
** RE-DISPLAY MEDIA/CLIENT/COMMERCIAL                                           
*                                                                               
*         MVC   TRAMED(L'QMED),QMED                                             
*         OI    TRAMEDH+6,X'80'                                                 
*                                                                               
*         MVC   TRACLT(L'QCLT),QCLT                                             
*         OI    TRACLTH+6,X'80'                                                 
*                                                                               
*         XC    TRACML,TRACML                                                   
*         MVC   TRACML(L'HOLDCML),HOLDCML                                       
*         OI    TRACMLH+6,X'80'                                                 
*                                                                               
*         OI    TRBPCMLH+6,X'40'    CURSOR POSITION                             
*                                                                               
** CLEAR FIELDS ON THE SCREEN                                                   
*                                                                               
*DRB40    DS    0H                                                              
*         LA    R2,TRBBCDEH                                                     
*         LA    R0,TRBTAGH                                                      
*                                                                               
*DRB42    ZIC   RF,0(R2)            GET FIELD LENGTH                            
*         LR    R1,RF                                                           
*         AHI   RF,-9               GET FIELD LENGTH -1                         
*                                                                               
*         EX    RF,*+8                                                          
*         B     *+10                                                            
*         XC    8(0,R2),8(R2)       CLEAR FIELD                                 
*         OI    6(R2),X'80'                                                     
*                                                                               
*         AR    R2,R1               PROTECTED TITLE FIELD                       
*         ZIC   RF,0(R2)            GET FLD LEN                                 
*         AR    R2,RF               NEXT INPUT FIELD                            
*                                                                               
*         CR    R2,R0               CK IF END OF SCREEN                         
*         BL    DRB42                NO, CONTINUE                               
*                                                                               
*         MVI   TRBBAGY,C'Y'        SET DEFAULT (BRAND AGY=Y)                   
*         CLI   SVT2PR9,C'Y'        BRAND AGENCY                                
*         BE    *+8                                                             
*         MVI   TRBBAGY,C'N'                                                    
*                                                                               
*         L     R6,AIO                                                          
*         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                     
*         BRAS  RE,GETEL                                                        
*         BNE   DRB55               DISPLAY DEFAULTS                            
*                                                                               
*         USING CMLBBEL,R6                                                      
*                                                                               
*         MVC   TRBPCML,CMLBBBCP    BASIC CODE (PARENT CML)                     
*         MVC   TRBBCDE,CMLBBBCM    BASIC CODE (MUSIC)                          
*                                                                               
*         CLI   CMLBBBAG,0                                                      
*         BE    *+20                                                            
*         CLC   CMLBBBAG,SVT2PR9    IS BRAND AGY SAME AS PROFILE                
*         BE    *+10                 YES                                        
*         MVC   TRBBAGY(L'CMLBBBAG),CMLBBBAG  BRAND AGY (LEO B. - Y/N)          
*                                                                               
*         MVC   TRBJOBN,CMLBBJOB    JOB NUMBER                                  
*         MVC   TRBBAPP(L'CMLBBAPR),CMLBBAPR BROADCST BUSINESS APPROVAL         
*         MVC   TRBREF,CMLBBREF     REFERENCE #                                 
*                                                                               
*         OC    CMLBBPDT,CMLBBPDT   ANY PRODUCTION DATE                         
*         BZ    DRB48                                                           
*                                                                               
*         GOTO1 DATCON,DMCB,(3,CMLBBPDT),(8,TRBPRDT) PRODUCTION DATE            
*                                                                               
*DRB48    OC    CMLBBCAD,CMLBBCAD   CLIENT APPROVAL DATE                        
*         BZ    DRB50                                                           
*                                                                               
*         GOTO1 DATCON,DMCB,(3,CMLBBCAD),(8,TRBCADT) CLIENT APPROV DTE          
*                                                                               
*DRB50    OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                            
*         BZ    DRB55                                                           
*                                                                               
*         GOTO1 DATCON,DMCB,(3,CMLBBMXD),(8,TRBMDTE) MAX USE DATE               
*                                                                               
*DRB55    L     R6,AIO                                                          
*         MVI   ELCODE,X'92'        INACTIVE ELEMENT                            
*         BRAS  RE,GETEL                                                        
*         BNE   DRB60                                                           
*                                                                               
*         USING CMLINAEL,R6                                                     
*                                                                               
*         ZIC   R1,1(R6)            GET ELEM LEN                                
*         AHI   R1,-3               GET DATA LEN-1                              
*         EX    R1,*+8                                                          
*         B     *+10                                                            
*         MVC   TRBIRSN(0),CMLINRSN RSN INACTIVE                                
*                                                                               
*DRB60    L     R6,AIO                                                          
*         MVI   ELCODE,X'95'        REASON ELEMENT                              
*         BRAS  RE,GETEL                                                        
*         BNE   DRB70                                                           
*                                                                               
*         USING CMLRSNEL,R6                                                     
*                                                                               
*         ZIC   R1,1(R6)            GET ELEM LEN                                
*         AHI   R1,-7               GET DATA LEN-1                              
*         EX    R1,*+8                                                          
*         B     *+10                                                            
*         MVC   TRBRSN(0),CMLRSN    REASON FOR CHANGE                           
*                                                                               
*DRB70    L     R6,AIO                                                          
*         MVI   ELCODE,X'10'        CMML DATA ELEMENT                           
*         BRAS  RE,GETEL                                                        
*         BNE   DRB80                                                           
*                                                                               
*         USING CMLDTAEL,R6                                                     
*                                                                               
*         GOTO1 DATCON,DMCB,(3,CMLRLSE),(8,TRBACDT) ACTIVE DATE                 
*                                                                               
*         CLC   CMLRCL,=X'FFFFFF'                                               
*         BNE   *+14                                                            
*         MVC   TRBIDTE(3),=CL3'UFN'                                            
*         B     DRB80                                                           
*                                                                               
*         GOTO1 (RF),(R1),(3,CMLRCL),(5,TRBIDTE) INACTIVE DATE                  
*                                                                               
*DRB80    L     R6,AIO                                                          
*         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                        
*         BRAS  RE,GETEL                                                        
*         BNE   DRBX                                                            
*         USING ACTVD,R6                                                        
*                                                                               
*         LA    R3,ACTVADDT         DATE CML RECORD ADDED                       
*         GOTO1 DATCON,DMCB,(3,(R3)),(8,TRBADTE)                                
*         OI    TRBADTEH+6,X'80'                                                
*                                                                               
*DRBX     OI    TRBPCMLH+1,X'01'    MODIFIED                                    
*         OI    TRBPCMLH+6,X'80'    TRANSMIT                                    
*         XIT1                                                                  
*         LTORG                                                                 
*         DROP  RB                                                              
*         EJECT                                                                 
*                                                                               
                                                                                
* FILTER CMMLS FOR LIST FUNCTION                                                
                                                                                
FTR      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         USING CMLDTAEL,R6                                                      
         TM    FTRFLAG,DELFTR      ONLY DELETED CML'S                           
         BZ    FTR04                NO                                          
         TM    CMLSTAT,X'80'       THIS DELETED                                 
         BZ    FTRNO                                                            
         B     FTR06                                                            
FTR04    TM    CMLSTAT,X'80'                                                    
         BNZ   FTRNO                                                            
                                                                                
FTR06    OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    FTR10               NO                                           
         CLC   TYPEFTR,CMLTYPE     THIS IT                                      
         BNE   FTRNO                                                            
                                                                                
FTR10    OC    SLNFTR,SLNFTR       SPOT LEN FILTER                              
         BZ    FTR14               NO                                           
         CLC   SLNFTR,CMLSLN       DOES THIS MATCH                              
         BNE   FTRNO               NO                                           
                                                                                
FTR14    TM    FTRFLAG,EASIFTR     FILTERING ON EASI COMMLS                     
         BZ    FTR20                NO                                          
         TM    CMLSTAT,X'20'                                                    
         BZ    FTRNO               NOT AN EASI COMML                            
                                                                                
FTR20    OC    RLDTFTR,RLDTFTR     RELEASE DATE FILTER                          
         BZ    FTR30                                                            
         CLI   RLDTSFTR,0                                                       
         BE    FTR24                                                            
                                                                                
         CLI   RLDTSFTR,X'6E'      GREATER THAN                                 
         BNE   FTR22                MUST BE LESS THAN                           
                                                                                
         CLC   CMLRLSE,RLDTFTR     FILTER TO RELEASE                            
         BL    FTRNO                BYPASS                                      
         B     FTR30               CK NEXT FILTER                               
                                                                                
FTR22    CLI   RLDTSFTR,X'4C'      LESS THAN                                    
         BE    *+6                  MUST BE LESS THAN                           
         DC    H'0'                                                             
         CLC   CMLRLSE,RLDTFTR                                                  
         BH    FTRNO                                                            
         B     FTR30                                                            
                                                                                
FTR24    CLC   CMLRLSE,RLDTFTR                                                  
         BNE   FTRNO                                                            
                                                                                
FTR30    OC    RCDTFTR,RCDTFTR   RECALL DATE FILTER                             
         BZ    FTR40                                                            
         CLI   RCDTSFTR,0                                                       
         BE    FTR34                                                            
                                                                                
         CLI   RCDTSFTR,X'6E'     GREATER THAN                                  
         BNE   FTR32               MUST BE LESS THAN                            
                                                                                
         CLC   CMLRCL,RCDTFTR     FILTER TO RECALL DATE                         
         BL    FTRNO               BYPASS                                       
         B     FTR40              CK NEXT FILTER                                
                                                                                
FTR32    CLI   RCDTSFTR,X'4C'      LESS THAN                                    
         BE    *+6                  MUST BE LESS THAN                           
         DC    H'0'                                                             
         CLC   CMLRCL,RCDTFTR                                                   
         BH    FTRNO                                                            
         B     FTR40                                                            
FTR34    CLC   RCDTFTR,CMLRCL                                                   
         BNE   FTRNO                                                            
                                                                                
FTR40    OC    CLSFTR,CLSFTR       CLASS FILTER                                 
         BZ    FTR50                NO                                          
         CLC   CLSFTR,CMLCLASS     DOES THIS MATCH                              
         BE    FTR50                YES                                         
                                                                                
         LA    R0,4                                                             
         LA    RE,CLSFTR                                                        
         LA    RF,CMLCLASS                                                      
FTR44    CLI   0(RE),C'*'          WILDCARD                                     
         BE    FTR46                                                            
         CLI   0(RE),0             NO CHAR ENT, TREAT AS WILDCARD               
         BE    FTR46                                                            
         CLC   0(1,RE),0(RF)       DOES NON WILDCARD MATCH                      
         BNE   FTRNO               NO                                           
FTR46    LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,FTR44                                                         
         EJECT                                                                  
                                                                                
FTR50    OC    PRODFTR,PRODFTR     IS THERE A PROD FILTER                       
         BZ    FTR60               NO                                           
         MVI   ELCODE,X'20'                                                     
         LR    R6,R4                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   PRODFTR,=C'ALL'                                                  
*        BE    FTR54                                                            
         USING CMLPRDEL,R6                                                      
         ZIC   R0,CMLPRDLN                                                      
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,CMLPRDS          START OF PROD LIST                           
FTR52    CLC   PRDFTR,0(R1)                                                     
         BE    FTR60                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,FTR52                                                         
         B     FTRNO                                                            
                                                                                
FTR60    TM    FTRFLAG,TELEFTR     FILTERING ON TELECASTER                      
         BZ    FTR64                NO                                          
         LR    R6,R4                                                            
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FTRNO                                                            
                                                                                
FTR64    TM    FTRFLAG,ADIDFTR     FILTERING ON AD-ID                           
         BZ    FTR70                NO                                          
         LR    R6,R4                                                            
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FTRNO                                                            
*                                                                               
         OC    ADIDFLT,ADIDFLT     LOOKING FOR SPECIFIC ADID                    
         BZ    FTR70                NO, DONE                                    
         SPACE                                                                  
         USING CMLADIEL,R6                                                      
         SPACE                                                                  
         CLC   CMLADID,ADIDFLT     SAME ?                                       
         BNE   FTRNO                                                            
*                                                                               
FTR70    CR    R1,R1               SET COND CODE FILTERED OK                    
         B     FTRXIT                                                           
FTRNO    CR    RB,RD               SET COND CODE NO FILTER                      
                                                                                
FTRXIT   XIT1                                                                   
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* VALIDATE 8 CHARACTER COMMERCIAL CODE                                          
* FIRST 4 CHARS MUST BE ALPHA, 5TH NUMERIC OR -, LAST 3 NUMERIC *               
* ON EXIT, 8 CHARACTER CMML WILL BE IN WORK!                                    
*====================================================================           
                                                                                
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK           CLEAR CMML OUTPUT                            
         MVI   ADIDFLAG,C'N'       ASSUME CMML IS ISCI                          
         CLI   5(R2),0                                                          
         BE    VCML2                                                            
         GOTO1 ANY                                                              
*                                                                               
VCML2    CLI   ACTNUM,ACTLIST                                                   
         BE    VCML60                                                           
*                                                                               
         CLI   5(R2),8             MUST BE AT LEAST 8 CHARS                     
         BH    VCML50              MORE IS ADID                                 
*                                                                               
         CLC   TRACML(8),=8C'9'    CML ID ALL 9'S (PROD HSE KEY)                
         BE    VCMLX                                                            
         CLI   ACTNUM,ACTADD       IF ADD FULLY EDIT CML                        
         BNE   VCML40                                                           
         CLI   5(R2),8             MUST BE LEN OF 8                             
         BNE   BADLENER            CK EXCEPTIONS                                
                                                                                
         LA    R0,8                                                             
         LA    R1,TRACML                                                        
VCML10   CLI   0(R1),C'A'                                                       
         BL    VCML20                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VCML10                                                        
         B     VCML40                                                           
*                                                                               
VCML20   CLI   SVT1PR10,C'A'       ALLOW ALL NON-STD CML CODES T1 PR 10         
         BE    VCML24                                                           
         CLI   SVT1PR10,C'Y'       ALLOW NON-STD COML CODES T1 PROF10           
         BNE   BADCMLER                                                         
         CLI   QMED,C'T'           NOT ALLOWED FOR TV                           
         BE    BADCMLER                                                         
                                                                                
VCML24   CLI   QMED,C'N'           NO NETWORK TV                                
         BNE   VCML26                                                           
         CLI   SPOTCAN,C'C'        UNLESS THIS IS CANADA                        
         BNE   BADCMLER                                                         
*                                                                               
VCML26   LA    R0,8                                                             
         LA    R1,TRACML                                                        
*                                                                               
VCML30   CLI   0(R1),C' '          LEADING BLANKS ALLOWED                       
         BNE   VCML34                                                           
         CLI   ACTNUM,ACTADD       IF ACTION ADD, NOT ALLOWED ANY MORE          
         BE    CMLBLKER                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VCML30                                                        
         B     CMLBLKER                                                         
                                                                                
VCML34   CLI   0(R1),C' '          EMBEDDED BLANK IS ERROR                      
         BE    CMLBLKER                                                         
         CLI   0(R1),C'-'                                                       
         BE    VCML36                                                           
         CLI   0(R1),C'A'                                                       
         BL    CMLBADER                                                         
         CLI   0(R1),C'Z'                                                       
         BNH   VCML36                                                           
         CLI   0(R1),C'0'                                                       
         BL    CMLBADER                                                         
         CLI   0(R1),C'9'                                                       
         BH    CMLBADER                                                         
VCML36   LA    R1,1(,R1)                                                        
         BCT   R0,VCML34                                                        
         EJECT                                                                  
*=============================================================                  
* IF T3PROF+6=Y, THEN ADD MEDIA N CMML WHEN ADDING T                            
* ONLY ALLOW MEDIA N FOR ADD IF MEDIA T CMML IS ALREADY THERE                   
*==============================================================                 
                                                                                
VCML40   CLI   SPOTCAN,C'C'                                                     
         BNE   VCMLX                                                            
         CLI   SVT3PROF+6,C'Y'     AUTO ADD MEDIA N CMML?                       
         BNE   VCMLX                                                            
*                                                                               
         CLI   TRAMED,C'N'         ADDING MEDIA N DIRECTLY?                     
         BNE   VCMLX                                                            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VCMLX                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         NI    KEY+2,X'F1'         CHANGE MEDIA TO T                            
         MVC   KEY+5(8),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IS MEDIA T CMML THERE?                       
         BE    VCMLX                                                            
         LA    R2,TRAMEDH                                                       
         LHI   R0,NOTVCMML                                                      
         J     TRAPERR2                                                         
*                                                                               
VCMLX    XIT1                                                                   
         EJECT                                                                  
*=================================================================              
* SEE IF THIS IS AN AD-ID CODE                                                  
*=================================================================              
                                                                                
VCML50   DS    0H                                                               
         CLI   5(R2),12            ADID MUST BE 9-12                            
         JH    BADLENER                                                         
                                                                                
         MVI   ERROR,INVALID                                                    
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         BNE   VCMLERR                                                          
         MVC   WORK(8),WORK+12                                                  
                                                                                
         MVI   ADIDFLAG,C'Y'       SET CMML IS ADID                             
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BE    VCML40                                                           
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCML52                                                           
         MVI   ERROR,NOTFOUND                                                   
         J     TRAPERR                                                          
*                                                                               
VCML52   LA    R0,TRACMLH                                                       
         CR    R0,R2               ARE WE DOING CMML IN KEY                     
         BNE   VCML40                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
                                                                                
         GOTO1 GETREC                                                           
*                                                                               
         XC    TRACML,TRACML                                                    
         MVC   TRACML(8),5(R6)     MOVE CMML FROM REC TO SCREEN                 
         MVC   WORK,TRACML                                                      
         OI    TRACMLH+6,X'80'     FORCE TRANSMIT                               
                                                                                
         MVI   ADIDFLAG,C'N'       ASSUME CMML IS ISCI                          
         TM    15(R6),X'01'                                                     
         BZ    VCML40              IF NOT PACKED, IT IS ISCI                    
         MVI   ADIDFLAG,C'Y'                                                    
         GOTO1 VTRPACK,DMCB,(C'U',5(R6)),TRACML                                 
         B     VCML40                                                           
                                                                                
*===========================================================                    
* ACTION IS LIST                                                                
* TO GET A VALID STARTING POINT FOR READ HIGH,                                  
* REPLACE C' ' WITH C'A'. SPACE FILLED FIELD IS IN WORK.                        
*===========================================================                    
                                                                                
VCML60   CLI   5(R2),12                                                         
         JH    BADLENER                                                         
         MVI   ADIDFLAG,C'A'       IF NO INPUT, SET DOING ADIDS                 
         CLI   5(R2),0             TEST NO INPUT                                
         BE    VCML40                                                           
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
         BE    VCML40                                                           
         MVI   ADIDFLAG,C'I'                      SET DOING ISCI                
         B     VCML40                                                           
*                                                                               
VCMLERR  MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
                                                                                
CMLBADER LHI   R0,CMLBADMS                                                      
         J     TRAPERR2                                                         
                                                                                
BADCMLER LHI   R0,BADCML                                                        
         J     TRAPERR2                                                         
                                                                                
CMLBLKER LHI   R0,CMLNOBLK  BLANKS NOT ALLOWED IN CML CODE                      
         J     TRAPERR2                                                         
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* FORMAT OFFLINE REPORT HERE                                                    
*====================================================================           
                                                                                
LRR      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING CMLKEY,R4                                                        
                                                                                
         LA    R5,P                PRINT LINE ADDRESS                           
         USING PRTLINE,R5                                                       
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    LRR10               YES                                          
                                                                                
         BRAS  RE,FCLT             GO GET CLIENT CLIST                          
         BNE   LRRXIT                                                           
                                                                                
         MVI   FORCEHED,C'Y'                                                    
LRR10    DS   0H                                                                
*                                                                               
         MVC   PCML(L'CMLKCML),CMLKCML                                          
         CLI   KEY+1,X'C1'        TEST READING ADID'S                           
         BNE   LRR11                                                            
         LA    R0,CMLKCML-CMLKEY+KEY     PACKED ADID IS IN KEY                  
         GOTO1 VTRPACK,DMCB,(C'U',(R0)),PCML                                    
*                                                                               
         CLC   CMLKCML-CMLKEY+KEY(8),CMLKCML   KEY/REC CMML MATCH               
         BE    LRR11                                                            
*                                                                               
         LA    R1,PCML+L'PCML-1    SET FLAG TO SHOW IT IS ISCI                  
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'!'                                                       
         DROP  R4                                                               
*                                                                               
LRR11    CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   LRR16               NO                                           
         MVI   ELCODE,C'1'         CK ACTIVITY ELEMENT                          
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LRR16                                                            
         GOTO1 DATCON,DMCB,(5,0),(3,WORK) GET TODAY'S DATE                      
         USING ACTVD,R6                                                         
         CLC   ACTVADDT,WORK       WAS ADD TODAY                                
         BNE   LRR12               NO, CK CHANGE                                
         MVC   PCML+8+132(3),=C'ADD'                                            
         B     LRR14                                                            
*                                                                               
LRR12    CLC   ACTVCHDT,WORK       WAS CHANGE TODAY                             
         BNE   LRR16                                                            
         MVC   PCML+8+132(3),=C'CHG'                                            
*                                                                               
LRR14    MVI   PCML-1,C'*'                                                      
*                                                                               
LRR16    MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    FTRFLAG,SEQFTR      DO THEY WANT COMML SEQ NO                    
         BZ    LRR18                                                            
                                                                                
         EDIT  CMLSEQ,(7,PCML+1+132)                                            
                                                                                
LRR18    EDIT  (1,CMLSLN),(3,PSLN),ZERO=BLANK                                   
                                                                                
         OC    CMLOVRD1(2),CMLOVRD1  ANY PRINT OVERRIDE?                        
         BZ    LRR19                  NO                                        
         EDIT  (1,CMLOVRD1),(3,PSLN),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R1,PSLN                                                          
         AR    R1,R0               NEXT BLANK SPACE                             
         MVI   0(R1),C'/'                                                       
         EDIT  (1,CMLOVRD2),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                   
                                                                                
LRR19    TM    FTRFLAG,HDEFFTR     SHOW HDEF CMML INSTEAD OF TTL                
         BO    *+10                                                             
         MVC   PTITLE,CMLTITLE                                                  
*                                                                               
         CLI   CMLSOLO,0           ANY ENTRY                                    
         BE    LRR20               NO                                           
         MVC   PSOLO,CMLSOLO                                                    
                                                                                
LRR20    GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,PRELSE)                               
         CLC   CMLRCL,=X'FFFFFF'                                                
         BNE   LRR21                                                            
         MVC   PRECALL+3(3),=CL3'UFN'                                           
         B     LRR22                                                            
LRR21    GOTO1 (RF),(R1),(3,CMLRCL),(5,PRECALL)                                 
                                                                                
LRR22    MVC   PTYPE,CMLTYPE                                                    
                                                                                
* PRINT ANY AD-ID NUMBER BELOW CMML                                             
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR23                                                            
*                                                                               
         TM    KEY+13,X'01'        IS CML PACKED IN KEY                         
         BO    LRR23               YES - SO ADID IS ALREADY DISPLAYED           
         CLI   KEY+1,X'C1'         TEST READING ADID KEYS                       
         BE    LRR23                                                            
*                                                                               
         LA    R2,PCML+132                                                      
         MVC   0(5,R2),=C'ADID='                                                
         MVC   5(12,R2),2(R6)                                                   
                                                                                
* PRINT ANY EXTRA DESCRIPTION LINES                                             
                                                                                
LRR23    L     R6,AIO                                                           
*                                                                               
         LA    R2,PTITLE+132                                                    
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR24                                                            
         MVC   0(20,R2),3(R6)                                                   
         LA    R2,132(R2)                                                       
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   LRR24                                                            
         MVC   0(20,R2),3(R6)                                                   
         LA    R2,132(R2)                                                       
                                                                                
* PRINT ANY TELECASTER NUMBER                                                   
                                                                                
LRR24    L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRR25                                                            
         MVC   0(4,R2),=C'TEL='                                                 
         MVC   4(8,R2),2(R6)                                                    
         LA    R2,14(R2)                                                        
*                                                                               
LRR25    L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLPRDEL,R6                                                      
         MVC   SVKEY,KEY                                                        
         CLI   CMLPRDS,X'FF'       IS THIS PRD=ALL                              
         BNE   LRR26               NO                                           
         MVC   PLIST(7),=CL7'PRD=ALL'                                           
         MVI   PRDCTR,0                                                         
         B     LRR28                                                            
*                                                                               
LRR26    ZIC   R3,CMLPRDLN         GET PROD LIST ELEM LEN                       
         BCTR  R3,0                                                             
         BCTR  R3,0                NOW # PRODS IN LIST                          
         LA    R4,CMLPRDS          POINT TO START BIN PROD LIST                 
         MVC   AIO,AIO2                                                         
         STC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
         BAS   RE,PPRDS                                                         
*                                                                               
         LA    R5,P1               RESTORE R5 TO P1                             
LRR28    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       SOFT DELETE                                  
         BZ    LRR30               NO                                           
         MVC   PTITLE+132(9),ASTDEL                                             
         DROP  R6                                                               
*                                                                               
LRR30    LR    R7,R6               SAVE R6                                      
         MVI   ELCODE,X'24'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LRR32                                                            
         USING CMLXDTEL,R6                                                      
         OC    CMLXHDEF,CMLXHDEF                                                
         BZ    LRR32                                                            
         MVC   PMISC(6),=C'HIDEF='                                              
         MVC   PMISC+6(12),CMLXHDEF                                             
         LA    R5,132(R5)                                                       
         DROP  R6                                                               
*                                                                               
LRR32    LR    R6,R7                                                            
         USING CMLDTAEL,R6                                                      
*                                                                               
         OC    CMLPROD,CMLPROD     PRODUCTION HOUSE?                            
         BZ    LRR40               NO                                           
         MVC   PMISC(18),=CL18'PRODUCTION HOUSE ='                              
         MVC   PMISC+19(L'CMLPROD),CMLPROD                                      
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
*                                                                               
LRR40    OC    CMLCLTNO,CMLCLTNO                                                
         BZ    LRR50                                                            
         MVC   PMISC(7),=CL7'CLIENT='                                           
         MVC   PMISC+7(L'CMLCLTNO),CMLCLTNO                                     
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
*                                                                               
LRR50    OC    CMLCOST,CMLCOST     IS THERE A COST?                             
         BZ    LRR60               NO                                           
         MVC   PMISC(10),=CL10'$/COMM = $'                                      
         EDIT  (B4,CMLCOST),(8,PMISC+10),ALIGN=LEFT,COMMAS=YES,        C        
               MINUS=YES                                                        
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
*                                                                               
LRR60    TM    CMLSTAT,X'40'       COMMERCIAL TEXT REC                          
         BZ    LRR64                NO                                          
         MVC   PMISC(31),=CL31'TEXT RECORD FOR THIS COMMERCIAL'                 
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
         LA    R1,P4               LAST LINE                                    
         CR    R1,R5               PAST LAST LINE                               
         BNL   LRR64                NO                                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR64                YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
                                                                                
LRR64    TM    CMLSTAT,X'20'       EASI COMMERCIAL?                             
         BZ    LRR66                NO                                          
         MVC   PMISC(24),=CL24'COMMERCIAL ADDED BY EASI'                        
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
         LA    R1,P4               LAST LINE                                    
         CR    R1,R5               PAST LAST LINE                               
         BNL   LRR70                NO                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR66                YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
                                                                                
LRR66    OC    CMLCLASS,CMLCLASS   ANY COMM CLASS                               
         BZ    LRR70                                                            
         MVC   PMISC(6),=CL6'CLASS='                                            
         MVC   PMISC+6(4),CMLCLASS                                              
         LA    R5,132(,R5)         POINT TO NEXT PRINT LINE                     
         LA    R1,P4               LAST LINE                                    
         CR    R1,R5               PAST LAST LINE                               
         BNL   LRR70               NO                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR70                YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
                                                                                
LRR70    MVI   ELCODE,X'21'                                                     
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BNE   LRR80                                                            
*                                                                               
         USING CMLCLSEL,R6                                                      
LRR72    MVC   PMISC(6),=CL6'CLASS='                                            
         MVC   PMISC+6(4),CMLCLS                                                
*                                                                               
         LA    R4,PMISC+10                                                      
         LA    RE,CMLCLS+3                                                      
*                                                                               
LRR74    CLI   0(RE),C' '                                                       
         BH    LRR76                                                            
         BCTR  R4,0                                                             
         BCT   RE,LRR74                                                         
*                                                                               
LRR76    MVI   0(R4),C'='                                                       
         EDIT  (B2,CMLCLSPC),(6,1(R4)),2,ALIGN=LEFT                             
         CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR77                YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
         B     LRR78                                                            
                                                                                
LRR77    GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
LRR78    LA    R5,P1                                                            
         BRAS  RE,NEXTEL                                                        
         BE    LRR72                                                            
                                                                                
LRR80    CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR90                YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
         B     LRR80                                                            
LRR90    MVC   AIO,AIO1                                                         
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
LRRXIT   XIT1                                                                   
                                                                                
* PRINT PROD LIST OF PROD/NAMES                                                 
                                                                                
         USING PRTLINE,R5                                                       
         DS    0H                                                               
PPRDS    NTR1                                                                   
         ZIC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         L     R4,PRDPTR           SAVE PROD LIST PTR                           
PPRDS10  LA    R1,P4                                                            
         CR    R1,R5                                                            
         BL    PPRDS30                                                          
         L     R6,ASVCLIST                                                      
PPRDS14  CLI   0(R6),C' '                                                       
         BNH   PPRDS20             UNKNOWN PRODUCT                              
         CLC   0(1,R4),3(R6)                                                    
         BE    PPRDS16                                                          
         LA    R6,4(,R6)                                                        
         B     PPRDS14                                                          
PPRDS16  MVC   PLIST(3),0(R6)       STORE IN WORK AREA                          
                                                                                
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT SYSTEM                      
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),0(R6)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME+3(3),=CL3'FIL' SWITCH TO SPOT SYSTEM                    
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   PLIST+4(20),PNAME                                                
                                                                                
         XC    FILENAME,FILENAME     SWITCH TO SPOT SYSTEM                      
                                                                                
         B     PPRDS24                                                          
                                                                                
PPRDS20  MVC   PLIST(3),=C'***'    SHOW UNKNOWN PRODUCT                         
         MVC   PLIST+4(20),=CL20'INVALID PRD-CALL DDS'                          
         DROP  R1                                                               
PPRDS24  LA    R5,132(,R5)                                                      
         LA    R4,1(,R4)           POINT TO NEXT IN PROD LIST                   
         BCT   R3,PPRDS10                                                       
PPRDS30  STC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
         B     LRRXIT                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE ACTUAL ID AND IT'S SLN TO ACTSLN                                     
*  R2 = A(FLD HDR)                                                              
                                                                                
VID      NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),8             MUST BE AT LEAST 8 CHARS                     
         BL    BADLENER                                                         
         BRAS  RE,VCML                                                          
                                                                                
* CMML CAN'T COVER ITSELF                                                       
                                                                                
         L     RF,AIO                                                           
         CLC   CMLKCML-CMLKEY(8,RF),WORK                                        
         BNE   *+12                                                             
         LHI   R0,SAMECMML                                                      
         J     TRAPERR2                                                         
                                                                                
* NOW READ REC                                                                  
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'       VCML SETS THIS FLAG                          
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,WORK        CMML FROM VCML                               
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOCOMM                                                     
         J     TRAPERR                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
* CHECK IF COV CMML                                                             
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+12                                                             
         LHI   R0,CMLISCOV                                                      
         J     TRAPERR2                                                         
                                                                                
* CHECK IF CMML IS DELETED                                                      
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'                                                    
         BZ    *+12                                                             
         LHI   R0,CMLISDEL                                                      
         J     TRAPERR2                                                         
                                                                                
* CHECK IF CMML COVERS DATE SPREAD                                              
                                                                                
         LR    R4,R6               SAVE PTR TO ACT X'10'                        
         L     R6,AIO1             GET X'10' FROM COV                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* CHECK IF CMML HAS SAME TYPE AS COVER                                          
                                                                                
VID10    DS    0H                                                               
         CLC   CMLTYPE,CMLTYPE-CMLDTAEL(R4)                                     
         BE    *+12                                                             
         LHI   R0,TYPEERR                                                       
         J     TRAPERR2                                                         
         LR    R6,R4                                                            
                                                                                
* ADD SPOT LENGTH TO ACCUM                                                      
                                                                                
         ZIC   R1,ACTSLN                                                        
         ZIC   RF,CMLSLN                                                        
         AR    R1,RF                                                            
         STC   R1,ACTSLN                                                        
         DROP  R6                                                               
                                                                                
* CHECK IF CMML HAS PROD IN COV CMML                                            
                                                                                
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
         LHI   R0,NOPRDHIT                                                      
         J     TRAPERR2                                                         
                                                                                
BADLENER LHI   R0,NOT812                                                        
         J     TRAPERR2                                                         
                                                                                
HIT      DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
         LTORG                                                                  
         DROP  R7,RB                                                            
         EJECT                                                                  
* UPDATE ACTUAL CMML *                                                          
* EXPECTS:                                                                      
*  R0 = H'1' FOR INC, H'-1' FOR DEC                                             
*  R1 = A(CMML ID)                                                              
                                                                                
UPDACT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'       TEST TO READ ADID                            
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM(3),BAGYMD    A-M/CLT                                      
         MVC   CMLKCML,0(R1)                                                    
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
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
         SR    RE,RE                                                            
         ICM   RE,3,CMLCOVCT                                                    
         AR    RE,R0                                                            
         STCM  RE,3,CMLCOVCT                                                    
                                                                                
* DO NOT LET GENCON UPDATE ACTIVITY (X'F1') ELEM - SCREWS UP ELCODE             
                                                                                
         MVI   ACTELOPT,C'N'                                                    
         GOTO1 PUTREC                                                           
         MVI   ACTELOPT,C'Y'                                                    
         MVC   AIO,AIO1                                                         
UAXIT    XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*====================================================================           
* UPDATE CMMLS IF NEEDED - CHANGE WAS OTHER THAN DELETE OR RESTORE              
*====================================================================           
                                                                                
NPUT     NTR1  BASE=*,LABEL=*                                                   
                                                                                
* FOR ANY ENTRY THAT IS IN OLDACTS AND NOT IN NEWACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'-1'                                                  
*                                                                               
         LHI   R0,-1               SET FLAG FOR UPDACT                          
         LA    R2,NUMACTS                                                       
         LA    R3,OLDACTS                                                       
         OC    0(12,R3),0(R3)                                                   
         BZ    NP20                                                             
*                                                                               
NP01     LA    R4,NUMACTS                                                       
         LA    R5,NEWACTS                                                       
*                                                                               
NP02     CLC   0(12,R3),0(R5)                                                   
         BE    NP10                                                             
         LA    R5,12(R5)                                                        
         BCT   R4,NP02                                                          
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         LR    R1,R3               POINT TO CMML                                
         CLI   8(R3),C' '          TEST ISCI                                    
         BNH   NP05                MUST BE 8 CHAR ISCII                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
                                                                                
NP05     BRAS  RE,UPDACT                                                        
                                                                                
NP10     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         BZ    NP20                                                             
         BCT   R2,NP01                                                          
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
NP21     LA    R4,NUMACTS                                                       
         LA    R5,OLDACTS                                                       
*                                                                               
NP24     CLC   0(12,R3),0(R5)                                                   
         BE    NP30                                                             
         LA    R5,12(R5)                                                        
         BCT   R4,NP24                                                          
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         LR    R1,R3               POINT TO CMML                                
         CLI   8(R3),C' '          TEST ISCI                                    
         BNH   NP26                MUST BE 8 CHAR ISCII                         
                                                                                
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
                                                                                
NP26     BRAS  RE,UPDACT                                                        
                                                                                
NP30     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         BZ    NPX                                                              
         BCT   R2,NP21                                                          
NPX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - DELETED, DATE, PROD, TYPE, LEN, REL DATE *         
                                                                                
VFTR     NTR1  BASE=*,LABEL=*                                                   
         XC    FILTERS,FILTERS                                                  
         XC    ADIDFLT,ADIDFLT     FILTER ON SPECIFIC ADID                      
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
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
         MVC   CONHEAD(FTRHELPL),FTRHELP                                        
         GOTO1 ERREX2                                                           
VFTR08   GOTO1 SCANNER,DMCB,(12,TRAFLTRH),(5,BLOCK)                             
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    INVALER             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         REL (RELEASE DATE)                           
         BNE   VFTR16                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    DATERRF             NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,RLDTFTR)                                 
         MVC   RLDTSFTR,HOLDSIGN                                                
         B     VFTR80                                                           
                                                                                
VFTR16   EX    R1,VFTRCLCJ         RCL (RECALL DATE)                            
         BNE   VFTR20                                                           
                                                                                
         CLI   1(R4),3             COULD THIS BE UFN                            
         BH    VFTR18                                                           
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,VFTRCLCU                                                      
         BNE   VFTR18                                                           
         MVC   RCDTFTR,=X'FFFFFF'                                               
         MVI   RCDTSFTR,0          NO GREATER THAN/LESS THAN ALLOWED            
         B     VFTR80                                                           
                                                                                
VFTR18   LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    DATERRF             NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,RCDTFTR)                                 
         MVC   RCDTSFTR,HOLDSIGN                                                
         B     VFTR80                                                           
                                                                                
VFTR20   EX    R1,VFTRCLCB         PRD (PRODUCT)                                
         BE    VFTR22                                                           
         EX    R1,VFTRCLCC         PROD (PRODUCT)                               
         BNE   VFTR30                                                           
VFTR22   OC    BCLT,BCLT           CLIENT MUST HAVE BEEN ENTERED                
         BZ    MISSCLT                                                          
*        CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
*        BE    VFTR28              YES, SAVE IT                                 
         LA    R0,220              MAX COUNT BUG CATCHER                        
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
VFTR24   CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   PRDERR               YES, ERROR                                  
         CLC   0(3,R1),22(R4)      THIS A VALID PROD CODE                       
         BE    VFTR26                                                           
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,VFTR24                                                        
         DC    H'0'                                                             
VFTR26   MVC   PRODFTR(4),0(R1)    PROD AND PRD                                 
         B     VFTR80                                                           
*FTR28   MVC   PRODFTR(3),=C'ALL'  MOVE IN ALL, LEAVE PRD ZERO                  
*        B     VFTR80                                                           
VFTR30   EX    R1,VFTRCLCD         TYPE                                         
         BNE   VFTR40                                                           
         MVC   WORK(12),22(R4)                                                  
         MVI   BYTE,1              BYPASS GOTO1 ANY                             
         BRAS  RE,VTYP                                                          
         MVC   TYPEFTR,WORK                                                     
         B     VFTR80                                                           
VFTR40   EX    R1,VFTRCLCE         SPOT LEN                                     
         BNE   VFTR50                                                           
         TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERRF                                                          
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      SPOT LEN                                     
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRF            GO PRINT ERROR                               
         MVC   SLNFTR,WORK                                                      
         B     VFTR80                                                           
                                                                                
VFTR50   EX    R1,VFTRCLCF         CLASS=                                       
         BNE   VFTR60                                                           
         MVC   CLSFTR,22(R4)                                                    
         LA    R1,CLSFTR+3                                                      
VFTR54   CLI   0(R1),C' '                                                       
         BNE   VFTR80                                                           
         MVI   0(R1),0                                                          
         BCT   R1,VFTR54                                                        
         DC    H'0'                                                             
                                                                                
VFTR60   EX    R1,VFTRCLCG         DELETED RECS                                 
         BNE   VFTR61                                                           
         OI    FTRFLAG,DELFTR                                                   
         B     VFTR80                                                           
                                                                                
VFTR61   EX    R1,VFTRCLCV         ADID ONLY                                    
         BE    *+12                                                             
         EX    R1,VFTRCLCW         AD-ID ONLY                                   
         BNE   VFTR62                                                           
         OI    FTRFLAG,ADIDFTR                                                  
         SPACE                                                                  
         CLI   1(R4),0             FILTER ON SPECIFIC ADID                      
         BE    VFTR80              NO                                           
         SPACE                                                                  
         CLI   1(R4),9             ADID = 12 ALPHA NUMERICS                     
         BL    BDADID              BAD ADID                                     
         SPACE                                                                  
         MVC   ADIDFLT,22(R4)      ADID FILTER                                  
         OC    ADIDFLT,SPACES                                                   
         B     VFTR80                                                           
*                                                                               
VFTR62   EX    R1,VFTRCLCS         SHOW COMML SEQ NO.                           
         BNE   VFTR64                                                           
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL?                         
         BNE   VFTR90                                                           
                                                                                
         OI    FTRFLAG,SEQFTR                                                   
         B     VFTR80                                                           
                                                                                
VFTR64   EX    R1,VFTRCLCI         EASI RECS                                    
         BNE   VFTR66                                                           
         OI    FTRFLAG,EASIFTR                                                  
         B     VFTR80                                                           
                                                                                
VFTR66   EX    R1,VFTRCLCT         TELECASTER                                   
         BNE   VFTR70                                                           
         OI    FTRFLAG,TELEFTR                                                  
         B     VFTR80                                                           
*                                                                               
VFTR70   DS    0H                                                               
         EX    R1,VFTRHDEF                                                      
         BE    *+12                                                             
         EX    R1,VFTRHIDE         TEST FOR HIDEF                               
         BNE   VFTR90                                                           
         OI    FTRFLAG,HDEFFTR                                                  
*                                                                               
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<                                      
*  NOP STARCOM ONLY FEATURE !!!                                                 
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<                                      
*NOP     EX    R1,VFTRCLCH         FILE - CREATE FILE FOR STARCOM               
*        BNE   VFTR90                                                           
*        TM    WHEN,X'A0'          MUST BE RUN NOW, OV, OR DDS                  
*        BNZ   IMMEDERR             NOT IMMED                                   
*                                                                               
*        CLI   OFFLINE,C'Y'        THIS OFFLINE?                                
*        BE    VFTR72               VALIDATED ONLINE, MUST BE OKAY              
*                                                                               
*        CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL?                         
*        BE    VFTR72                                                           
*        CLC   =C'MC',AGENCY       AGENCY IS GM MCCANN                          
*        BE    VFTR72                                                           
*        CLC   =C'H9',AGENCY       STARCOM ?                                    
*        BNE   VFTR80                                                           
*                                                                               
*FTR72   DS   0H                                                                
*        OI    FTRFLAG,FILFTR                                                   
*        MVI   PQSW,1              SUPPRESS AUTO PRTQUE OPEN                    
*        ZAP   CMLFILCT,=P'0'                                                   
******** ZAP   CMLPBCT,=P'0'                                                    
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<                                      
*FTR80   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
VFTR80   LA    R4,34(R4)           POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    XIT1                                                                   
                                                                                
VFTR90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(FTRMSGL),FTRMSG                                          
         GOTO1 ERREX2                                                           
                                                                                
IMMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG),FTRMSG                                         
         MVC   CONHEAD+L'FTRMSG+1(L'IMMEDMS),IMMEDMS                            
         GOTO1 ERREX2                                                           
*                                                                               
BDADID   DS    0H                                                               
         LHI   R0,ADID912    ADID MUST BE 9-12 ALPHA/NUM                        
         J     TRAPERR2                                                         
*                                                                               
INVALER  MVI   ERROR,INVALID                                                    
         J     TRAPERRF                                                         
NUMERRF  MVI   ERROR,NOTNUM                                                     
         J     TRAPERRF                                                         
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         J     TRAPERRF                                                         
DATERRF  MVI   ERROR,INVDATE                                                    
         J     TRAPERRF                                                         
MISSCLT  LA    R2,TRACLTH                                                       
MISSERRF MVI   ERROR,MISSING                                                    
                                                                                
TRAPERRF GOTO1 ERREX                                                            
VFTRCLCA CLC   12(0,R4),=CL4'REL '  RELEASE DATE FILTER                         
VFTRCLCJ CLC   12(0,R4),=CL4'RCL '  RECALL DATE FILTER                          
VFTRCLCB CLC   12(0,R4),=CL7'PRD=ALL'                                           
VFTRCLCC CLC   12(0,R4),=CL4'PROD'                                              
VFTRCLCD CLC   12(0,R4),=CL4'TYPE'                                              
VFTRCLCE CLC   12(0,R4),=CL3'LEN'                                               
VFTRCLCF CLC   12(0,R4),=CL3'CLS'                                               
VFTRCLCG CLC   12(0,R4),=C'DELETED'                                             
*VFTRCLCH CLC   12(0,R4),=C'FILE '                                              
VFTRCLCI CLC   12(0,R4),=CL5'EASI '                                             
VFTRCLCS CLC   12(0,R4),=CL4'SEQ '                                              
VFTRCLCT CLC   12(0,R4),=CL12'TELECASTER '                                      
VFTRCLCU CLC   22(0,R4),=C'UFN'                                                 
VFTRCLCV CLC   12(0,R4),=C'ADID'                                                
VFTRCLCW CLC   12(0,R4),=C'AD-ID'                                               
VFTRHIDE CLC   12(0,R4),=C'HIDEF'                                               
VFTRHDEF CLC   12(0,R4),=C'HDEF'                                                
FTRMSG   DC    C'* ERROR * '                                                    
FTRHELP  DC    C'VALID FILTERS=DEL/PRD/TYP/LEN/CLS/REL/HD/RCL='                 
FTRHELPL EQU   *-FTRHELP                                                        
FTRMSGL  EQU   *-FTRMSG                                                         
IMMEDMS  DC    C'FILE OPT MUST BE RUN NOW, OR OVERNIGHT *'                      
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
* VALIDATE COMMERCIAL TYPE                                                      
*                                                                               
VTYP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BYTE,0              NEED TO BYPASS GOTO1 ANY                     
         BNE   VTYP04               YES                                         
         GOTO1 ANY                                                              
         CLI   WORK,C'?'                                                        
         BNE   VTYP04                                                           
                                                                                
* LIST VALID TYPES ON 2 BOTTOM SCREEN LINES                                     
                                                                                
         OI    TRATYPLH+6,X'80'    SET XMIT                                     
         LA    R1,TRATYPL                                                       
         LA    RF,CTYPTABL                                                      
VTYP01   LA    R0,19               MAX 3 CHAR ENTRIES PER LINE                  
                                                                                
VTYP02   DS    0H                                                               
         CLI   0(RF),X'00'         END OF TABLE?                                
         BE    VTYPHX                                                           
         MVC   0(3,R1),0(RF)                                                    
         LA    R1,4(,R1)           NEXT SCREEN POSN                             
         LA    RF,3(,RF)           NEXT VALID TYPE                              
         BCT   R0,VTYP02                                                        
                                                                                
         LA    R1,TRATYPLH         GET TO SECOND HELP LINE                      
         ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         OI    6(R1),X'80'         SET XMIT                                     
         LA    R1,8(R1)                                                         
         OC    0(74,R1),0(R1)      ALREADY BEEN HERE?                           
         BNZ   VTYPHX              GET OUT                                      
         B     VTYP01                                                           
                                                                                
VTYPHX   MVI   GMSGTYPE,C'I'                                                    
         LA    R2,TRATYPEH                                                      
         LHI   R0,VTYPDIS                                                       
         J     TRAPERR2                                                         
                                                                                
VTYP04   CLI   WORK+3,C' '                                                      
         BH    VTYPER                                                           
         LA    R0,CTYPTBCT                                                      
         LA    R1,CTYPTABL                                                      
VTYP10   CLC   WORK(3),0(R1)                                                    
         BE    VTYPX                                                            
         LA    R1,3(,R1)                                                        
         BCT   R0,VTYP10                                                        
                                                                                
VTYPER   MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         GOTO1 ERREX                                                            
VTYPX    XIT1                                                                   
       ++INCLUDE SPTRCMLTYP                                                     
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PRODUCT LIST AND BUILD PROD LIST ELEMENT                             
                                                                                
VPRDL    NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRV             NO                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING CMLPRDEL,R6                                                      
         MVI   CMLPRDEL,X'20'      ELEM CODE                                    
         GOTO1 ANY                                                              
         CLC   =CL7'PRD=ALL',WORK                                               
         BNE   VPRDL06                                                          
                                                                                
         CLI   SVT2PR8,C'Y'        LIMIT ONE PRD PER ISCII                      
         BE    PRDSERR              YES ERROR                                   
                                                                                
         MVI   CMLPRDLN,3                                                       
         MVI   CMLPRDS,X'FF'                                                    
         B     VPRDL20                                                          
                                                                                
VPRDL06  MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         L     RE,AIO1             SAVE RECORD IN AIO3                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
*                                                                               
         CLI   ACTNUM,ACTADD       FOR ADDS                                     
         BNE   *+8                                                              
         LA    RF,1024             USE 1K AS LENGTH                             
*                                                                               
         L     R0,AIO3                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
VPRDL08  MVI   CMLPRDLN,2          ELEM LENGTH EMPTY                            
         MVC   FLDH,TRAPLSTH       SAVE HEADER                                  
         MVC   FLD(L'TRAPLST),TRAPLST                                           
         GOTO1 SCANNER,DMCB,FLDH,(12,BLOCK+64),0                                
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3                                                            
         BZ    MISSERRV                                                         
                                                                                
         CLI   DMCB+4,1                                                         
         BE    *+12                                                             
         CLI   SVT2PR8,C'Y'        LIMIT ONE PRD PER ISCII                      
         BE    PRDSERR              YES ERROR                                   
                                                                                
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
         LA    R5,ELEM+2           SAVE PRODUCT CODE HERE                       
VPRDL10  XC    TRAPLST,TRAPLST                                                  
         ZIC   R1,0(R4)                                                         
         STC   R1,TRAPLSTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,VPRDLMVC                                                      
         GOTO1 VALIPRD                                                          
         CLC   12(3,R4),=C'POL'    THIS IS ILLEGAL                              
         BE    PRDINV                                                           
         CLI   SVPROF13,C'Y'       THIS CLT USE PROD EQUIV                      
         BNE   VPRDL12               NO                                         
                                                                                
* CHECK PRODUCT TO BE A BASE *                                                  
                                                                                
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PEQKEY,R1                                                        
         MVC   PEQKID,=X'0A37'                                                  
         MVC   PEQKAM(3),BAGYMD                                                 
         MVC   PEQKPRD,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A BASE PRODUCT                          
         BNE   MISEQVER             NO, ERROR                                   
         DROP  R1                                                               
                                                                                
VPRDL12  LA    RF,ELEM+2                                                        
                                                                                
VPRDL14  CR    R5,RF               THIS FIRST/CURR ENTRY                        
         BE    VPRDL16             YES, NO DUPES                                
         CLC   0(1,RF),WORK+3      THIS A DUPE                                  
         BE    DUPRDER                                                          
         LA    RF,1(,RF)                                                        
         B     VPRDL14                                                          
VPRDLMVC MVC   TRAPLST(0),12(R4)                                                
                                                                                
VPRDL16  MVC   0(1,R5),WORK+3      SAVE BINARY PRODUCT CODE                     
         LA    R4,32(,R4)          NEXT SCANNER BLOCK                           
         LA    R5,1(,R5)           BUMP ELEMENT POINTER                         
         IC    R1,ELEM+1           GET ELEM LENGTH                              
         LA    R1,1(,R1)                                                        
         STC   R1,ELEM+1                                                        
*                                                                               
         CLI   SVTTPR3,C'Y'        IF SPOTTAL USER,                             
         BNE   *+8                                                              
         BAS   RE,CKTAL            SEE IF PRODUCT HAS TALENT AGENCY             
*                                                                               
         BCT   R3,VPRDL10                                                       
                                                                                
         MVC   TRAPLSTH,FLDH       RESTORE INPUT                                
         MVC   TRAPLST,FLD                                                      
                                                                                
         CLI   CMLPRDLN,2          WERE ANY PROD CODES FOUND                    
         BE    MISSERRV            NO                                           
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
         L     RE,AIO3             COPY RECORD BACK TO AIO1                     
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
*                                                                               
         CLI   ACTNUM,ACTADD       FOR ADDS                                     
         BNE   *+8                                                              
         LA    RF,1024             USE 1K AS LENGTH                             
*                                                                               
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VPRDL20  CLI   ACTNUM,ACTADD                                                    
         BE    VPRDLX                                                           
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'        FIND ORIGINAL ELEMENT                        
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,ELEM                                                          
         SR    RF,RF                                                            
         IC    RF,ELEM+1                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),0(R6)       COMPARE NEW/OLD PRD ELEMS                    
         BE    *+8                                                              
         OI    CHGFLAG1,CMLCH_PRDL                                              
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
VPRDLX   XIT1                                                                   
                                                                                
* CHECK IF PRODUCT HAS A TALENT AGENCY *                                        
                                                                                
CKTAL    NTR1                                                                   
         MVC   MYKEY2,KEY                                                       
         MVC   FILENAME(6),=C'SPTDIR'  SWITCH TO SPOT SYSTEM                    
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
         MVC   FILENAME(6),=C'SPTFIL'  SWITCH TO SPOT SYSTEM                    
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         OC    PTALAGY,PTALAGY                                                  
         BZ    *+14                                                             
         CLC   PTALAGY,SPACES                                                   
         BNE   CKTAL10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+PKEYAM-PKEY(3),BAGYMD                                        
         MVC   KEY+PKEYPRD-PKEY(3),WORK                                         
         MVC   FILENAME(6),=C'SPTDIR'  SWITCH TO SPOT SYSTEM                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         MVC   FILENAME(6),=C'SPTFIL'  SWITCH TO SPOT SYSTEM                    
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         CLI   PFKEY,1                                                          
         BE    CKTAL10                                                          
*                                                                               
         OC    PTALAGY,PTALAGY                                                  
         BZ    NOTALER                                                          
         CLC   PTALAGY,SPACES                                                   
CKTAL10  XC    KEY,KEY                                                          
         MVC   KEY(L'MYKEY2),MYKEY2                                             
         MVC   AIO,AIO1                                                         
         B     VPRDLX                                                           
         DROP  R6                                                               
                                                                                
PRDSERR  LHI   R0,PRDSMSG                                                       
         J     TRAPERR2                                                         
                                                                                
MISSERRV MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
                                                                                
PRDINV   MVI   ERROR,INVPRDCD      POL INVALID PROD                             
         J     TRAPERR                                                          
                                                                                
MISEQVER LHI   R0,MISEQVMS                                                      
         J     TRAPERR2                                                         
                                                                                
DUPRDER  LA    RE,ELEM+2-1         MAKE ALL FLDS RELATIVE TO 1                  
         SR    R5,RE               CURRENT DUP                                  
         SR    RF,RE               PREV DUP                                     
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R1),3             L'GASUBST + 1                                
         EDIT  (RF),(2,1(R1))                                                   
         MVI   3(R1),3             L'GASUBST + 1                                
         EDIT  (R5),(2,4(R1))                                                   
         LHI   R0,DUPPRD                                                        
         J     TRAPERR2                                                         
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
                                                                                
NOTALER  MVC   TRAPLSTH,FLDH       RESTORE INPUT                                
         MVC   TRAPLST,FLD                                                      
         OI    TRAPLSTH+1,X'01'    SET MODIFIED                                 
         OI    TRAPLSTH+6,X'80'    TRANSMIT                                     
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         STCM  R1,7,GASUBST                                                     
         MVI   DUB,4               L'SUBST TEXT + 1                             
         MVC   DUB+1(3),KEY+4                                                   
         LHI   R0,NOTALMS                                                       
         J     TRAPERR2                                                         
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* VALIDATE COMMERCIAL CLASS                                                     
*======================================================================         
                                                                                
VCLASS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
*                                                                               
         LA    R4,BLOCK+200        USE AS SAVE AREA                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCLS4                                                            
         USING CMLCLSEL,R6                                                      
*                                                                               
VCLS2    MVC   0(6,R4),CMLCLS      MOVE CLASS AND PERCENTAGE                    
         LA    R4,6(R4)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    VCLS2                                                            
         DROP  R6                                                               
*                                                                               
VCLS4    GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,TRACLSH          CLASS                                        
         CLI   5(R2),0                                                          
         BNE   VCLS10                                                           
         CLI   SVPROF14,C'Y'       IS COMMERCIAL CLASS REQUIRED                 
         JE    MISSERR                                                          
         CLI   SVPROF14,C'V'       IS COMML CLASS REQUIRED & VALIDATED          
         JE    MISSERR                                                          
         B     VCLSX                                                            
*                                                                               
VCLS10   LA    R4,BLOCK                                                         
         XC    CMLPRCNT,CMLPRCNT                                                
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK),0                                    
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
         JZ    MISSERR             NONE                                         
         LR    R5,R3                                                            
*                                                                               
VCLS12   CLI   0(R4),4                                                          
         JH    CLASIZER                                                         
*                                                                               
         CLI   SVPROF14,C'V'       IS COMMERCIAL CLASS VALIDATED                
         BNE   VCLS14                                                           
*                                                                               
         BRAS  RE,RDCLS             GO READ FOR CLASS                           
*                                                                               
VCLS14   CLI   1(R4),0                                                          
         BNE   VCLS16                                                           
         CHI   R5,1                                                             
         BE    VCLS30                                                           
         J     MISSERR              NO                                          
*                                                                               
VCLS16   ZIC   R0,1(R4)                                                         
         GOTO1 CASHVAL,DMCB,(2,22(R4)),(R0),0                                   
         CLI   DMCB,X'FF'                                                       
         JE    PRCNTER                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'21'                                                       
         MVI   ELEM+1,8                                                         
         MVC   ELEM+2(4),12(R4)                                                 
         L     RE,DMCB+4                                                        
         STCM  RE,3,ELEM+6                                                      
         AH    RE,CMLPRCNT                                                      
         STH   RE,CMLPRCNT                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R1,BLOCK+200        SEE IF THIS DATA IN SAVE AREA                
         LA    R0,4                                                             
*                                                                               
VCLS20   CLC   ELEM+2(6),0(R1)                                                  
         BE    VCLS30                                                           
         LA    R1,6(R1)                                                         
         BCT   R0,VCLS20                                                        
*                                                                               
         OI    CHGFLAG1,CMLCH_CLS  IF NOT THERE BEFORE, SET CHANGED             
*                                                                               
VCLS30   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VCLS12           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         CLC   CMLPRCNT,=H'10000'                                               
         BE    VCLSX                                                            
         CHI   R5,1                                                             
         JH    PRCNTER                                                          
         OC    CMLPRCNT,CMLPRCNT                                                
         BNZ   VCLSX                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CMLCLASS-CMLDTAEL(4,R6),8(R2)                                    
*                                                                               
VCLSX    XIT1                                                                   
                                                                                
*============================================================                   
* SEE IF A CLASS RECORD EXISTS FOR SOME PRD OR THE CLIENT                       
*============================================================                   
                                                                                
RDCLS    NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LR    R3,R4                                                            
         LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,BAGYMD                                                    
         MVC   CLSKCLAS,12(R3)                                                  
         OC    CLSKCLAS,SPACES                                                  
         MVC   CLSKCLT,BCLT                                                     
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET PRODUCT ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'FF'         THIS ALL PRODS                               
         BE    RDCLS30                                                          
                                                                                
         ZIC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         LA    R5,2(,R6)                                                        
*                                                                               
RDCLS10  LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
*                                                                               
RDCLS20  CLC   0(1,R5),3(R1)                                                    
         BE    RDCLS24                                                          
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,RDCLS20                                                       
         DC    H'0'                                                             
*                                                                               
RDCLS24  MVC   CLSKPROD,0(R1)                                                   
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDCLS40                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R5,1(,R5)           POINT TO NEXT PRODUCT IN LIST                
         BCT   R3,RDCLS10                                                       
*                                                                               
         XC    CLSKPROD,CLSKPROD                                                
                                                                                
RDCLS30  GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDCLS40                                                          
                                                                                
         MVC   KEY,KEYSAVE                                                      
         XC    CLSKCLT,CLSKCLT                                                  
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVCLASS                                                         
                                                                                
RDCLS40  MVC   KEY,SVKEY                                                        
         CLI   ACTNUM,ACTADD       UNLESS ADD                                   
         BE    RDCLSX                                                           
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDCLSX                                                           
         DC    H'0'                                                             
*                                                                               
RDCLSX   XIT1                                                                   
*                                                                               
INVCLASS XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INRDCLSMS),INRDCLSMS                                   
         GOTO1 ERREX2                                                           
*                                                                               
INRDCLSMS DC   C'* ERROR * COMMERCIAL CLASS NOT FOUND *'                        
         LTORG                                                                  
         DROP  R4,RB                                                            
         EJECT                                                                  
*=============================================================                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*=============================================================                  
                                                                                
FCLT     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
                                                                                
FCLT10   CLC   =X'0AC1',KEY                                                     
         BE    *+14                                                             
         CLC   =X'0A21',KEY                                                     
         BNE   FCLTNE                                                           
         CLC   KEY+2(1),BAGYMD                                                  
         BNE   FCLTNE                                                           
*                                                                               
         OC    KEY+5(8),KEY+5      IF ALL BIN ZEROS                             
         BZ    FCLT16              THEN CMML SEQ # REC, BYPASS                  
         CLC   KEY+5(8),=8C'9'     IF ALL 9'S                                   
         BE    FCLT16              THEN PROD HSE REC, BYPASS                    
*                                                                               
         MVC   BCLT,CMLKCLT                                                     
         MVC   SVKEY,KEY                                                        
         DROP  R4                                                               
                                                                                
         GOTO1 CLUNPK,DMCB,BCLT,QCLT VALICLT BELOW WILL FIX AAN CLT             
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
                                                                                
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT14                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         B     FCLT10                                                           
                                                                                
FCLT14   MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
         B     FCLT18                                                           
*                                                                               
FCLT16   GOTO1 SEQ                                                              
         B     FCLT10                                                           
*                                                                               
FCLT18   CLC   KEY(3),KEYSAVE                                                   
         BNE   FCLTNE                                                           
*                                                                               
         CLI   ADIDFLAG,C'I'       TEST READING ISCI                            
         BNE   FCLT19              NO                                           
         TM    KEY+13,X'01'        TEST PACKED CMML                             
         BO    FCLT16              YES - IGNORE                                 
         B     FCLT20                                                           
*                                                                               
FCLT19   LA    R0,CMLKCML-CMLKEY+KEY    UNPACK ADID                             
         GOTO1 VTRPACK,DMCB,(C'U',(R0)),WORK                                    
         CLI   WORK+8,C' '              TEST ONLY 8 CHARS                       
         BNH   FCLT16                   YES - SKIP FOR NOW                      
                                                                                
FCLT20   DS    0H                                                               
         GOTO1 GETREC                                                           
                                                                                
         BRAS  RE,RPROFILE         GO READ PROFILE                              
                                                                                
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    FCLT40              BYPASS                                       
                                                                                
         MVC   AIO,AIO2                                                         
         CLC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
                                                                                
FCLT40   MVC   AIO,AIO1                                                         
                                                                                
         CR    RB,RB                                                            
         B     FCLTX                                                            
                                                                                
FCLTNE   LTR   RB,RB                                                            
FCLTX    XIT1                                                                   
         DROP  RB                                                               
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
                                                                                
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         GOTO1 CLUNPK,DMCB,(SVCPROF6,BCLT),QCLT                                 
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   HDHK10                 NO                                        
         MVC   H3+42(11),=C'TURN-AROUND'                                        
HDHK10   DS    0H                                                               
                                                                                
* PRINT OUT ANY FILTERS                                                         
                                                                                
         OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    HDHKX               NO                                           
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
                                                                                
         CLC   RCDTFTR,=X'FFFFFF'                                               
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
         BZ    HDHK44                                                           
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
HDHK44   OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    HDHK50                                                           
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
HDHK50   OC    SLNFTR,SLNFTR       SPOT LENGTH FILTER                           
         BZ    HDHK60                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'L'                                                       
         MVI   1(R3),C'='                                                       
         LA    R3,2(,R3)                                                        
         EDIT  (B1,SLNFTR),(3,(R3)),ALIGN=LEFT                                  
         CLI   2(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,3(,R3)                                                        
HDHK60   OC    CLSFTR,CLSFTR       CLASS FILTER                                 
         BZ    HDHK70                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'R'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(4,R3),CLSFTR                                                   
                                                                                
HDHK70   MVC   H5+34(34),FLD                                                    
HDHKX    XIT1                                                                   
         DROP  RB                                                               
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
                                                                                
GENR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVPROF10,C'Y'       AUTO TURNAROUND                              
         BE    GENR10              YES                                          
         CLI   SVPROF10,C'D'       AUTO TURNAROUND                              
         BNE   GENRX                NO                                          
GENR10   XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),AGENCY                                              
         MVC   REQUEST+4(23),=CL23'*.COM.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(1),QMED                                               
         MVI   REQUEST+28,C'.'                                                  
         MVC   REQUEST+29(3),QCLT                                               
         MVI   REQUEST+32,C'.'                                                  
         MVC   REQUEST+33(4),HOLDCML                                            
         MVC   REQUEST+37(2),=C'.*'                                             
         CLI   SVPROF10,C'D'       IF REQ IS DDS                                
         BE    *+10                BYPASS                                       
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
GENRX    XIT1                                                                   
         DROP  RB                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* GET NEXT SEQUENCE NUMBER FROM MASTER RECORD FOR ADDS                          
*===============================================================                
                                                                                
PAREC    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF CMML SEQ REC FOUND                        
         BE    PAR10               GO SAVE CMML SEQ NUMBER                      
         DROP  R4                                                               
                                                                                
* NOW MUST ADD CMML SEQ REC FOR AGENCY/MEDIA/CLT-ONCE FOR EACH A/M/CLT          
                                                                                
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         L     R6,AIO                                                           
         USING CMLKEY,R6                                                        
         XC    CMLRECD(256),CMLRECD                                             
         MVC   CMLKEY,KEY                                                       
         MVC   20(2,R6),AGENCY                                                  
         XC    ELEM+2(CMLDTAX-CMLDTAEL),ELEM+2                                  
         LA    R6,ELEM                                                          
         USING CMLDTAEL,R6                                                      
         MVI   CMLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL                                        
         MVI   CMLSEQ+2,X'01'      START SEQ NUMBER ONE IN MASTER               
         MVC   HOLDSEQ,CMLSEQ      START SEQ NUMBER ONE IN DSECT                
         MVC   CMLTITLE,=CL15'CMML SEQ RECORD'                                  
         GOTO1 ADDELEM             ADD CMML DATA ELEMENT                        
         GOTO1 ADDREC                                                           
         B     PAR20                                                            
*                                                                               
PAR10    GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   HOLDSEQ,CMLSEQ                                                   
*                                                                               
PAR20    MVC   KEY(L'SVKEY),SVKEY       RESTORE KEY AND AIO                     
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        NOW GET DATA ELEM                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   CMLSEQ,HOLDSEQ      AND PUT COMMERCIAL SEQ # IN IT               
                                                                                
         CLI   SVT2PR7,C'Y'        THIS AN AUTO NUMBER CMMLS AGY                
         JNE   EXIT                                                             
                                                                                
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(1),BAGYMD                                                 
         NI    CMLKAM,X'F0'        SET OFF MEDIA                                
         OI    CMLKAM,X'01'        FORCE TO TV                                  
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF CMML SEQ REC FOUND                        
         BE    PAR50               GO SAVE CMML SEQ NUMBER                      
         DROP  R4                                                               
                                                                                
* NOW MUST ADD CMML SEQ REC FOR AGENCY/MEDIA                                    
                                                                                
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         L     R6,AIO                                                           
         USING CMLKEY,R6                                                        
         XC    CMLRECD(256),CMLRECD                                             
         MVC   CMLKEY,KEY                                                       
         MVC   20(2,R6),AGENCY                                                  
         XC    ELEM+2(CMLDTAX-CMLDTAEL),ELEM+2                                  
         LA    R6,ELEM                                                          
         USING CMLDTAEL,R6                                                      
         MVI   CMLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL                                        
         MVI   CMLSEQ+2,X'01'      START CML NUMBER ONE IN MASTER               
         MVC   CODESEQ,CMLSEQ      START CML NUMBER ONE IN DSECT                
         MVC   CMLTITLE,=CL15'CML CODE RECORD'                                  
         GOTO1 ADDELEM             ADD CMML DATA ELEMENT                        
                                                                                
         GOTO1 ADDREC                                                           
         B     PAR60                                                            
                                                                                
PAR50    GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   CODESEQ,CMLSEQ                                                   
                                                                                
PAR60    MVC   KEY(L'SVKEY),SVKEY       RESTORE KEY AND AIO                     
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
                                                                                
         MVC   KEY+CMLKCML-CMLKEY(2),AGENCY                                     
         SR    R0,R0                                                            
         ICM   R0,7,CODESEQ                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+CMLKCML+2-CMLKEY(6),DUB                                      
         MVC   0(13,R6),KEY                                                     
         MVC   TRACML(8),KEY+CMLKCML-CMLKEY                                     
         MVI   TRACML+8,C' '                                                    
         OI    TRACMLH+6,X'80'      TRANS                                       
         MVC   HOLDCML,TRACML                                                   
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* AFTER ADDED RECORD - ADD PASSIVE KEYS, UPDATE SEQUENCE NUMBER                 
*================================================================               
                                                                                
AAREC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERROR,0                                                          
         MVC   SVDSKAD,KEY         SAVE DISK ADDR OF ADDED REC                  
         MVC   SVDADRA,KEY         SAVE DISK ADDR FOR PASSIVE PTR               
         MVC   AIO,AIO2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         SR    R1,R1                                                            
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         MVC   CMLKID(2),=X'0AA1'                                               
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         MVC   CMLKCML(3),HOLDSEQ                                               
         XC    CMLKCML+3(5),CMLKCML+3                                           
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+8                                                              
         OI    CMLKSTAT,CMLKSTA_PCKD                                            
         MVC   KEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                      
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* CREATE ADID POINTER FOR 8-12 CHAR CMML                                        
                                                                                
         MVC   CMLKID(2),=X'0AC1'                                               
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         MVC   WORK(12),SPACES                                                  
         L     R6,AIO1                                                          
         MVC   CMLKCML,5(R6)       PRESET CML IN KEY                            
         TM    15(R6),X'01'        PACKED CML IN KEY                            
         BO    AAR02               YES, KEY IS SET                              
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),5(R6)       PRESET CML IN KEY                            
         GOTO1 VTRPACK,DMCB,(C'P',WORK),CMLKCML                                 
         BNE   AAR05                                                            
                                                                                
AAR02    GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
                                                                                
AAR05    L     R6,AIO1                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   AAR20                                                            
*                                                                               
*        MVC   CMLKID(2),=X'0AE1'                                               
*        MVC   CMLRTEL,2(R6)                                                    
*                                                                               
*        TM    FLAGS,DELTELP       TEST IF DEL PASSIVE PTR FOUND                
*        BO    AAR10                                                            
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
*        CLI   DMCB+8,0                                                         
*        BE    AAR20                                                            
*        DC    H'0'                                                             
*                                                                               
*AAR10    DS    0H                                                              
*        NI    FLAGS,X'FF'-DELTELP                                              
*                                                                               
*        OI    DMINBTS,X'08'                                                    
*        GOTO1 HIGH                                                             
*        NI    DMINBTS,X'F7'                                                    
*        CLC   KEY(13),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        TM    KEY+13,X'80'        DELETED RECORD                               
*        BO    *+6                                                              
*        DC    H'0'                                                             
*        NI    KEY+13,X'7F'                                                     
*                                                                               
*        MVC   KEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                      
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
*        CLI   DMCB+8,0                                                         
*        BE    AAR20                                                            
*        DC    H'0'                                                             
*                                                                               
AAR20    DS    0H                                                               
                                                                                
AAR24    L     R6,AIO1                                                          
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
AAR26    BRAS  RE,NEXTEL                                                        
         BNE   AAR30                                                            
         LA    R1,2(R6)                                                         
         LA    R0,1                                                             
                                                                                
         USING CMLACTEL,R6                                                      
         MVI   ADIDFLAG,C'N'                                                    
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         BE    AAR28                                                            
         CLI   8(R1),C' '          TEST ISCI                                    
         BNH   AAR28               YES                                          
         GOTO1 VTRPACK,DMCB,(C'P',2(R6)),DUB   PACK 12 CHAR ADID                
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                         AND POINT TO IT                   
                                                                                
AAR28    BRAS  RE,UPDACT                                                        
         B     AAR26                                                            
*                                                                               
AAR30    DS    0H                                                               
         XC    KEY,KEY                                                          
         OC    SVHDDELK,SVHDDELK                                                
         BZ    AAR32                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
*        BNE   AAR32                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDDELK),SVHDDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
                                                                                
AAR32    XC    KEY,KEY                                                          
         OC    SVCCDELK,SVCCDELK                                                
         BZ    AAR35                                                            
         CLC   SVCCDELK(2),=X'0AC3'                                             
*        BNE   AAR35                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCDELK),SVCCDELK                                         
         BRAS  RE,DELPSSV                                                       
         XC    SVCCDELK,SVCCDELK                                                
                                                                                
AAR35    XC    KEY,KEY                                                          
         OC    SVHDADDK,SVHDADDK                                                
         BZ    AAR38                                                            
         CLC   SVHDADDK(2),=X'0AC2'                                             
*        BNE   AAR38                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVHDADDK),SVHDADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
                                                                                
AAR38    XC    KEY,KEY                                                          
         OC    SVCCADDK,SVCCADDK                                                
         BZ    AAR40                                                            
         CLC   SVCCADDK(2),=X'0AC3'                                             
*        BNE   AAR40                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVCCADDK),SVCCADDK                                         
         BRAS  RE,ADDPSSV                                                       
         XC    SVCCADDK,SVCCADDK                                                
                                                                                
AAR40    DS    0H                                                               
                                                                                
AAR45    DS    0H                                                               
         CLI   SVT2PR7,C'Y'        THIS AUTO CODE AGY                           
         BNE   AAR70                                                            
                                                                                
         CLI   QMED,C'R'           NOT FOR RADIO                                
         BE    AAR70                                                            
         CLI   QMED,C'X'           NOT FOR NETWORK RADIO                        
         BE    AAR70                                                            
                                                                                
         CLI   QMED,C'T'           MUST BE TV, THEN WILL ADD NET                
         BE    *+6                                                              
         DC    H'0'                BUG SHOULD NEVER HAVE MEDIA N HERE           
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         NI    2(R6),X'F0'                                                      
         OI    2(R6),X'03'         CHANGE MEDIA TO NETWORK                      
         MVC   KEY(13),0(R6)                                                    
                                                                                
         GOTO1 ADDREC                                                           
                                                                                
         MVC   SVKEY(4),KEY        SAVE DISK ADDRESS                            
                                                                                
* GET NETWORK TV COMML SEQ REC IF THERE                                         
                                                                                
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
                                                                                
         NI    CMLKAM,X'F0'                                                     
         OI    CMLKAM,X'03'         FORCE TO NET TV                             
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   AAR50                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         CLC   CMLSEQ,HOLDSEQ      COMPARE NET TO TV SEQ                        
         BL    AAR50                                                            
         MVC   HOLDSEQ,CMLSEQ      USE NET COMML SEQ                            
         SR    R1,R1                                                            
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
AAR50    MVC   AIO,AIO1                                                         
         MVC   CMLKID,=X'0AA1'                                                  
         MVC   CMLKAM(3),BAGYMD & BCLT                                          
         NI    CMLKAM,X'F0'                                                     
         OI    CMLKAM,X'03'         FORCE TO NETWORK TV                         
                                                                                
         MVC   CMLKCML(3),HOLDSEQ                                               
         XC    CMLKCML+3(5),CMLKCML+3                                           
                                                                                
         MVC   KEY+14(4),SVKEY     MOVE IN DISK ADDR                            
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   AAR70                                                            
         OI    CMLKID+1,X'40'       CHANGE A1 TO E1                             
         MVC   CMLRTEL,2(R6)                                                    
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AAR70    DS    0H                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   KEY(13),0(R1)       RESTORE KEY AND AIO                          
         BRAS  RE,GENR             GO GENERATE AUTO-TURNAROUND REQ              
                                                                                
                                                                                
AAR80    CLI   SVT2PR7,C'Y'        THIS AUTO CODE AGY                           
         BNE   AARX                                                             
                                                                                
         CLI   QMED,C'R'           NOT FOR RADIO                                
         BE    AARX                                                             
                                                                                
         CLI   QMED,C'X'           NOT FOR NETWORK RADIO                        
         BE    AARX                                                             
                                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(1),BAGYMD                                                 
         NI    CMLKAM,X'F0'        SET OFF MEDIA                                
         OI    CMLKAM,X'01'        FORCE TO TV                                  
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         SR    R1,R1                                                            
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
         GOTO1 PUTREC                                                           
                                                                                
AARX     XIT1                                                                   
         DROP  R4,R6,RB                                                         
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
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
         SSPEC H8,3,C'COMML ID'                                                 
         SSPEC H9,3,C'--------'                                                 
         SSPEC H8,17,C'LEN'                                                     
         SSPEC H9,17,C'-----'                                                   
         SSPEC H8,24,C'TITLE'                                                   
         SSPEC H9,24,C'---------------'                                         
         SSPEC H8,40,C'P'                                                       
         SSPEC H9,40,C'S'                                                       
         SSPEC H8,42,C'RELEASE'                                                 
         SSPEC H9,42,C'--DATE--'                                                
         SSPEC H8,52,C'RECALL'                                                  
         SSPEC H9,51,C'--DATE--'                                                
         SSPEC H8,60,C'TYPE'                                                    
         SSPEC H9,60,C'----'                                                    
         SSPEC H8,66,C'PRODUCT LIST'                                            
         SSPEC H9,66,C'------------------------'                                
         SSPEC H8,91,C'OTHER DETAILS'                                           
         SSPEC H9,91,C'--------------------------'                              
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
VALMAT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         NI    FLAGS2,X'FF'-MATELEM                                             
         XC    FLD,FLD                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,CMLMATEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   VALMAT05                                                         
                                                                                
         GOTO1 REMELEM                                                          
         MVC   FLD,ELEM            SAVE PREVIOUS ELEMENT                        
                                                                                
*===================================================================            
* NOTE, THAT THIS LOOP RELIES ON 6 EQUAL LENGTH PERIOD FIELDS                   
* TAKE GREAT CARE WHEN CHANGING THE SCREEN, PLEASE                              
*===================================================================            
                                                                                
VALMAT05 LA    R2,TRADT1H                                                       
         LA    R3,6                                                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLMATCH,R6                                                      
         MVI   CMLMATEL,CMLMATEQ                                                
         MVI   CMLMATLN,CMLMATDL                                                
         LA    R5,CMLMPER1                                                      
                                                                                
* PERIOD VALIDATION LOOP                                                        
                                                                                
VALMAT10 DS    0H                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VALMAT50            NO, GO TO NEXT FIELD                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),PERVALST                                          
         CLI   DMCB+4,0                                                         
         BNE   INVPER                                                           
*                                                                               
         MVC   0(4,R5),PERVALST+PVALCSTA-PERVALD                                
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT50 DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LA    R5,L'CMLMPER1(R5)                                                
         BCT   R3,VALMAT10                                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                 
         L     RF,0(R1)                                                         
*                                                                               
         XC    CMLMSTIM,CMLMSTIM                                                
         MVC   CMLMETIM,=X'FFFF'                                                
*                                                                               
         LA    R2,TRASTIMH                                                      
         CLI   5(R2),0                                                          
         BE    VALMAT60                                                         
*                                                                               
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),WORK                                              
         CLI   0(R1),X'FF'                                                      
         JE    INVTERR                                                          
         MVC   CMLMSTIM,WORK                                                    
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT60 DS    0H                                                               
         LA    R2,TRAETIMH                                                      
         CLI   5(R2),0                                                          
         BE    VALMAT70                                                         
*                                                                               
         LA    R4,8(R2)                                                         
         ICM   R4,8,5(R2)                                                       
         GOTO1 (RF),DMCB,(R4),WORK                                              
         CLI   0(R1),X'FF'                                                      
         BE    INVTERR                                                          
         MVC   CMLMETIM,WORK                                                    
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT70 DS    0H                                                               
         LA    R2,TRADLYH                                                       
         OC    CMLMSTIM,CMLMSTIM       IF TIME ENTEREDD                         
         BNZ   VALMAT72                MUST INPUT DAILY FLAG                    
         CLC   CMLMETIM,=X'FFFF'                                                
         BE    VALMAT80                                                         
*                                                                               
VALMAT72 CLI   TRADLYH+5,0                                                      
         BE    INVDLY                                                           
*                                                                               
         CLI   TRADLY,C'N'                                                      
         BE    VALMAT80                                                         
         CLI   TRADLY,C'Y'                                                      
         BNE   INVDLY                                                           
         OI    CMLMFLAG,CMLMFDAY                                                
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT80 DS    0H                                                               
         TM    FLAGS2,MATELEM                                                   
         BNZ   VALMAT85                                                         
         OC    FLD,FLD                                                          
         BZ    VALMATX                                                          
                                                                                
VALMAT85 DS    0H                                                               
E        USING CMLMATEL,FLD                                                     
         USING CMLMATEL,R6                                                      
         OC    E.CMLMETIM,E.CMLMETIM                                            
         BNZ   *+10                                                             
         MVC   E.CMLMETIM,=X'FFFF'                                              
*                                                                               
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
         OI    FLAGS2,MATELEM                                                   
         BZ    VALMATX                                                          
         GOTO1 ADDELEM                                                          
*                                                                               
VALMATX  J     EXIT                                                             
         DROP  E,R6                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
INVPER   LHI   R0,INVPERMS                                                      
         J     TRAPERR2                                                         
*                                                                               
INVTERR  LHI   R0,INVTIMMS                                                      
         J     TRAPERR2                                                         
*                                                                               
INVDLY   MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
         EJECT                                                                  
*====================================================================           
* KEEP LAST 6 CHANGE ELEMENTS                                                   
* AIO3 HAS PREVIOUS VERSION OF RECORD                                           
* SAVE 8 PRODUCTS, START DATE, END DATE, AND SLN FROM IT                        
* ELEMENT LEN IS FIXED, SO FIELDS SAVED EVEN IF THEY DON'T CHANGE               
*====================================================================           
                                                                                
SETCHGEL NTR1  BASE=*,LABEL=*                                                   
         OC    CHGFLAGS,CHGFLAGS    TEST ANY CHANGES                            
         JZ    EXIT                                                             
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         JE    EXIT                OBVIOUSLY THIS IS NOT A CHANGE!              
*                                                                               
         XC    ELEM,ELEM                                                        
C        USING CMLCHEL,ELEM                                                     
         MVI   C.CMLCHEL,X'C0'                                                  
         MVI   C.CMLCHLEN,CMLCHELX-CMLCHEL                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,C.CMLCHDAT)                                 
         BAS   RE,GETTIME                                                       
         MVC   C.CMLCHTIM,FULL                                                  
*                                                                               
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
*                                                                               
SETCHG10 MVC   C.CMLCHDT1(3),CHGFLAGS MOVE CHANGE FLAGS                         
*                                                                               
         L     R6,AIO3             SAVE OLD DATA                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   C.CMLCHSDT,CMLRLSE                                               
         MVC   C.CMLCHEDT,CMLRCL                                                
         MVC   C.CMLCHSLN,CMLSLN                                                
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'20'        PRODUCT LIST                                 
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         CHI   RF,8                                                             
         BNH   *+8                                                              
         LHI   RF,8                                                             
         AHI   RF,-3               SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   C.CMLCHPRD(0),2(R6)                                              
*                                                                               
         L     R6,AIO3             SAVE OLD DATA                                
         MVI   ELCODE,X'B0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SETCHG18                                                         
         USING CMLMATEL,R6                                                      
*                                                                               
         MVC   C.CMLCHSTM,CMLMSTIM                                              
         MVC   C.CMLCHETM,CMLMETIM                                              
*                                                                               
SETCHG18 L     R6,AIO                                                           
         MVI   ELCODE,X'C0'        FIND MOST RECENT CHANGEL                     
         BRAS  RE,GETEL                                                         
         BNE   SETCHG30                                                         
         USING CMLCHEL,R6                                                       
*                                                                               
         CLC   CMLCHDAT(11),C.CMLCHDAT   TEST SAME DATE/PERSON                  
         BNE   SETCHG20                                                         
         MVC   CMLCHTIM,C.CMLCHTIM       MOVE IN LATEST TIME                    
         OC    CMLCHDT1(3),C.CMLCHDT1    'OR' IN NEW ACTIVITY                   
         J     EXIT                                                             
*                                                                               
SETCHG20 L     R6,AIO              IF > 6 ELEMENTS, DELETE OLDEST               
         SR    R5,R5                                                            
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
SETCHG22 BRAS  RE,NEXTEL                                                        
         BNE   SETCHG30                                                         
         AHI   R5,1                                                             
         CHI   R5,6                                                             
         BL    SETCHG22                                                         
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
*                                                                               
SETCHG30 L     R6,AIO              ADD NEW BEFORE OTHER CHGELS                  
         BRAS  RE,GETEL               OR AT EOR                                 
         GOTO1 VRECUP,DMCB,AIO,ELEM,(R6)                                        
         J     EXIT                                                             
*                                                                               
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
*==================================================================             
* DISPLAY TLCSTR ELEMENTS USING B2 SCREEN                                       
*==================================================================             
                                                                                
DRTELE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MYSCREEN,X'B2'                                                   
         BE    DRTEL50                                                          
*                                                                               
*        CLI   ACTNUM,ACTSEL                                                    
*        BNE   *+8                                                              
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE KEY FIELDS                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,TRAKEYH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'B2'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCREEN,X'B2'    SET B2 SCREEN LOADED                           
         BRAS  RE,SETPFK                                                        
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE KEY FIELDS                           
*                                                                               
DRTEL50  DS    0H                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD                                   
         BNE   DRTEL52             INVALID ON TELECASTER SCREEN                 
                                                                                
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         J     TRAPERR                                                          
*                                                                               
DRTEL52  DS    0H                                                               
         MVC   TRTHDTL,SPACES                                                   
         OI    TRTHDTLH+6,X'80'                                                 
         MVC   TRTTELC,SPACES                                                   
         OI    TRTTELCH+6,X'80'                                                 
         MVC   TRTCBC,SPACES                                                    
         OI    TRTCBCH+6,X'80'                                                  
         MVC   TRTTAL1,SPACES                                                   
         OI    TRTTAL1H+6,X'80'                                                 
         MVC   TRTTAL2,SPACES                                                   
         OI    TRTTAL2H+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'45'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRTEL55                                                          
         USING CMLXTLEL,R6                                                      
         LA    R2,TRTTELCH                                                      
         MVC   TRTTELC,SPACES                                                   
         MVC   TRTTELC(L'CMLXTLNM),CMLXTLNM                                     
         OI    6(R2),X'80'                                                      
         LA    R2,TRTHDTLH                                                      
         MVC   TRTHDTL,SPACES                                                   
         MVC   TRTHDTL(L'CMLXTLHD),CMLXTLHD                                     
         OI    6(R2),X'80'                                                      
         B     DRTEL60                                                          
         DROP  R6                                                               
*                                                                               
DRTEL55  L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRTEL60                                                          
         USING CMLTELEL,R6                                                      
         LA    R2,TRTTELCH                                                      
         MVC   TRTTELC,SPACES                                                   
         MVC   TRTTELC(L'CMLTELNO),CMLTELNO                                     
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DRTEL60  L     R6,AIO                                                           
         MVI   ELCODE,X'46'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRTEL70                                                          
         USING CMLCBCEL,R6                                                      
         LA    R2,TRTCBCH                                                       
         MVC   TRTCBC,SPACES                                                    
         MVC   TRTCBC,CMLCBCNM                                                  
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DRTEL70  L     R6,AIO                                                           
         MVI   ELCODE,X'47'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRTEL80                                                          
         USING CMLTCYEL,R6                                                      
         LA    R2,TRTTAL1H                                                      
         MVC   TRTTAL1,CMLTCYC1                                                 
         OI    6(R2),X'80'                                                      
         LA    R2,TRTTAL2H                                                      
         MVC   TRTTAL2,CMLTCYC2                                                 
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DRTEL80  DS    0H                                                               
*                                                                               
         OI    TRTMEDH+6,X'20'                                                  
         OI    TRTCLTH+6,X'20'                                                  
         OI    TRTCMLH+6,X'20'                                                  
*                                                                               
         OI    TRTMEDH+6,X'40'      POSITION CURSOR                             
         OI    CONSERVH+1,X'01'     FORCE MODIFIED                              
         OI    CONSERVH+6,X'80'     AND XMT                                     
*                                                                               
DRTEXIT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* VALIDATE TELECASTER INPUT                                                     
*==================================================================             
                                                                                
VRTELE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SPOTCAN,C'C'          IS THIS CANADA                             
         JNE   VRTEL70               FIX PF4 TO RETURN FROM TLCSTR SCR          
*SM->    JNE   EXIT                                                             
*                                                                               
         MVI   ELCODE,X'40'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'45'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'46'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'47'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VRTEL15  DS    0H                                                               
         CLI   TRTTELCH+5,0                                                     
         BNE   *+12                                                             
         CLI   TRTHDTLH+5,0                                                     
         BE    VRTEL20                                                          
                                                                                
         USING CMLXTLEL,R3                                                      
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'45'                                                       
         MVI   ELEM+1,CMLXTLEN                                                  
         MVC   CMLXTLNM,TRTTELC                                                 
         MVC   CMLXTLHD,TRTHDTL                                                 
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
VRTEL20  DS    0H                                                               
         CLI   TRTCBCH+5,0                                                      
         BE    VRTEL25                                                          
                                                                                
         USING CMLCBCEL,R3                                                      
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'46'                                                       
         MVI   ELEM+1,CMLCBLEN                                                  
         MVC   CMLCBCNM,TRTCBC                                                  
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
VRTEL25  DS    0H                                                               
         CLI   TRTTAL1H+5,0                                                     
         BNE   VRTEL30                                                          
         CLI   TRTTAL2H+5,0                                                     
         BE    VRTEL40                                                          
         LA    R2,TRTTAL1H                                                      
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
                                                                                
         USING CMLTCYEL,R3                                                      
VRTEL30  LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'47'                                                       
         MVI   ELEM+1,CMLTCLEN                                                  
         MVC   CMLTCYC1,TRTTAL1                                                 
         MVC   CMLTCYC2,TRTTAL2                                                 
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
VRTEL40  L     R6,AIO                                                           
         MVI   ELCODE,X'45'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VRTEL50                                                          
         USING CMLXTLEL,R6                                                      
         LA    R2,TRTTELCH                                                      
         MVC   TRTTELC,CMLXTLNM                                                 
         OI    6(R2),X'80'                                                      
         LA    R2,TRTHDTLH                                                      
         MVC   TRTHDTL,CMLXTLHD                                                 
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
VRTEL50  L     R6,AIO                                                           
         MVI   ELCODE,X'46'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VRTEL60                                                          
         USING CMLCBCEL,R6                                                      
         LA    R2,TRTCBCH                                                       
         MVC   TRTCBC,CMLCBCNM                                                  
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
VRTEL60  L     R6,AIO                                                           
         MVI   ELCODE,X'47'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VRTEL70                                                          
         USING CMLTCYEL,R6                                                      
         LA    R2,TRTTAL1H                                                      
         MVC   TRTTAL1,CMLTCYC1                                                 
         OI    6(R2),X'80'                                                      
         LA    R2,TRTTAL2H                                                      
         MVC   TRTTAL2,CMLTCYC2                                                 
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
VRTEL70  DS    0H                                                               
*                                                                               
         LA    R2,TRTMEDH                                                       
         OI    6(R2),X'40'          POSITION CURSOR                             
         OI    CONSERVH+1,X'01'     FORCE MODIFIED                              
         OI    CONSERVH+6,X'80'     AND XMT                                     
         J     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* DISPLAY PRODUCT SUBSTITUTE ELEMENTS ON F6 SCREEN                              
*==================================================================             
                                                                                
DRPRSUB  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETF6                                                         
*                                                                               
DRPRS52  LA    R2,TPRPRD1H                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'B1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRPRS56                                                          
         USING CMLPRSEL,R6                                                      
*                                                                               
DRPRS54  MVC   8(3,R2),CMLPRSPR                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT SYSTEM                      
         MVC   AIO,AIO2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),CMLPRSPR                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME+3(3),=CL3'FIL' SWITCH TO SPOT SYSTEM                    
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME       SWITCH TO SPOT SYSTEM                    
         MVC   AIO,AIO1                RESTORE AIO                              
*                                                                               
         L     R1,AIO2                                                          
         USING PRDHDRD,R1                                                       
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),PNAME                                                   
         OI    6(R2),X'80'                                                      
         DROP  R1                                                               
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,CMLPRSDT),(5,DUB)                                 
         MVC   8(8,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 (RF),(R1),(3,CMLPREDT),(5,DUB)                                   
         MVC   8(8,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BRAS  RE,NEXTEL                                                        
         BE    DRPRS54                                                          
*                                                                               
DRPRS56  LA    R0,TPRPFKH          CLEAR REMAINING FIELDS                       
         CR    R2,R0                                                            
         BNL   DRPRSX                                                           
         LLC   RE,0(R2)                                                         
         AHI   RE,-9                                                            
         EX    RE,DRPRSCLR                                                      
         LA    R2,9(RE,R2)                                                      
         B     DRPRS56                                                          
*                                                                               
DRPRSX   OI    TPRMEDH+6,X'20'                                                  
         OI    TPRCLTH+6,X'20'                                                  
         OI    TPRCMLH+6,X'20'                                                  
*                                                                               
         OI    TPRMEDH+6,X'40'      POSITION CURSOR                             
         OI    CONSERVH+1,X'01'     FORCE MODIFIED                              
         OI    CONSERVH+6,X'80'     AND XMT                                     
         J     EXIT                                                             
*                                                                               
DRPRSCLR XC    8(0,R2),8(R2)                                                    
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
*==================================================================             
* VALIDATE PRODUCT SUBSTITUTE ELEMENTS ON F6 SCREEN                             
*==================================================================             
                                                                                
VRPRSUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MYSCREEN,X'F6'                                                   
         BE    VRPRS2                                                           
         BRAS  RE,GETF6                                                         
         J     EXIT                                                             
*                                                                               
VRPRS2   MVI   ELCODE,X'B1'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,TPRPRD1H                                                      
*                                                                               
VRPRS4   CLI   5(R2),0             TEST FOR INPUT                               
         BE    VRPRS20             NO - SKIP INPUT LINE                         
*                                                                               
         L     R4,ASVCLIST         FIND PRODUCT IN PRDLIST                      
         MVC   WORK(3),8(R2)                                                    
         OC    WORK(3),SPACES                                                   
*                                                                               
VRPRS10  CLC   0(3,R4),WORK                                                     
         BE    VRPRS12                                                          
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    VRPRS10                                                          
         MVI   ERROR,INVPROD                                                    
         J     TRAPERR                                                          
                                                                                
* MAKE SURE PRD NOT IN CMML AND THAT CMML IS NOT FOR ALL PRDS                   
                                                                                
VRPRS12  L     R6,AIO                                                           
         USING CMLRECD,R6                                                       
         MVI   ERROR,INVPROD                                                    
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'FF'         TEST FOR ALL PRDS                            
         JE    TRAPERR                                                          
*                                                                               
         LLC   R0,1(R6)                                                         
         AHI   R0,-2                                                            
         LA    R1,2(R6)                                                         
VRPRS13  CLC   0(1,R1),3(R4)       MATCH PRD CODE                               
         JE    TRAPERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VRPRS13                                                       
*                                                                               
         USING CMLPRSUB,R3                                                      
VRPRS14  LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'B1'                                                       
         MVI   ELEM+1,CMLPRSUBX-CMLPRSUB                                        
         MVC   CMLPRSPR,0(R4)      MOVE 3 CHAR PRD                              
         MVC   CMLPRSCD,3(R4)      MOVE 1 BYTE PRD                              
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO START DATE                          
         GOTO1 DATVAL,DMCB,8(R2),DUB                                            
         OC    0(4,R1),0(R1)                                                    
         JZ    DATERR                                                           
         GOTO1 DATCON,DMCB,DUB,(3,CMLPRSDT)                                     
*                                                                               
         L     R6,AIO                                                           
         USING CMLRECD,R6                                                       
         CLC   CMLPRSDT,CMLRLSE    TEST START PRIOR TO RELEASE                  
         BL    VRPRSER1                                                         
         CLC   CMLPRSDT,CMLRCL     OR AFTER RECALL                              
         BH    VRPRSER1                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO END DATE                            
         GOTO1 DATVAL,DMCB,8(R2),DUB                                            
         OC    0(4,R1),0(R1)                                                    
         JZ    DATERR                                                           
         GOTO1 DATCON,DMCB,DUB,(3,CMLPREDT)                                     
*                                                                               
         CLC   CMLPRSDT,CMLPREDT                                                
         BH    VRPRSER2                                                         
*                                                                               
         L     R6,AIO                                                           
         USING CMLRECD,R6                                                       
         CLC   CMLPREDT,CMLRLSE    TEST START PRIOR TO RELEASE                  
         BL    VRPRSER1                                                         
         CLC   CMLPREDT,CMLRCL     OR AFTER RECALL                              
         BH    VRPRSER1                                                         
* SEE IF OVERLAP WITH ANY EXISTING ELEM FOR THIS PRD                            
         MVI   ELCODE,X'B1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VPRS18                                                           
*                                                                               
VPRS16   CLC   ELEM(6),0(R6)       SAME PRD                                     
         BNE   VPRS16X                                                          
         CLC   ELEM+6(3),9(R6)     ELEM START AFTER EXISTING END                
         BH    VPRS16X                                                          
         CLC   ELEM+9(3),6(R6)     ELEM END BEFORE EXISTING START               
         BL    VPRS16X                                                          
         LHI   R0,SUBOVLAP                                                      
         J     TRAPERR2                                                         
*                                                                               
VPRS16X  BRAS  RE,NEXTEL                                                        
         BE    VPRS16                                                           
*                                                                               
VPRS18   GOTO1 ADDELEM                                                          
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT PRD                            
         B     VRPRS22                                                          
*                                                                               
VRPRS20  AHI   R2,TPRPRD2-TPRPRD1  SKIP AN INPUT LINE                           
*                                                                               
VRPRS22  LA    R0,TPRPFKH                                                       
         CR    R2,R0               TEST REACHED EOS                             
         BL    VRPRS4                                                           
         OI    CHGFLAG3,CMLCH_PRSB  SET CHANGE FLAG                             
         BRAS  RE,SETCHGEL                                                      
         J     EXIT                                                             
         DROP  R3,R6                                                            
*                                                                               
VRPRSER1 LHI   R0,NOTINPER                                                      
         J     TRAPERR2                                                         
*                                                                               
VRPRSER2 LHI   R0,STENDERR                                                      
         J     TRAPERR2                                                         
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* GET F6 SCREEN                                                                 
*==================================================================             
                                                                                
GETF6    NTR1  BASE=*,LABEL=*                                                   
         CLI   MYSCREEN,X'F6'                                                   
         BE    GETF6X                                                           
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE KEY FIELDS                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,TRAKEYH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'F6'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCREEN,X'F6'    SET F6 SCREEN LOADED                           
         BRAS  RE,SETPFK                                                        
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXH-TRAKEYH                                               
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE KEY FIELDS                           
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD                                   
         BNE   GETF6X              INVALID                                      
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         J     TRAPERR                                                          
*                                                                               
GETF6X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* MAKE SURE DATE CHANGE KEEPS SUBST ELEMS IN CML PERIOD                         
* WORK HAS CMLRLSE/CMLRCL                                                       
*========================================================                       
                                                                                
CHKPRSOV NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'B1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKPRSX                                                          
*                                                                               
CHKPRS02 CLC   CMLPRSDT-CMLPRSEL(3,R6),WORK     DATE PRIOR TO RELEASE           
         BL    CHKOVERR                                                         
         CLC   CMLPREDT-CMLPRSEL(3,R6),WORK+3   OR AFTER RECALL                 
         BH    CHKOVERR                                                         
         BRAS  RE,NEXTEL                                                        
         BE    CHKPRS02                                                         
*                                                                               
CHKPRSX  J     EXIT                                                             
*                                                                               
CHKOVERR LHI   R0,NOTINPER                                                      
         J     TRAPERR2                                                         
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* DISPLAY CHANGE ELEMENTS USING DF SCREEN                                       
*==================================================================             
                                                                                
DSPCHG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,TRAFLTRH                                                      
         ST    RE,DMCB                                                          
         LA    R2,TRAFLTRH         FIRST DISPLAY LINE                           
         MVI   DMCB,X'DF'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCREEN,X'DF'      SET DF SCREEN LOADED                         
*                                                                               
         LA    R2,TRAFLTRH         FIRST DISPLAY LINE                           
         USING ACDD,R2                                                          
*                                                                               
         LA    R3,8(R2)                                                         
         MVC   0(5,R3),=C'ADDED'                                                
         XC    1(4,R3),SPACES      MAKE LOWERCASE                               
         MVI   8(R3),C'?'                                                       
         LA    R3,8(R3)                                                         
*MN                                                                             
         L     R6,AIO                                                           
         CLC   0(2,R6),=X'0A21'                                                 
         BE    DSP005                                                           
         CLC   KEY(2),=X'0A21'                                                  
         BNE   DSPCHG0                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*MN                                                                             
DSP005   L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   DSPCHG0                                                          
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(8,(R3))                                
*                                                                               
DSPCHG0  LA    R2,ACDNEXT                                                       
*                                                                               
         MVI   ELCODE,X'C0'                                                     
         L     R6,AIO              FIND MY CHANGE ELEMENTS                      
         USING CMLCHEL,R6                                                       
*                                                                               
         SR    R4,R4                                                            
         BRAS  RE,GETEL                                                         
         BE    DSPCHG1                                                          
         MVC   ACDDATA(28),=C'CHANGE HISTORY NOT AVAILABLE'                     
         B     DSPCHGX4                                                         
*                                                                               
DSPCHG1  MVC   CHGELNUM,SVNXTCHG   NOTE STARTING POINT                          
         CLI   SVNXTCHG,0                                                       
         BE    DSPCHG2                                                          
*                                                                               
DSPCHG1A LA    R4,1(R4)                                                         
         CLM   R4,1,SVNXTCHG                                                    
         BE    DSPCHG2                                                          
         BRAS  RE,NEXTEL                                                        
         BE    DSPCHG1A                                                         
         DC    H'0'                                                             
*                                                                               
DSPCHG2  OI    1(R2),X'08'            SET HIGH INTENSITY                        
         MVC   ACDDATA(7),=C'CHANGED'                                           
         XC    ACDDATA+1(6),SPACES    MAKE LOWERCASE                            
         LA    R4,ACDDATA+8                                                     
         GOTO1 DATCON,DMCB,(3,CMLCHDAT),(8,(R4))                                
         LA    R4,9(R4)                                                         
*                                                                               
         MVC   0(12,R4),=C'AT HH.MM BY '                                        
         XC    0(12,R4),SPACES                                                  
         MVI   5(R4),C'.'                                                       
*                                                                               
         LA    R4,3(R4)            POINT TO HH                                  
         LA    R1,CMLCHTIM                                                      
         LA    R0,2                                                             
*                                                                               
DSPCHG4  SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
*                                                                               
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DSPCHG4                                                       
*                                                                               
         LA    R4,3(R4)            NEXT OUTPUT POSITION                         
         MVC   0(8,R4),CMLCHWHO                                                 
*                                                                               
         LA    R2,ACDNEXT          NEXT DISPLAY LINE                            
         CLI   0(R2),0             TEST EOS                                     
         BE    DSPCHGX                                                          
*                                                                               
         LA    R3,12(R2)           FIRST OUTPUT POSITION                        
         LA    R4,CHGTAB1                                                       
         LA    R5,CHGTAB1N                                                      
         LA    R1,CMLCHDT1         BYTE TO BE TESTED                            
         BAS   RE,TESTIT                                                        
*                                                                               
         LA    R4,CHGTAB2                                                       
         LA    R5,CHGTAB2N                                                      
         LA    R1,CMLCHDT2                                                      
         BAS   RE,TESTIT                                                        
*                                                                               
         LA    R4,CHGTAB3                                                       
         LA    R5,CHGTAB3N                                                      
         LA    R1,CMLCHDT3                                                      
         BAS   RE,TESTIT                                                        
*                                                                               
         BCTR  R3,0                BACK UP TO END OF LAST FIELD                 
         CLI   0(R3),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '          AND GET RID OF /                             
                                                                                
         CLI   1(R6),20            TEST FOR OLD CHGEL LENGTH                    
         BNH   DSPCHG40                                                         
*                                                                               
         LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)           START OF OUTPUT DATA                         
                                                                                
*============================================================                   
* FOR MOST RECENT CHANGE, NEW DATA IS IN RECORD                                 
* FOR PREVIOUS CHANGES, IT IS IN PREVIOUS CHGEL                                 
*============================================================                   
                                                                                
         LR    R7,R6               SAVE CHANGE ELEMENT ADDRESS                  
         XC    WORK,WORK           EXTRACT NEW DATA INTO WORK                   
*                                                                               
         CLI   CHGELNUM,0          IF THIS IS FIRST CHGEL                       
         BNE   DSPCHG6                                                          
         BRAS  RE,GETCUR                                                        
         B     DSPCHG8                                                          
*                                                                               
DSPCHG6  LLC   R5,CHGELNUM         GET CURRENT CHGEL NUMBER                     
*                                                                               
         L     R6,AIO              FIND MY CHANGE ELEMENTS                      
         USING CMLCHEL,R6                                                       
         BRAS  RE,GETEL                                                         
         B     DSPCHG7A                                                         
*                                                                               
DSPCHG7  BRAS  RE,NEXTEL           AND GET THE ONE BEFORE IT                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DSPCHG7A BCT   R5,DSPCHG7                                                       
*                                                                               
         MVC   WORK(23),CMLCHPRD   SAVE AS THE CURRENT 'NEW'                    
*                                                                               
DSPCHG8  LLC   R0,CHGELNUM                                                      
         AHI   R0,1                                                             
         STC   R0,CHGELNUM                                                      
*                                                                               
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
*                                                                               
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
*                                                                               
         MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
         LA    R4,3(R4)                                                         
         GOTO1 DATCON,DMCB,(3,WORK+8),(8,0(R4))                                 
         MVI   8(R4),C'-'                                                       
         MVC   9(3,R4),=C'UFN'                                                  
         CLC   =X'FFFFFF',WORK+11                                               
         BE    DSPCHG12                                                         
         GOTO1 (RF),(R1),(3,WORK+11),(8,9(R4))                                  
*                                                                               
DSPCHG12 LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)                                                        
*                                                                               
DSPCHG14 TM    CMLCHDT1,CMLCH_SLN                                               
         BZ    DSPCHG20                                                         
*                                                                               
DSPCHG16 MVC   0(8,R4),=C'SLN FROM'                                             
         XC    1(7,R4),SPACES                                                   
         LA    R4,9(R4)                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,CMLCHSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
*                                                                               
         MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
         IC    R0,WORK+14                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(3,R4),DUB                                                      
*                                                                               
         LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)                                                        
*                                                                               
DSPCHG20 TM    CMLCHDT2,CMLCH_TIME                                              
         BZ    DSPCHG30                                                         
*                                                                               
         MVC   0(20,R4),=C'MATCHING TIMES FROM '                                
         XC    1(19,R4),SPACES                                                  
*                                                                               
         OC    CMLCHSTM(4),CMLCHSTM                                             
         BZ    DSPCHG21                                                         
         CLC   CMLCHSTM(4),=X'0000FFFF'                                         
         BNE   DSPCHG22                                                         
DSPCHG21 MVC   20(4,R4),=C'NONE'                                                
         LA    R4,25(R4)                                                        
         B     DSPCHG24                                                         
*                                                                               
DSPCHG22 GOTO1 UNTIME,DMCB,CMLCHSTM,20(R4)                                      
         LA    R4,32(R4)                                                        
         CLI   0(R4),C' '          FIND LAST CHAR OF TIME                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
*                                                                               
DSPCHG24 MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
*                                                                               
         MVC   3(4,R4),=C'NONE'                                                 
         CLC   WORK+15(4),=X'0000FFFF'                                          
         BE    DSPCHG26                                                         
         GOTO1 UNTIME,DMCB,WORK+15,3(R4)                                        
*                                                                               
DSPCHG26 LA    R2,ACDNEXT                                                       
         CLI   0(R2),0                                                          
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
         LA    R4,12(R2)                                                        
*                                                                               
DSPCHG30 TM    CMLCHDT1,CMLCH_PRDL                                              
         BZ    DSPCHG42                                                         
                                                                                
         MVC   0(9,R4),=C'PRDS FROM'                                            
         XC    1(8,R4),SPACES                                                   
         LA    R4,10(R4)                                                        
*                                                                               
         LA    R1,CMLCHPRD                                                      
         LA    R0,8                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   DSPCHG32                                                         
         LA    R4,4(R4)                                                         
         B     DSPCHG34                                                         
*                                                                               
DSPCHG32 BAS   RE,GETPRD                                                        
*                                                                               
         MVC   0(3,R4),0(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         LA    R4,2(R4)            POINT TO 3RD CHAR                            
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,DSPCHG32                                                      
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
*                                                                               
DSPCHG34 MVC   0(2,R4),=C'TO'                                                   
         XC    0(2,R4),SPACES                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         LA    R1,WORK                                                          
         LA    R0,8                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         CLI   0(R1),X'FF'                                                      
         BE    DSPCHG40                                                         
*                                                                               
DSPCHG36 BAS   RE,GETPRD                                                        
*                                                                               
         MVC   0(3,R4),0(RF)                                                    
*                                                                               
         LA    R4,2(R4)            POINT TO 3RD CHAR                            
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,DSPCHG36                                                      
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
DSPCHG40 LA    R2,ACDNEXT                                                       
         CLI   0(R2),0             TEST REACHED EOS                             
         BE    DSPCHGX                                                          
         XC    ACDDATA,ACDDATA                                                  
*                                                                               
DSPCHG42 MVI   ELCODE,X'C0'        RESTORE ELCODE                               
         BRAS  RE,NEXTEL           GET NEXT CHANGE ELEMENT                      
         BE    DSPCHG2             GO BACK TO DISPLAY                           
         B     DSPCHGX4                                                         
*                                                                               
DSPCHGX  CLI   0(R2),0             DID WE GET HERE BECAUSE OF EOS               
         BNE   DSPCHGX4            NO                                           
*                                                                               
         LR    R7,R6               SAVE CURRENT CHGEL ADDR                      
         L     R6,AIO              COUNT TO THIS CHGEL NUMBER                   
         LA    R6,24(R6)           FIRST ELEMENT                                
         SR    R4,R4                                                            
*                                                                               
DSPCHGX2 LA    R4,1(R4)            FIRST ELNUM IS 1                             
         STC   R4,SVNXTCHG         SAVE ELNUM FOR NEXT DISPLAY                  
         BRAS  RE,NEXTEL                                                        
         BNE   DSPCHGX4                                                         
         CR    R7,R6                                                            
         BNE   DSPCHGX2                                                         
         B     DSPCHGX6                                                         
*                                                                               
DSPCHGX4 MVI   SVNXTCHG,0          INDICATE NO MORE CHGELS                      
*                                                                               
DSPCHGX6 CLI   0(R2),0             FIND EOS                                     
         BE    *+12                                                             
         LA    R2,ACDNEXT                                                       
         B     DSPCHGX                                                          
*                                                                               
         MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
         BRAS  RE,SETPFK                                                        
*                                                                               
         OI    CONSERVH+1,X'01'     FORCE MODIFIED                              
         OI    CONSERVH+6,X'80'     AND XMT                                     
         J     EXIT                                                             
*                                                                               
GETPRD   L     RF,ASVCLIST                                                      
*                                                                               
GETPRD2  CLC   3(1,RF),0(R1)                                                    
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   GETPRD2                                                          
         LA    RF,=C'***'                                                       
         BR    RE                                                               
*                                                                               
TESTIT   SR    RF,RF                                                            
         IC    RF,0(R4)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    TESTIT2                                                          
         MVC   0(10,R3),1(R4)                                                   
         XC    0(10,R3),=X'00404040404040404040'  MAKE LOWERCASE                
*                                                                               
         LA    R3,11(R3)           PUT A / AFTER LAST CHAR                      
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'/'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
         LA    R0,60(R2)           CAN'T START A DESC AFTER HERE                
         CR    R3,R0                                                            
         BL    TESTIT2                                                          
         BCTR  R3,0                                                             
         MVI   0(R3),C' '          GET RID OF LAST /                            
*                                                                               
         LA    R2,ACDNEXT          CONTINUE ON NEXT LINE                        
         CLI   0(R2),0             TEST REACHED EOS                             
         BE    DSPCHGX             YES - CAN'T DISPLAY ANY MORE                 
         XC    ACDDATA,ACDDATA                                                  
         LA    R3,12(R2)           FIRST OUTPUT POSN                            
*                                                                               
TESTIT2  LA    R4,L'CHGTAB1(R4)                                                 
         BCT   R5,TESTIT                                                        
         BR    RE                                                               
         EJECT                                                                  
*=================================================================              
* BUILD SAVE AREA IN WORK OF USEFUL CURRENT DATA IN RECORD                      
*                                                                               
* WORK(8)     PRODUCT LIST                                                      
* WORK+8(6)   START/END DATES                                                   
* WORK+14(1)  SLN                                                               
* WORK+16(4)  MATCH START/END TIMES                                             
*=================================================================              
                                                                                
GETCUR   NTR1                                                                   
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   WORK+8(6),CMLRLSE   START/END DATES                              
         MVC   WORK+14(1),CMLSLN                                                
         DROP  R6                                                               
*                                                                               
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
*                                                                               
         MVI   ELCODE,X'B0'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING CMLMATEL,R6                                                      
         MVC   WORK+15(4),CMLMSTIM    SAVE START/END TIME                       
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
CHGTAB1  DS    0CL11                                                            
         DC    X'80',CL10'TITLE'                                                
         DC    X'40',CL10'PRDLIST'                                              
         DC    X'20',CL10'START DATE'                                           
         DC    X'10',CL10'END DATE  '                                           
         DC    X'08',CL10'SPOT LEN'                                             
         DC    X'04',CL10'CLASS'                                                
         DC    X'02',CL10'ADID'                                                 
         DC    X'01',CL10'TALENT OPT'                                           
CHGTAB1X EQU   *                                                                
CHGTAB1N EQU   (CHGTAB1X-CHGTAB1)/L'CHGTAB1                                     
*                                                                               
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
*                                                                               
CHGTAB3  DS    0CL11                                                            
         DC    X'80',CL10'PRNT CMML'                                            
         DC    X'40',CL10'HDEF CMML'                                            
         DC    X'20',CL10'CNTR CMML'                                            
         DC    X'10',CL10'PROD HSE '                                            
         DC    X'08',CL10'PRD SUB  '                                            
         DC    X'01',CL10'OTHER'                                                
CHGTAB3X EQU   *                                                                
CHGTAB3N EQU   (CHGTAB3X-CHGTAB3)/L'CHGTAB3                                     
*                                                                               
ACDD     DSECT                     ACTIVITY DISPLAY SCREEN LINE                 
ACDHDR   DS    XL8                 FIELD HEADER                                 
ACDDATA  DS    CL70                                                             
ACDNEXT  EQU   *                                                                
         EJECT                                                                  
*==================================================================             
* SET PFKEYS IN LAST LINE ON SCREEN                                             
*==================================================================             
                                                                                
T21602   CSECT                                                                  
*                                                                               
SETPFK   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,TRAFLTRH         FIND EOS                                     
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *-10                                                             
         SR    R2,R0               BACK UP TO LAST FIELD                        
         LA    R1,8(R2)            SET FIRST OUTPUT POSN                        
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AHI   RE,-9                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         CLI   MYSCREEN,X'DF'      TEST CHGHIST ACTIVE                          
         BNE   SETPFK2                                                          
         BAS   RE,CLRPFK                                                        
         MVC   0(9,R1),=C'PF4=COMML'                                            
         XC    5(4,R1),SPACES      MAKE LOWERCASE                               
         LA    R1,10(R1)                                                        
         CLI   SVNXTCHG,0          TEST ANY MORE CHGELS                         
         BE    SETPFK12                                                         
         MVC   0(9,R1),=C'5=CHGHST+'                                            
         XC    3(5,R1),SPACES                                                   
         LA    R1,10(R1)                                                        
         B     SETPFK12                                                         
*                                                                               
SETPFK2  BRAS  RE,CLRPFK                                                        
         LA    R1,8(R2)                                                         
         MVC   0(2,R1),=C'PF'                                                   
         LA    R1,2(R1)                                                         
                                                                                
*=======================================================                        
* BCOM AND LCOM ONLY FOR GM/H9/SJ                                               
*=======================================================                        
                                                                                
         L     R6,AIO                                                           
         B     SETPFK8             *NOP DISPLAY OF LCOM/BCOM PFKEYS !!!         
*                                                                               
         CLC   =C'SJ=NO',CMLTITLE-CMLRECD(R6)                                   
         BE    *+14                                                             
         CLC   =C'SJ',AGENCY       AGENCY IS SJR                                
         BE    SETPFK4                                                          
         CLC   =C'MC',AGENCY       AGENCY IS GM MCCANN                          
         BE    SETPFK4                                                          
         CLC   =C'H9',AGENCY       OR STARCOM                                   
         BE    SETPFK4                                                          
         B     SETPFK10                                                         
*                                                                               
SETPFK4  CLI   MYSCREEN,X'9F'      TEST DRLEGAL ACTIVE                          
         BE    SETPFK6                                                          
         MVC   0(6,R1),=C'2=LCOM'                                               
         XC    3(3,R1),SPACES      MAKE LOWERCASE                               
         LA    R1,7(R1)                                                         
*                                                                               
SETPFK6  CLI   MYSCREEN,X'6F'      TEST DRBROAD ACTIVE                          
         BE    SETPFK8                                                          
         MVC   0(6,R1),=C'3=BCOM'                                               
         XC    3(3,R1),SPACES                                                   
         LA    R1,7(R1)                                                         
*                                                                               
SETPFK8  CLI   MYSCREEN,X'F2'      TEST COMML SCREEN LOADED                     
         BE    SETPFK10                                                         
         MVC   0(7,R1),=C'4=COMML'                                              
         XC    3(4,R1),SPACES                                                   
         LA    R1,8(R1)                                                         
*                                                                               
SETPFK10 MVC   0(9,R1),=C'5=CHGHIST'                                            
         XC    3(6,R1),SPACES                                                   
*                                                                               
         LA    R1,10(R1)                                                        
SETPFK12 MVC   0(9,R1),=C'6=COMTEXT'                                            
         XC    3(6,R1),SPACES                                                   
*                                                                               
SETPFK15 DS    0H                                                               
         CLI   SPOTCAN,C'C'        THIS CANADA                                  
         BNE   SETPFK20                                                         
         LA    R1,10(R1)                                                        
         MVC   0(12,R1),=C'7=TELECASTER'                                        
         XC    3(9,R1),SPACES                                                   
         LA    R1,13(R1)                                                        
*                                                                               
SETPFK20 DS    0H                                                               
**NOP**  CLI   SVCXTRA8,C'P'       TEST P&G                                     
         CLI   SPOTCAN,C'C'        THIS CANADA                                  
         BNE   SETPFKX                                                          
         MVC   0(12,R1),=C'8=SUBSTITUTE'                                        
         XC    3(9,R1),SPACES                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'B1'        SEE IF ANY B1 (PRDSUB) ELS                   
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   12(R1),C'!'         SET IND                                      
*                                                                               
SETPFKX  OI    6(R2),X'80'         TRANSMIT                                     
         NI    1(R2),X'FF'-X'04'   CHANGE INTENSITY                             
         J     EXIT                                                             
*                                                                               
CLRPFK   SR    RF,RF               CLEAR PFKEY FIELD                            
         IC    RF,0(R2)                                                         
         AHI   RF,-9                                                            
         EX    RF,*+6                                                           
         BR    RE                                                               
         XC    8(0,R2),8(R2)                                                    
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* INCLUDE THAT IS NOP BELOW WAS FOR STARCOM BROADCAST AND LEGAL CODE            
* THIS SEEMED THE BEST WAY NOT TO LOSE IT - BY MAKING IT ITS OWN BOOK           
*===================================================================            
* ++INCLUDE SPTRA02H9                                                           
         EJECT                                                                  
*=================================================================              
* DOWNLOAD COMMERCIAL DATA                                                      
*=================================================================              
                                                                                
DL       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    MIDHOOK,MIDHOOK                                                  
         XC    FOOTHOOK,FOOTHOOK                                                
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
*                                                                               
         L     R0,AIO3                                                          
         LHI   R1,CDDATAX-CDDATA                                                
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24               CLEAR TO SPACES                              
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO3             POINT TO BUFFER                              
         USING CDDATA,R2                                                        
*                                                                               
         MVC   CDITM1,=CL4'ITM1'                                                
         MVC   CDACTN,SPACES                                                    
         MVC   CDMED,QMED           MEDIA                                       
         MVC   CDCLT,QCLT           CLIENT                                      
*                                                                               
         L     R6,AIO                                                           
         TM    15(R6),X'01'        TEST CMML PACKED IN RECORD                   
         BO    DL01                                                             
*                                                                               
         MVC   CDISCI,5(R6)                                                     
         MVC   CDISCI+8(4),SPACES                                               
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   CDADID,2(R6)        MOVE ADID                                    
         B     DL02                                                             
*                                                                               
DL01     GOTO1 VTRPACK,DMCB,(C'U',5(R6)),CDISCI                                 
*                                                                               
DL02     L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   CDTITLE1,CMLTITLE                                                
*                                                                               
         LLC   R0,CMLSLN                                                        
         BAS   RE,DLCVD                                                         
         UNPK  CDSLN,DUB                                                        
*                                                                               
         MVC   CDPIGGY,CMLSOLO                                                  
         CLI   CDPIGGY,C' '                                                     
         BNL   *+8                                                              
         MVI   CDPIGGY,C' '                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,CDPERIOD)                             
         MVC   CDPERIOD+8(4),=C'-UFN'                                           
         CLC   CMLRCL,=X'FFFFFF'                                                
         BE    DL04                                                             
         GOTO1 (RF),(R1),(3,CMLRCL),(5,CDPERIOD+9)                              
*                                                                               
DL04     MVC   CDTYPE,CMLTYPE                                                   
         MVC   CDCLTCML,CMLCLTNO                                                
         MVC   CDHOUSE,CMLPROD       PRODUCTION HOUSE                           
         MVC   CDCLASS,CMLCLASS                                                 
*                                                                               
         LLC   R0,CMLOVRD1                                                      
         LTR   R0,R0                                                            
         BZ    DL04A                                                            
         BAS   RE,DLCVD                                                         
         UNPK  CDSLNOV1,DUB                                                     
*                                                                               
DL04A    IC    R0,CMLOVRD2                                                      
         LTR   R0,R0                                                            
         BZ    DL04B                                                            
         BAS   RE,DLCVD                                                         
         UNPK  CDSLNOV2,DUB                                                     
*                                                                               
DL04B    L     R6,AIO              PRODUCT LIST ELEMENT                         
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLPRDEL,R6                                                      
         LLC   R0,1(R6)                                                         
         AHI   R0,-2               SET FOR NUMBER OF PRDS                       
         LA    R1,2(R6)            POINT TO FIRST PRD                           
         LA    R3,CDPRD1                                                        
         MVC   0(3,R3),=C'ALL'                                                  
         CLI   0(R1),X'FF'                                                      
         BE    DL12                                                             
*                                                                               
DL10     BAS   RE,DLGETPRD                                                      
         LA    R1,1(R1)                                                         
         LA    R3,3(R3)                                                         
         BCT   R0,DL10                                                          
         B     DL12                                                             
*                                                                               
DLGETPRD L     RF,ASVCLIST                                                      
*                                                                               
DLGETPR2 CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,=C'***'                                                       
         B     DLGETPRX                                                         
         CLC   0(1,R1),3(RF)                                                    
         BE    DLGETPRX                                                         
         LA    RF,4(RF)                                                         
         B     DLGETPR2                                                         
*                                                                               
DLGETPRX MVC   0(3,R3),0(RF)                                                    
         BR    RE                                                               
         EJECT                                                                  
DL12     L     R6,AIO                                                           
         MVI   ELCODE,X'21'        CMML CLASS ELEM                              
         BRAS  RE,GETEL                                                         
         BNE   DL14                                                             
         USING CMLCLSEL,R6                                                      
         MVC   CDCLASS,CMLCLS                                                   
*                                                                               
DL14     L     R6,AIO                                                           
         MVI   ELCODE,X'24'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DL20                                                             
         USING CMLXDTEL,R6                                                      
         CLI   CMLXPRHS,C' '                                                    
         BNH   *+10                                                             
         MVC   CDHOUSE,CMLXPRHS                                                 
         MVC   CDHIDEF,CMLXHDEF                                                 
         MVC   CDCENTER,CMLXCNTR                                                
         MVC   CDPARENT,CMLXPRNT                                                
         MVC   CDHDCCSW,CMLXSWAP                                                
*                                                                               
         CLI   CMLXDSDT,0          TEST HAVE DESTROY DATE                       
         BE    DL20                                                             
         GOTO1 DATCON,DMCB,(3,CMLXDSDT),(5,CDDSTRDT)                            
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET ADDRESS OF UNTIME                 
         L     RF,0(R1)                                                         
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),CMLXDSTM                                                 
         GOTO1 (RF),DMCB,WORK,CDDSTRTM                                          
*                                                                               
DL20     L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DL30                                                             
         MVC   CDTITLE2,3(R6)                                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DL30                                                             
         MVC   CDTITLE3,3(R6)                                                   
*                                                                               
DL30     LA    R4,CDACTL1                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'        ACTUAL CMML ELEMENTS                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DL32     BRAS  RE,NEXTEL                                                        
         BNE   DL40                                                             
         LLC   RE,1(R6)                                                         
         AHI   RE,-3                                                            
         EX    RE,*+8              MOVE 8-12 CHARS OF CMML                      
         B     *+10                                                             
         MVC   0(0,R4),2(R6)                                                    
         LA    R4,12(R4)                                                        
         B     DL32                                                             
*                                                                               
DL40     L     R6,AIO                                                           
         MVI   ELCODE,X'B0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DL70                                                             
*                                                                               
         USING CMLMATEL,R6                                                      
*                                                                               
         CLC   CMLMSTIM(4),=X'0000FFFF' TEST NO TIMES IN RECORD                 
         BZ    DL70                                                             
         MVI   CDTIMDLY,C'N'                                                    
         TM    CMLMFLAG,X'80'                                                   
         BZ    *+8                                                              
         MVI   CDTIMDLY,C'Y'                                                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET ADDRESS OF UNTIME                 
         L     RF,0(R1)                                                         
         XC    WORK(4),WORK                                                     
         OC    CMLMSTIM,CMLMSTIM   ANY START TIME                               
         BZ    DL40B                                                            
         MVC   WORK(2),CMLMSTIM                                                 
         GOTO1 (RF),DMCB,WORK,CDSTTIM                                           
*                                                                               
DL40B    CLC   =X'FFFF',CMLMETIM   ANY END TIME                                 
         BE    DL40D                                                            
         MVC   WORK(2),CMLMETIM                                                 
         GOTO1 (RF),DMCB,WORK,CDENDTIM                                          
*                                                                               
DL40D    LA    R4,CDMATCH1                                                      
         LA    R5,CMLMPER1                                                      
         LA    R0,6                                                             
*                                                                               
DL42     OC    0(2,R5),0(R5)                                                    
         BZ    DL44                                                             
         GOTO1 DATCON,DMCB,(2,0(R5)),(5,(R4))                                   
         MVI   8(R4),C'-'                                                       
         GOTO1 (RF),(R1),(2,2(R5)),(5,9(R4))                                    
*                                                                               
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R0,DL42                                                          
*                                                                               
DL44     B     DL70                                                             
*                                                                               
DLCVD    CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BR    RE                                                               
         EJECT                                                                  
*===============================================================                
* ALL FIELDS NOW IN OUTPUT BUFFERS                                              
* CALL DLFLD FOR EACH DATA FIELD                                                
*===============================================================                
                                                                                
DL70     LA    R6,DOWNTAB                                                       
*                                                                               
DL72     MVI   D.DLCBACT,DLCBPUT     ACTION IS PUT                              
         MVI   D.DLCBTYP,DLCBTXT     TYPE IS TEXT                               
         OI    D.DLCBFLG1,DLCBFXFL   USE EXTENDED FOR TEX                       
*                                                                               
         L     RE,AIO3               BUFFER 3                                   
         AH    RE,0(R6)              ADD DSPL TO BUFFER START                   
*                                                                               
         LLC   RF,2(R6)              GET DATA LEN                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLX(0),0(RE)                                               
*                                                                               
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         LA    R6,3(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   DL72                                                             
*                                                                               
         MVI   D.DLCBACT,DLCBEOL          END OF LINE                           
         GOTO1 VDLFLD,DLCB                                                      
         J     EXIT                                                             
DLHOOK   NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*DOWNTAB'                                                    
*                                                                               
DOWNTAB  DS    0D                                                               
         DC    AL2(CDITM1-CDDATA),AL1(L'CDITM1)                                 
         DC    AL2(CDMED-CDDATA),AL1(L'CDMED)                                   
         DC    AL2(CDCLT-CDDATA),AL1(L'CDCLT)                                   
         DC    AL2(CDISCI-CDDATA),AL1(L'CDISCI)                                 
         DC    AL2(CDADID-CDDATA),AL1(L'CDADID)                                 
         DC    AL2(CDPRD1-CDDATA),AL1(L'CDPRD1)                                 
         DC    AL2(CDPRD2-CDDATA),AL1(L'CDPRD2)                                 
         DC    AL2(CDPRD3-CDDATA),AL1(L'CDPRD3)                                 
         DC    AL2(CDPRD4-CDDATA),AL1(L'CDPRD4)                                 
         DC    AL2(CDPRD5-CDDATA),AL1(L'CDPRD5)                                 
         DC    AL2(CDPRD6-CDDATA),AL1(L'CDPRD6)                                 
         DC    AL2(CDPRD7-CDDATA),AL1(L'CDPRD7)                                 
         DC    AL2(CDPRD8-CDDATA),AL1(L'CDPRD8)                                 
         DC    AL2(CDPRD9-CDDATA),AL1(L'CDPRD9)                                 
         DC    AL2(CDTITLE1-CDDATA),AL1(L'CDTITLE1)                             
         DC    AL2(CDTITLE2-CDDATA),AL1(L'CDTITLE2)                             
         DC    AL2(CDTITLE3-CDDATA),AL1(L'CDTITLE3)                             
         DC    AL2(CDSLN-CDDATA),AL1(L'CDSLN)                                   
         DC    AL2(CDSLNOV1-CDDATA),AL1(L'CDSLNOV1)                             
         DC    AL2(CDSLNOV2-CDDATA),AL1(L'CDSLNOV2)                             
         DC    AL2(CDPERIOD-CDDATA),AL1(L'CDPERIOD)                             
         DC    AL2(CDSTTIM-CDDATA),AL1(L'CDSTTIM)                               
         DC    AL2(CDENDTIM-CDDATA),AL1(L'CDENDTIM)                             
         DC    AL2(CDTIMDLY-CDDATA),AL1(L'CDTIMDLY)                             
         DC    AL2(CDMATCH1-CDDATA),AL1(L'CDMATCH1)                             
         DC    AL2(CDMATCH2-CDDATA),AL1(L'CDMATCH2)                             
         DC    AL2(CDMATCH3-CDDATA),AL1(L'CDMATCH3)                             
         DC    AL2(CDMATCH4-CDDATA),AL1(L'CDMATCH4)                             
         DC    AL2(CDMATCH5-CDDATA),AL1(L'CDMATCH5)                             
         DC    AL2(CDMATCH6-CDDATA),AL1(L'CDMATCH6)                             
         DC    AL2(CDTYPE-CDDATA),AL1(L'CDTYPE)                                 
         DC    AL2(CDCLTCML-CDDATA),AL1(L'CDCLTCML)                             
         DC    AL2(CDPIGGY-CDDATA),AL1(L'CDPIGGY)                               
         DC    AL2(CDHIDEF-CDDATA),AL1(L'CDHIDEF)                               
         DC    AL2(CDTALXFR-CDDATA),AL1(L'CDTALXFR)                             
         DC    AL2(CDPARENT-CDDATA),AL1(L'CDPARENT)                             
         DC    AL2(CDCENTER-CDDATA),AL1(L'CDCENTER)                             
         DC    AL2(CDCLASS-CDDATA),AL1(L'CDCLASS)                               
         DC    AL2(CDHOUSE-CDDATA),AL1(L'CDHOUSE)                               
         DC    AL2(CDDSTRDT-CDDATA),AL1(L'CDDSTRDT)                             
         DC    AL2(CDDSTRTM-CDDATA),AL1(L'CDDSTRTM)                             
         DC    AL2(CDHDCCSW-CDDATA),AL1(L'CDHDCCSW)                             
         DC    AL2(CDACTL1-CDDATA),AL1(L'CDACTL1)                               
         DC    AL2(CDACTL2-CDDATA),AL1(L'CDACTL2)                               
         DC    AL2(CDACTL3-CDDATA),AL1(L'CDACTL3)                               
         DC    AL2(CDACTL4-CDDATA),AL1(L'CDACTL4)                               
         DC    X'FFFF'                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMLCLS                                                     
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
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
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF2D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA9FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA6FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB2D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF6D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE DMDTFIS                                                        
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR02RR DS    F                                                                
VDLFLD   DS    A                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
PRDPTR   DS    F                                                                
CMLPRCNT DS    H                                                                
RECCT    DS    H                                                                
TELNO    DS    CL8                 TELECASTER NO                                
OTELNO   DS    CL8                 PREVIOUS TELECASTER NO                       
                                                                                
ADIDNOP  DS    XL8                 AD-ID NO PACKED                              
ADIDYTM  DS    XL6                 DATE/TIME YMDHMS                             
ADIDFLT  DS    CL12                AD-ID FILTER                                 
                                                                                
SVDSKAD  DS    CL4                                                              
SVDADRA  DS    CL4                                                              
PRDCTR   DS    CL1                                                              
SCANCT   DS    XL1                                                              
DATE     DS    CL6                                                              
SVTODAY  DS    XL3                 SAVE TODAY'S DATE                            
HOLDSEQ  DS    XL3                                                              
HOLDCML  DS    XL8                 8-BYTE COMMERCIAL                            
*                                                                               
*                                  CONTROLED BY T2 PROFILE 7 (SVT2PR7)          
CODESEQ  DS    XL3                 NEW AUTO NUMBER COMML GENERATOR              
COMPKEY  DS    CL13                COMPARE KEY FOR ONLINE LIST                  
COMPKEYL DS    CL1                                                              
SVATOAIR DS    CL1                 APPROVE TO AIR                               
                                                                                
CMLFILCT DS    PL3                                                              
CMLPBCT  DS    PL3                                                              
*                                                                               
DSTRYDAT DS    XL3                                                              
DSTRYTIM DS    XL2                                                              
SVCMLSTR DS    XL3                 SAVE CML START DATE                          
SVDSKADR DS    XL4                 SAVE DISK ADDR OF COMML REC                  
SVHIDEF  DS    CL12                ORIGINAL HIDEF CHAR                          
SVHIDEFX DS    XL8                 ORIGINAL HIDEF HEX                           
NWHIDEFX DS    XL8                 NEW HIDEF HEX                                
SVCNTCT  DS    CL12                ORIGINAL CENTERCUT CHAR                      
SVCNTCTX DS    XL8                 ORIGINAL CENTERCUT HEX                       
NWCNTCTX DS    XL8                 NEW CENTERCUT HEX                            
SVSWAP   DS    CL1                 HIDEF/CENTERCUP SWAP INDICATOR               
*                                                                               
SVF2ACT  DS    CL3                                                              
SVB2ACT  DS    CL3                                                              
*                                                                               
SVPPKEY  DS    0C                                                               
SVHDDELK DS    XL13                                                             
SVHDADDK DS    XL13                                                             
SVCCDELK DS    XL13                                                             
SVCCADDK DS    XL13                                                             
*SVADADDK DS    XL13                                                            
SVPPKEYL EQU   *-SVPPKEY                                                        
SVCLTINT DS    CL2                 SAVED CCLTIFC FROM CLIENT HEADER             
*                                  FOR STARCOM FILE                             
TEMPADID DS    CL1                                                              
*                                  FOR STARCOM FILE                             
SVNEXT   DS    F                                                                
                                                                                
FILTERS  DS    0CL(HOLDSIGN+1-PRODFTR)                                          
PRODFTR  DS    CL3                 PRODFTR MUST BE FOLLOWED BY PRDFTR           
PRDFTR   DS    XL1                                                              
TYPEFTR  DS    CL4                                                              
SLNFTR   DS    CL1                                                              
RLDTFTR  DS    CL3                 RELEASE DATE FILTER                          
RLDTSFTR DS    CL1                                                              
RCDTFTR  DS    CL3                 RECALL DATE FILTER                           
RCDTSFTR DS    CL1                                                              
CLSFTR   DS    CL4                                                              
FTRFLAG  DS    XL1                                                              
DELFTR   EQU   X'80'               SHOW ONLY DELETED CML'S                      
EASIFTR  EQU   X'40'               FILTER ON EASI COMMLS                        
TELEFTR  EQU   X'20'               FILTER ON COMMLS WITH TELECASTER             
SEQFTR   EQU   X'10'               SHOW COMML SEQ NO                            
*FILFTR   EQU   X'08'  **REUSE**   CREATE FILE FOR STARCOM                      
ADIDFTR  EQU   X'04'               ONLY SHOW COMMLS WITH AD-ID                  
HDEFFTR  EQU   X'02'               SHOW HIDEF CMML OVER TITLE                   
                                                                                
HOLDSIGN DS    CL1                                                              
                                                                                
REQHDR   DS    CL26                REQUEST HEADER FOR TURNAROUND REPORT         
REQUEST  DS    CL80                                                             
SVPRDLST DS    CL256                                                            
NEEDADD  DS    XL1                 DO AN ADD INSTEAD OF A CHANGE                
*                                                                               
ELCODE2  DS    XL1                                                              
CHGELNUM EQU   ELCODE2                                                          
FLAGS    DS    X                   VARIOUS FLAGS                                
SOFTDEL  EQU   X'80'               JUST DID A SOFT DELETE                       
SOFTREST EQU   X'40'               JUST DID A SOFT RESTORE                      
ISCOVCML EQU   X'20'               THIS IS A COVER CMML                         
ISCOVERD EQU   X'10'               THIS CMML IS COVERED                         
UNCOVER  EQU   X'08'               ACTION UNCOVER                               
MASTERCP EQU   X'04'               MASTER RECORD, DO COPY (BOZELL)              
CONTINUE EQU   X'02'               CONTINUE PROCESSING                          
DELTELP  EQU   X'01'               FOUND DELETED 0AE1 TELECASTER #              
*                                                                               
FLAGS2   DS    X                   VARIOUS FLAGS                                
ADIDELP  EQU   X'80'               FOUND DELETED 0AC1 AD-ID NO                  
MATELEM  EQU   X'40'               ADDING MATCH ELEMENT (X'B0')                 
ALLCLTR  EQU   X'20'               ALL CLIENT REQUEST                           
*        EQU   X'10'                                                            
*        EQU   X'08'                                                            
RDCMLP   EQU   X'04'               READING PADDED CML                           
PADCML   EQU   X'02'               PADDED CML (W/AAA...) FOR LIST               
PACKCML  EQU   X'01'               PACKED CML ON KEY                            
*                                                                               
SVTTPR3  DS    CL1                 TALENT TRANSFER USER (TT PROFILE)            
SVT2PR6  DS    CL1                 TALENT TRANSFER USER                         
SVT2PR7  DS    CL1                 AUTO NUMBER CMML CODES BY AGY                
SVT2PR8  DS    CL1                 LIMIT ONE PRD PER ISCII                      
SVT2PR9  DS    CL1                 BRAND AGENCY (Y/N)                           
SVCXTRA8 DS    CL1                                                              
*                                                                               
ACTCT    DS    X                   COUNT OF ACTUAL CMMLS USED                   
ACTSLN   DS    CL1                                                              
OLDACTS  DS    (NUMACTS)CL12                                                    
NEWACTS  DS    (NUMACTS)CL12                                                    
NUMACTS  EQU   4                   NUMBER OF ACTUALS ALLOWED                    
CHGFLAGS DS    0XL3                                                             
CHGFLAG1 DS    XL1                                                              
CHGFLAG2 DS    XL1                                                              
CHGFLAG3 DS    XL1                                                              
CHKCMML  DS    CL1                                                              
         DS    0D                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
                                                                                
TMPPSSV  DS    XL8                 SAVE HIDEF/CENTERCUT TO BE ADDED             
TMPKTYP  DS    XL2                 PASSIVE KEY TYPE TO BE ADDED                 
CMLDSKAD DS    XL4                                                              
VTRPACK  DS    A                                                                
                                                                                
MYKEY    DS    CL24                                                             
MYKEY2   DS    CL24                                                             
         DS    0D                                                               
MYIO     DS    CL4000                                                           
         ORG   MYIO                                                             
DLCB     DS    XL256                                                            
         ORG                                                                    
         EJECT                                                                  
* OFFLINE REPORT                                                                
                                                                                
PRTLINE  DSECT                                                                  
         DS    CL2                                                              
PCML     DS    CL12                                                             
         DS    CL2                                                              
PSLN     DS    CL6                                                              
         DS    CL1                                                              
PTITLE   DS    CL15                                                             
         DS    CL1                                                              
PSOLO    DS    CL1                                                              
         DS    CL1                                                              
PRELSE   DS    CL8                                                              
         DS    CL1                                                              
PRECALL  DS    CL8                                                              
         DS    CL2                                                              
PTYPE    DS    CL4                                                              
         DS    CL1                                                              
PLIST    DS    CL24                                                             
         DS    CL1                                                              
PMISC    DS    CL26                                                             
                                                                                
* ONLINE LIST                                                                   
                                                                                
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
         DS    CL3                                                              
LCLASS   DS    CL4                                                              
         DS    CL2                                                              
LCOMTXT  DS    CL8                                                              
         EJECT                                                                  
* DUMP FILE TABLE DSECT                                                         
                                                                                
FILTSECT DSECT                                                                  
FILTFROM DS    A                   FIELD DISP FROM START OF EL OR KEY           
FILTTO   DS    A                   DISP OF OUTPUT IN RECORD                     
FILTRTN  DS    A                   ADDRESS OF ROUTINE TO MOVE DATA              
FILTEL   DS    X                   ELEM WITH DATA (ZERO = KEY)                  
FILFRLN  DS    X                   LENGTH OF FROM DATA-1                        
FILTOLN  DS    X                   LENGTH OF TO FIELD                           
         DS    X                   SPARE                                        
FILTNXT  EQU   *                                                                
                                                                                
* OFFLINE DUMP TO FILE                                                          
                                                                                
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
*                                                                               
       ++INCLUDE DDDLCB                                                         
*                                                                               
CDDATA   DSECT                                                                  
CDITM1   DS    CL4                                                              
CDACTN   DS    CL3                                                              
CDMED    DS    CL1                                                              
CDCLT    DS    CL3                                                              
CDISCI   DS    CL12                                                             
CDADID   DS    CL12                                                             
CDPRD1   DS    CL3                                                              
CDPRD2   DS    CL3                                                              
CDPRD3   DS    CL3                                                              
CDPRD4   DS    CL3                                                              
CDPRD5   DS    CL3                                                              
CDPRD6   DS    CL3                                                              
CDPRD7   DS    CL3                                                              
CDPRD8   DS    CL3                                                              
CDPRD9   DS    CL3                                                              
CDTITLE1 DS    CL15                                                             
CDTITLE2 DS    CL20                                                             
CDTITLE3 DS    CL20                                                             
CDSLN    DS    CL3                                                              
CDSLNOV1 DS    CL3                                                              
CDSLNOV2 DS    CL3                                                              
CDPERIOD DS    CL17                                                             
CDSTTIM  DS    CL5                                                              
CDENDTIM DS    CL5                                                              
CDTIMDLY DS    CL1                                                              
CDMATCH1 DS    CL17                                                             
CDMATCH2 DS    CL17                                                             
CDMATCH3 DS    CL17                                                             
CDMATCH4 DS    CL17                                                             
CDMATCH5 DS    CL17                                                             
CDMATCH6 DS    CL17                                                             
CDTYPE   DS    CL3                                                              
CDCLTCML DS    CL20                                                             
CDPIGGY  DS    CL1                                                              
CDHIDEF  DS    CL12                                                             
CDTALXFR DS    CL1                                                              
CDPARENT DS    CL12                                                             
CDCENTER DS    CL12                                                             
CDCLASS  DS    CL4                                                              
CDHOUSE  DS    CL6                                                              
CDDSTRDT DS    CL8                                                              
CDDSTRTM DS    CL5                                                              
CDHDCCSW DS    CL1                 HIDEF/CTRCUT SWAP                            
CDACTL1  DS    CL12                                                             
CDACTL2  DS    CL12                                                             
CDACTL3  DS    CL12                                                             
CDACTL4  DS    CL12                                                             
CDDATAX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127SPTRA02   05/12/20'                                      
         END                                                                    
