*          DATA SET SPTRA53    AT LEVEL 016 AS OF 10/13/09                      
*PHASE T21653B                                                                  
         TITLE 'T21653 COMMENT MAINT AND LIST'                                  
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - USED TO READ CLIENT RECS IN OFFLINE LIST                   
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
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* MOD LOG:                                                            *         
* --------                                                            *         
*                                                                     *         
* LEV WHO  WHEN    WHAT                                               *         
* --- ---  ----    ----                                               *         
*                                                                     *         
* 01  EJOR 05JAN93 ORIGINAL DEVELOPMENT                               *         
* 02  EJOR 08FEB93 FIX DUMP - MAKE SURE KEY HAS DTX KEY BEFORE FCLT   *         
* 03  EJOR 10FEB93 TURN OFF PRE-VALID BITS ON DK                      *         
* 04  EJOR 17FEB93 CLIENT '*' ONLY ALLOWED FOR ACTN LIST              *         
*                - ALWAYS VALIDATE PAGE                               *         
* 05  EJOR 25FEB93 DON'T SET PRE-VAL CLIENT FOR CLT '*'               *         
* 06  BGRI 14DEC98 FIX FOR NO RECS FOR CLT SPECIFIC REQUEST           *         
* LEV 07 BGRI MAY07/01 CHANGE DUMMY                                   *         
* LEV 08 SMUR JUN21/02 FIX LR WHEN NO RECORDS FOUND FOR THIS CLT      *         
* LEV 09 SMUR JUN28/02 CLIENT STRING SECURITY                         *         
* LEV 10 BGRI OCT20/03 ADD 253 PRODUCTS                               *         
* LEV 11 SMUR NOV17/03 BRAND LEVEL SECURITY                           *         
* LEV 12 SMUR JUL26/04 SOX                                            *         
* LEV 13 MNAS MORE BRANDS CONVERSION                                  *         
* LEV 14 FIX RECORD DELETION FROM LIST/SELECT                         *         
* LEV 14 NOV10/06 FIX ERROR MESSAGE SET FOR TESTING PURPOSES BACK TO  *         
*                 ORIGINAL - CHANGE SECLOCK TO NOTFOUND               *         
* LEV 15 MNAS JUL07/08 NEW TRAFFIC TYPE ON MASTER RECORD              *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21653   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21653**,R7,RR=R5                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R5,RELO                                                          
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   AHELLO,CHELLO                                                    
         DROP  R1                                                               
         MVC   AIO,AIO1            DEFAULT IO AREA                              
         MVI   IOOPT,C'Y'                                                       
         CLI   ACTNUM,ACTSEL                                                    
         BE    SFSET                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    SFSET                                                            
         B     *+8                                                              
SFSET    BRAS  RE,SF                                                            
                                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
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
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   EXIT                                                             
                                                                                
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    DEL                                                              
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    DEL                                                              
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        VALIDATE KEY ROUTINE                                                   
*-------------------------------------------------------------------*           
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
                                                                                
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
VK02     DS    0H                                                               
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM+5,1                                                         
         MVI   ELEM+8,C'N'         FAKE OUT VALIMED                             
         GOTO1 VALIMED                                                          
                                                                                
* VALIDATE CLIENT                                                               
                                                                                
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         XC    SVBCLT,SVBCLT                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+20                 NO                                          
         CLI   8(R2),C'*'          LIST ALL CLTS?                               
         BNE   *+12                 NO                                          
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    VK30                 YES - BUT DON'T SET PRE-VAL BIT             
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                 YES                                         
                                                                                
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK07                 NO                                          
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
                                                                                
         MVI   ERROR,0                                                          
                                                                                
VK07     CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   VK10                 NO                                          
         TM    WHEN,X'C0'          NOW?                                         
         BZ    VK20                 NO                                          
         MVC   GERROR,=Y(MISCLTL)                                               
         B     TRAPERR2                                                         
                                                                                
VK10     GOTO1 VALICLT                                                          
                                                                                
         CLI   ERROR,0             ANY ERROR                                    
         BE    VK15                                                             
                                                                                
         CLI   ACTNUM,ACTLIST      OK TO LIST IT                                
         BE    VK15                                                             
                                                                                
         CLI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         BE    *+6                                                              
         DC    H'0'                OOPS, CHECK VCLI ROUTINE                     
                                                                                
         TM    SECFLAG,BLSSW                                                    
         BZ    TRAPERR             SECURITY LOCKOUT MESSAGE                     
                                                                                
                                                                                
VK15     MVC   SVBCLT,BCLT                                                      
                                                                                
VK20     OI    4(R2),X'20'         SET PRE-VALID                                
                                                                                
* VALIDATE NETWORK                                                              
                                                                                
VK30     LA    R2,TRANETH          NETWORK                                      
         BAS   RE,VNET             VALIDATE NETWORK                             
         OI    4(R2),X'20'         SET PRE-VALID                                
                                                                                
* VALIDATE PRODUCT                                                              
                                                                                
VK40     LA    R2,TRAPRODH                                                      
         XC    PRODUCT,PRODUCT                                                  
         XC    BPROD,BPROD                                                      
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VK60                 NO                                          
*                                                                               
         OC    SVBCLT,SVBCLT       CLIENT REQD FOR FILTER BY PRD                
         BNZ   *+14                                                             
         MVC   GERROR,=Y(MISCLTP)                                               
         B     TRAPERR2                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK50                MEDIA OR NET NOT REQD FOR LIST               
         OC    MEDIA,MEDIA         MEDIA OR NETWORK REQD FOR PRD                
         BNZ   VK50                                                             
         OC    NETWORK,NETWORK                                                  
         BNZ   VK50                                                             
         MVC   GERROR,=Y(NETMDRQD)                                              
         B     TRAPERR2                                                         
*                                                                               
VK50     MVC   BCLT,SVBCLT                                                      
         GOTO1 VALIPRD                                                          
                                                                                
VK55     MVC   PRODUCT,WORK                                                     
         MVC   BPROD,WORK+3                                                     
         OI    4(R2),X'20'         SET PRE-VALID                                
                                                                                
* VALIDATE PAGE                                                                 
                                                                                
VK60     LA    R2,TRAPGH           VALIDATE PAGE                                
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK70                 YES                                         
         MVI   PAGNO,X'01'         SET TO PAGE 1                                
         MVI   TRAPG,C'1'                                                       
         OI    TRAPGH+6,X'80'                                                   
         B     *+8                                                              
VK70     BAS   RE,VPG              GO VALIDATE PAGE NUMBER                      
                                                                                
VK80     LA    R4,NEWKEY                                                        
         XC    NEWKEY(L'DT2KEY),NEWKEY                                          
         USING DT2KEY,R4                                                        
         MVC   DT2KID,=XL2'0A2D'                                                
         MVC   DT2KAM,BAGYMD                                                    
         MVC   DT2KCLT,SVBCLT      MOVE IN CLIENT                               
         MVC   DT2KMED,MEDIA                                                    
         MVC   DT2KNET,NETWORK                                                  
         MVC   DT2KPRD,PRODUCT                                                  
         MVC   DT2KPG,PAGNO                                                     
         MVC   KEY,NEWKEY                                                       
                                                                                
*------------------------------------------------------                         
*        ACTION ADD                                                             
*------------------------------------------------------                         
         MVC   AIO,AIO1                                                         
         CLI   ACTNUM,ACTADD            FIRST SEE IF ACTIVE RECORD              
         BNE   VK80G                    EXISTS                                  
                                                                                
VK80C    DS    0H                                                               
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+PASSDELQ,0                               
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    VK80D                                                            
         MVC   KEY,KEYSAVE           NEED THIS TO GET PAST ERROR MSG            
         B     EXIT                                                             
                                                                                
VK80D    DS    0H                                                               
         MVI   ERROR,49              RECORD ALREADY EXISTS                      
         TM    KEY+32,X'80'                                                     
         BZ    *+8                                                              
         MVI   ERROR,58              DELETED RECORD ALREADY EXISTS              
         LA    R2,TRACLTH                                                       
         B     TRAPERR                                                          
*------------------------------------------------------                         
*        ACTION RESTORE                                                         
*------------------------------------------------------                         
VK80G    CLI   ACTNUM,ACTREST           IF RESTORING MUST DO ALL WORK           
         BNE   VK80M                    IN VALKEKY                              
                                                                                
         MVC   KEY,NEWKEY                                                       
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+PASSDELQ,0                               
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    VK80H                                                            
         LA    R2,TRACLTH               RECORD DOES NOT EXIST                   
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
                                                                                
VK80H    LA    R2,TRACLTH                                                       
         MVI   ERROR,49                                                         
         TM    KEY+32,X'80'                                                     
         BZ    TRAPERR                                                          
         MVI   ERROR,0                                                          
                                                                                
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ+PASSDELQ,AIO1                        
         NI    KEY+32,X'FF'-X'80'                                               
         GOTO1 AIOCALL,DMCB,WRITEQ+DIRQ+PASSDELQ,0                              
         B     VK90F                                                            
                                                                                
*------------------------------------------------------                         
*        ACTION DIS/CHA/DEL                                                     
*------------------------------------------------------                         
VK80M    GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    VK85                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK90F                                                            
         LA    R2,TRACLTH               RECORD DOES NOT EXIST                   
         MVI   ERROR,53                                                         
         B     TRAPERR                                                          
                                                                                
VK85     EQU   *                                                                
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
                                                                                
* TO FIX VALKEY PROBLEM I NEED TO BUILD A KEY OF A RECORD THAT ALWAYS           
* EXISTS                                                                        
         TM    SECFLAG,NECONPRD+NEMORPRD                                        
         BZ    VK95                                                             
         CLI   ACTIVSW,1                                                        
         BNE   VK87                                                             
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=CL3'XSP'                                              
         MVC   SYSFIL(3),=CL3'XSP'                                              
         MVC   KEY,NEWKEY                                                       
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+PASSDELQ,0                               
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ+PASSDELQ,AIO1                        
         B     EXIT                                                             
                                                                                
VK87     DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   VK90F                                                            
         MVC   KEY,NEWKEY                                                       
         B     EXIT                                                             
                                                                                
VK90F    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
                                                                                
         MVI   DATADISP+1,24                                                    
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         MVC   SYSDIR(3),=CL3'SPT'                                              
         MVC   SYSFIL(3),=CL3'SPT'                                              
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*-------------------------------------------------------*                       
VK91     DS    0H                                                               
         TM    SECFLAG,NEMORPRD     BOTH                                        
         BO    VK92                 YES                                         
         TM    SECFLAG,NECONPRD     COMPLETELY CONVERTED                        
         BZ    VK95                 YES                                         
VK92     CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=CL3'XSP'                                              
         MVC   SYSFIL(3),=CL3'XSP'                                              
         B     EXIT                                                             
                                                                                
VK95     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING DTOKEY,R4                                                        
         MVC   DTOKID,=XL2'0A2D'                                                
         MVC   DTOKAM,BAGYMD                                                    
         MVC   DTOKCLT,SVBCLT      MOVE IN CLIENT                               
         MVC   DTOKMED,MEDIA                                                    
         MVC   DTOKNET,NETWORK                                                  
         MVC   DTOKPRD,BPROD                                                    
         MVC   DTOKPG,PAGNO                                                     
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SET FILE (UNIT OR XSPOT)                                               
*-------------------------------------------------------------------*           
SF       DS    0H                                                               
         MVI   DATADISP+1,24                                                    
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         MVC   SYSDIR(3),=CL3'SPT'                                              
         MVC   SYSFIL(3),=CL3'SPT'                                              
                                                                                
         TM    SECFLAG,NECONPRD+NEMORPRD       CONVERTED OR BOTH                
         BZR   RE                                                               
                                                                                
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=CL3'XSP'                                              
         MVC   SYSFIL(3),=CL3'XSP'                                              
         BR    RE                                                               
*-------------------------------------------------------------------*           
*        VALIDATE RECORD ROUTINE                                                
*-------------------------------------------------------------------*           
VR       DS    0H                                                               
         MVI   IOOPT,C'Y'                                                       
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
VR01     DS    0H                                                               
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    VRPFK5                                                           
         CLI   PFKEY,4             ADD LINE?                                    
         BE    VRPFK5                                                           
         CLI   PFKEY,0             ANY OTHER PFKEY IS INVALID                   
         BE    VR10                ENTER KEY HIT, GO VALIDATE INPUT             
         MVC   GERROR,=Y(PFUNDEF)                                               
         B     TRAPERR2                                                         
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
         BL    VR10                YES -- ONLY ALLOWED FOR ADD                  
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
         B     VR10                                                             
*                                                                               
VRPFK50  CLI   PFKEY,4             ADD LINE?                                    
         BNE   VRPFK80             NO                                           
*                                                                               
         CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR10                YES                                          
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
         B     VR10                                                             
*                                                                               
VRPFK80  TM    1(R2),X'08'         IS LINE CURRENTLY HIGH INTENSITY?            
         BZ    *+12                                                             
         NI    1(R2),X'FF'-X'0C'   YES -- FORCE NORMAL INTENSITY                
         B     *+8                                                              
         OI    1(R2),X'08'         NO -- FORCE HIGH INTENSITY                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
* ----HERE BEGINS THE NON-FUNCTION KEY VALIDATION FOR RECORD----                
*-------------------------------------------------------------------*           
VR10     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR12                                                             
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         XCEFL                                                                  
                                                                                
         L     R4,AIO                                                           
         USING DT2KEY,R4                                                        
         MVC   0(32,R4),NEWKEY                                                  
         MVC   KEY,NEWKEY                                                       
         MVI   DATADISP+1,42                                                    
         B     VR15                                                             
                                                                                
VR12     MVC   AIO,AIO1                                                         
         MVC   KEY(L'DT2KEY),NEWKEY                                             
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
         MVI   DATADISP+1,42                                                    
VR15     DS    0H                                                               
         L     R4,AIO                                                           
         USING DT2KEY,R4                                                        
         MVC   DT2AGYA,AGENCY                                                   
         MVC   BAGYMD,DT2KAM                                                    
         CLC   BCLT,DT2KCLT        IS CLIENT SAME                               
         BE    VR20                 YES                                         
         MVC   BCLT,DT2KCLT                                                     
         GOTO1 CLUNPK,DMCB,DT2KCLT,QCLT                                         
VR20     DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    VR25                                                             
                                                                                
         CLC   DT2KPRD,=XL3'00'    ANY PRD                                      
         BH    VR22                                                             
                                                                                
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    VR25                                                             
                                                                                
         LA    R2,TRACLTH          CLIENT                                       
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
                                                                                
VR22     DS    0H                                                               
         TM    SECFLAG,NECONPRD                                                 
         BO    VR25                                                             
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
                                                                                
         CLC   DT2KPRD,0(RE)       ASSUMING FORMAT IS CCCX                      
         BE    VR25                                                             
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERR                                                          
                                                                                
         DROP  R4                                                               
                                                                                
* GET ANY TEXT TITLE                                                            
                                                                                
VR25     MVI   ELCODE,X'20'                                                     
         GOTO1 AHELLO,DMCB,(C'D',=CL8'XSPFIL'),(X'20',AIO1)                     
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DTXTLEEL,R6                                                      
         MVI   DTXTLEEL,X'20'                                                   
         MVI   DTXTLELN,26         LENGTH                                       
         LA    R2,TRATLEH          TITLE LINE                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   *+12                   YES                                       
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   DTXTITLE,WORK                                                    
         GOTO1 AHELLO,DMCB,(C'P',=CL8'XSPFIL'),AIO1,ELEM,0                      
                                                                                
* GET ANY TEXT LINES                                                            
                                                                                
VR30     MVI   ELCODE,X'40'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 AHELLO,DMCB,(C'D',=CL8'XSPFIL'),(X'40',AIO1)                     
         LA    R3,1                SET TEXT LINE NUMBER                         
         LR    R5,R3               SET BLK LINE CTR                             
         LA    R6,ELEM                                                          
         USING DTXTXTEL,R6                                                      
         MVI   DTXTXTEL,X'40'                                                   
         LA    R2,TRAL01H          FIRST TEXT LINE                              
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
*                                                                               
         CLI   0(R1),C' '          IF BLANK                                     
         BH    *+10                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,*-10             CK ENTIRE FLD                                
*                                                                               
         STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
*                                                                               
VR40     GOTO1 ANY                                                              
                                                                                
         CLI   5(R2),58            MAX LENGTH                                   
         BNH   *+12                                                             
         MVI   ERROR,INVTXTLN      TEXT LINE TOO LONG                           
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)           TOTAL ELEMENT LENGTH                         
         STC   R0,DTXTXTLN         AND LENGTH                                   
         STC   R3,DTXLNNUM         TEXT LINE NUMBER                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTXTXT-DTXTXTEL(0,R6),WORK                                       
         GOTO1 AHELLO,DMCB,(C'P',=CL8'XSPFIL'),AIO1,ELEM,0                      
         MVI   DTXTXTEL,X'40'                                                   
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VR50     LA    R5,1(,R5)           INCREMENT BLANK LINE CTR                     
         ZIC   R1,0(R2)            GET THIS SCREEN FIELD LENGTH                 
         AR    R2,R1               ADD TO FIELD POINTER                         
         ZIC   R1,0(R2)            ALSO BYPASS PROTECTED FIELD                  
         AR    R2,R1               ADD TO FIELD POINTER                         
         LA    R1,TRATAGH          END OF SCREEN                                
         CR    R1,R2               IF THERE                                     
         BNH   VR70                GET OUT                                      
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
*                                                                               
         CLI   0(R1),C' '          IF BLANK                                     
         BH    *+10                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,*-10             CK ENTIRE FLD                                
*                                                                               
         STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR50                NO                                           
*                                                                               
VR60     CR    R3,R5               CK LAST LINE VS THIS LINE                    
         BE    VR40                NO INTERVENING BLANK LINES                   
         MVI   DTXTXTLN,4          ONLY 1 BLANK NEEDED FOR BLK LINE             
         STC   R3,DTXLNNUM         STORE TEXT LINE NUMBER                       
         MVI   DTXTXT,C' '         AND HERE IT IS                               
         GOTO1 AHELLO,DMCB,(C'P',=CL8'XSPFIL'),AIO1,ELEM,0                      
         LA    R3,1(,R3)           ADD TO TEXT LINE CT                          
         B     VR60                BUILD ALL BLK LINES NEEDED                   
                                                                                
VR70     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR73                                                             
         L     R4,AIO                                                           
         USING DT2KEY,R4                                                        
         MVC   DT2KPG,PAGNO                                                     
         MVC   DT2KPRD,PRODUCT                                                  
         GOTO1 AIOCALL,DMCB,ADDRECQ+FILQ+XSPQ,AIO1                              
         B     VR75                                                             
                                                                                
VR73     DS    0H                                                               
         GOTO1 AIOCALL,DMCB,PUTQ+FILQ+XSPQ,AIO1                                 
                                                                                
VR75     CLI   ACTNUM,ACTADD       UNLESS ADD, ALL DONE                         
         BNE   DR                                                               
         CLI   PAGNO,X'01'         PAGE 1 - NO PREV LINK NEEDED                 
         BE    DR                                                               
         LA    R4,KEY                                                           
         L     R6,AIO1                                                          
         USING DT2KEY,R4                                                        
         MVC   DT2KEY,NEWKEY                                                    
         ZIC   RE,DT2KPG                                                        
         BCTR  RE,0                                                             
         STC   RE,DT2KPG                                                        
                                                                                
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DR                  NOW DISPLAY VALIDATED RECORD                 
         DROP  R6,R4                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DISPLAY RECORD                                                                
*-------------------------------------------------------------------*           
DR       DS    0H                                                               
         MVC   AIO,AIO1                                                         
                                                                                
DR02     EQU   *                                                                
         MVC   KEY(L'DT2KEY),NEWKEY                                             
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
         MVI   DATADISP+1,42                                                    
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         MVI   DATADISP+1,42                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE TITLE ELEMENT                      
                                                                                
         USING DTXTLEEL,R6                                                      
         CLC   TRATLE,DTXTITLE                                                  
         BE    *+14                                                             
         MVC   TRATLE,DTXTITLE                                                  
         OI    TRATLEH+6,X'80'                                                  
DR10     L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         MVI   DATADISP+1,42                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE 1 TEXT ELEMENT                     
         LA    R2,TRAL01H                                                       
         LA    R4,8(,R2)                                                        
         USING DTXTXTEL,R6                                                      
DR20     ZIC   R1,DTXTXTLN         GET TEXT ELEMENT LENGTH                      
         SH    R1,=H'4'            GET TEXT LENGTH -1                           
         ZIC   RF,0(R2)            GET FIELD LENGTH                             
         LR    RE,RF                                                            
         SH    RF,=H'9'            GET FIELD LENGTH -1                          
         EX    RF,*+8              CLEAR OUTPUT FLD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         CR    RF,R1               SEE IF ENOUGH ROOM IN FIELD                  
         BNL   DR30                                                             
         DC    H'0'                NOT ENOUGH ROOM IN FLD FOR TEXT LINE         
DR30     EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DTXTXT-DTXTXTEL(R6)                                      
         OI    6(R2),X'80'         SET ON TRANSMIT BIT                          
         AR    R2,RE               POINT TO NEXT FIELD (TITLE)                  
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD (INPUT)                  
         BAS   RE,NEXTEL                                                        
         BNE   DR40                                                             
         LA    R0,TRATAGH                                                       
         CR    R0,R2               CK IF END OF SCREEN                          
         BH    DR20                NO                                           
         DC    H'0'                MORE TEXT LINES THAN SCREEN SPACE            
DR40     LA    R0,TRATAGH                                                       
         CR    R0,R2               CK IF END OF SCREEN                          
         BNH   DR60                NO                                           
         OC    8(L'TRAL01,R2),8(R2)                                             
         BZ    DR50                                                             
         XC    8(L'TRAL01,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
DR50     ZIC   R1,0(R2)            GET FLD LEN                                  
         AR    R2,R1               NOW AT TITLE                                 
         ZIC   R1,0(R2)            GET FLD LEN                                  
         AR    R2,R1               NOW AT NEXT INPUT FIELD                      
         B     DR40                                                             
                                                                                
DR60     DS    0H                                                               
         CLI   ACTNUM,ACTSEL          IF ACTION IS SELECT COULD                 
         BNE   DRXIT                  BE DELETE FROM SELECT                     
         CLI   THISLSEL,C'D'          IS DELETE FROM SELECT                     
         BNE   DRXIT                  IF NOT EXIT                               
         TM    SECFLAG,NECONPRD       COMPLETELY CONVERTED THEN                 
         BO    DRXIT                  GENCON CAN DO THE DELETE                  
                                                                                
         XC    KEY,KEY                                                          
         USING DTOKEY,R4                                                        
         LA    R4,KEY                 BUILD UNIT KEY                            
         MVC   DTOKEY(DTOKPRD-DTOKEY),NEWKEY                                    
         MVC   DTOKPG,NEWKEY+31                                                 
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
         CLC   NEWKEY+10(3),0(RE)     HEX PRODUCT CODE IN KEY                   
         BE    DR70                                                             
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
DR70     MVC   DTOKPRD,3(RE)                                                    
                                                                                
         MVI   DATADISP+1,24                                                    
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         MVC   SYSDIR(3),=CL3'SPT'                                              
         MVC   SYSFIL(3),=CL3'SPT'                                              
         GOTO1 HIGH                   READ UNIT KEY                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                 GET UNIT RECORD                           
                                                                                
         TM    SECFLAG,NEMORPRD       IF NOT CONVERTED OR CONCURRENT            
         BZ    DRXIT                  CAN LET GENCON DO THE REST                
                                                                                
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=CL3'XSP'                                              
         MVC   SYSFIL(3),=CL3'XSP'                                              
         MVC   KEY(L'DT2KEY),NEWKEY         READ XSOPT REC TO PREPARE           
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0    GENCON TO DELETE IT                 
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
                                                                                
DRXIT    B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DISPLAY KEY                                                            
*        DISPLAY CLIENT                                                         
*-------------------------------------------------------------------*           
DK       DS    0H                                                               
         MVC   AIO,AIO1                                                         
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DK02                                                             
         BAS   RE,FORMKEY                                                       
                                                                                
DK02     EQU   *                                                                
         MVC   KEY(L'DT2KEY),NEWKEY                                             
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
         MVI   DATADISP+1,42                                                    
                                                                                
         L     R4,AIO                                                           
         USING DT2KEY,R4                                                        
         CLC   DT2KCLT,LSTCLT      HAS CLT CHANGED SINCE LAST FCLT?             
         BE    *+14                 NO                                          
DK04     MVC   KEY(L'DT2KEY),DT2KEY  TO MAKE FCLT HAPPY...                      
         BAS   RE,FCLT             MAY HAVE IF COMING FROM LIST                 
         MVC   LSTCLT,DT2KCLT                                                   
         MVC   TRACLT(3),QCLT                                                   
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         NI    TRACLTH+4,X'FF'-X'20'  SET NOT VALIDATED                         
                                                                                
* DISPLAY NETWORK OR M=C,S,N,O IF EITHER EXIST                                  
                                                                                
         OC    DT2KNET,DT2KNET     ANY NETWORK?                                 
         BZ    *+14                 NO                                          
         MVC   TRANET,DT2KNET                                                   
         B     DK10                                                             
                                                                                
         OC    DT2KMED,DT2KMED     ANY MEDIA?                                   
         BZ    *+20                 NO                                          
         MVC   TRANET(2),=C'M='                                                 
         MVC   TRANET+2(1),DT2KMED                                              
DK10     OI    TRANETH+6,X'80'     XMIT                                         
         NI    TRANETH+4,X'FF'-X'20'  SET NOT VALIDATED                         
                                                                                
* DISPLAY PRODUCT                                                               
                                                                                
         OC    DT2KPRD,DT2KPRD     ANY PRODUCT?                                 
         BZ    DK20                                                             
                                                                                
         LA    RE,DT2KPRD                                                       
         TM    SECFLAG,NECONPRD                                                 
         BO    DK15                                                             
                                                                                
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
*                                                                               
         CLC   DT2KPRD,0(RE)       ASSUMING FORMAT IS CCCX                      
         BE    DK15                                                             
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERR                                                          
*                                                                               
DK15     MVC   TRAPROD,0(RE)                                                    
         OI    TRAPRODH+6,X'80'                                                 
         NI    TRAPRODH+4,X'FF'-X'20'  SET NOT VALIDATED                        
                                                                                
* DISPLAY PAGE                                                                  
                                                                                
DK20     ZIC   RE,DT2KPG                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         XC    WORK(L'TRAPG),WORK                                               
         UNPK  WORK(2),DUB                                                      
         CLI   WORK,C'0'                                                        
         BNE   DK30                                                             
         MVC   WORK(1),WORK+1                                                   
         MVI   WORK+1,C' '                                                      
                                                                                
DK30     CLC   TRAPG,WORK                                                       
         BE    *+14                                                             
         MVC   TRAPG,WORK          MOVE IN TEXT ID                              
         OI    TRAPGH+6,X'80'      SET ON TRANSMIT BIT                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*-------------------------------------------------------------------*           
LR       DS    0H                                                               
         LA    R3,TABLE                                                         
         LA    R5,4000(R3)                                                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+14                                                             
         L     R3,VADUMMY                                                       
         LR    R5,R3                                                            
         A     R5,=F'50000'                                                     
         ST    R3,ASORTTAB                                                      
         ST    R5,ASORTEND                                                      
         USING SORTD,R3                                                         
*                                                                               
         OC    KEY(L'DT2KEY),KEY   IF KEY ^0, DATA ALREADY SORTED               
         BZ    *+16                                                             
         L     R4,AIO                                                           
         AH    R3,LASTENT          DISP TO NEXT REC IN TABLE                    
         B     LR82                                                             
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A50'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VQSORT,DMCB                                                      
*                                                                               
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    LSTCLT,LSTCLT                                                    
         XC    LASTENT,LASTENT                                                  
         XC    RECCT,RECCT                                                      
* BUILD KEY, AND DO READHI                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DT2KEY,R4                                                        
         MVC   DT2KID(2),=XL2'0A2D'                                             
         MVC   DT2KAM,BAGYMD                                                    
         MVC   DT2KCLT,SVBCLT                                                   
*                                                                               
LR10     DS    0H                                                               
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         B     LR30                                                             
                                                                                
LR20     DS    0H                                                               
         GOTO1 AIOCALL,DMCB,SEQQ+DIRQ,0                                         
                                                                                
LR30     DS    0H                                                               
         CLC   KEY(5),KEYSAVE      SAME A/M CLT                                 
         BNE   LR60                 NO                                          
         MVI   ERROR,0             SOMEBODY GOOFED SOMEWHERE...                 
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT                           
         CLI   ERROR,0             ANY ERRORS?                                  
         BE    LR40                 NO                                          
         LA    R2,CONWHENH                                                      
         MVC   GERROR,=Y(TOOBIG)                                                
         OC    SPOOLKEY,SPOOLKEY   TEST REPORT GENERATED                        
         BZ    TRAPERR2                                                         
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0                                                          
         BE    TRAPERR2                                                         
         DC    H'0'                                                             
                                                                                
* FILTER OUT UNWANTED RECS                                                      
                                                                                
LR40     CLC   KEY(3),KEYSAVE      STILL ON SAME AGY?                           
         BNE   LR60                 NO                                          
                                                                                
         CLC   DT2KCLT,=X'8000'                                                 
         BL    LR20                                                             
*                                                                               
*        OC    BCLT,BCLT           IS THIS REQUEST CLIENT SPECIFIC              
*        BZ    LR44                                                             
*        CLC   BCLT,DTXKCLT        THIS REQUESTED CLIENT                        
*        BNE   LR60                 NO, DONE                                    
                                                                                
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    LR42                                                             
                                                                                
         CLC   LSTCLT,DT2KCLT      STILL ON SAME CLT?                           
         BE    LR44                                                             
                                                                                
LR42     DS    0H                                                               
         BAS   RE,FCLT                                                          
         BNE   LR60                                                             
*                                                                               
         MVC   LSTCLT,DT2KCLT                                                   
*                                                                               
LR44     DS    0H                                                               
         OC    SVBCLT,SVBCLT       FILTERING ON CLIENT?                         
         BZ    LR46                 NO                                          
         CLC   DT2KCLT,SVBCLT      STILL ON SAME CLIENT?                        
         BNE   LR60                 NO                                          
*                                                                               
LR46     DS    0H                                                               
         CLI   MEDIA,0             FILTERING ON MEDIA?                          
         BE    LR48                 NO                                          
         CLC   DT2KMED,MEDIA       THIS MEDIA?                                  
         BE    LR48                 YES                                         
         MVC   DT2KNET,=X'FFFFFFFF'  FORCE NEXT MEDIA                           
         B     LR10                                                             
*                                                                               
LR48     DS    0H                                                               
         OC    NETWORK,NETWORK     FILTERING ON NETWORK?                        
         BZ    LR48A                NO                                          
         CLC   DT2KNET,NETWORK     THIS NETWORK?                                
         BE    LR48A                YES                                         
         MVC   DT2KPRD,=X'FFFFFF'  FORCE NEXT NET                               
         B     LR10                                                             
                                                                                
LR48A    CLC   PRODUCT,SPACES      FILTERING ON PRODUCT?                        
         BNH   LR48B                NO                                          
*                                                                               
         CLC   DT2KPRD,PRODUCT     THIS PRODUCT?                                
         BE    LR48B                YES                                         
         MVI   DT2KPG,X'FF'        FORCE NEXT PROD                              
         B     LR10                                                             
*                                                                               
LR48B    XC    SRTPRD,SRTPRD                                                    
         OC    DT2KPRD,DT2KPRD                                                  
         BZ    LR50                                                             
                                                                                
         MVC   SRTPRD,DT2KPRD                                                   
         TM    SECFLAG,NECONPRD                                                 
         BO    LR50                                                             
*                                                                               
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
*                                                                               
         CLC   DT2KPRD,0(RE)       ASSUMING FORMAT IS CCCX                      
         BE    LR49                                                             
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         MVI   DT2KPG,X'FF'        FORCE NEXT PROD (HI ON PAGE NUM)             
         B     LR10                                                             
*                                                                               
LR49     MVC   SRTPRD,0(RE)                                                     
*                                                                               
LR50     MVC   SRTAM,DT2KAM                                                     
         MVC   SRTCLT,DT2KCLT                                                   
         MVC   SRTMED,DT2KMED                                                   
         MVC   SRTNET,DT2KNET                                                   
         OC    SRTNET,SRTNET       WAS THERE A NETWORK?                         
         BNZ   *+8                 NO                                           
         NI    SRTMED,X'FF'-X'80'  FLIP HOB TO FORCE MEDIA SPEC FIRST           
         MVC   SRTPG,DT2KPG        PAGE NUMBER                                  
         MVC   SRTDA,KEY+36        DISK ADDRESS OF RECORD                       
         LA    R3,SORTLEN(R3)                                                   
         C     R3,ASORTEND         EOT?                                         
         BL    LR55                                                             
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   *+6                  NO                                          
         DC    H'0'                                                             
         MVC   GERROR,=Y(TOOBIG)                                                
         B     TRAPERR2                                                         
LR55     LH    R1,RECCT                                                         
         LA    R1,1(R1)                                                         
         STH   R1,RECCT                                                         
         B     LR20                                                             
*                                                                               
LR60     ZICM  R3,RECCT,2          ANY RECS?                                    
         BNZ   LR70                                                             
         LA    R2,TRACLTH                                                       
         MVC   GERROR,=Y(NORECSEL)                                              
         MVI   GMSGTYPE,C'I'                                                    
         B     TRAPERR2                                                         
*                                                                               
LR70     GOTO1 VQSORT,DMCB,(0,ASORTTAB),(R3),SORTLEN,SRTKLEN,0                  
         XC    LSTCLT,LSTCLT                                                    
         L     R4,AIO                                                           
         L     R3,ASORTTAB                                                      
         B     LR82                                                             
LR80     LA    R3,SORTLEN(R3)                                                   
LR82     ZICM  R1,RECCT,2          #RECS LEFT IN TABLE                          
         BZ    EXIT                                                             
         BCTR  R1,0                                                             
         STH   R1,RECCT                                                         
         LH    R1,LASTENT          SAVE DISP TO LAST ENTRY                      
         LA    R1,SORTLEN(R1)                                                   
         STH   R1,LASTENT                                                       
                                                                                
         CLC   KEY(2),=X'0A2D'                                                  
         BE    *+10                                                             
         MVC   KEY,KEYSAVE                                                      
                                                                                
         MVC   KEY+14(4),SRTDA                                                  
                                                                                
         MVI   DATADISP+1,24                                                    
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         MVC   SYSDIR(3),=CL3'SPT'                                              
         MVC   SYSFIL(3),=CL3'SPT'                                              
                                                                                
         TM    SECFLAG,NEMORPRD                                                 
         BO    LR85                                                             
         TM    SECFLAG,NECONPRD                                                 
         BZ    LR90                                                             
                                                                                
LR85     MVC   KEY+36(4),SRTDA                                                  
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=CL3'XSP'                                              
         MVC   SYSFIL(3),=CL3'XSP'                                              
LR90     DS    0H                                                               
         GOTO1 GETREC                                                           
         MVC   KEY+36(4),SRTDA                                                  
         MVC   AIO,AIO1                                                         
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
         MVC   KEY(L'DT2KEY),DT2KEY   TO MAKE FCLT HAPPY...                     
                                                                                
         CLI   MODE,LISTRECS                                                    
         BE    LRL                                                              
         CLI   MODE,PRINTREP                                                    
         BE    LRR                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST                                                            
                                                                                
LRL      MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,DT2KCLT,LCLT                                         
         OC    DT2KNET,DT2KNET                                                  
         BZ    *+14                                                             
         MVC   LNET,DT2KNET                                                     
         B     LRL10                                                            
         OC    DT2KMED,DT2KMED                                                  
         BZ    LRL10                                                            
         MVC   LNET(2),=C'M='                                                   
         MVC   LNET+2(1),DT2KMED                                                
*                                                                               
LRL10    OC    DT2KPRD,DT2KPRD     ANY PRODUCT?                                 
         BZ    *+10                                                             
         MVC   LPROD,SRTPRD        ALREADY EXPLODED                             
*                                                                               
         ZIC   RE,DT2KPG                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         UNPK  LPG(2),DUB                                                       
         CLI   LPG,C'0'                                                         
         BNE   *+8                                                              
         MVI   LPG,C' '                                                         
                                                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         MVI   DATADISP+1,42                                                    
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
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LTEXT(0),DTXTXT-DTXTXTEL(R6)                                     
         GOTO1 LISTMON                                                          
         B     LR80                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
                                                                                
LRR      DS   0H                                                                
         CLC   DT2KCLT,LSTCLT      HAS CLT CHANGED SINCE LAST FCLT?             
         BE    LRR00                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   LSTCLT,DT2KCLT                                                   
LRR00    DS   0H                                                                
         GOTO1 CLUNPK,DMCB,DT2KCLT,QCLT                                         
         LA    R5,P                                                             
         OC    DT2KNET,DT2KNET                                                  
         BZ    *+14                                                             
         MVC   PNET,DT2KNET                                                     
         B     LRR10                                                            
         OC    DT2KMED,DT2KMED                                                  
         BZ    LRR10                                                            
         MVC   PNET(2),=C'M='                                                   
         MVC   PNET+2(1),DT2KMED                                                
*                                                                               
LRR10    OC    DT2KPRD,DT2KPRD     ANY PRODUCT?                                 
         BZ    *+10                                                             
         MVC   PPROD,SRTPRD        ALREADY EXPLODED                             
*                                                                               
         L     R6,AIO                                                           
                                                                                
         ZIC   RE,DT2KPG                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         UNPK  PPAGE(2),DUB                                                     
         CLI   PPAGE,C'0'                                                       
         BNE   *+8                                                              
         MVI   PPAGE,C' '                                                       
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         MVI   DATADISP+1,42                                                    
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
LRR30    ZIC   R1,DTXTXTLN                                                      
         SH    R1,=H'4'            GET TEXT LEN-1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PTEXT-P(0,R5),DTXTXT                                             
         CLC   0(132,R5),SPACES    IF STILL BLANK LINE                          
         BNE   *+8                                                              
         MVI   0(R5),0             FORCE LINE TO PRINT                          
         BAS   RE,NEXTEL                                                        
         BNE   LRR40                                                            
         LA    R5,132(,R5)                                                      
         BCT   R0,LRR30                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR20                                                            
LRR40    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR80                                                             
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DELETE RECORD - CHECK IF ANY PAGES AFTER THIS ONE *                           
*-------------------------------------------------------------------*           
DEL      L     R4,AIO                                                           
         USING DT2KEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'DT2KEY),NEWKEY                                             
         LA    R4,KEY                                                           
         ZIC   RE,DT2KPG                                                        
         LA    RE,1(RE)                                                         
         STC   RE,DT2KPG                                                        
         NI    DMINBTS,X'FF'-X'08' MAKE SURE DON'T PASS DELETES                 
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BNE   *+14                                                             
         MVC   GERROR,=Y(DELLAST)                                               
         B     TRAPERR2                                                         
                                                                                
         MVI   DATADISP+1,42                                                    
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'DT2KEY),NEWKEY                                             
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
                                                                                
         L     R4,AIO1                                                          
         OI    KEY+32,X'80'                                                     
         GOTO1 AIOCALL,DMCB,WRITEQ+DIRQ,0                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        FORMAT KEY INTO NEW FORMAT BASED ON FILE SETTINGS                      
*        ROUTINES WILL EXPECT THE KEY TO BE IN NEWKEY                           
*------------------------------------------------------------------*            
FORMKEY  NTR1                                                                   
                                                                                
         MVC   NEWKEY,KEY                                                       
                                                                                
         TM    SECFLAG,NECONPRD     COMPLETELY CONVERTED                        
         BO    EXIT                 YES                                         
         TM    SECFLAG,NEMORPRD     BOTH                                        
         BO    EXIT                 YES                                         
                                                                                
FMK10    LA    R4,NEWKEY                                                        
         XC    NEWKEY,NEWKEY                                                    
         USING DT2KEY,R4                                                        
         MVC   DT2KID,=XL2'0A2D'                                                
         MVC   DT2KAM,KEY+2         AGENCY/MEDIA XL1                            
         MVC   DT2KCLT,KEY+3        CLIENT XL2                                  
         MVC   DT2KMED,KEY+5        MEDIA CL1                                   
         MVC   DT2KNET,KEY+6        NETWORK CL4                                 
         MVC   DT2KPG,KEY+12        PAGE  NUMBER XL1                            
                                                                                
         CLI   KEY+10,0             PRODUCT NUMBER                              
         BE    FKXIT                                                            
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
         CLC   KEY+10(1),3(RE)     HEX PRODUCT CODE IN KEY                      
         BE    FK050                                                            
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
*                                                                               
FK050    MVC   DT2KPRD,0(RE)                                                    
                                                                                
FKXIT    B     EXIT                                                             
         EJECT                                                                  
         DS    0H                                                               
*------------------------------------------------------------------*            
* VALIDATE NETWORK                                                              
*------------------------------------------------------------------*            
VNET     NTR1                                                                   
         XC    MEDIA,MEDIA                                                      
         XC    NETWORK,NETWORK                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VNETX                NO                                          
         GOTO1 ANY                                                              
*                                                                               
         CLC   =C'M=',WORK         MEDIA LEVEL COMMENT?                         
         BNE   VNET10               NO                                          
         CLI   5(R2),3                                                          
         BNE   VNETBDMD                                                         
                                                                                
*MN                                                                             
*        CLI   WORK+2,C'H'                                                      
*        BE    VNET05                                                           
*        CLI   WORK+2,C'D'                                                      
*        BE    VNET05                                                           
*MN                                                                             
         CLI   WORK+2,C'N'                                                      
         BE    VNET05                                                           
         CLI   WORK+2,C'C'                                                      
         BE    VNET05                                                           
         CLI   WORK+2,C'S'                                                      
         BE    VNET05                                                           
         CLI   WORK+2,C'O'                                                      
         BNE   VNETBDMD                                                         
VNET05   MVC   MEDIA,WORK+2                                                     
         B     VNETX                                                            
*                                                                               
VNETBDMD MVC   GERROR,=Y(BADMEDIA)                                              
         B     TRAPERR2                                                         
*                                                                               
VNET10   MVI   KEY,C'0'                                                         
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
         BE    *+14                                                             
         MVC   GERROR,=Y(NONET)                                                 
         B     TRAPERR2                                                         
                                                                                
         MVC   NETWORK,WORK                                                     
         L     R4,AIO                                                           
*MN      MVC   MEDIA,SPTYPE                                                     
* PER MTG 7/16/08 DECIDED TO DUMP IF NEW TRFTYPE IS NOT SET                     
         CLI   STRTYPE,C' '                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   MEDIA,STRTYPE                                                    
VNETX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
* VALIDATE PAGE NUMBER - AND ALL PREVIOUS RECORDS TO EXIST *                    
*------------------------------------------------------------------*            
VPG      NTR1                                                                   
         GOTO1 VALINUM             VALIDATE TO BE NUMERIC                       
         ZIC   RE,ACTUAL                                                        
         LA    RF,1                                                             
         LTR   RE,RE               IF ZERO, DON'T SUBTRACT                      
         BZ    VPG10                                                            
         BCTR  RE,0                                                             
VPG10    AR    RE,RF                                                            
         STC   RE,PAGNO                                                         
                                                                                
* IF AN ADD, AND NOT PAGE 0/1, TEST IF PREV RECORD EXISTS *                     
                                                                                
         CLI   PAGNO,X'01'         IF PAGE 1, NO PREVIOUS                       
         BE    EXIT                                                             
         CLI   ACTNUM,ACTADD       ONLY CK IF ADD                               
         BNE   EXIT                                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DT2KEY,R4                                                        
         MVC   DT2KID,=XL2'0A2D'                                                
         MVC   DT2KAM,BAGYMD                                                    
         MVC   DT2KCLT,BCLT                                                     
         MVC   DT2KMED,MEDIA                                                    
         MVC   DT2KNET,NETWORK                                                  
         MVC   DT2KPRD,PRODUCT                                                  
         BCTR  RE,0                                                             
         STC   RE,DT2KPG                                                        
                                                                                
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY),KEYSAVE                                            
         BE    EXIT                                                             
         MVC   GERROR,=Y(NOPREVPG)                                              
         B     TRAPERR2                                                         
         EJECT                                                                  
*------------------------------------------------------------------*            
* FIND CLIENT HEADER AND SAVE CLIST                                             
*------------------------------------------------------------------*            
FCLT     NTR1                                                                   
         MVC   LASTKEY,KEY                                                      
         MVC   SVBCLT,KEY+(DT2KCLT-DT2KEY)                                      
         GOTO1 CLUNPK,DMCB,DT2KCLT,QCLT                                         
                                                                                
* SAVE CURRENT RECORD                                                           
                                                                                
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0             RETURN ON ERROR                              
                                                                                
         GOTO1 VALICLT                                                          
                                                                                
         MVI   ERROPT,0                                                         
         MVC   KEY(L'LASTKEY),LASTKEY    RESTORE KEY                            
                                                                                
         L     R0,AIO1             MOVE COMMENT RECORD BACK                     
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
                                                                                
         CLI   ERROR,0                                                          
         BNE   FCLT15                                                           
                                                                                
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT35               NO                                          
         B     FCLT18                                                           
                                                                                
FCLT15   CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT23                                                           
                                                                                
* CHECK OUT BRAND LEVEL SECURITY                                                
                                                                                
FCLT18   MVC   KEY(L'LASTKEY),LASTKEY         RESTORE KEY                       
                                                                                
         LA    R4,KEY                                                           
         USING DT2KEY,R4                                                        
         OC    DT2KPRD,DT2KPRD     ANY PRODUCT?                                 
         BZ    FCLT35              NO, OK TO DISPLAY RECORD                     
                                                                                
FCLT21   MVC   KEY(L'LASTKEY),LASTKEY         RESTORE KEY                       
                                                                                
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
                                                                                
         CLC   DT2KPRD,0(RE)       THIS PROD CODE?                              
         BE    FCLT35                                                           
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         MVI   ERROR,SECLOCK       PRD IS NOT IN PRD LIST (SEC LOCK)            
                                                                                
         CLI   MODE,PRINTREP                                                    
         BE    FCLT23                                                           
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    FCLT23                                                           
                                                                                
         CLI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         BE    *+6                                                              
         DC    H'0'                SOME OTHER ERROR???                          
                                                                                
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX                                                            
                                                                                
FCLT23   MVC   KEY(L'LASTKEY),LASTKEY         RESTORE KEY                       
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT25               NO                                          
                                                                                
         MVI   KEY+30,X'FF'        FORCE NEXT PROD                              
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(5),KEYSAVE      SAME AM/CLT                                  
         BNE   FCLT25                                                           
         MVC   LASTKEY,KEY         SAVE KEY                                     
         B     FCLT18                                                           
                                                                                
FCLT25   DS    0H                                                               
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         LTR   RB,RB               SET CC FOR RETURN                            
         B     EXIT                                                             
                                                                                
FCLT35   DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         CLC   KEY(L'DT2KEY-1),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO2                                 
                                                                                
FCLT40   MVC   AIO,AIO1                                                         
                                                                                
         CR    RB,RB               SET CC FOR RETURN                            
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*------------------------------------------------------------------*            
HDHK     NTR1                                                                   
*        GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         B     EXIT                                                             
                                                                                
* *********************************                                             
*        ERROR ROUTINES                                                         
* *********************************                                             
                                                                                
INVPOSN  MVC   GERROR,=Y(LINERR)                                                
         B     TRAPERR2                                                         
                                                                                
TRAPERR  GOTO1 ERREX                                                            
TRAPERR2 GOTO1 VTRAERR                                                          
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,42,C'COMMENT TEXT LIST'                                       
         SSPEC H2,42,C'-----------------'                                       
         SSPEC H1,71,AGYNAME                                                    
         SSPEC H2,71,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,83,RUN                                                        
         SSPEC H4,71,REPORT                                                     
         SSPEC H5,71,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H7,3,C'NET'                                                      
         SSPEC H8,3,C'---'                                                      
         SSPEC H7,9,C'PRD'                                                      
         SSPEC H8,9,C'---'                                                      
         SSPEC H7,13,C'PAGE'                                                    
         SSPEC H8,13,C'----'                                                    
         SSPEC H7,29,C'TITLE'                                                   
         SSPEC H8,29,C'------------------------'                                
         SSPEC H7,55,C'TEXT'                                                    
         SSPEC H8,55,C'-------------------------------------------'             
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRDTXT                                                       
       ++INCLUDE SPTRNCMT                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA83D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
AHELLO   DS    F                                                                
LASTKEY  DS    CL(L'DT2KEY)                                                     
NEWKEY   DS    CL(L'DT2KEY)                                                     
RECCT    DS    XL2                 REC COUNT                                    
PAGNO    DS    XL1                                                              
MEDIA    DS    XL1                 C, S, N, O                                   
NETWORK  DS    XL4                                                              
PRODUCT  DS    CL3                                                              
BPROD    DS    XL1                 BINARY PRODUCT                               
SVBCLT   DS    XL2                 SAVE OF BCLT                                 
LSTCLT   DS    XL2                 LAST CLIENT (IN LR)                          
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
LASTENT  DS    H                   DISPLACEMENT TO LAST ENTRY ON SCREEN         
VQSORT   DS    V                                                                
ASORTTAB DS    A                   A(SORT TABLE)                                
ASORTEND DS    A                   A(SORT TABLE END)                            
RELO     DS    A                                                                
LASTACT  DS    XL1                                                              
TABLE    DS    CL4000                                                           
                                                                                
SORTD    DSECT                                                                  
SRTAM    DS    XL1                 AGY/MED                                      
SRTCLT   DS    XL2                 CLT                                          
SRTMED   DS    CL1                 MEDIA                                        
SRTNET   DS    CL4                 NETWORK                                      
SRTPRD   DS    CL3                 PRODUCT                                      
SRTPG    DS    XL1                 PAGE                                         
*                                                                               
SRTKLEN  EQU   *-SORTD             SORT ENTRY LENGTH                            
*                                                                               
SRTDA    DS    XL4                 DSKADDR                                      
*                                                                               
SORTLEN  EQU   *-SORTD             SORT ENTRY LENGTH                            
*                                                                               
                                                                                
* OFFLINE PRINT DSECT                                                           
                                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PNET     DS    CL4                                                              
         DS    CL2                                                              
PPROD    DS    CL3                                                              
         DS    CL2                                                              
PPAGE    DS    CL3                                                              
         DS    CL12                                                             
PTITLE   DS    CL24                                                             
         DS    CL2                                                              
PTEXT    DS    CL60                                                             
                                                                                
* ONLINE SCREEN LINE DSECT                                                      
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LNET     DS    CL4                                                              
         DS    CL1                                                              
LPROD    DS    CL3                                                              
         DS    CL1                                                              
LPG      DS    CL2                                                              
         DS    CL1                                                              
LTITLE   DS    CL24                                                             
         DS    CL1                                                              
LTEXT    DS    CL32                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPTRA53   10/13/09'                                      
         END                                                                    
