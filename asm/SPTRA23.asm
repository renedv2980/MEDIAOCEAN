*          DATA SET SPTRA23    AT LEVEL 094 AS OF 10/08/14                      
*PHASE T21623C                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T21623 PATTERN REC LIST'                                        
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - STATION FILE (PMS - PRINT MARKET/STA RTN)                  
*                    CLIENT HEADER IN FCLT-FIND CLIENT RTN                      
*                    DCML - CHECK FOR DELETED CMLS RTN                          
*             AIO3 - USED AS PRINT BUFFER FOR OFFLINE REPORT                    
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
* LEV 66 SMUR JAN27/04 SUPPORT 2 CHAR MARKET GROUP                    *         
* LEV 67 SMUR APR18/04 REPORT ON ADDS AND CHANGES ONLY                *         
* LEV 68 SMUR APR30/07 FIX TEXT= NOT PRINTING ON REPORT               *         
* LEV 69 SMUR APR01/08 BPAT                                           *         
* LEV 71 MNAS MAY13/08 BPAT                                           *         
* LEV 71 MNAS MAY13/08 INCREASE PATTERN TABLE SIZE NEXTPTR            *         
* LEV 72 MHER AUG/08   REFORMAT REPORT/SUPPORT TIMES AND DAYPARTS               
* LEV 76 SMUR DEC10/08 OPEN BPAT FOR AGY ALPHA FR                     *         
* LEV 81 MHER JUN/09   SUPPORT ADID FOR PATTERN DOWNLOAD              *         
* LEV 86 MNAS JUL/09   OPEN BPAT TO AGENCY ALPHA H7                   *         
* LEV 88 SMUR NOV07/11 OPEN BPAT FOR AGY ALPHA OO                     *         
* LEV 89 SMUR MAR27/12 TURN OFF SOON BIT WHEN ONLINE TO STOP DUMPS    *         
* LEV 90 MNAS OCT23/12 MORE BANDS                                     *         
* LEV 91 MNAS JAN27/13 FIX R2 BUG WHEN GOING OFF TO LISTMON           *         
* LEV 92 MNAS MAR12/13 SET TWAFIRST TO GET RUNLAST MODE/GLOBAL FILES  *         
* LEV 94 SMUR OCT07/14 MOVE PATTERN TABLE TO LARGER STORAGE           *         
***********************************************************************         
         TITLE 'T21623 PATTERN REC LIST'                                        
T21623   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21623**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR23RR                                                      
         MVI   NLISTS,14           ONLY 14 LINES ON LIST SCREEN                 
         XC    SVREGS,SVREGS       CLEAR MULTI-LINE                             
         L     RE,=V(DLFLD)                                                     
         A     RE,SPTR23RR                                                      
         ST    RE,VDLFLD                                                        
*                                                                               
         CLI   TRLSTPGM,X'23'                                                   
         BE    *+10                                                             
         XC    FILTERS,FILTERS                                                  
         MVI   TRLSTPGM,X'23'                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*-------------------------------                                                
*     VALIDATE KEY ROUTINE                                                      
*-------------------------------                                                
VK       NI    PRDMATSW,X'FF'-BPATSW INIT LIST BPAT RECORDS                     
         CLC   =C'BPAT',CONREC                                                  
         BNE   *+8                                                              
         OI    PRDMATSW,BPATSW      LIST BPAT RECORDS                           
*                                                                               
*MN                                                                             
*        MVI   TWAFIRST,2          SEND RUN LAST MODE                           
*MN                                                                             
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT, BPRD, BSLN, BPRD2, BSLN2         
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK12                                                             
*                                                                               
         TM    WHEN,X'38'          SOON/OV                                      
         BZ    VK12                 NO                                          
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
         B     VK20                                                             
*                                                                               
VK12     GOTO1 VALICLT                                                          
*                                                                               
         MVC   SVCLT,BCLT                                                       
*                                                                               
* READ T0 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),WORK,SVT1PROF,DATAMGR                                  
*                                                                               
VK20     LA    R2,TRAPRLNH         PRODUCT                                      
         XC    BPRD(2),BPRD        CLEAR IF NONE                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                                                             
         NI    SECFLAG,X'FF'-PRDTHTR INIT THEATRICAL PRODUCT                    
         GOTO1 VALIPRD                                                          
         CLC   =C'SJ',AGENCY       SJR                                          
         BE    VK20C                                                            
         CLC   =C'H7',AGENCY       AGY H7                                       
         BE    VK20C                                                            
         CLC   =C'OO',AGENCY       AGY OO                                       
         BE    VK20C                                                            
         CLC   =C'FR',AGENCY       AGY FR                                       
         BE    VK20C                                                            
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BNE   VK22                                                             
*                                                                               
VK20C    TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BZ    VK21                                                             
         CLC   =C'BPAT',CONREC     MUST USE BPAT                                
         BNE   BPATERR                                                          
         B     VK22                                                             
*                                                                               
VK21     CLC   =C'PAT',CONREC      MUST USE PAT FOR NON THTR PROD               
         BNE   UPATERR                                                          
*                                                                               
VK22     MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
*                                                                               
VK30     LA    R2,TRAPTLNH         PRODUCT PARTNER                              
         XC    BPRD2(2),BPRD2      CLEAR IF NONE                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK40                NO                                           
         GOTO1 VALIPRD                                                          
         CLC   =C'SJ',AGENCY       SJR                                          
         BE    VK30B                                                            
         CLC   =C'H7',AGENCY       AGY H7                                       
         BE    VK30B                                                            
         CLC   =C'OO',AGENCY       AGY OO                                       
         BE    VK30B                                                            
         CLC   =C'FR',AGENCY       AGY FR                                       
         BE    VK30B                                                            
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BNE   VK30C                                                            
*                                                                               
VK30B    TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BO    NOPBERR             NO P/B ALLOWED                               
*                                                                               
VK30C    CLC   QPRD,WORK           COMPARE 2 PRODUCTS                           
         BH    PRDSQERR            MUST BE LOWER FIRST                          
         BE    EQPRDERR            CAN NOT BE 2 EQUAL                           
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2(2),WORK+3     GET BIN PROD AND SPOT LENGTH                 
*                                                                               
VK40     LA    R2,TRACODEH         CODE                                         
         BAS   RE,VCC                                                           
         EJECT                                                                  
VK50     LA    R2,TRAREFH          REFERENCE NUMBER                             
         BRAS  RE,VREF                                                          
*                                                                               
VK60     LA    R2,TRAFLTRH         FILTERS                                      
         BRAS  RE,VFLT                                                          
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
VK70     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT(6),BCLT                                                  
         MVC   PATKCODE,CODE                                                    
         MVC   PATKREF,BREFSUB                                                  
*                                                                               
* CHECK FOR ANY MISSING FIELDS (MUST ALL BE ENTERED, LEFT TO RIGHT)             
*                                                                               
         OC    BREFSUB,BREFSUB     IF ANY ENTRY                                 
         BZ    VK72                NO                                           
         OC    BPRD,BPRD           MUST HAVE ENTERED PRD                        
         BNZ   VK74                DID                                          
         LA    R2,TRAPRLNH         ERROR                                        
         B     MISSERR             MISSING PROD/SLN ENTRY                       
VK72     OC    BPRD,BPRD           IF ANY ENTRY                                 
         BZ    VK76                NO                                           
VK74     OC    BCLT,BCLT           MUST HAVE ENTERED CLT                        
         BNZ   VK76                DID                                          
         LA    R2,TRACLTH          ERROR                                        
         B     MISSERR             MISSING CLIENT ENTRY                         
*                                                                               
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
*                                                                               
VK76     MVI   COMPKEYL,2          MAX KEY COMPARE (-1)                         
         MVC   COMPKEY,KEY                                                      
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    EXIT                                                             
         MVI   COMPKEYL,4          MAX KEY COMPARE (-1)                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LR       CLI   OFFLINE,C'Y'        IF NOT OFFLINE                               
         BE    LR00                                                             
         TM    WHEN,X'20'          BUT SOON BIT IS ON                           
         BZ    LR00                                                             
         CLC   CONWHEN(4),=C'SOON' AND PRINT OPTION IS NOT SOON                 
         BE    LR00                                                             
         NI    WHEN,X'FF'-X'20'    TURN OFF SOON BIT                            
         OI    WHEN,X'80'          TURN ON IMMEDIATE                            
                                                                                
LR00     MVC   AIO,AIO1                                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    LR01                                                             
         LA    RF,STARTSRT         START OF SORT AREA                           
         MVC   0(8,RF),=C'*COMMLS*'  COMMERCIAL TABLE                           
         LA    RF,8(RF)                                                         
         ST    RF,ACMLTAB                                                       
         AHI   RF,1050             50 CMLS 1 FLAG/8 CHAR ISCII/12 ADID          
         ST    RF,ACMLTABX                                                      
*                                                                               
LR01     DS    0H                                                               
*MN                                                                             
*        MVI   TWAFIRST,2          SEND RUN LAST MODE                           
*MN                                                                             
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR09A               CK IF LOWEST SUBLINE                         
         XC    PATLSTCT,PATLSTCT                                                
         TM    WHEN,X'C0'          IF OFFLINE, USE KHDUMMY                      
         BZ    LR02                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,STARTSRT         START OF SORT AREA                           
*                                                                               
         MVC   0(8,RF),=C'*COMMLS*'  COMMERCIAL TABLE                           
         LA    RF,8(RF)                                                         
         ST    RF,ACMLTAB                                                       
         AHI   RF,1050             50 CMLS 1 FLAG/8 CHAR ISCII/12 ADID          
         ST    RF,ACMLTABX                                                      
*                                                                               
         SR    RF,R9               MAKE RELATIVE                                
         ST    RF,NEXTPTR                                                       
         ST    RF,NEXTSTRT                                                      
         L     RE,LSYSD            GET LENGTH OF SYSD                           
         SH    RE,=AL2(L'SRTREC)   MAX USABLE                                   
*                                                                               
         ST    RE,NEXTMAX                                                       
         B     LR04                                                             
LR02     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,VADUMMY                                                       
         MVC   0(8,RF),=C'*COMMLS*'  COMMERCIAL TABLE                           
         LA    RF,8(RF)                                                         
         ST    RF,ACMLTAB                                                       
         AHI   RF,1050             50 CMLS 1 FLAG/8 ISCII/12 ADID               
         ST    RF,ACMLTABX                                                      
*                                                                               
*NOP     ST    RF,NEXTPTR                                                       
*        ST    RF,NEXTSTRT                                                      
*        A     RF,=F'135000'       ROOM FOR 7500 18 BYTE ENTRIES                
******   ST    RF,NEXTMAX                                                       
*                                                                               
         L     R0,SORTSIZE                                                      
         STORAGE OBTAIN,LENGTH=(R0),LOC=24,COND=YES                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,NEXTPTR                                                       
         ST    R1,NEXTSTRT                                                      
         A     R1,SORTSIZE                                                      
         ST    R1,NEXTMAX                                                       
         B     LR04                                                             
SORTSIZE DC    F'800000'                                                        
*                                                                               
LR04     LA    RF,HEADING                                                       
         ST    RF,SPECS                                                         
         LAY   RF,HDHK             HEADLINES ROUTINE                            
         ST    RF,HEADHOOK                                                      
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
         BNE   LR04X                                                            
         BRAS  RE,PRDOWN                                                        
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* BUILD KEY, AND DO READHI                                                      
*================================================================               
                                                                                
LR04X    LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID(2),=X'0A22'                                               
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT(6),BCLT     CLIENT AND ALL BELOW                         
*                                                                               
         OC    BCLT,BCLT           WAS CLT ENTERED                              
         BNZ   LR06                YES                                          
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE                                                   
         BNE   LREXIT                                                           
         MVC   BCLT,PATKCLT                                                     
         BRAS  RE,FCLT                                                          
*                                                                               
LR06     CLI   COMPKEY+5,0         WAS PROD ENTERED                             
         BNE   LR10                                                             
         L     R1,ASVCLIST         PROD LIST                                    
         CLI   0(R1),C' '          IF AT END                                    
         BL    LR18                                                             
*                                                                               
LR08     MVC   PATKPRD,3(R1)       SAVE 1ST PROD                                
         XC    PATKSLN(7),PATKSLN  SET REST OF KEY TO ZERO                      
         LA    R1,4(,R1)           POINT TO NEXT                                
         SR    R1,R9               SAVE DISPL                                   
         ST    R1,SVR2             ONLY                                         
         B     LR10                                                             
*                                                                               
* SEE IF NEXT SUBLINE FROM GENCON, NOT NEXT REF                                 
* THE SITUATION IS THAT GENCON DID A READ SEQUENTIAL                            
* WHICH MAY HAVE CAUSED US TO READ PAST THE LAST REC FOR THIS PRODUCT           
* BUT WE'RE READING THE PRODUCTS IN ALPHA SEQUENCE, NOT FILE SEQUENCE           
* SO WE MAY NOT BE DONE JUST YET!                                               
*                                                                               
LR09A    CLC   KEY(2),=X'0A22'     TEST FOR CHANGE OF RECTYPE/CLIENT            
         BNE   LR16                GO DO NEXT PRODUCT                           
         CLC   PATKAM(3),BAGYMD    TEST SAME A-M/CLT                            
         BNE   LR16                GO DO NEXT PRODUCT                           
         L     R1,SVR2             TEST PRODUCT CHANGED                         
         AR    R1,R9                                                            
         BCTR  R1,0                                                             
         CLC   PATKPRD,0(R1)       CHANGE OF PRODUCT                            
         BNE   LR16                YES                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,KEY                                                           
         MVC   WORK(13),KEY                                                     
         NI    PATKREF+1,X'FC'    SET OFF 10 BITS OF SUBLINE                    
         MVI   PATKREF+2,0                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),WORK       SAME REF/SUBLINE                              
         BE    LR10                YES, USE IT                                  
         CLC   KEY(7),KEYSAVE     TEST SAME THROUGH PRD                         
         BNE   LR16                                                             
                                                                                
* FORCE NEXT SUBLINE *                                                          
                                                                                
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         SH    R1,=H'1'                                                         
         BP    LR09D                IF STILL NOT ZERO, OK                       
         LLC   R1,PATKCODE         BUMP CODE                                    
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,PATKCODE                                                      
         CLI   PATKCODE,0          IF OVER 255, BUMP SPOT LEN                   
         BNE   LR09C                                                            
         LLC   R1,PATKSLN2        BUMP SPOT LEN2                                
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,PATKSLN2                                                      
LR09C    SR    R1,R1               SET BREF                                     
         B     *+8                         ZERO AND LEAVE IT ZERO               
*                                                                               
LR09D    X     R1,=XL4'00003FFF'   NOW MAKE COMPLEMENT                          
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,PATKREF                                                     
*                                                                               
LR09E    L     R1,SVR2             TEST PRODUCT CHANGED                         
         AR    R1,R9                                                            
         BCTR  R1,0                                                             
         CLC   PATKPRD,0(R1)       CHANGE OF PRODUCT                            
         BNE   LR16                YES                                          
*                                                                               
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(6),KEYSAVE      ANY CHANGE UP TO PRD1                        
         BE    LR20                NO                                           
*                                                                               
LR14     OC    PATLSTCT,PATLSTCT   CT OF PAT RECS IF SORT=DATE                  
         BZ    LR16                YES, ALL DONE                                
         BAS   RE,LRRSRT           GO SORT & PRINT                              
*                                                                               
LR16     LA    R4,KEY                                                           
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT,BCLT                                                     
         XC    KEY+6(7),KEY+6                                                   
*                                                                               
         CLI   COMPKEY+5,0         ANY PROD ENTERED                             
         BNE   LREXIT              YES, ALL DONE                                
         L     R1,SVR2                                                          
         AR    R1,R9                                                            
         CLI   0(R1),C' '          IF AT END                                    
         BL    LR18                                                             
         B     LR08                                                             
*                                                                               
LR18     OC    COMPKEY+3(2),COMPKEY+3 WAS CLT ENTERED                           
         BNZ   LREXIT              YES, ALL DONE                                
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID(2),=X'0A22'                                               
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT,BCLT                                                     
         MVC   KEY+5(8),=8X'FF'    FORCE TO READ NEXT CLIENT                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      ANY CHANGE AGY/MED                           
         BNE   LREXIT              YES                                          
         XC    KEY+5(8),KEY+5                                                   
         MVC   BCLT,PATKCLT                                                     
         BRAS  RE,FCLT                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     LR06                                                             
*                                                                               
LR20     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LR26                YES                                          
*                                                                               
         TM    PRDMATSW,BPATSW     WANTS BPAT RECS                              
         BZ    LR21                NO, SHOW PATTERN RECS ONLY                   
         TM    KEY+13,X'01'        IS THIS BPAT REC                             
         BZ    LR24                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   LR21C                                                            
         TM    KEY+13,X'02'        INCOMPLETE RECORD ?                          
         BO    LR24                NO CMLS ON RECORD, BYPASS                    
         B     LR21C                                                            
*                                                                               
LR21     TM    KEY+13,X'01'                                                     
         BO    LR24                                                             
*                                                                               
LR21C    LLC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   LR26                YES                                          
         CLI   COMPKEY+5,0        PROD ENTERED                                  
         BE    *+14               NO                                            
         CLC   KEY+5(1),COMPKEY+5                                               
         BNE   LR24                                                             
*                                                                               
         CLI   COMPKEY+6,0        SLN ENTERED                                   
         BE    *+14               NO                                            
         CLC   KEY+6(1),COMPKEY+6                                               
         BNE   LR24                                                             
*                                                                               
         CLI   COMPKEY+7,0        PROD2 ENTERED                                 
         BE    *+14               NO                                            
         CLC   KEY+7(1),COMPKEY+7                                               
         BNE   LR24                                                             
*                                                                               
         CLI   COMPKEY+8,0        SLN2 ENTERED                                  
         BE    *+14               NO                                            
         CLC   KEY+8(1),COMPKEY+8                                               
         BNE   LR24                                                             
*                                                                               
         CLI   COMPKEY+9,0        COPY CODE ENTERED                             
         BE    *+14               NO                                            
         CLC   KEY+9(1),COMPKEY+9                                               
         BNE   LR24                                                             
*                                                                               
         B     LR30                                                             
*                                                                               
LR24     BAS   RE,NXK             BUMP KEY                                      
         B     LR10               AND GET NEXT                                  
*                                                                               
LR26     OC    PATLSTCT,PATLSTCT   CT OF PAT RECS IF SORT=DATE                  
         BZ    LR28                YES, ALL DONE                                
         BAS   RE,LRRSRT           GO SORT & PRINT                              
*                                                                               
LR28     MVC   KEY(13),COMPKEY     RESET IF ANOTHER LIST                        
         MVC   BCLT(6),PATKCLT     WITHOUT VALIDATE KEY                         
         B     LREXIT              YES, ALL DONE                                
LR30     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,NXK             SET UP FOR NEXT KEY                           
*                                                                               
         BRAS  RE,FLTR                                                          
         BNE   LR10                                                             
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
LRCLC    CLC   COMPKEY(0),KEY                                                   
*                                                                               
LRTOOBIG MVC   GERROR,=Y(TOOBIG)                                                
         LA    R2,CONWHENH                                                      
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
* FORMAT RECORDS FOR DATE SORT UNLESS REF (NORMAL SEQ) REQUESTED                
*                                                                               
LASTONE  DS    CL20                                                             
*                                                                               
LRR      MVC   LASTONE,KEY                                                      
         MVI   RCSUBPRG,1          SET CORRECT HEADLINES PRINTING               
         CLI   CONREC,C'B'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
*                                                                               
         TM    FLAGFTR,X'20'       DO DATE SORT                                 
         BZ    LRR10               NO                                           
         L     R3,NEXTPTR          GET RELATIVE                                 
         TM    WHEN,X'C0'          IF OFFLINE, NO RELOC                         
         BZ    BUG1                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R9               ADD REAL                                     
         B     *+14                                                             
BUG1     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SORTREC,R3                                                       
         XC    SRTREC,SRTREC                                                    
         MVC   SRTSLN,PATKSLN                                                   
         CLI   PATKPRD2,0          IF NO PRD2                                   
         BE    LRR06               LEAVE ZERO                                   
         L     R1,ASVCLIST         ADDRESS OF CLIST                             
LRR02    CLI   0(R1),C' '                                                       
         BNH   LRR03                                                            
*                                                                               
         CLC   PATKPRD2,3(R1)                                                   
         BE    LRR04                                                            
         LA    R1,4(R1)                                                         
         B     LRR02                                                            
*                                                                               
LRR03    LA    R1,=C'???'          UNKNOWN                                      
*                                                                               
LRR04    MVC   SRTPROD2,0(R1)                                                   
         MVC   SRTSLN2,PATKSLN2                                                 
*                                                                               
LRR06    MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         MVC   SRTDTS,PATSTART                                                  
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATLSTEL,R6                                                      
         CLC   PATLSTTY(6),=X'D40000000000'                                     
         BNE   *+12                                                             
         MVI   SRTORD,X'10'                                                     
         B     LRR08                                                            
         CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BNE   *+12                                                             
         MVI   SRTORD,X'14'                                                     
         B     LRR08                                                            
         CLI   PATLSTTY,C'T'       STATION TYPE                                 
         BNE   *+12                                                             
         MVI   SRTORD,X'20'                                                     
         B     LRR08                                                            
*                                                                               
         CLI   PATLSTTY,C'A'       STATION AFFILIATE                            
         BNE   *+12                                                             
         MVI   SRTORD,X'30'                                                     
         B     LRR08                                                            
*                                                                               
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFILIATE                    
         BNE   *+12                                                             
         MVI   SRTORD,X'34'                                                     
         B     LRR08                                                            
*                                                                               
         CLI   PATLSTTY,C'M'       MARKET LIST                                  
         BNE   *+12                                                             
         MVI   SRTORD,X'40'                                                     
         B     LRR08                                                            
*                                                                               
         CLI   PATLSTTY,C'S'       STATION LIST                                 
         BNE   *+12                                                             
         MVI   SRTORD,X'50'                                                     
         B     LRR08                                                            
         DC    H'0'                                                             
*                                                                               
LRR08    SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               GET RID OF SUB LINE                          
         STCM  R1,3,SRTREFS                                                     
         MVC   SRTDKAD,KEY+14                                                   
         LA    R3,L'SRTREC(,R3)                                                 
         TM    WHEN,X'C0'          IF OFFLINE, NO RELOC                         
         BZ    BUG2                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R3,R9               MAKE RELATIVE                                
         B     *+14                                                             
BUG2     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         C     R3,NEXTMAX                                                       
         BL    LRR09                                                            
         TM    WHEN,X'C0'          IF ONLINE, TELL MUST RUN OFFLINE             
         BNZ   MAXLSTER                                                         
         DC    H'0'                                                             
*                                                                               
LRR09    ST    R3,NEXTPTR                                                       
         L     R1,PATLSTCT                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,PATLSTCT                                                      
         B     LR10                                                             
         DROP  R3                                                               
         EJECT                                                                  
LRRSRT   NTR1                                                                   
         L     R3,PATLSTCT                                                      
         CHI   R3,1                                                             
         BE    LRRSRT06                                                         
         L     R2,NEXTSTRT                                                      
         TM    WHEN,X'C0'          IF OFFLINE, NO RELOC                         
         BZ    BUG3                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R9                                                            
         B     *+14                                                             
BUG3     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,(R2),(R3),18,14,0                                     
LRRSRT06 L     RF,NEXTSTRT                                                      
         TM    WHEN,X'C0'          IF OFFLINE, NO RELOC                         
         BZ    BUG4                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    RF,R9                                                            
         B     *+14                                                             
BUG4     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FORCEHED,C'Y'                                                    
LRRSRT10 LR    R1,RF                                                            
         TM    WHEN,X'C0'          IF OFFLINE, NO RELOC                         
         BZ    BUG5                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R9                                                            
         B     *+14                                                             
BUG5     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R1,NEXTPTR                                                       
         ST    R3,PATLSTCT                                                      
         USING SORTREC,RF                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),SRTDKAD                                                
         CLC   KEY+14(4),=X'004C823A'                                           
         B     *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)                                                    
         DROP  RF                                                               
         EJECT                                                                  
*================================================================               
* FORMAT OFFLINE REPORT HERE                                                    
*================================================================               
                                                                                
LRR10    L     R0,AIO3             CLEAR THE PRINT BUFFER                       
         LHI   R1,6000                                                          
         SR    RE,RE                                                            
         LA    RF,X'40'            FILL BUFFER WITH SPACES                      
         SLL   RF,24                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         L     R4,AIO3             PRINT BUFFER ADDRESS                         
         USING PRTLINED,R4                                                      
*                                                                               
         MVI   ALLOWLIN,4                                                       
*                                                                               
         LA    R3,PATKPRD-PATKEY+KEY                                            
         BRAS  RE,GETPRD           OUTPUT WILL BE IN FLD                        
         LA    RE,PPRD                                                          
         CLI   CONREC,C'B'         TEST BPAT                                    
         BNE   *+8                                                              
         LA    RE,BPPRLN                                                        
         MVC   0(7,RE),FLD                                                      
*                                                                               
         LA    R3,PATKPRD2-PATKEY+KEY                                           
         BAS   RE,GETPRD                                                        
         LA    RE,PPRD2                                                         
         CLI   CONREC,C'B'         TEST BPAT                                    
         BNE   *+8                                                              
         LA    RE,BPPTLN                                                        
         MVC   0(7,RE),FLD                                                      
*                                                                               
LRR11    CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   LRR16               NO                                           
         MVI   ELCODE,X'F1'        CK ACTIVITY ELEMENT                          
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(5,0),(3,WORK) GET TODAY'S DATE                      
         USING ACTVD,R6                                                         
         CLC   ACTVADDT,WORK       WAS ADD TODAY                                
         BE    LRR16                                                            
*                                                                               
LRR12    CLC   ACTVCHDT,WORK       WAS CHANGE TODAY                             
         BE    LRR14                                                            
         CLI   SVT1PROF+3,C'Y'     PRINT CHANGES ONLY=Y                         
         BE    LRR90               YES, BYPASS                                  
         B     LRR16                                                            
*                                                                               
LRR14    LA    RE,PCODE-3+110                                                   
         CLI   CONREC,C'B'                                                      
         BNE   *+8                                                              
         LA    RE,BPCODE-3+110                                                  
         MVC   0(3,RE),=C'***'                                                  
*                                                                               
LRR16    DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         CLI   PATKCODE-PATKEY+KEY,0                                            
         BE    LRR20                                                            
*                                                                               
         LLC   R0,PATKCODE-PATKEY+KEY                                           
         EDIT  (R0),(3,FULL)       ASSUME COPYCODE=EST                          
         CLI   CONREC,C'B'                                                      
         BE    LRR18                                                            
*                                                                               
         STC   R0,PCODE+2                                                       
         TM    PATSTAT,X'10'       TEST COPYCODE=EST                            
         BZ    LRR20                                                            
         MVC   PCODE,FULL                                                       
         MVI   PCODE+3,C'E'                                                     
         B     LRR20                                                            
*                                                                               
LRR18    STC   R0,BPCODE                                                        
         TM    PATSTAT,X'10'       TEST COPYCODE=EST                            
         BZ    LRR20                                                            
         MVC   BPCODE(3),FULL                                                   
         MVC   BPCODE+110(3),=C'EST'                                            
*                                                                               
LRR20    LA    R2,PPERIOD                                                       
         CLI   CONREC,C'B'                                                      
         BNE   *+8                                                              
         LA    R2,BPPERIOD                                                      
         GOTO1 DATCON,DMCB,(3,PATSTART),(5,(R2))                                
         MVI   8(R2),C'-'                                                       
         MVC   9(3,R2),=C'UFN'                                                  
         CLC   PATEND,=XL3'FFFFFF'                                              
         BE    LRR26                                                            
*                                                                               
LRR24    GOTO1 (RF),(R1),(3,PATEND),(5,9(R2))                                   
*                                                                               
LRR26    SR    R0,R0                                                            
         ICM   R0,7,PATKREF-PATKEY+KEY                                          
         SRL   R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         LA    R2,PREF                                                          
         CLI   CONREC,C'B'                                                      
         BNE   *+8                                                              
         LA    R2,BPREF                                                         
         EDIT  (R0),(5,0(R2))                                                   
*                                                                               
         LA    R2,PDESC                                                         
         CLI   CONREC,C'B'                                                      
         BNE   *+8                                                              
         LA    R2,BPDESC                                                        
         MVC   0(L'PDESC,R2),PATDESC                                            
*                                                                               
         MVI   HASTIME,C'N'        SET FLAG FOR PATTERN HAS TIME                
         OC    PATSTIM(4),PATSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
         MVI   HASDPT,C'N'                                                      
         CLI   PATDPT,0                                                         
         BE    *+8                                                              
         MVI   HASDPT,C'Y'                                                      
*                                                                               
         CLI   PATDTALN,38                                                      
         BE    LRR28                                                            
         TM    PATSTAT,X'80'       SOFT DELETE                                  
         BZ    LRR28                                                            
         LA    R2,PCODE                                                         
         CLI   CONREC,C'B'                                                      
         LA    R2,BPCODE                                                        
         MVC   110(9,R2),DELMSG                                                 
*                                                                               
LRR28    ST    R4,PDATAST          SET A(FIRST PRINT LINE)                      
*                                                                               
         BRAS  RE,PMS              GO FORMAT MKT/STA LIST TO BUFFER             
*                                                                               
         BAS   RE,DCML             CHECK FOR DELETED COMMERCIALS                
*                                                                               
         TM    PRDMATSW,BPATSW     BPAT RECORD                                  
         BZ    *+12                                                             
         BRAS  RE,PBPAT            PRINT BPAT CMLS                              
         B     LRR70                                                            
                                                                                
* FIND CMML PCT ELEM                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    LRR30                                                            
*                                                                               
         MVI   ELCODE,X'36'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LRR32                                                            
LRR30    MVC   ELEM,0(R6)          MOVE PCTEL TO ELEM                           
                                                                                
LRR32    MVI   ELCODE,X'30'        FIND CMML ELEMENT                            
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR70                                                            
*                                                                               
         USING PATCMLEL,R6                                                      
         LLC   R2,PATCMLLN                                                      
         SRL   R2,4                GIVES NUMBER OF CMMLS IN R2                  
         LA    R6,PATCML           START OF LIST                                
         DROP  R6                                                               
*                                                                               
LRR33A   LA    R5,ALPHATAB                                                      
*                                                                               
         BAS   RE,NEXTPDTA         THIS CODE ONE TIME ONLY                      
         MVC   0(11,R4),=C'COMMERCIALS'                                         
         B     LRR34A                                                           
*                                                                               
LRR34    BAS   RE,NEXTPDTA         POINT R4 TO NEXT PDATA                       
*                                                                               
LRR34A   LA    R4,12(R4)           FIRST COMMERCIAL POSN                        
         LA    R0,2                SET FOR 2 CMMLS/LINE                         
*                                                                               
LRR36    ST    R4,FULL             SAVE START POSN THIS CMML                    
         MVC   0(1,R4),0(R5)       MOVE ALPHA CHAR                              
         MVI   1(R4),C'='                                                       
         MVC   2(8,R4),0(R6)                                                    
         CLC   =X'5C00',0(R6)                                                   
         BE    LRR36A                                                           
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   LRR36A                                                           
         GOTO1 VTRPACK,DMCB,(C'U',(R6)),2(R4)                                   
*                                                                               
LRR36A   LA    R4,11(R4)                                                        
         OC    8(8,R6),8(R6)       PIGGY-BACK CMML                              
         BZ    LRR38               NO                                           
*                                                                               
         CLI   0(R4),C' '          FIND LAST CHAR USED                          
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'-'                                                       
         MVC   2(8,R4),8(R6)                                                    
         CLC   =X'5C00',8(R6)                                                   
         BE    LRR38                                                            
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   LRR38                                                            
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),2(R4)                                  
*                                                                               
LRR38    L     R4,FULL             GET STARTING POSN                            
         CLI   ADIDFLAG,C'Y'       TEST ADID                                    
         BE    LRR38A                                                           
         LA    R4,11(R4)           PCT PRINTS HERE IF ONE ISCI                  
         OC    8(8,R6),8(R6)       TEST P/B                                     
         BZ    *+8                                                              
         LA    R4,9(R4)                                                         
         B     LRR38B                                                           
*                                                                               
LRR38A   LA    R4,15(R4)           PCT PRINTS HERE IF ONE ADID                  
         OC    8(8,R6),8(R6)       TEST P/B                                     
         BZ    LRR38B                                                           
         LA    R4,12(R4)                                                        
         LA    R0,1                SET TO MOVE TO NEXT LINE - NO ROOM           
*                                                                               
LRR38B   BAS   RE,GETPCT                                                        
*                                                                               
         AHI   R2,-1               DECREMENT REMAINING COUNT                    
         BNP   LRR40                                                            
         LA    R4,6(R4)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,16(R6)                                                        
         BCT   R0,LRR36                                                         
         B     LRR34                                                            
*                                                                               
LRR40    BAS   RE,NEXTPDTA                                                      
*                                                                               
         MVI   ELCODE,X'32'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    LRR42                                                            
*                                                                               
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR50                                                            
*                                                                               
         USING PATPTNEL,R6                                                      
LRR42    MVC   0(8,R4),=C'ROTATION'                                             
         LLC   R1,PATPTNLN                                                      
         AHI   R1,-3               GET ROT LEN -1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R4),2(R6)                                                   
*                                                                               
         BAS   RE,NEXTPDTA                                                      
*                                                                               
LRR50    DS    0C                                                               
         USING PATDTAEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    PATSTIM,PATSTIM                                                  
         BZ    LRR51                                                            
         XC    FULL,FULL                                                        
         XC    WORK,WORK                                                        
         MVC   FULL(2),PATSTIM                                                  
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   FULL(2),PATETIM                                                  
         GOTO1 (RF),(R1),,WORK+10                                               
                                                                                
         MVC   0(11,R4),=C'START TIME='                                         
         MVC   12(5,R4),WORK                                                    
         MVC   20(9,R4),=C'END TIME='                                           
         MVC   30(5,R4),WORK+10                                                 
         BAS   RE,NEXTPDTA                                                      
         DROP  R6                                                               
*                                                                               
LRR51    MVI   ELCODE,X'50'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR52                                                            
*                                                                               
         MVC   0(5,R4),=C'TEXT='                                                
         MVC   12(7,R4),2(R6)                                                   
         BAS   RE,NEXTPDTA                                                      
*                                                                               
LRR52    MVI   ELCODE,X'40'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR70                                                            
*                                                                               
         USING PATCMTEL,R6                                                      
LRR54    MVC   0(8,R4),=C'COMMENTS'                                             
*                                                                               
LRR56    MVC   12(1,R4),PATCMTNO                                                
         OI    12(R4),X'F0'                                                     
         MVI   13(R4),C'.'                                                      
         LLC   R1,PATCMTLN                                                      
         AHI   R1,-4                                                            
         DROP  R6                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R4),3(R6)                                                   
*                                                                               
         BAS   RE,NEXTPDTA                                                      
         BRAS  RE,NEXTEL                                                        
         BE    LRR56                                                            
*                                                                               
LRR70    TM    CMLFLAG,X'80'       ANY DELETED CMLS                             
         BZ    LRR80               NO                                           
*                                                                               
         BAS   RE,NEXTPDTA                                                      
         MVC   0(27,R4),=C'* NOTE COMMERCIAL DELETED *'                         
*                                                                               
         L     R2,ACMLTAB          1 FLAG/8 ISCII/ 12 ADID                      
*                                                                               
LRR74    OC    1(8,R2),1(R2)       ANY CML                                      
         BZ    LRR80                                                            
         TM    1(R2),NOCMLSW       CML NOT FOUND SW IS ON?                      
         BZ    LRR76                                                            
         BAS   RE,NEXTPDTA                                                      
         MVC   0(8,R4),1(R2)                                                    
*                                                                               
LRR76    LA    R2,21(R2)                                                        
         C     RE,ACMLTABX                                                      
         BL    LRR74                                                            
                                                                                
*=================================================================              
* NOW PRINT THE LINES OUT 4 AT A TIME                                           
* REMEMBER PRINT LINES IN BUFFER ARE 110 BYTES                                  
*=================================================================              
                                                                                
LRR80    L     R4,AIO3                                                          
         SR    R6,R6                                                            
*                                                                               
LRR82    CLC   0(110,R4),SPACES                                                 
         BE    LRR84                                                            
         LA    R4,110(R4)                                                       
         BCT   R6,LRR82                                                         
*                                                                               
LRR84    LPR   R6,R6                                                            
         L     R4,AIO3                                                          
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
*                                                                               
LRR85    LA    R0,4                                                             
         CR    R0,R6                                                            
         BNH   *+6                                                              
         LR    R0,R6                                                            
         LA    R1,P                                                             
         CLI   CONREC,C'B'         BPAT PRINTS +1                               
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
LRR86    MVC   0(110,R1),0(R4)     BUFFER HAS 110 CHARS                         
         LA    R1,132(R1)          P HAS 132 CHARS                              
         LA    R4,110(R4)                                                       
         BCT   R0,LRR86                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AHI   R6,-4                                                            
         BP    LRR85                                                            
*                                                                               
LRR90    OC    PATLSTCT,PATLSTCT                                                
         BZ    LR10                                                             
*                                                                               
         L     RF,NEXTPTR                                                       
         TM    WHEN,X'C0'          IF OFFLINE, NO RELOC                         
         BZ    BUG6                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    RF,R9                                                            
         B     *+14                                                             
BUG6     CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,L'SRTREC(RF)                                                  
         L     R3,PATLSTCT                                                      
         BCT   R3,LRRSRT10                                                      
*                                                                               
         ST    R3,PATLSTCT                                                      
         MVC   NEXTPTR,NEXTSTRT                                                 
         J     EXIT                                                             
*                                                                               
LREXIT   CLI   OFFLINE,C'Y'                                                     
         JNE   EXIT                                                             
*                                                                               
         MVC   NEXTPTR,NEXTSTRT                                                 
*                                                                               
         L     R0,SORTSIZE                                                      
         STORAGE RELEASE,LENGTH=(R0),ADDR=NEXTSTRT,COND=YES                     
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
NEXTPDTA L     R4,PDATAST                                                       
         CLC   0(110,R4),SPACES                                                 
         JE    *+12                                                             
         LA    R4,110(R4)                                                       
         ST    R4,PDATAST                                                       
         LA    R4,PDATA-PRTLINED(R4)                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
*===========================================================                    
* ON ENTRY R5 POINTS TO ALPHATAB ENTRY                                          
* FORMAT PCT AT 1(R4)                                                           
*===========================================================                    
                                                                                
GETPCT   NTR1                                                                   
         CLI   ELEM,0              TEST NO PCTS                                 
         BE    GETPCTX                                                          
         LA    R6,ELEM+2                                                        
*                                                                               
GETPCT2  CLC   0(1,R5),0(R6)       MATCH LETTER                                 
         BE    GETPCT4                                                          
         BL    GETPCTX             CMML MUST BE DELETED                         
         LA    R6,3(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   GETPCT2                                                          
         B     GETPCTX                                                          
*                                                                               
GETPCT4  LA    RE,1(R4)                                                         
         MVI   0(RE),C'('                                                       
         LLC   R0,2(R6)                                                         
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT                                        
         AR    RE,R0                                                            
         MVI   1(RE),C')'                                                       
*                                                                               
GETPCTX  J     EXIT                                                             
         EJECT                                                                  
*=========================================================                      
* FORMAT ONLINE LIST LINE HERE                                                  
*=========================================================                      
                                                                                
LRL      MVI   RCSUBPRG,1                                                       
         MVC   LISTAR,SPACES                                                    
         L     R4,AIO                                                           
         USING PATKEY,R4                                                        
*                                                                               
         LA    R3,PATKPRD                                                       
         BAS   RE,GETPRD           OUTPUT WILL BE IN FLD                        
         MVC   LPRLN,FLD                                                        
*                                                                               
         LA    R3,PATKPRD2                                                      
         BAS   RE,GETPRD                                                        
         MVC   LPTLN,FLD                                                        
*                                                                               
         MVC   LCODE+1(1),PATKCODE    IN CASE IT'S REALLY COPYCODE              
*                                                                               
         EDIT  (B2,BREF),(3,LREF)                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R6,FULL             SAVE 10EL ADDRESS                            
         USING PATDTAEL,R6                                                      
         CLI   PATKCODE,0                                                       
         BE    LRL10                                                            
         TM    PATSTAT,X'10'       TEST COPYCODE=EST                            
         BZ    LRL10                                                            
         EDIT  (B1,PATKCODE),(3,LCODE)                                          
         MVI   LCODE+3,C'E'                                                     
*                                                                               
LRL10    GOTO1 DATCON,DMCB,(3,PATSTART),(5,LPER)                                
         MVI   LPER+8,C'-'                                                      
         CLC   PATEND,=XL3'FFFFFF'                                              
         BNE   LRL20                                                            
         MVC   LPER+9(3),=CL3'UFN'                                              
         B     LRL22                                                            
*                                                                               
LRL20    GOTO1 (RF),(R1),(3,PATEND),(5,LPER+9)                                  
*                                                                               
LRL22    MVC   LDESC,PATDESC                                                    
*                                                                               
         MVI   HASTIME,C'N'        SET FLAG FOR PATTERN HAS TIME                
         OC    PATSTIM(4),PATSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
         MVI   HASDPT,C'N'                                                      
         CLI   PATDPT,0                                                         
         BE    *+8                                                              
         MVI   HASDPT,C'Y'                                                      
*                                                                               
         CLI   PATDTALN,38                                                      
         BE    LRL24                                                            
         TM    PATSTAT,X'80'       SOFT DELETE                                  
         BZ    LRL24                                                            
         MVC   LDELETE,DELMSG                                                   
*                                                                               
LRL24    BAS   RE,DCML             CHECK FOR DELETED COMMERCIALS                
*                                                                               
         CLC   LDELETE,SPACES      ANY MESSAGE THERE                            
         BNE   LRL40               YES                                          
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,LTYPTABN         DISPLAY PATTERN TYPE                         
         LA    R1,LTYPTAB                                                       
*                                                                               
LRL26    CLC   2(1,R6),0(R1)                                                    
         BE    LRL28                                                            
         LA    R1,L'LTYPTAB(R1)                                                 
         BCT   R0,LRL26                                                         
         DC    H'0'                                                             
*                                                                               
LRL28    MVC   LDELETE(3),1(R1)                                                 
         CLI   0(R1),C'M'          IS THIS A MARKET LIST                        
         BNE   LRL30                                                            
         OC    3(5,R6),3(R6)       THIS ALL MARKETS                             
         BNZ   LRL30                                                            
         MVC   LDELETE(3),=C'ALL'                                               
*                                                                               
LRL30    L     R6,FULL             GET 10 ELEM ADDRESS                          
         USING PATDTAEL,R6                                                      
         CLI   PATDPT,0            TEST FOR DAYPART                             
         BE    LRL32               NO                                           
         MVC   LDELETE+4(4),=C'DPT='                                            
         MVC   LDELETE+8(1),PATDPT                                              
         B     LRL40                                                            
*                                                                               
LRL32    OC    PATSTIM(4),PATSTIM  TEST FOR TIMES                               
         BZ    LRL40                                                            
         GOTO1 UNTIME,DMCB,PATSTIM,WORK                                         
         MVC   LDELETE+4(7),WORK   SHOW AS MUCH AS WILL FIT                     
*                                                                               
LRL40    GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
*                                                                               
         L     R1,ASVCLIST                                                      
*                                                                               
LRL42    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'               MUST FIND THIS PRD                            
         CLC   3(1,R1),PATKPRD                                                  
         BE    LRL44                                                            
         LA    R1,4(R1)                                                         
         B     LRL42                                                            
*                                                                               
LRL44    LA    R1,4(R1)        BUMP TO NEXT PRD                                 
         SR    R1,R9                                                            
         ST    R1,SVR2                                                          
         B     LR10                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* BUMP KEY FOR NEXT RECORD *                                                    
*                                                                               
NXK      LA    R4,KEY                                                           
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         STH   R1,BREF                                                          
         SH    R1,=H'1'            BUILD NEXT KEY                               
         BP    NXK20               IF STILL NOT ZERO, OK                        
         LLC   R1,PATKCODE         BUMP CODE                                    
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,PATKCODE                                                      
         CLI   PATKCODE,0          IF OVER 255, BUMP SPOT LEN                   
         BNE   NXK10                                                            
         LLC   R1,PATKSLN2        BUMP SPOT LEN2                                
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,PATKSLN2                                                      
NXK10    SR    R1,R1               SET BREF                                     
         B     *+8                         ZERO AND LEAVE IT ZERO               
NXK20    X     R1,=XL4'00003FFF'             RESET REF TO 1'S COMPL             
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,PATKREF                                                     
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE COPY CODE (MAY BE ESTIMATE IF T1 PROFILE 12 ON) *                    
*                                                                               
VCC      NTR1                                                                   
         MVI   CODE,0                                                           
         MVI   CODESW,C'N'                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                                                             
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VCC30                                                            
*                                                                               
         GOTO1 VALINUM                                                          
         MVC   CODE,ACTUAL                                                      
         MVI   CODESW,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
VCC30    GOTO1 ANY                                                              
*                                                                               
         MVC   CODE,WORK                                                        
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* PRINT PRODUCT CODE AND SPOT LEN                                               
*=============================================================                  
                                                                                
GETPRD   NTR1                                                                   
         XC    FLD,FLD                                                          
         LA    R5,FLD              ADDRESS OF OUTPUT AREA                       
*                                                                               
         CLI   0(R3),0             ANY PRODUCT CODE                             
         JE    EXIT                NO,DONE                                      
         L     R1,ASVCLIST         ADDRESS OF SAVED CLIST                       
*                                                                               
GETPRD2  CLI   0(R1),C' '                                                       
         BNH   GETPRD4                                                          
         CLC   0(1,R3),3(R1)                                                    
         JE    GETPRD10                                                         
         LA    R1,4(R1)                                                         
         J     GETPRD2                                                          
*                                                                               
GETPRD4  BRAS  R1,*+8                                                           
         DC    CL4'??? '                                                        
*                                                                               
GETPRD10 MVC   0(3,R5),0(R1)                                                    
*                                                                               
         CLI   1(R3),0             ANY SPOT LEN                                 
         JE    GETPRDX             NO                                           
         LA    R5,2(R5)                                                         
         CLI   0(R5),C' '                                                       
         JNH   GETPRD12                                                         
         LA    R5,1(R5)                                                         
*                                                                               
GETPRD12 MVI   0(R5),C'-'                                                       
*                                                                               
         LLC   R0,1(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R5),DUB                                                      
         CLI   1(R5),C'0'                                                       
         JNE   GETPRDX                                                          
         MVC   1(2,R5),2(R5)                                                    
         MVI   3(R5),C' '                                                       
*                                                                               
GETPRDX  J     EXIT                                                             
*                                                                               
* PRINT PERIOD INTO WORK                                                        
*                                                                               
PPER     NTR1                                                                   
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,(R2)),(5,WORK)                                    
         MVI   WORK+8,C'-'                                                      
         CLC   =XL3'FFFFFF',3(R2)                                               
         BNE   PPER10                                                           
         MVC   WORK+9(3),=CL3'UFN'                                              
         B     EXIT                                                             
PPER10   GOTO1 (RF),(R1),(3,3(R2)),(5,WORK+9)                                   
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================                     
* CHECK FOR DELETED COMMERCIALS                                                 
* BUILD CML TABLE WITH ADID IF ANY                                              
*==========================================================                     
                                                                                
DCML     NTR1                                                                   
         MVI   CMLFLAG,0           INIT DELETED CML FOUND                       
         MVI   BYTE,0                                                           
         MVI   ADIDFLAG,C'N'                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PATSTAT1-PATDTAEL(R6),PATSADID                                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         L     RE,ACMLTAB          CLEAR CML TABLE                              
         L     RF,ACMLTABX                                                      
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO              (B)PAT REC - COMMERCIAL LIST ELEM            
         TM    15(R6),X'02'        INCOMPLETE RECORD?                           
         BZ    DCML02                                                           
         OI    PRDMATSW,NOCMLSW    NO CML FOUND - INCOMPLETE REC                
         B     DCML34                                                           
*                                                                               
DCML02   TM    PRDMATSW,BPATSW     BPAT RECORD                                  
         BO    DCML03                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         LLC   R3,PATCMLLN         GET ELEM LEN                                 
         SRL   R3,3                DIV BY 8=NO OF CMML PRS (DROPS ODD)          
         LA    R4,PATCML           1ST CMML                                     
         B     DCML04                                                           
         DROP  R6                                                               
*                                                                               
DCML03   MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATBCMEL,R6                                                      
         LLC   R3,PATBCMLN         GET ELEM LEN                                 
         SRL   R3,3                DIV BY 8=NO OF CMML PRS (DROPS ODD)          
         LA    R4,PATBCML           1ST CMML                                    
         DROP  R6                                                               
*                                                                               
         MVC   BYTE,ELCODE         SAVE THIS ELCODE                             
         ST    R6,SVREG            SAVE R6 PTR                                  
*                                                                               
DCML04   CLC   =C'HIATUS',0(R4)    HIATUS                                       
         BE    DCML50                                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2            USE I/O 2                                    
*                                                                               
DCML10   OC    0(8,R4),0(R4)       IS THIS A CML                                
         BZ    DCML40                                                           
*                                                                               
         CLC   =X'5C00',0(R4)      DELETED CML                                  
         BE    DCML40                                                           
*                                                                               
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
*                                                                               
         MVC   CMLKID,=XL2'0A21'                                                
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   CMLKID,=XL2'0AC1'                                                
*                                                                               
         L     R2,ACMLTAB          POINT TO CMML TABLE                          
*                                                                               
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,0(R4)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DCML15              JUST IGNORE CMML NOT FOUND                   
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2             CML REC                                      
         MVI   ELCODE,X'A0'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCML25                                                           
         USING CMLADIEL,R6                                                      
*                                                                               
         OC    CMLADID,CMLADID     WANT TO BE SURE                              
         BZ    DCML25                                                           
*                                                                               
DCML15   OC    1(8,R2),1(R2)       ANY CML                                      
         BZ    DCML20                                                           
         CLC   1(8,R2),0(R4)       SAME CML                                     
         BE    DCML25                                                           
         LA    R2,21(R2)                                                        
         C     R2,ACMLTABX                                                      
         BL    DCML15                                                           
         DC    H'0'                MAKE TABLE BIGGER                            
*                                                                               
DCML20   MVC   1(8,R2),0(R4)       8 CHAR ISCII                                 
         CLC   KEY(13),KEYSAVE     IF CMML NOT FOUND                            
         BNE   DCML36              GET OUT NOW!                                 
         MVC   9(12,R2),CMLADID    12 CHAR ADID                                 
*                                                                               
DCML25   CLC   KEY(13),KEYSAVE     IF CMML NOT FOUND                            
         BNE   DCML36              GET OUT NOW!                                 
*                                                                               
         L     R6,AIO2             CML REC                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BZ    DCML40              NO                                           
         CLI   MODE,LISTRECS                                                    
         BE    DCML36                                                           
*                                                                               
         OI    0(R2),X'01'         TURN ON DEL CML FLAG IN CML TABLE            
         OI    CMLFLAG,X'80'       DELETED CML FOUND                            
         OC    CMLADID,CMLADID                                                  
         BNZ   DCML40                                                           
         MVC   1(8,R2),0(R4)       SAVE 8 CHAR ISCII                            
         B     DCML40                                                           
*                                                                               
DCML34   XC    LDELETE,LDELETE                                                  
         MVC   LDELETE(6),=C'NO-CML '                                           
         B     DCML50                                                           
*                                                                               
DCML36   XC    LDELETE,LDELETE                                                  
         MVC   LDELETE(7),=C'DEL-CML'                                           
*                                                                               
DCML40   CLI   HASTIME,C'Y'        TEST PATTERN HAS TIME                        
         BE    *+12                                                             
         CLI   HASDPT,C'Y'                                                      
         BNE   DCML42                                                           
*                                                                               
         MVI   ELCODE,X'B0'        FIND MATCHING TIME ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DCML42                                                           
*                                                                               
         USING CMLMATEL,R6                                                      
         OC    CMLMSTIM,CMLMSTIM                                                
         BZ    DCML42                                                           
         OI    0(R2),X'80'         SET FLAG IN TABLE                            
         MVC   LDELETE(15),=C'=>CMML HAS TIME'                                  
*                                                                               
DCML42   LA    R4,8(R4)                                                         
         BCT   R3,DCML10                                                        
*                                                                               
         CLI   BYTE,X'31'                                                       
         BNE   DCML45                                                           
         MVI   ELCODE,X'31'                                                     
         L     R6,SVREG            RESTORE R6 PTR                               
         BAS   RE,NEXTEL                                                        
         BNE   DCML45                                                           
         ST    R6,SVREG            SAVE R6 PTR                                  
*                                                                               
         USING PATBCMEL,R6                                                      
         LLC   R3,PATBCMLN         GET ELEM LEN                                 
         SRL   R3,3                DIV BY 8=NO OF CMML PRS (DROPS ODD)          
         LA    R4,PATBCML           1ST CMML                                    
         B     DCML10                                                           
         DROP  R6                                                               
*                                                                               
DCML45   L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   KEY(13),0(R1)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   KEY(L'SVKEY),SVKEY                                               
         B     EXIT                                                             
*                                                                               
* IF ONLINE LIST, INDICATE HIATUS PATTERN *                                     
*                                                                               
DCML50   CLI   MODE,LISTRECS                                                    
         BNE   EXIT                                                             
         TM    PRDMATSW,NOCMLSW    NO CML FOUND - INCOMPLETE REC                
         BO    EXIT                                                             
         XC    LDELETE,LDELETE                                                  
         MVC   LDELETE(6),=C'HIATUS'                                            
         B     EXIT                                                             
*                                                                               
MAXLSTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAXLSTMS),MAXLSTMS                                     
         LA    R2,CONWHENH                                                      
         GOTO1 ERREX2                                                           
MAXLSTMS DC    C'* ERROR * TOO LARGE TO RUN ONLINE, RUN OFFLINE'                
PRDSQERR MVI   ERROR,INVPRDSQ      PRODS OUT OF SEQ                             
         B     PRDERR                                                           
EQPRDERR MVI   ERROR,INVEQPRD      PROD/PARTNER PROD EQ                         
PRDERR   LA    R2,TRAPRLNH         POINT TO PROD-SPOT LEN                       
         B     TRAPERR                                                          
*                                                                               
NOPBERR  MVC   GERROR,=Y(NOPBTHTR)  NO P/B FOR THEATRICAL PRODUCTS              
         LA    R2,TRAPTLNH                                                      
         J     ERREXT1                                                          
BPATERR  MVC   GERROR,=Y(BPATTHTR)  USE BPAT FOR THEATRICAL PRODUCTS            
         LA    R2,CONRECH                                                       
         J     ERREXT1                                                          
UPATERR  MVC   GERROR,=Y(UPATTERR) USE PAT FOR NON THEATRICAL PRODUCTS          
         LA    R2,CONRECH                                                       
ERREXT1  GOTO1 VTRAERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
LTYPTAB  DS    0F                                                               
         DC    C'A',CL3'AFF'                                                    
         DC    C'C',CL3'CMB'                                                    
         DC    C'G',CL3'MGR'                                                    
         DC    C'M',CL3'MKT'                                                    
         DC    C'S',CL3'STA'                                                    
         DC    C'T',CL3'STY'                                                    
LTYPTABN EQU   (*-LTYPTAB)/4                                                    
ALPHATAB DC    CL15'ABCDEFGHIJKLMNO'  15 LETTERS FOR 15 COMMERCIALS             
DELCML   DS    0XL16                                                            
         DC    C'*'                                                             
         DC    XL15'00'                                                         
DELMSG   DS    0CL9                                                             
         DC    C'*'                                                             
DELETE   DC    CL6'DELETE'                                                      
         DC    C'D*'                                                            
         EJECT                                                                  
HEADING  DS    0D                                                               
         SPROG 1,2                                                              
         SSPEC H1,2,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,40,C'P A T T E R N   L I S T'                                 
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,2,C'MEDIA'                                                    
         SSPEC H2,40,C'-----------------------'                                 
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
*                                                                               
         SPROG 1                   PATTERN LIST                                 
         SSPEC H8,2,C'PRD-LEN'                                                  
         SSPEC H9,2,C'PTR-LEN'                                                  
         SSPEC H8,11,C'COPY'                                                    
         SSPEC H9,11,C'CODE'                                                    
         SSPEC H8,18,C'REF'                                                     
         SSPEC H9,18,C'---'                                                     
         SSPEC H8,23,C'PERIOD/DESC'                                             
         SSPEC H9,23,C'-----------------'                                       
*                                                                               
         SPROG 2                   BPAT LIST                                    
         SSPEC H8,3,C'PRD-'                                                     
         SSPEC H8,10,C'PTR-'                                                    
         SSPEC H8,18,C'COPY'                                                    
         SSPEC H8,24,C'PERIOD/DESC'                                             
         SSPEC H8,43,C'REF'                                                     
         SSPEC H8,49,C'MKT/STATION LIST'                                        
         SSPEC H8,83,C'COMMERCIALS'                                             
         SSPEC H8,108,C'ROT'                                                    
         SSPEC H9,4,C'LEN'                                                      
         SSPEC H9,11,C'LEN'                                                     
         SSPEC H9,18,C'CODE'                                                    
         SSPEC H9,24,C'-----------------'                                       
         SSPEC H9,42,C'-----'                                                   
         SSPEC H9,49,C'---------------------------------'                       
         SSPEC H9,83,C'------------------------ ----'                           
*                                                                               
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
*==========================================================                     
* PRINT BPAT COMMERCIALS AND PERCENTS                                           
*==========================================================                     
                                                                                
         USING PRTLINED,R4                                                      
*                                                                               
PBPAT    NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO              ROTATION ELEMENT                             
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LLC   R1,1(R6)            ELEM LEN                                     
         SHI   R1,3                ADJ FOR EX MOVE                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),2(R6)                                                    
*                                                                               
         LAY   R2,SPTRRTAB         LETTER ROTATION TABLE                        
         LA    R3,ELEM             ROTATION ELEMENT POINTER                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
                                                                                
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATBCMEL,R6                                                      
PB20     ST    R6,NEXTELEM         SAVE CURRENT ROTATION ELEMENT                
*                                                                               
         LLC   R5,PATBCMLN                                                      
         SRL   R5,4                DIV BY 16 TO GET ENTRIES (DROPS ODD)         
         LA    R6,PATBCML          START OF LIST                                
         DROP  R6                                                               
*                                                                               
PB22     MVC   BPCMLA(2),0(R2)     LETTER ROTATION                              
         MVI   BPCMLA+2,C'='                                                    
         MVC   BPCML(8),0(R6)         COMMERCIAL                                
*                                                                               
         CLI   0(R6),C'*'                                                       
         BE    PB23                                                             
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PB23                                                             
         GOTO1 VTRPACK,DMCB,(C'U',0(R6)),BPCML                                  
*                                                                               
PB23     L     RE,ACMLTAB          LOOK FOR ADID                                
         L     RF,ACMLTABX                                                      
*                                                                               
PB24     OC    1(8,RE),1(RE)       ANY ENTRY                                    
         BZ    PB30                                                             
         CLC   1(8,RE),0(R6)       SAME CML                                     
         BNE   PB26                                                             
         OC    9(12,RE),9(RE)      ANY ADID FOR THIS CML                        
         BNZ   PB28                                                             
*                                                                               
PB26     LA    RE,21(RE)                                                        
         CR    RE,RF                                                            
         BL    PB24                                                             
         B     PB30                                                             
*                                                                               
PB28     DS    0H                                                               
         CLI   BPCML+8,C' '                                                     
         BNE   PB30                                                             
         MVC   BPADID,9(RE)        PRINT ADID                                   
*                                                                               
PB30     LLC   R0,2(R3)            GET PCT VALUE                                
         MVC   BPPCT,=C'100'                                                    
         CLI   2(R3),100                                                        
         BE    PB40                                                             
*                                                                               
PB32     MHI   R0,10                                                            
         EDIT  (R0),(4,BPPCT),1                                                 
*                                                                               
PB40     LA    R2,3(R2)            NEXT ROT TABLE ENTRY                         
         LA    R3,3(R3)            NEXT ROT ELEM SLOT                           
         LA    R4,110(R4)          NEXT PRINT LINE                              
         LA    R6,16(R6)           BUMP IN COMMERCIAL ELEMENT                   
         BCT   R5,PB22                                                          
*                                                                               
         L     R6,NEXTELEM                                                      
         BRAS  RE,NEXTEL                                                        
         BE    PB20                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    PATSTIM,PATSTIM                                                  
         BZ    PBX                                                              
         XC    FULL,FULL                                                        
         XC    WORK,WORK                                                        
         MVC   FULL(2),PATSTIM                                                  
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   FULL(2),PATETIM                                                  
         GOTO1 (RF),(R1),,WORK+10                                               
                                                                                
         LA    R6,77(R4)                                                        
         MVC   0(11,R6),=C'START TIME='                                         
         MVC   12(5,R6),WORK                                                    
         MVC   18(9,R6),=C'END TIME='                                           
         MVC   28(5,R6),WORK+10                                                 
         BRAS  RE,NEXTPDTA                                                      
         DROP  R6                                                               
PBX      J     EXIT                                                             
         LTORG                                                                  
                                                                                
*=================================================================              
* PRINT MARKET/STATION LIST IN BUFFER                                           
*=================================================================              
                                                                                
         USING PRTLINED,R4                                                      
PMS      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,PDATA-PRTLINED                                                
         CLI   CONREC,C'B'                                                      
         BNE   *+8                                                              
         LA    R0,BPSTLST-PRTLINED                                              
         AR    R4,R0               POINT TO STARTING POSITION                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        MARKET/STATION LIST                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATLSTEL,R6                                                      
*                                                                               
         MVC   MSCT,PATLSTTY       SAVE LIST TYPE                               
         CLC   =C'DOWN',CONOUT                                                  
         BNE   PMS2                                                             
         L     R4,PDATAST                                                       
         MVC   0(1,R4),PATLSTTY                                                 
         LA    R4,1(R4)                                                         
         ST    R4,PDATAST          UPDATE BUFFER START                          
*                                                                               
PMS2     CLI   PATLSTTY,C'T'       STATION TYPE                                 
         BNE   PMS10                                                            
         CLC   =C'DOWN',CONOUT                                                  
         BE    PMS4                                                             
         MVC   0(5,R4),=CL5'TYPE='                                              
         LA    R4,5(R4)                                                         
*                                                                               
PMS4     MVC   0(1,R4),PATLST      MOVE IN STATION TYPE                         
         B     PMSX                                                             
*                                                                               
PMS10    CLI   PATLSTTY,C'A'       AFFILIATE                                    
         BNE   PMS20                                                            
*                                                                               
         LLC   R0,PATLSTLN                                                      
         AHI   R0,-8                                                            
         LA    R1,PATLST                                                        
         CLC   =C'DOWN',CONOUT                                                  
         BE    PMS14                                                            
         MVC   0(4,R4),=CL4'AFF='                                               
         LA    R4,4(R4)                                                         
*                                                                               
PMS14    MVC   0(3,R4),0(R1)   MOVE IN AFFILIATE                                
         LTR   R0,R0               ANY MORE LEFT                                
         BZ    PMSX                 NO                                          
         MVI   3(R4),C','                                                       
         LA    R1,5(R1)                                                         
         LA    R4,4(R4)                                                         
         AHI   R0,-5                                                            
         B     PMS14                                                            
*                                                                               
PMS20    LLC   RF,PATLSTLN                                                      
         SR    RE,RE                                                            
         D     RE,=F'5'            RF HAS NUMBER OF ENTRIES THIS ELEM           
         LA    RE,PATLST                                                        
*                                                                               
         CLI   PATLSTTY,C'M'                                                    
         BNE   PMS30                                                            
         OC    PATLST(5),PATLST                                                 
         BNZ   PMS30                                                            
         MVC   0(3,R4),=C'ALL'                                                  
         CLI   CONREC,C'B'                                                      
         BE    PMSX                                                             
         CLC   =C'DOWN',CONOUT                                                  
         BE    PMSX                                                             
         MVC   0(6,R4),=C'MARKET'                                               
         MVC   12(11,R4),=C'ALL MARKETS'                                        
         B     PMSX                                                             
*                                                                               
PMS30    CLI   CONREC,C'B'         TEST BPAT                                    
         BE    PMS40                                                            
         CLC   =C'DOWN',CONOUT                                                  
         BE    PMS40                                                            
*                                                                               
         CLI   MSCT,C'S'                                                        
         BNE   *+10                                                             
         MVC   0(7,R4),=C'STATION'                                              
         CLI   MSCT,C'M'                                                        
         BNE   *+10                                                             
         MVC   0(6,R4),=C'MARKET'                                               
         CLI   MSCT,C'C'                                                        
         BNE   *+10                                                             
         MVC   0(7,R4),=C'MKT/AFF'                                              
         CLI   MSCT,C'G'                                                        
         BNE   *+10                                                             
         MVC   0(7,R4),=C'MKT GRP'                                              
         LA    R4,12(R4)           FIRST LIST ITEM                              
         DROP  R6                                                               
*                                                                               
PMS40    CLI   MSCT,C'C'           COMBINED MKT/AFF                             
         BE    *+12                 YES                                         
         CLI   MSCT,C'M'           MARKET                                       
         BNE   PMS46                NO, STATION                                 
*                                                                               
         CLI   0(RE),0             THIS AN AFFILIATE                            
         BNE   PMS44                YES                                         
*                                                                               
         SR    R0,R0               MARKET                                       
         ICM   R0,3,3(RE)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CLC   =C'DOWN',CONOUT                                                  
         BE    *+12                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PMS45                                                            
         UNPK  0(4,R4),DUB                                                      
         LA    R4,4(R4)                                                         
         B     PMS60                                                            
*                                                                               
PMS44    MVC   0(3,R4),0(RE)       AFFILIATE                                    
         LA    R4,3(R4)                                                         
         B     PMS60                                                            
*                                                                               
PMS45    STM   RE,R0,SVREGS                                                     
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO2                     
*                                                                               
         L     R1,AIO2                                                          
         USING MKTRECD,R1                                                       
         MVC   0(4,R4),MKTKMKT                                                  
         MVI   4(R4),C'='                                                       
         MVC   5(24,R4),=CL24'UNKNOWN MARKET'                                   
         CLC   KEY(8),0(R1)                                                     
         BNE   *+10                                                             
         MVC   5(19,R4),MKTNAME    MOVE ONLY 19 CHARS SO GET 2-UP               
         DROP  R1                                                               
*                                                                               
         LM    RE,R0,SVREGS                                                     
         MVC   KEY(L'SVKEY),SVKEY                                               
         LA    R4,24(R4)                                                        
         B     PMS60                                                            
*                                                                               
PMS46    CLI   MSCT,C'G'           MARKET GROUP                                 
         BE    PMS50                                                            
         CLI   MSCT,C'S'           STATION LIST                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    0(2,RE),0(RE)       IS THIS A CABLE HEAD STATION                 
         BNZ   PMS48                                                            
*                                                                               
         STM   RE,R0,SVREGS                                                     
         LR    R0,RE                                                            
         GOTO1 MSUNPK,DMCB,(X'80',(R0)),WORK,WORK+4                             
         LM    RE,R0,SVREGS                                                     
*                                                                               
         MVC   0(8,R4),WORK+4                                                   
         MVI   4(R4),C'/'                                                       
         LA    R4,7(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   PMS60                                                            
         LA    R4,1(R4)                                                         
         B     PMS60                                                            
*                                                                               
PMS48    MVC   0(4,R4),0(RE)       MOVE IN 1ST 4 LETTERS                        
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '          SEE IF ONLY THREE LETTERS                    
         BNH   *+8                 YES                                          
         LA    R4,1(R4)            BUMP OVER 4TH LETTER                         
         CLI   4(RE),C'T'          IF TV                                        
         BE    PMS60               ONLY PRINT STATION LETTERS                   
         MVI   0(R4),C'-'                                                       
         MVC   1(1,R4),4(RE)                                                    
*MNMB                                                                           
         MVI   2(R4),C'V'                                                       
         CLI   4(RE),C'D'                                                       
         BE    *+8                                                              
*MNMB                                                                           
         MVI   2(R4),C'M'                                                       
         LA    R4,3(R4)                                                         
         B     PMS60                                                            
*                                                                               
PMS50    STM   RE,R1,SVREGS                                                     
*                                                                               
* LOOK UP 1 CHAR MARKET GROUP IN THE CONVERT TABLE                              
*                                                                               
         LAY   R1,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
PMS52    CLC   0(1,RE),2(R1)      IS THIS IT                                    
         BE    PMS54                                                            
         LA    R1,3(R1)                                                         
         BCT   RF,PMS52                                                         
         DC    H'0'                                                             
*                                                                               
PMS54    MVC   0(2,R4),0(R1)       MOVE IN LETTER OF MARKET GROUP               
         LM    RE,R1,SVREGS        RESTORE REGS                                 
*                                                                               
         UNPK  DUB(5),1(3,RE)                                                   
*                                                                               
         CLI   1(R4),X'40'         SEE IF SECOND CHAR IS BLANK                  
         BNH   PMS56                                                            
         MVC   2(4,R4),DUB         MOVE 4 DIGITS AFTER 2 CHAR MKT GRP           
         LA    R4,6(R4)                                                         
         B     PMS60                                                            
*                                                                               
PMS56    MVC   1(4,R4),DUB         THEN 4 DIGITS                                
         LA    R4,5(R4)                                                         
*                                                                               
PMS60    LA    RE,5(RE)            POINT TO NEXT IN PATLST                      
         CLC   =C'DOWN',CONOUT                                                  
         BE    PMS70                                                            
*                                                                               
         L     R0,PDATAST          START OF THIS LINE                           
         AHI   R0,110              MAX CHARS PER LINE                           
         SR    R0,R4               GIVES REMAINING CHARS THIS LINE              
         CHI   R0,8                AT LEAST 8 SPACES REMAINING                  
         BNH   PMS62               NO                                           
         CLI   MSCT,C'M'           MARKET                                       
         BNE   *+12                                                             
         CHI   R0,25                                                            
         BL    PMS62                                                            
*                                                                               
PMS61    MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         B     PMS66                                                            
*                                                                               
PMS62    CLI   CONREC,C'B'                                                      
         BE    PMS64                                                            
*                                                                               
         STM   RE,R0,SVREGS                                                     
         BRAS  RE,NEXTPDTA                                                      
         LM    RE,R0,SVREGS                                                     
         LA    R4,12(R4)            FIRST OUTPUT POSN                           
         B     PMS66                                                            
*                                                                               
PMS64    L     R4,PDATAST                                                       
         LA    R4,110(R4)                                                       
         LA    R4,BPSTLST-PRTLINED(R4)                                          
*                                                                               
PMS66    BCT   RF,PMS40                                                         
         B     PMS80                                                            
*                                                                               
* THIS CODE FOR DOWNLOADS                                                       
*                                                                               
PMS70    L     R0,PDATAST          START OF THIS LINE                           
         AHI   R0,72               DOWNLOAD HAS 72 CHAR LINES                   
         SR    R0,R4               GIVES REMAINING CHARS THIS LINE              
         CHI   R0,8                AT LEAST 8 SPACES REMAINING                  
         BNL   PMS61               YES                                          
         L     R4,PDATAST                                                       
         LA    R4,72(R4)           SET START OF NEXT LINE                       
         ST    R4,PDATAST                                                       
         B     PMS66                                                            
*                                                                               
PMS80    BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
*                                                                               
PMSX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
* HEADING ROUTINE FOR REPORTS                                                   
*                                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
         MVC   H3+45(10),=CL10'DATE ORDER'                                      
         TM    FLAGFTR,X'20'                                                    
         BO    HDHK10                                                           
         MVC   H3+45(4),=CL4' REF'                                              
HDHK10   MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
*                                                                               
         CLI   CONREC,C'B'                                                      
         BE    HDHK15                                                           
         MVC   H8+107(3),SPACES    CLEAR ROT                                    
         MVC   H9+107(4),SPACES                                                 
*                                                                               
HDHK15   CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   HDHKX                  NO                                        
         MVC   H4+45(11),=C'TURN-AROUND'                                        
HDHKX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------                          
* VALIDATE REFERENCE NUMBER                                                     
*-----------------------------------------------------                          
VREF     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT(6),BCLT                                                  
*        MVC   PATKPRD(2),BPRD     PRODUCT/SPOT LENGTH                          
*        MVC   PATKPRD2(2),BPRD2   PARTNER PRODUCT/SPOT LENGTH                  
         MVC   PATKCODE,CODE                                                    
         XC    BREFSUB,BREFSUB                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VREF10                                                           
         CLI   ACTNUM,ACTLIST      IF LIST, NOT NEEDED                          
         BE    VREFX                                                            
VREF10   TM    4(R2),X'08'         WAS IT NUMERIC                               
         BZ    NUMERRA                                                          
         GOTO1 ANY                                                              
         LLC   R1,5(R2)            GET DATA LENGTH                              
         BCTR  R1,0                                                             
         EX    R1,VKPACK                                                        
         CVB   R0,DUB                                                           
         CH    R0,=H'16383'                                                     
         BH    BIGERR                                                           
         STH   R0,BREF                                                          
         X     R0,=XL4'00003FFF'                                                
         SLL   R0,10                                                            
         STCM  R0,7,BREFSUB                                                     
         STCM  R0,7,PATKREF        COMBINED REF NUMBER/GENERATED SUBLIN         
         OI    DMINBTS,X'08'       READ DELETED RECS                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         NI    DMOUTBTS,X'FD'      TURN OFF DELETED REC BIT                     
         NI    DMINBTS,X'F7'       RESET READ DELETED RECS                      
         CLC   KEY(10),KEYSAVE                                                  
         BNE   NOTFNDER                                                         
         SR    R0,R0                                                            
         ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         CH    R0,BREF                                                          
         BNE   NOTFNDER                                                         
         SRL   R1,22                                                            
         X     R1,=XL4'000003FF'                                                
         STH   R1,BSUB                                                          
VREFX    XIT1                                                                   
VKPACK   PACK  DUB,WORK(0)                                                      
BIGERR   MVI   ERROR,INVREFSZ      REF NUMBER TOO LARGE                         
         B     VREFERX                                                          
NOTFNDER MVI   ERROR,NOTFOUND                                                   
         B     VREFERX                                                          
NUMERRA  MVI   ERROR,NOTNUM                                                     
VREFERX  GOTO1 ERREX                                                            
         DROP  R4,RB,RC                                                         
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         LA    R1,KEY                                                           
         USING CLTHDR,R1                                                        
         XC    KEY,KEY                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT DIR                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT FILE                        
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   CLTNM,CNAME                                                      
         LA    R2,CLIST                                                         
         DROP  R1                                                               
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         LA    R3,880                                                           
         L     RE,ASVCLIST                                                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* VALIDATE FILTERS                                                              
         EJECT                                                                  
VFLT     NTR1  BASE=*,LABEL=*                                                   
         XC    FILTERS,FILTERS                                                  
         XC    CMLFTRP,CMLFTRP                                                  
         OI    FLAGFTR,X'20'                                                    
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFLT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFLT06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFLT02                                                           
         LA    R1,4                                                             
         B     VFLT04                                                           
VFLT02   LLC   R1,5(R2)                                                         
VFLT04   BCTR  R1,0                                                             
         EX    R1,VFLTCLCH                                                      
         BNE   VFLT08                                                           
VFLT06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FLTRHELP),FLTRHELP                                     
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
VFLT08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(7,BLOCK+64)                          
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    VMISSERR            NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VFLT10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFLT12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFLT14              NO, NETHER                                   
VFLT12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
VFLT14   EX    R1,VFLTCLCA         DATE                                         
         BNE   VFLT20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         ICM   R5,15,DMCB          WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,DATEFTR1)                                
         CLM   R5,1,1(R4)          WAS SECOND DATE ENTERED                      
         BL    VFLT16              YES                                          
         MVC   DATESFTR,HOLDSIGN                                                
         B     VFLT90                                                           
VFLT16   LA    R5,23(R4,R5)                                                     
         GOTO1 DATVAL,(R1),(0,(R5)),DATE                                        
         OC    DMCB,DMCB           WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,DATEFTR2)                                
         CLC   DATEFTR1,DATEFTR2   1ST MUST BE LESS OR EQ TO 2ND                
         BH    DATERR                                                           
         B     VFLT90                                                           
         EJECT                                                                  
VFLT20   EX    R1,VFLTCLCB         COPY CODE                                    
         BNE   VFLT30                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VFLT24                                                           
         MVC   CODEFTR,22(R4)      COPY CODE                                    
         B     VFLT90                                                           
*                                                                               
VFLT24   TM    3(R4),X'80'         VALID NUMERIC                                
         BZ    ESTCDER                                                          
         CLC   =F'255',8(R4)       MUST NOT BE MORE THAN 255                    
         BL    ESTCDER                                                          
         CLI   11(R4),0                                                         
         BE    ESTCDER                                                          
         MVC   CODEFTR,11(R4)                                                   
         MVI   CODEFTRS,C'Y'                                                    
         B     VFLT90                                                           
*                                                                               
VFLT30   EX    R1,VFLTCLCD         MKT (MARKET)                                 
         BE    VFLT34                                                           
         EX    R1,VFLTCLCI         MAR (MARKET)                                 
         BNE   VFLT40                                                           
*                                                                               
VFLT34   CLC   =CL3'ALL',22(R4)    FILTER ON ALL MARKETS                        
         BNE   VFLT36              NO                                           
         MVI   BMKTFTR,X'FF'       SPEC INDICATOR - ALL MKTS                    
         MVC   QMKTFTR,=CL4'ALL'                                                
         B     VFLT90                                                           
*                                                                               
VFLT36   TM    3(R4),X'80'         WAS MARKET NUMERIC                           
         BZ    NUMERR                                                           
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      MARKET LEN                                   
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         MVC   HOLDMS(5),BMKTSTA                                                
         MVC   HOLDMS+5(24),MKTNM                                               
         MVC   HOLDMS+29(16),STAPRNT                                            
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIMKT                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   VFLTERX             GO PRINT ERROR                               
         MVC   QMKTFTR,QMKT                                                     
         MVC   BMKTFTR+3(2),BMKT                                                
         MVC   BMKTSTA,HOLDMS                                                   
         MVC   MKTNM,HOLDMS+5                                                   
         MVC   STAPRNT(16),HOLDMS+29                                            
         B     VFLT90                                                           
*                                                                               
VFLT40   EX    R1,VFLTCLCC         AFFILIATE                                    
         BNE   VFLT50                                                           
*                                                                               
         CLI   1(R4),3             MUST HAVE ENTRY 3 CHAR LON                   
         BNE   AFFERR                                                           
         MVC   AFFFTR,22(R4)                                                    
         B     VFLT90                                                           
*                                                                               
VFLT50   EX    R1,VFLTCLCE         STATION                                      
         BNE   VFLT54                                                           
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      STATION                                      
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         MVC   HOLDMS(5),BMKTSTA                                                
         MVC   HOLDMS+5(24),MKTNM                                               
         MVC   HOLDMS+29(16),STAPRNT                                            
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALISTA                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   VFLTERX             GO PRINT ERROR                               
         MVC   STAFTR,QSTA                                                      
         MVC   BMKTSTA,HOLDMS                                                   
         MVC   MKTNM,HOLDMS+5                                                   
         MVC   STAPRNT(16),HOLDMS+29                                            
         B     VFLT90                                                           
*                                                                               
VFLT54   EX    R1,VFLTCLCF         CML ID                                       
         BNE   VFLT60                                                           
* VALIDATE CML                                                                  
         MVI   ADIDFLAG,C'N'                                                    
         MVC   CMLFTR,22(R4)       MOVE 8 CHAR INPUT CMML                       
         CLI   1(R4),8             CML ID MUST BE >= 8 CHAR                     
         BL    CMLLENER                                                         
* ALWAYS TRY TO PACK CMML                                                       
         MVI   ADIDFLAG,C'Y'                                                    
         GOTO1 VTRPACK,DMCB,(C'P',22(R4)),CMLFTRP                               
         BE    VFLT56                                                           
         MVI   ADIDFLAG,C'N'                                                    
         CLI   5(R2),8             8 CHAR CAN BE ISCI                           
         BE    VFLT56                                                           
         MVI   ERROR,INVALID                                                    
         B     VFLTERX                                                          
*                                                                               
VFLT56   XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
*                                                                               
         MVC   CMLKID,=XL2'0AC1'                                                
         MVC   CMLKCML,CMLFTRP                                                  
         CLI   ADIDFLAG,C'Y'                                                    
         BE    VFLT58                                                           
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKCML,CMLFTR                                                   
         DROP  R1                                                               
*                                                                               
VFLT58   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
         B     VFLT90                                                           
*                                                                               
VFLT60   EX    R1,VFLTCLCJ         PERIOD                                       
         BNE   VFLT70                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         L     RE,DMCB             GET LENGTH OF FIELD                          
         LTR   RE,RE                                                            
         BZ    DATERR                                                           
         LA    R5,1(RE,R5)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(3,PERFTR)                                  
         GOTO1 DATVAL,(R1),(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,PERFTR+3)                                
         CLC   PERFTR(3),PERFTR+3                                               
         BH    DATERR                                                           
         B     VFLT90                                                           
*                                                                               
VFLT70   EX    R1,VFLTCLCK         DELETED (SOFT-DELETE)                        
         BNE   VFLT80                                                           
         OI    FLAGFTR,X'80'                                                    
         B     VFLT90                                                           
VFLT80   EX    R1,VFLTCLCL         SORT                                         
         BNE   VFLT85                                                           
         OI    FLAGFTR,X'40'                                                    
         CLC   =C'DATE',22(R4)                                                  
         BE    VFLT90                                                           
         CLC   =C'REF',22(R4)                                                   
         BNE   SORTERR                                                          
         NI    FLAGFTR,X'FF'-X'20'                                              
         B     VFLT90                                                           
*                                                                               
VFLT85   EX    R1,VFLTCLCM         INVERTED FILTER                              
         BNE   VFLT100                                                          
         MVI   INVFLT,C'Y'                                                      
         CLI   22(R4),C'Y'                                                      
         BE    VFLT90                                                           
         MVI   INVFLT,C'N'                                                      
         CLI   22(R4),C'N'                                                      
         BE    VFLT90                                                           
         MVI   INVFLT,0                                                         
         CLI   22(R4),C'B'         FOR BOTH                                     
         BE    VFLT90                                                           
         B     INVTERR                                                          
*                                                                               
VFLT90   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFLT10           FOR NUMBER OF BLOCKS FOUND                   
VFLT96   DS    0H                                                               
         XIT1                                                                   
*                                                                               
VFLT100  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FLTRMSG+L'FLTRHELP),FLTRMSG                            
         B     ERREXIT                                                          
ESTCDER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTCDMS),ESTCDMS                                       
         B     ERREXIT                                                          
SORTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SORTERMS),SORTERMS                                     
         B     ERREXIT                                                          
INVTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVERMS),INVERMS                                       
         B     ERREXIT                                                          
AFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'AFFERRMS),AFFERRMS                                     
         B     ERREXIT                                                          
*                                                                               
VFLTCLCA CLC   12(0,R4),=CL4'DATE'                                              
VFLTCLCB CLC   12(0,R4),=CL3'CDE'                                               
VFLTCLCC CLC   12(0,R4),=CL5'AFFIL'                                             
VFLTCLCD CLC   12(0,R4),=CL3'MKT'                                               
VFLTCLCE CLC   12(0,R4),=CL3'STA'                                               
VFLTCLCF CLC   12(0,R4),=CL3'CML'                                               
VFLTCLCH CLC   8(0,R2),=CL4'HELP'                                               
VFLTCLCI CLC   12(0,R4),=CL3'MAR'                                               
VFLTCLCJ CLC   12(0,R4),=CL3'PER'                                               
VFLTCLCK CLC   12(0,R4),VDELETE                                                 
VFLTCLCL CLC   12(0,R4),=CL4'SORT'                                              
VFLTCLCM CLC   12(0,R4),=CL3'INV'                                               
         EJECT                                                                  
CMLLENER MVC   GERROR,=Y(NOT812)   CML ID MUST BE 8-12 CHAR                     
         J     ERREXT1                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     VFLTERX                                                          
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH CMML FOR CLT                         
         B     VFLTERX                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     VFLTERX                                                          
VMISSERR MVI   ERROR,MISSING                                                    
*                                                                               
VFLTERX  GOTO1 ERREX                                                            
         LTORG                                                                  
FLTRMSG  DC    C'* ERROR *'                                                     
FLTRHELP DC    C'FILTERS-DEL,DATE/SORT/CDE/MKT/STA/CML/PER/INV/AFF='            
SORTERMS DC    C'* ERROR * SORT = DATE OR REF'                                  
INVERMS  DC    C'* ERROR * INV = Y,N,B (BOTH)'                                  
ESTCDMS  DC    C'* ERROR * COPY CD=EST MUST BE NUMERIC 1 TO 255 *'              
AFFERRMS DC    C'* ERROR * ENTER 3 CHARACTER AFFILIATE *'                       
VDELETE  DC    CL7'DELETED'                                                     
         EJECT                                                                  
*===========================================================                    
* VALIDATE COMMERCIAL ROTATION                                                  
* FILTER CMMLS FOR LIST FUNCTION                                                
*===========================================================                    
                                                                                
FLTR     NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING PATKEY,R6                                                        
*                                                                               
         OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    FLTR50              NO, SO EVERYTHING PASSES                     
         OC    CODEFTR,CODEFTR     CODE FILTER                                  
         BZ    FLTR02              NO                                           
         CLC   CODEFTR,PATKCODE    THIS IT                                      
         BNE   FLTRNO                                                           
*                                                                               
FLTR02   MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         TM    FLAGFTR,X'80'       ANY DELETE FILTER                            
         BZ    FLTR04              NO                                           
         CLI   PATDTALN,38         IS THIS OLD PAT LEN (NO PATSTAT)             
         BE    FLTRNO              YES, CAN'T BE DELETED                        
         TM    PATSTAT,X'80'       SOFT DELETE                                  
         BZ    FLTRNO                                                           
         B     FLTR10                                                           
*                                                                               
FLTR04   CLI   PATDTALN,38         IS THIS OLD PAT LEN (NO PATSTAT)             
         BE    FLTR10              YES, NO SOFT DELETE CK                       
         TM    PATSTAT,X'80'                                                    
         BO    FLTRNO                                                           
*                                                                               
FLTR10   CLI   DATEFTR1,0          DATE FILTER                                  
         BE    FLTR16                                                           
         CLI   DATEFTR2,0          SECOND DATE FILTER                           
         BE    FLTR11                                                           
         CLC   DATEFTR1,PATEND                                                  
         BH    FLTRNO                                                           
         CLC   DATEFTR2,PATSTART                                                
         BL    FLTRNO                                                           
         B     FLTR16                                                           
*                                                                               
FLTR11   CLI   DATESFTR,0                                                       
         BE    FLTR14                                                           
         CLI   DATESFTR,X'4C'      GREATER THAN                                 
         BNE   FLTR12              MUST BE LESS THAN                            
         CLC   DATEFTR1,PATEND     FILTER TO RECALL                             
         BNH   FLTRNO              BYPASS                                       
         B     FLTR16              CK NEXT FILTER                               
*                                                                               
FLTR12   CLC   DATEFTR1,PATEND                                                  
         BNL   FLTRNO                                                           
         B     FLTR16                                                           
*                                                                               
FLTR14   CLC   DATEFTR1,PATSTART                                                
         BL    FLTRNO                                                           
         CLC   DATEFTR1,PATEND                                                  
         BH    FLTRNO                                                           
*                                                                               
FLTR16   OC    PERFTR,PERFTR       PERIOD FILTER                                
         BZ    FLTR18                                                           
         CLC   PERFTR(3),PATSTART  PERIOD START TO PAT START                    
         BNE   FLTRNO                                                           
         CLC   PERFTR+3(3),PATEND  PERIOD END TO PAT END                        
         BNE   FLTRNO                                                           
*                                                                               
FLTR18   CLI   INVFLT,0            IS THERE AN INVERTED PRD FILTER              
         BE    FLTR20                                                           
         CLI   INVFLT,C'Y'         INVERTED ONLY?                               
         BNE   FLTR18C                                                          
         TM    PATSTAT,X'04'                                                    
         BZ    FLTRNO                                                           
         B     FLTR20                                                           
*                                                                               
FLTR18C  CLI   INVFLT,C'N'         NOT INVERTED ONLY?                           
         BNE   FLTR20                                                           
         TM    PATSTAT,X'04'                                                    
         BO    FLTRNO                                                           
         B     FLTR20                                                           
*                                                                               
FLTR20   OC    BMKTFTR,BMKTFTR     IS THERE A MARKET FILTER                     
         BNZ   FLTR22               YES                                         
         OC    STAFTR,STAFTR       IS THERE A STATION FILTER                    
         BNZ   FLTR22               YES                                         
         OC    AFFFTR,AFFFTR       IS THERE AN AFFILIATE FILTER                 
         BZ    FLTR40               NO                                          
*                                                                               
FLTR22   MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATLSTEL,R6                                                      
         LLC   R0,PATLSTLN                                                      
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,PATLST           START OF MKT/STA LIST                        
         STM   R0,R1,DUB                                                        
*                                                                               
* CHECK OUT MARKET LIST *                                                       
*                                                                               
         CLI   PATLSTTY,C'M'                                                    
         BNE   FLTR26                                                           
         OC    STAFTR,STAFTR       IS THERE A STATION FILTER                    
         BNZ   FLTRNO               YES                                         
         OC    AFFFTR,AFFFTR       IS THERE AN AFFILIATE FILTER                 
         BNZ   FLTRNO               YES                                         
         CLI   BMKTFTR,X'FF'       FILTER ON MKT ALL                            
         BNE   FLTR24               NO                                          
         OC    PATLST,PATLST       IF 1ST ENTRY ZERO, IS MKT ALL                
         BZ    FLTR40               YES                                         
         B     FLTRNO                                                           
*                                                                               
FLTR24   OC    0(5,R1),0(R1)       IF 1ST ENTRY ZERO, IS MKT ALL                
         BZ    FLTRNO               YES                                         
         CLC   0(5,R1),BMKTFTR                                                  
         BE    FLTR40                                                           
         LA    R1,5(,R1)                                                        
         S     R0,=F'5'                                                         
         BP    FLTR24                                                           
         B     FLTRNO                                                           
*                                                                               
* CHECK OUT STATION LIST *                                                      
*                                                                               
FLTR26   CLI   PATLSTTY,C'S'       STATION LIST                                 
         BNE   FLTR30                                                           
         OC    BMKTFTR,BMKTFTR                                                  
         BNZ   FLTRNO                                                           
         OC    AFFFTR,AFFFTR                                                    
         BNZ   FLTRNO                                                           
*                                                                               
FLTR28   CLC   0(5,R1),STAFTR                                                   
         BE    FLTR40                                                           
         LA    R1,5(,R1)                                                        
         S     R0,=F'5'                                                         
         BP    FLTR28                                                           
         B     FLTRNO                                                           
*                                                                               
* CHECK OUT STATION LIST *                                                      
*                                                                               
FLTR30   CLI   PATLSTTY,C'A'       AFFILIATE LIST                               
         BNE   FLTR34                                                           
         OC    STAFTR,STAFTR       IS THERE A STATION FILTER                    
         BNZ   FLTRNO               YES                                         
         OC    BMKTFTR,BMKTFTR                                                  
         BNZ   FLTRNO                                                           
*                                                                               
FLTR32   CLC   0(5,R1),AFFFTR                                                   
         BE    FLTR40                                                           
         LA    R1,5(,R1)                                                        
         S     R0,=F'5'                                                         
         BP    FLTR32                                                           
         B     FLTRNO                                                           
*                                                                               
FLTR34   CLI   PATLSTTY,C'C'       COMBINED MKT/AFF                             
         BNE   FLTRNO                                                           
         OC    STAFTR,STAFTR       IS THERE A STATION FILTER                    
         BNZ   FLTRNO               YES                                         
*                                                                               
         MVI   BYTE,0                                                           
FLTR36   OC    BMKTFTR,BMKTFTR                                                  
         BZ    FLTR36C                                                          
         CLC   0(5,R1),BMKTFTR                                                  
         BNE   FLTR36C                                                          
         OI    BYTE,X'80'                                                       
*                                                                               
FLTR36C  OC    AFFFTR,AFFFTR                                                    
         BZ    FLTR36E                                                          
         CLC   0(5,R1),AFFFTR                                                   
         BNE   FLTR36E                                                          
         OI    BYTE,X'40'                                                       
*                                                                               
FLTR36E  LA    R1,5(,R1)                                                        
         S     R0,=F'5'                                                         
         BP    FLTR36                                                           
         OC    BMKTFTR,BMKTFTR                                                  
         BZ    FLTR36G                                                          
         TM    BYTE,X'80'                                                       
         BZ    FLTRNO                                                           
*                                                                               
FLTR36G  OC    AFFFTR,AFFFTR                                                    
         BZ    FLTR40                                                           
         TM    BYTE,X'40'                                                       
         BZ    FLTRNO                                                           
*                                                                               
FLTR40   OC    CMLFTR,CMLFTR       IS THERE A CML ID FILTER                     
         BZ    FLTR50              NO                                           
         TM    PRDMATSW,BPATSW     BPAT RECS ???                                
         BZ    FLTR41                                                           
         MVI   ELCODE,X'31'                                                     
*                                                                               
FLTR40C  BRAS  RE,NEXTEL                                                        
         BNE   FLTRNO                                                           
         USING PATBCMEL,R6                                                      
         LLC   R0,PATBCMLN                                                      
         SRL   R0,4                DIVIDE BY 16 = NUMBER OF CML'S               
         LA    R1,PATBCML          START OF CML LIST                            
         B     FLTR42                                                           
*                                                                               
FLTR41   MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FLTRNO                                                           
         USING PATCMLEL,R6                                                      
         LLC   R0,PATCMLLN                                                      
         SRL   R0,4                DIVIDE BY 16 = NUMBER OF CML'S               
         LA    R1,PATCML           START OF CML LIST                            
*                                                                               
FLTR42   CLC   0(8,R1),CMLFTR                                                   
         BE    FLTR50                                                           
         CLC   0(8,R1),CMLFTRP                                                  
         BE    FLTR50                                                           
         CLC   8(8,R1),CMLFTR                                                   
         BE    FLTR50                                                           
         CLC   8(8,R1),CMLFTRP                                                  
         BE    FLTR50                                                           
         LA    R1,16(R1)                                                        
         BCT   R0,FLTR42                                                        
         CLI   ELCODE,X'31'                                                     
         BE    FLTR40C                                                          
         B     FLTRNO                                                           
*                                                                               
FLTR50   CR    R1,R1               SET COND CODE FILTERED OK                    
         J     EXIT                                                             
*                                                                               
FLTRNO   CR    RB,RD               SET COND CODE NO FILTER                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* DOWNLOAD PATTERN DATA                                                         
*=================================================================              
                                                                                
D        USING DLCBD,DLCB                                                       
                                                                                
PRDOWN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'NOW',CONWHEN                                                  
         JNE   PRD05                                                            
         MVC   P(20),=C'PATTERN DOWNLOADABLE'                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,99                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRD05    XC    D.DLCBD(DLCBL),D.DLCBD                                           
         MVI   D.DLCBACT,DLCBINIT         DOWNLOAD ACTION IS START              
         LA    RF,DWNHOOK                 DUMMY HOOK                            
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
         XC    KEY,KEY                                                          
         MVC   KEY(L'COMPKEY),COMPKEY                                           
         NI    DMINBTS,X'FF'-X'08'                                              
         GOTO1 HIGH                                                             
         J     PRD10                                                            
*                                                                               
PRDSEQ   GOTO1 SEQ                                                              
*                                                                               
PRD10    CLC   KEY(3),KEYSAVE                                                   
         JNE   PRDOWNX                                                          
*                                                                               
         CLC   KEY(10),PREVKEY     SAME UP TO COPY CODE?                        
         JNE   PRD15                                                            
         MVC   CURREF,KEY+10                                                    
*                                                                               
         NC    CURREF,=X'FFFC'                                                  
         NC    PREVREF,=X'FFFC'                                                 
         CLC   CURREF,PREVREF                                                   
         JE    PRDSEQ                                                           
*                                                                               
PRD15    LA    R6,KEY                                                           
         USING PATRECD,R6                                                       
*                                                                               
         CLC   PATKCLT,BCLT                                                     
         JNE   PRDSEQ                                                           
*                                                                               
         OC    BPRD,BPRD                                                        
         BZ    *+14                                                             
         CLC   PATKPRD,BPRD                                                     
         JNE   PRDSEQ                                                           
*                                                                               
         OC    BSLN,BSLN                                                        
         BZ    *+14                                                             
         CLC   PATKSLN,BSLN                                                     
         JNE   PRDSEQ                                                           
*                                                                               
         OC    BPRD2,BPRD2                                                      
         BZ    *+14                                                             
         CLC   PATKPRD2,BPRD2                                                   
         JNE   PRDSEQ                                                           
*                                                                               
         OC    BSLN2,BSLN2                                                      
         BZ    *+14                                                             
         CLC   PATKSLN2,BSLN2                                                   
         JNE   PRDSEQ                                                           
*                                                                               
         MVC   PREVKEY,KEY                                                      
         MVC   PREVREF,PATKREF                                                  
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,FLTR                                                          
         JNE   PRDSEQ                                                           
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
         LHI   R1,PDDATAX-PDDATA                                                
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24               CLEAR TO SPACES                              
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO3             POINT TO BUFFER                              
         USING PDDATA,R4                                                        
*                                                                               
         MVC   PDITM,=CL4'ITM1'                                                 
         MVC   PDACTN,SPACES                                                    
         MVC   PDMED,QMED                     MEDIA                             
         GOTO1 CLUNPK,DMCB,PATKCLT,PDCLI      CLIENT                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,PATKREF                                                     
         SRL   R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PDREF,DUB                                                        
*                                                                               
         LA    R3,PATKPRD                                                       
         BRAS  RE,GETPRD                                                        
         MVC   PDPRD(7),FLD                                                     
*                                                                               
         CLI   PATKPRD2,0                                                       
         JE    PRD30                                                            
*                                                                               
         LA    R3,PATKPRD2                                                      
         BRAS  RE,GETPRD                                                        
         MVC   PDPTR(7),FLD                                                     
*                                                                               
PRD30    LLC   R0,PATKCODE                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PDCODE,DUB          ASSUME COPYCODE=EST                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         MVC   PDDESC,SPACES                                                    
         MVC   PDDESC(L'PATDESC),PATDESC      DESCRIPTION                       
*                                                                               
         MVI   PDINVPRD,C'N'                  INVERTED PRODUCT                  
         TM    PATSTAT,X'04'                                                    
         JZ    *+8                                                              
         MVI   PDINVPRD,C'Y'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,PATSTART),(5,PDPERIOD)                            
         MVI   PDPERIOD+8,C'-'                                                  
         MVC   PDPERIOD+9(10),=C'UFN       '  PERIOD                            
         CLC   PATEND,=XL3'FFFFFF'                                              
         JE    PRD40                                                            
         GOTO1 DATCON,DMCB,(3,PATEND),(5,PDPERIOD+9)                            
*                                                                               
         XC    FULL,FULL                                                        
         OC    PATSTIM,PATSTIM                                                  
         BZ    PRD32                                                            
         MVC   FULL(2),PATSTIM                                                  
         GOTO1 UNTIME,DMCB,FULL,PDSTIM                                          
         MVC   FULL(2),PATETIM                                                  
         GOTO1 (RF),(R1),,PDETIM                                                
*                                                                               
PRD32    MVC   PDDPT,PATDPT                                                     
*                                                                               
PRD40    LA    R1,PDT              START OF MKT/STA BUFFER                      
         ST    R1,PDATAST          SET BUFFER START ADDRESS                     
         BRAS  RE,PMS              BUILD BUFFER OF MKTS/STATIONS...             
*                                                                               
         MVI   ELCODE,X'32'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   PRD50                                                            
*                                                                               
         USING PATPTNEL,R6                                                      
PRD42    LLC   R1,PATPTNLN                                                      
         AHI   R1,-3               GET ROT LEN -1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDROT(0),2(R6)                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         JNE   PRD50                                                            
*                                                                               
         USING PATTXTEL,R6                                                      
         MVC   PDTEXT(6),PATTXTKY+1           TEXT                              
*                                                                               
PRD50    L     R6,AIO                                                           
         USING PATRECD,R6                                                       
*                                                                               
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRD60                                                            
         USING PATCMLEL,R6                                                      
*                                                                               
         LLC   R0,PATCMLLN                                                      
         SRL   R0,4                                                             
         LA    R3,PDCMLA                                                        
         LA    R6,PATCML                                                        
*                                                                               
PRD52    ST    R3,FULL                                                          
         MVC   0(25,R3),SPACES                                                  
         MVC   0(8,R3),0(R6)       MOVE ISCI CMML                               
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRD52A                                                           
         GOTO1 VTRPACK,DMCB,(C'U',0(R6)),0(R3)                                  
*                                                                               
PRD52A   CLI   8(R6),C' '          TEST FOR SECOND CMML                         
         BNH   PRD52X              NO                                           
*                                                                               
         LA    R3,13(R3)           BACK UP TO LAST CHAR                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         MVI   1(R3),C'-'                                                       
         MVC   2(8,R3),8(R6)                                                    
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRD52X                                                           
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),2(R3)                                  
*                                                                               
PRD52X   L     R3,FULL             GET START OF FIELD ADDRESS                   
         LA    R3,PDCMLB-PDCMLA(R3)                                             
         LA    R6,16(R6)                                                        
         BCT   R0,PRD52                                                         
*                                                                               
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JNE   PRD60                                                            
*                                                                               
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
         LA    R6,2(R6)            FIRST ENTRY                                  
*                                                                               
PRD54    LLC   RE,0(R6)            GET CML LETTER                               
         AHI   RE,-193             A=X'C1'=193 IS +0                            
         CHI   RE,9                                                             
         BNH   *+8                                                              
         AHI   RE,-7               J=X'D1'=209 IS +9                            
         MHI   RE,PDPCTB-PDPCTA    GIVES DISTANCE BETWEEN PCTS                  
         LA    RE,PDPCTA(RE)                                                    
         SR    R1,R1                                                            
         ICM   R1,3,1(R6)          GET PCT                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RE),DUB                                                      
         LA    R6,3(R6)            NEXT PCT ENTRY                               
         BCT   R0,PRD54                                                         
                                                                                
* COMMENTS *                                                                    
                                                                                
PRD60    LA    R0,4                MAX COMMENTS LINES                           
         LA    R2,PDCMT1                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        COMMENT ELEMENT(S)                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRD62    BRAS  RE,NEXTEL                                                        
         BNE   PRD70                                                            
         USING PATCMTEL,R6                                                      
         LLC   R1,PATCMTLN         GET COMMENT ELEM LEN                         
         AHI   R1,-4               GET DATA LEN -1                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),3(R6)                                                    
         LA    R2,PDCMT2-PDCMT1(R2)                                             
         BCT   R0,PRD62                                                         
         EJECT                                                                  
*===============================================================                
* ALL FIELDS NOW IN OUTPUT BUFFERS                                              
* CALL DLFLD FOR EACH DATA FIELD                                                
*===============================================================                
                                                                                
PRD70    LA    R6,DOWNTAB                                                       
*                                                                               
PRD72    MVI   D.DLCBACT,DLCBPUT     ACTION IS PUT                              
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
         BNE   PRD72                                                            
*                                                                               
         MVI   D.DLCBACT,DLCBEOL          END OF LINE                           
         GOTO1 VDLFLD,DLCB                                                      
         J     PRDSEQ                                                           
*                                                                               
PRDOWNX  DS    0H                                                               
         MVI   D.DLCBACT,DLCBEOR          END OF REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
         J     EXIT                                                             
*                                                                               
DWNHOOK  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*DOWNTAB'                                                    
*                                                                               
* 2 BYTE DSPL FROM PDDATA (IF BYTE3=0)                                          
DOWNTAB  DS    0D                                                               
         DC    AL2(PDITM-PDDATA),AL1(L'PDITM)                                   
         DC    AL2(PDACTN-PDDATA),AL1(L'PDACTN)                                 
         DC    AL2(PDMED-PDDATA),AL1(L'PDMED)                                   
         DC    AL2(PDCLI-PDDATA),AL1(L'PDCLI)                                   
         DC    AL2(PDPRD-PDDATA),AL1(L'PDPRD)                                   
         DC    AL2(PDPTR-PDDATA),AL1(L'PDPTR)                                   
         DC    AL2(PDCODE-PDDATA),AL1(L'PDCODE)                                 
         DC    AL2(PDREF-PDDATA),AL1(L'PDREF)                                   
         DC    AL2(PDFILT-PDDATA),AL1(L'PDFILT)                                 
         DC    AL2(PDDESC-PDDATA),AL1(L'PDDESC)                                 
         DC    AL2(PDPERIOD-PDDATA),AL1(L'PDPERIOD)                             
         DC    AL2(PDSTIM-PDDATA),AL1(L'PDSTIM)                                 
         DC    AL2(PDETIM-PDDATA),AL1(L'PDETIM)                                 
         DC    AL2(PDDPT-PDDATA),AL1(L'PDDPT)                                   
         DC    AL2(PDTEXT-PDDATA),AL1(L'PDTEXT)                                 
         DC    AL2(PDINVPRD-PDDATA),AL1(L'PDINVPRD)                             
         DC    AL2(PDT-PDDATA),AL1(L'PDT)                                       
         DC    AL2(PDT2-PDDATA),AL1(L'PDT2)                                     
         DC    AL2(PDT3-PDDATA),AL1(L'PDT3)                                     
         DC    AL2(PDT4-PDDATA),AL1(L'PDT4)                                     
         DC    AL2(PDCMLA-PDDATA),AL1(L'PDCMLA)                                 
         DC    AL2(PDPCTA-PDDATA),AL1(L'PDPCTA)                                 
         DC    AL2(PDCMLB-PDDATA),AL1(L'PDCMLB)                                 
         DC    AL2(PDPCTB-PDDATA),AL1(L'PDPCTB)                                 
         DC    AL2(PDCMLC-PDDATA),AL1(L'PDCMLC)                                 
         DC    AL2(PDPCTC-PDDATA),AL1(L'PDPCTC)                                 
         DC    AL2(PDCMLD-PDDATA),AL1(L'PDCMLD)                                 
         DC    AL2(PDPCTD-PDDATA),AL1(L'PDPCTD)                                 
         DC    AL2(PDCMLE-PDDATA),AL1(L'PDCMLE)                                 
         DC    AL2(PDPCTE-PDDATA),AL1(L'PDPCTE)                                 
         DC    AL2(PDCMLF-PDDATA),AL1(L'PDCMLF)                                 
         DC    AL2(PDPCTF-PDDATA),AL1(L'PDPCTF)                                 
         DC    AL2(PDCMLG-PDDATA),AL1(L'PDCMLG)                                 
         DC    AL2(PDPCTG-PDDATA),AL1(L'PDPCTG)                                 
         DC    AL2(PDCMLH-PDDATA),AL1(L'PDCMLH)                                 
         DC    AL2(PDPCTH-PDDATA),AL1(L'PDPCTH)                                 
         DC    AL2(PDCMLI-PDDATA),AL1(L'PDCMLI)                                 
         DC    AL2(PDPCTI-PDDATA),AL1(L'PDPCTI)                                 
         DC    AL2(PDCMLJ-PDDATA),AL1(L'PDCMLJ)                                 
         DC    AL2(PDPCTJ-PDDATA),AL1(L'PDPCTJ)                                 
         DC    AL2(PDCMLK-PDDATA),AL1(L'PDCMLK)                                 
         DC    AL2(PDPCTK-PDDATA),AL1(L'PDPCTK)                                 
         DC    AL2(PDCMLL-PDDATA),AL1(L'PDCMLL)                                 
         DC    AL2(PDPCTL-PDDATA),AL1(L'PDPCTL)                                 
         DC    AL2(PDCMLM-PDDATA),AL1(L'PDCMLM)                                 
         DC    AL2(PDPCTM-PDDATA),AL1(L'PDPCTM)                                 
         DC    AL2(PDCMLN-PDDATA),AL1(L'PDCMLN)                                 
         DC    AL2(PDPCTN-PDDATA),AL1(L'PDPCTN)                                 
         DC    AL2(PDCMLO-PDDATA),AL1(L'PDCMLO)                                 
         DC    AL2(PDPCTO-PDDATA),AL1(L'PDPCTO)                                 
         DC    AL2(PDROT-PDDATA),AL1(L'PDROT)                                   
         DC    AL2(PDCMT1-PDDATA),AL1(L'PDCMT1)                                 
         DC    AL2(PDCMT2-PDDATA),AL1(L'PDCMT2)                                 
         DC    AL2(PDCMT3-PDDATA),AL1(L'PDCMT3)                                 
         DC    AL2(PDCMT4-PDDATA),AL1(L'PDCMT4)                                 
         DC    X'FFFF'                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPTRRTAB                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF3D                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* THIS STORAGE DSECT AND THE ONE IN SPTRA03 MUST BE THE SAME                    
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR23RR DS    F                                                                
VDLFLD   DS    A                                                                
PDATAST  DS    A                                                                
VTRPACK  DS    A                                                                
         ORG                                                                    
PREVKEY  DS    XL13                                                             
CURREF   DS    XL2                                                              
PREVREF  DS    XL2                                                              
CMLFTRP  DS    XL8                                                              
       ++INCLUDE SPTRA03WRK                                                     
         ORG                                                                    
         DS    0D                                                               
DLCB     DS    XL256                                                            
         EJECT                                                                  
* OFFLINE SORT RECORD FOR DATE SORT *                                           
*                                                                               
SORTREC  DSECT                                                                  
SRTREC   DS    0CL18                                                            
SRTSLN   DS    XL1                                                              
SRTPROD2 DS    CL3                                                              
SRTSLN2  DS    XL1                                                              
SRTDTS   DS    XL6                                                              
SRTORD   DS    XL1                                                              
SRTREFS  DS    XL2                                                              
SRTDKAD  DS    XL4                                                              
*                                                                               
* PRINT COMMERCIAL AND PERCENT ROTATION FOR BPAT                                
*                                                                               
* OFFLINE PRINT                                                                 
*                                                                               
PRTLINED DSECT                                                                  
         DS    CL1                                                              
PPRD     DS    CL7                                                              
PPRD2    EQU   PPRD+110,7                                                       
         DS    CL2                                                              
PCODE    DS    CL4                                                              
         DS    CL1                                                              
PREF     DS    CL5                                                              
         DS    CL2                                                              
PPERIOD  DS    CL17                                                             
PDESC    EQU   PPERIOD+110,16                                                   
         DS    CL2                                                              
PDATA    DS    CL68                                                             
*                                                                               
         ORG   PRTLINED                                                         
         DS    CL1                                                              
BPPRLN   DS    CL7                                                              
BPPTLN   DS    CL7                                                              
         DS    CL3                                                              
BPCODE   DS    CL1                                                              
         DS    CL3                                                              
BPPERIOD DS    CL17                                                             
         DS    CL1                                                              
BPREF    DS    CL5                                                              
         DS    CL2                                                              
BPSTLST  DS    CL33                                                             
         DS    CL1                                                              
BPCMLST  DS    CL22                                                             
         ORG   BPCMLST                                                          
BPCMLA   DS    CL3                                                              
BPCML    DS    CL8                                                              
         DS    CL1                                                              
BPADID   DS    CL12                                                             
         DS    CL1                                                              
BPPCT    DS    CL4                                                              
         ORG   BPPERIOD+110                                                     
BPDESC   DS    CL16                LINE 2                                       
*                                                                               
* ONLINE LIST                                                                   
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LPRLN    DS    CL7                                                              
         DS    CL1                                                              
LPTLN    DS    CL7                                                              
         DS    CL1                                                              
LCODE    DS    CL4                                                              
         DS    CL1                                                              
LREF     DS    CL3                                                              
         DS    CL1                                                              
LPER     DS    CL17                                                             
         DS    CL1                                                              
LDESC    DS    CL16                                                             
         DS    CL1                                                              
LDELETE  DS    CL15                                                             
*                                                                               
PDDATA   DSECT                                                                  
PDITM    DS    CL4                                                              
PDACTN   DS    CL3                                                              
PDMED    DS    CL1                                                              
PDCLI    DS    CL3                                                              
PDPRD    DS    CL8                 PRD-LEN                                      
PDPTR    DS    CL8                 PARTNER-LEN                                  
PDCODE   DS    CL3                                                              
PDREF    DS    CL6                                                              
PDFILT   DS    CL40                                                             
PDDESC   DS    CL17                                                             
PDPERIOD DS    CL20                                                             
PDSTIM   DS    CL5                                                              
PDETIM   DS    CL5                                                              
PDDPT    DS    CL1                                                              
PDTEXT   DS    CL7                                                              
PDINVPRD DS    CL1                                                              
PDT      DS    CL1                                                              
PDT2     DS    CL72                                                             
PDT3     DS    CL72                                                             
PDT4     DS    CL72                                                             
PDROT    DS    CL69                                                             
*                                                                               
PDCMLA   DS    CL25                                                             
PDPCTA   DS    CL3                                                              
PDCMLB   DS    CL25                                                             
PDPCTB   DS    CL3                                                              
PDCMLC   DS    CL25                                                             
PDPCTC   DS    CL3                                                              
PDCMLD   DS    CL25                                                             
PDPCTD   DS    CL3                                                              
PDCMLE   DS    CL25                                                             
PDPCTE   DS    CL3                                                              
PDCMLF   DS    CL25                                                             
PDPCTF   DS    CL3                                                              
PDCMLG   DS    CL25                                                             
PDPCTG   DS    CL3                                                              
PDCMLH   DS    CL25                                                             
PDPCTH   DS    CL3                                                              
PDCMLI   DS    CL25                                                             
PDPCTI   DS    CL3                                                              
PDCMLJ   DS    CL25                                                             
PDPCTJ   DS    CL3                                                              
PDCMLK   DS    CL25                                                             
PDPCTK   DS    CL3                                                              
PDCMLL   DS    CL25                                                             
PDPCTL   DS    CL3                                                              
PDCMLM   DS    CL25                                                             
PDPCTM   DS    CL3                                                              
PDCMLN   DS    CL25                                                             
PDPCTN   DS    CL3                                                              
PDCMLO   DS    CL25                                                             
PDPCTO   DS    CL3                                                              
*                                                                               
PDCMT1   DS    CL72                                                             
PDCMT2   DS    CL72                                                             
PDCMT3   DS    CL72                                                             
PDCMT4   DS    CL72                                                             
*                                                                               
PDDATAX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094SPTRA23   10/08/14'                                      
         END                                                                    
