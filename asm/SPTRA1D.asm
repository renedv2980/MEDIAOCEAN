*          DATA SET SPTRA1D    AT LEVEL 075 AS OF 11/09/20                      
*PHASE T2161DB                                                                  
*                                                                               
***********************************************************************         
***    NEW PERIOD ON REV REC ******************************************         
***********************************************************************         
***  1ST BYTE = YEAR                                              *****         
***  2ND BYTE = HOB MONTH AND LOB WEEK NUMBER FOR THAT MONTH      *****         
***  EG. 1/27/20 CONVERT FROM X'F03B' TO X'7814' MONTH=1 WEEK=4   *****         
***                                                               *****         
***  FLAG ON STATUS BYTE: REC WAS CONVERTED AND IS MONTHLY/WEEKLY *****         
***********************************************************************         
***********************************************************************         
*  TITLE: T2161D - NETWORK TRAFFIC REVISION COMMENT MAINTENANCE       *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM MAINTAINS REVISION COMMENTS FOR NETWORK     *         
*            TRAFFIC INSTRUCTIONS.                                    *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAAD (T216AD) LIST                           *         
*          SEE SCREEN SPTRABD (T216BD) MAINT                          *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
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
*  LEV 24    OCT26/87 ALLOW PERIOD INPUT WITHOUT PROGRAM INPUT        *         
*  LEV 25    APR22/88 FIX BUG - REVNUM NOT UPDATED IN DR              *         
*                               AND CK END DATE OF PROG TO PERIOD     *         
*                               NOT TODAY.                            *         
*  LEV 26-27 SEP23/88 ADD CODE FOR WEEKLY PERIOD                      *         
*  LEV 28    JAN16/89 FIX BUG IN DATE CK                              *         
*  LEV 29    FEB08/89 ADD PRD,PGR                                     *         
*  LEV 30-33 FEB16/89 FIX MONTHLY FILTER, PRINT FILTERS IN HEADING    *         
*  LEV 34    FEB24/89 CHANGE PGR TO PGRP                              *         
*  LEV 35    APR11/89 CLEAR ALL SCREEN AND SET PROPER PROTECT BIT     *         
*  LEV 36    MAR15/91 ADD NEW AUTO SEED COMMENT & NET SEED COMMENTS   *         
*  LEV 37    MAR25/91 FIX MISSING CMT ERROR                           *         
*  LEV 38    OCT04/91 SHOW REVFLAG IN ONLINE LIST FOR DDS TERMINALS   *         
*  LEV 39    JUL13/92 CHANGE TN PROF 14 PROD = * (NOT P)              *         
*  LEV 40    SEP01/92 ADD TN2 PROFILE                                 *         
*  LEV 41 EJOR 01MAR93 DISPLAY '@' IN LIST IF FAXED                   *         
*                    - USE EQU'S FOR REVFLAG                          *         
*  LEV 42 BGRI 14JUN94 ADD ACTIVITY OPTION FOR REV LIST               *         
*  LEV 43 SMUR 15MAR95 SCREEN CHANGES - PROGRAM FIELD TO PROG AND     *         
*                      MOVE PERIOD FIELD 1 BYTE TO THE LEFT SO WE     *         
*                      CAN SHOW MM/DD/YY AND THE REVFLAG.             *         
*                    - SHOW NETWORK REVISION NUMBER                   *         
*  LEV 44 BGRI 11MAY95 ADD LOCK TEST                                  *         
*  LEV 45 SMUR 23OCT95 SHOW CAL/BROADCAST MONTH                       *         
*  LEV 46 SMUR 14AUG96 DO VOPT BEFORE VPER                            *         
*  LEV 47 SMUR 13JUL98 TRACK (T)SUPP/(N)ON-TSUPP                      *         
*  LEV 48 SMUR 04FEB99 FIX PRINTING & DISPLAYING PGROUP               *         
*  LEV 49 SMUR 23DEC99 TEMP CODE (IF PRD DONT MATCH THEN ITS A PGROUP)*         
*  LEV 50 SMUR 23DEC99 DDS ONLY - SHOW 'P' FOR PAT GEN                *         
*                               - LIST ALL NETWORKS                   *         
*                      DISPLAY NETWORK REVISION RECORDS (PAT GEN )    *         
*                      CHK IF CLT LOCKED BY PATTERN GEN               *         
*  LEV 51 SMUR 02MAY00 FIX R1 IN LRL40                                *         
*  LEV 52 BGRI 15NOV00 ADD 3 DIGIT PRODUCT GROUP CODES                *         
*  LEV 53 BGRI 12JAN01 SHOW FLAGS ON OFFLINE LIST                     *         
*  LEV 55 BGRI 11DEC02 ALLOW HISPANIC                                 *         
*  LEV 56 BGRI 08OCT02 ADD 253 PRODUCTS                               *         
*  LEV 57 SMUR 03DEC03 BRAND LEVEL SECURITY                           *         
*  LEV 58 SMUR 04MAY04 ADD MEDIA (D) NET RADIO                        *         
*  LEV 59 SMUR 26JUL04 SOX                                            *         
*  LEV 60 BGRI 13DEC04 FIX PRODUCT GROUP FILTER                       *         
*  LEV 61 BGRI 29SEP05 MORE PRODUCTS                                  *         
*  LEV 65 SMUR 10AUG07 FIX YET ANOTHER PROBLEM WITH GETEL             *         
*  LEV 66 SMUR 16JUL08 SHOW @,C,S,I,P,B, FLAGS (WAS DDS ONLY)         *         
*  LEV 73 MNAS 11JUN13 PROFILE TO INCLUDE/EXCLUDE DIGITAL             *         
*  LEV 73 SMUR 08DEC16 DISPLAY PATTERN GEN COMMENTS                   *         
*  SPEC-46336  05MAY20 FIX INSTRUCTIONS FAXED INDICATOR ON LIST SCREEN*         
*  SPEC-42474  23JAN20 SUPPORT CONVERTED DATES ON REVISION RECS       *         
*                           REMOVE  - OLD X'21' REV KEY               *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2161D NETWORK TRAFFIC REVISION COMMENT MAINTENANCE'            
T2161D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2161D**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR1DRR                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE INVALID                               
         BE    ACTERR                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       CLI   ACTNUM,ACTADD       TEST ACTION ADD                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTDEL       TEST ACTION DELETE                           
         BE    ACTERR                                                           
*                                                                               
         NI    SVFLAG,X'FF'-CONVSW INIT CONVERTED RECORDS                       
*                                                                               
         BRAS  RE,INITSPT          READ FROM SPOT FILE                          
*                                                                               
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    BCLT,BCLT           CLIENT -- REQUIRED                           
         LA    R2,TRACLTH                                                       
         GOTO1 VALICLT                                                          
*                                                                               
* READ TN2 PROFILE TO SEE IF PRDGRP REQUIRED                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'S'-X'40'     LOWERCASE S                                  
         MVC   WORK+1(3),=C'TN2'                                                
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
*                                                                               
         CLI   SVTN2PRO+0,C'A'     TEST PRDGRP REQD                             
         BL    VK02                NO                                           
         CLI   SVTN2PRO+0,C'Z'                                                  
         BH    VK02                                                             
*                                                                               
         XC    DUB,DUB                                                          
         LA    R4,DUB                                                           
         GOTO1 VALIPGR             DUMMY CALL FOR NUM DIGITS IN GRP             
*                                                                               
VK02     XC    NETWORK,NETWORK    NETWORK - REQUIRED (NOT REQ FOR DDS)          
*                                                                               
         NI    SVFLAG,X'FF'-(ALLNETSW+VALNETSW) INIT                            
         LA    R2,TRANETH                                                       
         CLI   5(R2),0             TEST NETWORK ENTERED                         
         BNE   VK05                 YES                                         
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, ACCEPT                           
         BE    *+12                                                             
*                                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   MISSERR                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
*                                                                               
         OI    SVFLAG,ALLNETSW     ALL NETWORK REQUEST                          
*                                                                               
*MNV     B     VK10                DO PROFILES LATER                            
         B     VK09                                                             
*                                                                               
VK05     BRAS  RE,VNET                                                          
*                                                                               
         XC    WORK,WORK           * READ TN PROFILE *                          
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
*                                                                               
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK08                                                             
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQUAL                       
         BO    VK08                                                             
*                                                                               
         MVC   WORK+11(1),SVPRDOFF ELSE USE PROD OFFICE                         
*                                                                               
VK08     GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* MUST READ TN2 PROFILE AFTER VNET (YES, READ IT AGAIN!)                        
*                                                                               
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   READ TN2 PROFILE                             
         MVC   WORK+6(1),SVMEDIA   BY SPECIFIC MEDIA!!!                         
         GOTO1 (RF),(R1),WORK,SVTN2PRO,DATAMGR                                  
*                                                                               
VK09     XC    OPTIONS,OPTIONS                                                  
         LA    R2,TRAOPTH                                                       
*                                                                               
         BRAS  RE,VOPT                                                          
*                                                                               
*MNV                                                                            
         CLI   TRANETH+5,0         TEST NETWORK ENTERED                         
         BE    VK10                YES                                          
         TM    SVOPTSW,OPTDIGI     INCLUDE DIGITAL SUBMEDIA V                   
         BO    VK10                                                             
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   VK10                                                             
         CLI   SVMEDIA,C'V'                                                     
         BNE   VK10                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVNETMS),INVNETMS                                     
         GOTO1 ERREX2                                                           
*MNV                                                                            
*                                                                               
VK10     BRAS  RE,INITXSP          SET TO XSPOT FILE                            
*                                                                               
* READ ANY REVISION RECORD TO SEE IF THE FILE HAS BEEN CONVERTED.               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A1D'                                                  
         GOTO1 HIGH                                                             
         CLC   =X'0A1D',KEY                                                     
         BE    VK16                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVC   0(L'NOTEMSG,R1),NOTEMSG                                          
         LA    R1,L'NOTEMSG(,R1)                                                
         MVC   0(2,R1),AGENCY                                                   
         MVC   3(3,R1),QCLT                                                     
         MVC   7(4,R1),TRANET                                                   
         MVC   12(8,R1),TRAPER                                                  
                                                                                
         OC    ELEM,SPACES                                                      
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'NOTEMSG+72,ELEM)                       
         B     VK17                NO REC FOUND, TREATE AS CONVERTED            
*                                                                               
VK16     TM    KEY+32,X'03'        CONVERTED RECORDS?                           
         BZ    *+8                                                              
VK17     OI    SVFLAG,CONVSW                                                    
*                                                                               
         XC    PERIOD,PERIOD       PERIOD                                       
         LA    R2,TRAPERH                                                       
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         BRAS  RE,VPER                                                          
         B     VK40                                                             
*                                                                               
VK30     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
*                                                                               
VK40     XC    PROGRAM,PROGRAM     PROGRAM                                      
         LA    R2,TRAPROGH                                                      
         CLI   5(R2),0             TEST PROGRAM ENTERED                         
         BE    VK60                                                             
         BRAS  RE,VPROG                                                         
         B     VK60                                                             
*                                                                               
VK50     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
*                                                                               
VK60     BRAS  RE,INITNET          READ FROM UNIT FILE                          
*                                                                               
*                                                                               
         MVI   REVNUM,0            REVISION NUMBER                              
         LA    R2,TRARNUMH                                                      
         CLI   5(R2),0                                                          
         BNE   VK70                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK90                                                             
*                                                                               
VK70     OC    PERIOD,PERIOD       TEST PERIOD ENTERED                          
         BZ    NOPERERR            REVISION NUMBER REQUIRES PERIOD              
*                                                                               
         BRAS  RE,VREVNUM                                                       
*                                                                               
VK90     XC    KEY,KEY             FILL IN REVISION COMMENT KEY                 
         LA    R4,KEY                                                           
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT FILE                            
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM,BAGYMD                                                   
         MVC   REVXKCLT,BCLT                                                    
         MVC   REVXKNET,NETWORK                                                 
         MVC   REVXKPRG,PROGRAM                                                 
         MVC   REVXKPER,PERIOD                                                  
         MVC   REVXKNUM,REVNUM                                                  
         MVC   REVXKPRD,OPTPROD                                                 
         OC    OPTPRGR,OPTPRGR     ANY PRODUCT GROUP                            
         BZ    EXIT                 NO                                          
         MVC   REVXKPGR,OPTPRGR                                                 
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
NOTEMSG  DC    C'AUTONOTE*SMUR:NO 0A1D REC FOUND IN 1D'                         
*                                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
* SEE IF WE ARE LOCKED OUT BY UNIT ALLOCATION                                   
*                                                                               
VR       DS    0H                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=C'TNET'     TEST                                         
         GOTO1 VALILOC                                                          
*                                                                               
* SEE IF WE ARE LOCKED OUT BY PATTERN GEN                                       
*                                                                               
         MVC   DUB(4),=C'TNET'                                                  
         MVI   DUB+4,X'21'                                                      
         XC    DUB+5(3),DUB+5                                                   
         GOTO1 VALILOC                                                          
*                                                                               
         L     R4,AIO                                                           
         BRAS  RE,INITXSP          SET TO XSPOT FILE                            
         USING REVXKEY,R4                                                       
         MVC   BAGYMD,REVXKAM                                                   
         MVC   BCLT,REVXKCLT                                                    
         MVC   NETWORK,REVXKNET                                                 
         MVC   PROGRAM,REVXKPRG                                                 
         MVC   PERIOD,REVXKPER                                                  
         MVC   REVNUM,REVXKNUM                                                  
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING REVREV,R6                                                        
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    VR05                                                             
         TM    PERIOD+1,X'F0'      FOR MONTHLY HOB = ZEROS                      
         BNZ   VR06                WEEKLY                                       
         B     *+12                                                             
VR05     TM    PERIOD,X'80'        THIS WEEKLY PERIOD                           
         BO    VR07                                                             
*                                                                               
         MVC   REVRDATE(2),PERIOD                                               
         MVI   REVRDATE+2,01                                                    
         B     VR07                                                             
*                                                                               
VR06     LA    R1,PERIOD                                                        
         BAS   RE,FPERDTE          FIND ACTUAL PERIOD DATE                      
         MVC   REVRDATE,NEWPER                                                  
         B     VR08                                                             
*                                                                               
         FIXDT02                                                                
VR07     GOTO1 DATCON,DMCB,(2,PERIOD),(3,REVRDATE)                              
*                                                                               
VR08     CLI   LASTSW,0            IS THIS LAST REVISION                        
         BE    VR10                NO                                           
         CLI   LASTSW,C'*'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,TRAREVH                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR10                NO                                           
         CLC   =C'YES',TRAREV      WANT TO RERUN LAST                           
         BNE   REVERR                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
         NI    REVFLAG,X'FF'-REVINS  SET OFF INSTRUCTIONS RUN                   
         XC    REVIDATE,REVIDATE   ZERO INSTRUCTIONS DATE                       
*MNREVT                                                                         
         BRAS  RE,BTIME                                                         
*MNREVT                                                                         
*                                                                               
VR10     MVI   CMTFOUND,C'N'       NO COMMENTS FOUND YET                        
         LA    R2,TRARCOMH         COMMENT FIELD                                
         MVI   ELCODE,X'40'        COMMENT ELEMENT CODE                         
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING REVCMTEL,R6                                                      
         MVI   SEQNUM,0                                                         
*                                                                               
VR20     CLI   5(R2),0             TEST FOR A COMMENT IN THIS FIELD             
         BE    VR30                                                             
*                                                                               
         MVI   CMTFOUND,C'Y'       A COMMENT WAS FOUND                          
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT ELEMENT SEQUENCE NUMBER            
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   REVCMTEL,X'40'      ELEMENT IDENTIFIER                           
         MVI   REVCMTLN,63         ELEMENT LENGTH                               
         MVC   REVNUMB,SEQNUM      ELEMENT SEQUENCE NUMBER                      
*                                                                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   REVCMT,WORK                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
VR30     ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRALCOMH                                                      
         CR    R2,RF               TEST END OF SCREEN                           
         BNL   VR40                                                             
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         B     VR20                                                             
*                                                                               
VR40     CLI   CMTFOUND,C'Y'       TEST COMMENT FOUND                           
         BE    DR20                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL            GET NET SEED AUTO SEED COMMENT               
         BE    DR20                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL            GET NET SEED COMMENTS                        
         BE    DR20                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'65'                                                     
         BAS   RE,GETEL            GET PAT GEN COMMENTS                         
         BE    DR20                MUST BE THERE                                
         OC    REVXKPRG,REVXKPRG   ANY PROGRAM                                  
         BZ    DR20                 NO, PAT GEN COMMENT NOT REQUIRED            
         CLI   REVNUM,0            TEST REVISION ZERO                           
         BNE   NOCMTERR            NEED COMMENT FOR NON-ZERO REVISION           
         B     DR20                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
* FIND PERIOD DATE                                                              
* ON ENTRY R1 = PERIOD = 1 BYTE YEAR/4 BITS MONTH#/4 BITS WEEK#                 
* WHEN DONE R1 = YYMMDD                                                         
*                                                                               
FPERDTE  NTR1                                                                   
         MVC   NEWPER(1),0(R1)     SAVE YEAR                                    
         LLC   R4,1(R1)            MD OF YMD                                    
         SRDL  R4,4                                                             
         STC   R4,NEWPER+1         MM                                           
         SRL   R5,28               WEEK NUMBER                                  
         MVI   NEWPER+2,1          START WITH 1ST OF THE MONTH                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,NEWPER),(0,DUB)                                   
         MVC   WORK(6),DUB                                                      
*                                                                               
FPER02   GOTO1 GETDAY,DMCB,DUB,WORK+10                                          
         CLI   0(R1),1             MUST BE MONDAY                               
         BE    FPER05                                                           
*                                                                               
         GOTO1 ADDAY,DMCB,DUB,DUB,1                                             
         CLC   WORK+2(2),DUB+2    SAME MONTH?                                   
         BE    FPER02                                                           
         DC    H'0'                BUG CATCHER                                  
*                                                                               
FPER05   CHI   R5,1                WEEK ONE ?                                   
         BE    FPER06              YES, THIS IS THE DATE                        
         BCTR  R5,0                WEEK# MINUS 1                                
         MH    R5,=H'7'                                                         
         GOTO1 ADDAY,DMCB,DUB,DUB,(R5)     YYMMDD                               
*                                                                               
FPER06   GOTO1 DATCON,DMCB,(0,DUB),(3,NEWPER)                                   
         XIT1                                                                   
                                                                                
*                                                                               
*MNREVT                                                                         
*                                                                               
* BUILD/UPDATE TIME ELEMENT                                                     
*                                                                               
BTIME    NTR1                                                                   
         BRAS  RE,INITXSP                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BE    BTIM20                                                           
                                                                                
         XC    ELEM,ELEM                                                        
         USING REVTMEL,R6                                                       
         LA    R6,ELEM                                                          
         USING REVTMEL,R6                                                       
         MVI   REVTMEL,X'70'                                                    
         MVI   REVTMLN,REVTMEQ                                                  
         EDIT  (TIME,NOW),(8,WORK)                                              
         MVC   REVTIME(2),WORK                                                  
         MVC   REVTIME+2(2),WORK+3                                              
         MVC   REVTIME+4(2),WORK+6                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,REVDATE)                                    
         MVI   REVTPG,REVREVL                                                   
                                                                                
BTIM10   GOTO1 ADDELEM                                                          
         B     BTIMXIT                                                          
                                                                                
BTIM20   DS    0H                                                               
         MVC   REVTIME2,REVTIME1                                                
         MVC   REVDATE2,REVDATE1                                                
         MVC   REVTPG2,REVTPG1                                                  
         MVC   REVTIME1,REVTIME                                                 
         MVC   REVDATE1,REVDATE                                                 
         MVC   REVTPG1,REVTPG                                                   
         EDIT  (TIME,NOW),(8,WORK)                                              
         MVC   REVTIME(2),WORK                                                  
         MVC   REVTIME+2(2),WORK+3                                              
         MVC   REVTIME+4(2),WORK+6                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,REVDATE)                                    
         MVI   REVTPG,REVREVL                                                   
                                                                                
BTIM30   DS    0H                                                               
                                                                                
BTIMXIT  XIT1                                                                   
*MNREVT                                                                         
* DISPLAY RECORD                                                                
*                                                                               
* SEE IF THIS IS LAST REVISION *                                                
*                                                                               
DR       L     R6,AIO                                                           
         USING REVXKEY,R4                                                       
         LA    R4,KEY                                                           
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT FILE                            
         MVC   KEY(32),0(R6)                                                    
         ZIC   RF,REVXKNUM                                                      
         STC   RF,REVNUM                                                        
         LA    RF,1(,RF)                                                        
         STC   RF,REVXKNUM                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE     FIND NEXT REVSION                            
         BE    DR10                                                             
         MVI   LASTSW,C'*'         THIS MUST BE LAST                            
         MVC   TRAREVT,=C'RERUN LAST REV'                                       
         XC    TRAREV,TRAREV                                                    
         NI    TRAREVH+1,X'FF'-X'20' UNPROTECT                                  
         B     DR14                                                             
*                                                                               
DR10     XC    TRAREVT,TRAREVT                                                  
         XC    TRAREV,TRAREV                                                    
         OI    TRAREVH+6,X'20'     SET ON PROTECT                               
DR14     OI    TRAREVTH+6,X'80'                                                 
         OI    TRAREVH+6,X'80'                                                  
*                                                                               
         MVC   KEY(32),0(R6)                                                    
         GOTO1 HIGH                                                             
         DROP  R4                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
DR20     L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET REVISION ELEMENT                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
*                                                                               
         USING REVREVEL,R6                                                      
*                                                                               
         CLI   REVDTALN,12         OLD REVISION REC                             
         BE    DR30                                                             
         EDIT  (B1,REVNNUM),(4,TRANNUM),ALIGN=LEFT,ZERO=NOBLANK                 
         OI    TRANNUMH+6,X'80'                                                 
*                                                                               
DR30     MVC   TRARDTE,SPACES      ASSIGNMENT DATE                              
         MVC   TRARDTE(16),=C'ORIGINAL DATE = '                                 
         CLI   REVNUM,0            TEST REVISION ZERO                           
         BNE   *+10                                                             
         MVC   TRARDTE(16),=C'REVISION DATE = '                                 
         GOTO1 DATCON,DMCB,(3,REVADATE),(8,TRARDTE+16)                          
         OI    TRARDTEH+6,X'80'                                                 
*                                                                               
         MVC   TRAIDTE,SPACES      INSTRUCTIONS DATE                            
         TM    REVFLAG,REVINS      TEST INSTRUCTIONS RUN                        
         BZ    DR40                                                             
*                                                                               
         OC    REVIDATE,REVIDATE   TEST FOR INSTRUCTIONS DATE                   
         BNZ   *+6                 BLOW UP IF NONE                              
         DC    H'00'                                                            
         MVC   TRAIDTE(20),=C'INSTRUCTIONS DATE = '                             
         GOTO1 DATCON,DMCB,(3,REVIDATE),(8,TRAIDTE+20)                          
         B     DR50                                                             
*                                                                               
DR40     OC    REVIDATE,REVIDATE   TEST FOR INSTRUCTIONS DATE                   
         BZ    *+6                 BLOW UP IF THERE IS ONE                      
         DC    H'00'                                                            
         MVC   TRAIDTE(21),=C'(NO INSTRUCTIONS YET)'                            
*                                                                               
DR50     OI    TRAIDTEH+6,X'80'                                                 
         XC    TRASEED,TRASEED                                                  
         OI    TRASEEDH+6,X'80'                                                 
         TM    REVFLAG,REVSEED     TEST NET SEED RUN                            
         BZ    *+10                                                             
         MVC   TRASEED,=C'SEED'                                                 
*                                                                               
         XC    TRACABL,TRACABL                                                  
         OI    TRACABLH+6,X'80'                                                 
         TM    REVFLAG,REVCAB      TEST CABLE INSTRUCTIONS RUN                  
         BZ    *+10                                                             
         MVC   TRACABL,=C'CABLE'                                                
*                                                                               
         USING REVCMTEL,R6                                                      
*                                                                               
         LA    R2,TRARCOMH         FIRST COMMENT DISPLAY FIELD                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL            TEST ANY COMMENTS IN RECORD                  
         BNE   DR70                CLEAR SCREEN AND EXIT                        
*                                                                               
DR60     MVC   WORK(L'TRARCOM),SPACES                                           
         MVC   WORK(L'REVCMT),REVCMT                                            
         MVC   8(L'TRARCOM,R2),WORK   TRANSMIT NEW FIELD                        
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRALCOMH                                                      
         CR    R2,RF               TEST END OF SCREEN                           
         BNL   DR80                                                             
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
*                                                                               
         BAS   RE,NEXTEL           NEXT COMMENT LINE                            
         BE    DR60                                                             
*                                                                               
DR70     CLC   8(L'TRARCOM,R2),SPACES   BLANK IS OK                             
         BE    DR74                                                             
         OC    8(L'TRARCOM,R2),8(R2)    ZEROS TOO                               
         BZ    DR74                                                             
         MVC   8(L'TRARCOM,R2),SPACES   BLANK OUT REMAINING FIELDS              
         OI    6(R2),X'80'                                                      
DR74     ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRALCOMH                                                      
         CR    R2,RF               TEST END OF COMMENT FIELDS                   
         BNL   DR80                                                             
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         B     DR70                                                             
*                                                                               
DR80     OC    TRAASED,TRAASED                                                  
         BZ    *+14                                                             
         XC    TRAASED,TRAASED                                                  
         OI    TRAASEDH+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL            GET NET AUTO SEED COMMENT IF ANY             
         BNE   DR84                MUST BE THERE                                
         MVC   TRAASED,3(R6)                                                    
         OI    TRAASEDH+6,X'80'                                                 
*                                                                               
DR84     LA    R2,TRASCOMH                                                      
         LR    RE,R2                                                            
         LA    RF,4                                                             
DR86     OC    8(L'TRASCOM,RE),8(RE)                                            
         BZ    *+14                                                             
         XC    8(L'TRASCOM,RE),8(RE)                                            
         OI    6(RE),X'80'                                                      
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         BCT   RF,DR86                                                          
*                                                                               
         LA    R4,4                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL            GET NET SEED COMMENT IF ANY                  
         BE    DR88                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'65'                                                     
         BAS   RE,GETEL            GET PAT GEN COMMENT IF ANY                   
         BNE   DRX                  NONE                                        
*                                                                               
DR88     MVC   8(60,R2),3(R6)                                                   
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL           GET NEXT SEED COMMENT IF ANY                 
         BNE   DRX                                                              
         BCT   R4,DR88                                                          
         DC    H'0'                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       CLI   ACTNUM,ACTDEL       TEST ACTION DELETE                           
         BE    ACTERR                                                           
*                                                                               
         L     R4,AIO              RECORD SELECTED                              
*                                                                               
DK60     DS    0H                                                               
         USING REVXKEY,R4                                                       
         GOTO1 CLUNPK,DMCB,REVXKCLT,TRACLT                                      
         OI    TRACLTH+6,X'80'                                                  
         MVC   TRANET,REVXKNET                                                  
         OI    TRANETH+6,X'80'                                                  
         MVC   TRAPROG,REVXKPRG                                                 
         OI    TRAPROGH+6,X'80'                                                 
         MVI   WORK+2,0                                                         
         MVC   WORK(2),REVXKPER                                                 
*                                                                               
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    DK65                                                             
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BNZ   DK66                WEEKLY                                       
         B     *+12                                                             
DK65     TM    REVXKPER,X'80'      THIS WEEKLY REV REC                          
         BO    DK70                 YES                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,TRAPER)                                  
         B     DK74                                                             
*                                                                               
DK66     LA    R1,REVXKPER                                                      
         BAS   RE,FPERDTE          FIND ACTUAL PERIOD DATE                      
         GOTO1 DATCON,DMCB,(3,NEWPER),(5,TRAPER)                                
         B     DK74                                                             
*                                                                               
         FIXDT02                                                                
DK70     GOTO1 DATCON,DMCB,(2,WORK),(5,TRAPER)                                  
*                                                                               
DK74     OI    TRAPERH+6,X'80'                                                  
         EDIT  (B1,REVXKNUM),(4,TRARNUM),ALIGN=LEFT,ZERO=NOBLANK                
         OI    TRARNUMH+6,X'80'                                                 
*                                                                               
         XC    TRAOPT,TRAOPT                                                    
*                                                                               
         OC    REVXKPGR,REVXKPGR                                                
         BNZ   DK80                 YES                                         
*                                                                               
         OC    REVXKPRD,REVXKPRD   ANY PRD                                      
         BZ    DK86                 NO                                          
         MVC   TRAOPT(4),=C'PRD='                                               
         MVC   TRAOPT+4(3),REVXKPRD                                             
         B     DK88                                                             
*                                                                               
DK80     LA    R1,REVXKPGR                                                      
         BRAS  RE,PGRUNPK                                                       
         MVC   TRAOPT+0(4),WORK    MOVE PGR=                                    
         MVC   TRAOPT+4(3),WORK+5  AND GROUP DIGITS                             
*                                                                               
DK86     OI    TRAOPTH+6,X'80'                                                  
*                                                                               
DK88     CLC   TRALSTR,SPACES                                                   
         BE    DKX                                                              
         OC    TRALSTR,TRALSTR                                                  
         BZ    DKX                                                              
         XC    TRALSTR,TRALSTR                                                  
         OI    TRALSTRH+6,X'80'                                                 
*                                                                               
DKX      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       DS   0H                                                                
         CLI   PGRLEN,0            TEST HAVE PRDGRP LEN                         
         BNE   LR02                                                             
         CLI   SVTN2PRO+00,C'A'    TEST PRDGRP REQD                             
         BL    LR02                                                             
*                                                                               
LR02     LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         LARL  R1,HEADING                                                       
         ST    R1,SPECS                                                         
*                                                                               
         OC    KEY(20),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
                                                                                
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT                                 
*                                                                               
         MVC   REVXKID,=X'0A1D'        REVISION COMMENT RECORD KEY              
         MVC   REVXKAM,BAGYMD                                                   
         MVC   REVXKCLT,BCLT                                                    
         MVC   REVXKNET,NETWORK                                                 
         MVC   REVXKPRG,PROGRAM                                                 
         MVC   REVXKPER,PERIOD                                                  
         MVC   REVXKNUM,REVNUM                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         TM    SVFLAG,ALLNETSW     ALL NETWORK REQUEST ?                        
         BZ    LR30                 NO                                          
         OI    SVFLAG,VALNETSW     VALIDATE FOR THIS NETWORK                    
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     TM    SVFLAG,ALLNETSW     ALL NETWORK REQUEST ?                        
         BZ    LR32                NETWORK SPECIFIC REQUEST                     
*                                                                               
         CLC   KEY(5),SAVEKEY      TEST SAME TYPE/AGMD/CLT                      
         BNE   LRX                                                              
*                                                                               
         MVC   MYKEY,KEY                                                        
*                                                                               
         OC    NETWORK,NETWORK                                                  
         BNZ   *+14                                                             
         MVC   WORK(L'REVXKNET),REVXKNET                                        
         B     LR31                                                             
*                                                                               
         CLC   NETWORK,REVXKNET    SAME NETWORK?                                
         BE    LR33                 YES                                         
         MVC   WORK,REVXKNET                                                    
*                                                                               
LR31     BRAS  RE,VNET                                                          
         MVC   KEY,MYKEY                                                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                DUMMY READ HIGH FOR GETREC                   
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK           * READ TN PROFILE *                          
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
*                                                                               
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    LR31F                                                            
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQUAL                       
         BO    LR31F                                                            
*                                                                               
         MVC   WORK+11(1),SVPRDOFF ELSE USE PROD OFFICE                         
*                                                                               
LR31F    GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* MUST READ TN2 PROFILE AFTER VNET                                              
*                                                                               
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   READ TN2 PROFILE                             
         MVC   WORK+6(1),SVMEDIA   BY SPECIFIC MEDIA!!!                         
         GOTO1 (RF),(R1),WORK,SVTN2PRO,DATAMGR                                  
*                                                                               
         XC    OPTIONS,OPTIONS                                                  
         LA    R2,TRAOPTH                                                       
*                                                                               
         MVC   MYKEY,KEY                                                        
         BRAS  RE,VOPT                                                          
         MVC   KEY,MYKEY                                                        
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT FILE                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                DUMMY READ HIGH FOR SEQ                      
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PERIOD,PERIOD       PERIOD                                       
         LA    R2,TRAPERH                                                       
         CLI   5(R2),0                                                          
         BE    LR33                                                             
         BRAS  RE,VPER                                                          
         B     LR33                                                             
*                                                                               
LR32     CLC   KEY(9),SAVEKEY      TEST SAME TYPE/AGMD/CLT/NETWORK              
         BNE   LRX                                                              
         B     LR33                                                             
*                                                                               
LR33     DS    0H                                                               
*MNV                                                                            
*        OI    SVFLAG,ALLNETSW     ALL NETWORK REQUEST                          
*        BZ    *+28                                                             
         TM    SVOPTSW,OPTDIGI     INCLUDE DIGITAL SUBMEDIA V                   
         BO    *+20                                                             
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   *+12                                                             
         CLI   SVMEDIA,C'V'                                                     
         BE    LR20                                                             
*MNV                                                                            
*                                                                               
         OC    SAVEKEY+9(6),SAVEKEY+9     TEST PROGRAM ENTERED                  
         BZ    LR34                                                             
         CLC   REVXKPRG,SAVEKEY+9         IF SO, TEST KEY MATCH                 
         BNE   LRX                                                              
*                                                                               
LR34     TM    SVFLAG,ALLNETSW     ALL NETWORK REQUEST ?                        
         BZ    LR35                 NO                                          
         OC    PERIOD,PERIOD       WAS PERIOD ENTERED?                          
         BZ    LR36                 NO                                          
*                                                                               
         CLC   REVXKPER,PERIOD     SAME PERIOD?                                 
         BE    LR36                 YES                                         
         B     LR20                                                             
*                                                                               
LR35     DS   0H                                                                
         OC    SAVEKEY+15(2),SAVEKEY+15   TEST PERIOD ENTERED                   
         BZ    LR36                                                             
         CLC   REVXKPER,SAVEKEY+15        IF SO, TEST KEY MATCH                 
         BNE   LR20                                                             
*                                                                               
LR36     DS   0H                                                                
         OC    SAVEKEY+17(1),SAVEKEY+17   TEST REVISION NUMBER ENTERED          
         BZ    LR40                                                             
         CLC   REVXKNUM,SAVEKEY+17        IF SO, TEST KEY MATCH                 
         BNE   LR20                                                             
*                                                                               
LR40     TM    SVOPTSW,OPTWEEK+OPTMONTH                                         
         BZ    LR46                                                             
         TM    SVOPTSW,OPTWEEK                                                  
         BO    LR44                                                             
*                                                                               
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    LR42                                                             
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BZ    LR46                MONTHLY                                      
         B     LR20                                                             
*                                                                               
LR42     TM    REVXKPER,X'80'       WEEKLY REVISION                             
         BZ    LR46                                                             
         B     LR20                                                             
LR44     DS   0H                                                                
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+16                                                             
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BZ    LR20                MONTHLY                                      
         B     LR46                                                             
         TM    REVXKPER,X'80'      WEEKLY REVISION                              
         BZ    LR20                                                             
*                                                                               
LR46     DS    0H                                                               
         OC    OPTPROD,OPTPROD                                                  
         BNZ   *+12                                                             
         CLI   OPTPRD,0            ANY FILTER ON PROD                           
         BE    LR48                 NO                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BNE   LR20                                                             
*                                                                               
LR48     OC    OPTPRGR,OPTPRGR     ANY FILTER ON PRODUCT GROUP                  
         BZ    *+14                 NO                                          
         CLC   REVXKPGR,OPTPRGR                                                 
         BNE   LR20                                                             
*                                                                               
*                                                                               
* CHECK BRAND LEVEL SECURITY                                                    
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    LR53                                                             
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),22(R4)                                                 
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   LR20                BYPASS PRD                                   
*                                                                               
LR53     L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         LR    R6,R4                                                            
         GOTO1 GETREC              GET REVISION COMMENT RECORD                  
         MVI   ELCODE,X'10'        REVISION DATA ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
         MVI   NREVSW,0            NET REV SWITCH                               
         USING REVREVEL,R6                                                      
         CLI   REVDTALN,12         OLD RECORD                                   
         BE    *+24                                                             
         OC    REVNNUM,REVNNUM     REV NUM=0                                    
         BZ    *+14                                                             
         MVC   SVREVNUM,REVNNUM    SAVE NET REV #                               
         MVI   NREVSW,C'Y'         NET REV SWITCH                               
         CLI   OPTPRD,0            ANY FILTER ON PROD                           
         BE    LR54                 NO                                          
         TM    REVFLAG,REVPRGRP    THIS A PROD GROUP REC                        
         BO    LR20                                                             
LR54     OC    OPTPRGR,OPTPRGR     ANY FILTER ON PRODUCT GROUP                  
         BZ    LR60                 NO                                          
         TM    REVFLAG,REVPRGRP    THIS A PROD GROUP REC                        
         BZ    LR20                                                             
*                                                                               
LR60     TM    SVOPTSW,OPTACT      IS ACTIVITY REQUESTED?                       
         BZ    LR64                                                             
*                                                                               
         TM    REVFLAG,REVINS      WERE INSTR RUN                               
         BO    LR20                 YES                                         
*                                                                               
LR64     TM    SVOPTSW,OPTSEED     IS ACTIVITY REQUESTED?                       
         BZ    LR66                                                             
*                                                                               
         TM    REVFLAG,REVSEED     WAS SEED RUN                                 
         BZ    LR20                 NO                                          
*                                                                               
LR66     TM    SVOPTSW,OPTCAB      WAS CABLE RUN                                
         BZ    LR68                                                             
*                                                                               
         TM    REVFLAG,REVCAB      WAS CABLE RUN                                
         BZ    LR20                 NO                                          
*                                                                               
LR68     TM    SVOPTSW,OPTFAX      WAS FAX SENT                                 
         BZ    LR70                                                             
*                                                                               
         TM    REVFLAG,REVFAX      WAS FAX SENT                                 
         BZ    LR20                 NO                                          
*                                                                               
LR70     CLI   MODE,LISTRECS       ONLINE LIST                                  
         BE    LRL                                                              
         CLI   MODE,PRINTREP       OFFLINE LIST                                 
         BE    LRR                                                              
         DC    H'0'                                                             
LRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* ONLINE LIST *                                                                 
*                                                                               
LRL      DS    0H                                                               
         MVC   LISTAR,SPACES       FILL KEY IN LIST LINE                        
         MVC   LSTPROG,REVXKPRG                                                 
         MVI   WORK+2,0                                                         
         MVC   WORK(2),REVXKPER                                                 
*                                                                               
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+16                                                             
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BNZ   LRL110              WEEKLY                                       
         B     *+12                                                             
         TM    REVXKPER,X'80'       WEEKLY REVISION                             
         BO    LRL110                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,LSTPER)                                  
         B     LRL112                                                           
*                                                                               
LRLBKUP  CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)            LEAVE A SPACE AFTER LAST CHAR                
         BR    RE                                                               
* WEEKLY                                                                        
LRL110   TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    LRL111                                                           
         LA    R1,REVXKPER                                                      
         BAS   RE,FPERDTE          FIND ACTUAL PERIOD DATE                      
         GOTO1 DATCON,DMCB,(3,NEWPER),(5,LSTPER)                                
         B     LRL112                                                           
*                                                                               
         FIXDT02                                                                
LRL111   GOTO1 DATCON,DMCB,(2,WORK),(5,LSTPER)                                  
*                                                                               
LRL112   CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING REVREV,R6                                                        
         TM    REVFLAG,REVCAL      PERIOD IS CALENDAR                           
         BZ    *+8                                                              
         MVI   LSTPER+7,C'C'                                                    
         TM    REVFLAG,REVBRD      PERIOD IS BROADCAST                          
         BZ    *+8                                                              
         MVI   LSTPER+7,C'B'                                                    
         TM    REVFLAG,REVPAT      PATTERN GEN ?                                
         BZ    *+8                                                              
         MVI   LSTPER+8,C'P'                                                    
         TM    REVFLAG,REVINS      INSTR GENED?                                 
         BZ    *+8                                                              
         MVI   LSTPER+9,C'I'                                                    
         TM    REVFLAG,REVSEED     FROM NET SEED?                               
         BZ    *+8                                                              
         MVI   LSTPER+10,C'S'                                                   
         TM    REVFLAG,REVCAB      CABLE INSTR?                                 
         BZ    *+8                                                              
         MVI   LSTPER+11,C'C'                                                   
         TM    REVFLAG,REVFAX      INSTRUCTION FAXED?                           
         BZ    *+8                                                              
         MVI   LSTPER+12,C'@'                                                   
LRL114   CLI   REVXKNUM,0                                                       
         BE    LRL116                                                           
         EDIT  (B1,REVXKNUM),(3,LSTRNUM)                                        
         B     *+8                                                              
LRL116   MVI   LSTRNUM+2,C'0'                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,REVADATE),(8,LSTRDATE)                            
         TM    REVFLAG,REVINS      TEST INSTRUCTIONS HAVE BEEN RUN              
         BZ    LRL120                                                           
*                                                                               
         OC    REVIDATE,REVIDATE   TEST FOR INSTRUCTIONS DATE                   
         BNZ   *+6                 BLOW UP IF NONE                              
         DC    H'00'                                                            
         MVI   LSTRNUM+3,C'*'      INDICATES INSTRUCTIONS HAVE BEEN RUN         
         GOTO1 DATCON,DMCB,(3,REVIDATE),(8,LSTIDATE)                            
         B     LRL130                                                           
*                                                                               
LRL120   OC    REVIDATE,REVIDATE   TEST FOR INSTRUCTIONS DATE                   
         BZ    *+6                 BLOW UP IF THERE IS ONE                      
         DC    H'00'                                                            
*                                                                               
LRL130   LA    R2,LSTCOMNT                                                      
         OC    REVXKPRD,REVXKPRD                                                
         BZ    LRL134                                                           
         MVC   0(4,R2),=C'PRD='                                                 
         MVC   4(3,R2),REVXKPRD                                                 
         LA    R2,6(R2)                                                         
         CLI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         BNH   LRL138                                                           
         LA    R2,1(R2)                                                         
         B     LRL138                                                           
*                                                                               
LRL134   DS    0H                                                               
         OC    REVXKPGR,REVXKPGR                                                
         BZ    LRL138                                                           
         LA    R1,REVXKPGR                                                      
         BRAS  RE,PGRUNPK                                                       
         MVC   0(8,R2),WORK                                                     
         LA    R2,9(R2)                                                         
         BAS   RE,LRLBKUP                                                       
*                                                                               
LRL138   DS    0H                                                               
         CLI   NREVSW,C'Y'         NET REV SWITCH                               
         BNE   LRL140                                                           
         MVI   NREVSW,C'N'                                                      
         MVC   0(3,R2),=C'NR='                                                  
         EDIT  (B1,SVREVNUM),(3,3(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         LA    R2,4(R2)                                                         
         BAS   RE,LRLBKUP                                                       
*                                                                               
LRL140   LR    R6,R4                                                            
         MVI   ELCODE,X'40'        COMMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   LRL150                                                           
         USING REVCMTEL,R6                                                      
         LA    R1,LSTCOMNT                                                      
         CR    R1,R2                                                            
         BE    LRL144                                                           
*                                                                               
         LA    R1,L'LSTCOMNT(R1)                                                
         SR    R1,R2               GET LENGTH OF OUTPUT FIELD                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),REVCMT                                                   
         B     LRL150                                                           
*                                                                               
LRL144   MVC   LSTCOMNT,REVCMT                                                  
*                                                                               
LRL150   CLI   1(RA),C'*'          DDS TERMINAL                                 
         BNE   LRL170                                                           
*                                                                               
         TM    SVFLAG,ALLNETSW     ALL NETWORKS?                                
         BZ    LRL155                                                           
         MVI   LSTNET,C' '                                                      
         MVC   LSTNET+1(L'NETWORK),NETWORK  YES, SHOW NETWORK                   
         XC    LSTCOMTS,LSTCOMTS                                                
*                                                                               
LRL155   LR    R6,R4                                                            
         MVI   ELCODE,X'20'        TRAFFIC SUPPLIER ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   LRL170                                                           
         USING REVTSEL,R6                                                       
         TM    REVTSFLG,REVITS+REVINTS                                          
         BZ    LRL160                                                           
         MVC   LSTCOMTS,=C' T'                                                  
         TM    REVTSFLG,REVITS                                                  
         BO    LRL160                                                           
         MVI   LSTCOMTS+1,C'N'                                                  
         TM    REVTSFLG,REVINTS                                                 
         BO    LRL160                                                           
         MVI   LSTCOMTS+1,C' '                                                  
LRL160   BAS   RE,NEXTEL                                                        
         BNE   LRL170                                                           
         MVI   LSTCOMTS+1,C'+'     MORE THAN 1 TSUPP ELEMENTS FOUND             
*                                                                               
LRL170   GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
*                                                                               
* ONLINE REPORT *                                                               
*                                                                               
         USING REVXKEY,R4                                                       
LRR      MVC   P,SPACES                                                         
*                                                                               
         MVC   PPROG,REVXKPRG                                                   
         MVC   WORK(2),REVXKPER                                                 
         MVI   WORK+2,0                                                         
*                                                                               
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+16                                                             
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BNZ   LRR20               WEEKLY                                       
         B     *+12                                                             
         TM    REVXKPER,X'80'       WEEKLY REVISION                             
         BO    LRR21                                                            
         GOTO1 DATCON,DMCB,(3,WORK),(6,PPER)                                    
         B     LRR22                                                            
*                                                                               
* CONVERTED WEEKLY                                                              
LRR20    DS    0H                                                               
         LA    R1,REVXKPER                                                      
         BAS   RE,FPERDTE          FIND ACTUAL PERIOD DATE                      
         GOTO1 DATCON,DMCB,(3,NEWPER),(5,PPER)                                  
         B     LRR22                                                            
                                                                                
         FIXDT02                                                                
LRR21    GOTO1 DATCON,DMCB,(2,WORK),(5,PPER)                                    
*                                                                               
LRR22    CLI   REVXKNUM,0                                                       
         BE    LRR23                                                            
         EDIT  (B1,REVXKNUM),(3,PREVNO)                                         
         B     *+8                                                              
LRR23    MVI   PREVNO+2,C'0'                                                    
*                                                                               
         OC    REVXKPRD,REVXKPRD     THIS PRODUCT                               
         BNZ   LRR23C                 YES                                       
*                                                                               
         OC    REVXKPGR,REVXKPGR     OR PRODUCT GROUP                           
         BZ    LRR25                  NO                                        
*                                                                               
LRR23C   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREV,R6                                                        
         OC    REVXKPRD,REVXKPRD   THIS PROD                                    
         BZ    LRR24                YES                                         
         MVC   PPRD,=C'PRD='                                                    
         MVC   PPRD+132(3),REVXKPRD                                             
         B     LRR25                                                            
*                                                                               
LRR24    OC    REVXKPGR,REVXKPGR   THIS PROD GRP                                
         BZ    LRR25                                                            
*                                                                               
         LA    R1,REVXKPGR                                                      
         BRAS  RE,PGRUNPK                                                       
         MVC   PPRD(4),WORK        MOVE PGR=                                    
         MVC   PPRD+133(4),WORK+4    MOVE X108                                  
*                                                                               
LRR25    GOTO1 DATCON,DMCB,(3,REVADATE),(8,PRDATE)                              
         TM    REVFLAG,REVINS      TEST INSTRUCTIONS HAVE BEEN RUN              
         BZ    LRR26                                                            
*                                                                               
         OC    REVIDATE,REVIDATE   TEST FOR INSTRUCTIONS DATE                   
         BNZ   *+6                 BLOW UP IF NONE                              
         DC    H'0'                                                             
         MVI   PINSTR,C'*'         INDICATES INSTRUCTIONS HAVE BEEN RUN         
         GOTO1 DATCON,DMCB,(3,REVIDATE),(8,PIDATE)                              
         B     LRR27                                                            
*                                                                               
LRR26    OC    REVIDATE,REVIDATE   TEST FOR INSTRUCTIONS DATE                   
         BZ    *+6                 BLOW UP IF THERE IS ONE                      
         DC    H'0'                                                             
*                                                                               
LRR27    LA    R2,PCMT                                                          
         CLI   NREVSW,C'Y'         NET REV SWITCH                               
         BNE   LRR30                                                            
         MVI   NREVSW,C'N'                                                      
         MVC   0(3,R2),=C'NR='                                                  
         EDIT  (B1,SVREVNUM),(3,3(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         LA    R2,6(R2)                                                         
*                                                                               
LRR30    DS   0H                                                                
         TM    SVFLAG,ALLNETSW     ALL NETWORK REQUEST                          
         BZ    *+10                                                             
         MVC   PPROG+132+2(4),REVXKNET                                          
*                                                                               
         TM    REVFLAG,REVCAL      PERIOD IS CALENDAR                           
         BZ    *+8                                                              
         MVI   PPER+132,C'C'                                                    
         TM    REVFLAG,REVBRD      PERIOD IS BROADCAST                          
         BZ    *+8                                                              
         MVI   PPER+132,C'B'                                                    
         TM    REVFLAG,REVPAT      PATTERN GEN ?                                
         BZ    *+8                                                              
         MVI   PPER+132+1,C'P'                                                  
         TM    REVFLAG,REVINS      INSTR GENED?                                 
         BZ    *+8                                                              
         MVI   PPER+132+2,C'I'                                                  
         TM    REVFLAG,REVSEED     FROM NET SEED?                               
         BZ    *+8                                                              
         MVI   PPER+132+3,C'S'                                                  
         TM    REVFLAG,REVCAB      CABLE INSTR?                                 
         BZ    *+8                                                              
         MVI   PPER+132+4,C'C'                                                  
         TM    REVFLAG,REVFAX      INSTRUCTION FAXED?                           
         BZ    *+8                                                              
         MVI   PPER+132+5,C'@'                                                  
         LR    R6,R4                                                            
         USING REVCMTEL,R6                                                      
         MVI   ELCODE,X'40'        COMMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   LRR50                                                            
*                                                                               
         LA    R0,PCMT                                                          
         CR    R0,R2                                                            
         BE    LRR40                                                            
         MVC   PCMT+6(L'PCMT-6),REVCMT                                          
         B     *+10                                                             
LRR40    MVC   PCMT,REVCMT                                                      
*                                                                               
LRR46    GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL                                                        
         BE    LRR40                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
LRR50    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
         B     LR20                                                             
*                                                                               
*                                                                               
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         MVC   H6+10(4),NETWORK                                                 
         LA    R0,H3+36                                                         
         LR    R1,R0                                                            
         TM    SVOPTSW,OPTMONTH                                                 
         BZ    HDHK10                                                           
         MVC   0(7,R1),=C'MONTHLY'                                              
         LA    R1,7(,R1)                                                        
HDHK10   TM    SVOPTSW,OPTWEEK                                                  
         BZ    HDHK20                                                           
         MVC   0(6,R1),=C'WEEKLY'                                               
         LA    R1,6(,R1)                                                        
HDHK20   CLI   OPTPRD,0            ANY FILTER ON PROD                           
         BE    HDHK30               NO                                          
         CR    R0,R1                                                            
         BE    HDHK24                                                           
         MVI   0(R1),C','                                                       
         LA    R1,1(,R1)                                                        
HDHK24   MVC   0(4,R1),=C'PRD='                                                 
         MVC   4(3,R1),OPTPROD                                                  
HDHK30   OC    OPTPRGR,OPTPRGR     ANY FILTER ON PRODUCT GROUP                  
         BZ    HDHK40               NO                                          
         CR    R0,R1                                                            
         BE    HDHK34                                                           
         MVI   0(R1),C','                                                       
         LA    R1,1(,R1)                                                        
HDHK34   MVC   0(5,R1),=C'PGRP='                                                
         MVC   5(1,R1),SVTN2PR1                                                 
         MVC   DUB(2),OPTPRGR                                                   
         UNPK  DUB+3(5),DUB(3)                                                  
         LLC   RE,PGRLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R1),DUB+3                                                    
HDHK40   BR    RE                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
REVERR   DS   0H                                                                
         LA    RF,L'REVERRMS-1                                                  
         L     R1,=A(REVERRMS)                                                  
         B     ERREXIT                                                          
*                                                                               
NOPERERR DS   0H                                                                
         LA    RF,L'NOPERMS-1                                                   
         L     R1,=A(NOPERMS)                                                   
         LA    R2,TRAPERH                                                       
         B     ERREXIT                                                          
*                                                                               
NOCMTERR DS   0H                                                                
         LA    RF,L'NOCMTMS-1                                                   
         L     R1,=A(NOCMTMS)                                                   
         LA    R2,TRARCOMH                                                      
ERREXIT  DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTR1DRR                                                      
         EX    RF,EREXMVC                                                       
         GOTO1 ERREX2                                                           
EREXMVC  MVC   CONHEAD(0),0(R1)                                                 
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
ACTERR   MVI   ERROR,INVACT                                                     
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRLENMS  DC    C'* ERROR * PRODUCT MORE THAN 3 CHARACTERS *'                    
BDKYWDMS DC    C'* ERROR * ENTER WEEK OR MONTH, LEAVE BLANK FOR BOTH *'         
REVERRMS DC    C'* ERROR * ENTER YES TO RERUN LAST REVISION *'                  
NOPERMS  DC    C'* ERROR * REVISION NUMBER REQUIRES PERIOD *'                   
NOCMTMS  DC    C'* ERROR * MUST HAVE COMMENT FOR THIS REVISION *'               
BDPGRPMS DC    C'* ERROR * NO PRODUCT GROUP X0000 FOUND *'                      
PGRPERMS DC    C'* ERROR * PROD GROUP MUST BE 1-2 DIGITS *'                     
INVNETMS DC    C'* ERROR * INVALID NETWORK REQUEST *'                           
*                                                                               
         DS    0H                                                               
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,34,C'TRAFFIC REVISIONS LIST'                                  
         SSPEC H2,34,C'----------------------'                                  
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H6,3,C'NETWORK'                                                  
         SSPEC H4,86,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'PROGRAM'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,11,C'PERIOD'                                                  
         SSPEC H9,11,C'------'                                                  
         SSPEC H8,25,C'REV'                                                     
         SSPEC H9,25,C'---'                                                     
         SSPEC H8,30,C'COMMENTS'                                                
         SSPEC H9,30,C'--------'                                                
         SSPEC H8,93,C'REV DATE'                                                
         SSPEC H9,93,C'--------'                                                
         SSPEC H8,103,C'INS DATE'                                               
         SSPEC H9,103,C'--------'                                               
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
*                                                                               
VPROG    NMOD1 0,**+VPR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING NPGRECD,R4                                                       
*                                                                               
         CLI   5(R2),3            INPUT LEN                                     
         BNE   VPROG04                                                          
         CLI   ACTNUM,ACTLIST     THIS LIST FUNCTION                            
         BNE   VPROG04                                                          
         CLC   =C'ALL',TRAPROG                                                  
         BE    VPROGX                                                           
*                                                                               
VPROG04  GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         BRAS  RE,INITSPT                                                       
*                                                                               
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVC   NPGKTYP,=X'0D20'    NETWORK PROGRAM RECORD                       
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BNET                                                     
         MVC   NPGKPROG,WORK                                                    
         GOTO1 HIGH                                                             
         MVI   FOUNDFLG,C'N'       NO PROGRAM FOUND YET                         
         B     VPROG20                                                          
*                                                                               
VPROG10  GOTO1 SEQ                                                              
*                                                                               
VPROG20  CLC   KEY(11),KEYSAVE     TEST KEYS WITHOUT END DATE                   
         BE    VPROG30                                                          
         CLI   FOUNDFLG,C'Y'       SEE IF PROGRAM WAS FOUND BEFORE              
         BE    PROGEXP             PROGRAM IS PAST END DATE                     
         B     PROGERR             PROGRAM DOES NOT EXIST                       
*                                                                               
VPROG30  MVI   FOUNDFLG,C'Y'       PROGRAM WAS FOUND                            
         OC    PERIOD,PERIOD       WAS PERIOD ENTERED                           
         BZ    VPROG40              NO                                          
*                                                                               
* NEED TO CHANGE THIS WHEN NPGKEND IS CONVERTED.                                
* NEED TO CHANGE THIS WHEN NPGKEND IS CONVERTED.                                
* NEED TO CHANGE THIS WHEN NPGKEND IS CONVERTED.                                
* >>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<                                      
         TM    OLDPER,X'80'        WEEKLY PERIOD                                
         BO    VPROG34              YES                                         
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,PRGEXPMS+36)                          
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,NPGKEND),(3,WORK+6)                                 
*                                                                               
         CLC   OLDPER,WORK+6       TEST PROGRAM IS SAME YEAR AND MONTH          
         BH    VPROG10                                                          
         B     VPROG40                                                          
*                                                                               
*PROG34  CLC   PERIOD,NPGKEND      PERIOD COVERED BY END DATE                   
VPROG34  CLC   OLDPER,NPGKEND      PERIOD COVERED BY END DATE                   
         BH    VPROG10              NO                                          
*                                                                               
VPROG40  MVC   PROGRAM,WORK        SAVE IT                                      
*                                                                               
         BRAS  RE,INITNET                                                       
VPROGX   XIT1                                                                   
*                                                                               
         DROP  R4                                                               
PROGEXP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRGEXPMS),PRGEXPMS                                     
         LA    R2,TRAPROGH                                                      
         B     PROGERX                                                          
*                                                                               
PROGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRGERRMS),PRGERRMS                                     
         LA    R2,TRAPROGH                                                      
PROGERX  GOTO1 ERREX2                                                           
*                                                                               
PRGERRMS DC    C'* ERROR * PROGRAM NOT FOUND *'                                 
PRGEXPMS DC    C'* ERROR * PROGRAM NOT CURRENT AS OF          *'                
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE PERIOD (FOR MONTH AND YEAR)                                          
*                                                                               
VPER     NMOD1 0,**+VPE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         TM    SVFLAG,ALLNETSW     ALL NETWORK REQUEST?                         
         BZ    *+12                                                             
         TM    SVFLAG,VALNETSW     READY TO VALIDATE FOR THIS NETWORK?          
         BZ    VPERX                NO                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    VPER10                                                           
         TM    SVOPTSW,OPTWEEK                                                  
         BO    *+12                                                             
         CLI   SVTNPR2,C'W'        INSTR PERIOD = WEEKLY                        
         BNE   DATENTER                                                         
*                                                                               
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLI   0(R1),1             MUST BE MONDAY                               
         BNE   DAYDATER                                                         
*                                                                               
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    VPER05                                                           
*                                                                               
         BAS   RE,CONVPER                                                       
         B     VPERX                                                            
*                                                                               
         FIXDT02                                                                
VPER05   GOTO1 DATCON,(R1),(0,WORK),(2,PERIOD)                                  
         MVC   OLDPER,PERIOD                                                    
         B     VPERX                                                            
*                                                                               
VPER10   GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         L     RE,0(R1)                                                         
         LTR   RE,RE                                                            
         BZ    DATENTER                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   PERIOD,WORK+6                                                    
         MVC   OLDPER,PERIOD                                                    
*                                                                               
VPERX    XIT1                                                                   
*                                                                               
*                                                                               
****************************************************************                
* ON ENTRY WORK CONTAINS PERIOD (YYMMDD)                                        
* CONVERT PERIOD TO:                                                            
* 1ST BYTE = YEAR                                                               
* 2ND BYTE = HOB MONTH AND LOB WEEK NUMBER FOR THAT MONTH                       
* EG. 1/27/20 CONVERT X'7814' YEAR= X'78' MONTH=1 WEEK=4                        
****************************************************************                
CONVPER  NTR1                                                                   
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,OLDPER)                                  
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(3,WORK+6) YYMMDD TO YMD                    
*                                                                               
         MVC   PERIOD,WORK+6       SAVE YM                                      
*                                                                               
* CALC WEEK NUMBER FOR THIS MONTH                                               
         SR    R5,R5               INIT WEEK NUMBER                             
         MVC   DUB(6),WORK                                                      
CONV02   AHI   R5,1                INCR WEEK NUMBER                             
         GOTO1 ADDAY,(R1),DUB,DUB,F'-7'                                         
         CLC   WORK+2(2),DUB+2     SAME MONTH?                                  
         BE    CONV02                                                           
*                                                                               
         SLL   R5,28               WEEK NUMBER IN HOB                           
         LLC   R4,PERIOD+1                                                      
         SLDL  R4,28               HOB MONTH/LOB WEEK NUMBER                    
         STCM  R4,8,PERIOD+1       MONTH/WEEK NO (X'C4'= DEC/WEEK 4)            
         B     VPERX                                                            
*                                                                               
*                                                                               
WKDATER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'WKDATMS),WKDATMS                                       
         B     VPERERX                                                          
*                                                                               
DATENTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DATENTMS),DATENTMS                                     
         B     VPERERX                                                          
*                                                                               
DAYDATER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DAYDATMS),DAYDATMS                                     
VPERERX  GOTO1 ERREX2                                                           
*                                                                               
WKDATMS  DC    C'* ERROR * ENTER MO/DA/YR AND DATE MUST BE MONDAY *'            
DAYDATMS DC    C'* ERROR * DATE MUST START WITH MON, NOT XXX *'                 
DATENTMS DC    C'* ERROR * ENTER MONTH/YEAR, NOT MONTH/DAY/YEAR *'              
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NMOD1 0,**+VNE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         TM    SVFLAG,ALLNETSW                                                  
         BO    VNET10                                                           
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
*                                                                               
VNET10   XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET             SAVE NETWORK MARKET NUMBER                   
*                                                                               
         MVC   SVMEDIA,STRTYPE     SAVE MEDIA (N,C,S,O,D,H)                     
*                                                                               
VNETX    XIT1                                                                   
*                                                                               
NETERR   DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         GOTO1 ERREX2                                                           
*                                                                               
NETERR2  DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERMS2),NETERMS2                                     
         GOTO1 ERREX2                                                           
*                                                                               
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
NETERMS2 DC    C'* ERROR * INVALID MEDIA ON NETWORK *'                          
         DROP  R4,RB,RC                                                         
         EJECT                                                                  
* VALIDATE REVISION NUMBER (0-255)                                              
*                                                                               
VREVNUM  NMOD1 0,**+VRE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         TM    4(R2),X'08'         TEST DATA NUMERIC                            
         BZ    RNUMERR             NO                                           
*                                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,VREVPACK                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'0'            TEST NUMBER BETWEEN 0 AND 255                
         BL    RNUMERR                                                          
         CH    R1,=H'255'                                                       
         BH    RNUMERR                                                          
         STC   R1,REVNUM                                                        
*                                                                               
VREVX    XIT1                                                                   
*                                                                               
VREVPACK PACK  DUB,TRARNUM(0)                                                   
NOREVERR DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOREVMS),NOREVMS                                       
         GOTO1 ERREX2                                                           
RNUMERR  DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RNUMMS),RNUMMS                                         
         LA    R2,TRARNUMH                                                      
         GOTO1 ERREX2                                                           
*                                                                               
RNUMMS   DC    C'* ERROR * NUMBER MUST BE NUMERIC (0-255) OR ''LAST'' *+        
               '                                                                
NOREVMS  DC    C'* ERROR * NO REVISION RECORD FOUND *'                          
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*          WEEKLY                                                               
*          MONTHLY                                                              
*                                                                               
VOPT     NMOD1 0,**+VOP**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPT90                                                           
*                                                                               
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,4                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
VOPT04   EX    R1,VOPTCLCH                                                      
         BE    VOPTHLP                                                          
*                                                                               
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,BLOCK)                                   
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRO            NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         MVC   HOLDSIGN,0(R5)                                                   
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VOPT12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VOPT20              NO, NETHER                                   
VOPT12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
VOPT20   EX    R1,VOPTCLCA         WEEKLY                                       
         BNE   VOPT30                                                           
         TM    SVOPTSW,OPTMONTH                                                 
         BNZ   BDKYWDER                                                         
         OI    SVOPTSW,OPTWEEK                                                  
         B     VOPT80                                                           
*                                                                               
VOPT30   EX    R1,VOPTCLCB         MONTHLY                                      
         BNE   VOPT34                                                           
         TM    SVOPTSW,OPTWEEK                                                  
         BNZ   BDKYWDER                                                         
         OI    SVOPTSW,OPTMONTH                                                 
         B     VOPT80                                                           
*                                                                               
VOPT34   EX    R1,VOPTCLCI         BOTH                                         
         BNE   VOPT40                                                           
         OI    SVOPTSW,OPTMONTH+OPTWEEK                                         
         B     VOPT80                                                           
*                                                                               
VOPT40   EX    R1,VOPTCLCC         PRD PRODUCT                                  
         BNE   VOPT50                                                           
*                                                                               
         CLI   SVTN2PR1,C'*'       TRAFFIC BY PRODUCT                           
         BNE   VOPTHLP                                                          
         CLC   =C'POL',22(R4)      IS PRD=POL                                   
         BE    INVPRER              YES, ERROR                                  
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    INVPRER              YES, ERROR                                  
         CLI   1(R4),3             MAX 3 CHAR                                   
         BH    PRLENERR                                                         
         BE    *+8                                                              
         MVI   24(R4),C' '                                                      
*                                                                               
         BRAS  RE,INITSPT          READ FROM SPOT FILE                          
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(3),22(R4)       PROD                                         
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERRORS                                   
         BNE   SECERR                                                           
*                                                                               
         MVC   OPTPROD(4),WORK     PROD/PRD (IF NOT OVERFLOW)                   
*                                                                               
         B     VOPT80                                                           
*                                                                               
VOPT50   EX    R1,VOPTCLCD         PGRP (PRODUCT GROUP)                         
         BNE   VOPT60                                                           
*                                                                               
         CLI   SVTN2PR1,C'A'       TRAFFIC BY PRODUCT GROUP                     
         BL    VOPT60                                                           
         CLI   SVTN2PR1,C'Z'       TRAFFIC BY PRODUCT GROUP                     
         BH    VOPT60                                                           
*                                                                               
*        VALIDATE PRODUCT GROUP *                                               
         BAS   RE,VPGR                                                          
         B     VOPT80                                                           
*                                                                               
VOPT60   EX    R1,VOPTCLCE         ACTIVITY                                     
         BNE   VOPT62                                                           
         OI    SVOPTSW,OPTACT                                                   
         B     VOPT80                                                           
*                                                                               
VOPT62   EX    R1,VOPTCLCF         FAX                                          
         BNE   VOPT64                                                           
         OI    SVOPTSW,OPTFAX                                                   
         B     VOPT80                                                           
*                                                                               
VOPT64   EX    R1,VOPTCLCG         CABLE                                        
*MNV     BNE   VOPT66                                                           
         BNE   VOPT65                                                           
         OI    SVOPTSW,OPTCAB                                                   
         B     VOPT80                                                           
*                                                                               
*MNV                                                                            
VOPT65   EX    R1,VOPTCLCV         DIGITAL                                      
         BNE   VOPT66                                                           
         OI    SVOPTSW,OPTDIGI                                                  
         B     VOPT80                                                           
*                                                                               
*MNV                                                                            
VOPT66   EX    R1,VOPTCLCS         SEED                                         
         BNE   VOPTHLP                                                          
*                                                                               
         OI    SVOPTSW,OPTSEED                                                  
*                                                                               
VOPT80   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VOPT90   DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VOPTX                                                            
*                                                                               
         OC    OPTPROD,OPTPROD     WAS PRODUCT OPTION ENTERED                   
         BZ    VOPTX                                                            
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
VOPT95   CLC   OPTPROD,0(R1)                                                    
         BE    VOPTX                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VOPT95                                                        
*                                                                               
         LA    R2,TRAOPTH                                                       
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
         B     TRAPERRO                                                         
*                                                                               
VOPTX    XIT1                                                                   
*                                                                               
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHLPMS-10),OPTHLPMS+10                               
         CLI   SVTN2PR1,0          ANY PROD OR PROD GROUP EXPECTED              
         BE    ERREXITN             NO                                          
         CLI   SVTN2PR1,C'0'       ANY PROD OR PROD GROUP EXPECTED              
         BE    ERREXITN             NO                                          
         MVC   CONHEAD+L'OPTHLPMS-10-2(7),=C'/PRD= *'                           
         CLI   SVTN2PR1,C'*'       EXPECTING PRODUCT                            
         BE    *+10                                                             
         MVC   CONHEAD+L'OPTHLPMS-10-2+2(3),=C'GRP'                             
ERREXITN GOTO1 ERREX2                                                           
*                                                                               
MISSERRO MVI   ERROR,MISSING                                                    
         B     TRAPERRO                                                         
INVPRER  MVI   ERROR,INVPROD                                                    
         B     TRAPERRO                                                         
*                                                                               
SECERR   LA    R2,TRAOPTH                                                       
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERRO                                                         
TRAPERRO GOTO1 ERREX                                                            
*                                                                               
BDKYWDER DS   0H                                                                
         LA    RF,L'BDKYWDMS-1                                                  
         L     R1,=A(BDKYWDMS)                                                  
         B     ERREXITO                                                         
PRLENERR DS   0H                                                                
         LA    RF,L'PRLENMS-1                                                   
         L     R1,=A(PRLENMS)                                                   
         B     ERREXITO                                                         
ERREXITO DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTR1DRR                                                      
         EX    RF,EREXMVCO                                                      
         GOTO1 ERREX2                                                           
EREXMVCO MVC   CONHEAD(0),0(R1)                                                 
*                                                                               
         EJECT                                                                  
VOPTCLCA CLC   12(0,R4),=CL7'WEEKLY'                                            
VOPTCLCB CLC   12(0,R4),=CL8'MONTHLY'                                           
VOPTCLCC CLC   12(0,R4),=CL4'PRD'                                               
VOPTCLCD CLC   12(0,R4),=CL5'PGRP'                                              
VOPTCLCE CLC   12(0,R4),=CL9'ACTIVITY '                                         
VOPTCLCF CLC   12(0,R4),=CL4'FAX '                                              
VOPTCLCG CLC   12(0,R4),=CL6'CABLE '                                            
VOPTCLCH CLC   8(0,R2),=CL5'HELP'                                               
VOPTCLCI CLC   12(0,R4),=CL5'BOTH'                                              
VOPTCLCS CLC   12(0,R4),=CL5'SEED '                                             
*MNV                                                                            
VOPTCLCV CLC   12(0,R4),=CL7'DIGITAL'                                           
*MNV                                                                            
OPTHLPMS DC   C'* ERROR * OPTNS=WEEKLY/MONTHLY/BOTH/ACT/CAB/FAX/SEED *'         
         EJECT                                                                  
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
VPGR     NTR1                                                                   
         GOTO1 VALIPGR                                                          
         MVC   OPTPRGR,SVPGRP      MOVE PRDGRP TO LOCAL STORAGE                 
*                                                                               
VPGR20   LA    R5,OPTPGRPL                                                      
         LA    R0,L'OPTPGRPL                                                    
*                                                                               
VPGR24   DS    0H                                                               
         MVC   SVKEY,KEY           SAVE                                         
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0B01000184030001'                                     
         MVC   ELEM+8(3),SVKEY+8                                                
         MVI   ERROPT,C'Y'                                                      
         MVI   ERROR,0                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   VPGR50               YES, BYPASS THIS PROD                       
*                                                                               
         MVC   0(3,R5),SVKEY+8                                                  
         LA    R5,3(,R5)                                                        
         BCT   R0,*+6                                                           
         DC    H'0'                MORE PRODUCTS THAN TABLE SIZE                
*                                                                               
VPGR50   MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BE    VPGR24                                                           
*                                                                               
         BRAS  RE,INITNET          READ FROM UNIT FILE                          
         B     VOPTX                                                            
*                                                                               
* ON ENTRY R1 POINTS TO 2 BYTE PWOS GROUP CODE                                  
*                                                                               
PGRUNPK  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(4),=C'PGR='                                                 
         MVC   WORK+4(1),SVTN2PR1                                               
         MVC   WORK+5(3),SPACES                                                 
         UNPK  DUB(5),0(3,R1)                                                   
         LLC   RE,PGRLEN                                                        
         LTR   RE,RE                                                            
         BP    *+8                                                              
         LA    RE,3                IF NO KNOWN LEN, SHOW 3 DIGITS               
         BCTR  RE,0                                                             
         EX    RE,PGRUNMVC                                                      
PGRUNPKX XIT1                                                                   
PGRUNMVC MVC   WORK+5(0),DUB                                                    
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* SET SPOT FILE PARAMETERS                                                      
*                                                                               
         USING GEND,RC                                                          
INITSPT  MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVI   LSTATUS+1,1                                                      
*                                                                               
         BR    RE                                                               
* 5                                                                             
* SET UNIT FILE PARAMETERS                                                      
*                                                                               
INITNET  MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,20                                                        
         MVI   DATADISP+1,27                                                    
         MVI   LSTATUS+1,1                                                      
*                                                                               
         BR    RE                                                               
* POINT TO EXTENDED SPOT FILE                                                   
*                                                                               
INITXSP  DS    0H                                                               
         MVI   SYSDIR,C'X'                                                      
         MVI   SYSDIR+1,C'S'                                                    
         MVI   SYSDIR+2,C'P'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,32          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,42                                                    
         MVI   LSTATUS+1,4                                                      
         BR    RE                                                               
         DROP  RC                                                               
         EJECT                                                                  
       ++INCLUDE SPTRNREV                                                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRABDD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR1DRR DS    F                                                                
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
NETWORK  DS    CL4                                                              
BNET     DS    XL2                                                              
SEQNUM   DS    XL1                                                              
SVMEDIA  DS    CL1                                                              
PROGRAM  DS    CL6                                                              
PERIOD   DS    XL2                                                              
REVNUM   DS    XL1                                                              
FOUNDFLG DS    CL1                                                              
LASTSW   DS    CL1                 * = THIS IS THE LAST REVISION                
CMTFOUND DS    CL1                                                              
SAVEKEY  DS    XL32                                                             
MYKEY    DS    XL32                                                             
SVREVNUM DS    CL1                                                              
NREVSW   DS    CL1                 NET REV SWITCH                               
*                                                                               
NEWPER   DS    XL3                                                              
OLDPER   DS    XL2                                                              
*                                                                               
SVFLAG   DS    CL1                                                              
ALLNETSW EQU   X'80'               ALL NETWORK REQUEST                          
VALNETSW EQU   X'40'               VALIDATE NETWORK                             
CONVSW   EQU   X'20'               CONVERTED RECORDS                            
*                                                                               
*                                                                               
OPTIONS  DS   0CL128                                                            
OPTPRGR  DS    XL2                                                              
OPTPGRPL DS    CL120                                                            
OPTPROD  DS    CL3                 OPTPROD AND OPTPRD MUST BE TOGETHER          
OPTPRD   DS    XL1                 AND IN ORDER                                 
SVOPTSW  DS    XL1                                                              
OPTWEEK  EQU   X'80'                                                            
OPTMONTH EQU   X'40'                                                            
OPTACT   EQU   X'20'                                                            
OPTSEED  EQU   X'10'                                                            
OPTCAB   EQU   X'08'                                                            
OPTFAX   EQU   X'04'                                                            
*MNV                                                                            
OPTDIGI  EQU   X'02'                                                            
*MNV                                                                            
HOLDSIGN DS    CL1                                                              
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PPROG    DS    CL6                                                              
         DS    CL2                                                              
PPER     DS    CL8                                                              
         DS    CL1                                                              
PPRD     DS    CL4                                                              
         DS    CL1                                                              
PREVNO   DS    CL3                                                              
PINSTR   DS    CL1                                                              
         DS    CL1                                                              
PCMT     DS    CL60                                                             
         DS    CL3                                                              
PRDATE   DS    CL8                                                              
         DS    CL2                                                              
PIDATE   DS    CL8                                                              
* 3                                                                             
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTPROG  DS    CL6                                                              
         DS    CL1                                                              
LSTPER   DS    CL10                                                             
         DS    CL3                                                              
LSTRNUM  DS    CL3                                                              
         DS    CL2                                                              
LSTRDATE DS    CL8                                                              
         DS    CL1                                                              
LSTIDATE DS    CL8                                                              
         DS    CL1                                                              
LSTCOMNT DS    CL31                                                             
         ORG   LSTCOMNT+29                                                      
LSTCOMTS DS    CL2                                                              
         ORG   LSTCOMNT+24                                                      
LSTNET   DS    CL5                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075SPTRA1D   11/09/20'                                      
         END                                                                    
