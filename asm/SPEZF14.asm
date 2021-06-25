*          DATA SET SPEZF14    AT LEVEL 101 AS OF 03/02/09                      
*PHASE T23014A                                                                  
*INCLUDE QSORT                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T23014 - EASI INVOICE LIST ACTIVE STATIONS FOR A PERIOD     *         
*  COMMENTS: THIS PROGRAM LISTS ACTIVE STATIONS                       *         
*                                                                     *         
*  OUTPUTS: NOW REPORT OR SCREEN LIST                                 *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, EZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*                  - AGENCY IDS IF USER=ALL                           *         
*             AIO2 - EZBLOCK                                          *         
*             AIO3 - CONTROLLER FIRST 1024 BYTES                      *         
*                  - EZWKRREC NEXT 1024 BYTES                         *         
*                  - EZAREC NEXT 2048 BYTES                           *         
*  STATAB IN GETMAIN STORAGE 500,000 BYTES LONG                       *         
*  MEDTAB IN DUMMY               900 BYTES                            *         
*  SRCTAB IN DUMMY AFTER MEDTAB 3952 BYTES                            *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  LEV 40    OCT19/94 FORCE ENTRY OF DATES                            *         
*  LEV 41    OCT24/94 BYPASS SJR/IRNY/WUNDERMAN                       *         
*  LEV 42-43 DEC01/94 BYPASS PRINTING AGENCIES IF NOT USR=ALL         *         
*  LEV 44    FEB02/96 ADD TO AGY TABLE SIZE                           *         
*  LEV 45    FEB13/96 MOVE SWITCH TO MEDIA                            *         
*  LEV 46    MAR11/96 CHANGE AGYTAB FROM 350 TO 400                   *         
*  LEV 47    MAR12/96 CHANGE AGYTAB FROM 400 TO 500                   *         
*  LEV 48    APR02/96 MOVE SRCETAB, MEDTAB, MAKE STATAB LARGER        *         
*                     DELETE AGYTAB                                   *         
*  LEV 49    APR09/96 MAKE STATAB LARGER, STAENT SMALLER              *         
*  LEV 50    MAY26/96 ADD NWK FILES                                   *         
*  LEV 51-52 AUG15/96 CHG C TO NORMAL, MOVE MEDTAB, ADD TRACE         *         
*                     FIX MAXUIDS CT                                  *         
*  LEV 53    SEP09/96 FIX RQSTA FROM BLK TO PERIOD 3RD POS            *         
*  LEV 54    OCT10/96 FIX FTRSRCE                                     *         
*  LEV 55    OCT02/97 BYPASS VARIOUS SOURCES                          *         
*  LEV 56    JAN06/98 CHANGE USA EXCLUSION                            *         
*  LEV 57    APR14/98 CHANGE SDI TO COMPARE ON SDIJ & SDIZ            *         
*  LEV 58    SEP15/98 SPEED UP I/O'S USING INDEX FOR DATES            *         
*  LEV 59    DEC07/98 USE GETMAIN FOR STATION COUNTS MEMORY           *         
*  LEV 60    DEC29/98 FORCE SOURCE TO BLANKS                          *         
*  LEV 61    JAN19/99 ALLOW FOR INCREASE IN EQVSTATB                  *         
*  LEV 62    APR08/99 ADD OPTION DOWNLOAD AND BYPASS                  *         
*                     SOURCES RAPP/SHAI/STRA/WIM                      *         
*                     ADD COUNT OF BYPASSED BATCHES                   *         
*  LEV 63    NOV04/99 ADD FREEMAIN                                    *         
*  LEV 64    NOV11/99 ADD KATZ TO BYPASSED SOURCE                     *         
*  LEV 65    JAN31/00 FIX EOJ TOTALS PRINTING, SHOW MEDIAS            *         
*  LEV 66    MAR09/00 CHANGE DOWNLOAD                                 *         
*  LEV 67    JUN08/00 BYPASS SJR & NFNY CHG STAUIDS FRON 40 TO 60     *         
*  LEV 68    JAN03/01 BYPASS SOURCE GNC                               *         
*  LEV 69    FEB02/01 SHOW LOCAL STATION AS L, SEND # SPTS/NET $ DOWN *         
*  LEV 70    APR13/01 DEL OVERFLOW CHECK                              *         
*  LEV 71    MAY14/01 FIX DOWNLOAD FOR OVER $99M                      *         
*  LEV 72    MAY29/01 FIX DUMMY                                       *         
*  LEV 73    JUL11/01 STOP JUMPING OVER MEMORY                        *         
*  LEV 74    OCT05/01 ADD MORE SID'S                                  *         
*  LEV 76    JUN03/02 BYPASS SOURCES COMV/CORE/COZE                   *         
*  LEV 77    JUL24/02 BYPASS SOURCES CMR/CFM/SDIC/SDIL                *         
*                     MOVE SWITCH (VALIFAS) BEFORE VALIMED            *         
*  LEV 79 BG MAR12/03 BYPASS SOURCES RMR/IPS -FIX TABLE SIZES         *         
*  LEV 80 BG JUL15/03 FIX FREEMAIN, HARD CODE FOR 15 NWRKR FILES      *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23014 - ACTIVE STATIONS PER AGENCY LIST'                       
T23014   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T23014**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         SPACE                                                                  
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    INVAL                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    INVAL                                                            
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
         SPACE                                                                  
VKEY     CLI   ACTNUM,14                                                        
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
         SPACE                                                                  
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
         SPACE                                                                  
         GOTO1 VALIMED             VALIMED SETS TO SPOT OR NET                  
         SPACE                                                                  
         LA    R2,LINSTAH          STATION                                      
         XC    RQSTA,RQSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         SPACE                                                                  
         GOTO1 VALISTA                                                          
         SPACE                                                                  
         MVC   RQSTA,QSTA                                                       
         SPACE                                                                  
         CLI   RQSTA+3,C' '                                                     
         BH    VK100                                                            
         MVI   RQSTA+3,C'.'                                                     
         SPACE                                                                  
VK100    OI    4(R2),X'20'                                                      
         SPACE                                                                  
         LA    R2,LINBDTH          BATCH DATE                                   
         SPACE                                                                  
         XC    RQDTES,RQDTES                                                    
         CLI   5(R2),0             IF NO DATE                                   
         BE    MISSERR                                                          
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         SPACE                                                                  
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),(0,WORK),(2,RQDTSTR)                                 
         SPACE                                                                  
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK160                YES                                         
         LA    R3,1+8(R2,R3)                                                    
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,RQDTEND)                                 
         SPACE                                                                  
         CLC   RQDTSTR,RQDTEND     DATES MUST BE IN SEQ                         
         BH    BADATE                                                           
         SPACE                                                                  
         B     VK200                                                            
         SPACE                                                                  
VK160    MVC   RQDTEND,RQDTSTR                                                  
         SPACE                                                                  
VK200    OI    4(R2),X'20'                                                      
         LA    R2,LINBSQH          SEQ                                          
         XC    RQSEQ,RQSEQ                                                      
         XC    RQBSEQ,RQBSEQ                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    VK300                                                            
         SPACE                                                                  
VK210    MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   NUMERR                                                           
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         UNPK  FULL,DUB                                                         
         SPACE                                                                  
         MVC   RQSEQ,FULL                                                       
         STCM  R0,3,RQBSEQ                                                      
         B     VK300                                                            
         SPACE                                                                  
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
         SPACE                                                                  
*                                  FILTERS LINE                                 
VK300    OI    4(R2),X'20'                                                      
         LA    R2,LINFTRH          FILTERS                                      
         GOTO1 =A(VFTR),RR=RELO                                                 
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
VKXIT    XC    KEY,KEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   LIST - LIST RECORDS                                               *         
***********************************************************************         
         SPACE                                                                  
LIST     DS    0H                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                WHERE IS T23010                              
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
         SPACE                                                                  
         L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         LR    RE,R6               CLEAR EZBLOCK                                
         LHI   RF,EZBLOCKL                                                      
         XCEFL                                                                  
*                                                                               
         LA    RE,EZCOUNT          COUNT IT ALL ROUTINE                         
         ST    RE,EZHOOK                                                        
         MVC   EZWKRFIL,EASIWK                                                  
         MVC   EZPRINT,VPRINT                                                   
         LH    RE,=AL2(WRKFBUFR-SYSD)                                           
         AR    RE,R9                                                            
         ST    RE,EZWKRBUF                                                      
         L     RE,AIO3                                                          
         LA    RE,1024(,RE)                                                     
         ST    RE,EZWKRREC                                                      
         LA    RE,1024(,RE)                                                     
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZLOOKSW,X'E0'                                                   
         MVI   EZTRACE,0           TRACE FIELDS                                 
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         DROP  R6                                                               
         SPACE                                                                  
         MVI   USEIO,C'Y'          SET USER WILL DO ALL I/O                     
         LM    R0,R1,=A(HEADING,HDHK)                                           
         A     R0,RELO                                                          
         ST    R0,SPECS                                                         
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         CLI   FTRDOWN,C'Y'        THIS A DOWNLOAD REPORT                       
         BNE   LS040                NO                                          
         SPACE                                                                  
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*        MVI   LINE,1                                                           
         MVC   P(L'DOWNHEAD),DOWNHEAD                                           
         MVI   P+1+L'DOWNHEAD,X'5E'                                             
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LS040                                                            
         SPACE                                                                  
DOWNHEAD DC    C'"CALL LETTER" "MEDIA" "SOURCE" "INVOICES" "SPOTS" "NETX        
                DOLLARS" "GROSS DOLLARS"'                                       
*                                                                               
LS040    DS    0H                                                               
         XC    AAGYTAB,AAGYTAB     NO AGENCY NAME TABLE                         
*                                                                               
* DETERMINE AMOUNT OF STORAGE NEEDED                                            
         LA    R1,TABTAB           A(MAIN TABLE)                                
         LHI   RF,TABTABNQ         NUMBER OF TABLES                             
         SR    R0,R0               SIZE ACCUMULATOR                             
*                                                                               
LS050    ICM   RE,15,8(R1)         LENGTH OF INDIVIDUAL TABLE                   
         AHI   RE,EYECATLQ         PLUS LENGTH OF EYCATCHER                     
         AR    R0,RE               ADD IT TO ACCUMULATOR                        
         LA    R1,TABTABLQ(R1)     NEXT ENTRY                                   
         BCT   RF,LS050                                                         
*                                                                               
* R0 HAS SUM OF LENGTHS OF ALL TABLES AT THIS POINT                             
         ST    R0,SVMEMSIZ                                                      
*                                                                               
         LA    RF,TABTAB           A(VERY FIRST TABLE)                          
         GETMAIN  EC,LV=(0),A=(15)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,TABTAB           A(GETMAIN STORAGE AREA)                      
         LA    R2,TABTAB           MAIN TABLE                                   
         LHI   R3,TABTABNQ         NUMBER OF ENTRIES IN MAIN TABLE              
*                                                                               
LS060    MVC   0(EYECATLQ,R1),12(R2) PUT THE EYECATCHER IN GETMAIN AREA         
         LA    R1,EYECATLQ(,R1)    MOVE PAST EYCATCHER                          
         ST    R1,0(R2)            STORE IT IN ADDRESS TABLE                    
*                                                                               
         ICM   R0,15,8(R2)         LENGTH OF TABLE                              
         AR    R1,R0               END OF TABLE IN GETMAIN AREA                 
         ST    R1,4(R2)            STORE END OF TABLE IN ADDRESS TABLE          
*                                                                               
         L     RE,0(R2)            ADDRESS OF THE TABLE (FROM ADDR TAB)         
         L     RF,8(R2)            LENGTH OF THE TABLE                          
*                                                                               
         XCEF                                                                   
*                                                                               
         LA    R2,TABTABLQ(R2)                                                  
         BCT   R3,LS060                                                         
*                                                                               
         MVI   NLISTS,NUMLINS                                                   
         LA    R4,SVWEZIND                                                      
         USING EZWKRIXD,R4                                                      
         XC    SVWEZIND,SVWEZIND                                                
         XC    BATSRD,BATSRD                                                    
         XC    MAXUID,MAXUID                                                    
         XC    JTOT(JTOTLQ),JTOT                                                
         ZAP   JNDOLS,=P'0'                                                     
         ZAP   JGDOLS,=P'0'                                                     
         MVC   SVSTA,SPACES                                                     
         MVC   SVSRCE,SPACES                                                    
         MVC   SVSTAMED,SPACES                                                  
         MVC   UKCIADDR-UKRECD(,R4),SVCIADDR                                    
         B     LS101                                                            
         EJECT                                                                  
*                                                                               
LS100    DS    0H                                                               
         LA    R4,SVWEZIND                                                      
         OI    UKFLAG-UKRECD(R4),UKFLDAT                                        
         MVC   UKCIADDR-UKRECD(,R4),SVCIADDR                                    
         SPACE                                                                  
         LH    R3,=AL2(WRKFBUFR-SYSD)                                           
         AR    R3,R9                                                            
         L     R5,AIO3                                                          
         LA    R5,1024(,R5)                                                     
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'INDEX',EASIWK,SVWEZIND,(R5),(R3)                 
         SPACE                                                                  
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    *+6                  SHOULD NOT BE                               
         DC    H'0'                                                             
*                                                                               
* LOOP THRU ALL BATCHES - BUILDING TABLE OF STATIONS & ACTIVE AGENCIES*         
*                                                                               
LS101    DS    0H                                                               
         LH    R3,=AL2(WRKFBUFR-SYSD)                                           
         AR    R3,R9                                                            
         L     R5,AIO3                                                          
         LA    R5,1024(,R5)                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EASIWK,SVWEZIND,(R5),(R3)                 
*                                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    LS102                NO                                          
*                                                                               
         CLI   FUIDNUM,X'FF'       ALL USERS                                    
         BNE   LS200                NO                                          
*                                                                               
         CLI   EASIWK+4,C'F'       DONE ALL FILES                               
         BE    LS200                YES                                         
*                                                                               
         CLI   EASIWK+4,C'9'                                                    
         BNE   *+12                                                             
*                                                                               
         MVI   EASIWK+4,C'A'                                                    
         B     LS101C                                                           
*                                                                               
         ZIC   R1,EASIWK+4                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,EASIWK+4                                                      
*                                                                               
LS101C   DS    0H                                                               
         L     RF,AIO2                                                          
         MVC   EZWKRFIL-EZBLOCKD(,RF),EASIWK                                    
         XC    SVWEZIND,SVWEZIND                                                
         B     LS101                                                            
*                                                                               
LS102    L     R1,BATSRD                                                        
         AHI   R1,1                                                             
         ST    R1,BATSRD                                                        
*                                                                               
         TM    FTRFLAG,FTRTEST     TEST - ONLY READ 500 BATCHES                 
         BZ    LS103                NO                                          
         CH    R1,=H'499'                                                       
         BH    LS200                STOP AT 500                                 
*                                                                               
LS103    DS    0H                                                               
*                                                                               
         USING EZWKRIXD,R4                                                      
LS104    DS    0H                                                               
         LA    R4,SVWEZIND                                                      
         CLI   EZWIDAY,X'99'       MUST BE DAY 99                               
*        BNE   LS100                                                            
         BNE   LS101                                                            
*                                                                               
         OC    RQDTES,RQDTES       DATE FILTER                                  
         BZ    LS105                                                            
         CLC   UKAGELD-UKRECD+SVWEZIND,RQDTSTR                                  
         BL    LS100               LOW, SKIP                                    
         BE    LS105               EQUAL, OK                                    
*                                                                               
         CLC   UKAGELD-UKRECD+SVWEZIND,RQDTEND                                  
         BH    LS100               HIGH, SKIP                                   
*                                                                               
LS105    DS    0H                                                               
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BE    LS108                                                            
         OC    FUIDNUM,FUIDNUM     DIFFERENT USER ID                            
         BZ    LS106                                                            
         CLC   EZWIUID,FUIDNUM     ELSE, TEST RIGHT ID                          
         BNE   LS100                                                            
         B     LS108                                                            
*                                                                               
LS106    CLC   EZWIUID,TWAORIG     ELSE, TEST RIGHT ID                          
         BNE   LS100                                                            
*                                                                               
LS108    DS    0H                                                               
* IF RUNNING FOR ONE ID IGNORE COUNTRY FILTER                                   
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BNE   LS109                                                            
*                                                                               
         CLC   SVLASTID,EZWIUID                                                 
         BE    LS108A                                                           
         LA    R1,EZWIUID                                                       
         BRAS  RE,CHKCTRY                                                       
         MVC   SVLASTID,EZWIUID                                                 
*                                                                               
LS108A   DS    0H                                                               
         CLI   SVCTRY,C'C'         CANADIEN AGENCY?                             
         BNE   *+16                NO - PROCEED TO THE US LOGIC                 
         TM    FTRFLAG2,FTR2CAN    ARE WE DOING CANADIAN AGENCIES?              
         BZ    LS100               NO - SKIP THIS BATCH                         
         B     *+12                YES - SKIP THE US LOGIC                      
*                                                                               
* US AGENCY HERE                                                                
         TM    FTRFLAG2,FTR2CAN    ARE WE DOING CANADIAN AGENCIES?              
         BO    LS100               YES - SKIP THE US BATCH                      
*                                                                               
LS109    DS    0H                                                               
         MVC   SVUID,EZWIUID                                                    
*                                                                               
         CLI   FTRALL,C'Y'         ALLOW ALL                                    
         BE    LS110                                                            
*                                                                               
         CLC   SVUID,=H'0011'      BYPASS SJR                                   
         BE    LS100                                                            
         CLC   SVUID,=H'2894'      BYPASS NFNY                                  
         BE    LS100                                                            
*                                                                               
LS110    DS    0H                                                               
         CLI   FTRDDS,0            FILTERING ON DDS/NON-DDS AGENCIES            
         BE    LS114                NO                                          
         SPACE                                                                  
* FILTER ON DDS/NON-DDS AGENCIES - ALL EIX IDS ARE IN BLOCK *                   
         SPACE                                                                  
         LA    R0,240                                                           
         LA    R1,BLOCK                                                         
         SPACE                                                                  
LS111    OC    0(2,R1),0(R1)       END OF TABLE                                 
         BZ    LS112                                                            
         CLC   SVUID,0(R1)                                                      
         BE    LS113                                                            
         LA    R1,2(,R1)                                                        
         BCT   R0,LS111                                                         
LS112    CLI   FTRDDS,C'Y'         ONLY DDS AGENCIES                            
         BE    LS114                                                            
         B     LS100                                                            
LS113    CLI   FTRDDS,C'N'         ONLY NON-DDS AGENCIES                        
         BNE   LS100                                                            
         SPACE                                                                  
LS114    MVC   SRCESTA(4),EZWISTN  STATION                                      
         MVC   SRCESTA+4(1),EZWIMED                                             
         MVC   ORIGSTA,SRCESTA                                                  
         SPACE                                                                  
         CLI   SRCESTA+3,C' '                                                   
         BH    *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         SPACE                                                                  
         MVC   EQUISTA,SRCESTA                                                  
         SPACE                                                                  
         OC    RQSTA,RQSTA         STATION FILTER                               
         BZ    LS116                                                            
         CLC   EQUISTA(5),RQSTA                                                 
         BNE   LS100                                                            
         SPACE                                                                  
LS116    OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    *+14                                                             
         CLC   SVWEZIND+10(2),RQBSEQ                                            
         BNE   LS100                                                            
         SPACE                                                                  
         MVC   SVSNMED,EQUISTA+4                                                
         SPACE                                                                  
         CLI   EQUISTA,C'0'        THIS LOCAL CABLE                             
         BL    LS117                                                            
         CLI   EQUISTA+4,C'L'                                                   
         BE    LS126                                                            
         CLI   EQUISTA+4,C'T'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SVSNMED,C'L'                                                     
         B     LS126                                                            
         SPACE                                                                  
LS117    DS   0H                                                                
         CLI   SVSNMED,C'T'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'N'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'X'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'S'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'C'                                                     
         BE    LS126                                                            
         SPACE                                                                  
         CLI   EQUISTA+4,C'A'                                                   
         BE    LS118                                                            
         CLI   EQUISTA+4,C'F'                                                   
         BNE   LS120                                                            
LS118    MVI   SVSNMED,C'R'                                                     
         B     LS126                                                            
         SPACE                                                                  
LS120    MVI   SVSNMED,C'?'        MARK UNKNOWN MEDIA                           
         MVI   EQUISTA+4,C'?'                                                   
         SPACE                                                                  
LS126    CLI   FTRMEDIA,0                                                       
         BE    LS128                                                            
         CLC   FTRMEDIA,SVSNMED                                                 
         BNE   LS100                                                            
*                                                                               
LS128    DS    0H                                                               
         CLI   FTRTRCE,C'Y'        PRINT TRACE OF INDEX                         
         BNE   LS130                                                            
*                                                                               
* PRINT HEXADECIMAL TRACE OF INDEXES *                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,SVWEZIND,P+3,16                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS130    L     R1,RECRDCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,RECRDCT                                                       
         SPACE                                                                  
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   SET ON RETURN                                
         GOTO1 CATCHIOS            SEE IF 90% OF MAX                            
         CLI   ERROR,0             IF ERROR, OVER MAX                           
         BNE   MAXIOSER                                                         
         SPACE                                                                  
         MVC   SVCIADDR,UKCIADDR-UKRECD(R4)                                     
         SPACE                                                                  
         LH    R3,=AL2(WRKFBUFR-SYSD)                                           
         AR    R3,R9                                                            
         L     R5,AIO3                                                          
         LA    R5,1024(,R5)                                                     
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'READ',EASIWK,SVWEZIND,(R5),(R3)                  
         SPACE                                                                  
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BNZ   LS100                YES, SKIP                                   
         SPACE                                                                  
         USING W_RECD,R3                                                        
         MVC   SVWCMNT,W_DESC                                                   
         SPACE                                                                  
         OC    RQDTES,RQDTES       DATE FILTER                                  
         BZ    LS140                                                            
         CLC   W_AGELD,RQDTSTR                                                  
         BL    LS100               LOW, SKIP                                    
         BE    LS140               EQUAL, OK                                    
         SPACE                                                                  
         CLC   W_AGELD,RQDTEND                                                  
         BH    LS100               HIGH, SKIP                                   
         SPACE                                                                  
LS140    OC    FTRBSDT,FTRBSDT     FILTERING ON BATCH DATE                      
         BZ    LS142                NO                                          
         CLC   W_AGELD,FTRBSDT                                                  
         BL    LS100               LOW, SKIP                                    
         CLC   W_AGELD,FTRBEDT                                                  
         BH    LS100               HIGH, SKIP                                   
         SPACE                                                                  
*                                  DISPLAY FOUND DATE AND BATCH SEQ             
         SPACE                                                                  
LS142    MVC   SVWKDTEC,W_AGELD       SAVE DATE AND BATCH SEQ                   
         MVC   SVWKFILN,W_FILENO                                                
         MVC   SVWKSTAT,W_STAT                                                  
         SPACE                                                                  
         LA    R2,W_DESC                                                        
         USING EZWKRCMD,R2                                                      
         SPACE                                                                  
         CLI   FTRALL,C'Y'         ALLOW ALL                                    
         BE    LS143                                                            
         CLC   EZWCSRCE,=C'IRNY'                                                
         BE    LS100                                                            
         CLC   EZWCSRCE,=C'DDS '     BYPASS ANY SOURCE = DDS                    
         BE    LS100                                                            
         SPACE                                                                  
LS143    OC    FTRSRCE,FTRSRCE     FILTERING ON SOURCE                          
         BZ    LS144                                                            
         CLC   FTRSRCE,EZWCSRCE                                                 
         BNE   LS100                                                            
         SPACE                                                                  
LS144    L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         SPACE                                                                  
         OC    EZWCSRCE,SPACES                                                  
         MVC   EZSRCE,EZWCSRCE                                                  
         MVC   SVSRCE,EZWCSRCE                                                  
         DROP  R2,R3                                                            
*                                                                               
         XC    TINVCTS(TINVLQ),TINVCTS                                          
         ZAP   TINVNDOL,=P'0'                                                   
         ZAP   TINVGDOL,=P'0'                                                   
*                                                                               
         MVC   EZWKRIND,SVWEZIND                                                
         GOTO1 VEZMOD,DMCB,(R6)                                                 
         CLI   FTRALL,C'Y'         ALLOW ALL                                    
         BE    LS146                                                            
*                                                                               
         CLC   EZAGY,=C'SJ'        BYPASS SJR                                   
         BE    LS100                                                            
         CLC   EZAGY,=C'WW'        BYPASS WUNDERMAN                             
         BE    LS100                                                            
*                                                                               
*                                                                               
         LA    R0,SRCECT                                                        
         LA    R1,SRCETAB                                                       
         CLC   EZSRCE,0(R1)                                                     
         BE    LS145                                                            
         LA    R1,8(,R1)                                                        
         BCT   R0,*-14                                                          
         B     LS146                                                            
*                                                                               
LS145    DS   0H                                                                
         AP    4(4,R1),=P'1'                                                    
         B     LS100                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
* RETURN HERE AT END OF BATCH, THEN ADD TO TABLE IF TINVCTS NON-ZERO *          
*                                                                               
LS146    OC    TINVCTS(TINVLQ),TINVCTS                                          
         BZ    LS180                                                            
*                                                                               
         L     R3,ASTATAB                                                       
         USING STAENTD,R3                                                       
LS150    OC    STAENT,STAENT       EMPTY ENTRY                                  
         BZ    LS152                                                            
         SPACE                                                                  
         CLC   STAMED,SVSNMED                                                   
         BNE   LS151                                                            
         CLI   FTRSORT,C'R'                                                     
         BE    LS150A                                                           
         CLC   STASRCE,SVSRCE                                                   
         BNE   LS151                                                            
         CLC   STASTA,SVSTA                                                     
         BE    LS153                                                            
         B     LS151                                                            
LS150A   CLC   STARSRCE,SVSRCE                                                  
         BNE   LS151                                                            
         CLC   STARSTA,SVSTA                                                    
         BE    LS153                                                            
LS151    LA    R3,STANEXT                                                       
         C     R3,ASTATABE         AT END OF TABLE                              
         BNL   TABSIZER                                                         
         B     LS150                                                            
         SPACE                                                                  
LS152    MVC   STAMED,SVSNMED                                                   
         MVC   STASTA,SVSTA                                                     
         MVC   STASRCE,SVSRCE                                                   
         SPACE                                                                  
         CLI   FTRSORT,C'R'                                                     
         BNE   *+16                                                             
         MVC   STARSTA,SVSTA                                                    
         MVC   STARSRCE,SVSRCE                                                  
         SPACE                                                                  
         ZAP   STANDOLS,=P'0'                                                   
         ZAP   STAGDOLS,=P'0'                                                   
         SPACE                                                                  
LS153    ICM   RE,15,STABAT           ADD TO BATCHES                            
         AHI   RE,1                                                             
         STCM  RE,15,STABAT                                                     
*                                                                               
*        OC    STADDR,SPACES                                                    
*        CLC   STADDR,SPACES                                                    
*        BNE   *+10                                                             
*        MVC   STADDR,SVSTADDR                                                  
*                                                                               
         LA    R0,6                INV/CONV/OVER/UNCONV/DEL/SPTS                
         LA    RE,TINVCTS                                                       
         LA    RF,STAINV                                                        
*                                                                               
LS154    ICM   R1,15,0(RF)                                                      
         A     R1,0(,RE)                                                        
         STCM  R1,15,0(RF)                                                      
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,LS154                                                         
*                                                                               
         AP    STANDOLS,TINVNDOL                                                
         AP    STAGDOLS,TINVGDOL                                                
*                                                                               
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BNE   LS154G                                                           
         SPACE                                                                  
         LA    R0,(L'STAUIDS)/2                                                 
         LA    R1,STAUIDS                                                       
         LA    RF,1                                                             
         SPACE                                                                  
LS154C   OC    0(2,R1),0(R1)       SLOT EMPTY                                   
         BZ    LS154E                                                           
         CLC   SVUID,0(R1)         ALREADY SAVED                                
         BE    LS154G                                                           
         LA    R1,2(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,LS154C                                                        
         B     *+10                                                             
         SPACE                                                                  
LS154E   MVC   0(2,R1),SVUID                                                    
         SPACE                                                                  
         CH    RF,MAXUID                                                        
         BNH   LS154G                                                           
         STH   RF,MAXUID                                                        
         SPACE                                                                  
         DROP  R3                                                               
         SPACE                                                                  
LS154G   ICM   R3,15,ASRCTAB                                                    
         BZ    LS156                                                            
         USING SRCENTD,R3                                                       
LS155    OC    SRCENT,SRCENT       EMPTY ENTRY                                  
         BZ    LS155C                                                           
         CLC   SRCSRC,SVSRCE                                                    
         BE    LS155E                                                           
         LA    R3,SRCNEXT                                                       
         C     R3,ASRCTABE                                                      
         BL    LS155                                                            
         SPACE                                                                  
         DC    H'0'                                                             
         SPACE                                                                  
LS155C   MVC   SRCSRC,SVSRCE                                                    
         ZAP   SRCNDOLS,=P'0'                                                   
         ZAP   SRCGDOLS,=P'0'                                                   
         SPACE                                                                  
LS155E   ICM   RE,15,SRCBAT           ADD TO BATCHES                            
         AHI   RE,1                                                             
         STCM  RE,15,SRCBAT                                                     
*                                                                               
         LA    R0,6                INV/CONV/OVER/UNCONV/DEL/SPTS                
         LA    RE,TINVCTS                                                       
         LA    RF,SRCINV                                                        
*                                                                               
LS155F   ICM   R1,15,0(RF)                                                      
         A     R1,0(,RE)                                                        
         STCM  R1,15,0(RF)                                                      
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,LS155F                                                        
*                                                                               
         AP    SRCNDOLS,TINVNDOL                                                
         AP    SRCGDOLS,TINVGDOL                                                
*                                                                               
         DROP  R3                                                               
         SPACE                                                                  
LS156    ICM   R3,15,AMEDTAB                                                    
         BZ    LS158                                                            
         USING MEDENTD,R3                                                       
LS157    OC    MEDENT,MEDENT       EMPTY ENTRY                                  
         BZ    LS157C                                                           
         CLC   MEDMED,SVSNMED                                                   
         BE    LS157E                                                           
         LA    R3,MEDNEXT                                                       
         C     R3,AMEDTABE                                                      
         BL    LS157                                                            
         SPACE                                                                  
         DC    H'0'                                                             
         SPACE                                                                  
LS157C   MVC   MEDMED,SVSNMED                                                   
         ZAP   MEDNDOLS,=P'0'                                                   
         ZAP   MEDGDOLS,=P'0'                                                   
LS157E   SR    RE,RE                                                            
         ICM   RE,15,MEDBAT         ADD TO BATCHES                              
         AHI   RE,1                                                             
         STCM  RE,15,MEDBAT                                                     
*                                                                               
         LA    R0,6                INV/CONV/OVER/UNCONV/DEL/SPTS                
         LA    RE,TINVCTS                                                       
         LA    RF,MEDINV                                                        
*                                                                               
LS157F   ICM   R1,15,0(RF)                                                      
         A     R1,0(,RE)                                                        
         STCM  R1,15,0(RF)                                                      
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,LS157F                                                        
*                                                                               
         AP    MEDNDOLS,TINVNDOL                                                
         AP    MEDGDOLS,TINVGDOL                                                
*                                                                               
         DROP  R3                                                               
*                                                                               
* ADD TO JOB TOTALS                                                             
LS158    L     RE,JBAT             ADD TO BATCH CT                              
         AHI   RE,1                                                             
         ST    RE,JBAT                                                          
*                                                                               
         LA    R0,6                INV/CONV/OVER/UNCONV/DEL/SPTS                
         LA    RE,TINVCTS                                                       
         LA    RF,JINV                                                          
*                                                                               
LS160    L     R1,0(,RE)                                                        
         A     R1,0(,RF)                                                        
         ST    R1,0(,RF)                                                        
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,LS160                                                         
*                                                                               
         AP    JNDOLS,TINVNDOL                                                  
         AP    JGDOLS,TINVGDOL                                                  
*                                                                               
LS180    OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BNZ   LS200                YES, ALL DONE                               
*                                                                               
         B     LS100                                                            
         SPACE                                                                  
* READ ENTIRE FILE, NOW SORT AND PRINT TABLE *                                  
         SPACE                                                                  
LS200    DS   0H                                                                
         CLI   MODE,PRINTREP       UNLESS PRINTING REPORT                       
         BE    *+6                  ALL DONE                                    
         DC    H'0'                                                             
* CLEAR AIO1 FOR SAVING AGENCY IDS                                              
         SPACE                                                                  
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         SPACE                                                                  
* PRINT JOB TOTALS *                                                            
         SPACE                                                                  
         L     R3,ASTATAB                                                       
         SR    R5,R5                                                            
         USING STAENTD,R3                                                       
LS210    OC    STAENT,STAENT                                                    
         BZ    LS214                                                            
         LA    R5,1(,R5)            COUNT OF ENTRIES                            
         SPACE                                                                  
         LA    R3,STANEXT                                                       
         C     R3,ASTATABE                                                      
         BL    LS210                                                            
         SPACE                                                                  
LS214    ST    R5,CTSTAS           SAVE COUNT OF STATIONS                       
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,ASTATAB,(R5),STAENTL,L'STAENT,0                        
         SPACE                                                                  
         L     R3,ASTATAB                                                       
*                                                                               
         XC    ATOT(ATOTLQ),ATOT                                                
         ZAP   ANDOLS,=P'0'                                                     
         ZAP   AGDOLS,=P'0'                                                     
         XC    AMED,AMED                                                        
         SPACE                                                                  
         USING STAENTD,R3                                                       
         MVC   SVSRCE,STARSRCE                                                  
         MVC   SVSTAMED,STAMED                                                  
         SPACE                                                                  
LS220    OC    STAENT,STAENT       AT END OF ENTRIES                            
         BZ    LS260                YEP                                         
         SPACE                                                                  
         MVC   WORK,STAENT                                                      
         SPACE                                                                  
         CLI   FTRDOWN,C'Y'        THIS A DOWNLOAD REPORT                       
         BE    LS236                YES                                         
         SPACE                                                                  
         L     R1,AMED                                                          
         LA    R1,1(,R1)                                                        
         ST    R1,AMED                                                          
         SPACE                                                                  
         MVC   PSTATN(4),STASTA                                                 
         MVI   PSTATN+4,C'-'                                                    
         MVC   PSTATN+5(1),STASTA+4                                             
*                                                                               
         CLI   STAMED,C'R'                                                      
         BE    *+10                                                             
         MVC   PSTATN+5(1),STAMED                                               
*                                                                               
         MVC   PMED(1),STAMED                                                   
*                                                                               
         MVC   PSRCE,STASRCE                                                    
*                                                                               
         CLI   FTRSORT,C'R'        SORT BY SOURCE                               
         BNE   LS224                                                            
         MVC   PSTATN(4),STARSTA                                                
         MVI   PSTATN+4,C'-'                                                    
         MVC   PSTATN+5(1),STARSTA+4                                            
         SPACE                                                                  
         MVC   PSRCE,STARSRCE                                                   
         SPACE                                                                  
LS224    LA    R0,6                BAT/INV/CONV/OVER/UNCONV/DEL/SPTS            
         LA    RE,STAINV                                                        
         LA    RF,AINV                                                          
*                                                                               
LS230    ICM   R1,15,0(RE)                                                      
         A     R1,0(,RF)                                                        
         ST    R1,0(,RF)                                                        
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,LS230                                                         
*                                                                               
         ICM   R1,15,STABAT                                                     
         A     R1,ABAT                                                          
         ST    R1,ABAT                                                          
         AP    ANDOLS,STANDOLS                                                  
         AP    AGDOLS,STAGDOLS                                                  
*                                                                               
         LA    R4,6                                                             
         LA    R5,STAINV                                                        
         LA    R6,PINVS-1                                                       
*                                                                               
LS234    EDIT  (B4,0(R5)),(9,0(R6)),COMMAS=YES                                  
         LA    R5,4(,R5)                                                        
         LA    R6,10(,R6)                                                       
         BCT   R4,LS234                                                         
*                                                                               
         EDIT  (P8,STANDOLS),(16,PNDOL),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,STAGDOLS),(16,PGDOL),2,COMMAS=YES,MINUS=YES                  
         EDIT  (B4,STABAT),(6,PBAT),COMMAS=YES                                  
*                                                                               
         MVI   ALLOWLIN,3          ALLOW SPACE FOR IDS                          
         B     LS238                                                            
*                                                                               
LS236    DS    0H                                                               
         MVI   P,C'"'                                                           
         MVC   P+1(4),STASTA                                                    
         MVI   P+5,C'-'                                                         
         MVC   P+6(1),STASTA+4                                                  
*                                                                               
         CLI   STAMED,C'R'                                                      
         BE    *+10                                                             
         MVC   P+6(1),STAMED                                                    
         MVI   P+7,C'"'                                                         
*                                                                               
         LA    R2,P+9                                                           
         MVI   0(R2),C'"'                                                       
         MVC   1(1,R2),STAMED                                                   
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(4,R2),STASRCE                                                  
         MVI   5(R2),C'"'                                                       
         LA    R2,7(,R2)                                                        
*                                                                               
         ICM   R0,15,STAINV                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(6,R2),DUB                                                      
         MVI   6(R2),C'.'                                                       
         LA    R2,8(,R2)                                                        
*                                                                               
         ICM   R0,15,STASPTS                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(6,R2),DUB                                                      
         MVI   6(R2),C'.'                                                       
         LA    R2,8(,R2)                                                        
*                                                                               
         OI    STANDOLS+7,X'0F'                                                 
         UNPK  WORK(11),STANDOLS                                                
         MVC   0(9,R2),WORK                                                     
         MVI   9(R2),C'.'                                                       
         MVC   10(2,R2),WORK+9                                                  
         LA    R2,13(,R2)                                                       
*                                                                               
         OI    STAGDOLS+7,X'0F'                                                 
         UNPK  WORK(11),STAGDOLS                                                
         MVC   0(9,R2),WORK                                                     
         MVI   9(R2),C'.'                                                       
         MVC   10(2,R2),WORK+9                                                  
         MVI   13(R2),X'5E'                                                     
*                                                                               
         MVI   LINE,0                                                           
*                                                                               
LS238    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         CLI   FTRDOWN,C'Y'        THIS A DOWNLOAD REPORT                       
         BE    LS240                YES                                         
         SPACE                                                                  
         BAS   RE,PIDS             PRINT ALL IDS FOR THIS STATION               
         SPACE                                                                  
*        TM    FTRFLAG,FTRSTAD                                                  
*        BZ    LS240                                                            
         SPACE                                                                  
*        MVC   PSTATN(30),STADDR                                                
*        GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LS240    LA    R3,STANEXT                                                       
         SPACE                                                                  
         CLI   FTRSORT,C'R'        SORT BY SOURCE                               
         BNE   LS250                                                            
         CLC   SVSRCE,STARSRCE     CHANGE IN MEDIA OR SOURCE                    
         BNE   LS244                                                            
         CLC   SVSTAMED,STAMED                                                  
         BE    LS250                                                            
         SPACE                                                                  
LS244    DS    0H                                                               
         CLI   FTRDOWN,C'Y'        THIS A DOWNLOAD REPORT                       
         BE    LS246                YES                                         
         SPACE                                                                  
         EDIT  (B4,AMED),(6,PSTATN),COMMAS=YES                                  
         MVC   PSTATN+7(8),=C'STATIONS'                                         
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS246    DS    0H                                                               
         XC    AMED,AMED                                                        
         MVC   SVSRCE,STARSRCE                                                  
         SPACE                                                                  
         MVC   SVSTAMED,STAMED                                                  
         SPACE                                                                  
LS250    XC    ATOT(ATOTLQ),ATOT                                                
         ZAP   ANDOLS,=P'0'                                                     
         ZAP   AGDOLS,=P'0'                                                     
         C     R3,ASTATABE                                                      
         BL    LS220                                                            
         SPACE                                                                  
LS260    DS    0H                                                               
         CLI   FTRDOWN,C'Y'        THIS A DOWNLOAD REPORT                       
         BE    LS340                YES, DONE                                   
*                                                                               
         MVC   PSRCE(5),=C'TOTAL'                                               
         SPACE                                                                  
*        EDIT COUNT OF STATIONS                                                 
         SPACE                                                                  
         EDIT  (B4,CTSTAS),(8,PSTATN-2),COMMAS=YES                              
         LA    R4,6                                                             
         LA    R5,JINV                                                          
         LA    R6,PINVS-1                                                       
LS270    EDIT  (B4,0(R5)),(9,0(R6)),COMMAS=YES                                  
         LA    R5,4(,R5)                                                        
         LA    R6,10(,R6)                                                       
         BCT   R4,LS270                                                         
*                                                                               
         EDIT  (P8,JNDOLS),(16,PNDOL),2,COMMAS=YES,ZERO=NOBLANK,       C        
               MINUS=YES                                                        
         EDIT  (P8,JGDOLS),(17,PGDOL-1),2,COMMAS=YES,ZERO=NOBLANK,     C        
               MINUS=YES                                                        
         EDIT  (B4,JBAT),(6,PBAT),COMMAS=YES,ZERO=NOBLANK                       
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PNDOL(18),=C'TOTAL BATCHES READ'                                 
*                                                                               
         EDIT  (B4,BATSRD),(6,PNDOL+19),COMMAS=YES,ZERO=NOBLANK                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PNDOL(16),=C'MAX UIDS PER STA'                                   
*                                                                               
         EDIT  (B2,MAXUID),(6,PNDOL+16),COMMAS=YES,ZERO=NOBLANK                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
         OC    ASRCTAB,ASRCTAB     IS THERE A RECAP                             
         BZ    LS286                                                            
         SPACE                                                                  
         L     R3,ASRCTAB                                                       
         USING SRCENTD,R3                                                       
         OC    SRCENT,SRCENT                                                    
         BZ    LS286                                                            
         SPACE                                                                  
         SR    R2,R2                                                            
         CLI   SRCENT,0                                                         
         BE    *+12                                                             
         LA    R3,SRCNEXT                                                       
         BCT   R2,*-12                                                          
         SPACE                                                                  
         LPR   R2,R2                                                            
         L     R3,ASRCTAB                                                       
         SPACE                                                                  
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R3),(R2),SRCENTL,L'SRCSRC,0                           
         SPACE                                                                  
         MVC   P+2(12),=C'SOURCE RECAP'                                         
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS280    MVC   P+2(4),SRCSRC                                                    
         LA    R4,6                                                             
         LA    R5,SRCINV                                                        
         LA    R6,PINVS-1                                                       
*                                                                               
LS284    EDIT  (B4,0(R5)),(9,0(R6)),COMMAS=YES                                  
         LA    R5,4(,R5)                                                        
         LA    R6,10(,R6)                                                       
         BCT   R4,LS284                                                         
*                                                                               
         EDIT  (P8,SRCNDOLS),(16,PNDOL),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SRCGDOLS),(16,PGDOL),2,COMMAS=YES,MINUS=YES                  
         EDIT  (B4,SRCBAT),(6,PBAT),COMMAS=YES                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,SRCNEXT                                                       
         OC    SRCENT,SRCENT                                                    
         BNZ   LS280                                                            
         DROP  R3                                                               
         SPACE                                                                  
LS286    OC    AMEDTAB,AMEDTAB     IS THERE A RECAP                             
         BZ    LS296                                                            
         L     R3,AMEDTAB                                                       
         USING MEDENTD,R3                                                       
         OC    MEDENT,MEDENT                                                    
         BZ    LS296                                                            
         SPACE                                                                  
         SR    R2,R2                                                            
         CLI   MEDENT,0                                                         
         BE    *+12                                                             
         LA    R3,MEDNEXT                                                       
         BCT   R2,*-12                                                          
         SPACE                                                                  
         LPR   R2,R2                                                            
         L     R3,AMEDTAB                                                       
         SPACE                                                                  
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R3),(R2),MEDENTL,L'MEDMED,0                           
         SPACE                                                                  
         MVC   P+2(11),=C'MEDIA RECAP'                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LS290    MVC   P+2(1),MEDMED                                                    
         LA    R4,6                                                             
         LA    R5,MEDINV                                                        
         LA    R6,PINVS-1                                                       
LS294    EDIT  (B4,0(R5)),(9,0(R6)),COMMAS=YES                                  
         LA    R5,4(,R5)                                                        
         LA    R6,10(,R6)                                                       
         BCT   R4,LS294                                                         
         SPACE                                                                  
         EDIT  (P8,MEDNDOLS),(16,PNDOL),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,MEDGDOLS),(16,PGDOL),2,COMMAS=YES,MINUS=YES                  
         EDIT  (B4,MEDBAT),(6,PBAT),COMMAS=YES                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,MEDNEXT                                                       
         OC    MEDENT,MEDENT                                                    
         BNZ   LS290                                                            
         DROP  R3                                                               
         SPACE                                                                  
LS296    OC    AAGYTAB,AAGYTAB     IS THERE A RECAP                             
         BZ    LS310                                                            
         L     R3,AAGYTAB                                                       
         USING AGYENTD,R3                                                       
         OC    AGYNM,AGYNM                                                      
         BZ    LS310                                                            
         SPACE                                                                  
         SR    R2,R2                                                            
         CLI   AGYNM,0                                                          
         BE    *+12                                                             
         LA    R3,AGYNEXT                                                       
         BCT   R2,*-12                                                          
         SPACE                                                                  
         LPR   R2,R2                                                            
         L     R3,AAGYTAB                                                       
         SPACE                                                                  
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R3),(R2),AGYENTL,L'AGYNM,0                            
         SPACE                                                                  
         MVC   P+2(11),=C'AGENCY LIST'                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LS300    MVC   P+2(30),AGYNM                                                    
         SPACE                                                                  
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BNE   LS306                NOPE, NOTHING TO PRINT                      
         SPACE                                                                  
         LA    RE,166                                                           
         L     RF,AIO1                                                          
LS302    CLC   AGYENTID,0(RF)                                                   
         BE    LS304                                                            
         LA    RF,12(RF)                                                        
         BCT   RE,LS302                                                         
         DC    H'0'                                                             
LS304    MVC   P+34(8),4(RF)                                                    
         SPACE                                                                  
LS306    GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,AGYNEXT                                                       
         OC    AGYNM,AGYNM                                                      
         BNZ   LS300                                                            
         DROP  R3                                                               
LS310    DS   0H                                                                
         CLI   FTRALL,C'Y'         ALLOW ALL                                    
         BE    LS340                                                            
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+3(16),=C'EXCLUDED SOURCES'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R3,SRCECT                                                        
         LA    R4,SRCETAB                                                       
*                                                                               
LS320    DS   0H                                                                
         MVC   P+4(4),0(R4)                                                     
         EDIT  (P4,4(R4)),(9,P+10),COMMAS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,8(,R4)                                                        
         BCT   R3,LS320                                                         
*                                                                               
LS340    DS   0H                                                                
         ICM   R0,15,SVMEMSIZ                                                   
         L     R1,ASTATAB          TABLE ADDRESS                                
         AHI   R1,-8                                                            
         FREEMAIN RC,LV=(0),A=(1)                                               
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
* TABLE OF TABLES                                                               
* CONTAINS START, END ADDRESSES OF EACH TABLE, ITS LENGTH AND EYCATCHER         
*                                                                               
TABTAB   DS    0A                                                               
*                                                                               
TABTABNQ EQU   3                  NUMBER OF TABLES MAINTAINED                   
EYECATLQ EQU   8                  LENGTH OF THE EYCATCHER                       
*                                                                               
STABENTN EQU   32000              NUMBER OF STATAB ENTRIES                      
ASTATAB  DS    A                  STATION TABLE                                 
ASTATABE DS    A                  END OF STATION TABLE                          
STTABSIZ DC    AL4(STABENTN*STAENTL)            STATAB SIZE                     
STTABCAT DC    CL8'*STATAB*'                                                    
*                                                                               
TABTABLQ EQU   *-TABTAB           LENGTH OF TABTAB ENTRY                        
*                                                                               
MTABENTN EQU   20                 NUMBER OF MEDTAB ENTRIES                      
AMEDTAB  DS    A                  MEDIA TABLE                                   
AMEDTABE DS    A                  END OF MEDIA TABLE                            
MDTABSIZ DC    AL4(MTABENTN*MEDENTL)            MEDTAB SIZE                     
MDTABCAT DC    CL8'*MEDTAB*'                                                    
*                                                                               
RTABENTN EQU   150                 NUMBER OF SRCTAB ENTRIES                     
ASRCTAB  DS    A                  SOURCE TABLE                                  
ASRCTABE DS    A                  END OF SOURCE TABLE                           
SRTABSIZ DC    AL4(RTABENTN*SRCENTL)            SRCTAB SIZE                     
SRTABCAT DC    CL8'*SRCTAB*'                                                    
*                                                                               
* AGYTAB NOT SUPPORTED NOW, BUT CODE LEFT IN PLACE                              
*                                                                               
ATABENTN EQU   500                 NUMBER OF AGYTAB ENTRIES                     
AAGYTAB  DS    A                                                                
AAGYTABE DS    A                                                                
AGTABSIZ DC    AL4(ATABENTN*AGYENTL)            AGYTAB SIZE                     
AGTABCAT DC    CL8'*AGYTAB*'                                                    
*                                                                               
*                                                                               
         EJECT                                                                  
* FIND PRINTABLE POWER CODE & SIGNON CODE, SORT, AND PRINT *                    
         SPACE                                                                  
         DS    0H                                                               
         USING STAENTD,R3                                                       
PIDS     NTR1                                                                   
         LA    R2,BLOCK                                                         
         SPACE                                                                  
         MVI   BLOCK,X'FF'                                                      
         MVC   BLOCK+1(239),BLOCK                                               
         MVC   BLOCK+240(240),BLOCK                                             
         SPACE                                                                  
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BNE   PIDSX                NOPE, NOTHING TO PRINT                      
         SPACE                                                                  
         LA    R0,(L'STAUIDS)/2                                                 
         LA    R5,STAUIDS                                                       
         DROP  R3                                                               
         SPACE                                                                  
         OC    0(2,R5),0(R5)       ANY ENTRIES?                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
PIDS10   OC    0(2,R5),0(R5)       END OF ENTRIES                               
         BZ    PIDS20                                                           
         SPACE                                                                  
         LA    R4,166                                                           
         L     R3,AIO1                                                          
         SPACE                                                                  
PIDS14   OC    0(2,R3),0(R3)       END OF AGY IDS TABLE                         
         BZ    PIDS16                                                           
         CLC   0(2,R5),0(R3)                                                    
         BE    PIDS15                                                           
         LA    R3,12(R3)                                                        
         BCT   R4,PIDS14                                                        
         DC    H'0'                                                             
PIDS15   MVC   0(10,R2),2(R3)                                                   
         B     PIDS18                                                           
         SPACE                                                                  
PIDS16   MVC   0(2,R3),0(R5)                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),0(R5)   FROM TABLE                                   
         L     R4,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        AGY ID                                       
         MVC   DATADISP,=AL2(28)                                                
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSCD,R6                                                        
         MVC   2(8,R2),CTDSC                                                    
         MVC   4(8,R3),CTDSC                                                    
         SPACE                                                                  
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'06'        POWER CODE                                   
         MVC   DATADISP,=AL2(28)                                                
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTAGYEL,R6                                                       
         MVC   0(2,R2),CTAGYID                                                  
         MVC   2(2,R3),CTAGYID                                                  
PIDS18   LA    R2,10(,R2)                                                       
         LA    R5,2(,R5)                                                        
         BCT   R0,PIDS10                                                        
         DROP  R4,R6                                                            
         SPACE                                                                  
PIDS20   L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,BLOCK,48,10,10,0                                       
         SPACE                                                                  
         LA    R2,PINVS                                                         
         LA    R3,BLOCK                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   PIDS36                                                           
         DC    H'0'                                                             
PIDS30   CLI   0(R3),X'FF'                                                      
         BE    PIDS40                                                           
         SPACE                                                                  
         LA    R1,P+110                                                         
         CR    R1,R2               START NEW LINE                               
         BH    PIDS34                                                           
         SPACE                                                                  
         LA    R2,PINVS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PIDS36                                                           
         SPACE                                                                  
PIDS34   MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
PIDS36   MVC   0(2,R2),0(R3)                                                    
         MVI   2(R2),C'/'                                                       
         MVC   3(8,R2),2(R3)                                                    
         LA    R2,12(,R2)                                                       
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(,R2)                                                        
         LA    R3,10(,R3)                                                       
         B     PIDS30                                                           
         SPACE                                                                  
PIDS40   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
PIDSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   COUNT INVOICES (CONV, UNCONV, DEL) SPOTS, DOLLARS IN BATCH        *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
EZCOUNT  NTR1                                                                   
         L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         CLI   EZMODE,EZINVL       END OF INVOICE                               
         BE    EZC200                                                           
         SPACE                                                                  
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   EZCX                                                             
         SPACE                                                                  
         CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   EZC160                                                           
         CLC   EZIHDMOS,FTRMOS                                                  
         BNE   EZC360                                                           
         SPACE                                                                  
EZC160   CLI   FTRINVNO,0          TEST HAVE INVOICE FILTER                     
         BNH   EZC170                                                           
         ZIC   RF,FTRINVLN         GET INVNO LENGTH                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHINV(0),FTRINVNO                                              
         BNE   EZC360                                                           
         SPACE                                                                  
EZC170   CLI   FTRDATE,0           FILTER ON CONVERTED DATE                     
         BNH   EZC180                                                           
         SPACE                                                                  
         OC    EZIHCVDT,EZIHCVDT   IS THIS CONVERTED                            
         BZ    EZC360               NO                                          
         SPACE                                                                  
         CLI   FTRDATES,0          FILTER ON CONVERTED DATE RANGE               
         BNE   EZC174                                                           
         SPACE                                                                  
         CLC   EZIHCVDT,FTRDATE    FILTER ON EXACT DATE                         
         BNE   EZC360                                                           
         B     EZC180                                                           
EZC174   CLI   FTRDATES,C'+'       PLUS                                         
         BE    EZC176                                                           
         CLI   FTRDATES,C'-'       MINUS                                        
         BE    EZC178                                                           
         DC    H'0'                                                             
EZC176   CLC   EZIHCVDT,FTRDATE                                                 
         BL    EZC360                                                           
         B     EZC180                                                           
EZC178   CLC   EZIHCVDT,FTRDATE                                                 
         BH    EZC360                                                           
         SPACE                                                                  
EZC180   TM    FTRFLAG,FTRDONE     DONE ONLY                                    
         BZ    EZC181                                                           
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    EZC360                                                           
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BO    EZC190               YES                                         
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BO    EZC190               YES                                         
         B     EZC360                                                           
         SPACE                                                                  
EZC181   TM    FTRFLAG,FTRUCVQ     UNCONVERTED ONLY                             
         BZ    EZC182               YES                                         
         SPACE                                                                  
* IF CONVERTED, DELETED, OR EVEN A RECONVERT (HAS CONVERT ON) BYPASS *          
         SPACE                                                                  
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL   CONVERTED OR DELETED                 
         BNZ   EZC360                       YES, BYPASS                         
         B     EZC190                                                           
         SPACE                                                                  
EZC182   TM    FTRFLAG,FTRRCVQ     RECONVERT ONLY                               
         BZ    EZC184                                                           
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    EZC190               YES                                         
         B     EZC360                                                           
         SPACE                                                                  
EZC184   TM    FTRFLAG,FTRCVQ      CONVERTED ONLY                               
         BZ    EZC186                                                           
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    EZC360                                                           
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BO    EZC190               YES                                         
         B     EZC360                                                           
         SPACE                                                                  
EZC186   TM    FTRFLAG,FTROVR      OVERRIDES ONLY                               
         BZ    EZC188                                                           
         TM    EZIHCVST,EZIHCOVR   OVERRIDE                                     
         BO    EZC190               YES                                         
         B     EZC360                                                           
         SPACE                                                                  
EZC188   TM    FTRFLAG,FTRDEL      DELETES ONLY                                 
         BZ    EZC190                                                           
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BZ    EZC360               NO, BYPASS                                  
EZC190   MVI   PROCESS,C'Y'                                                     
         SPACE                                                                  
         OC    AAGYTAB,AAGYTAB     IS THERE AN AGENCY NAME TABLE                
         BZ    EZCX                                                             
         L     RE,AAGYTAB                                                       
         USING AGYENTD,RE                                                       
EZC194   OC    AGYNM,AGYNM                                                      
         BZ    EZC196                                                           
         CLC   EZAGNAM,AGYNM                                                    
         BE    EZCX                                                             
         LA    RE,AGYNEXT                                                       
         C     RE,AAGYTABE                                                      
         BL    EZC194                                                           
         DC    H'0'                                                             
EZC196   MVC   AGYNM,EZAGNAM                                                    
         MVC   AGYENTID(2),SVUID                                                
         SPACE                                                                  
         B     EZCX                                                             
         SPACE                                                                  
* HAVE FILTERED AT INVOICE HEADER, NOW HAVE HEADER AND TOTAL *                  
         SPACE                                                                  
EZC200   DS   0H                                                                
         CLI   PROCESS,C'Y'                                                     
         BNE   EZCX                                                             
         SPACE                                                                  
         MVC   SVINVSEQ,EZRECSEQ     SAVE INVOICE HEADER SEQ                    
         MVC   SVSNBND,EZSNBND         BAND                                     
         MVC   SVSTA(4),EZSNSTA                                                 
         OC    EZSNNAME(100),SPACES                                             
         OC    EZSNNAME+100(50),SPACES                                          
         LA    R0,4                                                             
         LA    R1,EZSNNAME+120                                                  
EZC204   CLC   0(30,R1),SPACES                                                  
         BNE   EZC206                                                           
         SH    R1,=H'30'                                                        
         BCT   R0,EZC204                                                        
         SPACE                                                                  
EZC206   MVC   SVSTADDR,0(R1)                                                   
         SPACE                                                                  
         CLI   EZSNMED,C'C'                                                     
         BE    EZC210                                                           
         CLI   EZSNMED,C'S'                                                     
         BNE   EZC212                                                           
EZC210   MVC   EZSNMED,=C'N '                                                   
         SPACE                                                                  
EZC212   CLI   EZSNBND,C'C'                                                     
         BE    EZC214                                                           
         CLI   EZSNBND,C'S'                                                     
         BNE   EZC216                                                           
EZC214   MVC   EZSNBND,=C'N '                                                   
         SPACE                                                                  
EZC216   MVC   SVSTA+4(1),EZSNMED                                               
         SPACE                                                                  
         CLI   EZSNMED,C'R'                                                     
         BNE   *+10                                                             
         MVC   SVSTA+4(1),EZSNBND                                               
         SPACE                                                                  
         MVC   SVSTAMED,EZSNMED                                                 
         SPACE                                                                  
*        CLC   EZIHINST,=CL25'BILLING INVOICE REVERSAL'                         
*        BNE   EZC214                                                           
* NEW COUNT?                                                                    
         SPACE                                                                  
         SPACE                                                                  
EZC230   TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BZ    EZC240               NO                                          
*                                                                               
*   COUNT CONVERTED HERE                                                        
*                                                                               
         L     R1,TINVC                                                         
         AHI   R1,1                                                             
         ST    R1,TINVC                                                         
*                                                                               
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    EZC300                                                           
*                                                                               
*   COUNT OVERRIDES HERE                                                        
*                                                                               
         L     R1,TINVO                                                         
         AHI   R1,1                                                             
         ST    R1,TINVO                                                         
         B     EZC300                                                           
*                                                                               
EZC240   TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BZ    EZC250                                                           
*                                                                               
*   COUNT DELETED HERE                                                          
*                                                                               
         L     R1,TINVD                                                         
         AHI   R1,1                                                             
         ST    R1,TINVD                                                         
*                                                                               
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    EZC300                                                           
*                                                                               
*   COUNT OVERRIDES HERE                                                        
*                                                                               
         L     R1,TINVO                                                         
         AHI   R1,1                                                             
         ST    R1,TINVO                                                         
         B     EZC300                                                           
*                                                                               
*   COUNT UNCONVERTED HERE                                                      
*                                                                               
EZC250   L     R1,TINVU                                                         
         AHI   R1,1                                                             
         ST    R1,TINVU                                                         
*                                                                               
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    EZC300                                                           
*                                                                               
*   COUNT OVERRIDES HERE                                                        
*                                                                               
         L     R1,TINVO                                                         
         AHI   R1,1                                                             
         ST    R1,TINVO                                                         
*                                                                               
EZC300   L     R1,TINVSPT                                                       
         A     R1,EZIHTSPN                                                      
         ST    R1,TINVSPT                                                       
*                                                                               
         ICM   R1,15,EZITBDUE                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         AP    TINVNDOL,DUB                                                     
*                                                                               
         ICM   R1,15,EZITBACT                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         AP    TINVGDOL,DUB                                                     
*                                                                               
         L     R1,TINV                                                          
         AHI   R1,1                                                             
         ST    R1,TINV                                                          
*                                                                               
         B     EZCX                                                             
*                                                                               
* THIS IS USED TO BYPASS THIS INVOICE - NOT IN TOTALS                           
*                                                                               
EZC360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         MVI   PROCESS,C'N'                                                     
*                                                                               
EZCX     B     EXIT                                                             
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
NOCLTFND L     R1,=A(NOCLTMS)                                                   
         B     ERREXIT                                                          
         SPACE                                                                  
* CAN'T CHANGE RECS ON OTHER USERS *                                            
         SPACE                                                                  
UIDVRERR L     R1,=A(UIDVRMS)                                                   
         B     ERREXIT                                                          
MAXIOSER L     R1,=A(MAXIOSMS)                                                  
         B     ERREXIT                                                          
TABSIZER L     R1,=A(TABSIZMS)                                                  
         B     ERREXIT                                                          
         SPACE                                                                  
ERREXIT  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1-10                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         EX    RF,ERREXITM                                                      
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         SPACE                                                                  
ERREXITA GOTO1 ERREX2                                                           
ERREXITM MVC   CONHEAD+10(0),1(R1)                                              
         EJECT                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
* THESE SOURCES ARE BYPASSED UNLESS SPECIFICALLY REQUESTED                      
* OR FILTER ALL IS ENTERED                                                      
*                                                                               
SRCETAB  DS   0F                                                                
         DC    CL4'ACT ',PL4'0'                                                 
         DC    CL4'APNY',PL4'0'                                                 
         DC    CL4'BBDO',PL4'0'                                                 
         DC    CL4'BVS ',PL4'0'                                                 
         DC    CL4'CAT ',PL4'0'                                                 
         DC    CL4'CCOL',PL4'0'                                                 
         DC    CL4'CFM ',PL4'0'                                                 
         DC    CL4'CMR ',PL4'0'                                                 
         DC    CL4'COG2',PL4'0'                                                 
         DC    CL4'COKA',PL4'0'                                                 
         DC    CL4'COKE',PL4'0'                                                 
         DC    CL4'COKM',PL4'0'                                                 
         DC    CL4'COMV',PL4'0'                                                 
         DC    CL4'CORE',PL4'0'                                                 
         DC    CL4'COWU',PL4'0'                                                 
         DC    CL4'COZE',PL4'0'                                                 
         DC    CL4'DGUT',PL4'0'                                                 
         DC    CL4'GNC ',PL4'0'                                                 
         DC    CL4'ICG2',PL4'0'                                                 
         DC    CL4'ICOO',PL4'0'                                                 
         DC    CL4'ICWU',PL4'0'                                                 
         DC    CL4'ICZE',PL4'0'                                                 
         DC    CL4'IPBS',PL4'0'                                                 
         DC    CL4'IPCS',PL4'0'                                                 
         DC    CL4'IPDS',PL4'0'                                                 
         DC    CL4'IPGS',PL4'0'                                                 
         DC    CL4'IPMS',PL4'0'                                                 
         DC    CL4'IPOS',PL4'0'                                                 
         DC    CL4'IPS ',PL4'0'                                                 
         DC    CL4'IPSC',PL4'0'                                                 
         DC    CL4'IPSD',PL4'0'                                                 
         DC    CL4'IPSG',PL4'0'                                                 
         DC    CL4'IPSM',PL4'0'                                                 
         DC    CL4'IPSO',PL4'0'                                                 
         DC    CL4'IPSP',PL4'0'                                                 
         DC    CL4'IPSR',PL4'0'                                                 
         DC    CL4'IPSS',PL4'0'                                                 
         DC    CL4'IPSW',PL4'0'                                                 
         DC    CL4'IPSY',PL4'0'                                                 
         DC    CL4'IPSZ',PL4'0'                                                 
         DC    CL4'IPYS',PL4'0'                                                 
         DC    CL4'IPZS',PL4'0'                                                 
         DC    CL4'IRNY',PL4'0'                                                 
         DC    CL4'ISD ',PL4'0'                                                 
         DC    CL4'JSTA',PL4'0'                                                 
         DC    CL4'KATZ',PL4'0'                                                 
         DC    CL4'KGIL',PL4'0'                                                 
         DC    CL4'LIPA',PL4'0'                                                 
         DC    CL4'MATT',PL4'0'                                                 
         DC    CL4'MHEE',PL4'0'                                                 
         DC    CL4'MHER',PL4'0'                                                 
         DC    CL4'MLO ',PL4'0'                                                 
         DC    CL4'MLOP',PL4'0'                                                 
         DC    CL4'OMD ',PL4'0'                                                 
         DC    CL4'PGEO',PL4'0'                                                 
         DC    CL4'RAPP',PL4'0'                                                 
         DC    CL4'RMR ',PL4'0'                                                 
         DC    CL4'SDI ',PL4'0'                                                 
         DC    CL4'SDIC',PL4'0'                                                 
         DC    CL4'SDIJ',PL4'0'                                                 
         DC    CL4'SDIL',PL4'0'                                                 
         DC    CL4'SDIM',PL4'0'                                                 
         DC    CL4'SDIO',PL4'0'                                                 
         DC    CL4'SDIT',PL4'0'                                                 
         DC    CL4'SDIV',PL4'0'                                                 
         DC    CL4'SDIY',PL4'0'                                                 
         DC    CL4'SDIZ',PL4'0'                                                 
         DC    CL4'SHAI',PL4'0'                                                 
         DC    CL4'SMV ',PL4'0'                                                 
         DC    CL4'STRA',PL4'0'                                                 
         DC    CL4'TEST',PL4'0'                                                 
         DC    CL4'TZIH',PL4'0'                                                 
         DC    CL4'WAP ',PL4'0'                                                 
         DC    CL4'WIM ',PL4'0'                                                 
         DC    CL4'WPRI',PL4'0'                                                 
         DC    CL4'WWNY',PL4'0'                                                 
         DC    CL4'ZCAM',PL4'0'                                                 
         DC    CL4'ZCAR',PL4'0'                                                 
*        DC    CL4'USA ',PL4'0'     PER PAT GEORGE JAN06                        
SRCECT   EQU   (*-SRCETAB)/8                                                    
         LTORG                                                                  
         SPACE 2                                                                
         DC    AL1(L'MAXIOSMS-1)                                                
MAXIOSMS DC    C'BE MORE SPECIFIC, TOO MUCH DATA FOR ONLINE *'                  
         DC    AL1(L'INVUIDMS-1)                                                
INVUIDMS DC    C'INVALID USER ID *'                                             
         DC    AL1(L'CCLENMS-1)                                                 
CCLENMS  DC    C'CLIENT CODE MUST BE 2 OR 3 CHARACTERS *'                       
         DC    AL1(L'PCLENMS-1)                                                 
PCLENMS  DC    C'PRODUCT CODE MUST BE 2 OR 3 CHARACTERS *'                      
         DC    AL1(L'INVLENMS-1)                                                
INVLENMS DC    C'INVOICE CAN''T BE MORE THAN 10 CHARACTERS *'                   
         DC    AL1(L'USIDLNMS-1)                                                
USIDLNMS DC    C'USER ID CAN''T BE MORE THAN 8 CHARACTERS *'                    
         DC    AL1(L'UIDVRMS-1)                                                 
UIDVRMS  DC    C'USER ID CAN''T DO CHANGE *'                                    
         DC    AL1(L'TABSIZMS-1)                                                
TABSIZMS DC    C'TOO BIG TO RUN NOW *'                                          
         DC    AL1(L'MISPRDMS-1)                                                
MISPRDMS DC    C'STATION REQUIRES PRODUCT *'                                    
         DC    AL1(L'NOCLTMS-1)                                                 
NOCLTMS  DC    C'NO RECORD FOR 25 CHAR ADVERTISER CODE *'                       
         SPACE                                                                  
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'EASI'                                                     
         SSPEC H1,49,C'ACTIVE STATIONS'                                         
         SSPEC H2,49,C'---------------'                                         
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,4,C'STATION'                                                  
         SSPEC H9,4,C'-------'                                                  
         SSPEC H8,13,C'MED'                                                     
         SSPEC H9,13,C'---'                                                     
         SSPEC H8,17,C'SRCE'                                                    
         SSPEC H9,17,C'----'                                                    
         SSPEC H8,23,C'INVOICES'                                                
         SSPEC H9,23,C'--------'                                                
         SSPEC H8,32,C'CONVERTED'                                               
         SSPEC H9,32,C'---------'                                               
         SSPEC H8,42,C'OVERIDES'                                                
         SSPEC H9,42,C'--------'                                                
         SSPEC H8,52,C'UNCONVRTD'                                               
         SSPEC H9,52,C'---------'                                               
         SSPEC H8,64,C'DELETED'                                                 
         SSPEC H9,64,C'-------'                                                 
         SSPEC H8,76,C'SPOTS'                                                   
         SSPEC H9,76,C'-----'                                                   
         SSPEC H8,89,C'NET DOLLARS'                                             
         SSPEC H9,89,C'-----------'                                             
         SSPEC H8,107,C'GROSS DOLLARS'                                          
         SSPEC H9,107,C'-------------'                                          
         SSPEC H8,125,C'BATCHES'                                                
         SSPEC H9,125,C'-------'                                                
         DC    X'00'                                                            
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
***********************************************************************         
*   HEADHOOK ROUTINE FOR REPORT                                       *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,RQDTSTR),(5,H3+48)                                
         MVI   H3+56,C'-'                                                       
         GOTO1 (RF),(R1),(2,RQDTEND),(5,H3+57)                                  
         CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   HDHK10                                                           
         MVC   H4+30(4),=C'MOS='                                                
         MVC   H4+34(6),FTRMOS                                                  
HDHK10   CLI   FTRSRCE,0           TEST HAVE SOURCE FILTER                      
         BNH   HDHK20                                                           
         MVC   H4+45(7),=C'SOURCE='                                             
         MVC   H4+52(4),FTRSRCE                                                 
HDHK20   CLI   FTRMEDIA,0          TEST HAVE SOURCE FILTER                      
         BNH   HDHK30                                                           
         MVC   H4+58(6),=C'MEDIA='                                              
         MVC   H4+64(1),FTRMEDIA                                                
HDHK30   CLI   FUIDNUM,X'FF'       TEST READING ALL USERS                       
         BNE   HDHKX                                                            
         MVC   H4+70(8),=C'USER=ALL'                                            
HDHKX    XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILTERS (IF ANY)                                           *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
VFTR     NMOD1 0,**VFTR**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         XC    FILTERS,FILTERS                                                  
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         SPACE                                                                  
         SPACE                                                                  
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,3                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFTRHLP                                                          
         LA    R0,25               NON-STANDARD LENGTH                          
         MVI   BYTE,1                                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,((R0),(R2)),(5,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    MISSERRA             SCANNER DIDN'T FIND ANYTHING                
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         CH    R1,=H'2'                                                         
         BL    FTRLENER                                                         
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),C'+'          PLUS                                         
         BE    VFTR12               YES, SAVE IT                                
         CLI   0(R5),C'-'          MINUS                                        
         BNE   VFTR14               NO, NETHER                                  
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         ACTIVITY DATE                                
         BNE   VFTR20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    BADATEA              NO                                          
         GOTO1 DATCON,(R1),(0,WORK),(1,FTRDATE)                                 
         MVC   FTRDATES,HOLDSIGN                                                
         B     VFTR90                                                           
VFTR20   EX    R1,VFTRCLCB         CLIENT CODE (CC)                             
         BNE   VFTR24                                                           
         CLI   1(R4),2                                                          
         BL    CCLENER                                                          
         CLI   1(R4),3                                                          
         BH    CCLENER                                                          
         MVC   FTRQCLT,22(R4)                                                   
         B     VFTR90                                                           
VFTR24   EX    R1,VFTRCLCD         PRODUCT CODE (PC)                            
         BNE   VFTR30                                                           
         CLI   1(R4),2                                                          
         BL    PCLENER                                                          
         CLI   1(R4),3                                                          
         BH    PCLENER                                                          
         MVC   FTRQPRD,22(R4)                                                   
         B     VFTR90                                                           
VFTR30   EX    R1,VFTRCLCF         CONVERTED (CONV)                             
         BNE   VFTR32                                                           
         TM    FTRFLAG,FTRUCVQ                                                  
         BNZ   VFTRERRC                                                         
         OI    FTRFLAG,FTRCVQ                                                   
         B     VFTR90                                                           
VFTR32   EX    R1,VFTRCLCG         RECONVERTS (RECONV)                          
         BNE   VFTR34                                                           
         OI    FTRFLAG,FTRRCVQ                                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR34   EX    R1,VFTRCLCH         UNCONVERTED (UNCONV)                         
         BNE   VFTR36                                                           
         TM    FTRFLAG,FTRCVQ                                                   
         BNZ   VFTRERRC                                                         
         OI    FTRFLAG,FTRUCVQ                                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR36   EX    R1,VFTRCLCO         OVERRIDES                                    
         BNE   VFTR40                                                           
         OI    FTRFLAG,FTROVR                                                   
         B     VFTR90                                                           
         SPACE                                                                  
VFTR40   EX    R1,VFTRCLCI         INVOICE                                      
         BNE   VFTR44                                                           
         CLI   1(R4),10                                                         
         BH    INVLENER                                                         
         MVC   FTRINVNO,22(R4)                                                  
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRINVLN                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR44   EX    R1,VFTRCLCV         DDS AGENCIES ONLY                            
         BNE   VFTR46                                                           
         MVI   FTRDDS,C'Y'                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR46   EX    R1,VFTRCLCW         NON-DDS AGENCIES ONLY                        
         BNE   VFTR48                                                           
         MVI   FTRDDS,C'N'                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR48   EX    R1,VFTRCLCX         TRACE                                        
         BNE   VFTR50                                                           
         MVI   FTRTRCE,C'Y'                                                     
         B     VFTR90                                                           
         SPACE                                                                  
VFTR50   EX    R1,VFTRCLCJ         SOURCE                                       
         BNE   VFTR54                                                           
         CLI   1(R4),4                                                          
         BH    SRCLENER                                                         
         MVC   FTRSRCE,22(R4)                                                   
         B     VFTR90                                                           
         SPACE                                                                  
VFTR54   EX    R1,VFTRCLCK         DONE (MEANS CONV/DEL INC KEEP)               
         BNE   VFTR56                                                           
         SPACE                                                                  
         OI    FTRFLAG,FTRDONE                                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR56   EX    R1,VFTRCLCZ         DOWN (DOWNLOAD REPORT)                       
         BNE   VFTR60                                                           
         SPACE                                                                  
         MVI   FTRDOWN,C'Y'                                                     
         SPACE                                                                  
*        OI    GENSTAT2,NOREQDET   DON'T PRINT REQUEST DETAILS PAGE             
         B     VFTR90                                                           
         SPACE                                                                  
VFTR60   EX    R1,VFTRCLCL         BATCH DATE(S)                                
         BNE   VFTR66                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS DATE VALID                               
         BZ    BADATEA              NO                                          
         MVC   WORK+6(6),WORK                                                   
         CLM   RE,1,1(R4)          WAS ONLY 1 DATE ENTERED                      
         BE    VFTR64                                                           
         LA    R5,1(RE,R5)                                                      
         GOTO1 (RF),(R1),(0,(R5)),WORK+6                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATEA                                                          
         SPACE                                                                  
VFTR64   GOTO1 DATCON,(R1),(0,WORK),(2,FTRBSDT)                                 
         GOTO1 (RF),(R1),(0,WORK+6),(2,FTRBEDT)                                 
         B     VFTR90                                                           
         SPACE                                                                  
VFTR66   EX    R1,VFTRCLCM         MEDIA                                        
         BNE   VFTR70                                                           
         LA    R5,22(,R4)                                                       
         CLI   0(R5),C'T'          TV                                           
         BE    VFTR68                                                           
         CLI   0(R5),C'R'          RADIO                                        
         BE    VFTR68                                                           
         CLI   0(R5),C'N'          NETWORK                                      
         BE    VFTR68                                                           
         CLI   0(R5),C'C'          CABLE                                        
         BE    VFTR68                                                           
         CLI   0(R5),C'S'          SYNDICATION                                  
         BE    VFTR68                                                           
         CLI   0(R5),C'X'          NETWORK RADIO                                
         BNE   VFTRMDER                                                         
VFTR68   MVC   FTRMEDIA,0(R5)                                                   
         B     VFTR90                                                           
         SPACE                                                                  
VFTR70   EX    R1,VFTRCLCT         DELETE                                       
         BNE   VFTR72                                                           
         OI    FTRFLAG,FTRDEL                                                   
         B     VFTR90                                                           
         SPACE                                                                  
VFTR72   EX    R1,VFTRCLCN         TEST - ONLY READ 500 BATCHES                 
         BNE   VFTR74                                                           
         OI    FTRFLAG,FTRTEST                                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR74   EX    R1,VFTRCLCU         USER ID                                      
         BNE   VFTR80                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VFTRHLP                                                          
         CLI   1(R4),8                                                          
         BH    USIDLNER                                                         
         MVC   FUID,22(R4)                                                      
         SPACE                                                                  
         MVI   FUIDNUM,X'FF'       ALL IDS                                      
         CLC   FUID(3),=C'ALL'                                                  
         BE    VFTR90                                                           
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+15(0),22(R4)                                                 
         L     R5,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R5)                    
         CLI   8(R1),0                                                          
         BNE   USERIDER                                                         
*                                                                               
         USING CTIKEY,R5                                                        
         LA    R6,CTIDATA                                                       
         DROP  R5                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,VFNEXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   FUIDNUM,2(R6)       BINARY USER ID (ORIGIN)                      
         XC    WORK,WORK                                                        
         MVC   WORK(2),2(R6)                                                    
         SPACE                                                                  
         LH    R5,=AL2(WRKFBUFR-SYSD)                                           
         AR    R5,R9                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WORK,AIO3,(R5)             
         SPACE                                                                  
         LA    R1,WORK                                                          
         MVC   EASIWK,UKUSRINF-UKRECD(R1)                                       
         SPACE                                                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR80   EX    R1,VFTRCLCP         MOS - MONTH OF SERVICE                       
         BNE   VFTR82                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               YES, ERROR                                  
         SPACE                                                                  
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
         GOTO1 DATCON,(R1),(0,WORK),(6,FTRMOS)                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR82   EX    R1,VFTRCLCQ         STA - PRINT STATION ADDRESS                  
         BNE   VFTR83                                                           
         OI    FTRFLAG,FTRSTAD                                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR83   EX    R1,VFTRCLCR         ALL - INCLUDE SJR/IRNY/WWNY                  
         BNE   VFTR84                                                           
         MVI   FTRALL,C'Y'                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR84   EX    R1,VFTRCLCS         SORT - STA OR SRC                            
*        BNE   VFTRERR                                                          
         BNE   VFTR88                                                           
*                                                                               
         CLC   =C'STA',22(R4)                                                   
         BNE   VFTR86                                                           
         MVI   FTRSORT,C'S'                                                     
         B     VFTR90                                                           
*                                                                               
VFTR86   CLC   =C'SRC',22(R4)                                                   
         BNE   SORTERR                                                          
         MVI   FTRSORT,C'R'                                                     
         B     VFTR90                                                           
*                                                                               
VFTR88   DS    0H                                                               
         EX    R1,VFTRCLC1                                                      
         BNE   VFTRERR                                                          
         OI    FTRFLAG2,FTR2CAN                                                 
         B     VFTR90                                                           
*                                                                               
VFTR90   ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-STANDARD LENGTH                    
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         SPACE                                                                  
         CLI   FUIDNUM,X'FF'       ALL IDS                                      
         BNE   *+8                                                              
         MVI   EASIWK+4,C'1'                WRKF1                               
         SPACE                                                                  
         CLI   FTRDDS,0                                                         
         BE    VFTRX                                                            
         SPACE                                                                  
         LA    R3,240                                                           
         LA    R5,BLOCK                                                         
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID(3),=C'EIX'                                                
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
VFTR94   CLC   KEY(18),0(R4)                                                    
         BNE   VFTRX                                                            
*                                                                               
         USING CTIKEY,R4                                                        
         LA    R6,CTIDATA                                                       
         DROP  R4                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,VFNEXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   0(2,R5),2(R6)       BINARY USER ID (ORIGIN)                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ ',=C'CTFILE ',KEY,(R4)                    
         LA    R5,2(,R5)                                                        
         BCT   R3,VFTR94                                                        
         DC    H'0'                                                             
VFTRX    XIT1                                                                   
         SPACE                                                                  
VFTRCLCA CLC   12(0,R4),=CL4'ACT'      ACTIVITY DATE                            
VFTRCLCB CLC   12(0,R4),=CL3'CC'       CLIENT CODE                              
VFTRCLCC CLC   12(0,R4),=CL3'CN'              NAME                              
VFTRCLCD CLC   12(0,R4),=CL3'PC'       PRODUCT CODE                             
VFTRCLCE CLC   12(0,R4),=CL3'PN'               NAME                             
VFTRCLCF CLC   12(0,R4),=CL5'CONV'                                              
VFTRCLCG CLC   12(0,R4),=CL7'RECONV'                                            
VFTRCLCH CLC   12(0,R4),=CL7'UNCONV'                                            
VFTRCLCI CLC   12(0,R4),=CL8'INVOICE'                                           
VFTRCLCJ CLC   12(0,R4),=CL7'SOURCE'                                            
VFTRCLCK CLC   12(0,R4),=CL5'DONE'                                              
VFTRCLCL CLC   12(0,R4),=CL6'BATCH'                                             
VFTRCLCM CLC   12(0,R4),=CL6'MEDIA'                                             
VFTRCLCN CLC   12(0,R4),=CL5'TEST'                                              
VFTRCLCO CLC   12(0,R4),=CL9'OVERRIDE'                                          
VFTRCLCP CLC   12(0,R4),=CL4'MOS'                                               
VFTRCLCQ CLC   12(0,R4),=CL4'STA'                                               
VFTRCLCR CLC   12(0,R4),=CL4'ALL'                                               
VFTRCLCS CLC   12(0,R4),=CL5'SORT '                                             
VFTRCLCT CLC   12(0,R4),=CL7'DELETE'                                            
VFTRCLCU CLC   12(0,R4),=CL5'USER'                                              
VFTRCLCV CLC   12(0,R4),=CL4'DDS '                                              
VFTRCLCW CLC   12(0,R4),=CL7'NONDDS '                                           
VFTRCLCX CLC   12(0,R4),=CL6'TRACE '                                            
VFTRCLCZ CLC   12(0,R4),=CL5'DOWN '                                             
VFTRCLC1 CLC   12(0,R4),=CL6'CANADA'                                            
*                                                                               
VFGETEL  LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         B     VFNEXT2                                                          
         SPACE                                                                  
VFNEXTEL CLI   0(R6),0                                                          
         BE    VFNEXTX                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
VFNEXT2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     VFNEXTEL                                                         
         SPACE                                                                  
VFNEXTX  LTR   RE,RE                                                            
         BR    RE                                                               
USERIDER L     R1,=A(INVUIDMS)                                                  
         B     VFTRERX                                                          
USIDLNER L     R1,=A(UIDLENMS)                                                  
         B     VFTRERX                                                          
VFTRMDER L     R1,=A(VFTRMDMS)                                                  
         B     VFTRERX                                                          
VFTRERRC L     R1,=A(VFTRMS)                                                    
         B     VFTRERX                                                          
CCLENER  L     R1,=A(CCLENMS)                                                   
         B     VFTRERX                                                          
PCLENER  L     R1,=A(PCLENMS)                                                   
         B     VFTRERX                                                          
INVLENER L     R1,=A(INVLENMS)                                                  
         B     VFTRERX                                                          
SRCLENER L     R1,=A(SRCLENMS)                                                  
         B     VFTRERX                                                          
MOSERR   L     R1,=A(MOSERMS)                                                   
         B     VFTRERX                                                          
SORTERR  L     R1,=A(SORTMS)                                                    
         B     VFTRERX                                                          
FTRLENER L     R1,=A(FTRLENMS)                                                  
         B     VFTRERX                                                          
VFTRHLP  L     R1,=A(FTRHELP)                                                   
VFTRERX  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     VFTRERY                                                          
         MVC   CONHEAD(0),1(R1)                                                 
VFTRERR  XC    CONHEAD,CONHEAD                                                  
         OI    BYTE,X'F0'                                                       
         MVC   CONHEAD(30),=C'* ERROR * INVALID FILTER FIELD'                   
         MVC   CONHEAD+31(1),BYTE                                               
         MVI   CONHEAD+33,C'*'                                                  
         SPACE                                                                  
VFTRERY  GOTO1 ERREX2                                                           
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRA                                                         
BADATEA  MVI   ERROR,INVDATE                                                    
         SPACE                                                                  
TRAPERRA GOTO1 ERREX                                                            
         DC    AL1(L'FTRHELP-1)                                                 
FTRHELP  DC    CL60'FILTERS=ACT/CC/CN/PC/PN/CONV/RECONV/INV/SOURCE/DEL/C        
               UNCONV *'                                                        
         DC    AL1(L'VFTRMS-1)                                                  
VFTRMS   DC    C'* ERROR * CAN''T USE CONVERT/UNCOVERT TOGETHER *'              
         DC    AL1(L'VFTRMDMS-1)                                                
VFTRMDMS DC    C'* ERROR * VALID MEDIAS - T, R, N S, C, X *'                    
         DC    AL1(L'UIDLENMS-1)                                                
UIDLENMS DC    C'* ERROR * USERID CAN''T BE MORE THAN 8 CHARACTERS *'           
         DC    AL1(L'SRCLENMS-1)                                                
SRCLENMS DC    C'* ERROR * SOURCE CAN''T BE MORE THAN 4 CHARACTERS *'           
         DC    AL1(L'MOSERMS-1)                                                 
MOSERMS  DC    C'* ERROR * ENTER MOS MO/YR OR MOMYR *'                          
         DC    AL1(L'SORTMS-1)                                                  
SORTMS   DC    C'* ERROR * SORT=STA/SRC *'                                      
         DC    AL1(L'FTRLENMS-1)                                                
FTRLENMS DC    C'* ERROR * ENTER AT LEAST 2 CHARACTERS OF CODE *'               
         LTORG                                                                  
NUMLINS  EQU   16                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* ON ENTRY: R1 EXPECTED TO ADDRESS BINARY UID                                   
* ON EXIT: SVCTRY FLAG SET TO EITHER 'U' - US OR 'C' - CANADA                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
CHKCTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),0(R1)                                                
         LH    R4,=AL2(MYIOAREA-SYSD)                                           
         AR    R4,R9                                                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'CTFILE ',KEY,(R4)                    
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CTIKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD, SOMETHING VERY WRONG           
*                                                                               
         LR    R6,R4                                                            
         USING CTAGYD,R6                                                        
         MVI   ELCODE,CTAGYELQ     AGY ALPHA ID ELEM                            
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CTAGYID                                                 
         LH    R4,=AL2(MYIOAREA-SYSD)                                           
         AR    R4,R9                                                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'CTFILE ',KEY,(R4)                    
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CT5KEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                NO AGY REC, SOMETHING VERY WRONG             
*                                                                               
         LR    R6,R4                                                            
         USING CTAGDD,R6                                                        
         MVI   ELCODE,CTAGDELQ                                                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO AGY ELEM                                  
*                                                                               
         MVI   SVCTRY,C'U'                                                      
         TM    CTAGDCTY,X'08'                                                   
         BZ    *+8                                                              
         MVI   SVCTRY,C'C'                                                      
*                                                                               
CHKCTRYX J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* EZBLOCK                                                                       
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFE4D                                                                      
       ++INCLUDE SPEZFE4D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
         EJECT                                                                  
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE DMWRKFL                                                        
         EJECT                                                                  
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
* DSECT FOR THIS PROGRAM *                                                      
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
DUB2     DS    D                                                                
RELO     DS    A                                                                
SVRC     DS    A                                                                
*                                                                               
SVMEMSIZ DS    F                                                                
*                                                                               
SVITBACT DS    F                                                                
SVITBDUE DS    F                                                                
AMED     DS    F                                                                
         SPACE                                                                  
         SPACE                                                                  
* COUNTS OF INDEX READS AND WORKER REC READS  *                                 
* ONLY A COUNT OF CALLS, DOESN'T INCLUDE REAL *                                 
* INDEX READS OR SECOND RECORD GETS IN EZMOD  *                                 
         SPACE                                                                  
WKRCTS   DS    0F                                                               
IDXRDCT  DS    F                                                                
RECRDCT  DS    F                                                                
BATSRD   DS    F                                                                
CTSTAS   DS    F                                                                
*                                                                               
ATOT     DS    0F                  AGENCY TOTALS                                
AINV     DS    F                                                                
AINVC    DS    F                                                                
AINVO    DS    F                                                                
AINVU    DS    F                                                                
AINVD    DS    F                                                                
ASPTS    DS    F                                                                
ABAT     DS    F                                                                
ATOTLQ   EQU   *-ATOT                                                           
*                                                                               
ANDOLS   DS    PL8                                                              
AGDOLS   DS    PL8                                                              
*                                                                               
JTOT     DS    0F                  JOB TOTALS                                   
JINV     DS    F                                                                
JINVC    DS    F                                                                
JINVO    DS    F                                                                
JINVU    DS    F                                                                
JINVD    DS    F                                                                
JSPTS    DS    F                                                                
JBAT     DS    F                                                                
JTOTLQ   EQU   *-JTOT                                                           
*                                                                               
JNDOLS   DS    PL8                                                              
JGDOLS   DS    PL8                                                              
*                                                                               
MAXUID   DS    H                   COUNT OF MAX UIDS FOR ANY STATION            
*                                                                               
CURWKIXD DS    CL16                CURRENT WORKER INDEX                         
*                                                                               
SVWCMNT  DS    0CL16               SAVED WORKER RECORD COMMENT AREA             
SVWCSTAT DS    XL1                 STATUS                                       
*              X'40'               ALL INV CONVERTED  OR DELETED                
SVWCPDAT DS    XL3                 PROCESSED DATE                               
SVWCPTIM DS    XL2                 PROCESSED TIME-FORCED 0100                   
SVWCICNT DS    XL2                 INV CT - NOT USED YET                        
SVWCPCNT DS    XL2                 PROCESSED INV CT - NOT USED YET              
SVWCSRCE DS    CL4                                                              
         DS    CL2                 SPARE                                        
*                                                                               
SVCIADDR DS    XL2                                                              
*                                                                               
RQSTA    DS    CL5                 REQUESTED STATION                            
RQDTES   DS   0XL4                           BATCH DATE                         
RQDTSTR  DS    CL2                           BATCH DATE START                   
RQDTEND  DS    CL2                           BATCH DATE END                     
RQSEQ    DS    CL4                           BATCH SEQ                          
RQBSEQ   DS    XL2                                                              
*                                                                               
* INFO ON LAST INVOICE PROCESSED - *                                            
*                                                                               
SVWKFILN DS    XL2                         SEQUENCE NUMBER                      
SVWKSTAT DS    XL1                         STATUS                               
SVWKDTEC DS    XL3                         DATE                                 
SVINVSEQ DS    XL2                         INVOICE SEQ WITHIN BATCH             
*                                                                               
SVWEZIND DS    CL42                                                             
*                                                                               
SVSTA    DS    CL5                                                              
SVSRCE   DS    CL4                                                              
SVSTAMED DS    CL1                 MEDIA AS ARRIVED ON TAPE                     
SVSTADDR DS    CL30                                                             
SVUID    DS    XL2                                                              
SVCTRY   DS    C                                                                
SVLASTID DS    XL2                                                              
SVSNMED  DS    CL1                 MEDIA CHANGED TO R, T, X, N, C, S            
SVSNBND  DS    CL1                                                              
PROCESS  DS    CL1                                                              
*                                                                               
ORIGSTA  DS    CL5                 CURRENT BATCH BEFORE CHANGES                 
*                                                                               
FILTERS  DS    0CL(FILTERND-FUID)                                               
FUID     DS    CL8                                                              
FUIDNUM  DS    XL2                 USER ID (DDS TERMS ONLY)                     
FTRQCLT  DS    CL3                 CLIENT CODE                                  
FTRQPRD  DS    CL3                 PRODUCT CODE                                 
FTRINVNO DS    CL10                INVOICE NO                                   
FTRINVLN DS    XL1                            LENGTH                            
FTRSRCE  DS    CL4                                                              
FTRDATE  DS    XL3                 ACTIVITY DATE                                
FTRDATES DS    CL1                                                              
FTRBSDT  DS    XL2                 BATCH DATE START                             
FTRBEDT  DS    XL2                 BATCH DATE END                               
FTRMOS   DS    CL6                 MONTH OF SERVICE DATE                        
FTRMEDIA DS    CL1                 MEDIA                                        
FTRALL   DS    CL1                 Y=COUNT SJR/IRNY/WWNY                        
FTRFLAG  DS    XL1                                                              
FTRUCVQ  EQU   X'80'               FILTER ON UNCONVERTED                        
FTRCVQ   EQU   X'40'                         CONVERTED                          
FTRRCVQ  EQU   X'20'                         RECONVERTS                         
FTROVR   EQU   X'10'                         OVERRIDES                          
FTRDEL   EQU   X'08'                         DELETES                            
FTRDONE  EQU   X'04'                         DONE - KEEP STATUS FILES           
FTRTEST  EQU   X'02'                         TEST - ONLY READ 500 BATS          
FTRSTAD  EQU   X'01'                         PRINT STATION ADDR                 
FTRSORT  DS    CL1                 SORT = (S)STA (R)SOURCE                      
FTRDDS   DS    CL1                 Y = DDS ONLY, N = NONDDS ONLY                
FTRTRCE  DS    CL1                 Y = TRACE INDEX READS                        
FTRDOWN  DS    CL1                 Y = DOWNLOAD REPORT FORMAT                   
HOLDSIGN DS    CL1                                                              
FTRFLAG2 DS    XL1                                                              
FTR2CAN  EQU   X'80'               ONLY CANADIAN AGENCIES                       
FILTERND EQU   *                                                                
*                                                                               
TINVCTS  DS    0F                                                               
TINV     DS    F                                                                
TINVC    DS    F                   CONVERTED                                    
TINVO    DS    F                   OVERRIDE                                     
TINVU    DS    F                   UNCONV                                       
TINVD    DS    F                   DEL                                          
TINVSPT  DS    F                                                                
TINVLQ   EQU   *-TINVCTS                                                        
*                                                                               
TINVNDOL DS    PL8                                                              
TINVGDOL DS    PL8                                                              
*                                                                               
MYIOAREA DS    2000X                                                            
*                                                                               
WRKFBUFR DS    0D                                                               
         DS    14336X                                                           
WRKFEND  EQU   *                                                                
*                                                                               
STAENTD  DSECT                                                                  
STAENT   DS    0CL10                                                            
STAMED   DS    CL1                                                              
STASTA   DS    CL5                                                              
STASRCE  DS    CL4                                                              
         ORG   STASTA                                                           
STARSRCE DS    CL4                                                              
STARSTA  DS    CL5                                                              
*                                                                               
STABAT   DS    XL4                                                              
*                                                                               
STAINV   DS    XL4                                                              
STAINVC  DS    XL4                                                              
STAINVO  DS    XL4                                                              
STAINVU  DS    XL4                                                              
STAINVD  DS    XL4                                                              
STASPTS  DS    XL4                                                              
*                                                                               
STANDOLS DS    PL8                                                              
STAGDOLS DS    PL8                                                              
*                                                                               
STAUIDS  DS    XL60                                                             
STANEXT  EQU   *                                                                
STAENTL  EQU   *-STAENT                                                         
*                                                                               
SRCENTD  DSECT                                                                  
SRCENT   DS   0CL(SRCNEXT-SRCSRC)                                               
SRCSRC   DS    CL4                                                              
SRCINV   DS    XL4                                                              
SRCINVC  DS    XL4                                                              
SRCINVO  DS    XL4                                                              
SRCINVU  DS    XL4                                                              
SRCINVD  DS    XL4                                                              
SRCSPTS  DS    XL4                                                              
*                                                                               
SRCNDOLS DS    PL8                                                              
SRCGDOLS DS    PL8                                                              
*                                                                               
SRCBAT   DS    XL4                                                              
SRCNEXT  EQU   *                                                                
SRCENTL  EQU   *-SRCENTD                                                        
*                                                                               
*                                                                               
MEDENTD  DSECT                                                                  
MEDENT   DS   0CL(MEDNEXT-MEDMED)                                               
MEDMED   DS    CL1                                                              
MEDINV   DS    XL4                                                              
MEDINVC  DS    XL4                                                              
MEDINVO  DS    XL4                                                              
MEDINVU  DS    XL4                                                              
MEDINVD  DS    XL4                                                              
MEDSPTS  DS    XL4                                                              
*                                                                               
MEDNDOLS DS    PL8                                                              
MEDGDOLS DS    PL8                                                              
*                                                                               
MEDBAT   DS    XL4                                                              
MEDNEXT  EQU   *                                                                
MEDENTL  EQU   *-MEDENTD                                                        
*                                                                               
*                                                                               
AGYENTD  DSECT                                                                  
AGYNM    DS    CL28                                                             
AGYENTID DS    CL2                                                              
AGYNEXT  EQU   *                                                                
AGYENTL  EQU   *-AGYENTD                                                        
*                                                                               
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PSTATN   DS    CL8                                                              
         DS    CL1                                                              
PMED     DS    CL3                                                              
         DS    CL1                                                              
PSRCE    DS    CL4                                                              
         DS    CL2                                                              
PINVS    DS    CL8                 INVOICES - TOTAL                             
         DS    CL2                                                              
PINVC    DS    CL8                 CONVERTED                                    
         DS    CL2                                                              
PINVO    DS    CL8                 OVERRIDES                                    
         DS    CL2                                                              
PINVU    DS    CL8                 UNCONVERTED                                  
         DS    CL2                                                              
PINVD    DS    CL8                 DELETED                                      
         DS    CL2                                                              
PSPTS    DS    CL8                                                              
         DS    CL4                                                              
PNDOL    DS    CL16                                                             
         DS    CL4                                                              
PGDOL    DS    CL16                                                             
         DS    CL5                                                              
PBAT     DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101SPEZF14   03/02/09'                                      
         END                                                                    
