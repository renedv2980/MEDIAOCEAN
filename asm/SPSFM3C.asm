*          DATA SET SPSFM3C    AT LEVEL 064 AS OF 05/01/02                      
*PHASE T2173CA                                                                  
*INCLUDE SPDSTCAL                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T2173C - INFO COMMERCIAL BUY PROGRAM                        *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SCREENS SCSFM9C  (T2179C) -- MAINTENANCE                   *         
*                                                                     *         
*  OUTPUTS: UPDATED INFO BUY RECORDS - ELEMENT 72 OF BUY RECORD       *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - INFO RECORD                                           *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T2173C INFOMERCIAL BUY MAINTENANCE'                             
T2173C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2173C*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD           GENERAL PRINT AREAS                         
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'N'       DO *NOT* MONITOR ACTIVITY                    
         OI    CONSERVH+6,X'81'                                                 
*                                                                               
         CLI   XFRCALL,C'Y'        TEST XFR CONTROL                             
         BNE   PR2                                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',SBUMEDIH,,GLVSPMD                             
         GOTO1 (RF),(R1),,SBUCLTH,,GLVSPCLT                                     
         GOTO1 (RF),(R1),,SBUPRODH,,GLVSPPRD                                    
         GOTO1 (RF),(R1),,SBUESTH,,GLVSPEST                                     
         GOTO1 (RF),(R1),,SBUSTATH,,GLVSPSTA                                    
         GOTO1 (RF),(R1),,SBUBUYLH,,GLVSPBUY                                    
*                                                                               
PR2      LA    R2,PFTABLE                                                       
         GOTO1 INITPFKY,DMCB,(R2)                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,VR                                                            
         BAS   RE,DR                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+8                                                              
         BAS   RE,DR                                                            
* LIST FUNCTION NOT AVAILABLE NOW                                               
*        CLI   MODE,DISPKEY        DISPLAY KEY                                  
*        BNE   *+8                                                              
*        BAS   RE,DK                                                            
         XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       NTR1                                                                   
* CLEAR KEY DESCRIPTION FIELDS                                                  
         TWAXC SBUESTNH,SBUESTNH,PROT=Y                                         
         TWAXC SBUSTADH,SBUSTADH,PROT=Y                                         
         TWAXC SBUPRDDH,SBUPRDDH,PROT=Y                                         
         TWAXC SBUSTANH,SBUSTANH,PROT=Y                                         
         LA    R4,SAVEKEY                                                       
         USING BUYREC,R4                                                        
         XC    SAVEKEY,SAVEKEY                                                  
         LA    R2,SBUMEDIH         AGENCY/MEDIA                                 
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         MVC   BUYKAM,BAGYMD      AGENCY/MEDIA                                  
         LA    R2,SBUCLTH           CLIENT                                      
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         MVC   BUYKCLT,BCLT                                                     
         L     R6,AIO              POINT TO CLTHDR                              
         USING CLTHDRD,R6                                                       
         TM    COPT1,COP1INFQ        CHECK INFOMERCIAL BIT                      
         BO    VK03                                                             
         DROP  R6                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** Client not an Infomercial client **'           
         GOTO1 ERREX2                                                           
*                                                                               
VK03     LA    R2,SBUPRODH                                                      
         CLC   SBUPROD,=C'POL'     DON'T ACCEPT PRODUCT POL                     
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
         GOTO1 ANY                                                              
         GOTO1 VALIPRD                                                          
         MVC   BUYKPRD,BPRD        PRODUCT                                      
         MVC   SBUPRDD,PRDNM      PRODUCT DESCRIPTION                           
         FOUT  SBUPRDDH                                                         
*                                                                               
         LA    R2,SBUESTH                                                       
         GOTO1 ANY                                                              
         GOTO1 VALIEST                                                          
         MVC   BUYKEST,BEST         ESTIMATE                                    
         MVC   SBUESTN,ESTNM        ESTIMATE NAME                               
         FOUT  SBUESTNH                                                         
*                                                                               
         LA    R2,SBUSTATH          STATION                                     
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         MVC   BUYMSTA,BMKTSTA       PACKED MARKET/STATION                      
         MVC   SBUSTAN,QMKT          MARKET NUMBER                              
         OI    SBUSTANH+6,X'80'                                                 
         OI    SBUSTANH+4,X'08'                                                 
         OI    SBUSTANH+5,4                                                     
         L     R6,AIO                                                           
         USING STAREC,R6                                                        
         MVC   TIMEFLAG,SFLAG1       DAYLIGHT SAVINGS TIME FLAG                 
         DROP  R6                                                               
*                                                                               
         LA    R2,SBUSTANH                                                      
         GOTO1 VALIMKT                                                          
         L     R6,AIO                                                           
         LA    R6,500(R6)           MARKET RECORD                               
         USING MKTREC,R6                                                        
         MVC   TIMEDISP,MKTZONE      TIME DISPLACEMENT FROM EST                 
         DROP  R6                                                               
         MVC   SBUSTAD,MKTNM         MARKET NAME                                
         FOUT  SBUSTADH                                                         
* VALIDATE BUY DETAIL FIELD                                                     
VK05     LA    R2,SBUBUYLH                                                      
         GOTO1 ANY                                                              
* SEE IF BUY DETAIL FIELD IS BETWEEN 1 AND 255                                  
         TM    4(R2),X'08'                                                      
         BNZ   *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     ERRX                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'1'                                                         
         BL    VKERR                                                            
         CH    R1,=H'255'                                                       
         BH    VKERR                                                            
* BUY - POL BRAND                                                               
         STC   R1,BUYKBUY+2    LINE NUMBER - BYTE 12                            
         MVI   BUYKBUY+1,X'00' BYTE 11                                          
         MVI   BUYKBUY,X'FF'   BYTE 10                                          
         MVC   KEY,SAVEKEY                                                      
         GOTO1 READ                                                             
*                                                                               
* VALIDATE THAT THERE IS EXACTLY 1 X'0B' ELEMENT IN RECORD                      
VK10     GOTO1 GETREC                                                           
         MVI   ELCODE,X'0B'                                                     
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    VK20                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'** Spot not found **'                             
         GOTO1 ERREX2                                                           
*                                                                               
VK20     BAS   RE,NEXTEL                                                        
         BNE   VKEND                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'** Multiple Spots **'                             
         GOTO1 ERREX2                                                           
         B     VKEND                                                            
*                                                                               
VKERR    MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
VKEND    MVC   KEY,SAVEKEY                                                      
         DROP  R4                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* CODE TO VALIDATE BUY RECORD                                                   
*                                                                               
VR       NTR1                                                                   
         BAS   RE,MISSED    CHECK FOR PREEMPTED, MADE GOOD                      
*                                                                               
* VALIDATE THE ACTUAL DATA LINE. THE AFFIDAVIT ELEMENT - X'10'                  
* MUST FOLLOW THE '0B' ELEMENT.                                                 
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         USING BUYREC,R4                                                        
         L     R3,AIO                                                           
         MVI   ELCODE,X'0B'                                                     
         USING REGELEM,R3                                                       
         BAS   RE,GETEL                                                         
         BE    VR01              MUST BE ONE 0B ELEMENT                         
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(12),=C'** NO BUY **'                                     
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
VR01     CLI   MISSFLG,1         IS IT A MISSED BUY?                            
         BNE   VR02A                                                            
         LA    R2,SBUADATH                                                      
         CLI   5(R2),0  SHOULD BE NO ACTUAL DATA FOR MISSED BUYS                
         BE    VR02                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'** No actual data for missed buys **'             
         GOTO1 ERREX2                                                           
*                                                                               
VR02     LA    R2,SBUATIMH                                                      
         CLI   5(R2),0  SHOULD BE NO ACTUAL DATA FOR MISSED BUYS                
         BE    VR30      SKIP ACTUAL LINE AND AFFADAVIT PROCESSING              
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'** No actual data for missed buys **'             
         GOTO1 ERREX2                                                           
*                                                                               
* IF NOT A MISSED BUY, THE ACTUAL LINE FIELDS ARE NOT REQUIRED                  
*                                                                               
VR02A    LA    R2,SBUADATH                                                      
*                                                                               
* DELETE AFFIDAVIT ELEMENT                                                      
         L     R3,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   VR02B                                                            
         GOTO1 RECUP,DMCB,(0,AIO),(R3)                                          
*                                                                               
VR02B    CLI   5(R2),0             IS THERE ANY INPUT                           
         BE    VR30                NOPE                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING AFFELEM,R3                                                       
         MVI   ACODE,X'10'                                                      
         MVI   ALEN,X'06'                                                       
         LA    R2,SBUADATH                 ACTUAL DATE FIELD                    
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(1,SBUADAT),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
* FILL IN YEAR                                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK2) YMD BINARY                        
         MVC   WORK2(1),BDSTART     Y FROM START DATE                           
         CLC   WORK2(3),BDSTART                                                 
         BH    *+10                                                             
         MVC   WORK2(1),BDEND       Y FROM END DATE                             
         GOTO1 DATCON,DMCB,(3,WORK2),(2,ADATE) COMPRESSED FORMAT                
*                                                                               
* VALIDATE ACTUAL TIME FIELD                                                    
VR20     LA    R2,SBUATIMH                 ACTUAL TIME FIELD                    
         GOTO1 ANY           IF DATE FILLED IN, ALSO WANT TIME                  
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),DUB                                     
         CLI   DMCB,X'FF'                                                       
         BE    TIMERR                                                           
         CLC   DUB(4),=C'NONE'                                                  
         BE    TIMERR                                                           
         CLC   DUB+2(2),=2X'00'                                                 
         BE    VR25                                                             
*                                                                               
TIMERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
VR25     MVC   ATIME,DUB       TIME - 2 BYTES                                   
* INSERT AFFID AFTER 0B ELEMENT                                                 
         L     R3,AIO                                                           
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0               POINT BEYOND X'0B' ELEMENT                   
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)                                     
         DROP  R4                                                               
*                                                                               
* STORE PROTECTED FIELD COUNTS BEFORE DELETION OF ELEMENT                       
*                                                                               
VR30     L     R3,AIO                                                           
         XC    COUNTS,COUNTS                                                    
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         BNE   VR33                                                             
         USING INFOELEM,R3                                                      
         MVC   COUNTS(24),INFOCTRS                                              
         DROP  R3                                                               
*                                                                               
* DELETE 72 ELEMENT                                                             
VR33     L     R3,AIO                                                           
         MVI   ELCODE,X'72'       INFO BUY ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   VR35                                                             
         SPACE 1                                                                
         GOTO1 RECUP,DMCB,(0,AIO),(R3)                                          
         XC    ELEM,ELEM                                                        
*                                                                               
VR35     LA    R4,ELEM                                                          
         USING INFOELEM,R4                                                      
         MVI   INFOELEM,X'72'                                                   
         MVI   INFOELEM+1,36                                                    
*                                                                               
* BUY STATUS FIELD                                                              
* P = PRE EMPT, M = MAKE GOOD, C = CONFIRMED, BLANK = NEW                       
         L     R3,AIO                                                           
         MVI   ELCODE,X'0B'         POOL ORIGINAL                               
         USING REGELEM,R3                                                       
         BAS   RE,GETEL                                                         
         BE    VR45                                                             
* IS FOLLOWING ELEMENT A X'10'?                                                 
VR40     LR    R6,R3              LAST ELEMENT                                  
         ZIC   R0,1(R6)           LENGTH                                        
         AR    R6,R0              GET TO NEXT ELEMENT                           
         MVI   INFOSTAT,C'C'       C= CONFIRMED                                 
         CLI   0(R6),X'10'        IS IT AFFADAVIT ELEMENT                       
         BE    VR50                                                             
         MVI   INFOSTAT,C' '       BLANK = NEW BUY                              
         B     VR50                                                             
* LOOK UP, FILL IN THE BUY STATUS.                                              
* IF RSTATUS = X'40' THEN P, IF RSTATUS = X'42' THEN M, IF                      
* FOLLOWING ELEMENT IS X'10', THEN C                                            
* P = PRE EMPT, M = MAKE GOOD, C = CONFIRMED                                    
*                                                                               
VR45     TM    RSTATUS,X'42'     MAKE GOOD                                      
         BNO   *+12                                                             
         MVI   INFOSTAT,C'M'     MAKE GOOD                                      
         B     VR50                                                             
         TM    RSTATUS,X'40'     SPOT HAS BEEN MINUSED                          
         BZ    VR40                                                             
         MVI   INFOSTAT,C'P'    PREEMPT                                         
         DROP  R3                                                               
*                                                                               
* TYPE CAN BE R,G,P,N,K, OR BLANK WITH DEFAULT OF R                             
VR50     LA    R2,SBUBTYPH        BUY TYPE                                      
         MVI   INFOTYPE,C'R'      DEFAULT TO REGULAR                            
         CLI   5(R2),0                                                          
         BE    VR60                                                             
         CLI   8(R2),C'R'         R= REGULAR                                    
         BE    VR60                                                             
         MVI   INFOTYPE,C'G'      G = GUARANTEE                                 
         CLI   8(R2),C'G'                                                       
         BE    VR60                                                             
         MVI   INFOTYPE,C'P'      P = PER INQUIRY                               
         CLI   8(R2),C'P'                                                       
         BE    VR60                                                             
         MVI   INFOTYPE,C'N'      N = NO CHARGE                                 
         CLI   8(R2),C'N'                                                       
         BE    VR60                                                             
         MVI   INFOTYPE,C'K'      K = PACKAGE                                   
         CLI   8(R2),C'K'                                                       
         BE    VR60                                                             
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
                                                                                
*                                                                               
* FILL IN DATE AND TIME FROM WITH ADJUSTED VALUES                               
VR60     MVC   INFODATE,ADJDATE    STD DATE FIELD - ADJUSTED                    
         MVC   INFOTIMS,ADJTIMS    STD START TIME FIELD - ADJUSTED EST          
         MVC   INFOTIME,ADJTIME    STD END TIME FIELD -  ADJUSTED EST           
*                                                                               
* VALIDATE RESPONSE DATA                                                        
VR80     LA    R6,INFOCTRS         START OF RESPONSE DATA                       
         MVC   INFOCTRS(2),COUNTS  CREDIT CARDS SUM                             
*                                                                               
VR90     LA    R2,SBUCHECH         CHECK/DEBIT                                  
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         BAS   RE,CONVERT                                                       
         STH   R5,2(R6)                                                         
*                                                                               
VR100    LA    R2,SBUCODH          COD                                          
         CLI   5(R2),0                                                          
         BE    VR110                                                            
         BAS   RE,CONVERT                                                       
         STH   R5,4(R6)                                                         
*                                                                               
VR110    LA    R2,SBUPROMH         PROMISED                                     
         CLI   5(R2),0                                                          
         BE    VR120                                                            
         BAS   RE,CONVERT                                                       
         STH   R5,6(R6)                                                         
*                                                                               
VR120    LA    R2,SBUNUM9H         900 NUMBER                                   
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         BAS   RE,CONVERT                                                       
         STH   R5,8(R6)                                                         
*                                                                               
VR130    LA    R2,SBUATTTH         ATT TRKING                                   
         CLI   5(R2),0                                                          
         BE    VR140                                                            
         BAS   RE,CONVERT                                                       
         STH   R5,10(R6)                                                        
*                                                                               
VR140    MVC   INFOCTRS+12(12),COUNTS+12 SUM OF LEADS,UPSELLS 1-5               
*                                                                               
*                                                                               
         LA    R2,SBUCHEKH         BUYER CHECKING                               
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         CLI   SBUCHEK,C'Y'                                                     
         BE    VR150                                                            
         CLI   SBUCHEK,C'N'                                                     
         BE    VR150                                                            
         CLI   SBUCHEK,C'A'                                                     
         BE    VR150                                                            
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
VR150    MVC   INFOCHEK,SBUCHEK    BUYER CHECKING                               
*                                                                               
VR160    OC    ELEM+2(34),ELEM+2                                                
         BZ    VREND                                                            
* INSERT AFTER 0B OR 10 ELEMENT                                                 
         L     R3,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    VR162                                                            
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VR162    DS    0H                  CAREFUL TO PUT IT ** AFTER                   
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)                                     
         DROP  R4                                                               
*                                                                               
VREND    MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CLEAR OUT SCREEN INPUT                                                        
* GET ELEMENT DATA                                                              
DR       NTR1                                                                   
         TWAXC SBUDATAH,SBUDATAH,PROT=Y  CLEAR BUY DATA LINE                    
         TWAXC SBUADATH                  CLEAR ALL DATA FROM SCREEN             
         TWAXC SBUADAYH,SBUADAYH,PROT=Y  CLEAR PROTECTED DAY FIELD              
         TWAXC SBUAMISH,SBUAMISH,PROT=Y  CLEAR PROTECTED MISSED FIELD           
         TWAXC SBUSDATH,SBUSDATH,PROT=Y  CLEAR STD DATE FIELD                   
         TWAXC SBUSTIMH,SBUSTIMH,PROT=Y  CLEAR STD TIME FIELD                   
         TWAXC SBUCREDH,SBUCREDH,PROT=Y  CLEAR CREDIT CARD FIELD                
         TWAXC SBULEADH,SBULEADH,PROT=Y  CLEAR LEADS FIELD                      
         TWAXC SBUUPS1H,SBUUPS1H,PROT=Y  CLEAR UPSELL FIELD                     
         TWAXC SBUUPS2H,SBUUPS2H,PROT=Y  CLEAR UPSELL FIELD                     
         TWAXC SBUUPS3H,SBUUPS3H,PROT=Y  CLEAR UPSELL FIELD                     
         TWAXC SBUUPS4H,SBUUPS4H,PROT=Y  CLEAR UPSELL FIELD                     
         TWAXC SBUUPS5H,SBUUPS5H,PROT=Y  CLEAR UPSELL FIELD                     
         XC    ADJFLDS,ADJFLDS                                                  
*                                                                               
         BAS   RE,MISSED            CHECK FOR PREEMPTED, MADE GOOD              
*                                                                               
         L     R3,AIO               RECORD AREA                                 
         USING BUYREC,R3            UPSELL CATEGORY NAMES ELEMENT               
*                                                                               
* SEE IF BUY RECORD IS MARKED FOR DELETION                                      
         TM    BUYRCNTL,X'80'        IS DELETE BIT ON                           
         BZ    DR05                  NO, THEN DISPLAY                           
         LA    R2,SBUBUYLH           POSITION CURSOR                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'** Buy record deleted **'                         
         GOTO1 ERREX2                                                           
*                                                                               
* NEED THE BUY DATA FOR STD DATE AND TIME CONVERSION LATER ON                   
DR05     MVC   DSTDATE,BDSTART      FOR STD DATE CONVERSION                     
         MVC   STRTIME,BDTIMST      FOR STD. TIME CONV.                         
         MVC   ENDTIME,BDTIMEND     FOR STD. TIME CONV.                         
*                                                                               
* DISPLAY THE BUY DATA LINE                                                     
* DISPLAY START DATE                                                            
*                                                                               
         LA    R4,SBUDATA                                                       
         GOTO1 DATCON,DMCB,(3,BDSTART),(7,(R4))                                 
         MVI   5(R4),C'-'                                                       
         MVI   6(R4),C'E'                                                       
         CLI   BDINPUT,2           TEST INPUT METHOD                            
         BH    DR10                -E                                           
         BL    DR8                 WEEKS                                        
* MUST BE END DATE                                                              
         GOTO1 (RF),(R1),(3,BDEND),(7,6(R4))                                    
         B     DR10                                                             
*                                                                               
DR8      SR    R0,R0                                                            
         IC    R0,BDWKS                                                         
         EDIT  (R0),(2,6(R4)),ALIGN=LEFT                                        
         LA    RE,6(R4)                                                         
         AR    RE,R0                                                            
         MVI   0(RE),C'W'                                                       
         CLI   BDWKIND,C'O'                                                     
         BE    DR10                                                             
         MVC   1(1,RE),BDWKIND                                                  
*                                                                               
DR10     MVI   11(R4),C'*'                                                      
*                                                                               
* DISPLAY DAYS                                                                  
         LA    R4,12(R4)                                                        
         CLI   BDDAY,0                                                          
         BNE   *+8                                                              
         MVI   BDDAY,X'7C'         GET AROUND PROGRAM DUMPS                     
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(BDSEDAY,BDDAY),WORK                                   
         MVC   0(8,R4),WORK                                                     
         MVI   8(R4),C'*'                                                       
* NUMBER PER WEEK                                                               
         LA    R4,9(R4)                                                         
         SR    R0,R0                                                            
         IC    R0,BDNOWK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         MVI   2(R4),C'*'                                                       
* TIME                                                                          
DR11     LA    R4,3(R4)                                                         
         MVC   WORK,SPACES                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),BDTIMST,WORK                                           
         MVC   0(11,R4),WORK                                                    
         MVI   11(R4),C'*'                                                      
* DPT                                                                           
         LA    R4,12(R4)                                                        
         MVC   0(1,R4),BDDAYPT                                                  
         MVI   1(R4),C'*'                                                       
* SPTLEN                                                                        
         LA    R4,2(R4)                                                         
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         CLI   BDSEC,100                                                        
         BL    DR12                                                             
         BCTR  R4,0                                                             
         UNPK  0(3,R4),DUB                                                      
         LA    R4,1(R4)                                                         
DR12     MVI   2(R4),C'*'                                                       
*                                                                               
* PROGRAMMING                                                                   
*                                                                               
         LA    R4,3(R4)                                                         
         MVC   WORK(17),BDPROGRM                                                
DR14     MVC   0(17,R4),WORK                                                    
         MVI   19(R4),C'*'                                                      
* ADJ CODE                                                                      
         LA    R4,20(R4)                                                        
         CLI   SVCPROF+9,C'0'                                                   
         BE    DR20                                                             
         MVC   WORK(1),BDPROGT                                                  
         MVI   WORK+1,C' '                                                      
         CLI   SVCPROF+9,C'1'      TEST ALPHA ADJ                               
         BE    DR16                YES                                          
         SR    R0,R0                                                            
         IC    R0,BDPROGT                                                       
         SRDL  R0,4                                                             
         STC   R0,WORK                                                          
         OI    WORK,X'F0'                                                       
         SRL   R1,28                                                            
         STC   R1,WORK+1                                                        
         OI    WORK+1,X'F0'                                                     
*                                                                               
DR16     SH    R4,=H'4'                                                         
         MVI   0(R4),C'*'                                                       
         MVC   1(2,R4),WORK                                                     
         MVI   3(R4),C'*'                                                       
         LA    R4,4(R4)                                                         
*                                                                               
* COST                                                                          
*                                                                               
DR20     TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    DR20A                                                            
         MVC   WORK2(1),BDCIND                                                  
         B     DR22                                                             
*                                                                               
DR20A    MVI   WORK2,C'C'                                                       
         TM    BDCIND2,X'80'                                                    
         BO    DR22                                                             
         MVI   WORK2,C' '                                                       
         TM    BDCIND,X'20'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'F'                                                       
         TM    BDCIND,X'80'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'Q'                                                       
         TM    BDCIND,X'40'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'N'                                                       
         TM    BDCIND,X'10'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'V'                                                       
         TM    BDCIND,X'08'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'S'                                                       
         TM    BDCIND,X'04'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'X'                                                       
         TM    BDCIND,X'02'                                                     
         BO    DR22                                                             
         MVI   WORK2,C'P'                                                       
*                                                                               
DR22     L     R0,BDCOST                                                        
         SRL   R0,8                                                             
*                                                                               
DR22N    DS    0H                                                               
         LTR   R0,R0                                                            
         BZ    DR22EDT                                                          
         CLI   SVAPROF+7,C'C'      TEST CANAD NTWK                              
         BNE   *+12                                                             
         TM    BDCIND2,X'01'       YES-TEST COST IN PENNIES                     
         BZ    DR22B                   NO                                       
         C     R0,=F'9999999'      IF COST TOO BIG                              
         BH    DR22A                DROP CENTS                                  
*                                                                               
DR22EDT  EDIT  (R0),(8,(R4)),2                                                  
         B     DR22C                                                            
*                                                                               
DR22A    SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
*                                                                               
DR22B    EDIT  (R0),(8,(R4))                                                    
*                                                                               
DR22C    LA    R5,8(R4)                                                         
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    DR22D                                                            
         TM    BDCIND2,BDC2NEG     TEST NEGATIVE COST                           
         BZ    DR24                                                             
         B     DR22E                                                            
DR22D    TM    BDCIND,X'01'        TEST NEGATIVE COST                           
         BZ    *+12                                                             
DR22E    MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
* FLOAT COST CHAR TO LEFT OF COST                                               
*                                                                               
DR24     CLI   WORK2,C' '                                                       
         BE    DR30                                                             
* FIND FIRST BLANK TO LEFT OF COST                                              
         LA    R1,6(R4)                                                         
         LA    R0,7                                                             
         CLI   0(R1),C' '                                                       
         BE    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         MVC   0(1,R1),WORK2       MOVE SPECIAL COST CODE                       
*                                                                               
         FOUT  SBUDATAH             BUY DATA LINE                               
*                                                                               
* DISPLAY ACTUAL DATA LINE                                                      
*                                                                               
DR30     L     R3,AIO                                                           
         CLI   MISSFLG,1                                                        
         BE    DR34              NO ACTUAL DATA FOR MISSED BUY                  
         MVI   ELCODE,X'10'      AFFIDAVIT ELEMENT                              
         USING AFFELEM,R3                                                       
         BAS   RE,GETEL                                                         
         BNE   DR34                                                             
         GOTO1 DATCON,DMCB,(2,ADATE),(4,SBUADAT)  ACTUAL DATE                   
*                                                                               
         FOUT  SBUADATH                           DISPLAY DATE                  
*                                                                               
* DISPLAY THE DAY OF WEEK                                                       
         GOTO1 DATCON,DMCB,(2,ADATE),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,(0,WORK),SBUADAY                                     
         FOUT  SBUADAYH                                                         
* TIME FIELD - ACTUAL                                                           
         OC    ATIME,ATIME                                                      
         BZ    DR34                                                             
         CLC   ATIME,=C'NO'                                                     
         BNE   DR32                                                             
         MVC   SBUATIM,=C'NONE  '                                               
         B     DR33                                                             
*                                                                               
DR32     XC    WORK(4),WORK                                                     
         MVC   WORK(2),ATIME                                                    
         GOTO1 UNTIME,DMCB,WORK,SBUATIM                                         
*                                                                               
DR33     FOUT  SBUATIMH                                                         
* BUY STATUS FIELD                                                              
* P = PRE EMPT, M = MAKE GOOD, C = CONFIRMED, BLANK = NEW                       
DR34     L     R3,AIO                                                           
         MVI   ELCODE,X'0B'         POOL ORIGINAL                               
         USING REGELEM,R3                                                       
         BAS   RE,GETEL                                                         
         BE    DR36                                                             
*                                                                               
* IS FOLLOWING ELEMENT A X'10'?                                                 
DR35     LR    R4,R3              LAST ELEMENT                                  
         ZIC   R0,1(R4)           LENGTH                                        
         AR    R4,R0              GET TO NEXT ELEMENT                           
         MVI   SBUBSTA,C'C'       C= CONFIRMED                                  
         CLI   0(R4),X'10'        IS IT AFFADAVIT ELEMENT                       
         BE    DR37                                                             
         MVI   SBUBSTA,C' '       BLANK = NEW BUY                               
         B     DR37                                                             
* LOOK UP, FILL IN THE BUY STATUS.                                              
* IF RSTATUS = X'40' THEN P, IF RSTATUS = X'42' THEN M, IF                      
* FOLLOWING ELEMENT IS X'10', THEN C                                            
* P = PRE EMPT, M = MAKE GOOD, C = CONFIRMED                                    
*                                                                               
DR36     TM    RSTATUS,X'42'     MAKE GOOD                                      
         BNO   *+12                                                             
         MVI   SBUBSTA,C'M'     MAKE GOOD                                       
         B     DR37                                                             
         TM    RSTATUS,X'40'    SPOT HAS BEEN MINUSED                           
         BZ    DR35                                                             
         MVI   SBUBSTA,C'P'    PRE EMPT                                         
         DROP  R3                                                               
DR37     FOUT  SBUBSTAH        PUT OUT STATUS BYTE TO SCREEN                    
*                                                                               
* SET UP CALL TO DSTCALC                                                        
*                                                                               
DR38     XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING DSTBLKD,R4                                                       
         MVC   DSTGTDAY,GETDAY                                                  
         GOTO1 DATCON,DMCB,(3,DSTDATE),(0,DSTIDATE)  YYMMDD                     
         MVC   DSTISTTM,STRTIME  START TIME TO BE CONVERTED                     
         MVC   DSTINDTM,ENDTIME  END TIME TO BE CONVERTED                       
         MVC   DSTADDAY,ADDAY   ADDAY'S ADDRESS                                 
         MVC   DSTTIMEV,TIMEDISP  DISP FROM ESTTIME                             
* CHECK DST TIME FLAG                                                           
*                                                                               
         MVI   DSTFLG,C'Y'                                                      
         TM    TIMEFLAG,X'40' IF TRUE, STATION DOES GO ON DST TIME              
         BZ    *+8                                                              
         MVI   DSTFLG,C'N'                                                      
*                                                                               
         GOTO1 =V(DSTCALC),DMCB,(R4),RR=RELO                                    
         CLI   DSTERR,0                                                         
         BE    DR40                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'** Error ** Date not in DST Calendar'             
         GOTO1 ERREX2                                                           
*                                                                               
DR40     GOTO1 DATCON,DMCB,(0,DSTODATE),(11,SBUSDAT)                            
         FOUT  SBUSDATH             STD DATE                                    
*                                                                               
* SAVE ADJUSTED DATE IN BINARY YMD                                              
         GOTO1 DATCON,DMCB,(0,DSTODATE),(3,ADJDATE)                             
         MVC   ADJTIMS,DSTOSTTM   SAVE ADJUSTED START TIME                      
         MVC   ADJTIME,DSTONDTM   SAVE ADJUSTED END TIME                        
         GOTO1 UNTIME,DMCB,DSTOSTTM,SBUSTIM                                     
         FOUT  SBUSTIMH                                                         
         DROP  R4                                                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'72'   UPSELL NAMES ELEMENT                              
         BAS   RE,GETEL                                                         
         USING INFOELEM,R3                                                      
         BE    DR44                                                             
         MVI   SBUBTYP,C'R'  R IS DEFAULT BUY TYPE                              
         FOUT  SBUBTYPH                                                         
         B     DREND                                                            
*                                                                               
DR44     MVC   SBUBTYP,INFOTYPE     BUY TYPE                                    
         FOUT  SBUBTYPH                                                         
*                                                                               
* DISPLAY RESPONSE DATA LINES. THE CREDIT CARDS, LEADS, AND UPSELLS             
* 1-5 ARE THE SUM OF THE COUNTS FROM THE X'73' COUNT ELEMENTS.                  
* COUNTS GREATER THAN 9999 ARE DISPLAYED AS ASTERICKS.                          
*                                                                               
         LH    R5,=H'9999'     LARGEST COUNT THAT CAN BE DISPLAYED              
         LA    R4,INFOCTRS     COUNTERS                                         
*                                                                               
         MVC   SBUCRED,=C'****'                                                 
         CH    R5,0(R4)                                                         
         BL    DR50                                                             
         EDIT  (2,0(R4)),(4,SBUCRED),ALIGN=LEFT   SUM OF CREDIT CARDS           
DR50     FOUT  SBUCREDH                                                         
*                                                                               
         EDIT  (2,2(R4)),(4,SBUCHEC),ALIGN=LEFT   CHECK/DEBIT                   
         FOUT  SBUCHECH                                                         
*                                                                               
         EDIT  (2,4(R4)),(4,SBUCOD),ALIGN=LEFT    COD                           
         FOUT  SBUCODH                                                          
*                                                                               
         EDIT  (2,6(R4)),(4,SBUPROM),ALIGN=LEFT   PROMISED                      
         FOUT  SBUPROMH                                                         
*                                                                               
         EDIT  (2,8(R4)),(4,SBUNUM9),ALIGN=LEFT   900 NUMBER                    
         FOUT  SBUNUM9H                                                         
*                                                                               
         EDIT  (2,10(R4)),(4,SBUATTT),ALIGN=LEFT   ATT TRKING                   
         FOUT  SBUATTTH                                                         
*                                                                               
         MVC   SBULEAD,=C'****'                                                 
         CH    R5,12(R4)       SUM OF LEADS                                     
         BL    DR55                                                             
         EDIT  (2,12(R4)),(4,SBULEAD),ALIGN=LEFT  SUM OF LEADS                  
DR55     FOUT  SBULEADH                                                         
*                                                                               
         MVC   SBUUPS1,=C'****'                                                 
         CH    R5,14(R4)       SUM OF UPSELL1 COUNTS                            
         BL    DR60                                                             
         EDIT  (2,14(R4)),(4,SBUUPS1),ALIGN=LEFT  SUM OF UPSELL1 CNTS           
DR60     FOUT  SBUUPS1H                                                         
*                                                                               
         MVC   SBUUPS2,=C'****'                                                 
         CH    R5,16(R4)       SUM OF UPSELL2 COUNTS                            
         BL    DR65                                                             
         EDIT  (2,16(R4)),(4,SBUUPS2),ALIGN=LEFT  SUM OF UPSELL2 CNTS           
DR65     FOUT  SBUUPS2H                                                         
*                                                                               
         MVC   SBUUPS3,=C'****'                                                 
         CH    R5,18(R4)       SUM OF UPSELL3 COUNTS                            
         BL    DR70                                                             
         EDIT  (2,18(R4)),(4,SBUUPS3),ALIGN=LEFT  SUM OF UPSELL3 CNTS           
DR70     FOUT  SBUUPS3H                                                         
*                                                                               
         MVC   SBUUPS4,=C'****'                                                 
         CH    R5,20(R4)       SUM OF UPSELL4 COUNTS                            
         BL    DR75                                                             
         EDIT  (2,20(R4)),(4,SBUUPS4),ALIGN=LEFT  SUM OF UPSELL4 CNTS           
DR75     FOUT  SBUUPS4H                                                         
*                                                                               
         MVC   SBUUPS5,=C'****'                                                 
         CH    R5,22(R4)       SUM OF UPSELL5 COUNTS                            
         BL    DR80                                                             
         EDIT  (2,22(R4)),(4,SBUUPS5),ALIGN=LEFT  SUM OF UPSELL5 CNTS           
DR80     FOUT  SBUUPS5H                                                         
*                                                                               
         MVC   SBUCHEK,INFOCHEK   BUYER CHECKING                                
         FOUT  SBUCHEKH                                                         
*                                                                               
DREND    MVC   AIO,AIO1                                                         
         DROP  R3                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY THE KEY FOR THE LIST FUNCTION                                         
* LIST FUNCTION NOT AVAILABLE NOW                                               
**** THIS HAS NOT BEEN TESTED. REFERENCE SPSFM2F FOR SIMILIAR                   
*** FUNCTION.                                                                   
*DK       NTR1                                                                  
*        L     R4,AIO                                                           
*        USING BUYREC,R4                                                        
*        MVC   ELEM,BUYREC       KEEP THE KEY FIELDS AROUND                     
*        LA    R4,ELEM                                                          
*        MVC   KEY,MYSAVE                                                       
* GET MEDIA FROM AGENCY RECORD BY MATCHING THE CODE                             
*        XC    KEY,KEY                                                          
*        MVI   KEY,X'06'                                                        
*        MVC   KEY+1(2),AGENCY                                                  
*        GOTO1 READ                                                             
*        GOTO1 GETREC                                                           
*        L     R3,AIO                                                           
*        MVI   ELCODE,X'02'                                                     
*        BAS   RE,GETEL                                                         
*        USING AGYMEDEL,R3                                                      
*        CLC   AGYMEDBT,BUYKAM           COMPARE AGENCY/MEDIA CODES             
*        BE    DK02                                                             
*DK01    BAS   RE,NEXTEL                                                        
*        BNE   DKEND                                                            
*        CLC   AGYMEDBT,BUYKAM           COMPARE AGENCY/MEDIA CODES             
*        BNE   DK01                                                             
*DK02     MVC   SBUMEDI,AGYMEDCD          MEDIA                                 
*        FOUT  SBUMEDIH                                                         
*        MVC   BCLT,BUYKCLT              PACKED FORMAT                          
*        GOTO1 CLUNPK,DMCB,BCLT,QCLT     UNPACK CLIENT FOR DISPLAY              
*        MVC   SBUCLT,QCLT                                                      
*        FOUT  SBUCLTH                                                          
* GET PRODUCT FROM CLIENT REC                                                   
* READ CLIENT HEADER *                                                          
*        SPACE 1                                                                
*        XC    KEY,KEY                                                          
*        MVC   KEY+1(1),BAGYMD                                                  
*        MVC   KEY+2(2),BCLT                                                    
*                                                                               
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 READ                                                             
*        L     R6,AIO                                                           
*        USING CLTHDR,R6                                                        
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 GETREC                                                           
*        LA    R5,CLIST                                                         
*DK03     CLC   BUYKPRD,3(R5)    PRODUCT NUMBER                                 
*        BE    DK05                                                             
*        CLI   3(R5),X'00'                                                      
*        BE    DKKEY                                                            
*        LA    R5,4(R5)                                                         
*        B     DK03                                                             
*DK05     MVC   SBUPROD,0(R5)  PRODUCT MNEMONIC                                 
*        FOUT  SBUPRODH                                                         
*        LA    R2,SBUPRODH                                                      
*        GOTO1 VALIPRD                                                          
*        MVC   BUYKPRD,BPRD        PRODUCT                                      
*        MVC   SBUPRDD,PRDNM      PRODUCT DESCRIPTION                           
*        FOUT  SBUPRDDH                                                         
*        EDIT  (1,BUYKEST),(3,SBUEST),ALIGN=LEFT  ESTIMATE NO.                  
*        FOUT  SBUESTH                                                          
*        XC    KEY,KEY                                                          
*        LA    R6,KEY              READ ESTIMATE HEADER                         
*        USING ESTHDR,R6                                                        
*        MVC   EKEYAM,BAGYMD                                                    
*        MVC   EKEYCLT,BCLT                                                     
*        MVC   EKEYPRD,QPRD                                                     
*        MVC   EKEYEST,BUYKEST                                                  
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 READ                                                             
*        GOTO1 GETREC                                                           
*        L     R6,AIO                                                           
*        MVC   SBUESTN,EDESC    ESTIMATE NAME                                   
*        FOUT  SBUESTNH                                                         
*        OC    BUYMSTA,BUYMSTA                                                  
*        BNZ   DKUNPK                                                           
*        MVC   SBUSTAT,=C'ALL'                                                  
*        FOUT  SBUSTATH                                                         
*        B     DKBUYL                                                           
*DKUNPK   XC    WORK,WORK                                                       
*        GOTO1 MSUNPK,DMCB,BUYMSTA,WORK,WORK+10                                 
*        MVC   SBUSTAT,WORK+10                                                  
*        FOUT  SBUSTATH                                                         
*        LA    R2,SBUSTATH                                                      
*        GOTO1 VALISTA                                                          
*        MVC   BUYMSTA(5),BMKTSTA    PACKED MARKET AND STATION FIELD            
*        MVC   SBUSTAN,QMKT          MARKET NUMBER                              
*        OI    SBUSTANH+6,X'80'                                                 
*        OI    SBUSTANH+4,X'08'                                                 
*        OI    SBUSTANH+5,4                                                     
*        LA    R2,SBUSTANH                                                      
*        GOTO1 VALIMKT                                                          
*        MVC   SBUSTAD,MKTNM         MARKET NAME                                
*        FOUT  SBUSTADH                                                         
*        MVC   BYTE,BUYKBUY+2                                                   
*DKBUYL   EDIT  (1,BYTE),(3,SBUBUYL),ALIGN=LEFT  BUY LINE                       
*        FOUT  SBUBUYLH                                                         
*                                                                               
*DKKEY    XC    KEY,KEY                                                         
*        MVC   KEY,MYSAVE                                                       
*        GOTO1 HIGH                                                             
*        MVC   AIO,AIO1                                                         
*        GOTO1 GETREC                                                           
*        DROP  R3,R4,R6                                                         
*DKEND    XIT1                                                                  
         EJECT                                                                  
*                                                                               
* CONVERT CHARACTER STRING TO HEX WITH AN EXECUTED PACK                         
CONVERT  CLI   MISSFLG,1     IS IS A MISSED BUY                                 
         BNE   CONV1                                                            
         B     CONVRES       MUST BE A REPONSE DATA FIELD                       
*                                                                               
CONV1    TM    4(R2),X'08'   IS IT NUMERIC?                                     
         BZ    CONVERR                                                          
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R5,DUB                                                           
         B     CONVEND                                                          
*                                                                               
CONVERR  MVI   ERROR,NOTNUM                                                     
         B     ERRX                                                             
*                                                                               
CONVRES  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** No Response data for Missed buys **'           
         GOTO1 ERREX2                                                           
*                                                                               
CONVEND  BR    RE                                                               
         EJECT                                                                  
* PUT OUT 'MISSED' MESSAGE FOR PREEMPTED, MADE GOOD BUYS                        
*                                                                               
MISSED   NTR1                                                                   
         MVC   AIO,AIO1         BUY RECORD                                      
         MVI   MISSFLG,0                                                        
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'     PACKAGE ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   MISSED1                                                          
         USING PKGELEM,R3                                                       
         CLI   PKGIND,7         IS IT MAKEGOOD MASTER                           
         BE    ERMISS                                                           
*                                                                               
MISSED1  MVI   ELCODE,X'07'     OTO ELEMENT                                     
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   MISSED2                                                          
         USING REGELEM,R3                                                       
         TM    RSTATUS,X'80'    IS IT MINUS SPOT                                
         BO    ERMISS                                                           
*                                                                               
MISSED2  MVI   ELCODE,X'0C'     POOL OTO ELEMENT                                
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   MISSEND                                                          
         USING REGELEM,R3                                                       
         TM    RSTATUS,X'80'    IS IT MINUS SPOT                                
         BNO   MISSEND                                                          
*                                                                               
ERMISS   MVI   MISSFLG,1                                                        
         MVC   SBUAMIS,=C'** MISSED **'                                         
         FOUT  SBUAMISH                                                         
*                                                                               
MISSEND  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                                                                               
         EJECT                                                                  
         GETEL R3,24,ELCODE                                                     
         EJECT                                                                  
RELO     DS    A                                                                
ERRX     GOTO1 ERREX                                                            
ZEROS    DC    20C'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* INFB DISPLAY                                                                  
         DC    AL1(PF06X-*,06,PFTCPROG,(PF06X-PF06)/KEYLNQ,0)                   
         DC    CL3' ',CL8'INFC',CL8'DISPLAY'                                    
PF06     DC    AL1(KEYTYTWA,L'SBUMEDI-1),AL2(SBUMEDI-T217FFD)                   
         DC    AL1(KEYTYTWA,L'SBUCLT-1),AL2(SBUCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'SBUPROD-1),AL2(SBUPROD-T217FFD)                   
         DC    AL1(KEYTYTWA,L'SBUEST-1),AL2(SBUEST-T217FFD)                     
         DC    AL1(KEYTYTWA,L'SBUSTAT-1),AL2(SBUSTAT-T217FFD)                   
         DC    AL1(KEYTYTWA,L'SBUBUYL-1),AL2(SBUBUYL-T217FFD)                   
PF06X    EQU   *                                                                
*                                                                               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPGENNDEF              NETWORK DEFINITION DSECT                
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT               CLIENT DEFINITION DSECT                 
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPDSTBLK   PARMLIST FOR SPDSTCAL                               
         EJECT                                                                  
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
*****                                                                           
       ++INCLUDE SCSFM9CD   MAINT SCREEN                                        
         ORG   CONTAGH                                                          
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
         PRINT ON                                                               
* STORAGE DSECT                                                                 
*                                                                               
         ORG   SYSSPARE                                                         
SAVEKEY  DS    CL24                                                             
WORK2    DS    CL48                                                             
COUNTS   DS    CL24                                                             
ELCDLO   DS    CL1                                                              
ELCDHI   DS    CL1                                                              
GDATE    DS    CL6                                                              
ENDTIME  DS    XL2                                                              
STRTIME  DS    XL2                                                              
DSTDATE  DS    XL3                                                              
ADJFLDS  DS    0CL7                                                             
ADJTIMS  DS    XL2                                                              
ADJTIME  DS    XL2                                                              
ADJDATE  DS    XL3                                                              
MISSFLG  DS    XL1                                                              
TIMEFLAG DS    XL1                                                              
TIMEDISP DS    XL1                                                              
         SPACE 5                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064SPSFM3C   05/01/02'                                      
         END                                                                    
