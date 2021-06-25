*          DATA SET SPSFM3D    AT LEVEL 045 AS OF 05/01/02                      
*PHASE T2173DA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T2173D - INFO COUNT BUY PROGRAM                             *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SCREENS SCSFM9C  (T2179B) -- MAINTENANCE                   *         
*                                                                     *         
*  OUTPUTS: UPDATED INFO COUNTS - ELEMENT 73 OF BUY RECORD            *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - INFO RECORD                                           *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - WORK                                                  *         
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
         TITLE 'T2173D INFOMERCIAL COUNT MAINTENANCE'                           
T2173D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2173D*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD           GENERAL PRINT AREAS                         
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         CLI   XFRCALL,C'Y'        TEST XFR CONTROL                             
         BNE   PR2                                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',SCTMEDIH,,GLVSPMD                             
         GOTO1 (RF),(R1),,SCTCLTH,,GLVSPCLT                                     
         GOTO1 (RF),(R1),,SCTPRODH,,GLVSPPRD                                    
         GOTO1 (RF),(R1),,SCTESTH,,GLVSPEST                                     
         GOTO1 (RF),(R1),,SCTSTATH,,GLVSPSTA                                    
         GOTO1 (RF),(R1),,SCTBUYLH,,GLVSPBUY                                    
*                                                                               
PR2      MVI   ACTELOPT,C'N'       DO *NOT* MONITOR ACTIVITY                    
         OI    CONSERVH+6,X'81'    FOR PFKEY TO WORK                            
*                                                                               
* PFKEYS 7,8,19,20 ARE HANDLED IN THIS PROGRAM. PF6 SWITCHES TO INFB            
* RECORD.                                                                       
*                                                                               
         CLI   PFAID,8                                                          
         BE    MODE10                                                           
         CLI   PFAID,20                                                         
         BE    MODE10                                                           
         CLI   PFAID,7                                                          
         BE    MODE10                                                           
         CLI   PFAID,19                                                         
         BE    MODE10                                                           
         LA    R2,PFTABLE                                                       
         GOTO1 INITPFKY,DMCB,(R2)                                               
*                                                                               
MODE10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,VR                                                            
         BAS   RE,DR                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+8                                                              
         BAS   RE,DR                                                            
         XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       NTR1                                                                   
         LA    R4,KEYAREA                                                       
         USING BUYREC,R4                                                        
         XC    KEYCHNG,KEYCHNG     KEY CHANGE FLAG                              
*                                                                               
* RESET VALIDATE BITS (x'20') WHEN WE SWITCH SCREEN FROM INFB                   
         LA    R2,SCTMEDIH         AGENCY/MEDIA                                 
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BZ    VK03                                                             
         NI    SCTMEDIH+4,X'DF'                                                 
         NI    SCTCLTH+4,X'DF'                                                  
         NI    SCTPRODH+4,X'DF'                                                 
         NI    SCTESTH+4,X'DF'                                                  
         NI    SCTSTATH+4,X'DF'                                                 
         NI    SCTBUYLH+4,X'DF'                                                 
*                                                                               
VK03     TM    4(R2),X'20'         VALIDATED                                    
         BO    VK05                                                             
         MVI   KEYCHNG,1           KEY HAS CHANGED                              
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         MVC   BUYKAM,BAGYMD      AGENCY/MEDIA                                  
*                                                                               
VK05     OI    4(R2),X'20'        VALIDATED                                     
         LA    R2,SCTCLTH         CLIENT                                        
         TM    4(R2),X'20'        VALIDATED                                     
         BO    VK10                                                             
         MVI   KEYCHNG,1          KEY HAS CHANGED                               
         GOTO1 ANY                                                              
* ON RETURN CLTHDR IS IN AIO1                                                   
         GOTO1 VALICLT                                                          
         MVC   BUYKCLT,BCLT                                                     
         L     R3,AIO1                                                          
         USING CLTHDRD,R3                                                       
         TM    COPT1,COP1INFQ        CHECK INFOMERCIAL BIT                      
         BO    VK10                                                             
         DROP  R3                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** Client not an Infomercial client **'           
         GOTO1 ERREX2                                                           
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SCTPRODH         PRODUCT                                      
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK15                                                             
         MVI   KEYCHNG,1           KEY CHANGE FLAG                              
         CLC   SCTPROD,=C'POL'     DON'T ACCEPT PRODUCT POL                     
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
         GOTO1 ANY                                                              
         GOTO1 VALIPRD                                                          
         MVC   BUYKPRD,BPRD        PRODUCT                                      
         MVC   SCTPRDD,PRDNM      PRODUCT DESCRIPTION                           
         FOUT  SCTPRDDH                                                         
*                                                                               
VK15     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SCTESTH          ESTIMATE                                     
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK20                                                             
         MVI   KEYCHNG,1           KEY CHANGE FLAG                              
         GOTO1 ANY                                                              
         GOTO1 VALIEST                                                          
         MVC   BUYKEST,BEST        ESTIMATE                                     
         MVC   SCTESTN,ESTNM       ESTIMATE NAME                                
         FOUT  SCTESTNH                                                         
*                                                                               
         L     R6,AIO              POINT TO ESTIMATE RECORD                     
         USING ESTHDRD,R6                                                       
         MVC   SVESTART,ESTART     SAVE START/END DATES                         
         MVC   SVEEND,EEND                                                      
         DROP  R6                                                               
*                                                                               
VK20     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SCTSTATH         STATION                                      
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK30                                                             
         MVI   KEYCHNG,1           KEY HAS CHANGED                              
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         MVC   BUYMSTA,BMKTSTA     PACKED MARKET/STATION                        
         MVC   SCTSTAN,QMKT        MARKET NUMBER                                
         OI    SCTSTANH+6,X'80'                                                 
         OI    SCTSTANH+4,X'08'                                                 
         OI    SCTSTANH+5,4                                                     
*                                                                               
         LA    R2,SCTSTANH                                                      
         GOTO1 VALIMKT                                                          
         MVC   SCTSTAD,MKTNM       MARKET NAME                                  
         FOUT  SCTSTADH                                                         
*                                                                               
* VALIDATE BUY DETAIL FIELD                                                     
VK30     OI    SCTSTATH+4,X'20'    VALIDATED                                    
         LA    R2,SCTBUYLH                                                      
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK40                                                             
         MVI   KEYCHNG,1           KEY HAS CHANGED                              
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
*                                                                               
* BUY - POL BRAND                                                               
         OI    4(R2),X'20'         VALIDATED                                    
         STC   R1,BUYKBUY+2        LINE NUMBER - BYTE 12                        
         MVI   BUYKBUY+1,X'00'     BYTE 11                                      
         MVI   BUYKBUY,X'FF'       BYTE 10                                      
         MVC   KEY,BUYKEY                                                       
         GOTO1 READ                                                             
*                                                                               
* VALIDATE THAT THERE IS EXACTLY 1 X'0B' ELEMENT IN RECORD                      
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'0B'                                                     
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    VK35                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'** Spot not found **'                             
         GOTO1 ERREX2                                                           
*                                                                               
VK35     BAS   RE,NEXTEL                                                        
         BNE   VKEND                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'** Multiple Spots **'                             
         GOTO1 ERREX2                                                           
         B     VKEND                                                            
*                                                                               
VKERR    MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
VK40     CLI   KEYCHNG,0           DID KEY CHANGE                               
         BNE   VKEND                                                            
         MVC   KEY,BUYKEY          USE THE SAVED KEY                            
         B     VKEXIT                                                           
*                                                                               
VKEND    XC    USERDATE,USERDATE   CLEAR OUT USER START DATE                    
         MVC   KEY,BUYKEY          FILL IN THE KEY                              
*                                                                               
VKEXIT   XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* CODE TO VALIDATE BUY RECORD                                                   
*                                                                               
VR       NTR1                                                                   
*                                                                               
* CONFIRM THAT WE HAVE A POOL ORIGINAL-'0B' ELEMENT IN RECORD                   
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         MVI   CHNGFLG,C'N'        CHANGE RECORD FLAG                           
         USING BUYREC,R4                                                        
         LA    R2,SCTBUYLH     POSITION CURSOR IN CASE OF ERROR MSGS            
*                                                                               
* SEE IF BUY RECORD IS MARKED FOR DELETION                                      
         TM    BUYRCNTL,X'80'        IS DELETE BIT ON                           
         BZ    VR01                  NO, THEN DISPLAY                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'** Buy record deleted **'                         
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
VR01     L     R3,AIO                                                           
         MVI   ELCODE,X'0B'                                                     
         USING REGELEM,R3                                                       
         BAS   RE,GETEL                                                         
         BE    VR01A               MUST BE ONE 0B ELEMENT                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(12),=C'** No Buy **'                                     
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
*                                                                               
VR01A    BAS   RE,MISSED           CHECK FOR PREEMPTED, MADE GOOD               
         CLI   MISSFLG,1           IS IT A MISSED BUY?                          
         BNE   VR02                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'** No Counts for missed buys **'                  
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
VR02     GOTO1 DATCON,DMCB,(0,USERDATE),(2,STRTDATE) 1ST DATE ON SCREEN         
         GOTO1 ADDAY,DMCB,USERDATE,WORK,F'+9'    LAST DATE ON SCREEN            
         GOTO1 DATCON,DMCB,(0,WORK),(2,ENDDATE)  YYMMDD -> COMPRESSED           
*                                                                               
* SEE IF A 72 ELEMENT.                                                          
         L     R3,AIO                                                           
         MVI   ELCODE,X'72'      INFO COUNT ELEMENT                             
         USING INFOELEM,R3                                                      
         BAS   RE,GETEL                                                         
         BE    VR02A                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'** INFB record must precede INFC **'              
         GOTO1 ERREX2                                                           
VR02A    SR    R0,R0                                                            
         IC    R0,INFOLEN                                                       
         AR    R3,R0                                                            
*                                                                               
* DELETE ONLY THE '73' ELEMENTS THAT ARE DISPLAYED ON SCREEN                    
         L     R3,AIO                                                           
         MVI   ELCODE,X'73'        INFOCOUNT ELEMENT                            
         USING INFDCNTS,R3                                                      
         BAS   RE,GETEL                                                         
         BNE   VR05                                                             
*                                                                               
VR2B     CLC   INFDDATE,STRTDATE   IS DATE LOWER THAN START DATE                
         BL    VR2N                                                             
         CLC   INFDDATE,ENDDATE IS DATE GREATER THAN LAST DATE ON SCRN          
         BH    VR04                                                             
         MVI   INFDCNTS,X'FF'      MARK ELEMENT FOR DELETION                    
*                                                                               
VR2N     BAS   RE,NEXTEL                                                        
         BE    VR2B                                                             
*                                                                               
* DELETE MARKED ELEMENTS                                                        
*                                                                               
VR04     MVI   ELCODE,X'FF' INFO COUNT ELEMENT MARKED FOR DEL WITH FF           
         SPACE 1                                                                
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VR05                                                             
         GOTO1 RECUP,DMCB,(0,AIO),(R3)    DELETE THE ELEMENT                    
         B     VR04                                                             
         DROP  R3                                                               
         EJECT                                                                  
VR05     LA    R2,SCTDATEH         DATE FIELD                                   
*                                                                               
VR06     CLI   7(R2),0             END OF DATES?                                
         BE    VR60                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'73'                                                     
         LA    R3,ELEM                                                          
         USING INFDCNTS,R3                                                      
*                                                                               
         MVI   INFDCNTS,X'73'                                                   
         MVI   INFDCNTS+1,18                                                    
         LA    R6,INFDCTRS                                                      
*                                                                               
* VALIDATE DATE, CREDIT CARDS, LEADS, AND UPSELL COUNTS                         
* CONVERT AND STORE DATES IN COMPRESSED FORM                                    
*                                                                               
         XC    UPSFLG,UPSFLG                                                    
         GOTO1 DATVAL,DMCB,(1,8(R2)),WORK                                       
*                                                                               
         MVC   WORK(2),SVESTART      ASSUME DATE IS IN EST START YEAR           
         CLC   SVESTART(2),SVEEND    TEST ESTIMATE ALL IN ONE YEAR              
         BNE   VR06B                 NO                                         
         CLC   WORK+2(4),SVESTART+2  INPUT MMDD TO EST START MMDD               
         BNL   VR06X                                                            
* DATE IS IN NEXT YEAR                                                          
         PACK  DUB,WORK(2)                                                      
         CVB   R1,DUB                                                           
         LA    R1,1(R1)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         B     VR06X                                                            
*                                                                               
VR06B    MVC   WORK(2),SVEEND        ASSUME EST END YEAR                        
         CLC   WORK+2(4),SVEEND+2    INPUT MMDD TO EST END MMDD                 
         BNH   VR06X                                                            
         CLC   WORK+2(4),SVESTART+2  INPUT MMDD TO EST START MMDD               
         BL    VR06X                                                            
         MVC   WORK(2),SVESTART                                                 
*                                                                               
VR06X    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,INFDDATE) COMPRESSED                     
*                                                                               
VR07     LA    R2,LEN1(R2)         CREDIT CARDS                                 
         CLI   5(R2),0                                                          
         BE    VR09                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,0(R6)                                                         
*                                                                               
VR09     LA    R2,LEN2(R2)         LEADS                                        
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,2(R6)                                                         
*                                                                               
VR10     MVI   UPSFLG,1            UPS1                                         
         LA    R2,LEN2(R2)         UPS1                                         
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,4(R6)                                                         
*                                                                               
VR20     LA    R2,LEN2(R2)         UPS2                                         
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,6(R6)                                                         
*                                                                               
VR30     LA    R2,LEN2(R2)         UPSELL3                                      
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,8(R6)                                                         
*                                                                               
VR40     LA    R2,LEN2(R2)         UPSELL4                                      
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,10(R6)                                                        
*                                                                               
VR50     LA    R2,LEN2(R2)         UPSELL5                                      
         CLI   5(R2),0                                                          
         BE    VR55                                                             
         BAS   RE,CONVERT                                                       
         STH   R5,12(R6)                                                        
*                                                                               
VR55     LA    R2,LEN2(R2)         POINT TO DATE HEADER                         
         OC    ELEM+4(14),ELEM+4                                                
         BZ    VR06                                                             
         L     R3,AIO                                                           
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VR57     ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'73'                                                      
         BNE   VR59                                                             
         CLC   ELEM+2(2),2(R3)                                                  
         BH    VR57                                                             
VR59     DS    0H                                                               
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)                                     
*                                                                               
         B     VR06                                                             
         DROP  R3,R4                                                            
*                                                                               
* GET THE SUM OF THE COUNTS FROM THE X'73' ELEMENTS                             
*                                                                               
VR60     XC    COUNTS,COUNTS                                                    
         L     R3,AIO                                                           
         MVI   ELCODE,X'73'      INFO COUNT ELEMENT                             
         USING INFDCNTS,R3                                                      
         BAS   RE,GETEL                                                         
         BNE   VR80                                                             
*                                                                               
VR70     LA    R6,INFDCTRS                                                      
         LH    R5,CCCNT                                                         
         AH    R5,0(R6)          CREDIT CARD COUNT                              
         STH   R5,CCCNT                                                         
*                                                                               
         LH    R5,LEADCNT                                                       
         AH    R5,2(R6)          LEADS COUNT                                    
         STH   R5,LEADCNT                                                       
*                                                                               
         LH    R5,UPS1CNT                                                       
         AH    R5,4(R6)          UPSELL1 COUNT                                  
         STH   R5,UPS1CNT                                                       
*                                                                               
         LH    R5,UPS2CNT                                                       
         AH    R5,6(R6)          UPSELL2 COUNT                                  
         STH   R5,UPS2CNT                                                       
*                                                                               
         LH    R5,UPS3CNT                                                       
         AH    R5,8(R6)          UPSELL3 COUNT                                  
         STH   R5,UPS3CNT                                                       
*                                                                               
         LH    R5,UPS4CNT                                                       
         AH    R5,10(R6)         UPSELL4 COUNT                                  
         STH   R5,UPS4CNT                                                       
*                                                                               
         LH    R5,UPS5CNT                                                       
         AH    R5,12(R6)         UPSELL5 COUNT                                  
         STH   R5,UPS5CNT                                                       
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    VR70                                                             
         DROP  R3                                                               
*                                                                               
*  PUT THE SUM OF THE COUNTS IN THE INFB X'72' ELEMENT                          
*                                                                               
VR80     L     R3,AIO                                                           
         MVI   ELCODE,X'72'      INFO COUNT ELEMENT                             
         USING INFOELEM,R3                                                      
         BAS   RE,GETEL                                                         
*                                                                               
* FILL IN INFB ELEMENT WITH THE SUM OF THE COUNTS.                              
*                                                                               
         LA    R4,INFOCTRS           COUNTERS                                   
         MVC   0(2,R4),CCCNT           CREDIT CARDS COUNT                       
         MVC   12(2,R4),LEADCNT        LEADS COUNT                              
         MVC   14(2,R4),UPS1CNT        UPS1CNT                                  
         MVC   16(2,R4),UPS2CNT        UPS2CNT                                  
         MVC   18(2,R4),UPS3CNT        UPS3CNT                                  
         MVC   20(2,R4),UPS4CNT        UPS4CNT                                  
         MVC   22(2,R4),UPS5CNT        UPS5CNT                                  
*                                                                               
         DROP  R3                                                               
VREND    MVC   AIO,AIO1                                                         
         LA    R2,SCTCCH              POSITION CURSOR                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CLEAR OUT SCREEN INPUT                                                        
* GET ELEMENT DATA                                                              
DR       NTR1                                                                   
         TWAXC SCTDATAH,SCTDATAH,PROT=Y  CLEAR BUY DATA LINE                    
         TWAXC SCTCCH                    CLEAR ALL DATA FROM SCREEN             
         TWAXC SCTSDAYH,SCTSDAYH,PROT=Y  CLEAR PROTECTED DAY FIELD              
         TWAXC SCTSMISH,SCTSMISH,PROT=Y  CLEAR PROTECTED MISSED FIELD           
         TWAXC SCTDATEH,SCTLLINH,PROT=Y  CLEAR PROTECTED DATE FIELDS            
*                                                                               
         BAS   RE,MISSED     CHECK FOR PREEMPTED, MADE GOOD                     
*                                                                               
         L     R3,AIO        RECORD AREA                                        
         USING BUYREC,R3     UPSELL CATEGORY NAMES ELEMENT                      
*                                                                               
* SEE IF BUY RECORD IS MARKED FOR DELETION                                      
         TM    BUYRCNTL,X'80'        IS DELETE BIT ON                           
         BZ    DR05                  NO, THEN DISPLAY                           
         LA    R2,SCTBUYLH           POSITION CURSOR                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'** Buy record deleted **'                         
         GOTO1 ERREX2                                                           
*                                                                               
* DISPLAY THE BUY DATA LINE                                                     
* DISPLAY START DATE                                                            
*                                                                               
DR05     LA    R4,SCTDATA                                                       
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
DR22N    DS    0H                                                               
         LTR   R0,R0                                                            
         BZ    DR22EDT                                                          
         CLI   SVAPROF+7,C'C'      TEST CANAD NTWK                              
         BNE   *+12                                                             
         TM    BDCIND2,X'01'       YES-TEST COST IN PENNIES                     
         BZ    DR22B                   NO                                       
         C     R0,=F'9999999'      IF COST TOO BIG                              
         BH    DR22A                DROP CENTS                                  
DR22EDT  EDIT  (R0),(8,(R4)),2                                                  
         B     DR22C                                                            
DR22A    SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
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
         FOUT  SCTDATAH             BUY DATA LINE                               
*                                                                               
DR30     CLI   MISSFLG,1                                                        
         BE    DREND             NO COUNT DATA FOR MISSED BUY                   
*                                                                               
         MVC   BUYDATE,BDSTART    BUY START DATE -  YMD                         
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,SAVEBUY) STORE BUY START DATE         
         GOTO1 ADDAY,DMCB,SAVEBUY,MAXDATE,F'+59' LAST DATE ALLOWED              
*                                                                               
         OC    USERDATE,USERDATE   IS IT THE FIRST DISPLAY?                     
         BZ    DR32                                                             
         LA    R2,SCTSDATH                                                      
         CLI   5(R2),0             DID THEY ERASE                               
         BE    DR32                                                             
         BAS   RE,DATEVAL          VALIDATE START DATE                          
         GOTO1 DATCON,DMCB,(0,USERDATE),(4,SCTSDAT)  USE USER DATE              
         B     DR35                                                             
*                                                                               
DR32     GOTO1 DATCON,DMCB,(3,BUYDATE),(4,SCTSDAT) START WITH BUY DATE          
         GOTO1 DATCON,DMCB,(3,BUYDATE),(0,USERDATE) YMD TO YYMMDD               
         B     DR40                                                             
*                                                                               
DR35     CLI   PFAID,8                                                          
         BE    DR36                                                             
         CLI   PFAID,20                                                         
         BE    DR36                                                             
         CLI   PFAID,7                                                          
         BE    DR37                                                             
         CLI   PFAID,19                                                         
         BE    DR37                                                             
         BNE   DR40                                                             
*                                                                               
* IF PF8,PF7 THEN ADJUST DATE 10 DAYS, AND DISPLAY FROM THAT DATE               
* DOWN.                                                                         
DR36     GOTO1 ADDAY,DMCB,USERDATE,NEWDATE,F'+10'   YYMMDD                      
         CLC   MAXDATE,NEWDATE  IS NEW DATE > 60 DAYS LATER THAN START          
         BNL   DR38                                                             
         MVC   NEWDATE,USERDATE      DON'T USE NEW DATE                         
         B     DR38                                                             
*                                                                               
* THE MINIMUM DATE ALLOWED IS THE BUY START DATE.                               
*                                                                               
DR37     GOTO1 ADDAY,DMCB,USERDATE,NEWDATE,F'-10'   YYMMDD                      
         CLC   NEWDATE,SAVEBUY    IS ADJUSTED DATE LESS THAN BUY START          
         BNL   DR38                                                             
         MVC   NEWDATE,SAVEBUY    START WITH BUY START DATE                     
*                                                                               
DR38     MVC   USERDATE,NEWDATE     DATE ADJUSTED 10 DAYS                       
         GOTO1 DATCON,DMCB,(0,NEWDATE),(4,SCTSDAT)  MMMDD FOR DISPLAY           
*        GOTO1 DATCON,DMCB,(0,NEWDATE),(2,BUYDATE) YYMMDD TO COMPRESSED         
*                                                                               
DR40     FOUT  SCTSDATH       WRITE OUT FORMATTED DATE                          
         GOTO1 GETDAY,DMCB,(0,USERDATE),SCTSDAY                                 
         FOUT  SCTSDAYH                                                         
*                                                                               
* GET DATA FROM X'73' ELEMENTS. MATCH UP DATES FOR THE DISPLAY.                 
*                                                                               
DR45     L     R3,AIO                                                           
         MVI   ELCODE,X'73'                                                     
         LA    R2,SCTDATEH                                                      
         SR    R5,R5               LINE COUNTER                                 
         GOTO1 DATCON,DMCB,(0,USERDATE),(2,NXTDATE) YYMMDD -> COMPR.            
         MVC   DATECL6,USERDATE                                                 
         B     DR70                                                             
*                                                                               
* DISPLAY THE DATES THAT FOLLOW THE START DATE                                  
*                                                                               
DR50     LA    R2,LENNXT(R2)           NEXT LINE                                
*                                                                               
DR60     GOTO1 ADDAY,DMCB,DATECL6,OUTDATE,F'+1'  NEXT DAY                       
         MVC   DATECL6,OUTDATE        KEEP INCREMENTED DAY                      
*                                                                               
* THE MAX. DATE ALLOWED IS 60 DAYS AFTER THE BUY START DATE.                    
*                                                                               
         CLC   OUTDATE,MAXDATE        IS LINE DATE <= MAX DATE                  
         BNH   DR65                                                             
*                                                                               
         LA    R2,SCTCCH        POSITION CURSOR ON 1ST RESPONSE LINE            
         CLI   ACTNUM,ACTCHA           ACTION IS A CHANGE                       
         BNE   DR61                                                             
         CLI   CHNGFLG,C'Y'            DID USER CHANGE RECORD?                  
         BE    DREND                   IF YES, GENCONS MESSAGE                  
*                                                                               
DR61     XC    CONHEAD,CONHEAD                                                  
         OI    GENSTAT2,USMYOK         USE MY MESSAGE                           
         OI    CONHEADH+6,X'80'                                                 
         CLI   ACTNUM,ACTCHA           ACTION IS A CHANGE                       
         BNE   DR62                                                             
         MVC   CONHEAD(33),=C'** End of list - enter changes **'                
         B     DREND                                                            
DR62     MVC   CONHEAD(17),=C'** End of list **'                                
         B     DREND                                                            
*                                                                               
DR65     GOTO1 DATCON,DMCB,(0,OUTDATE),(2,NXTDATE) YYMMDD -> COMPR.             
*                                                                               
DR70     CH    R5,=H'10'        DISPLAY UP TO TEN DATES ON A SCREEN             
         BE    DREND                                                            
         LA    R5,1(R5)                                                         
         GOTO1 DATCON,DMCB,(2,NXTDATE),(4,8(R2))   DISPLAY FORMAT               
         FOUT  (R2)                                DISPLAY DATE                 
*                                                                               
*                                                                               
DR71     L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DR50              NO ELEMENTS, THEN JUST DISPLAY DATES           
         USING INFDCNTS,R3                                                      
DR72     CLC   NXTDATE,INFDDATE  DOES ELEMENT DATE MATCH THE LINE DATE          
         BE    DR75         IF YES, FILL IN LINE ON SCREEN WITH COUNTS          
         BL    DR50                                                             
         BAS   RE,NEXTEL                                                        
         BE    DR72                                                             
         B     DR50              GO TO NEXT LINE                                
*                                                                               
DR75     LA    R4,INFDCTRS                     COUNTERS                         
*                                                                               
         LA    R2,LEN1(R2)                     CC HEADER                        
         EDIT  (2,0(R4)),(4,8(R2)),ALIGN=LEFT   CREDIT CARDS                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)                     LEADS HEADER                     
         EDIT  (2,2(R4)),(4,8(R2)),ALIGN=LEFT   LEADS                           
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)                                                      
         EDIT  (2,4(R4)),(4,8(R2)),ALIGN=LEFT   UPS1 COUNT                      
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)                                                      
         EDIT  (2,6(R4)),(4,8(R2)),ALIGN=LEFT   UPS2 COUNT                      
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)                                                      
         EDIT  (2,8(R4)),(4,8(R2)),ALIGN=LEFT   UPS3 COUNT                      
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)                                                      
         EDIT  (2,10(R4)),(4,8(R2)),ALIGN=LEFT    UPS4 COUNT                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)                                                      
         EDIT  (2,12(R4)),(4,8(R2)),ALIGN=LEFT    UPS5 COUNT                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LEN2(R2)  GET TO NEXT LINE - DATE FIELD HEADER                
         B     DR60                                                             
*                                                                               
DREND    CLI   PFAID,8                                                          
         BE    DR80                                                             
         CLI   PFAID,20                                                         
         BE    DR80                                                             
         CLI   PFAID,7                                                          
         BE    DR80                                                             
         CLI   PFAID,19                                                         
         BNE   *+8                                                              
*                                                                               
DR80     OI    SCTCCH+6,X'40'   POSITION CURSOR ON 1ST RESPONSE LINE            
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* CONVERT CHARACTER STRING TO HEX WITH AN EXECUTED PACK                         
CONVERT  CLI   MISSFLG,1     IS IS A MISSED BUY                                 
         BNE   CONV1                                                            
         CLI   UPSFLG,1      IS IT AN UPSELL FIELD                              
         BE    CONVUPS                                                          
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
         TM    4(R2),X'80'    WAS FIELD CHANGED THIS TIME                       
         BZ    CONVEND                                                          
         MVI   CHNGFLG,C'Y'   FIELD INPUT THIS TIME                             
         B     CONVEND                                                          
*                                                                               
CONVERR  MVI   ERROR,NOTNUM                                                     
         B     ERRX                                                             
*                                                                               
CONVUPS  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'** No Upsells for missed buys **'                 
         GOTO1 ERREX2                                                           
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
         BZ    MISSEND                                                          
*                                                                               
ERMISS   MVI   MISSFLG,1                                                        
         MVC   SCTSMIS,=C'** MISSED **'                                         
         FOUT  SCTSMISH                                                         
*                                                                               
MISSEND  XIT1                                                                   
         EJECT                                                                  
* THE BUY START DATE CAN BE OVERRIDDEN, SO WE NEED TO VALIDATE.                 
*                                                                               
DATEVAL  NTR1                                                                   
         LA    R2,SCTSDATH                                                      
         GOTO1 DATVAL,DMCB,(1,SCTSDAT),WORK     VALIDATE MMMDD                  
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
         MVC   WORK(2),SAVEBUY   FILL IN WITH SAME YEAR AS IN BUY DATE          
         CLC   SAVEBUY,WORK      IS BUY START DATE > USER DATE?                 
         BH    DTNXTYR                                                          
         B     DTMAX                                                            
*                                                                               
* USER DATE MAY BE IN FOLLOWING YEAR                                            
*                                                                               
DTNXTYR  GOTO1 ADDAY,DMCB,WORK,NEXTYEAR,F'+365'                                 
         MVC   WORK(2),NEXTYEAR    FILL IN YY FOR THE NEXT YEAR                 
*                                                                               
* IS USERDATE WITHIN 60 DAYS OF BUY START DATE                                  
*                                                                               
DTMAX    CLC   WORK,MAXDATE                                                     
         BNH   DT05                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'** Date is out of range **'                       
         GOTO1 ERREX2                                                           
*                                                                               
DT05     MVC   USERDATE,WORK    SAVE THE DISPLAYED DATE,YYMMDD FORMAT           
         GOTO1 DATCON,DMCB,(0,USERDATE),(2,STRTDATE) YYMMDD -> COMPRESS         
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
         EJECT                                                                  
         GETEL R3,24,ELCODE                                                     
         EJECT                                                                  
RELO     DS    A                                                                
ERRX     GOTO1 ERREX                                                            
ZEROS    DC    20C'0'                                                           
LEN1     EQU   13       8+5                                                     
LEN2     EQU   12       8+4                                                     
LENNXT   EQU   97       ONE LINE LENGTH                                         
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PFTABLE  DS    0C                                                               
*                                                                               
* INFB DISPLAY                                                                  
         DC    AL1(PF06X-*,06,PFTCPROG,(PF06X-PF06)/KEYLNQ,0)                   
         DC    CL3' ',CL8'INFB',CL8'DISPLAY'                                    
PF06     DC    AL1(KEYTYTWA,L'SCTMEDI-1),AL2(SCTMEDI-T217FFD)                   
         DC    AL1(KEYTYTWA,L'SCTCLT-1),AL2(SCTCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'SCTPROD-1),AL2(SCTPROD-T217FFD)                   
         DC    AL1(KEYTYTWA,L'SCTEST-1),AL2(SCTEST-T217FFD)                     
         DC    AL1(KEYTYTWA,L'SCTSTAT-1),AL2(SCTSTAT-T217FFD)                   
         DC    AL1(KEYTYTWA,L'SCTBUYL-1),AL2(SCTBUYL-T217FFD)                   
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
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SCSFM9BD   MAINT SCREEN                                        
         ORG   CONTAGH                                                          
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
         PRINT ON                                                               
* STORAGE DSECT                                                                 
*                                                                               
         ORG   SYSSPARE                                                         
SVESTART DS    CL6                 ESTIMATE START DATE                          
SVEEND   DS    CL6                 ESTIMATE END DATE                            
KEYAREA  DS    CL24                                                             
WORK2    DS    CL48                                                             
ELCDLO   DS    CL1                                                              
USERDATE DS    CL6                                                              
ELCDHI   DS    CL1                                                              
MISSFLG  DS    XL1                                                              
CHNGFLG  DS    XL1                                                              
ELEMS    DS    XL1                                                              
UPSFLG   DS    XL1      IS IT AN UPSELL FIELD                                   
KEYCHNG  DS    XL1      KEY CHANGE INDICATOR                                    
NXTDATE  DS    XL2                                                              
DATECL6  DS    CL6      YYMMDD                                                  
STRTDATE DS    XL2      COMPRESSED                                              
ENDDATE  DS    XL2      COMPRESSED                                              
NEWDATE  DS    CL6      YYMMDD                                                  
BUYDATE  DS    XL3                                                              
SAVEBUY  DS    CL6      BUY START DATE IN YYMMDD FORMAT                         
MAXDATE  DS    CL6      LAST DATE ALLOWED IN DISPLAY                            
NEXTYEAR DS    CL6                                                              
OUTDATE  DS    CL6                                                              
         DS    0H                                                               
COUNTS   DS    0CL14     COUNTS FROM INFO COUNT ELEMENT                         
CCCNT    DS    XL2                                                              
LEADCNT  DS    XL2                                                              
UPS1CNT  DS    XL2                                                              
UPS2CNT  DS    XL2                                                              
UPS3CNT  DS    XL2                                                              
UPS4CNT  DS    XL2                                                              
UPS5CNT  DS    XL2                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPSFM3D   05/01/02'                                      
         END                                                                    
