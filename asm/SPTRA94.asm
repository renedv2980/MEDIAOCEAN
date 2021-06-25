*          DATA SET SPTRA94    AT LEVEL 053 AS OF 06/20/19                      
*PHASE T21694A                                                                  
*INCLUDE COVAIL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*        THIS PROGRAM PURGES FLIGHTS                                  *         
*                            TRAFFIC BUY ELEMENTS AND EMPTY RECS      *         
*                            FLIGHTS                                  *         
*                            DEALER TAGS                              *         
*                            INSTRUCTION RECAP ELEMS AND EMPTY RECS   *         
*                            SHIPPING RECAP ELEMS AND EMPTY RECS      *         
*                            PATTERNS                                 *         
*                            COMMERCIALS                              *         
*                                                                     *         
*   WILL ONLY RUN OFFLINE, AS IT READS AND WRITES TOO MANY RECORDS    *         
*   FOR ONLINE.                                                       *         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN C*NTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - VOFF, FCLT                                       *         
*             AIO3 - READ IN COMMERCIAL TEXT RECS FOR COMML           *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER TO STATION RECORD                               *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - IN APL ACTIVE PATTERN LIST PTR               *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE                                             *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21694 PURGE INST/SHIP RECAP PAT/COMML/FLT/TBUY/BUY/DLRC        
                RECS'                                                           
***********************************************************************         
*                                                                     *         
*  LEV  8-9  NOV02/87 ONLY ALLOW DDS TERMINALS ACCESS                 *         
*  LEV 10-13 NOV12/87 IF ONLY 1 PROD INPUT, CK NO PARTNERS DEL DLR TAG*         
*                     INSTR RECAP ELEMS                               *         
*  LEV 14    NOV20/87 FIX BUG DELETING NON-EMPTY INSTR RECAP RECS     *         
*  LEV 15    DEC07/87 BYPASS ALL PRINT LOGIC IF NOT PRINTING          *         
*  LEV 16    JAN08/88 FIND BUG, RUNNING INTO ACTIVE CML LIST IN PAT   *         
*  LEV 17    APR08/91 BYPASS TBA PATTERNS IN INSTR RECAP              *         
*  LEV 18    JAN21/92 READ PU PROFILE TO BYPASS PURGE                 *         
*  LEV 19    FEB25/93 USE PU PROFILE TO CATAGORIZE TYPE A RECS -      *         
*                    Y=BYPASS ALL, N=PURGE ALL, A=PURGE FLIGHTS,      *         
*                    DEALER TAGS, TRAFFIC BUYS, SHIPPING RECAPS       *         
*                    CABLE HEAD IS IN TOO                             *         
*  LEV 20    MAR11/94 STRAFFIC                                        *         
*  LEV 21    MAR18/94 FIX ACTIVE PATTERN/COMMERCIAL LIST BUG          *         
*  LEV 22    APR11/94 FIX EOJ VSWIT                                   *         
*  LEV 23    MAR02/95 CHANGE TO FILENAME, NOT SWITCH                  *         
*  LEV 24    MAR23/95 MAKE PATTERN TABLE LARGER                       *         
*                     DELETE ELEMENTS FROM PATTERN STEXT RECS         *         
*  LEV 25    APR21/98 FIX PTEXT UPDATE TO INCLUDE GETREC FOR PATTN    *         
*  LEV 26 SMUR NOV10/99 USE RECUP FROM FACPAK                         *         
*  LEV 27 SMUR APR03/01 USE TRAFFIC OFFICE                            *         
*  LEV 30 SMUR APR03/01 USE TRAFFIC OFFICE                            *         
*  LEV 31 BGRI APR16/04 ADD TO PATTERN/COMML TABLE SIZE               *         
*  LEV 32 BGRI FEB02/05 DELETE ANY TRAFFIC RECS WITH NO SPOT CLIENT   *         
*                       DO NOT PURGE ANY EASI COMMERCIALS             *         
*                       END OF JOB REPORT OF ALL CLIENTS BYPASSED     *         
*                       CK INVOICES FOR ACTIVE COMMLS                 *         
*  LEV 33 SMUR APR17/06 FIX READING CLIENT RECORD BUGS                *         
*  LEV 34 SMUR MAY08/06 FIX ADDING P/B CMLS TO ACTIVE CML TABLE       *         
*  LEV 35 SMUR MAY25/06 FIX BUG WHEN DELETING ONLY CML RECORD AND OR  *         
*                       PAT REC MUST BUILD ACTIVE CML/PAT TABLE       *         
*                       MOVE VK ROUTINE INTO ITS OWN NMOD             *         
*                       MOVE CML, INSTR OPTION TO  PROFILE            *         
*                       READ T0 PROFILE FOR COPY CODE EST/DAYP/ADJ    *         
*                       DISABLE FIXMIS (WAS DELETING OFFICE RECODS)   *         
*  LEV 36 SMUR AUG31/07 FOR CANADIAN AGENCY PURGE MEDIA N RECS AS WELL*         
*                       AS MEDIA T WHEN REQUESTED FOR MEDIA T         *         
*  LEV 38 SMUR JAN13/08 ADD CODE FOR INCOMPLETE PATTERNS              *         
*  LEV 39 SMUR APR23/09 USE COVAIL TO GET LARGER BUFFER FOR ACTIVE CML*         
*                       LIST, MOVE INV ROUTINE FOR ADDRESSABILITY     *         
*  LEV 41 SMUR AUG10/09 ALL ADID SUPPORT, HIDEF/CENTRCUT PASSIVES     *         
*  LEV 42 SMUR NOV16/09 NO 0AC1 PASSIVE KEYS FOR ISCII W/SPECIAL CHAR *         
*  LEV 44 SMUR DEC10/09 FIX XSORT CALLS                               *         
*  LEV 44 SMUR DEC28/09 FIX 0A25 GETREC/PUTREC                        *         
*  LEV 45 SMUR MAY03/10 BYPASS COMMERCIAL CHK IF NO X'30' ELEM IN BPAT*         
*  LEV 46 SMUR NOV05/10 READ AGENCY LEVEL PU PROFILE NOT USER ID      *         
*  LEV 50 SMUR JUN/11   MAKE ACTIVE PAT TBL BIGGER AND GET ABOVE THE  *         
*                       LINE STORAGE. KEEP CMLS WITH UFN              *         
*  LEV 51 SMUR MAY18/12 FIX DELETE PASSIVE POINTERS, ALSO DROP R4 FOR *         
*                       CORRECT ADDRESSABILITY TO ELEMS IN CML        *         
*  SPEC-36842  SMUR  INCRESE ACTIVE PAT AND ACTIVE CML TABLES         *         
*                                                                     *         
***********************************************************************         
*                                                                               
T21694   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1694**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR94RR                                                      
*                                                                               
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,PRINTREP       NOW PURGE RECORDS                            
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
FILELIST DS   0D                                                                
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NXSPFILE'                                                    
         DC    CL8'NSPTDIR '                                                    
         DC    CL8'NXSPDIR '                                                    
         DC    CL10'X'                                                          
* INITIALIZE *                                                                  
*                                                                               
LR       TM    WHEN,X'18'          OV OR DDS                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP     BRAS  RE,FIXMIS           GO FIND ANY MISSING CLIENT RECORDS           
*                                                                               
         L     R1,=A(HEADING)      HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         L     R1,=A(HDHK)         HEAD HOOK RTN                                
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
*                                                                               
         L     R1,VADUMMY                                                       
         MVC   0(8,R1),=C'XCLIENT*'  EXCLUDED CLIENTS                           
         LA    R1,8(R1)                                                         
         LR    RE,R1                                                            
         ST    R1,AXCLTBLE                                                      
         AHI   R1,600                200 CLIENTS                                
         ST    R1,MAXCLTBL                                                      
*                                                                               
         LR    RF,R1               GET END OF TABLES                            
         SR    RF,RE               VDUMMY IN RE, RF = TOTAL LENGTH              
         XCEF                                                                   
         XC    COUNTERS(ENDCTRS-COUNTERS),COUNTERS                              
         XC    AGYCTRS(ENDCTRS-COUNTERS),AGYCTRS                                
         OC    SVBCLT,SVBCLT                                                    
         BNZ   LR30                                                             
*                                                                               
LR10     CLI   OFFICE,0            BY OFFICE                                    
         BE    LR20                 YES GET NEXT CLIENT FOR OFFICE              
*                                                                               
         BRAS  RE,NOFF                                                          
         BNE   EOJ                                                              
         B     LR30                                                             
*                                                                               
* GET NEXT SEQUENTIAL CLIENT *                                                  
*                                                                               
LR20     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT                                                  
LR24     MVI   KEY+4,X'FF'                                                      
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME   SWITCH BACK TO STRAFFIC SYSTEM               
         CLC   KEY(2),KEYSAVE                                                   
         BNE   EOJ                                                              
         OC    KEY+4(9),KEY+4      CLIENT HDR                                   
         BNZ   LR24                                                             
         MVC   SVBCLT,KEY+2                                                     
LR30     BRAS  RE,FCLT                                                          
         BNE   LR32                                                             
*                                                                               
*PU PROFILE SET TO BYPASS CLIENT                                                
*                                                                               
         OC    BCLT,BCLT           1 CLIENT REQUEST                             
         BZ    LR20                 NO                                          
         B     EOJ                 DO TOTALS AND EXIT                           
         EJECT                                                                  
* SET UP TABLE POINTERS AND MAX TABLE SIZES                                     
*                                                                               
LR32     LA    R1,TABLES                                                        
*NOP     ST    R1,STRAPLST         START OF ACTIVE PATTERN LIST                 
*        ST    R1,ENDAPLST                                                      
*        MVC   0(3,R1),=X'FFFFFF'                                               
*        LA    R1,2048(,R1)                                                     
*                                                                               
*        LR    R0,R9               GET MAX TABLE SIZE BOUNDARY                  
*        A     R0,LSYSD                                                         
*****    ST    R0,MAXTBLSZ                                                      
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   LR34                USE DUMMY AREA                               
*                                                                               
         L     R1,MAXCLTBL                                                      
*                                                                               
****MOVE PAT TABLE TO TSAR BUFFER                                               
*NOP     LA    R1,8(R1)                                                         
*        ST    R1,STRAPLST         START OF ACTIVE PATTERN LIST                 
*        ST    R1,ENDAPLST                                                      
*        MVC   0(3,R1),=X'FFFFFF'                                               
*                                                                               
*        A     R1,=F'180000'                                                    
*        A     R1,=F'90000'                                                     
******   ST    R1,MAXTBLSZ                                                      
*                                                                               
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         OC    0(4,RE),0(RE)                                                    
         BNZ   LR32B                                                            
*                                                                               
         LAY   R1,CMLNUM           NUMBER OF ENTRIES                            
         MHI   R1,CMLELEN          NUM OF ENTRIES * ENTRY LEN                   
         ST    R1,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,STRACLST         SET A(ACTIVE CML TABLE)                      
         ST    RE,ENDACLST                                                      
         ICM   RF,15,8(R1)         TABLE LEN                                    
         AR    RF,RE                                                            
         ST    RF,MAXACLST         MAX ACITVE CML LIST                          
*                                                                               
* GET STORAGE FOR PATTERN TABLE                                                 
         LAY   R1,PATNUM           NUMBER OF PATTERN ENTRIES                    
         MHI   R1,PATELEN          NUM OF ENTRIES * ENTRY LEN                   
*                                                                               
         LR    R0,R1               BUFFER LENGTH                                
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,STRAPLST         START OF ACTIVE PATTERN LIST                 
         ST    R1,ENDAPLST                                                      
*                                                                               
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         MVC   0(4,RE),STRACLST  SAVE ADDRESS OF GETMN BUFFER                   
*                                                                               
         SAM31                                                                  
         MVC   4(4,RE),STRAPLST  SAVE ADDRESS OF ACTIVE PAT LIST                
                                                                                
         MVC   0(3,R1),=X'FFFFFF'                                               
         AR    R1,R0               PLUS LENGTH OF TABLE                         
         ST    R1,MAXTBLSZ         =MAX ACTIVE PATTERN TABLE                    
         SAM24                                                                  
         B     LR32C                                                            
                                                                                
LR32B    DS    0H                                                               
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         MVC   STRACLST,0(RE)                                                   
         MVC   ENDACLST,0(RE)                                                   
                                                                                
         MVC   STRAPLST,4(RE)                                                   
         MVC   ENDAPLST,4(RE)                                                   
         L     RF,STRAPLST         START OF ACTIVE PATTERN LIST                 
         LAY   R1,PATNUM           NUMBER OF PATTERN ENTRIES                    
         MHI   R1,PATELEN          TIMES ENTRY LEN                              
         AR    RF,R1                                                            
         ST    RF,MAXTBLSZ         MAX ACITVE PAT LIST                          
*                                                                               
         LAY   RF,CMLNUM           NUMBER OF ENTRIES                            
         MHI   RF,CMLELEN          TOTAL BUFFER SIZE                            
         L     RE,STRACLST         A(OF LIST)                                   
         AR    RF,RE                                                            
         ST    RF,MAXACLST         MAX ACITVE CML LIST                          
*                                                                               
* RE-INIT PATTERN TABLE                                                         
*                                                                               
         SAM31                                                                  
         L     R1,STRAPLST         START OF ACTIVE PATTERN LIST                 
         ST    R1,ENDAPLST                                                      
         MVC   0(3,R1),=X'FFFFFF'                                               
         SAM24                                                                  
         B     LR33                                                             
*                             RE-INIT TABLE                                     
LR32C    L     RE,STRACLST         START OF ACTIVE CML LIST                     
         ST    RE,ENDACLST                                                      
         L     RF,MAXACLST         MAX ACTIVE CML LIST                          
LR33     SR    RF,RE                                                            
         XCEF                                                                   
         L     RE,STRACLST                                                      
         MVI   0(RE),X'FF'                                                      
*                                                                               
         EJECT                                                                  
* FIRST DELETE ANY FLIGHT RECS FOR THIS CLIENT *                                
*                                                                               
LR34     MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   SVPROF1,C'Y'        BETTER NOT BE BYPASS THIS CLIENT             
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    FLT                                                              
         TM    FTRSW1,FLTSW        DO FLIGHTS                                   
         BZ    BUY                  NO, CK BUYS                                 
*                                                                               
FLT      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FLTKEY,R4                                                        
         MVC   FLTKID,=X'0A27'                                                  
         MVC   FLTKAM,BAGYMD                                                    
         MVC   FLTKCLT,SVBCLT                                                   
         MVC   FLTKPRD,BPRD                                                     
         GOTO1 HIGH                                                             
FLT10    CLC   KEY(5),KEYSAVE                                                   
         BNE   FLTX                                                             
         L     R1,FLTRECR                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,FLTRECR                                                       
         CLI   BPRD,0              PROD ENTERED                                 
         BE    FLT14                NO                                          
         CLC   FLTKPRD,BPRD                                                     
         BNE   FLTX                                                             
FLT14    CLC   FLTKEDT,ENDATE                                                   
         BH    FLT40                                                            
         DROP  R4                                                               
*                                                                               
* PRINT OUT FLIGHT RECORD AND DELETE *                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         L     R1,FLTRECS          CT TO BE DELETED                             
         LA    R1,1(,R1)                                                        
         ST    R1,FLTRECS                                                       
         USING FLTKEY,R6                                                        
         CLI   FLTKPRD,0                                                        
         BE    FLT16                                                            
         LA    R0,FLTKPRD                                                       
         BAS   RE,FPRD                                                          
         MVC   PFPRD,DUB                                                        
FLT16    GOTO1 DATCON,DMCB,(3,FLTKEDT),(5,PFEDATE)                              
         LA    R2,PFDATES                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING FLTDTAEL,R6                                                      
FLT20    GOTO1 DATCON,DMCB,(3,FLTSTART),(5,(R2))                                
         MVI   8(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,FLTEND),(5,9(R2))                                 
*                                                                               
         LA    R2,20(,R2)                                                       
         LA    R1,PFDATES+L'PFDATES                                             
         CR    R1,R2                                                            
         BH    FLT24                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,PFDATES                                                       
*                                                                               
FLT24    BRAS  RE,NEXTEL                                                        
         BE    FLT20                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    FLT40                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    FLT40                                                            
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
         L     R1,FLTRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,FLTRECDL                                                      
*                                                                               
FLT40    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FLT10                                                            
         DROP  R6                                                               
*                                                                               
* END OF FLIGHTS FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *                
*                                                                               
FLTX     LA    R2,FLTRECR                                                       
         LA    R3,=C'FLT'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,FLTRECR                                                       
         LA    R3,AGYCTRS+FLTRECR-COUNTERS                                      
         LA    R4,3                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         EJECT                                                                  
* NEXT DELETE ANY TRAFFIC BUYS FOR THIS CLIENT *                                
*                                                                               
BUY      MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,BUYSW        DO BUYS                                      
         BZ    DLR                  NO CK DLRS                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TBYKEY,R4                                                        
         MVC   TBYPID,=X'0AB2'                                                  
         MVC   TBYPAM,BAGYMD                                                    
         MVC   TBYPCLT,SVBCLT                                                   
         GOTO1 HIGH                                                             
BUY10    CLC   KEY(5),KEYSAVE                                                   
         BNE   BUYX                                                             
         L     R1,BUYRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECR                                                       
         CLI   BPRD,0              PROD ENTERED                                 
         BE    BUY14                NO                                          
         CLC   TBYPPRD,BPRD                                                     
         BNE   BUY50                                                            
*                                                                               
* PRINT OUT BUY RECORD KEY AND CK FOR ELEMS TO DELETE *                         
*                                                                               
BUY14    MVC   SVPRD,TBYPPRD                                                    
         LA    R0,TBYPMKT                                                       
         BRAS  RE,FMS              FORMAT MKT STA TO QMKT/STAPRNT               
         MVI   WRTRECSW,0                                                       
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    BUY20                                                            
         L     R1,BUYRECEM         EMPTY BUY REC                                
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECEM                                                      
         B     BUY46                                                            
         USING TBYDTAEL,R6                                                      
BUY20    L     R1,BUYRECER         ELEM CT READ                                 
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECER                                                      
         CLI   BPRD2,0                                                          
         BE    BUY22                                                            
         CLC   BPRD2,TBYPRD2                                                    
         BNE   BUY40                                                            
BUY22    CLC   ENDATE,TBYEND                                                    
         BL    BUY40                                                            
         L     R1,BUYRECEL         ELEM CT TO BE DELETED                        
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECEL                                                      
         MVC   PBMKT,QMKT          PRINT MARKET/STATION                         
         MVC   PBSTA,STAPRNT                                                    
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   PBSTA(8),STANET                                                  
*                                                                               
         MVC   QMKT,SPACES           BUT ONLY ONCE                              
         MVC   STAPRNT,SPACES                                                   
         MVC   SVSLN,TBYSLN                                                     
         LA    R0,SVPRD                                                         
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PBPRD,WORK                                                       
         CLI   TBYPRD2,0                                                        
         BE    BUY24                                                            
         LA    R0,TBYPRD2                                                       
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PBPTR,WORK                                                       
BUY24    CLI   TBYCODE,0                                                        
         BE    BUY34                                                            
         MVC   PBCODE+1(1),TBYCODE                                              
         CLI   SVCODE,C'E'         COPY CODE = EST                              
         BE    BUY30                                                            
         CLI   TBYCODE,C'A'        IF NOT APLHA, MUST BE EST                    
         BL    BUY30                                                            
         CLI   TBYCODE,C'Z'                                                     
         BNH   BUY34                                                            
         CLI   TBYCODE,C'1'        IF NUMERIC, LEAVE ALONE                      
         BNL   BUY34                                                            
BUY30    LLC   R0,TBYCODE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PBCODE,DUB                                                       
BUY34    GOTO1 DATCON,DMCB,(3,TBYSTART),(5,PBDATES)                             
         MVI   PBDATES+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,TBYEND),(5,PBDATES+9)                             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* DELETE ELEMENT *                                                              
*                                                                               
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         MVI   WRTRECSW,1                                                       
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    BUY36                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    BUY36                                                            
         L     R1,BUYRECED                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECED                                                      
BUY36    CLI   TBYDTAEL,X'10'      THIS NEXT ELEM                               
         BE    BUY20                                                            
         CLI   WRTRECSW,1          THIS RECORD HAVE ELEMS REMOVED               
         BE    BUY44                                                            
         DC    H'0'                                                             
*                                                                               
BUY40    BRAS  RE,NEXTEL                                                        
         BE    BUY20                                                            
         CLI   WRTRECSW,1          THIS RECORD HAVE ELEMS REMOVED               
         BNE   BUY50                                                            
BUY44    L     R1,BUYRECU          CT TO BE UPDATED                             
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECU                                                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   BUY46                                                            
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    BUY50                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    BUY50                                                            
         L     R1,BUYRECW          CT TO BE WRITTEN                             
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECW                                                       
         GOTO1 PUTREC                                                           
         B     BUY50                                                            
BUY46    TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    BUY50                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    BUY50                                                            
         GOTO1 GETREC                                                           
         L     R1,BUYRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,BUYRECDL                                                      
*                                                                               
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVI   KEY+1,X'32'         DELETE ACTIVE KEY                            
         MVC   KEY+5(1),KEY+11                                                  
         MVI   KEY+11,0                                                         
         BAS   RE,DELKEY                                                        
*                                                                               
BUY50    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     BUY10                                                            
         DROP  R4,R6                                                            
*                                                                               
* END OF TRAFFIC BUYS FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *           
*                                                                               
BUYX     OC    BUYRECER,BUYRECER     ANY READ                                   
         BZ    *+14                                                             
         OC    BUYRECEL,BUYRECEL    ANY TO UPDATE                               
         BNZ   BUYX10                                                           
         MVI   FORCEHED,C'N'                                                    
BUYX10   LA    R2,BUYRECER                                                      
         LA    R3,=C'BUY'                                                       
         BRAS  RE,ELEMTOT                                                       
*                                                                               
         LA    R2,BUYRECER                                                      
         LA    R3,AGYCTRS+BUYRECER-COUNTERS                                     
         LA    R4,8                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         SPACE 3                                                                
*                                                                               
*-------------------------------------------------                              
* NEXT DELETE ANY DEALER TAGS FOR THIS CLIENT                                   
*-------------------------------------------------                              
*                                                                               
DLR      MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,DLRSW        DO DLRS                                      
         BZ    INS                  NO CK INSTR                                 
*                                                                               
         BRAS  RE,DDLR             DELETE DEALER TAGS                           
         EJECT                                                                  
*                                                                               
*----------------------------------------------                                 
* NEXT DELETE ANY INSTR RECAP FOR THIS CLIENT                                   
*----------------------------------------------                                 
*                                                                               
INS      MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   SVPROF1,C'A'        ONLY DO CATAGORY A RECS                      
         BE    SHP                  YES                                         
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    INS00                                                            
*                                                                               
         TM    FTRSW1,INSSW        DO INSTRS                                    
         BO    INS00                                                            
         TM    FTRSW1,PATSW+CMLSW  DO PATTERN AND OR COMMERCIAL                 
         BZ    SHP                  NO CK SHIP                                  
*                                                                               
INS00    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM,BAGYMD                                                    
         MVC   INSKCLT,SVBCLT                                                   
         MVC   INSKPRD,BPRD                                                     
         GOTO1 HIGH                                                             
INS10    CLC   KEY(5),KEYSAVE                                                   
         BNE   INSX                                                             
         L     R1,INSRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECR                                                       
         CLI   BPRD,0              PROD ENTERED                                 
         BE    INS14                NO                                          
         CLC   INSKPRD,BPRD                                                     
         BNE   INSX                                                             
*                                                                               
* PRINT OUT INS RECORD KEY AND CK FOR ELEMS TO DELETE *                         
*                                                                               
INS14    LA    R0,INSKMKT                                                       
         BRAS  RE,FMS                                                           
         MVI   WRTRECSW,0                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    INS20                                                            
*                                                                               
         MVI   ELCODE,X'20'        DO DEALER TAGS NOW                           
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    INS20                                                            
*                                                                               
         MVI   ELCODE,X'30'        SPOT ASSIGN/GEN                              
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    INS20                                                            
*                                                                               
         L     R1,INSRECEM         EMPTY INST REC                               
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECEM                                                      
         B     INS66                                                            
         USING INSDTAEL,R6                                                      
INS20    L     R1,INSRECER         ELEM CT READ                                 
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECER                                                      
         CLI   BPRD2,0                                                          
         BE    INS22                                                            
         CLC   BPRD2,INSPRD2                                                    
         BNE   INS50                                                            
         EJECT                                                                  
* FIND LAST TELECAST DATE TO CHECK FOR PURGE *                                  
*                                                                               
INS22    LLC   RF,INSDTALN                                                      
         LA    R3,0(R6,RF)                                                      
         SHI   R3,2                                                             
         CLC   ENDATEP,0(R3)       LAST TELECAST DATE                           
         BNL   INS24                                                            
         BRAS  RE,APL              ADD TO ACTIVE PATTERN LIST                   
         B     INS50                                                            
*                                                                               
INS24    L     R1,INSRECEL         ELEM CT TO BE DELETED                        
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECEL                                                      
         TM    FTRSW2,PRTSW                                                     
         BZ    INS40                                                            
         LA    R0,INSPRD1                                                       
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PIPRD,WORK                                                       
         CLI   INSPRD2,0                                                        
         BE    INS26                                                            
         LA    R0,INSPRD2                                                       
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PIPTR,WORK                                                       
INS26    MVC   PIMKT,QMKT                                                       
         MVC   PISTA,STAPRNT                                                    
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   PISTA(8),STANET                                                  
*                                                                               
         MVC   QMKT,SPACES                                                      
         MVC   STAPRNT,SPACES                                                   
         CLI   INSKCOPY,0                                                       
         BE    INS28                                                            
         TM    INSFLAG,X'20'       COPY CODE = EST                              
         BO    *+14                                                             
         MVC   PICODE+1(1),INSKCOPY                                             
         B     INS28                                                            
         LLC   R0,INSKCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PICODE,DUB                                                       
         MVC   PICODE+132,=C'EST'                                               
*                                                                               
INS28    LLC   R0,INSREV           REVISION                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PIREV,DUB                                                        
         CLI   INSREV,0                                                         
         BNE   *+10                                                             
         MVC   PIREV,=C'ORG'                                                    
         GOTO1 DATCON,DMCB,(2,INSDATE),(5,PIINSTDT)                             
*                                                                               
         LLC   R3,INSDTALN                                                      
         SHI   R3,(INSPTTN-INSDTAEL)                                            
         SR    R2,R2                                                            
         D     R2,=A(INSSUBEL)                                                  
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,INSPTTN                                                       
*                                                                               
INS30    CLI   ELCODE,X'20'        DEALER TAGS                                  
         BE    INS36                YES                                         
*                                                                               
         OC    0(3,R2),0(R2)       HIATUS                                       
         BNZ   INS32                NO                                          
         MVC   PIREF(6),=C'HIATUS'                                              
         B     INS36                                                            
*                                                                               
INS32    CLC   0(3,R2),=X'FFFFFF'  TBA (TO BE ANNOUNCED)                        
         BNE   INS34                NO                                          
         MVC   PIREF(3),=C'TBA'                                                 
         B     INS36                                                            
INS34    SR    RE,RE                                                            
         ICM   RE,7,0(R2)                                                       
         SRDL  RE,10                                                            
         X     RE,=XL4'00003FFF'                                                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PIREF,DUB                                                        
         LTR   RE,RE               MUST NOT BE ZERO                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SRL   RF,22                                                            
         X     RF,=XL4'000003FF'                                                
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PISUB,DUB                                                        
         LTR   RF,RF               MUST NOT BE ZERO                             
         BNZ   INS36                                                            
         CLI   3(R2),X'54'         IF BACK IN 1984, OK                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PISUB+2,C'1'                                                     
*                                                                               
INS36    GOTO1 DATCON,DMCB,(2,3(R2)),(5,PIFTDLTD)                               
         MVI   PIFTDLTD+8,C'-'                                                  
         GOTO1 DATCON,DMCB,(2,5(R2)),(5,PIFTDLTD+9)                             
*                                                                               
         CLI   ELCODE,X'20'        THIS DEALER TAGS                             
         BNE   *+10                                                             
         MVC   PIDEALER,=C'DEALER TAG'                                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,INSSUBEL(,R2)                                                 
         BCT   R3,INS30                                                         
*                                                                               
* DELETE ELEMENT *                                                              
*                                                                               
INS40    GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         MVI   WRTRECSW,1                                                       
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    INS46                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    INS46                                                            
         L     R1,INSRECED                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECED                                                      
INS46    CLI   INSDTAEL,X'10'      THIS NEXT ELEM                               
         BE    INS20                                                            
         B     INS62                                                            
*                                                                               
INS50    BRAS  RE,NEXTEL                                                        
         BE    INS20                                                            
         CLI   ELCODE,X'20'        DONE DEALER TAGS YET                         
         BE    INS60                                                            
         MVI   ELCODE,X'20'        DO DEALER TAGS NOW                           
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    INS20                                                            
*                                                                               
INS60    CLI   WRTRECSW,1          THIS RECORD HAVE ELEMS REMOVED               
         BNE   INS70                                                            
INS62    L     R1,INSRECU          CT UPDATED                                   
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECU                                                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    INS64                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   INS66                                                            
INS64    TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    INS70                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    INS70                                                            
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    INS65                                                            
*                                                                               
         TM    FTRSW1,INSSW        DO INSTRS                                    
         BZ    INS70               NO, BYPASS DELETE                            
*                                                                               
INS65    L     R1,INSRECW          CT WRITTEN                                   
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECW                                                       
*                                                                               
         GOTO1 PUTREC                                                           
         B     INS70                                                            
INS66    TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    INS70                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    INS70                                                            
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    INS68                                                            
*                                                                               
         TM    FTRSW1,INSSW        DO INSTRS                                    
         BZ    INS70               NO, BYPASS DELETE                            
*                                                                               
INS68    GOTO1 GETREC                                                           
         L     R1,INSRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,INSRECDL                                                      
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
INS70    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     INS10                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
* END OF INST RECAPS FOR CLIENT, SORT ACTIVE      *                             
* PATTERN LIST, PRINT TOTALS, ADD TO MEDIA TOTALS *                             
*                                                                               
INSX     OC    INSRECER,INSRECER   ANY READ                                     
         BZ    INSX20                                                           
*                                                                               
         SAM31                                                                  
         CLC   STRAPLST,ENDAPLST  ANY ENTRIES IN LIST                           
         BE    INSX10                                                           
         L     R4,STRAPLST         START OF ACTIVE PATTERN LIST                 
         L     R3,ENDAPLST                                                      
         SR    R3,R4               GET LIST LEN                                 
         SR    R2,R2                                                            
         D     R2,=A(APLISTLN)     ENTRY LEN                                    
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CHI   R3,1                ANYTHING TO SORT                             
         BNH   INSX06                                                           
*                                                                               
*NOP     GOTO1 XSORT,DMCB,(R4),(R3),APLISTLN,APLISTLN,0                         
*                                                                               
* 31-BIT CALL                                                                   
         GOTO1 XSORT,DMCB,X'00FFFFFF',(R3),APLISTLN,APLISTLN,0,(R4)             
*                                                                               
INSX06   L     R3,ENDAPLST                                                      
         MVC   0(3,R3),=X'FFFFFF'                                               
         C     R3,MAXTBLSZ         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R3),X'FF'                                                      
*                                                                               
INSX10   SAM24                                                                  
         OC    INSRECEL,INSRECEL   ANY TO UPDATE                                
         BNZ   INSX30                                                           
INSX20   MVI   FORCEHED,C'N'                                                    
*                                                                               
INSX30   LA    R2,INSRECER                                                      
         LA    R3,=C'INS'                                                       
         BRAS  RE,ELEMTOT                                                       
*                                                                               
*                                                                               
         LA    R2,INSRECER                                                      
         LA    R3,AGYCTRS+INSRECER-COUNTERS                                     
         LA    R4,8                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         EJECT                                                                  
* NEXT DELETE ANY SHIP RECAP FOR THIS CLIENT *                                  
*                                                                               
SHP      MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    SHP00                                                            
         TM    FTRSW1,SHPSW        DO SHIP                                      
         BO    SHP00                                                            
         TM    FTRSW1,PATSW+CMLSW  DO PATTERN AND OR COMMERCIAL                 
         BZ    PAT                  NO CK PAT                                   
*                                                                               
SHP00    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SHPKEY,R4                                                        
         MVC   SHPKID,=X'0A25'                                                  
         MVC   SHPKAM,BAGYMD                                                    
         MVC   SHPKCLT,SVBCLT                                                   
         GOTO1 HIGH                                                             
SHP10    CLC   KEY(5),KEYSAVE                                                   
         BNE   SHPX                                                             
         L     R1,SHPRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECR                                                       
*                                                                               
* PRINT OUT SHIP RECORD KEY AND CK FOR ELEMS TO DELETE *                        
*                                                                               
         LA    R0,SHPKMKT                                                       
         BRAS  RE,FMS                                                           
         MVI   WRTRECSW,0                                                       
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    SHP20                                                            
         L     R1,SHPRECEM         EMPTY SHIP REC                               
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECEM                                                      
         B     SHP36                                                            
         USING SHPDTAEL,R6                                                      
SHP20    L     R1,SHPRECER         ELEM CT READ                                 
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECER                                                      
         CLC   ENDATE,SHPLTD                                                    
         BL    SHP30                                                            
         L     R1,SHPRECEL         ELEM CT TO BE DELETED                        
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECEL                                                      
         TM    FTRSW2,PRTSW                                                     
         BZ    SHP26                                                            
         MVC   PSMKT,QMKT                                                       
         MVC   PSSTA,STAPRNT                                                    
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   PSSTA(8),STANET                                                  
*                                                                               
         MVC   PSCML,SHPCMML                                                    
         MVC   PSCML2,SHPCMML2                                                  
         GOTO1 DATCON,DMCB,(3,SHPQDATE),(5,PSINSTDT)                            
         OC    SHPSHPDT,SHPSHPDT   EVER SHIPPED                                 
         BZ    SHP22                                                            
         GOTO1 DATCON,DMCB,(3,SHPSHPDT),(5,PSHIPDT)                             
SHP22    GOTO1 DATCON,DMCB,(3,SHPFTD),(5,PSFTD)                                 
         GOTO1 DATCON,DMCB,(3,SHPLTD),(5,PSLTD)                                 
*                                                                               
         LA    RE,PSSTATUS                                                      
         TM    SHPNOSHP,X'80'      NO SHIP                                      
         BZ    *+14                                                             
         MVC   0(2,RE),=C'NS'                                                   
         LA    RE,2(,RE)                                                        
*                                                                               
         TM    SHPNOSHP,X'40'      MANUAL SHIP                                  
         BZ    SHP24                                                            
         CR    RE,RF                                                            
         BE    *+12                                                             
         MVI   0(RE),C','                                                       
         LA    RE,1(,RE)                                                        
         MVC   0(2,RE),=C'MS'                                                   
*                                                                               
SHP24    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* DELETE ELEMENT *                                                              
*                                                                               
SHP26    GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         MVI   WRTRECSW,1                                                       
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    SHP28                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    SHP28                                                            
         L     R1,SHPRECED                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECED                                                      
SHP28    CLI   SHPDTAEL,X'10'      THIS NEXT ELEM                               
         BE    SHP20                                                            
         B     SHP34                                                            
*                                                                               
* CK COMMLS TO BE IN ACTIVE LIST *                                              
*                                                                               
SHP30    MVI   ADIDFLAG,C'N'                                                    
         TM    SHPNOSHP,SHPISADI                                                
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         BAS   RE,CCL              GO CHECK IF IN ACTIVE CML LIST               
         BRAS  RE,NEXTEL                                                        
         BE    SHP20                                                            
         CLI   WRTRECSW,1          THIS RECORD HAVE ELEMS REMOVED               
         BNE   SHP40                                                            
SHP34    L     R1,SHPRECU          CT TO BE UPDATED                             
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECU                                                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   SHP36                                                            
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    SHP40                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    SHP40                                                            
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    SHP35                                                            
*                                                                               
         TM    FTRSW1,SHPSW        DO SHIP                                      
         BZ    SHP40               NO, BYPASS DELETE                            
*                                                                               
SHP35    L     R1,SHPRECW          CT TO BE WRITTEN                             
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECW                                                       
*                                                                               
         GOTO1 PUTREC                                                           
         B     SHP40                                                            
SHP36    TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    SHP40                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    SHP40                                                            
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    SHP38                                                            
*                                                                               
         TM    FTRSW1,SHPSW        DO SHIP                                      
         BZ    SHP40                NO, BYPASS SHP DELETE                       
*                                                                               
SHP38    L     R1,SHPRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,SHPRECDL                                                      
         GOTO1 GETREC                                                           
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
SHP40    MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
*                                                                               
         GOTO1 HIGH                DUMMY READ HI FOR SEQ                        
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMOUTBTS,X'02'      RESET FOR DELETE                             
*                                                                               
         CLC   KEY(13),KEYSAVE     MUST FIND KEY JUST PROCESSED                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                                                              
         B     SHP10                                                            
         DROP  R4,R6                                                            
* END OF SHIPPING RECAPS FOR CLIENT, SORT ACTIVE   *                            
* COMMERCIAL LIST, PRINT TOTALS, ADD TO MEDIA TOTALS  *                         
*                                                                               
SHPX     OC    SHPRECER,SHPRECER   ANY READ                                     
         BZ    *+14                                                             
         OC    SHPRECEL,SHPRECEL   ANY TO UPDATE                                
         BNZ   SHPX10                                                           
         MVI   FORCEHED,C'N'                                                    
SHPX10   LM    R2,R3,STRACLST                                                   
         LR    R4,R3                                                            
         CR    R2,R3                                                            
         BE    SHPX20                                                           
         SR    R3,R2                                                            
         SR    R0,R0                                                            
         LR    R1,R3                                                            
         D     R0,=F'12'           DIV BY THE LENGTH OF AN ENTRY                
         LR    R3,R1                                                            
*                                                                               
         GOTO1 XSORT,DMCB,(R2),(R3),12,12,0                                     
*                                                                               
SHPX20   MVI   0(R4),X'FF'                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,SHPRECER            1ST CTR                                   
         LA    R3,=C'SHP'                                                       
         BRAS  RE,ELEMTOT                                                       
*                                                                               
         LA    R2,SHPRECER                                                      
         LA    R3,AGYCTRS+SHPRECER-COUNTERS                                     
         LA    R4,8                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         EJECT                                                                  
* PURGE PATTERN RECORDS *                                                       
*                                                                               
PAT      CLI   SVPROF1,C'A'        ONLY DO CATAGORY A RECS                      
         BE    CKEOJ                YES                                         
*                                                                               
         MVI   RCSUBPRG,6                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R5,STRAPLST         START OF ACTIVE PATTERN LIST                 
         USING APLIST,R5                                                        
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    PAT00                                                            
         TM    FTRSW1,PATSW        DO PATTERNS                                  
         BO    PAT00                                                            
         TM    FTRSW1,CMLSW        DO COMMERCIAL                                
         BZ    PATXX                NO CK INV                                   
*                                                                               
PAT00    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT,SVBCLT                                                   
         MVC   PATKPRD(4),BPRD                                                  
         GOTO1 HIGH                                                             
PAT10    SAM31                                                                  
         C     R5,MAXTBLSZ         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
         SAM24                                                                  
         CLC   KEY(5),KEYSAVE                                                   
         BNE   PATX                                                             
         L     R1,PATRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,PATRECR                                                       
         CLI   BPRD,0              PROD ENTERED                                 
         BE    PAT14                NO                                          
         CLC   PATKPRD,BPRD                                                     
         BNE   PAT80                                                            
PAT14    CLI   BPRD2,0             PROD2 ENTERED                                
         BE    PAT16                NO                                          
         CLC   PATKPRD2,BPRD2                                                   
         BNE   PAT80                                                            
*                                                                               
PAT16    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    PAT16F                                                           
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
PAT16F   MVI   ADIDFLAG,C'N'                                                    
         TM    PATSTAT1,PATSADID                                                
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
PAT18    DS    0H                                                               
         SAM31                                                                  
         CLC   APLENT,PATKPRD      THIS AN ACTIVE PATTERN                       
         SAM24                                                                  
         BE    PAT60                YES                                         
         BL    PAT78                                                            
*NOP     BL    PAT70                MISSING PATTERN                             
         EJECT                                                                  
* THIS PATTERN IS NOT USED IN ANY RECAP *                                       
*                                                                               
         CLC   PATEND,=X'FFFFFF'   RECALL UFN                                   
         BNE   *+14                                                             
         CLC   PATSTART,ENDATE     RELEASE BEFORE PURGE DATE                    
         BNH   *+14                 YES, DELETE IT                              
*                                                                               
         CLC   ENDATE,PATEND       RECALL DATE BEFORE PURGE DATE                
         BL    PAT64                GO ADD COMMLS TO ACTIVE LIST                
         L     R1,PATRECS          REC CT TO BE DELETED                         
         LA    R1,1(,R1)                                                        
         ST    R1,PATRECS                                                       
*                                                                               
* PRINT OUT PATTERN RECORD FOR DELETE LIST *                                    
*                                                                               
         LA    R0,PATKPRD                                                       
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PPPRD,WORK                                                       
         CLI   PATKPRD2,0                                                       
         BE    PAT22                                                            
         LA    R0,PATKPRD2                                                      
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PPPTR,WORK                                                       
PAT22    CLI   PATKCODE,0                                                       
         BE    PAT24                                                            
         CLI   PATDTALN,38         OLD PATTERN                                  
         BE    *+12                 YES, NO STATUS BYTE                         
         TM    PATSTAT,X'10'       COPY CODE = EST                              
         BO    *+14                                                             
         MVC   PPCODE+1(1),PATKCODE                                             
         B     PAT24                                                            
         LLC   R0,PATKCODE                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPCODE,DUB                                                       
         MVC   PPCODE+132,=C'EST'                                               
PAT24    SR    RE,RE                                                            
         ICM   RE,7,PATKREF                                                     
         SRDL  RE,10                                                            
         X     RE,=XL4'00003FFF'                                                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPREF,DUB                                                        
         SRL   RF,22                                                            
         X     RF,=XL4'000003FF'                                                
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPSUB,DUB                                                        
         MVC   PPDESC,PATDESC                                                   
         GOTO1 DATCON,DMCB,(3,PATSTART),(5,PPFTDLTD)                            
         MVI   PPFTDLTD+8,C'-'                                                  
         GOTO1 DATCON,DMCB,(3,PATEND),(5,PPFTDLTD+9)                            
         CLC   PATEND,=X'FFFFFF'                                                
         BNE   *+10                                                             
         MVC   PPFTDLTD+9(8),=CL8'UFN'                                          
*                                                                               
         OC    PATUSED,PATUSED     EVER USED                                    
         BNZ   PAT26                YES                                         
         MVC   PPUSED(5),=C'NEVER'                                              
         B     PAT28                                                            
PAT26    GOTO1 DATCON,DMCB,(3,PATUSED),(5,PPUSED)                               
*                                                                               
PAT28    CLI   PATDTALN,38         OLD PATTERN                                  
         BE    PAT30                YES, NO STATUS BYTE                         
         TM    PATSTAT,X'80'       DELETED PATTN                                
         BZ    PAT30                                                            
         MVC   PPDEL,=C'DEL PAT'                                                
*                                                                               
* CHECK FOR PATTERN TEXT *                                                      
*                                                                               
PAT30    MVI   ELCODE,X'50'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PAT40                                                            
*                                                                               
         MVC   PPTXT(3),=C'TX='                                                 
         MVC   PPTXT+3(7),2(R6)                                                 
*                                                                               
         BRAS  RE,PTXT              PAT TEXT & DEL ELEM IN TEXT REC             
*                                                                               
PAT40    L     R6,AIO                                                           
*                                                                               
* PRINT TYPE/AFFIL/MARKET/STATION LIST *                                        
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,PLST                                                          
*                                                                               
PAT50    MVI   ELCODE,X'31'        PRESET TO BPAT                               
         TM    KEY+13,X'01'        BPAT RECORD                                  
         BO    *+8                                                              
         MVI   ELCODE,X'30'        FOR CML LIST                                 
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    PAT50C                                                           
*                                                                               
         TM    KEY+13,X'02'        INCOMPLETE PATTERN                           
         BO    PAT80                YES, NO CML TO PROCESS                      
         DC    H'0'                                                             
*                                                                               
PAT50C   LR    R3,R6                                                            
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    PAT50E                                                           
         CLC   =C'HIATUS',2(R3)    MUST BE HIATUS PATTERN                       
         BE    PAT52                                                            
         CLI   0(R3),X'31'         OR BPAT                                      
         BE    PAT52                                                            
         DC    H'0'                                                             
                                                                                
PAT50E   LLC   R1,1(R6)                                                         
         SHI   R1,3                                                             
         LA    RE,P3                                                            
         CLC   SPACES,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,132(,RE)                                                      
         B     *-14                                                             
         MVC   PPPTR-P(4,RE),=C'ROT='                                           
         EX    R1,PATMVC                                                        
         B     PAT52                                                            
PATMVC   MVC   PPPTR-P+4(0,RE),2(R6)                                            
*                                                                               
* PRINT COMMERCIALS *                                                           
*                                                                               
PAT52    LR    R6,R3                                                            
         CLI   0(R6),X'31'         BPAT?                                        
         BNE   PAT53                                                            
                                                                                
         USING PATBCMEL,R6                                                      
                                                                                
PAT52C   LLC   R2,PATBCMLN                                                      
         SRL   R2,4                DIVIDE BY 16                                 
         LA    R3,PATBCML                                                       
         B     PAT53C                                                           
         DROP  R6                                                               
                                                                                
         USING PATCMLEL,R6                                                      
PAT53    LLC   R2,PATCMLLN                                                      
         SRL   R2,4                DIVIDE BY 16                                 
         LA    R3,PATCML                                                        
PAT53C   LA    R1,PPCMLS                                                        
         ST    R1,SVREG            SAVE R1                                      
*                                                                               
PAT54    L     R1,SVREG            RESTORE R1                                   
         MVC   0(8,R1),0(R3)                                                    
         OC    8(8,R3),8(R3)       SECOND CMML                                  
         BZ    *+14                                                             
         MVI   8(R1),C'-'                                                       
         MVC   9(8,R1),8(R3)                                                    
*                                                                               
         MVC   WORK(8),0(R3)                                                    
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PAT54C                                                           
*                                                                               
         ST    R1,SVREG            SAVE R1                                      
         GOTO1 VTRPACK,DMCB,(C'U',0(R3)),WORK                                   
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         L     R1,SVREG            RESTORE R1                                   
*                                                                               
         MVC   0(12,R1),WORK                                                    
*                                                                               
         OC    8(8,R3),8(R3)       SECOND CMML                                  
         BZ    PAT54C                                                           
         MVC   WORK(8),8(R3)                                                    
         MVC   WORK+8(4),SPACES                                                 
*                                                                               
         ST    R1,SVREG            SAVE R1                                      
         GOTO1 VTRPACK,DMCB,(C'U',8(R3)),WORK                                   
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         L     R1,SVREG            RESTORE R1                                   
*                                                                               
         LA    RF,8(R1)                                                         
         CLI   0(RF),X'40'         SPACE                                        
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C'-'                                                       
         MVC   1(12,RF),WORK                                                    
*                                                                               
PAT54C   LA    R1,132(,R1)                                                      
         LA    R0,PPCMLS+528       USED 4 PRINT LINES                           
         CR    R0,R1                                                            
         BH    PAT56                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R1,PPCMLS                                                        
PAT56    ST    R1,SVREG            SAVE R1                                      
         LA    R3,16(,R3)                                                       
         BCT   R2,PAT54                                                         
         CLC   P,SPACES            STILL NEED TO PRINT                          
         BE    PAT58                NO                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PAT58    CLI   0(R6),X'31'         BPAT?                                        
         BNE   PAT58C                                                           
         BRAS  RE,NEXTEL           YES, GET NEXT ELEM                           
         BE    PAT52C                                                           
*                                                                               
PAT58C   TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    PAT80                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    PAT80                                                            
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    PAT59                                                            
*                                                                               
         TM    FTRSW1,PATSW        DO PAT                                       
         BZ    PAT80                NO, BYPASS DELETE                           
*                                                                               
PAT59    L     R1,PATRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,PATRECDL                                                      
*                                                                               
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
         B     PAT80                                                            
         SPACE 2                                                                
* THIS IS AN ACTIVE PATTERN                                                     
*                                                                               
PAT60    SAM31                                                                  
*NOP     LA    R5,APLNEXT                                                       
         AHI   R5,APLISTLN                                                      
         C     R5,MAXTBLSZ         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
         SAM24                                                                  
*                                                                               
PAT64    L     R6,AIO1                                                          
         MVI   ELCODE,X'31'        PRESET TO BPAT                               
         TM    KEY+13,X'01'        BPAT RECORD                                  
         BO    *+8                                                              
         MVI   ELCODE,X'30'        FOR CML LIST                                 
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    PAT66                                                            
*                                                                               
         TM    KEY+13,X'02'        INCOMPLETE PATTERN                           
         BO    PAT80                YES, NO CML TO PROCESS                      
*                                                                               
         DC    H'0'                                                             
*                                                                               
* BUILD ACTIVE COMMERCIAL LIST *                                                
*                                                                               
PAT66    BRAS  RE,ACL              GO ADD TO ACTIVE CML LIST                    
         B     PAT80                                                            
         EJECT                                                                  
*&&DO                                                                           
*                                                                               
* ACTIVE PATTERN HAS NOT BEEN FOUND - COULD BE FROM COPY CODED *                
* INSTR RECAP THAT USED A NON-COPY CODED PATTERN, SO SEE IF    *                
* MISSING PATTERN IS IN ACTIVE LIST WITH BLANK COPY CODE       *                
*                                                                               
PAT70    CLI   APLCODE,0           IF ZERO COPY CODE                            
         BE    PAT74                NO SEARCH                                   
*                                                                               
* SEE IF PATTERN EXISTS WITH ZERO COPY CODE *                                   
*                                                                               
         L     RE,STRAPLST         START OF ACTIVE PATTERN LIST                 
         MVC   WORK(L'APLENT),APLENT                                            
         MVI   WORK+APLCODE-APLENT,0 ZERO COPY CODE                             
PAT72    CR    RE,R5               UP TO CURRENT ENTRY                          
         BE    PAT74                                                            
         CLC   WORK(L'APLENT),0(RE) EQUAL ENTRY                                 
         BE    PAT78                NOT MISSING, THEN                           
         LA    RE,L'APLENT(,RE)                                                 
         B     PAT72                                                            
*                                                                               
* PRINT OUT MISSING PATTERN KEY FROM INSTR RECAP RECORD *                       
*                                                                               
PAT74    LA    R0,APLENT                                                        
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PPPRD,WORK                                                       
         CLI   APLPRD2,0                                                        
         BE    *+18                                                             
         LA    R0,APLPRD2                                                       
         BRAS  RE,FPS              GET PRINTABLE PROD & LEN                     
         MVC   PPPTR,WORK                                                       
         CLI   APLCODE,0                                                        
         BE    PAT76                                                            
         CLI   APLCODE,C'Z'        COPY CODE                                    
         BH    *+22                                                             
         CLI   APLCODE,C'A'        COPY CODE                                    
         BL    *+14                                                             
         MVC   PPCODE+1(1),APLCODE                                              
         B     PAT76                                                            
         LLC   R0,APLCODE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPCODE,DUB                                                       
         MVC   PPCODE+132,=C'EST'                                               
PAT76    SR    RE,RE                                                            
         ICM   RE,7,APLREFS                                                     
         SRDL  RE,10                                                            
         X     RE,=XL4'00003FFF'                                                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPREF,DUB                                                        
         SRL   RF,22                                                            
         X     RF,=XL4'000003FF'                                                
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPSUB,DUB                                                        
         MVC   PPDESC(34),=CL34'MISSING PATTERN (FROM INSTR RECAP)'             
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*&&                                                                             
*                                                                               
PAT78    SAM31                                                                  
         C     R5,ENDAPLST         AT END OF LIST                               
         BNL   PAT18               CK NEXT ENTRY AGAINST SAME PATTERN           
*NOP     LA    R5,APLNEXT                                                       
         AHI   R5,APLISTLN                                                      
         SAM24                                                                  
         B     PAT18               CK NEXT ENTRY AGAINST SAME PATTERN           
         EJECT                                                                  
* GET NEXT SEQUENTIAL PATTERN RECORD *                                          
*                                                                               
PAT80    MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
*                                                                               
         GOTO1 HIGH                DUMMY READ HI FOR SEQ                        
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMOUTBTS,X'02'      RESET FOR DELETE                             
*                                                                               
         CLC   KEY(13),KEYSAVE     MUST FIND KEY JUST PROCESSED                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     PAT10                                                            
         DROP  R4,R5,R6                                                         
         SPACE 3                                                                
* AT END OF PATTERNS FOR CLIENT, SORT ACTIVE         *                          
* COMMERCIAL LIST, PRINT TOTALS, ADD TO MEDIA TOTALS *                          
*                                                                               
PATX     OC    PATRECR,PATRECR     ANY READ                                     
         BZ    *+14                                                             
         OC    PATRECS,PATRECS    ANY TO UPDATE                                 
         BNZ   PATX10                                                           
         MVI   FORCEHED,C'N'                                                    
PATX10   LM    R2,R3,STRACLST                                                   
         LR    R4,R3                                                            
         CR    R2,R3               ANY ACTIVE COMMERCIALS FOUND                 
         BE    PATX20               NO                                          
         SR    R3,R2                                                            
         SR    R0,R0                                                            
         LR    R1,R3                                                            
         D     R0,=F'12'           DIV BY THE LENGTH OF AN ENTRY                
         LR    R3,R1                                                            
*                                                                               
         GOTO1 XSORT,DMCB,(R2),(R3),12,12,0                                     
*                                                                               
PATX20   MVI   0(R4),X'FF'                                                      
*                                                                               
         LA    R2,PATRECR                                                       
         LA    R3,=C'PAT'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,PTXRECR                                                       
         LA    R3,=C'PTX'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,PATRECR                                                       
         LA    R3,AGYCTRS+PATRECR-COUNTERS                                      
         LA    R4,6                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
PATXX    BRAS  RE,INV                                                           
         EJECT                                                                  
* PURGE COMMERCIAL RECORDS *                                                    
*                                                                               
CML      MVI   RCSUBPRG,7                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,CMLSW        DO COMMERCIALS                               
         BZ    CKEOJ                NO, CHECK EOJ                               
*                                                                               
         L     R5,STRACLST         START OF ACTIVE COMML LIST                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         GOTO1 HIGH                                                             
CML05    CLC   KEY(5),KEYSAVE                                                   
         BNE   CMLX                                                             
*                                                                               
         MVC   SVMYKEY,KEY         SAVE KEY                                     
*                                                                               
         OC    CMLKCML,CMLKCML     CMML SEQ REC                                 
         BZ    CML60                YES                                         
*                                                                               
         CLC   CMLKCML,=8C'9'      PROD HOUSE REC                               
         BE    CML60                YES                                         
*                                                                               
         L     R1,CMLRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,CMLRECR                                                       
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVC   SVCML,CMLKCML                                                    
         MVC   SVCML+8(4),SPACES                                                
         MVC   WORK(8),CMLKCML                                                  
         MVC   WORK+8(4),SPACES                                                 
         TM    KEY+13,CMLKSTA_PCKD                                              
         BZ    CML08                                                            
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',CMLKCML),WORK                                 
*                                                                               
CML08    CLC   WORK(12),0(R5)       THIS COMML ACTIVE                           
         BE    CML55                                                            
         BL    CML14                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
*        MVC   P+5(16),=C'BAD INVOICE CML='                                     
*        MVC   P+21(12),0(R5)                                                   
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CML10    LA    R5,12(R5)           BUMP IN ACTIVE CML LIST                      
*                                                                               
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    CML14               YES, PROCESS CML                             
*                                                                               
         C     R5,ENDACLST         END OF TABLE                                 
         BL    CML08                NO                                          
*                                                                               
CML14    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    CML18                                                            
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
CML18    DS   0H                                                                
         TM    CMLSTAT,X'20'        THIS AN EASI COMML                          
         BZ    CML19                 NO, CONTINUE CHECKING                      
         L     R0,CEASICT                                                       
         AHI   R0,1                                                             
         ST    R0,CEASICT                                                       
         B     CML60                 BYPASS, DO NOT DELETE                      
*                                                                               
CML19    DS   0H                                                                
         MVC   SVCRLSE,CMLRLSE     SAVE RELEASE DATE                            
         MVC   SVCRCL,CMLRCL        AND RECALL DATES                            
         CLC   CMLRCL,=X'FFFFFF'    RECALL UFN                                  
         BE    CML60               YES, KEEP IT                                 
*                                                                               
*NOP CODE BELOW TO KEEP UFN RECORDS                                             
*        BNE   *+14                                                             
*        CLC   SVCRLSE,ENDATE      RELEASE BEFORE PURGE DATE                    
******   BNH   CML20                NO, ONLY PRINT AS INACTIVE                  
*                                                                               
         CLC   ENDATE,CMLRCL       RECALL DATE BEFORE END DATE                  
         BL    CML60                                                            
*                                                                               
CML20    CLI   BPRD,0              JUST 1 PROD                                  
         BE    *+12                 NO                                          
         BRAS  RE,CCP              CK COMML PRODUCT                             
         BNE   CML60                                                            
*                                                                               
         L     R1,CMLRECS          REC CT TO BE DELETED                         
         LA    R1,1(,R1)                                                        
         ST    R1,CMLRECS                                                       
*                                                                               
* PRINT OUT COMMERCIAL RECORD FOR DELETE/INACTIVE LIST *                        
*                                                                               
         MVC   PCCML,WORK                                                       
         MVC   SVCMLCOD,WORK                                                    
         MVC   PCTITLE,CMLTITLE                                                 
         LLC   RE,CMLSLN                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PCLEN,DUB                                                        
         MVC   PCTYPE,CMLTYPE                                                   
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,PCDATES)                              
         MVI   PCDATES+8,C'-'                                                   
         CLC   CMLRCL,=X'FFFFFF'                                                
         BNE   *+14                                                             
         MVC   PCDATES+9(8),=CL8'UFN'                                           
         B     CML22                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRCL),(5,PCDATES+9)                             
CML22    MVC   PCCLTCML,CMLCLTNO                                                
         MVC   SVCMLSEQ,CMLSEQ                                                  
*                                                                               
         TM    CMLSTAT,X'80'       WAS COMML DELETED                            
         BZ    *+10                                                             
         MVC   PCINACT+20(11),=C'DELETED CML'                                   
*                                                                               
         TM    CMLSTAT,X'40'       COMML TEXT FOR THIS                          
         BZ    *+10                                                             
         MVC   PCINACT+32(10),=C'COMML TEXT'                                    
*                                                                               
         MVC   SVCMLSTA,CMLSTAT                                                 
*                                                                               
* PRINT PRODUCT LIST *                                                          
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PCTITLE+132+4(7),=C'PRD(S)='                                     
         LA    R2,2(R6)                                                         
         LLC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         LA    R1,PCTITLE+132+4+7                                               
         ST    R1,DUB+4                                                         
         B     CML26                                                            
CML24    MVI   0(R1),C','                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,DUB+4                                                         
CML26    LA    R0,0(,R2)           ADDR OF PRD IN LIST                          
         BAS   RE,FPRD                                                          
         L     R1,DUB+4                                                         
         MVC   0(3,R1),DUB                                                      
         CLI   2(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,3(,R1)                                                        
         ST    R1,DUB+4                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,CML24                                                         
         CLC   SVCRCL,=X'FFFFFF'   RECALL DATE UFN                              
         BNE   CML30                                                            
         MVC   PCINACT(9),=C'INACT CML'                                         
         CLC   SVCRLSE,ENDATE      RELEASE BEFORE PURGE DATE                    
         BH    CML30                                                            
         MVC   PCINACT+10(9),=C'TO BE DEL'                                      
*                                                                               
CML30    MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLC   SVCRCL,=X'FFFFFF'   RECALL DATE UFN                              
         BNE   *+14                                                             
         CLC   SVCRLSE,ENDATE      RELEASE BEFORE PURGE DATE                    
         BH    CML60                                                            
*                                                                               
         MVC   SVKEY,KEY                                                        
         DROP  R6                                                               
*                                                                               
         XC    SVADIDP,SVADIDP                                                  
         XC    SVHIDEF,SVHIDEF                                                  
         XC    SVCNTRCT,SVCNTRCT                                                
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'24'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CML32                                                            
*                                                                               
         USING CMLXDTEL,R6                                                      
*                                                                               
         CLC   CMLXHDPK,SPACES                                                  
         BE    CML30C                                                           
         OC    CMLXHDPK,CMLXHDPK                                                
         BZ    CML30C                                                           
         MVC   SVHIDEF,CMLXHDPK    SAVE HIDEF                                   
*                                                                               
CML30C   CLC   CMLXCCPK,SPACES                                                  
         BE    CML32                                                            
         OC    CMLXCCPK,CMLXCCPK                                                
         BZ    CML32                                                            
         MVC   SVCNTRCT,CMLXCCPK   SAVE CENTERCUT                               
*                                                                               
         DROP  R6                                                               
*                                                                               
CML32    L     R6,AIO1                                                          
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CML33                                                            
                                                                                
         USING CMLADIEL,R6                                                      
                                                                                
         CLC   CMLADIDP,SPACES                                                  
         BE    CML33                                                            
         OC    CMLADIDP,CMLADIDP                                                
         BZ    CML33                                                            
         MVC   SVADIDP,CMLADIDP                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
CML33    L     R1,CMLRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,CMLRECDL                                                      
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
         MVI   KEY+1,X'A1'         DELETE PASSIVE KEY                           
         MVC   KEY+5(3),SVCMLSEQ                                                
         XC    KEY+8(5),KEY+8                                                   
         BAS   RE,DELKEY                                                        
*                                                                               
CML35    OC    SVADIDP,SVADIDP     ANY ADID                                     
         BZ    CML38                                                            
                                                                                
         MVI   KEY+1,X'C1'         DELETE PASSIVE KEY (ADID)                    
         MVC   KEY+5(8),SVADIDP                                                 
         BAS   RE,DELKEY                                                        
         XC    SVADIDP,SVADIDP                                                  
*                                                                               
CML38    GOTO1 VTRPACK,DMCB,(C'P',SVCML),WORK PACK AND                          
         BNE   CML40               MUST BE SPECIAL CHAR ISCII                   
*                                                                               
         MVI   KEY+1,X'C1'         DELETE PASSIVE KEY (ADID)                    
         MVC   KEY+5(8),WORK                                                    
         BAS   RE,DELKEY                                                        
*                                                                               
CML40    OC    SVHIDEF,SVHIDEF     ANY HIDEF CML                                
         BZ    CML45                                                            
                                                                                
         MVI   KEY+1,X'C2'         DELETE PASSIVE KEY (HIDEF)                   
         MVC   KEY+5(8),SVHIDEF                                                 
         BAS   RE,DELKEY                                                        
         XC    SVHIDEF,SVHIDEF                                                  
*                                                                               
CML45    OC    SVCNTRCT,SVCNTRCT   ANY CENTERCUT CML                            
         BZ    CML50                                                            
                                                                                
         MVI   KEY+1,X'C3'         DELETE PASSIVE KEY (CC)                      
         MVC   KEY+5(8),SVCNTRCT                                                
         BAS   RE,DELKEY                                                        
         XC    SVCNTRCT,SVCNTRCT                                                
*                                                                               
CML50    TM    SVCMLSTA,X'40'      COMML TEXT FOR THIS                          
         BZ    CML60                NO                                          
*                                                                               
         BAS   RE,DELCT           GO DELETE ANY COMMERCIAL TEXT RECS            
*                                                                               
         B     CML60                                                            
*                                                                               
CML55    LA    R5,12(R5)           NEXT IN ACTIVE CML LIST                      
*                                                                               
*                                                                               
CML60    MVC   KEY,SVMYKEY         RESTORE KEY                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     CML05                                                            
*                                                                               
* END OF COMMERCIALS FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *            
*                                                                               
CMLX     OC    CMLRECR,CMLRECR     ANY READ                                     
         BZ    *+14                                                             
         OC    CMLRECS,CMLRECS    ANY TO UPDATE                                 
         BNZ   CMLX10                                                           
         MVI   FORCEHED,C'N'                                                    
*                                                                               
CMLX10   LA    R2,CMLRECR                                                       
         LA    R3,=C'CML'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,CMTRECR                                                       
         LA    R3,=C'CMT'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,CMLRECR                                                       
         LA    R3,AGYCTRS+CMLRECR-COUNTERS                                      
         LA    R4,4                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
*                                                                               
         LA    R2,CMTRECR                                                       
         LA    R3,AGYCTRS+CMTRECR-COUNTERS                                      
         LA    R4,3                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         EJECT                                                                  
* NOW CK IF 1 CLIENT REQUEST (QUIT) OR GET NEXT CLIENT *                        
*                                                                               
CKEOJ    OC    BCLT,BCLT           1 CLIENT                                     
         BZ    LR10                 NO                                          
*                                                                               
         CLI   TRAMED,C'T'         MEDIA T                                      
         JNE   EXIT                                                             
         CLI   SPOTCAN,C'C'        CANADIAN AGENCY                              
         JNE   EXIT                                                             
*                                                                               
         MVI   TRAMED,C'N'         PROCESS MEDIA N AS WELL                      
         BRAS  RE,VK                                                            
         B     LR                                                               
         SPACE 3                                                                
* PRINT TOTALS FOR AGENCY/MEDIA (ONLY USED FOR ALL CLT REQUEST) *               
*                                                                               
EOJ      EQU   *                                                                
         BRAS  RE,EOJTOT                                                        
*                                                                               
         CLI   TRAMED,C'T'         MEDIA T                                      
         JNE   EXIT                                                             
         CLI   SPOTCAN,C'C'        CANADIAN AGENCY                              
         JNE   EXIT                                                             
*                                                                               
         MVI   TRAMED,C'N'         PROCESS MEDIA N AS WELL                      
         BRAS  RE,VK                                                            
         B     LR                                                               
*                                                                               
*                                                                               
*                                                                               
*====================================================================           
* DELETE ALL COMMERCIAL TEXT RECS FOR THIS COMML                                
*====================================================================           
*                                                                               
DELCT    NTR1                                                                   
*                                                                               
         MVI   CMLCMTFL,0                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         MVI   KEY+1,X'35'         0A35 (COMTEXT RECORD)                        
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DELCT40                                                          
*                                                                               
DELCT10  CLC   KEY(5),KEYSAVE                                                   
         BNE   DELCT30                                                          
*                                                                               
         L     R1,CMTRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,CMTRECR                                                       
*                                                                               
         CLC   CMTKSEQ-CMTKEY+KEY,SVCMLSEQ+1                                    
         BNE   DELCT20                                                          
*                                                                               
         L     R1,CMTRECS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,CMTRECS                                                       
*                                                                               
         MVI   CMLCMTFL,1          SET COMMENT FOUND                            
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    DELCT20                                                          
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    DELCT20                                                          
         L     R1,CMTRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,CMTRECDL                                                      
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
DELCT20  GOTO1 SEQ                                                              
         B     DELCT10                                                          
*                                                                               
DELCT30  CLI   CMLCMTFL,0          WAS A COMMENT FOUND                          
         BNE   DELCTX                                                           
*                                                                               
DELCT40  MVC   P+10(24),=C'COMMENT FLAGGED FOR CMML'                            
         MVC   P+35(12),SVCMLCOD                                                
         MVC   P+48(10),=C'NONE FOUND'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
DELCTX   MVC   AIO,AIO1            RESET BACK TO AIO1                           
*                                                                               
         OI    DMINBTS,X'80'       RESET FOR DELETE                             
         MVC   KEY(L'SVKEY),SVKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
         CLC   KEY(13),KEYSAVE                                                  
         JE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* DELETE KEY ONLY *                                                             
*                                                                               
         DS    0H                                                               
DELKEY   NTR1                                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         JO    EXIT                                                             
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         JE    EXIT                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'C0'                                                     
         NI    DMINBTS,X'FF'-X'80' SET FOR DELETE                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
         OI    DMINBTS,X'80'       RESET FOR DELETE                             
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMOUTBTS,X'02'      RESET FOR DELETE                             
         CLC   KEY(13),KEYSAVE                                                  
         JE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* CHECK SHIPPING COMMERCIALS TO ACTIVE COMMERCIAL LIST *                        
*                                                                               
         USING SHPDTAEL,R6                                                      
CCL      NTR1                                                                   
         MVC   WORK(8),SHPCMML                                                  
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   CCL18                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),WORK                                 
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
CCL18    LM    RE,RF,STRACLST                                                   
*                                                                               
CCL20    CR    RE,RF               AT END OF LIST                               
         BE    CCL24                                                            
         CLC   WORK(12),0(RE)      EQUAL TO THIS CML                            
         BE    CCL30                                                            
         LA    RE,12(RE)                                                        
         B     CCL20                                                            
*                                                                               
CCL24    MVC   0(12,RE),WORK                                                    
         LA    RF,12(RF)                                                        
         ST    RF,ENDACLST                                                      
         C     RF,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         STM   RE,RF,SVREGRE       SAVE REGISTER RE,RF                          
         BRAS  RE,FACT             FIND ACTUAL CML                              
         LM    RE,RF,SVREGRE       RESTORE RE,RF                                
                                                                                
CCL30    OC    SHPCMML2,SHPCMML2   P/B CML                                      
         JZ    EXIT                 NO                                          
         CLC   SHPCMML2,SPACES     GET AROUND BAD RECORDS                       
         JE    EXIT                 NO                                          
*                                                                               
         MVC   WORK(8),SHPCMML2   P/B CML                                       
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   CCL38                                                            
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML2),WORK                                
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
CCL38    LM    RE,RF,STRACLST                                                   
*                                                                               
CCL40    CR    RE,RF               AT END OF LIST                               
         BE    CCL44                                                            
         CLC   WORK(12),0(RE)      EQUAL TO P/B CML                             
         JE    EXIT                                                             
         LA    RE,12(RE)                                                        
         B     CCL40                                                            
*                                                                               
CCL44    MVC   0(12,RE),WORK                                                    
         LA    RF,12(RF)                                                        
         ST    RF,ENDACLST                                                      
         C     RF,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STM   RE,RF,SVREGRE       SAVE REGISTER RE,RF                          
         BRAS  RE,FACT             FIND ACTUAL CML                              
         LM    RE,RF,SVREGRE       RESTORE RE,RF                                
         J     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* FIND PRINTABLE PRODUCT *                                                      
*                                                                               
FPRD     LR    R1,R0                                                            
         L     RF,ASVCLIST                                                      
FPRD10   CLI   0(RF),C' '          END OF LIST                                  
         BNH   FPRD30                                                           
         CLC   0(1,R1),3(RF)                                                    
         BE    FPRD20                                                           
         LA    RF,4(,RF)                                                        
         B     FPRD10                                                           
FPRD20   MVC   DUB(3),0(RF)                                                     
         BR    RE                                                               
FPRD30   MVC   DUB(3),=C'???'                                                   
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
*                                                                               
         EJECT                                                                  
REPERMS  DC    CL60'* ERROR * MUST BE RUN OV OR DDS *'                          
OFFLNMS  DC    CL60'* ERROR * OFFICE CODE MUST BE * AND 1 CHAR *'               
OFFERMS  DC    CL60'* ERROR * NO CLIENTS FOR OFFICE *'                          
PRDOFFMS DC    CL60'* ERROR * CLIENT MUST BE ENTERED IF PROD ENTERED *'         
         SPACE 3                                                                
HEADING  SPROG 0,THRU,9                                                         
         SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'--------------'                                           
         SSPEC H4,3,C'MEDIA'                                                    
         SSPEC H5,3,C'CLIENT'                                                   
         SSPEC H1,30,C'T R A F F I C  P U R G E  L I S T'                       
         SSPEC H2,30,C'-----------------------------------'                     
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,PAGE                                                       
         SSPEC H6,73,REQUESTOR                                                  
*                                                                               
* FLIGHTS                                                                       
*                                                                               
         SPROG 1                                                                
         SSPEC H7,3,C'FLIGHTS'                                                  
         SSPEC H8,3,C'PROD'                                                     
         SSPEC H9,3,C'----'                                                     
         SSPEC H8,10,C'END DATE'                                                
         SSPEC H9,10,C'--------'                                                
         SSPEC H8,20,C'FLIGHTS'                                                 
         SSPEC H9,20,C'-----------------------------------------------'         
*                                                                               
* TRAFFIC BUYS                                                                  
*                                                                               
         SPROG 2                                                                
         SSPEC H7,3,C'TRAFFIC BUYS'                                             
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,13,C'STATION'                                                 
         SSPEC H9,13,C'-------'                                                 
         SSPEC H8,23,C'PRODUCT'                                                 
         SSPEC H9,23,C'-------'                                                 
         SSPEC H8,33,C'PARTNER'                                                 
         SSPEC H9,33,C'-------'                                                 
         SSPEC H8,43,C'CODE'                                                    
         SSPEC H9,43,C'----'                                                    
         SSPEC H8,50,C'START/END DATES'                                         
         SSPEC H9,50,C'-----------------'                                       
*                                                                               
* DEALER TAG RECORDS                                                            
*                                                                               
         SPROG 3                                                                
         SSPEC H7,3,C'DEALER TAGS'                                              
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,11,C'PROD'                                                    
         SSPEC H9,11,C'----'                                                    
         SSPEC H8,20,C'TAG NO.'                                                 
         SSPEC H9,20,C'-------'                                                 
         SSPEC H8,32,C'DEALER NAME/ADDR/DEALER2/ADDR'                           
         SSPEC H9,32,C'-----------------------------------------------'         
         SSPEC H8,83,C'LEN'                                                     
         SSPEC H9,83,C'---'                                                     
         SSPEC H8,88,C'RELSE/RECALL DTES'                                       
         SSPEC H9,88,C'-----------------'                                       
         SSPEC H8,107,C'TYPE'                                                   
         SSPEC H9,107,C'----'                                                   
*                                                                               
* INSTR RECAP RECORDS                                                           
*                                                                               
         SPROG 4                                                                
         SSPEC H7,3,C'INSTR RECAPS'                                             
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,10,C'STATION'                                                 
         SSPEC H9,10,C'-------'                                                 
         SSPEC H8,20,C'PRD-LEN'                                                 
         SSPEC H9,20,C'-------'                                                 
         SSPEC H8,30,C'PTR-LEN'                                                 
         SSPEC H9,30,C'-------'                                                 
         SSPEC H8,40,C'CODE'                                                    
         SSPEC H9,40,C'----'                                                    
         SSPEC H8,47,C'REV'                                                     
         SSPEC H9,47,C'---'                                                     
         SSPEC H8,54,C'INS DATE'                                                
         SSPEC H9,54,C'--------'                                                
         SSPEC H8,64,C'REF/SUB'                                                 
         SSPEC H9,64,C'-------'                                                 
         SSPEC H8,76,C'FT DATE  LT DATE'                                        
         SSPEC H9,76,C'-----------------'                                       
*                                                                               
* SHIP RECAP RECORDS                                                            
*                                                                               
         SPROG 5                                                                
         SSPEC H7,3,C'SHIP RECAPS'                                              
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,10,C'STATION'                                                 
         SSPEC H9,10,C'-------'                                                 
         SSPEC H8,20,C'COMMERCIAL P/B CMML'                                     
         SSPEC H9,20,C'-------------------'                                     
         SSPEC H8,45,C'INST-DTE   SHIP-DTE'                                     
         SSPEC H9,45,C'--------   --------'                                     
         SSPEC H8,70,C'FTD-DATE   LTD-DATE'                                     
         SSPEC H9,70,C'-------------------'                                     
*                                                                               
* PATTERN RECORDS                                                               
*                                                                               
         SPROG 6                                                                
         SSPEC H7,3,C'PATTERN RECS'                                             
         SSPEC H8,3,C'PRD-LEN'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,12,C'PTR-LEN'                                                 
         SSPEC H9,12,C'-------'                                                 
         SSPEC H8,22,C'CODE'                                                    
         SSPEC H9,22,C'----'                                                    
         SSPEC H8,28,C'REF/SUB'                                                 
         SSPEC H9,28,C'-------'                                                 
         SSPEC H8,38,C'PATTERN NAME'                                            
         SSPEC H9,38,C'----------------'                                        
         SSPEC H8,56,C'PTTN DATES'                                              
         SSPEC H9,56,C'-----------------'                                       
         SSPEC H8,75,C'USED DTE'                                                
         SSPEC H9,75,C'--------'                                                
         SSPEC H8,85,C'COMMERCIALS'                                             
         SSPEC H9,85,C'-----------'                                             
*                                                                               
* COMMERCIAL RECORDS                                                            
*                                                                               
         SPROG 7                                                                
         SSPEC H7,3,C'COMMERCIAL RECS'                                          
         SSPEC H8,3,C'COMML'                                                    
         SSPEC H9,3,C'------------'                                             
         SSPEC H8,17,C'COMML-TITLE'                                             
         SSPEC H9,17,C'---------------'                                         
         SSPEC H8,34,C'SLN'                                                     
         SSPEC H9,34,C'---'                                                     
         SSPEC H8,40,C'TYPE'                                                    
         SSPEC H9,40,C'----'                                                    
         SSPEC H8,46,C'CMML DATES'                                              
         SSPEC H9,46,C'-----------------'                                       
         SSPEC H8,65,C'CLT COMML NO'                                            
         SSPEC H9,65,C'-------------------'                                     
*                                                                               
* BUY ACTIVITY                                                                  
*                                                                               
*        SPROG X                                                                
*        SSPEC H7,3,C'BUY ACTIVITY'                                             
*        SSPEC H8,3,C'CLIENT'                                                   
*        SSPEC H9,3,C'------'                                                   
*        SSPEC H8,10,C'PRODUCT'                                                 
*        SSPEC H9,10,C'-------'                                                 
*        SSPEC H8,20,C'EST'                                                     
*        SSPEC H9,20,C'---'                                                     
*        SSPEC H8,30,C'PARTNER'                                                 
*        SSPEC H9,30,C'-------'                                                 
*        SSPEC H8,40,C'DATE LIST'                                               
*        SSPEC H9,40,C'---------------------------------------------'           
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------                       
* DELETE DEALER TAG RECORDS                                                     
*--------------------------------------------------------                       
*                                                                               
DDLR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DLRKEY,R4                                                        
         MVC   DLRKID,=X'0A2C'                                                  
         MVC   DLRKAM,BAGYMD                                                    
         MVC   DLRKCLT,SVBCLT                                                   
         GOTO1 HIGH                                                             
DLR10    CLC   KEY(5),KEYSAVE                                                   
         BNE   DLRX                                                             
         L     R1,DLRRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,DLRRECR                                                       
         CLI   BPRD,0              PROD ENTERED                                 
         BE    DLR14                NO                                          
         CLC   DLRKPROD,QPRD                                                    
         BNE   DLR40                                                            
DLR14    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    DLR20                                                            
         DC    H'0'                                                             
         USING DLRDTAEL,R6                                                      
DLR20    EQU   *                                                                
         CLC   ENDATE,DLRRCL       RECALL DATE BEFORE END DATE                  
         BL    DLR40                                                            
         L     R1,DLRRECS          REC CT TO BE DELETED                         
         LA    R1,1(,R1)                                                        
         ST    R1,DLRRECS                                                       
*                                                                               
* PRINT OUT DLR RECORD FOR DELETE LIST *                                        
*                                                                               
         LA    R0,DLRKMKT                                                       
         BAS   RE,FMK                                                           
         MVC   PDMKT,QMKT                                                       
         OC    DLRKMKT,DLRKMKT                                                  
         BNZ   *+10                                                             
         MVC   PDMKT,=CL4'ALL'                                                  
*                                                                               
         MVC   PDPROD,DLRKPROD                                                  
         SR    RE,RE                                                            
         ICM   RE,3,DLRKTAG                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PDTAG,DUB                                                        
         MVC   PDNAME,DLRNAME1                                                  
         MVC   PDADDR,DLRADDR1                                                  
         MVC   PDNAME+132,DLRNAME2                                              
         MVC   PDADDR+132,DLRADDR2                                              
         LLC   RE,DLRTLN                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PDDLN,DUB                                                        
         GOTO1 DATCON,DMCB,(3,DLRRLSE),(5,PDDATES)                              
         MVI   PDDATES+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,DLRRCL),(5,PDDATES+9)                             
         MVC   PDTYPE,DLRTYPE                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    DLR40                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    DLR40                                                            
         L     R1,DLRRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,DLRRECDL                                                      
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
DLR40    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     DLR10                                                            
         DROP  R4,R6                                                            
*                                                                               
* END OF DEALER TAGS FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *            
*                                                                               
DLRX     OC    DLRRECR,DLRRECR     ANY READ                                     
         BZ    *+14                                                             
         OC    DLRRECS,DLRRECS     ANY TO UPDATE                                
         BNZ   DLRX10                                                           
         MVI   FORCEHED,C'N'                                                    
*                                                                               
DLRX10   LA    R2,DLRRECR                                                       
         LA    R3,=C'DLR'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,DLRRECR                                                       
         LA    R3,AGYCTRS+DLRRECR-COUNTERS                                      
         LA    R4,3                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         J     EXIT                                                             
*                                                                               
*------------------                                                             
* FORMAT MARKET                                                                 
*------------------                                                             
*                                                                               
FMK      LR    R1,R0               GET INPUT ADDR                               
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  QMKT,DUB                                                         
         BR    RE                                                               
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*-----------------------------------------                                      
* DELETE RECORD AND KEY                                                         
*-----------------------------------------                                      
*                                                                               
DELREC   NTR1  BASE=*,LABEL=*                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         JO    EXIT                                                             
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         JE    EXIT                                                             
         L     R6,AIO                                                           
         OI    15(R6),X'C0'                                                     
         OI    KEY+13,X'C0'                                                     
         NI    DMOUTBTS,X'FF'-X'02' SET FOR DELETE                              
         GOTO1 PUTREC                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
         J     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*------------------------------------------------                               
* ADD CLT COUNTERS TO AGENCY AND ZERO CLT                                       
*------------------------------------------------                               
*                                                                               
ACT      NTR1  BASE=*,LABEL=*                                                   
         LA    R0,4                                                             
         SR    R1,R1                                                            
ACT10    L     RF,0(R2)            GET INPUT CTR                                
         A     RF,0(R3) ADD TO OUTPUT                                           
         ST    RF,0(R3)                                                         
         ST    R1,0(R2)            ZERO INPUT                                   
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         BCT   R4,ACT10                                                         
         J     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*---------------------------------------------                                  
* FIND PRINTABLE PRODUCT & SPOT LEN                                             
*---------------------------------------------                                  
*                                                                               
FPS      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WORK(7),SPACES                                                   
         LR    R1,R0                                                            
         L     RF,ASVCLIST                                                      
FPS10    CLI   0(RF),C' '          END OF LIST                                  
         BNH   FPS30                                                            
         CLC   0(1,R1),3(RF)                                                    
         BE    FPS20                                                            
         LA    RF,4(,RF)                                                        
         B     FPS10                                                            
FPS20    MVC   WORK(3),0(RF)                                                    
         B     FPS40                                                            
FPS30    MVC   WORK(3),=C'???'                                                  
*                                                                               
FPS40    LA    RF,WORK+3                                                        
         CLI   WORK+2,C' '                                                      
         BH    FPS44                                                            
         BCTR  RF,0                                                             
FPS44    MVI   0(RF),C'-'                                                       
         LLC   R0,1(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+7(3),DUB                                                    
         CLI   WORK+7,C'0'                                                      
         BNE   FPS46                                                            
         MVC   1(2,RF),WORK+8                                                   
         J     EXIT                                                             
FPS46    MVC   1(3,RF),WORK+7                                                   
         J     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------                    
* CHECK COMMERCIAL REC PRODUCT LIST FOR REQUESTED PRODUCT                       
*-----------------------------------------------------------                    
*                                                                               
CCP      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CMLPRDEL,R6                                                      
         CLI   CMLPRDS,X'FF'       ALL PRODUCTS                                 
         BE    CCP30                                                            
                                                                                
         LLC   R0,CMLPRDLN                                                      
         LA    R1,CMLPRDS                                                       
CCP10    CLC   BPRD,0(R1)                                                       
         BE    CCP20                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,CCP10                                                         
         B     CCP30                                                            
                                                                                
CCP20    CLI   BPRD2,0             PARTNER PRODUCT                              
         BE    CCP40                NO                                          
                                                                                
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CMLPRDEL,R6                                                      
                                                                                
         LLC   R0,CMLPRDLN                                                      
         LA    R1,CMLPRDS                                                       
CCP24    CLC   BPRD2,0(R1)         GOT THIS PROD TOO                            
         BE    CCP40                YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,CCP24                                                         
                                                                                
CCP30    MVI   ELCODE,X'10'                                                     
         CR    RB,RD                                                            
         J     EXIT                                                             
                                                                                
CCP40    MVI   ELCODE,X'10'                                                     
         J     EXIT                                                             
         EJECT                                                                  
*----------------------------------                                             
* QUERY INVOICE RECORDS                                                         
*----------------------------------                                             
*                                                                               
INV      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VSWITCH,=C'SPT'                                                  
         BRAS  RE,INITXSP                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SNVKEYD,R4                                                       
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,SVBCLT                                                   
         GOTO1 HIGH                                                             
*                                                                               
INV10    CLC   KEY(5),KEYSAVE                                                   
         BNE   INVX                                                             
         L     R1,INVRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,INVRECR                                                       
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING SNVCMEL,R6                                                       
INV20    BRAS  RE,NEXTEL                                                        
         BNE   INV80                                                            
*                                                                               
* BUILD ACTIVE COMMERCIAL LIST FROM INVOICE *                                   
*                                                                               
         BRAS  RE,ACLI             GO ADD TO ACTIVE CML LIST                    
*                                                                               
         CLC   =C'BB/',7(R6)                                                    
         BNE   *+6                                                              
         DC    X'0700'                                                          
         B     INV20                                                            
         EJECT                                                                  
* GET NEXT SEQUENTIAL INVOICE RECORD *                                          
*                                                                               
INV80    MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
         GOTO1 SEQ                                                              
*                                                                               
         B     INV10                                                            
*                                                                               
         DROP  R4,R6                                                            
         SPACE 3                                                                
* AT END OF INVOICES FOR CLIENT, SORT ACTIVE         *                          
* COMMERCIAL LIST, PRINT TOTALS, ADD TO MEDIA TOTALS *                          
*                                                                               
INVX     OC    INVRECR,INVRECR     ANY READ                                     
         BZ    *+14                                                             
         OC    INVRECS,INVRECS    ANY TO UPDATE                                 
         BNZ   INVX10                                                           
         MVI   FORCEHED,C'N'                                                    
INVX10   LM    R2,R3,STRACLST                                                   
         LR    R4,R3                                                            
         CR    R2,R3               ANY ACTIVE COMMERCIALS FOUND                 
         BE    INVX20               NO                                          
         SR    R3,R2                                                            
         SR    R0,R0                                                            
         LR    R1,R3                                                            
         D     R0,=F'12'           DIV BY THE LENGTH OF AN ENTRY                
         LR    R3,R1                                                            
*                                                                               
         GOTO1 XSORT,DMCB,(R2),(R3),12,12,0                                     
*                                                                               
INVX20   MVI   0(R4),X'FF'                                                      
*                                                                               
         LA    R2,INVRECR                                                       
         LA    R3,=C'INV'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,INVRECR                                                       
         LA    R3,AGYCTRS+INVRECR-COUNTERS                                      
         LA    R4,3                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
*                                                                               
         GOTO1 VSWITCH,=C'STR'                                                  
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------                                    
* FORMAT MARKET/STATION FOR PRINTING                                            
*-------------------------------------------                                    
*                                                                               
FMS      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',(R0)),QMKT,WORK                               
*                                                                               
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES    CABLE HEAD                                   
         BE    FMS10                                                            
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
*                                                                               
FMS10    CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         JE    EXIT                                                             
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         JE    EXIT                                                             
         MVI   3(RE),C' '                                                       
         J     EXIT                                                             
         EJECT                                                                  
*----------------------------------                                             
* VALIDATE KEY ROUTINE                                                          
*----------------------------------                                             
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        DON'T CHECK IF OFFLINE                       
         BE    VK00                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   INVACTER             NO, PURGE NOT ALLOWED                       
*                                                                               
VK00     LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT, BPRD, BSLN, BPRD2, BSLN2         
         XC    SVBCLT,SVBCLT                                                    
         MVI   OFFICE,0                                                         
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK20                                                             
         CLI   8(R2),C'*'          BY OFFICE                                    
         BE    VK14                                                             
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         B     VK20                                                             
VK14     CLI   5(R2),2             MUST BE 2 CHAR                               
         BNE   OFFLNERR                                                         
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERR                                                           
*                                                                               
         BRAS  RE,VOFF             GO VALIDATE OFFICE                           
         BNE   OFFERR                                                           
         MVC   OFFICE,9(R2)                                                     
*                                                                               
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                NO                                           
         OC    BCLT,BCLT                                                        
         BZ    MISSCLT                                                          
         CLI   TRACLT,C'*'         BY OFFICE                                    
         BE    PRDOFFER                                                         
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3         GET BIN PROD                                 
*                                                                               
VK30     CLI   TRAPTRH+5,0         ANY ENTRY                                    
         BE    VK40                NO                                           
         CLI   TRAPRDH+5,0         PRODUCT ENTERED                              
         BE    PRDOFFER                                                         
         LA    R2,TRAPTRH          PARTNER PRODUCT                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3        GET BIN PROD                                 
*                                                                               
VK40     LA    R2,TRAEDTH          END DATE                                     
         BRAS  RE,VEDT                                                          
*                                                                               
         LA    R2,TRAFLTRH                                                      
         BRAS  RE,VFTR             GO VALIDATE FILTERS                          
*                                                                               
VKX      J     EXIT                                                             
         SPACE 3                                                                
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
INVACTER MVI   ERROR,INVACT                                                     
         LA    R2,CONRECH                                                       
         B     TRAPERR                                                          
REPERR   L     R1,=A(REPERMS)                                                   
         B     ERREXIT                                                          
OFFLNERR L     R1,=A(OFFLNMS)                                                   
         B     ERREXIT                                                          
OFFERR   L     R1,=A(OFFERMS)                                                   
         B     ERREXIT                                                          
PRDOFFER L     R1,=A(PRDOFFMS)                                                  
         LA    R2,TRACLTH                                                       
*                                                                               
ERREXIT  A     R1,SPTR94RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------                                  
* VALIDATE END DATE IN KEY, FROM VK                                             
*---------------------------------------------                                  
*                                                                               
VEDT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSERRA                                                         
         GOTO1 DATVAL,DMCB,(0,TRAEDT),DATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRA                                                          
         GOTO1 DATCON,DMCB,(0,DATE),(3,ENDATE)                                  
         GOTO1 (RF),(R1),(0,DATE),(2,ENDATEP)                                   
         GOTO1 DATCON,DMCB,(5,0),(3,WORK)                                       
         LLC   R1,WORK             GET YEAR                                     
         BCTR  R1,0                AND SUBTRACT                                 
         STC   R1,WORK                                                          
         MVC   WORK+1(2),=XL2'0C1F' AND FORCE MO/DA TO 12/31                    
         CLC   ENDATE,WORK                                                      
         BH    ENDTERR                                                          
         GOTO1 DATCON,DMCB,(3,ENDATE),(5,PRTDATE)                               
         J     EXIT                                                             
DATERRA  MVI   ERROR,INVDATE                                                    
         B     TRAPERRA                                                         
MISSERRA MVI   ERROR,MISSING                                                    
TRAPERRA GOTO1 ERREX                                                            
ENDTERR  MVC   CONHEAD,ENDTERMS                                                 
         GOTO1 ERREX2                                                           
ENDTERMS DC    CL60'* ERROR * DATE MUST BE BEFORE START OF THIS YEAR *'         
         EJECT                                                                  
*------------------------------------------------------                         
* VALIDATE OFFICE CODE - READ CLIENT HEADER RECORD(S)                           
*------------------------------------------------------                         
*                                                                               
VOFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
VOFF10   CLI   KEY,0               TEST CLIENT HEADER RECS                      
         BNE   VOFFX                                                            
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   VOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
*                                                                               
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         B     VOFF10                                                           
*                                                                               
VOFF30   L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   VOFF34                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
*                                                                               
VOFF34   LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
VOFF40   CLC   OFFICE,0(R1)        TEST RIGHT OFFICE                            
         BE    VOFF45                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VOFF40                                                        
         B     VOFF20                                                           
*                                                                               
VOFF45   MVC   SVBCLT,CKEYCLT                                                   
         MVC   CLTNM,CNAME                                                      
*                                                                               
         CLI   OFFLINE,C'Y'            IF OFFLINE, NO SEC CK                    
         BE    VOFFX                                                            
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VOFFX                                                            
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    VOFF60                                                           
*                                                                               
         CLC   T216FFD+6(2),CKEYCLT    ELSE SINGLE CLIENT ACCESS                
         BNE   VOFF20                                                           
         B     VOFFX                                                            
*                                                                               
VOFF60   CLC   T216FFD+7(1),OFFICE    MATCH OFFICE CODE                         
         BNE   VOFF20                                                           
*                                                                               
VOFFX    MVC   BCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
*                                                                               
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* VALIDATE FILTER - ONLY OPTIONS ARE FLT,BUYS,DLR,INSTR,SHIP,       *           
* PATTERNS, COMMERCIALS, PRTALL, TEST                               *           
*********************************************************************           
*                                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   LLC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELPA),FTRHELPA                                     
ERREXITA GOTO1 ERREX2                                                           
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK)                             
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRB            NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
*                                                                               
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR20              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
VFTR20   EX    R1,VFTRCLCA         FLIGHTS                                      
         BNE   VFTR22                                                           
         OI    FTRSW1,FLTSW                                                     
         B     VFTR70                                                           
VFTR22   EX    R1,VFTRCLCB         TRAFFIC BUYS                                 
         BNE   VFTR24                                                           
         OI    FTRSW1,BUYSW                                                     
         B     VFTR70                                                           
VFTR24   EX    R1,VFTRCLCC         DEALERS                                      
         BNE   VFTR28                                                           
         OI    FTRSW1,DLRSW                                                     
         B     VFTR70                                                           
*                                                                               
VFTR28   EX    R1,VFTRCLCE         SHIP RECAPSS                                 
         BNE   VFTR30                                                           
         OI    FTRSW1,SHPSW                                                     
         B     VFTR70                                                           
VFTR30   EX    R1,VFTRCLCF         PATTERNS                                     
         BNE   VFTR40                                                           
         OI    FTRSW1,PATSW                                                     
         B     VFTR70                                                           
*                                                                               
VFTR40   EX    R1,VFTRCLCH         PRINT ALL                                    
         BNE   VFTR44                                                           
         OI    FTRSW2,PRTSW                                                     
         B     VFTR70                                                           
*                                                                               
VFTR44   EX    R1,VFTRCLCT         TEST                                         
         BNE   VFTR50                                                           
         OI    FTRSW2,TESTSW                                                    
         B     VFTR70                                                           
*                                                                               
VFTR50   EX    R1,VFTRCLCS         SKIPPU                                       
         BNE   VFTR80                                                           
         OI    FTRSW2,SKIPPU                                                    
*                                                                               
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    J     EXIT                                                             
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELPA),FTRMSG                              
         B     ERREXITA                                                         
VFTRCLCA CLC   12(0,R4),=CL9'FLIGHTS'                                           
VFTRCLCB CLC   12(0,R4),=CL7'BUYS '                                             
VFTRCLCC CLC   12(0,R4),=CL4'DLR'                                               
*VFTRCLCD CLC   12(0,R4),=CL8'INSTR '                                           
VFTRCLCE CLC   12(0,R4),=CL8'SHIP '                                             
VFTRCLCF CLC   12(0,R4),=CL9'PATTERNS'                                          
*VFTRCLCG CLC   12(0,R4),=CL12'COMMERCIALS'                                     
VFTRCLCH CLC   12(0,R4),=CL7'PRTALL'                                            
VFTRCLCS CLC   12(0,R4),=CL7'SKIPPU'                                            
VFTRCLCT CLC   12(0,R4),=CL5'TEST'                                              
DATERRB  MVI   ERROR,INVDATE                                                    
         B     TRAPERRB                                                         
MISSERRB MVI   ERROR,MISSING                                                    
TRAPERRB GOTO1 ERREX                                                            
FTRMSG   DC    C'* ERROR * '                                                    
FTRHELPA DC    C'VALID FILTERS=FLT/BUY/DLR/INSTR/SHIP/PAT/COM/PRTALL'           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------                        
* PRINT PATTERN LIST OF MKT/STA/TYPE/AFFIL/MGROUP                               
*-------------------------------------------------------                        
*                                                                               
PLST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PATLSTEL,R6                                                      
         LA    R4,PPPTR+132                                                     
         CLC   P2,SPACES                                                        
         BE    *+8                                                              
         LA    R4,132(,R4)                                                      
*                                                                               
         LA    R5,72(,R4)          LIMIT                                        
*                                                                               
         CLI   PATLSTTY,C'A'       AFFILIATE                                    
         BNE   *+14                                                             
         MVC   0(7,R4),=C'AFFIL ='                                              
         B     PLST30                                                           
*                                                                               
         CLI   PATLSTTY,C'T'       AFFILIATE                                    
         BNE   *+14                                                             
         MVC   0(7,R4),=C'TYPE ='                                               
         B     PLST30                                                           
*                                                                               
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFIL                        
         BNE   *+14                                                             
         MVC   0(7,R4),=C'MKT/AF='                                              
         B     PLST30                                                           
*                                                                               
         CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BNE   *+14                                                             
         MVC   0(7,R4),=C'MKTGRP='                                              
         B     PLST30                                                           
*                                                                               
         CLI   PATLSTTY,C'S'       STATION                                      
         BNE   *+14                                                             
         MVC   0(7,R4),=C'STA(S)='                                              
         B     PLST30                                                           
*                                                                               
         CLI   PATLSTTY,C'M'       MARKET                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(7,R4),=C'MKT(S)='                                              
         OC    3(5,R6),3(R6)       THIS ALL MARKETS                             
         BNZ   PLST30                                                           
         MVC   7(3,R4),=C'ALL'                                                  
         CLI   1(R6),8                                                          
         JE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
PLST30   LA    R4,7(,R4)                                                        
         LLC   R3,PATLSTLN                                                      
         SR    R2,R2                                                            
         SHI   R3,3                                                             
         D     R2,=F'5'                                                         
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,PATLST                                                        
         B     PLST50                                                           
*                                                                               
PLST40   MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         CR    R5,R4                                                            
         BH    PLST50                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,PPPTR+7          START                                        
         LA    R5,72-7(,R4)        END LIMIT                                    
*                                                                               
PLST50   CLI   PATLSTTY,C'A'       AFFILIATE                                    
         BNE   PLST54                                                           
         MVC   0(3,R4),0(R2)                                                    
         LA    R4,3(,R4)                                                        
         B     PLST80                                                           
*                                                                               
PLST54   CLI   PATLSTTY,C'T'       TYPE                                         
         BNE   PLST56                                                           
         MVC   0(1,R4),0(R2)                                                    
         LA    R4,1(,R4)                                                        
         B     PLST80                                                           
*                                                                               
PLST56   CLI   PATLSTTY,C'G'                                                    
         BNE   PLST60                                                           
         MVC   0(1,R4),0(R2)                                                    
         UNPK  DUB(5),1(3,R2)                                                   
         MVC   1(4,R4),DUB                                                      
         LA    R4,5(,R4)                                                        
         B     PLST80                                                           
*                                                                               
PLST60   CLI   PATLSTTY,C'S'                                                    
         BNE   PLST70                                                           
         OC    0(2,R2),0(R2)       CABLE HEAD                                   
         BNZ   PLST64                                                           
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',(R2)),WORK,WORK+4                             
*                                                                               
*NOP     CLC   WORK+4+5(3),SPACES                                               
*        BNE   *+6                                                              
****     DC    H'0'                BAD PATTERN REC S=AAAA/                      
         MVC   0(8,R4),WORK+4                                                   
         MVI   4(R4),C'/'                                                       
         LA    R4,8(,R4)                                                        
         B     PLST80                                                           
*                                                                               
PLST64   MVC   0(5,R4),0(R2)                                                    
         LA    R4,5(,R4)                                                        
         B     PLST80                                                           
*                                                                               
PLST70   CLI   PATLSTTY,C'C'       COMBINED                                     
         BE    PLST74                                                           
         CLI   PATLSTTY,C'M'       MARKET                                       
         BE    PLST74                                                           
         DC    H'0'                                                             
PLST74   CLI   0(R2),0             THIS A MARKET                                
         BE    PLST76                                                           
         MVC   0(3,R4),0(R2)                                                    
         LA    R4,3(,R4)                                                        
         B     PLST80                                                           
*                                                                               
PLST76   SR    RE,RE                                                            
         ICM   RE,3,3(R2)                                                       
         LTR   RE,RE               THIS ALL MARKETS                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R4),DUB                                                      
         LA    R4,4(,R4)                                                        
*                                                                               
PLST80   LA    R2,5(,R2)                                                        
         BCT   R3,PLST40                                                        
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------               
* FIND PATTERN TEXT REC AND DELETE X'70' ELEM FOR THIS PATRTERN                 
*----------------------------------------------------------------               
*                                                                               
PTXT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY,KEY                                                        
         L     R1,PTXRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,PTXRECR                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         MVI   KEY+1,X'2D'                                                      
         MVC   DTXKDESC-DTXKEY+KEY,2(R6)                                        
         MVI   DTXKTYP-DTXKEY+KEY,C'L'                                          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PTXT40                                                           
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PTXT16                                                           
         SR    R0,R0                                                            
         ICM   R0,7,SVKEY+10                                                    
         SRL   R0,10                                                            
         X     R0,=X'00003FFF'                                                  
         STH   R0,HALF                                                          
*                                                                               
PTXT10   CLC   2(5,R6),SVKEY+5                                                  
         BNE   PTXT14                                                           
         CLC   HALF,7(R6)                                                       
         BE    PTXT20                                                           
PTXT14   BRAS  RE,NEXTEL                                                        
         BE    PTXT10                                                           
*                                                                               
PTXT16   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PPTXT-25(24),=C'COMMENT FLAGGED FOR PTTN'                        
         MVC   PPTXT(10),=C'NO ELEMENT'                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PTXTX                                                            
*                                                                               
PTXT20   GOTO1 VRECUP,DMCB,AIO,(R6)                                             
*                                                                               
         L     R1,PTXRECS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,PTXRECS                                                       
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    PTXT30                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    PTXT30                                                           
         L     R1,PTXRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,PTXRECDL                                                      
         GOTO1 PUTREC              GO DELETE RECORD AND KEY                     
*                                                                               
PTXT30   B     PTXTX                                                            
*                                                                               
PTXT40   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PPTXT-25(24),=C'COMMENT FLAGGED FOR PTTN'                        
         MVC   PPTXT(10),=C'NONE FOUND'                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PTXTX    MVC   AIO,AIO1            RESET BACK TO AIO1                           
*                                                                               
         OI    DMINBTS,X'80'       RESET FOR DELETE                             
         MVC   KEY(L'SVKEY),SVKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMOUTBTS,X'02'      RESET FOR DELETE                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              RESTORE FOR DELETE LATER                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------                               
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*------------------------------------------------                               
*                                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    FTRSW2,TESTSW       TEST RUN                                     
         BZ    *+10                                                             
         MVC   H4+43(8),=CL8'**TEST**'                                          
*                                                                               
         MVC   H4+10(L'QMED),QMED                                               
         MVC   H4+15(L'MEDNM),MEDNM                                             
         MVC   H3+34(16),=CL16'PURGE ALL BEFORE'                                
         MVC   H3+51(8),PRTDATE                                                 
*                                                                               
         CLI   OFFICE,0            TEST OFFICE CODE GIVEN                       
         BE    *+16                                                             
         MVC   H4+47(6),=C'OFFICE'                                              
         MVC   H4+56(1),OFFICE     SHOW OFFICE CODE                             
*                                                                               
         MVC   H5+10(L'QCLT),QCLT                                               
         MVC   H5+15(L'CLTNM),CLTNM                                             
         J     EXIT                                                             
         EJECT                                                                  
*-------------------------------------                                          
* FIND CLIENT HEADER AND SAVE CLIST                                             
*-------------------------------------                                          
*                                                                               
FCLT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   BBCLT,BCLT          SAVE                                         
*                                                                               
* SAVE CURRENT RECORD                                                           
*                                                                               
FCLT10   L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT14                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    FCLT14                                                           
         DC    H'0'                                                             
*                                                                               
FCLT14   L     R0,AIO1             MOVE BACK THE ORIGINAL RECORD                
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
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
         MVC   SVCODE,SVPROF11      SAVE COPY CODE                              
*                                                                               
* READ PU PROFILE *                                                             
*                                                                               
         MVC   WORK+2(2),=C'PU'                                                 
         GOTO1 (RF),(R1),(X'90',WORK),SVPROF                                    
*                                                                               
         CLI   SVPROF+5,C'N'       PURGE ALL RECORDS                            
         BE    FCLT15              YES                                          
*                                                                               
         CLI   SVPROF+5,C'C'       CML ONLY                                     
         BNE   *+8                                                              
         OI    FTRSW1,CMLSW        PURGE COMMERCIALS ONLY                       
*                                                                               
         CLI   SVPROF+5,C'I'       INSTRUCTION RECAP RECS ONLY                  
         BNE   *+8                                                              
         OI    FTRSW1,INSSW        PURGE INST RECAPS ONLY                       
*                                                                               
         CLI   SVPROF+5,C'B'       PURGE BOTH CML AND INST RECAP                
         BNE   *+8                                                              
         OI    FTRSW1,INSSW+CMLSW  PURGE CML AND INST ONLY                      
*                                                                               
FCLT15   TM    FTRSW2,SKIPPU       IS SKIP PU PROFILE OPTION SET?               
         BZ    *+8                  NO                                          
         MVI   SVPROF,C'N'         FORCE BYPASS OFF                             
*                                                                               
         CLI   SVPROF,C'Y'         IS PROFILE SET TO BYPASS PURGE               
         BNE   FCLT20                NO                                         
*                                                                               
         L     R1,AXCLTBLE         XCLUDE CLT TABLE                             
FCLT16   OC    0(3,R1),0(R1)       ANY ENTRY                                    
         BZ    FCLT18                                                           
         CLC   QCLT,0(R1)          IS CLT ALREADY IN TABLE                      
         BE    FCLTX                YES, EQ CC WILL BYPASS                      
         LA    R1,3(R1)                                                         
         C     R1,MAXCLTBL         END OF TALBE                                 
         BL    FCLT16                                                           
         DC    H'0'                MAKE TABLE BIGGER                            
*                                                                               
FCLT18   MVC   0(3,R1),QCLT        SAVE BYPASSED CLT IN TABLE                   
         CR    RB,RB               SET CC EQ                                    
         B     FCLTX                 YES, EQ CC WILL BYPASS                     
*                                                                               
FCLT20   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2                                                         
         MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         CR    RB,RC               SET CC NE TO ALLOW PURGE TO HAPPEN           
         B     *+6                                                              
FCLTNE   CR    RB,RB               SET CC EQUAL, TO BYPASS PURGE                
*                                                                               
FCLTX    MVC   BCLT,BBCLT          RESTORE                                      
         J     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------                                
* GET NEXT SEQUENTIAL CLIENT FOR THIS OFFICE                                    
*-----------------------------------------------                                
*                                                                               
NOFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT      LAST CLIENT THIS OFFICE                     
         MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
NOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   NOFF80                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   NOFF80                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    NOFF30               YES                                         
*                                                                               
NOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         B     NOFF10                                                           
*                                                                               
NOFF30   L     R6,AIO1                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   NOFF34                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
*                                                                               
NOFF34   LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
NOFF40   CLC   OFFICE,0(R1)        TEST RIGHT OFFICE                            
         BE    NOFF45                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,NOFF40                                                        
         B     NOFF20                                                           
*                                                                               
NOFF45   MVC   SVBCLT,CKEYCLT                                                   
         MVC   CLTNM,CNAME                                                      
*                                                                               
         CLI   OFFLINE,C'Y'            IF OFFLINE, NO SEC CK                    
         BE    NOFFX                                                            
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    NOFF70                                                           
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    NOFF60                                                           
*                                                                               
         CLC   T216FFD+6(2),CKEYCLT   ELSE SINGLE CLIENT ACCESS                 
         BNE   NOFF20                                                           
         B     NOFF70                                                           
*                                                                               
NOFF60   CLC   T216FFD+7(1),OFFICE    MATCH OFFICE CODE                         
         BNE   NOFF20                                                           
*                                                                               
NOFF70   MVC   SVBCLT,CKEYCLT                                                   
         MVC   CLTNM,CNAME                                                      
*                                                                               
         CR    R0,R0                                                            
         B     NOFFX                                                            
*                                                                               
NOFF80   CR    RB,RC               SET NE COND CODE                             
*                                                                               
NOFFX    J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*------------------------------------------------                               
* PRINT TOTALS FOR PURGED ELEMENTS AND RECORDS                                  
*------------------------------------------------                               
*                                                                               
ELEMTOT  NTR1  BASE=*,LABEL=*                                                   
         EDIT  (4,0(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(10),=C'ELEMS READ'                                          
         EDIT  (4,4(R2)),(9,P+35),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+45(3),0(R3)                                                    
         MVC   P+49(18),=C'ELEMS TO BE PURGED'                                  
         EDIT  (4,8(R2)),(9,P+70),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+80(3),0(R3)                                                    
         MVC   P+84(12),=C'ELEMS PURGED'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (4,12(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+13(12),=C'EMPTY REC(S)'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (4,16(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(9),=C'RECS READ'                                            
         EDIT  (4,20(R2)),(9,P+35),0,COMMAS=YES,ZERO=NOBLANK                    
         MVC   P+45(3),0(R3)                                                    
         MVC   P+49(18),=C'RECS TO BE UPDATED'                                  
         EDIT  (4,24(R2)),(9,P+70),0,COMMAS=YES,ZERO=NOBLANK                    
         MVC   P+80(3),0(R3)                                                    
         MVC   P+84(12),=C'RECS WRITTEN'                                        
         EDIT  (4,28(R2)),(9,P+100),0,COMMAS=YES,ZERO=NOBLANK                   
         MVC   P+110(3),0(R3)                                                   
         MVC   P+114(12),=C'RECS DELETED'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         EJECT                                                                  
* PRINT TOTALS FOR PURGED ELEMENTS AND RECORDS *                                
*                                                                               
RECTOT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (4,0(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(9),=C'RECS READ'                                            
         CLC   0(3,R3),=C'INV'                                                  
         BE    RECTOT20                                                         
         EDIT  (4,4(R2)),(9,P+35),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+45(3),0(R3)                                                    
         MVC   P+49(18),=C'RECS TO BE DELETED'                                  
         EDIT  (4,8(R2)),(9,P+70),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+80(3),0(R3)                                                    
         MVC   P+84(12),=C'RECS DELETED'                                        
         CLC   0(3,R3),=C'CML'                                                  
         BNE   RECTOT20                                                         
         EDIT  (4,CEASICT),(9,P+100),0,COMMAS=YES,ZERO=NOBLANK                  
         MVC   P+110(14),=C'EASI RECS KEPT'                                     
RECTOT20 DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         EJECT                                                                  
*--------------------------------------                                         
* ADD TO ACTIVE PATTERN LIST                                                    
*--------------------------------------                                         
*                                                                               
APL      NTR1  BASE=*,LABEL=*                                                   
         USING INSKEY,R4                                                        
         USING INSDTAEL,R6                                                      
         LLC   R3,INSDTALN         GET NUMBER OF SUBELS                         
         AHI   R3,-INSBSCEL                                                     
         SR    R2,R2                                                            
         D     R2,=A(INSSUBEL)                                                  
         LTR   R2,R2               NO REMAINDER                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,INSPTTN                                                       
*                                                                               
APL10    SAM24                                                                  
         OC    0(3,R2),0(R2)       HIATUS                                       
         BZ    APL24                YES, BYPASS                                 
*                                                                               
         CLC   0(3,R2),=X'FFFFFF'  TBA                                          
         BE    APL24                YES, BYPASS                                 
*                                                                               
         SAM31                                                                  
         L     R5,STRAPLST         START OF ACTIVE PATTERN LIST                 
         USING APLIST,R5                                                        
APL14    C     R5,ENDAPLST         AT END OF LIST                               
         BE    APL20                YES                                         
         BL    *+6                 BUT NOT PAST END                             
         DC    H'0'                                                             
         CLC   INSPRD1(4),APLPRD1  SAME PRD, ETC                                
         BNE   APL16                                                            
         CLC   INSKCOPY,APLCODE    SAME COPY CODE                               
         BNE   APL16                                                            
         CLC   0(3,R2),APLREFS     SAME REF/SUB                                 
         BE    APL24                                                            
*PL16    LA    R5,APLNEXT                                                       
APL16    AHI   R5,APLISTLN                                                      
         B     APL14                                                            
*                                                                               
APL20    MVC   APLPRD1(4),INSPRD1                                               
         MVC   APLCODE,INSKCOPY    COPY CODE/EST                                
         MVC   APLREFS,0(R2)       REFSUB                                       
*NOP     LA    R5,APLNEXT                                                       
         AHI   R5,APLISTLN                                                      
         ST    R5,ENDAPLST                                                      
         C     R5,MAXTBLSZ         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
APL24    LA    R2,INSSUBEL(,R2)                                                 
         BCT   R3,APL10                                                         
         SAM24                                                                  
         J     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------                       
* ADD PATTERN COMMERCIAL LIST TO ACTIVE COMMERCIAL LIST                         
*--------------------------------------------------------                       
*                                                                               
ACL      NTR1  BASE=*,LABEL=*                                                   
         LA    R3,2(,R6)                                                        
         LLC   R0,1(R6)                                                         
         SRL   R0,4                DIVIDE BY 16                                 
ACL10    LM    RE,RF,STRACLST                                                   
*                                                                               
         CLC   0(8,R3),=XL8'5C00000000000000' DELETED COMML                     
         BE    ACL30                                                            
         CLC   =C'HIATUS',0(R3)    HIATUS PATTERN                               
         BE    ACL30                                                            
         CR    RE,RF               AT END OF LIST                               
         BE    ACL24                                                            
*                                                                               
         MVC   WORK(8),0(R3)                                                    
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   ACL18                                                            
         GOTO1 VTRPACK,DMCB,(C'U',0(R3)),WORK                                   
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
ACL18    LM    RE,RF,STRACLST                                                   
*                                                                               
ACL22    CLC   WORK(12),0(RE)      EQUAL TO THIS CML                            
         BE    ACL30                                                            
         LA    RE,12(RE)                                                        
         CR    RE,RF               AT END OF LIST                               
         BNE   ACL22                                                            
*                                                                               
ACL24    MVC   0(12,RE),WORK                                                    
         LA    RF,12(RF)                                                        
         ST    RF,ENDACLST                                                      
         C     RF,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STM   RE,RF,SVREGRE       SAVE REGISTER RE,RF                          
         BRAS  RE,FACT             FIND ACTUAL CML                              
         LM    RE,RF,SVREGRE       RESTORE RE,RF                                
*                                                                               
ACL30    OC    8(8,R3),8(R3)       P/B CML                                      
         BZ    ACL50                NO                                          
         LM    RE,RF,STRACLST                                                   
         CLC   8(8,R3),=XL8'5C00000000000000' DELETED COMML                     
         BE    ACL50                                                            
         CLC   =C'HIATUS',8(R3)    HIATUS PATTERN                               
         BE    ACL50                                                            
*                                                                               
ACL40    CR    RE,RF               AT END OF LIST                               
         BE    ACL44                                                            
*                                                                               
         MVC   WORK(8),8(R3)                                                    
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   ACL41                                                            
         GOTO1 VTRPACK,DMCB,(C'U',8(R3)),WORK                                   
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
ACL41    LM    RE,RF,STRACLST                                                   
*                                                                               
ACL42    CLC   WORK(12),0(RE)      EQUAL TO THIS CML                            
         BE    ACL50                                                            
         LA    RE,12(RE)                                                        
         CR    RE,RF               AT END OF LIST                               
         BNE   ACL42                                                            
*                                                                               
ACL44    MVC   0(12,RE),WORK                                                    
         LA    RF,12(RF)                                                        
         ST    RF,ENDACLST                                                      
         C     RF,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STM   RE,RF,SVREGRE       SAVE REGISTER RE,RF                          
         BRAS  RE,FACT             FIND ACTUAL CML                              
         LM    RE,RF,SVREGRE       RESTORE RE,RF                                
*                                                                               
ACL50    LA    R3,16(,R3)                                                       
         BCT   R0,ACL10                                                         
         J     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
* ADD INVOICE COMMERCIAL TO ACTIVE COMMERCIAL LIST *                            
* NOTE ALL COMMERCIALS ON INVOICE RECORDS ARE 8-12 CHAR                         
*--------------------------------------------------------------                 
ACLI     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,2(,R6)                                                        
         LLC   R0,1(R6)                                                         
         SRL   R0,4                DIVIDE BY 16                                 
*                                                                               
         MVC   WORK(12),7(R6)      8-12 CHAR CML                                
         OC    WORK(12),SPACES     BLANK PADDED                                 
*                                                                               
ACLI12   LM    RE,RF,STRACLST                                                   
*                                                                               
ACLI20   CR    RE,RF               AT END OF LIST                               
         BE    ACLI24                                                           
         CLC   WORK(12),0(RE)      EQUAL TO THIS CML                            
         BE    ACLI30                                                           
         LA    RE,12(RE)                                                        
         B     ACLI20                                                           
                                                                                
ACLI24   MVC   0(12,RE),WORK                                                    
         LA    RF,12(RF)                                                        
         ST    RF,ENDACLST                                                      
         C     RF,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VSWITCH,=C'STR'                                                  
         BRAS  RE,FACT             FIND ACTUAL CMLS FOR THIS CML                
         GOTO1 VSWITCH,=C'SPT'                                                  
         BRAS  RE,INITXSP                                                       
ACLI30   J    EXIT                                                              
*                                                                               
*---------------------------------------------                                  
* TABLE ACTUAL COMMERCIALS IF ANY                                               
*---------------------------------------------                                  
*                                                                               
FACT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITTRF          GO SW TO TRAFFIC FILE                        
         MVC   SVMYKEY,KEY         SAVE KEY                                     
         MVC   SVELCODE,ELCODE                                                  
*                                                                               
         L     R5,STRACLST         START OF ACTIVE COMML LIST                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK                                                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FACTX                                                            
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FACTX                                                            
*                                                                               
         XC    SVACTCML,SVACTCML                                                
*                                                                               
         LA    R3,SVACTCML                                                      
         LA    R0,4                4 ACTUAL CMLS                                
*                                                                               
         USING CMLACTEL,R6                                                      
*                                                                               
FACT10   MVC   0(8,R3),CMLACTID    MOVE IN 8 CHAR CML                           
         MVC   8(4,R3),SPACES                                                   
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         BE    FACT20                                                           
*                                                                               
         MVC   0(12,R3),CMLACTID   MOVE IN 12 CHAR CML                          
*                                                                               
FACT20   LA    R3,12(R3)                                                        
         BCTR  R0,0                                                             
         LTR   R0,R0               INSURANCE                                    
         BZ    FACTX                                                            
                                                                                
         BRAS  RE,NEXTEL                                                        
         BE    FACT10                                                           
                                                                                
FACTX    OC    SVACTCML,SVACTCML   ANY ACTUAL CMLS                              
         BZ    FACTXIT              NO, DONE                                    
*                                                                               
         LA    R3,SVACTCML                                                      
         LA    R0,4                MAX 4 ACTUAL CMLS                            
*                                                                               
FACTX1   LM    RE,RF,STRACLST                                                   
*                                                                               
FACTX2   CLC   0(12,R3),0(RE)       EQUAL TO THIS CML                           
         BE    FACTX3                                                           
         LA    RE,12(RE)                                                        
         CR    RE,RF               AT END OF LIST                               
         BNE   FACTX2                                                           
*                                                                               
         MVC   0(12,RE),0(R3)                                                   
         LA    RF,12(RF)                                                        
         ST    RF,ENDACLST                                                      
         C     RF,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FACTX3   LA    R3,12(R3)           POINT TO NEXT ACTUAL CML                     
         OC    0(12,R3),0(R3)      ANY MORE ACTUAL CMLS                         
         BZ    FACTXIT              NO, DONE                                    
         BCT   R0,FACTX1                                                        
*                                                                               
FACTXIT  MVC   KEY,SVMYKEY         RESTORE KEY                                  
         MVC   ELCODE,SVELCODE      AND ELCODE                                  
         MVC   AIO,AIO1            RESET BACK TO AIO1                           
*                                                                               
         CLC   =X'0E03',KEY        INVOICE RECORD KEY                           
         JE    EXIT                 YES, DONE                                   
*                                                                               
*SAVE MODIFIED RECORD - MOVE FROM AIO1 TO AIO2                                  
         L     R0,AIO2             TO                                           
         L     RE,AIO1             FROM                                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         REC LEN                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              DUMMY GETREC FOR PUTREC                      
*                                                                               
*MOVE BACK MODIFIED RECORD TO AIO1                                              
         L     R0,AIO1             TO                                           
         L     RE,AIO2             FROM                                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         REC LEN                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*---------------------------------------------                                  
* PRINT OVERALL TOTALS FOR ALL CLIENTS                                          
*---------------------------------------------                                  
*                                                                               
EOJTOT   NTR1  BASE=*,LABEL=*                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   OFFICE,0            BLANK HEADINGS                               
         OC    BCLT,BCLT           1 CLIENT REQ                                 
         BNZ   *+16                 YES                                         
         MVC   QCLT,SPACES                                                      
         MVC   CLTNM,SPACES                                                     
*                                                                               
         L     R3,AXCLTBLE         EXCLUDED CLIENTS TABLE                       
*                                                                               
         MVC   P1+2(21),=CL21'EXCLUDED CLIENTS ARE:'                            
         MVC   P2+2(21),=CL21'---------------------'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    FTRSW2,SKIPPU       IS SKIP PU PROFILE OPTION SET?               
         BO    XCLT50                                                           
*                                                                               
         OC    0(3,R3),0(R3)       ANY CLTS                                     
         BZ    XCLT50               NO                                          
*                                                                               
         LA    R2,P1+2             PRINT LINE                                   
         LA    R0,5                PRINT IN 5 COLUMNS                           
         B     XCLT30                                                           
*                                                                               
         B     XCLT30                                                           
*                                                                               
XCLT20   LA    R3,3(R3)                                                         
         C     R3,MAXCLTBL         TEST END OF TABLE                            
         BNL   XCLT40               YES                                         
*                                                                               
XCLT30   OC    0(3,R3),0(R3)       ANY MORE CLTS                                
         BZ    XCLT40               NO, DONE                                    
*                                                                               
         MVC   0(3,R2),0(R3)       PRINT CLIENT                                 
         LA    R2,10(R2)                                                        
         BCT   R0,XCLT20                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,P1+2             PRINT LINE                                   
         LA    R0,5                PRINT IN 5 COLUMNS                           
         B     XCLT20                                                           
*                                                                               
XCLT40   MVI   SPACING,2                                                        
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XCLT60                                                           
*                                                                               
XCLT50   MVI   SPACING,2                                                        
         MVC   P+2(19),=C'NO CLIENTS EXCLUDED'                                  
         TM    FTRSW2,SKIPPU       IS SKIP PU PROFILE OPTION SET?               
         BZ    XCLT54               NO                                          
         MVC   P+22(16),=C'SKIPPU OPTION ON'                                    
*                                                                               
XCLT54   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
XCLT60   MVI   SPACING,2                                                        
         MVC   P+2(33),=CL33'TOTALS FOR ALL CLIENTS THIS MEDIA'                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* FLIGHT REC TOTALS *                                                           
*                                                                               
         LA    R2,AGYCTRS+FLTRECR-COUNTERS                                      
         LA    R3,=C'FLT'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* BUY REC TOTALS *                                                              
*                                                                               
         LA    R2,AGYCTRS+BUYRECER-COUNTERS                                     
         LA    R3,=C'BUY'                                                       
         GOTO1 =A(ELEMTOT),RR=SPTR94RR                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* DLR REC TOTALS *                                                              
*                                                                               
         LA    R2,AGYCTRS+DLRRECR-COUNTERS                                      
         LA    R3,=C'DLR'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* INST RECAP TOTALS *                                                           
*                                                                               
         LA    R2,AGYCTRS+INSRECER-COUNTERS                                     
         LA    R3,=C'INS'                                                       
         GOTO1 =A(ELEMTOT),RR=SPTR94RR                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* SHP RECAP TOTALS *                                                            
*                                                                               
         LA    R2,AGYCTRS+SHPRECER-COUNTERS                                     
         LA    R3,=C'SHP'                                                       
         GOTO1 =A(ELEMTOT),RR=SPTR94RR                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PATTERN REC TOTALS *                                                          
*                                                                               
         LA    R2,AGYCTRS+PATRECR-COUNTERS                                      
         LA    R3,=C'PAT'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,AGYCTRS+PTXRECR-COUNTERS                                      
         LA    R3,=C'PTX'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* INVOICE REC TOTALS *                                                          
*                                                                               
         LA    R2,AGYCTRS+INVRECR-COUNTERS                                      
         LA    R3,=C'INV'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* COMMERCIAL REC TOTALS *                                                       
*                                                                               
         LA    R2,AGYCTRS+CMLRECR-COUNTERS                                      
         LA    R3,=C'CML'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* COMMERCIAL TEXT REC TOTALS *                                                  
*                                                                               
         LA    R2,AGYCTRS+CMTRECR-COUNTERS                                      
         LA    R3,=C'CMT'                                                       
         GOTO1 =A(RECTOT),RR=SPTR94RR                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* FIND ANY MISSING CLIENTS FOR TRAFFIC RECORDS *                                
*                                                                               
FIXMIS   NTR1  BASE=*,LABEL=*                                                   
         XC    PTCT,PTCT                                                        
         XC    DELCTR,DELCTR                                                    
         L     RF,VADUMMY                                                       
         AHI   RF,32                                                            
         SRL   RF,4                                                             
         SLL   RF,4                FORCE TO DW BOUDARY                          
         SHI   RF,8                                                             
*                                                                               
         MVC   0(8,RF),=CL8'*MISCLT*'                                           
         AHI   RF,8                                                             
         ST    RF,AMISCLT                                                       
         AHI   RF,10240                                                         
         ST    RF,AMISMAX                                                       
         AHI   RF,8                                                             
*                                                                               
         MVC   0(8,RF),=CL8'*TYPTBL*'                                           
         AHI   RF,8                                                             
         ST    RF,ATYPTBL                                                       
         AHI   RF,4096                                                          
         ST    RF,ATYPMAX                                                       
*                                                                               
         L     RE,AMISCLT                                                       
         L     RF,AMISMAX                                                       
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         L     RE,ATYPTBL                                                       
         L     RF,ATYPMAX                                                       
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         BAS   RE,INITTRF          GO SW TO STRF FILE                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLI   KEY,X'0A'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
*                                                                               
LRR010   DS    0H                                                               
         MVC   SVKEY,KEY           SAVE TRAFFIC KEY                             
         XC    SVBCLT,SVBCLT                                                    
         MVI   CLTOKSW,0                                                        
*                                                                               
         TM    KEY+1,X'80'         BYPASS PASSIVE POINTERS                      
         BO    LRR120                                                           
*                                                                               
         CLC   BAGYMD,KEY+2        ONLY THIS AGENCY                             
         BNE   LRR120                NEXT REC                                   
*                                                                               
         CLI   KEY+1,X'28'         BYPASS TRAFFIC STATION ADDRESS               
         BE    LRR120                                                           
*                                                                               
         CLI   KEY+1,X'29'         BYPASS PRODUCT HOUSE ADDRESS                 
         BE    LRR120               (SPTRPRH)                                   
*                                                                               
         CLI   KEY+1,X'2B'         BYPASS NET FEED RECS                         
         BE    LRR120               (SPTRNFEED)                                 
*                                                                               
         CLI   KEY+1,X'2E'         BYPASS SPOT BUY ACTIVITY (FOR NOW)           
         BE    LRR120               (SPTRNFEED)                                 
*                                                                               
         CLI   KEY+1,X'43'         BYPASS PROG DIST LIST                        
         BE    LRR120               (SPTRNPRG)                                  
*                                                                               
         CLI   KEY+1,X'44'         BYPASS COMML CLASS                           
         BE    LRR120               (SPTRCMLCLS)                                
*                                                                               
         CLI   KEY+1,X'23'         STANDARD TEXT                                
         BNE   LRR010A              (SPTRTXT)                                   
*                                                                               
         OC    KEY+3(2),KEY+3      NO CLIENT?                                   
         BZ    LRR120                                                           
         B     LRR016                                                           
*                                                                               
LRR010A  DS    0H                                                               
         CLI   KEY+1,X'21'         COMMERCIAL RECORD                            
         BNE   LRR010C              (SPTRCMML)                                  
*                                                                               
         OC    KEY+3(2),KEY+3      NO CLIENT?                                   
         BZ    LRR120               IT IS A CML CODE REC (FOR AUTO GEN)         
         B     LRR016                                                           
*                                                                               
LRR010C  DS    0H                                                               
         CLI   KEY+1,X'2D'         SPECIAL TEXT                                 
         BNE   LRR011               (SPTRDTXT)                                  
*                                                                               
         OC    KEY+3(2),KEY+3      NO CLIENT?                                   
         BZ    LRR120                                                           
         B     LRR016                                                           
*                                                                               
LRR011   DS    0H                                                               
         CLI   KEY+1,X'2F'         STATION LABEL LIST                           
         BNE   LRR012               (SPTRLBLS)                                  
*                                                                               
         OC    KEY+3(2),KEY+3      NO CLIENT?                                   
         BZ    LRR120                                                           
         B     LRR016                                                           
*                                                                               
LRR012   DS    0H                                                               
         CLI   KEY+1,X'31'         STATION LIST                                 
         BNE   LRR013               (SPTRSTAL)                                  
*                                                                               
         OC    KEY+3(2),KEY+3      NO CLIENT?                                   
         BZ    LRR120                                                           
         B     LRR016                                                           
*                                                                               
LRR013   DS    0H                                                               
         CLI   KEY+1,X'36'         AGENCY CONTACT                               
         BNE   LRR014               (SPTRAGYCON)                                
*                                                                               
         OC    KEY+3(2),KEY+3      NO CLIENT?                                   
         BZ    LRR120                                                           
         B     LRR016                                                           
*                                                                               
LRR014   DS    0H                                                               
         CLI   KEY+1,X'41'         IS THIS NET CLIENT DIST LIST REC             
         BNE   LRR016               (SPTRNCLT)                                  
*                                                                               
         GOTO1 CLPACK,DMCB,KEY+3,SVBCLT IT HAS 3 CHAR CLT CODE                  
*                                                                               
         CLI   DMCB,X'00'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+3(2),HALF                                                    
*                                                                               
LRR016   DS    0H                                                               
         CLC   KEY+1(4),SVTPAGCL                                                
         BNE   LRR018                                                           
         CLI   CLTOKSW,C'N'        WAS IT NO CLT FOUND                          
         BE    LRR070                                                           
         B     LRR120                                                           
*                                                                               
LRR018   DS    0H                                                               
         MVC   SVTPAGCL,KEY+1                                                   
*                                                                               
         BAS   RE,INITSPT          GO SW TO SPOT FILE                           
*                                                                               
         MVI   CLTOKSW,0                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY+2                                                 
*                                                                               
         OC    SVBCLT,SVBCLT                                                    
         BZ    *+10                                                             
         MVC   KEY+2(2),SVBCLT                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         BAS   RE,INITTRF          GO SW TO TRF  FILE                           
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    LRR060                                                           
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
         MVI   CLTOKSW,C'N'        SET NO CLT FOUND                             
         BAS   RE,PTKEY                                                         
*                                                                               
         L     R4,AMISCLT                                                       
LRR020   DS    0H                                                               
         OC    0(MISLEN,R4),0(R4)                                               
         BZ    LRR024                                                           
         CLC   SVKEY+1(4),0(R4)                                                 
         BE    LRR026                                                           
*                                                                               
         LA    R4,MISLEN(,R4)                                                   
         C     R4,AMISMAX                                                       
         BL    LRR020                                                           
         DC    H'0'                                                             
LRR024   DS    0H                                                               
         MVC   0(4,R4),SVTPAGCL SAVE TYPE, BAGYMD, BCLT                         
*                                                                               
LRR026   DS    0H                                                               
         L     R0,4(,R4)                                                        
         AHI   R0,1                                                             
         ST    R0,4(,R4)                                                        
MISLEN   EQU   8                                                                
TYPLEN   EQU   8                                                                
*                                                                               
LRR060   DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CLTOKSW,C'N'        WAS NO CLT FOUND                             
         BNE   LRR120               NO, OKAY                                    
*                                                                               
* NEED TO DELETE RECS WITHOUT CLIENT                                            
*                                                                               
LRR070   DS    0H                                                               
         LH    R0,DELCTR                                                        
         AHI   R0,1                                                             
         STH   R0,DELCTR                                                        
*                                                                               
         CLI   TWAWRITE,C'Y'                                                    
         BNE   LRR100               NO UPDATE                                   
*                                                                               
         OI    KEY+13,X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
*                                                                               
LRR100   DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         L     R6,ATYPTBL                                                       
LRR110   DS    0H                                                               
         OC    0(TYPLEN,R6),0(R6)                                               
         BZ    LRR114                                                           
         CLC   0(2,R6),SVKEY+1                                                  
         BE    LRR116                                                           
         LA    R6,TYPLEN(,R6)                                                   
         C     R6,ATYPMAX                                                       
         BL    LRR110                                                           
         DC    H'0'                                                             
LRR114   DS    0H                                                               
         MVC   0(2,R6),SVKEY+1                                                  
LRR116   DS    0H                                                               
         L     R0,4(,R6)                                                        
         AHI   R0,1                                                             
         ST    R0,4(,R6)                                                        
*                                                                               
LRR120   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLI   KEY,X'0A'                                                        
         BNE   LRREND                                                           
*                                                                               
         CLC   KEY+1(4),SVTPAGCL                                                
         BNE   LRR010                                                           
*                                                                               
         CLI   CLTOKSW,0           WAS CLIENT OKAY                              
         BE    LRR120                                                           
*                                                                               
         CLC   KEY+1(4),0(R4)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R0,4(,R4)                                                        
         AHI   R0,1                                                             
         ST    R0,4(,R4)                                                        
         B     LRR070                                                           
*                                                                               
LRREND   DS    0H                                                               
         L     R4,AMISCLT                                                       
         OC    0(MISLEN,R4),0(R4)  ANY ENRIES                                   
         BNZ   LRREND20                                                         
         MVC   P+5(36),=C'ALL TRAFFIC RECORDS HAVE CLIENT RECS'                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRREND40                                                         
LRREND20 DS    0H                                                               
         MVC   P+1(23),=C'TOTAL CLIENTS MISSING ='                              
         EDIT  (B2,PTCT),(5,P+24),COMMAS=YES                                    
         MVC   P+35(15),=C'TOTAL DEL RECS='                                     
         EDIT  (B2,DELCTR),(7,P+50),COMMAS=YES                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRREND30 DS    0H                                                               
         MVC   P+1(5),=C'TYPE='                                                 
         GOTO1 HEXOUT,DMCB,0(R4),P+6,1,=C'TOG'                                  
         MVC   P+10(6),=C'AGYMD='                                               
         GOTO1 HEXOUT,DMCB,1(R4),P+16,1,=C'TOG'                                 
         MVC   P+20(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,2(R4),P+24                                           
         EDIT  (B4,4(R4)),(8,P+30),COMMAS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AHI   R4,MISLEN                                                        
         OC    0(MISLEN,R4),0(R4)                                               
         BNZ   LRREND30                                                         
*                                                                               
LRREND40 DS    0H                                                               
         BAS   RE,INITTRF          GO SW TO STRF FILE                           
*                                                                               
         J     EXIT                                                             
         L     R6,ATYPTBL                                                       
         OC    0(TYPLEN,R6),0(R6)                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
LRREND42 DS    0H                                                               
         MVC   P+1(5),=C'TYPE='                                                 
         GOTO1 HEXOUT,DMCB,0(R6),P+6,1,=C'TOG'                                  
         MVC   P+10(6),=C'AGYMD='                                               
         GOTO1 HEXOUT,DMCB,1(R6),P+16,1,=C'TOG'                                 
*        MVC   P+20(4),=C'CLT='                                                 
*        GOTO1 CLUNPK,DMCB,2(R6),P+24                                           
         EDIT  (B4,4(R6)),(8,P+30),COMMAS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AHI   R6,TYPLEN                                                        
         OC    0(TYPLEN,R6),0(R6)                                               
         BNZ   LRREND42                                                         
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PRINT THE RECORD WITH MISSING CLIENT                                          
*                                                                               
PTKEY    NTR1                                                                   
         LH    R0,PTCT                                                          
         AHI   R0,1                                                             
         STH   R0,PTCT                                                          
         MVC   P(4),=C'A/M='                                                    
         LA    R2,1(R4)                                                         
         GOTO1 HEXOUT,DMCB,SVKEY+2,P+4,1,=C'TOG'                                
*                                                                               
         MVC   P+7(2),=C'C='                                                    
         CLI   SVKEY+1,X'41'       THIS HAS UNPACKED CLIENT                     
         BNE   PTKEY10                                                          
         MVC   P+9(3),SVKEY+3                                                   
         B     PTKEY24                                                          
PTKEY10  DS    0H                                                               
         CLI   SVKEY+3,X'80'                                                    
         BNL   PTKEY20                                                          
         MVC   P+9(2),=C'X"'                                                    
         GOTO1 HEXOUT,DMCB,SVKEY+3,P+11,2,=C'TOG'                               
         MVI   P+15,C'"'                                                        
         B     PTKEY24                                                          
PTKEY20  DS    0H                                                               
         GOTO1 CLUNPK,DMCB,SVKEY+3,P+9                                          
*                                                                               
PTKEY24  DS    0H                                                               
*                                                                               
         MVC   P+17(5),=C'TYPE='                                                
         GOTO1 HEXOUT,DMCB,SVKEY+1,P+23,1,=C'TOG'                               
*                                                                               
         MVC   P+29(4),=C'KEY='                                                 
         GOTO1 HEXOUT,DMCB,SVKEY,P+34,18,=C'TOG'                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PTKEYX   DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
* RESET FILES TO SPOT *                                                         
*                                                                               
INITSPT  MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVI   SYSFIL,C'S'                                                      
         MVI   SYSFIL+1,C'P'                                                    
         MVI   SYSFIL+2,C'T'                                                    
         BR    RE                                                               
*                                                                               
* RESET FILES TO NET *                                                          
*                                                                               
INITTRF  MVI   DATADISP+1,24       SET FROM SPOT TO NET                         
         MVI   LKEY+1,13                                                        
         MVI   SYSDIR,C'T'                                                      
         MVI   SYSDIR+1,C'R'                                                    
         MVI   SYSDIR+2,C'F'                                                    
         MVI   SYSFIL,C'T'                                                      
         MVI   SYSFIL+1,C'R'                                                    
         MVI   SYSFIL+2,C'F'                                                    
         BR    RE                                                               
*                                                                               
INITXSP  MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   SYSDIR,C'X'                                                      
         MVI   SYSDIR+1,C'S'                                                    
         MVI   SYSDIR+2,C'P'                                                    
         MVI   SYSFIL,C'X'                                                      
         MVI   SYSFIL+1,C'S'                                                    
         MVI   SYSFIL+2,C'P'                                                    
         MVI   LSTATUS+1,4                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
TRAPERR2 GOTO1 VTRAERR             USING GETTXT CALL                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
UNITSW   DC    X'00'                                                            
*                                                                               
*                                                                               
HEADINGM SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,34,C'MISSING CLIENT LIST'                                     
         SSPEC H2,34,C'-------------------'                                     
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H5,85,RUN                                                        
         SSPEC H5,73,REPORT                                                     
         SSPEC H6,73,REQUESTOR                                                  
         SSPEC H6,103,PAGE                                                      
         SSPEC H9,3,C'DATE'                                                     
         SSPEC H10,3,C'--------'                                                
         DC    X'00'               END MARKER FOR SSPECS                        
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRKEYS                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV          INVOICE RECORD                               
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE SPTRAFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAE7D                                                       
*        PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR94RR DS    F                                                                
COUNTERS DS    0F                                                               
FLTRECR  DS    F                   FLIGHT RECS READ                             
FLTRECS  DS    F                               TO BE DEL                        
FLTRECDL DS    F                               WRITTEN DELETED                  
BUYRECER DS    F                   BUY ELEMS READ                               
BUYRECEL DS    F                       ELEMS TO BE DELETED                      
BUYRECED DS    F                       ELEMS DELETED                            
BUYRECEM DS    F                       EMPTY RECS READ                          
BUYRECR  DS    F                       RECS READ                                
BUYRECU  DS    F                       RECS UPDATED                             
BUYRECW  DS    F                       RECS WRITTEN                             
BUYRECDL DS    F                       RECS DELETED                             
DLRRECR  DS    F                                                                
DLRRECS  DS    F                                                                
DLRRECDL DS    F                                                                
INSRECER DS    F                   INSTR RECAP ELEMS READ                       
INSRECEL DS    F                               ELEMS TO BE DELETED              
INSRECED DS    F                               ELEMS DELETED                    
INSRECEM DS    F                               EMPTY RECS READ                  
INSRECR  DS    F                               RECS READ                        
INSRECU  DS    F                               RECS UPDATED                     
INSRECW  DS    F                               RECS WRITTEN                     
INSRECDL DS    F                               RECS DELETED                     
SHPRECER DS    F                                                                
SHPRECEL DS    F                                                                
SHPRECED DS    F                                                                
SHPRECEM DS    F                                                                
SHPRECR  DS    F                                                                
SHPRECU  DS    F                                                                
SHPRECW  DS    F                                                                
SHPRECDL DS    F                                                                
PATRECR  DS    F                                                                
PATRECS  DS    F                                                                
PATRECDL DS    F                                                                
INVRECR  DS    F                                                                
INVRECS  DS    F                                                                
INVRECDL DS    F                                                                
PTXRECR  DS    F                                                                
PTXRECS  DS    F                                                                
PTXRECDL DS    F                                                                
CMLRECR  DS    F                                                                
CMLRECS  DS    F                                                                
CMLRECDL DS    F                                                                
CEASICT  DS    F                                                                
CMTRECR  DS    F                                                                
CMTRECS  DS    F                                                                
CMTRECDL DS    F                                                                
ENDCTRS  EQU   *                                                                
CTRSIZ   EQU   (*-COUNTERS)/4                                                   
AGYCTRS  DS    (CTRSIZ)F                                                        
STRAPLST DS    F                   START OF ACTIVE PATTERN LIST                 
ENDAPLST DS    F                                                                
MAXTBLSZ DS    F                                                                
STRACLST DS    F                   START OF ACTIVE COMMERCIAL LIST              
ENDACLST DS    F                                                                
MAXACLST DS    F                   MAX ACTIVE CML LIST                          
AMISCLT  DS    F                                                                
AMISMAX  DS    F                                                                
ATYPTBL  DS    F                                                                
ATYPMAX  DS    F                                                                
AXCLTBLE DS    F                   START OF EXCLUDED CLIENTS LIST               
MAXCLTBL DS    F                                                                
*                                                                               
SVREGRE  DS    F                   SAVE REGISTER RE                             
SVREGRF  DS    F                    RF                                          
SVREG    DS    F                    AND ANY OTHER REGISTER                      
*                                                                               
UTL      DS    A                                                                
VTRPACK  DS    A                                                                
*                                                                               
TRFSYS   DS    XL1                                                              
SPTSYS   DS    XL1                                                              
*                                                                               
WRTRECSW DS    XL1                 WRITE RECORD SWITCH                          
*                                                                               
OFFICE   DS    CL1                                                              
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
ENDATE   DS    XL3                                                              
ENDATEP  DS    XL2                                                              
PRTDATE  DS    CL8                                                              
SVBCLT   DS    XL2                                                              
BBCLT    DS    XL2                                                              
*                                                                               
SVPRD    DS    XL1                                                              
SVSLN    DS    XL1                                                              
SVMKTSTA DS    XL5                                                              
SVCODE   DS    XL1                                                              
SVCMLCOD DS    CL12                                                             
SVCMLSEQ DS    XL3                                                              
SVCMLSTA DS    XL1                                                              
SVCRLSE  DS    XL3                                                              
SVCRCL   DS    XL3                                                              
CMLCMTFL DS    XL1                 FLAG TO SHOW IF COMMENTS REC FOUND           
*                                                                               
SVCML    DS    CL12                8 CHAR ISCII                                 
SVCNTRCT DS    XL8                 PACKED CENTERCUT                             
SVHIDEF  DS    XL8                   HIDEF                                      
SVADIDP  DS    XL8                   ADID                                       
*                                                                               
THISDATE DS    XL3                                                              
*                                                                               
FILTERS  DS    0CL16                                                            
DATE     DS    CL6                                                              
DATEFTR  DS    XL3                                                              
DATE2FTR DS    XL3                                                              
DATESFTR DS    XL1                                                              
HOLDSIGN DS    XL1                                                              
FTRSW1   DS    XL1                                                              
FLTSW    EQU   X'80'                                                            
BUYSW    EQU   X'40'                                                            
DLRSW    EQU   X'20'                                                            
INSSW    EQU   X'10'                                                            
SHPSW    EQU   X'08'                                                            
PATSW    EQU   X'04'                                                            
CMLSW    EQU   X'02'                                                            
ONLY     EQU   X'01'                                                            
FTRSW2   DS    XL1                                                              
TESTSW   EQU   X'80'                                                            
PRTSW    EQU   X'40'                                                            
SKIPPU   EQU   X'20'                                                            
PTCT     DS    H                                                                
DELCTR   DS    H                                                                
*                                                                               
SVTPAGCL DS    XL4                 SAVED TYPE, BAGY, BCLT                       
*                                                                               
SVWORK   DS    CL64                SAVE WORK AREA                               
SVMYKEY  DS    CL(L'KEY)           SAVE KEY                                     
SVACTCML DS    XL48                UPTO 4 12 CHAR CMLS                          
SVELCODE DS    CL(L'ELCODE)        SAVE ELCODE                                  
*                                                                               
CLTOKSW  DS    XL1                 0 = CLT FOUND, 1 = NO CLT                    
*                                                                               
SVAGMDCL DS    XL3                 SAVED AGYMD & CLT                            
*                                                                               
HOLDP    DS    CL132                                                            
                                                                                
* BUFFER SIZE FOR ACTIVE CML TABLE                                              
*                                                                               
CMLELEN  EQU   12                  LEN OF CML ENTRY                             
CMLNUM   EQU   100000              NUMBER OF CMLS                               
*                                                                               
* BUFFER SIZE FOR ACTIVE PAT TABLE                                              
*                                                                               
PATELEN  EQU   APLISTLN            LEN OF PAT ENTRY                             
PATNUM   EQU   150000              NUMBER OF PATTERNS                           
*                                                                               
* FORMAT OF MISSING CLT REC COUNTS TABLE                                        
*                                                                               
* TYPE   1                                                                      
* BAGYMD 1 BYTE                                                                 
* BCLT   2                                                                      
* RECS   4                                                                      
*                                                                               
* FORMAT OF REC TYPES AND COUNTS                                                
*                                                                               
* TYPE   1 BYTE                                                                 
* BAGYMD 1                                                                      
* BAGYMD 1                                                                      
* SPARE  2                                                                      
* COUNT  4                                                                      
*                                                                               
TABLES   DS    0D                                                               
         SPACE 3                                                                
* OFFLINE REPORT                                                                
*                                                                               
APLIST   DSECT                                                                  
APLENT   DS   0XL8                                                              
APLPRD1  DS    XL1                                                              
APLSLN1  DS    XL1                                                              
APLPRD2  DS    XL1                                                              
APLSLN2  DS    XL1                                                              
APLCODE  DS    XL1                                                              
APLREFS  DS    XL3                                                              
APLNEXT  EQU   *                                                                
APLISTLN EQU   APLNEXT-APLIST                                                   
         SPACE 3                                                                
* OFFLINE REPORT                                                                
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PFPRD    DS    CL3                 FLIGHTS                                      
         DS    CL3                                                              
PFEDATE  DS    CL8                                                              
         DS    CL2                                                              
PFDATES  DS    CL92                                                             
         ORG   P                                                                
         DS    CL3                                                              
PBMKT    DS    CL4                                                              
         DS    CL5                                                              
PBSTA    DS    CL7                                                              
         DS    CL3                                                              
PBPRD    DS    CL7                 TRAFFIC BUYS                                 
         DS    CL3                                                              
PBPTR    DS    CL7                                                              
         DS    CL4                                                              
PBCODE   DS    CL3                                                              
         DS    CL4                                                              
PBDATES  DS    CL17                                                             
         ORG   P                                                                
         DS    CL3                                                              
PDMKT    DS    CL4                 DEALER TAGS                                  
         DS    CL4                                                              
PDPROD   DS    CL3                                                              
         DS    CL7                                                              
PDTAG    DS    CL4                                                              
         DS    CL6                                                              
PDNAME   DS    CL24                                                             
         DS    CL1                                                              
PDADDR   DS    CL24                                                             
         DS    CL2                                                              
PDDLN    DS    CL3                                                              
         DS    CL2                                                              
PDDATES  DS    CL17                                                             
         DS    CL2                                                              
PDTYPE   DS    CL4                                                              
         ORG   P                                                                
         DS    CL3                 INSTR RECAP                                  
PIMKT    DS    CL4                                                              
         DS    CL2                                                              
PISTA    DS    CL7                                                              
         DS    CL3                                                              
PIPRD    DS    CL7                                                              
         DS    CL3                                                              
PIPTR    DS    CL7                                                              
         DS    CL4                                                              
PICODE   DS    CL3                                                              
         DS    CL3                                                              
PIREV    DS    CL3                                                              
         DS    CL4                                                              
PIINSTDT DS    CL8                                                              
         DS    CL2                                                              
PIREF    DS    CL3                                                              
         DS    CL1                                                              
PISUB    DS    CL3                                                              
         DS    CL5                                                              
PIFTDLTD DS    CL17                                                             
         DS    CL5                                                              
PIDEALER DS    CL10                                                             
         ORG   P                                                                
         DS    CL3                                                              
PSMKT    DS    CL4                 SHIP RECAP                                   
         DS    CL2                                                              
PSSTA    DS    CL7                                                              
         DS    CL3                                                              
PSCML    DS    CL8                                                              
         DS    CL3                                                              
PSCML2   DS    CL8                                                              
         DS    CL6                                                              
PSINSTDT DS    CL8                                                              
         DS    CL3                                                              
PSHIPDT  DS    CL8                                                              
         DS    CL6                                                              
PSFTD    DS    CL8                                                              
         DS    CL3                                                              
PSLTD    DS    CL8                                                              
         DS    CL3                                                              
PSSTATUS DS    CL8                                                              
         ORG   P                                                                
         DS    CL2                                                              
PPPRD    DS    CL7                 PATTERN RECORD                               
         DS    CL2                                                              
PPPTR    DS    CL7                                                              
         DS    CL4                                                              
PPCODE   DS    CL3                                                              
         DS    CL2                                                              
PPREF    DS    CL3                                                              
         DS    CL1                                                              
PPSUB    DS    CL3                                                              
         DS    CL3                                                              
PPDESC   DS    CL16                                                             
         DS    CL2                                                              
PPFTDLTD DS    CL17                                                             
         DS    CL2                                                              
PPUSED   DS    CL8                                                              
         DS    CL2                                                              
PPCMLS   DS    CL17                                                             
         DS    CL1                                                              
PPDEL    DS    CL7                                                              
         DS    CL1                                                              
PPTXT    DS    CL10                                                             
         ORG   P                                                                
         DS    CL2                                                              
PCCML    DS    CL12                COMMERCIAL RECORD                            
         DS    CL2                                                              
PCTITLE  DS    CL15                                                             
         DS    CL2                                                              
PCLEN    DS    CL3                                                              
         DS    CL3                                                              
PCTYPE   DS    CL4                                                              
         DS    CL2                                                              
PCDATES  DS    CL17                                                             
         DS    CL2                                                              
PCCLTCML DS    CL20                                                             
         DS    CL2                                                              
PCINACT  DS    CL14                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPTRA94   06/20/19'                                      
         END                                                                    
