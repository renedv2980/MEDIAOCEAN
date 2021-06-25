*          DATA SET ACPRO54    AT LEVEL 033 AS OF 01/07/16                      
*PHASE T60B54A                                                                  
*                                                                               
*INCLUDE ACSRCHC                                                                
*INCLUDE SRCHCALL                                                               
*                                                                               
***********************************************************************         
* LEVEL CHANGE DOCUMENTATION                                          *         
* --------------------------                                          *         
* TKLU 24APR03 001 - NEW ARTICLE MAINT. PROGRAM TO REPLACE PL/MAINT., *         
*                    SEE PROJECT (DU01-0584), CONTAINS ACPROABAD      *         
* TKLU 19OCT03 002 - LIVE DATE (INCLUDE LIVE SCREEN)                  *         
* TKLU 22OCT03 003 - MAKE SURE NO CHNAGE FROM LIST IS DONE            *         
* TKLU 03NOV03 004 - USE RAPPER TO MAINTAIN ACTIVITY POINTERS         *         
* TKLU 11NOV03 005 - INIT CURSUPC TO ALLOW FC ON INTERNAL ARTICLES    *         
* TKLU 15JAN04 006 - ERROR RATHER THEN DUMP IF CHANGE FROM LIST/SEL   *         
* TKLU 04FEB04 007 - SUPPRESS FOREIGN DESCRITION FOR UK               *         
* TKLU 17FEB04 008 - BUG FIX WHEN ADDING PASSIVES                     *         
* TKLU 26FEB04 009 - SEARCH SUPPORT ON SUPPLIER                       *         
* TKLU 31AUG04 010 - <ACCQ 8841> DISALLOW AGENCY CURRENCY AS CURRENCY *         
* TKLU 27SEP04 011 - UK WORK CODE INT/EXT BUG FIX                     *         
* TKLU 17DEC05 012 - <DU01-5029> - MCS ARTICLE ENHANCEMENTS           *         
* TKLU 08FEB06 012 - <DU01-5029> - CHANGE W/C VALIDATION FOR UK       *         
* TKLU 23DEC15 019 <PCA02189> DSIALLOW NEGATIVE PRICES                *         
***********************************************************************         
* INFORMATION                                                         *         
* -----------                                                         *         
* THIS BOOK WILL ADD/CHANGE/DISPLAY ARTICLE RECORDS (DELETE/RESTORE   *         
* AS DDS ONLY) OF NEW STYLE. IT WILL MAINTAIN ACTIVITY POINTERS AND   *         
* ARTICLE PASSIVES, TOO.                                              *         
* THERE IS A PROFILE TO DRIVE OFFICE/CLIENT MATCHING (#5 OF ACC/PRO)  *         
* NOTE: GEGENCON WILL CALL THIS APPLICATION WITH 'OTHERS' TYPE, FIRST *         
*       WITH MODE=NEWSCR (SCREEN), THEN WITH MODE=VALKEY (IGNORED),   *         
*       FINALLY WITH MODE=VALREC (PROCESS). SELECT IS DIFFERENT.      *         
***********************************************************************         
         TITLE 'ITEMS RECORD MAINTENANCE'                                       
T60B54   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B54**,R7,R8,RR=R2                                           
         ST    R2,RELO                                                          
         L     RE,=V(ACSRCHC)                                                   
         A     RE,RELO                                                          
         ST    RE,AACSRCHC                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VBLDCUR,CBLDCUR-COMFACSD(RF)                                     
*                                                                               
         GOTO1 DICTATE,DMCB,C'LU  ',DDIN,DDOUT                                  
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)   TODAY'S DATE                       
*                                                                               
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTNSEL                                                   
         BNE   MODE02                                                           
         CLI   MODE,DISPKEY                                                     
         BE    MODE50                                                           
         CLI   MODE,VALREC                                                      
         BNE   MODE02                                                           
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
MODE02   CLI   MODE,NEWSCR         PROCESS NEWSCR AND VALREC ONLY               
         BE    MODE04                                                           
         CLI   MODE,VALREC                                                      
         BNE   MODE90                                                           
*                                                                               
         USING FLDHDRD,R2                                                       
MODE04   CLI   MODE,NEWSCR         NEW SCREEN DISPLAY CURRENCY                  
         BNE   MODE10                                                           
         XC    OKEYX,OKEYX                                                      
         XC    OKEYY,OKEYY                                                      
*&&UK                                                                           
         CLI   AGYCNTRY,CTRYGBR                                                 
         BNE   MODE06                                                           
         MVC   PROTFDE,MYSPACES                                                 
         OI    PROTFDEH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVC   PROTLNG,MYSPACES                                                 
         OI    PROTLNGH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVC   PROTTXT,MYSPACES                                                 
         OI    PROTTXTH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    PROLDSCH+FLDATB-FLDHDRD,FATBPROT                                 
         OI    PROLLNGH+FLDATB-FLDHDRD,FATBPROT                                 
         OI    PROLTXTH+FLDATB-FLDHDRD,FATBPROT                                 
         OI    PROLTX2H+FLDATB-FLDHDRD,FATBPROT                                 
         OI    PROLTX3H+FLDATB-FLDHDRD,FATBPROT                                 
         B     MODE08                                                           
*                                                                               
MODE06   MVC   PRONICL,MYSPACES                                                 
         OI    PRONICLH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    PRONICIH+FLDATB-FLDHDRD,FATBPROT                                 
         MVC   PROISAL,MYSPACES                                                 
         OI    PROISALH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    PROISAIH+FLDATB-FLDHDRD,FATBPROT                                 
*&&                                                                             
MODE08   MVI   ADDIND,0                                                         
*&&UK                                                                           
         TM    COMPSTA6,CPYSFBIL   FOREIGN CURRENCY ON BILLING OR               
         BNZ   MODE90                                                           
         TM    COMPSTAA,CPYSFCES   ON ESTIMATES ALLOWED                         
         BNZ   MODE90                                                           
         MVC   PROCURT,MYSPACES                                                 
         LA    R2,PROCURTH                                                      
         OI    FLDOIND,FOUTTRN                                                  
         MVC   PROCURC,MYSPACES                                                 
         LA    R2,PROCURCH                                                      
         OI    FLDATB,FATBPROT                                                  
         OI    FLDOIND,FOUTTRN                                                  
*&&                                                                             
         B     MODE90                                                           
*                                                                               
MODE10   MVI   CUDTORYN,C'N'                                                    
         CLI   DDS,C'Y'                                                         
         BNE   MODE12                                                           
         CLC   CONOTH(8),=C'DATE=ALL'                                           
         BNE   MODE12                                                           
         MVI   CUDTORYN,C'Y'                                                    
*                                                                               
MODE12   LA    R2,PROARTCH                                                      
         CLI   ACTNUM,ACTNCHA                                                   
         BE    MODE20                                                           
         XC    OKEYX,OKEYX                                                      
         CLI   ACTNUM,ACTNADD                                                   
         BE    MODE30                                                           
         CLI   ACTNUM,ACTNDIS                                                   
         BE    MODE40                                                           
         CLI   ACTNUM,ACTNDEL      (DDS ONLY)                                   
         BE    MODE60                                                           
         CLI   ACTNUM,ACTNRES      (DDS ONLY)                                   
         BE    MODE70                                                           
         B     MODE90                                                           
*                                                                               
MODE20   DS    0H                  CHANGE A RECORD                              
         BAS   RE,VKEY                                                          
         BAS   RE,GREC                                                          
         BNE   ERREND                                                           
         BAS   RE,DKEY                                                          
         CLC   OKEYX,OKEY1         SAME RECORD?                                 
         BE    MODE22                                                           
         BAS   RE,DREC                                                          
         MVC   OKEYX,OKEY1                                                      
*                                                                               
MODE22   BAS   RE,VREC                                                          
         BAS   RE,CREC                                                          
         BAS   RE,CPAS                                                          
         BAS   RE,DREC             REDISPLAY RECORD                             
         B     MODE80                                                           
*                                                                               
MODE30   DS    0H                  ADD A RECORD                                 
         BAS   RE,VKEY                                                          
         CLC   OKEY1,OKEYY                                                      
         BE    MODE32                                                           
         OI    ADDIND,ADD80                                                     
         MVC   OKEYY,OKEY1                                                      
*                                                                               
MODE32   BAS   RE,SREC                                                          
         BNE   ERREND                                                           
         BAS   RE,LPAS                                                          
         BAS   RE,VREC                                                          
         BAS   RE,AREC                                                          
         BAS   RE,APAS                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         BNE   MODE34                                                           
         OI    ADDIND,ADD08        SET RECORD ADDED                             
         B     MODE80                                                           
*                                                                               
MODE34   LA    R2,PROARTCH                                                      
         DC    H'0'                                                             
*                                                                               
MODE40   DS    0H                  DISPLAY A RECORD                             
         BAS   RE,VKEY                                                          
         BAS   RE,GREC                                                          
         BNE   ERREND                                                           
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         BE    MODE90                                                           
         LA    R2,PROARTCH                                                      
         B     ERREND                                                           
*                                                                               
MODE50   DS    0H                  SELECT A RECORD                              
         L     RF,AIO              (PASS RECORD KEY INTO OKEY1)                 
         ST    RF,MYAIO2                                                        
         MVC   OKEY1,0(RF)                                                      
         BAS   RE,GREC                                                          
         BNE   ERREND                                                           
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         L     RF,MYAIO2                                                        
         BE    MODE90                                                           
         LA    R2,PROARTCH                                                      
         B     ERREND                                                           
*                                                                               
MODE60   DS    0H                  DELETE A RECORD                              
         BAS   RE,VKEY                                                          
         BAS   RE,XREC                                                          
         CLI   ERROR,0                                                          
         BNE   ERREND                                                           
         B     MODE80                                                           
*                                                                               
MODE70   DS    0H                  RESTORE A RECORD                             
         BAS   RE,VKEY                                                          
         BAS   RE,GREC                                                          
         BNE   ERREND                                                           
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         BE    MODE72                                                           
         DC    H'0'                                                             
MODE72   BAS   RE,SPAS                                                          
         BAS   RE,VREC                                                          
         BAS   RE,CREC                                                          
         BAS   RE,CPAS                                                          
         B     MODE80                                                           
*                                                                               
MODE80   DS    0H                  DO ACTIVITY POINTERS HERE                    
         MVI   RAPACTN,RAPAPTR     (SEE VRAPPER CALL AT VALREC)                 
         MVC   RAPCPY,CUL                                                       
         MVI   RAPRTYP,RAPKRART                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOMFACS                                                 
         LA    RF,OREC1                                                         
         ST    RF,RAPAREC                                                       
         MVC   RAPRDA,MYDA                                                      
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    MODE90                                                           
         DC    H'0'                                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
MODE90   LA    R2,PROARTCH         ALL EXIT HERE                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ARTRECD,R4                                                       
VKEY     NTR1  ,                   VALIDATE INPUT FIELDS FIRST                  
         LA    R4,OKEY1                                                         
         XC    ARTKEY,ARTKEY                                                    
         MVI   ARTKTYP,ARTKTYPQ                                                 
         MVI   ARTKSUB,ARTKAQ                                                   
         MVC   ARTKCPY,CUL                                                      
         SPACE 1                                                                
         LA    R2,PROARTCH                                                      
         MVI   ERROR,MISSING       ITEMS NUMBER                                 
         CLI   PROARTCH+5,0                                                     
         BE    ERREND                                                           
         MVI   ERROR,INVALID                                                    
         CLI   PROARTCH+5,L'PROARTC                                             
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVI   ERROR,INVALID       CHECK FOR ALPHANUMERIC INPUT                 
         OC    PROARTC,MYSPACES                                                 
         LA    RF,PROARTC                                                       
         LA    R1,L'PROARTC                                                     
VKEY02   LA    RE,MYCHARS                                                       
VKEY04   CLI   0(RE),HEXFF                                                      
         BE    ERREND                                                           
         CLC   0(1,RE),0(RF)                                                    
         BE    VKEY06                                                           
         LA    RE,1(RE)                                                         
         B     VKEY04                                                           
VKEY06   AHI   RF,1                                                             
         BCT   R1,VKEY02                                                        
         MVC   ARTKART,PROARTC                                                  
         SPACE 1                                                                
         MVC   ARTKOFF,MYSPACES    OFFICE CODE (OR BLANK)                       
         MVC   CUROFFC,MYSPACES                                                 
         LA    R2,PROOFFCH                                                      
         CLI   PROOFFCH+5,0                                                     
         BE    VKEY08                                                           
         OC    PROOFFC,MYSPACES                                                 
         BAS   RE,GETOFF                                                        
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK          CHECK FOR LIMIT ACCESS                       
         MVC   OFFAOFFC,CUROFFC                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         MVI   ERROR,SECLKOUT                                                   
         GOTO1 OFFAL                                                            
         BNE   ERREND                                                           
         MVC   ARTKOFF,CUROFFC                                                  
         DROP  R1                                                               
         SPACE 1                                                                
VKEY08   MVC   ARTKCLI,MYSPACES    CLIENT CODE (OR MYSPACES)                    
         LA    R2,PROCLICH                                                      
         CLI   PROCLICH+5,0                                                     
         BE    VKEY14                                                           
         OC    PROCLIC,MYSPACES                                                 
         BAS   RE,GETCLI                                                        
         CLC   CURCLIO,MYSPACES                                                 
         BE    VKEY10                                                           
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK          CHECK FOR LIMIT ACCESS                       
         MVC   OFFAOFFC,CURCLIO                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         MVI   ERROR,SECLKOUT                                                   
         GOTO1 OFFAL                                                            
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY10   CLC   CUROFFC,MYSPACES                                                 
         BE    VKEY12                                                           
         CLC   CURCLIO,MYSPACES                                                 
         BE    VKEY12                                                           
         MVI   ERROR,MIXDOFF       OFFICES MUST MATCH IF SET                    
*&&UK                                                                           
         CLI   PRODPROF+4,NOMATCHQ (UNLESS PROFILE OVERRIDE)                    
         BE    VKEY12                                                           
*&&                                                                             
         CLC   CURCLIO,CUROFFC                                                  
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY12   MVC   ARTKCLI,CURCLIC                                                  
         DROP  R1                                                               
         SPACE 1                                                                
VKEY14   MVC   ARTKSUP,MYSPACES    CLIENT CODE (OR MYSPACES)                    
         XC    CURSUPF,CURSUPF                                                  
         LA    R2,PROSUPCH                                                      
         CLI   PROSUPCH+5,0                                                     
         BE    VKEYX                                                            
         LA    R1,OKEY3                                                         
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,MYSPACES                                                  
         MVC   ACTKCPY,CUL                                                      
         MVI   ERROR,INVALID                                                    
         GOTO1 AACSRCHC,DMCB,(4,PROSUPCH),ATWA,0,ACOMFACS,(0,0)                 
         OC    PROSUPC,MYSPACES                                                 
         BAS   RE,GETSUP                                                        
         MVC   ARTKSUP,PROSUPC                                                  
         SPACE 1                                                                
VKEYX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING ARTRECD,R4                                                       
DKEY     NTR1  ,                                                                
         LA    R4,OKEY1                                                         
         MVC   PROARTC,ARTKART                                                  
         OI    PROARTCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         SPACE 1                                                                
         MVC   PROOFFN,MYSPACES                                                 
         MVC   PROCLIN,MYSPACES                                                 
         MVC   PROSUPN,MYSPACES                                                 
         OI    PROOFFNH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    PROCLINH+FLDOIND-FLDHDRD,FOUTTRN                                 
         OI    PROSUPNH+FLDOIND-FLDHDRD,FOUTTRN                                 
         SPACE 1                                                                
         CLC   ARTKOFF,MYSPACES                                                 
         BE    DKEY02                                                           
         MVC   PROOFFC,ARTKOFF                                                  
         OI    PROOFFCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         BAS   RE,GETOFF                                                        
         MVC   PROOFFN,CUROFFN                                                  
         SPACE 1                                                                
DKEY02   CLC   ARTKCLI,MYSPACES                                                 
         BE    DKEY04                                                           
         MVC   PROCLIC,ARTKCLI                                                  
         OI    PROCLICH+FLDOIND-FLDHDRD,FOUTTRN+FOUTCUR                         
         BAS   RE,GETCLI                                                        
         MVC   PROCLIN,CURCLIN                                                  
         SPACE 1                                                                
DKEY04   XC    CURSUPF,CURSUPF                                                  
         CLC   ARTKSUP,MYSPACES                                                 
         BE    DKEY06                                                           
         MVC   PROSUPC,ARTKSUP                                                  
         OI    PROSUPCH+FLDOIND-FLDHDRD,FOUTTRN+FOUTCUR                         
         BAS   RE,GETSUP                                                        
         MVC   PROSUPN,CURSUPN                                                  
         SPACE 1                                                                
DKEY06   DS    0H                                                               
         SPACE 1                                                                
DKEYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DREC     NTR1  ,                                                                
         LA    R4,OREC1                                                         
         ST    R4,AIO                                                           
         SPACE 1                                                                
*&&UK                                                                           
         TM    COMPSTA6,CPYSFBIL   FOREIGN CURRENCY ON BILLING OR               
         BNZ   DREC00                                                           
         TM    COMPSTAA,CPYSFCES   ON ESTIMATES ALLOWED                         
         BZ    DREC02                                                           
DREC00   LA    R2,PROCURCH         SET TO UNPROTECTED                           
         NI    FLDATB,HEXFF-FATBPROT                                            
*&&                                                                             
DREC02   LA    R2,PRODAT1H                                                      
         LA    R1,9                                                             
DREC04   NI    FLDATB,HEXFF-FATBPROT                                            
         AHI   R2,PROPRI1H-PRODAT1H                                             
         NI    FLDATB,HEXFF-FATBPROT                                            
         AHI   R2,PRODAT2H-PROPRI1H                                             
         BCT   R1,DREC04                                                        
         SPACE 1                                                                
         MVC   PRODADR,MYSPACES                                                 
         OI    PROASEQH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVC   PROASEQ,MYSPACES                                                 
         OI    PRODADRH+FLDOIND-FLDHDRD,FOUTTRN                                 
         SPACE 1                                                                
         TWAXC PROWRKCH                                                         
         SPACE 1                                                                
         MVI   PRODESCH+5,0        ENSURE ALL TEXTS ARE 0 LENGTH                
*&&UK*&& MVI   PROLDSCH+5,0                                                     
*&&UK*&& MVI   PROLLNGH+5,0                                                     
         LA    R2,PROTEXTH                                                      
         MVI   5(R2),0                                                          
         AHI   R2,L'PROTEXT+L'PROTEXTH+L'PROTEXTX                               
         MVI   5(R2),0                                                          
         AHI   R2,L'PROTEXT+L'PROTEXTH+L'PROTEXTX                               
         MVI   5(R2),0                                                          
*&&UK                                                                           
         LA    R2,PROLTXTH                                                      
         MVI   5(R2),0                                                          
         AHI   R2,L'PROLTXT+L'PROLTXTH+L'PROLTXTX                               
         MVI   5(R2),0                                                          
         AHI   R2,L'PROLTXT+L'PROLTXTH+L'PROLTXTX                               
         MVI   5(R2),0                                                          
         LA    R2,PROLLNGH                                                      
         MVI   5(R2),0                                                          
*&&                                                                             
         BAS   RE,MYPEROUT         DISPLAY RECORD VALUES                        
         MVC   PROLACT,MYSPACES                                                 
         MVC   PROLACT(20),WORK+20                                              
         OI    PROLACTH+FLDOIND-FLDHDRD,FOUTTRN                                 
         SPACE 1                                                                
         CLI   DDS,C'Y'            TEST DDS TERMINAL                            
         BNE   DREC06                                                           
         XOUT  MYDA,PRODADR,4                                                   
         OI    PRODADRH+FLDOIND-FLDHDRD,FOUTTRN                                 
         DROP  R4                                                               
         SPACE 1                                                                
         USING ARTRECD,R3                                                       
DREC06   L     R3,AIO              POINT TO ITEMS RECORD                        
         MVI   PROPFLX,C' '                                                     
         TM    ARTRSTA,ARTKFLXQ                                                 
         BZ    *+10                                                             
         MVC   PROPFLX,AC@YES                                                   
         OI    PROPFLXH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROPFLXH+5,L'PROPFLX                                             
         SPACE 1                                                                
         MVI   PRONOPR,C' '                                                     
         TM    ARTRSTA,ARTKNOPQ                                                 
         BZ    *+10                                                             
         MVC   PRONOPR,AC@YES                                                   
         OI    PRONOPRH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PRONOPRH+5,L'PRONOPR                                             
         SPACE 1                                                                
         MVI   PRODSCO,C' '                                                     
         TM    ARTRSTA,ARTKDAMQ                                                 
         BZ    *+10                                                             
         MVC   PRODSCO,AC@YES                                                   
         OI    PRODSCOH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PRODSCOH+5,L'PRODSCO                                             
*&&UK                                                                           
         MVI   PRONICI,C' '                                                     
         TM    ARTRSTA,ARTKNICQ                                                 
         BZ    *+10                                                             
         MVC   PRONICI,AC@YES                                                   
         OI    PRONICIH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PRONICIH+5,L'PRONICI                                             
         SPACE 1                                                                
         MVI   PROISAI,C' '                                                     
         TM    ARTRSTA+ARTKSTA2-ARTKSTAT,ARTKSART                               
         BZ    *+10                                                             
         MVC   PROISAI,AC@YES                                                   
         OI    PROISAIH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROISAIH+5,L'PROISAI                                             
*&&                                                                             
         MVI   PROISTI,C' '                                                     
         TM    ARTRSTA+ARTKSTA2-ARTKSTAT,ARTKSTIM                               
         BZ    *+10                                                             
         MVC   PROISTI,AC@YES                                                   
         OI    PROISTIH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROISTIH+5,L'PROISTI                                             
         SPACE 1                                                                
DREC07   AHI   R3,ARTRFST-ARTRECD                                               
         USING AFDELD,R3                                                        
         USING ARTRECD,R4                                                       
DREC08   CLI   AFDEL,AFDELQ                                                     
         BE    DREC12                                                           
         CLI   AFDEL,PRIELQ                                                     
*&&UK*&& BE    DREC16                                                           
*&&US*&& BE    DREC20                                                           
         CLI   AFDEL,NAMELQ                                                     
         BE    DREC24                                                           
*&&UK*&& CLI   AFDEL,XNMELQ                                                     
*&&UK*&& BE    DREC26                                                           
         CLI   AFDEL,SCMELQ                                                     
         BE    DREC28                                                           
         CLI   AFDEL,0                                                          
         BE    DREC36                                                           
DREC10   XR    R0,R0                                                            
         IC    R0,AFDLN                                                         
         AR    R3,R0                                                            
         B     DREC08                                                           
         SPACE 1                                                                
DREC12   CLI   DDS,C'Y'            TEST DDS TERMINAL                            
         BNE   DREC14                                                           
         XOUT  AFDSEQ,PROASEQ,3                                                 
         OI    PROASEQH+FLDOIND-FLDHDRD,FOUTTRN                                 
         SPACE 1                                                                
DREC14   MVC   MYSEQNO,AFDSEQ      SAVE FOR CHANGE                              
         MVC   PROWRKC,AFDWC                                                    
         OI    PROWRKCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROWRKCH+5,L'PROWRKC                                             
         MVC   PROFLTC,AFDFILT                                                  
         OI    PROFLTCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROFLTCH+5,L'PROFLTC                                             
         MVI   PROLOCK,C' '                                                     
         TM    AFDSTAT,AFDSLOCK                                                 
         BZ    *+10                                                             
         MVC   PROLOCK,AC@YES                                                   
         OI    PROLOCKH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROLOCKH+5,L'PROLOCK                                             
         B     DREC10                                                           
         SPACE 1                                                                
         USING PRIELD,R3                                                        
*&&UK                                                                           
DREC16   TM    COMPSTA6,CPYSFBIL   FC ON BILLING?                               
         BNZ   DREC18                                                           
         TM    COMPSTAA,CPYSFCES   FC ON ESTIMATES?                             
         BZ    DREC20                                                           
         SPACE 1                                                                
DREC18   MVC   PROCURC,PRICURR                                                  
         OI    PROCURCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROCURCH+5,L'PROCURC                                             
*&&                                                                             
DREC20   XR    R5,R5               GET PRICE ENTRIES (LATEST FIRST, SEE         
         IC    R5,PRICNTR          SORT NOTE ON ELEMENT DSECT)                  
         LTR   R5,R5                                                            
         BZ    DREC10                                                           
         CHI   R5,8                8 ENTRIES ARE DISPLAYED MAX.                 
         BNH   *+8                                                              
         LA    R5,8                                                             
         LA    R6,PRINTRY                                                       
         USING PRINTRY,R6                                                       
         LA    R2,PRODAT1H                                                      
         SPACE 1                                                                
DREC22   DS    0H                                                               
         SPACE 1                                                                
* PROTECT THE FIELD WHERE A DISPLAY WILL BE SET|                                
         SPACE 1                                                                
         OI    FLDATB,FATBPROT                                                  
         GOTO1 DATCON,DMCB,(1,0(R6)),(13,8(R2))                                 
         AHI   R2,PROPRI1H-PRODAT1H                                             
         OI    FLDATB,FATBPROT                                                  
         CURED (P6,PRIAMT),(L'PROPRI1,8(R2)),2,ZERO=YES,ALIGN=LEFT              
         AHI   R2,PRODAT2H-PROPRI1H                                             
         AHI   R6,PRINTRQ                                                       
         BCT   R5,DREC22                                                        
         B     DREC10                                                           
         SPACE 1                                                                
         USING NAMELD,R3                                                        
DREC24   XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRODESC(0),NAMEREC                                               
         AHI   RE,1                                                             
         STC   RE,PRODESCH+5                                                    
         OI    PRODESCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         B     DREC10                                                           
*&&UK                                                                           
         USING XNMELD,R3                                                        
DREC26   XR    RE,RE                                                            
         IC    RE,XNMLN                                                         
         SHI   RE,XNMLN1Q+2                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PROLDSC(0),XNMSUBN                                               
         OI    PROLDSCH+FLDOIND-FLDHDRD,FOUTTRN                                 
         AHI   RE,1                                                             
         STC   RE,PROLDSCH+5                                                    
         MVC   PROLLNG(3),GERLANGS+1                                            
         OI    PROLLNGH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROLLNGH+5,3                                                     
         B     DREC10                                                           
*&&                                                                             
         USING SCMELD,R3                                                        
DREC28   XR    RE,RE                                                            
         IC    RE,SCMLN                                                         
         SHI   RE,SCMLN1Q+1                                                     
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),SCMNARR     ALL DATA HELD NOW IN TEMP                    
*&&UK*&& TM    SCMSEQ,SCMSEQLQ     LANGUAGE ENTRY?                              
*&&UK*&& BNZ   DREC30                                                           
         LA    R2,PROTEXTH         AND PASS IT TO TEXT FIELD                    
         OI    FLDOIND,FOUTTRN                                                  
         MVC   8(L'PROTEXT,R2),TEMP                                             
         MVI   5(R2),L'PROTEXT                                                  
         AHI   R2,L'PROTEXT+8+L'PROTEXTX                                        
         OI    FLDOIND,FOUTTRN                                                  
         MVC   8(L'PROTEXT,R2),TEMP+L'PROTEXT                                   
         MVI   5(R2),L'PROTEXT                                                  
         AHI   R2,L'PROTEXT+8+L'PROTEXTX                                        
         OI    FLDOIND,FOUTTRN                                                  
         MVC   8(L'PROTEXT,R2),TEMP+2*(L'PROTEXT)                               
         MVI   5(R2),L'PROTEXT                                                  
         B     DREC10                                                           
         SPACE 1                                                                
*&&UK                                                                           
DREC30   LA    R2,PROLTXTH         AND PASS IT TO LANGUAGE TEXT FIELD           
         OI    FLDOIND,FOUTTRN                                                  
         MVC   8(L'PROLTXT,R2),TEMP                                             
         MVI   5(R2),L'PROLTXT                                                  
         AHI   R2,L'PROLTXT+8+L'PROLTXTX                                        
         OI    FLDOIND,FOUTTRN                                                  
         MVC   8(L'PROLTXT,R2),TEMP+L'PROLTXT                                   
         MVI   5(R2),L'PROLTXT                                                  
         AHI   R2,L'PROLTXT+8+L'PROLTXTX                                        
         OI    FLDOIND,FOUTTRN                                                  
         MVC   8(L'PROLTXT,R2),TEMP+2*(L'PROLTXT)                               
         MVI   5(R2),L'PROLTXT                                                  
         SPACE 1                                                                
         MVC   PROLLNG(3),GERLANGS+1  FINALLY SET LANGUAGE                      
         OI    PROLLNGH+FLDOIND-FLDHDRD,FOUTTRN                                 
         MVI   PROLLNG+5,3                                                      
         B     DREC10                                                           
*&&                                                                             
DREC36   DS    0H                  ALL EXIT HERE                                
         CLI   ACTNUM,ACTNADD                                                   
         BE    DREC40                                                           
         CLI   ACTNUM,ACTNCHA                                                   
         BE    DREC50                                                           
         CLI   ACTNUM,ACTNDIS                                                   
         BE    DREC60                                                           
         CLI   ACTNUM,ACTNDEL                                                   
         BE    DREC70                                                           
         B     DRECX                                                            
         SPACE 1                                                                
DREC40   DS    0H                  ADD A NEW ITEMS RECORD                       
*&&UK*&& LA    R2,PROCURCH                                                      
*&&UK*&& NI    FLDATB,HEXFF-FATBPROT                                            
         LA    R2,PRODAT2H                                                      
         LA    R1,8                                                             
DREC42   OI    FLDATB,FATBPROT                                                  
         AHI   R2,PROPRI1H-PRODAT1H                                             
         OI    FLDATB,FATBPROT                                                  
         AHI   R2,PRODAT2H-PROPRI1H                                             
         BCT   R1,DREC42                                                        
         B     DRECX                                                            
         SPACE 1                                                                
DREC50   DS    0H                  CHANGE AN ITEMS RECORD                       
**       CLC   PROWRKC,MYSPACES      MOVED                                      
**UK     BNE   DREC52                                                           
**US     BNE   DRECX                                                            
**       LA    R2,PROWRKCH                                                      
**       OI    FLDATB,FATBPROT                                                  
*&&UK                                                                           
DREC52   LA    R2,PROCURCH         - CURRENCY CANNOT BE CHANGED                 
         OI    FLDATB,FATBPROT                                                  
*&&                                                                             
         B     DRECX                                                            
         SPACE 1                                                                
DREC60   DS    0H                  DISPLAY AN ITEMS RECORD                      
         B     DRECX                                                            
         SPACE 1                                                                
DREC70   DS    0H                  DELETE AN ITEMS RECORD                       
         CLC   ARTKSDAT,TODAY                                                   
         BH    DRECX               ONLY IF DATE IS HIGHER THEN TODAY            
         CLI   CUDTORYN,C'Y'                                                    
         BE    DRECX                                                            
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 1                                                                
DRECX    CR    RB,RB               ENSURE CC EQU                                
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ARTRECD,R5                                                       
VREC     NTR1                                                                   
         LA    R5,OREC1                                                         
         ST    R5,AIO              (NEEDED FOR MYADDEL/MYREMEL)                 
         MVC   CURSUPC,MYSPACES                                                 
         SPACE 1                                                                
         LA    R2,PRODESCH         VALIDATE NAME                                
         CLI   5(R2),0                                                          
         BNE   VREC002                                                          
         MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VREC002  MVI   ELCODE,NAMELQ                                                    
         BAS   RE,MYREMEL                                                       
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING NAMELD,R4                                                        
         MVI   NAMEL,NAMELQ                                                     
         XR    R3,R3               AND BUILD NAME ELEMENT                       
         IC    R3,5(R2)                                                         
         AHI   R3,NAMLN1Q                                                       
         STC   R3,NAMLN                                                         
         SHI   R3,NAMLN1Q+1                                                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),PRODESC                                               
         BAS   RE,MYADDEL                                                       
         SPACE 1                                                                
         MVI   ELCODE,AFDELQ       VALIDATE FILTER ELEMENT                      
         BAS   RE,MYREMEL                                                       
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT                                                  
         USING AFDELD,R4                                                        
         LA    R4,ELEMENT                                                       
         MVI   AFDEL,AFDELQ                                                     
         MVI   AFDLN,AFDLNQ                                                     
         MVC   AFDWC,MYSPACES                                                   
         MVC   CURWRKC,MYSPACES                                                 
         LA    R2,PROWRKCH         - WORK CODE                                  
         CLI   5(R2),0                                                          
         BE    VREC008                                                          
         CLI   PROWRKC,X'40'                                                    
         BNH   VREC008                                                          
         BAS   RE,GETWCD                                                        
         MVI   ERROR,INVALID       WORK CODE'S TYPE MUST MATCH KEY              
*&&UK                                                                           
         CLI   AGYCNTRY,CTRYGER    BUT FOR GERMANY ONLY                         
         BNE   VREC006                                                          
         CLC   ARTKSUP,MYSPACES                                                 
         BNE   VREC004                                                          
         CLI   CURWRKS,INTQ                                                     
         BNE   ERREND                                                           
         B     VREC006                                                          
         SPACE 1                                                                
VREC004  CLI   CURWRKS,EXTQ                                                     
         BNE   ERREND                                                           
*&&                                                                             
VREC006  MVC   AFDWC,CURWRKC                                                    
         SPACE 1                                                                
VREC008  MVC   AFDFILT,MYSPACES                                                 
         MVC   CURFILT,MYSPACES                                                 
         LA    R2,PROFLTCH         - FREE FORM FILTER                           
         CLI   5(R2),0                                                          
         BE    VREC016                                                          
         CLI   PROFLTC,X'40'                                                    
         BNH   VREC016                                                          
         OC    PROFLTC,MYSPACES                                                 
         LA    RE,MYCHARS                                                       
VREC010  CLI   0(RE),HEXFF          VALID FILTER CHARACTER?                     
         BE    VREC012                                                          
         CLC   0(1,RE),PROFLTC                                                  
         BE    VREC014                                                          
         LA    RE,1(RE)                                                         
         B     VREC010                                                          
         SPACE 1                                                                
VREC012  MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VREC014  MVC   AFDFILT,PROFLTC                                                  
         MVC   CURFILT,PROFLTC                                                  
         SPACE 1                                                                
VREC016  LA    R2,PROLOCKH         - LOCK STATUS                                
         MVI   CURSTAT,0                                                        
         MVI   CURSTA2,0                                                        
         CLI   5(R2),0                                                          
         BE    VREC018                                                          
         CLI   PROLOCK,X'40'                                                    
         BNH   VREC018                                                          
         OC    PROLOCK,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PROLOCK,AC@YES                                                   
         BNE   ERREND                                                           
         OI    AFDSTAT,AFDSLOCK                                                 
         OI    CURSTAT,AFDSLOCK                                                 
         SPACE 1                                                                
VREC018  CLC   ARTKSUP,MYSPACES    - INTERNAL/EXTERNAL (BY SUPPLIER)?           
         BNE   VREC019                                                          
         OI    AFDSTAT,AFDSINT                                                  
         SPACE 1                                                                
VREC019  LA    R2,PROPFLXH         - PRICE FLEXIBLE                             
         CLI   5(R2),0                                                          
         BE    VREC020                                                          
         CLI   PROPFLX,X'40'                                                    
         BNH   VREC020                                                          
         OC    PROPFLX,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PROPFLX,AC@YES                                                   
         BNE   ERREND                                                           
         OI    CURSTAT,ARTKFLXQ                                                 
         SPACE 1                                                                
VREC020  LA    R2,PRONOPRH         - NO PRICE                                   
         CLI   5(R2),0                                                          
         BE    VREC021                                                          
         CLI   PRONOPR,X'40'                                                    
         BNH   VREC021                                                          
         OC    PRONOPR,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PRONOPR,AC@YES                                                   
         BNE   ERREND                                                           
         TM    CURSTAT,ARTKFLXQ      CANNOT BE BOTH                             
         BNZ   ERREND                                                           
         OI    CURSTAT,ARTKNOPQ                                                 
         SPACE 1                                                                
VREC021  LA    R2,PRODSCOH         - DESCRIPTION OVERRIDE                       
         CLI   5(R2),0                                                          
*&&UK*&& BE    VREC022                                                          
*&&US*&& BE    VREC024                                                          
         CLI   PRODSCO,X'40'                                                    
*&&UK*&& BNH   VREC022                                                          
*&&US*&& BNH   VREC024                                                          
         OC    PRODSCO,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PRODSCO,AC@YES                                                   
         BNE   ERREND                                                           
         OI    CURSTAT,ARTKDAMQ                                                 
         SPACE 1                                                                
*&&UK                                                                           
VREC022  CLI   AGYCNTRY,CTRYGBR                                                 
         BNE   VREC023                                                          
         LA    R2,PRONICIH         - NIC'ABLE                                   
         CLI   5(R2),0                                                          
         BE    VREC023                                                          
         CLI   PRONICI,X'40'                                                    
         BNH   VREC023                                                          
         OC    PRONICI,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PRONICI,AC@YES                                                   
         BNE   ERREND                                                           
         OI    CURSTAT,ARTKNICQ                                                 
         SPACE 1                                                                
VREC023  LA    R2,PROISAIH         - ARTIST                                     
         CLI   5(R2),0                                                          
         BE    VREC024                                                          
         CLI   PROISAI,X'40'                                                    
         BNH   VREC024                                                          
         OC    PROISAI,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PROISAI,AC@YES                                                   
         BNE   ERREND                                                           
         OI    CURSTA2,ARTKSART                                                 
         SPACE 1                                                                
*&&                                                                             
VREC024  LA    R2,PROISTIH         - TIME                                       
         CLI   5(R2),0                                                          
         BE    VREC025                                                          
         CLI   PROISTI,X'40'                                                    
         BNH   VREC025                                                          
         OC    PROISTI,MYSPACES                                                 
         MVI   ERROR,INVALID                                                    
         CLC   PROISTI,AC@YES                                                   
         BNE   ERREND                                                           
         OI    CURSTA2,ARTKSTIM                                                 
         SPACE 1                                                                
VREC025  MVC   AFDSEQ,MYSEQNO      - PASS SEQUENCE NUMBER                       
         SPACE 1                                                                
         BAS   RE,MYADDEL                                                       
         SPACE 1                                                                
*&&UK                                                                           
         CLI   ACTNUM,ACTNADD                                                   
         BE    VREC026                                                          
         MVI   ELCODE,XNMELQ       VALIDATE LANGUAGE AND FOREIGN DESCR.         
         BAS   RE,MYREMEL                                                       
         SPACE 1                                                                
VREC026  MVC   CURLANG,MYSPACES    - LANGUAGE CODE                              
         LA    R2,PROLLNGH                                                      
         CLI   5(R2),0                                                          
         BE    VREC029                                                          
         LA    R1,ALLLANGS                                                      
         CLI   AGYCNTRY,CTRYGER                                                 
         BNE   *+8                                                              
         LA    R1,GERLANGS                                                      
         MVI   ERROR,INVALID                                                    
         XR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         SHI   RE,1                                                             
         CHI   RE,2                ENSURE THREE CHARS MAXIMUM                   
         BL    VREC027                                                          
         LHI   RE,2                                                             
         SPACE 1                                                                
VREC027  CLI   0(R1),HEXFF                                                      
         BE    ERREND                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R1),PROLLNG                                                  
         BE    VREC028                                                          
         AHI   R1,L'GERLANGS                                                    
         B     VREC027                                                          
         SPACE 1                                                                
VREC028  MVC   CURLANG,1(R1)                                                    
         MVC   CURLANQ,0(R1)                                                    
         SPACE 1                                                                
VREC029  DS    0H                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R2,PROLDSCH         - FOREIGN DESCRIPTION                        
         CLI   5(R2),0                                                          
         BE    VREC030                                                          
         MVI   ERROR,INVALID                                                    
         CLC   CURLANG,MYSPACES                                                 
         BE    ERREND                                                           
         SPACE 1                                                                
         USING XNMELD,R4                                                        
         LA    R4,ELEMENT                                                       
         MVI   XNMEL,XNMELQ                                                     
         MVC   XNMSTAT,CURLANQ                                                  
         XR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         AHI   R3,1                                                             
         STC   R3,XNMSUBL          LENGTH OF SUBELEMENT                         
         AHI   R3,XNMLN1Q          LENGTH OF ELEMENT                            
         STC   R3,XNMLN                                                         
         SHI   R3,XNMLN1Q+2        LENGTH OF NAME 2                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   XNMSUBN(0),PROLDSC                                               
         BAS   RE,MYADDEL                                                       
*&&                                                                             
VREC030  CLI   ACTNUM,ACTNADD                                                   
         BE    VREC032                                                          
         MVI   ELCODE,SCMELQ       VALIDATE LONG DESCRIPTION ELEMENT            
         BAS   RE,MYREMEL          IN BOTH LANGUAGES                            
         MVI   ELCODE,SCMELQ                                                    
         BAS   RE,MYREMEL                                                       
         SPACE 1                                                                
         USING SCMELD,R4                                                        
VREC032  XC    ELEMENT,ELEMENT     LONG DESCRIPTION (AGENCY)                    
         LA    R4,ELEMENT                                                       
         LA    R2,PROTEXTH                                                      
         CLI   5(R2),0             ENSURE FIRST LINE SET                        
         BNE   VREC034                                                          
         AHI   R2,L'PROTEXT+8+L'PROTEXTX                                        
         MVC   8(L'PROTEXT,R2),MYSPACES                                         
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,L'PROTEXT+8+L'PROTEXTX                                        
         MVC   8(L'PROTEXT,R2),MYSPACES                                         
         OI    FLDOIND,FOUTTRN                                                  
*&&UK*&& B     VREC042                                                          
*&&US*&& B     VREC054                                                          
VREC034  MVI   TEMP,C' '           THEN PASS ALL DATA INTO TEMP                 
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         MVC   TEMP(L'PROTEXT),8(R2)                                            
         AHI   R2,L'PROTEXT+8+L'PROTEXTX                                        
         MVC   TEMP+L'PROTEXT(L'PROTEXT),8(R2)                                  
         AHI   R2,L'PROTEXT+8+L'PROTEXTX                                        
         MVC   TEMP+2*(L'PROTEXT)(L'PROTEXT),8(R2)                              
         SPACE 1                                                                
         LA    RE,TEMP+L'TEMP-1    ENSURE NO BINARY ZEROES     P                
         LA    RF,L'TEMP-1                                                      
         SPACE 1                                                                
VREC036  CLI   0(RE),X'00'                                                      
         BNE   *+8                                                              
         MVI   0(RE),X'40'                                                      
         SHI   RE,1                                                             
         BCT   RF,VREC036                                                       
         SPACE 1                                                                
         LA    RE,TEMP+L'TEMP-1    DETERMINE LENGTH OF USED TEMP                
         LA    RF,L'TEMP                                                        
         SPACE 1                                                                
VREC038  CLI   0(RE),X'40'                                                      
         BH    VREC040                                                          
         SHI   RE,1                                                             
         BCT   RF,VREC038                                                       
*&&UK*&& B     VREC042             NO INPUT - STARNGE, BUT SKIP                 
*&&US*&& B     VREC054             NO INPUT - STARNGE, BUT SKIP                 
         SPACE 1                                                                
VREC040  SHI   RF,1                                                             
         MVI   SCMEL,SCMELQ        BUILD ELEMENT NOW                            
         LR    RE,RF                                                            
         AHI   RE,SCMLN1Q+1                                                     
         STC   RE,SCMLN                                                         
         MVI   SCMSEQ,0                                                         
         MVI   SCMTYPE,SCMTSTND                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCMNARR(0),TEMP                                                  
         BAS   RE,MYADDEL                                                       
*&&UK                                                                           
VREC042  XC    ELEMENT,ELEMENT     LONG DESCRIPTION (LANGUAGE)                  
         LA    R4,ELEMENT                                                       
         LA    R2,PROLTXTH                                                      
         CLI   5(R2),0             ENSURE FIRST LINE SET                        
         BNE   VREC044                                                          
         AHI   R2,L'PROLTXT+8+L'PROLTXTX                                        
         MVC   8(L'PROLTXT,R2),MYSPACES                                         
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,L'PROLTXT+8+L'PROLTXTX                                        
         MVC   8(L'PROLTXT,R2),MYSPACES                                         
         OI    FLDOIND,FOUTTRN                                                  
         B     VREC054                                                          
         SPACE 1                                                                
VREC044  CLC   CURLANG,MYSPACES    ENSURE LANGUAGE SET                          
         BNE   VREC046                                                          
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VREC046  MVI   TEMP,C' '           THEN PASS ALL DATA INTO TEMP                 
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         MVC   TEMP(L'PROTEXT),8(R2)                                            
         AHI   R2,L'PROLTXT+8+L'PROLTXTX                                        
         MVC   TEMP+L'PROLTXT(L'PROLTXT),8(R2)                                  
         AHI   R2,L'PROLTXT+8+L'PROLTXTX                                        
         MVC   TEMP+2*(L'PROLTXT)(L'PROLTXT),8(R2)                              
         SPACE 1                                                                
         LA    RE,TEMP+L'TEMP-1    ENSURE NO BINARY ZEROES     P                
         LA    RF,L'TEMP-1                                                      
         SPACE 1                                                                
VREC048  CLI   0(RE),X'00'                                                      
         BNE   *+8                                                              
         MVI   0(RE),X'40'                                                      
         SHI   RE,1                                                             
         BCT   RF,VREC048                                                       
         SPACE 1                                                                
         LA    RE,TEMP+L'TEMP-1    DETERMINE LENGTH OF USED TEMP                
         LA    RF,L'TEMP                                                        
VREC050  CLI   0(RE),X'40'                                                      
         BH    VREC052                                                          
         SHI   RE,1                                                             
         BCT   RF,VREC050                                                       
         B     VREC054             NO INPUT - STARNGE, BUT SKIP                 
         SPACE 1                                                                
VREC052  SHI   RF,1                                                             
         MVI   SCMEL,SCMELQ        BUILD ELEMENT NOW                            
         LR    RE,RF                                                            
         AHI   RE,SCMLN1Q+1                                                     
         STC   RE,SCMLN                                                         
         MVI   SCMSEQ,SCMSEQLQ                                                  
         MVI   SCMTYPE,SCMTSTND                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCMNARR(0),TEMP                                                  
         BAS   RE,MYADDEL                                                       
*&&                                                                             
VREC054  DS    0H                  NOW DEAL WITH DATE/PRICE TABLE               
         SPACE 3                                                                
* NOTE: ON DISPLAY THE FIRST EIGHT PRICE ENTRIES ARE DISPLAYED (AND             
*       THOSE ARE SORTED WITHIN THE ITEMS RECORD). THE 9TH ENTRY IS             
*       SPARED FOR ANY CHANGE ACTION ON THE PRICES. THIS MEANS THAT             
*       (BY LOOKING AT PRICNTR) ONLY ONE SPECIFIC FIELD IS ALLOWED FOR          
*       INPUT, ALL OTHERS ARE NOT VALID.                                        
*       INPUT CAN BE DELETED FOR THE LASTEST DATE/PRICE ENTRY BY TYPING         
*       IN DATE/'DELETE'                                                        
*       MCS ADDITIONS: IF NO PRICE THEN CLEAR TABLE AND SET DUMMY PRICE         
         SPACE 3                                                                
         LA    R2,PROPRI1H         LOOK FOR VALID INPUT FIELD                   
         TM    ADDIND,ADD80                                                     
         BZ    VREC058                                                          
         MVC   PROPRI1,MYSPACES                                                 
         MVC   PRODAT1,MYSPACES                                                 
         MVI   PROPRI1H+5,0                                                     
         MVI   PRODAT1H+5,0                                                     
         NI    ADDIND,HEXFF-ADD80                                               
         SPACE 1                                                                
VREC058  TM    ADDIND,ADD08        DUPLICATE ADD ACTION?                        
         BZ    VREC060                                                          
         NI    ADDIND,HEXFF-ADD08  RESET                                        
         B     VREC062                                                          
         SPACE 1                                                                
VREC060  TM    FLDATB,FATBPROT                                                  
         BZ    VREC062                                                          
         AHI   R2,PROPRI2H-PROPRI1H                                             
         B     VREC060                                                          
         SPACE 1                                                                
VREC062  XC    PDHIDAT,PDHIDAT     SET HIGHEST EFFDATE/PRICE DATE               
         XC    PDNUM,PDNUM                                                      
         MVC   PDCURR,MYSPACES                                                  
         XC    ELEMENT,ELEMENT                                                  
         SPACE 1                                                                
         MVC   CUDATE,ARTKSDAT-ARTKSTA+ARTRSTA     ENSURE DEFAULT SET           
         SPACE 1                                                                
         CLI   ACTNUM,ACTNRES      DO NOT CHANGE ELEMENT ON RESTORE             
         BE    VREC118                                                          
         SPACE 1                                                                
         USING PRIELD,RE                                                        
         L     RE,AIO              GET EXISTING PRICE ELEMENT                   
         AHI   RE,ARTRFST-ARTRECD                                               
         XR    R0,R0                                                            
VREC064  CLI   PRIEL,PRIELQ                                                     
         BE    VREC066                                                          
         CLI   PRIEL,0                                                          
         BE    VREC068                                                          
         IC    R0,PRILN                                                         
         AR    RE,R0                                                            
         B     VREC064                                                          
         SPACE 1                                                                
VREC066  MVC   PDHIDAT,PRIDAT      SAVE DATE, COUNTER AND ELEMENT               
         MVC   PDNUM,PRICNTR                                                    
         MVC   CUCNTR,PRICNTR                                                   
         MVC   PDCURR,PRICURR                                                   
         XR    R1,R1                                                            
         IC    R1,PRILN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),PRIELD                                                
         DROP  RE                                                               
         SPACE 1                                                                
         MVI   ELCODE,PRIELQ       AND REMOVE OLD ELEMENT                       
         BAS   RE,MYREMEL                                                       
         SPACE 1                                                                
VREC068  SHI   R2,PROPRI1H-PRODAT1H   ANY ENTRIES YET?                          
         CLI   PDNUM,0                                                          
         BNE   VREC070                                                          
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VREC072                                                          
         MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VREC070  CLI   5(R2),0             ANYTHING INPUT HERE?                         
         BNE   VREC074                                                          
         SPACE 1                                                                
VREC072  AHI   R2,PROPRI1H-PRODAT1H                                             
         CLI   5(R2),0                                                          
         BE    VREC108             THEN SKIP HERE                               
         SHI   R2,PROPRI1H-PRODAT1H   VALIDATE (NEW) ENTRY FIRST                
         SPACE 1                                                                
VREC074  MVI   DELIND,0            SET TO NO 'DELETE PRICE' ACTION              
         LA    RE,PROPRI1H-PRODAT1H(R2)    POINT TO PRICE                       
         XR    RF,RF                                                            
         IC    RF,5(RE)                                                         
         SHI   RF,1                                                             
         EX    RF,VREC076                                                       
         B     VREC078                                                          
VREC076  CLC   8(0,RE),AC@DEL      DELETE ACTION?                               
VREC078  BNE   VREC080                                                          
         OI    DELIND,DELETEQ      THEN SET IT                                  
         B     VREC092                                                          
         SPACE 1                                                                
VREC080  EX    RF,VREC082                                                       
         B     VREC084                                                          
VREC082  CLC   8(0,RE),AC@CHG      CHANGE ON DATE ACTIOON?                      
VREC084  BNE   VREC086                                                          
         OI    DELIND,CHGDATQ      THEN SET IT                                  
         MVI   ERROR,INVALID       BUT ONLY IF ONE PRICE EXISTS AND             
         CLI   PDNUM,1             IT LAYS IN THE FUTURE                        
         BNE   ERREND                                                           
         B     VREC092                                                          
         SPACE 1                                                                
VREC086  XR    RF,RF               CHANGE ONE PRICE ONLY?                       
         IC    RF,5(R2)                                                         
         SHI   RF,1                                                             
         EX    RF,VREC088                                                       
         B     VREC090                                                          
VREC088  CLC   8(0,R2),AC@CHG      CHANGE ACTION?                               
VREC090  BNE   VREC092                                                          
         MVI   ERROR,INVALID       BUT ONLY IF ONE PRICE EXISTS AND             
         CLI   PDNUM,1             IT LAYS IN THE FUTURE                        
         BNE   ERREND                                                           
         CLI   CUDTORYN,C'Y'                                                    
         BE    *+14                                                             
         CLC   PDHIDAT,TODAY                                                    
         BNH   ERREND                                                           
         OI    DELIND,CHGAMTQ      THEN SET IT                                  
         MVC   CUDATE,PDHIDAT                                                   
         B     VREC096                                                          
         SPACE 1                                                                
VREC092  MVI   ERROR,INVDATE                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   DATELEN,5(R2)       DATE LENGTH                                  
         MVC   DATEFORM,AGYLANG    AGENCY LANGUAGE                              
         GOTO1 PERVAL,DMCB,(DATELEN,8(R2)),(DATEFORM,PERVBLK)                   
         CLI   4(R1),PVRCONE       ONE VALID DATE                               
         BNE   ERREND                                                           
         LA    R1,PERVBLK                                                       
         MVC   CUDATE,PVALPSTA-PERVALD(R1)                                      
         SPACE 1                                                                
         MVI   ERROR,INVDAY        NEW MUST BE HIGHER THAN PREVIOUS             
         TM    DELIND,DELETEQ                                                   
         BZ    VREC094                                                          
         CLC   PDHIDAT,CUDATE      DELETE ACTION THAN DATE MUST MATCH           
         BNE   ERREND                                                           
         CLI   PDNUM,1             MUST BE MORE THAN 2                          
         BNH   ERREND              2                                            
         CLI   CUDTORYN,C'Y'                                                    
         BE    *+14                                                             
         CLC   CUDATE,TODAY        AND IN THE FUTURE                            
         BNH   ERREND                                                           
         SPACE 1                                                                
         USING PRIELD,R3                                                        
         MVC   MYELEM,ELEMENT      OK THEN DELETE LATEST ENTRY                  
         LA    R3,ELEMENT                                                       
         IC    RE,PRILN                                                         
         LR    RF,RE                                                            
         SHI   RF,PRINTRQ                                                       
         SHI   RE,PRINTRQ+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRINTRY(0),MYELEM+PRILN1Q                                        
         IC    RE,PRICNTR                                                       
         SHI   RE,1                                                             
         STC   RE,PRICNTR                                                       
         STC   RF,PRILN                                                         
         MVC   CUCNTR,PRICNTR      AND SET UPDATED CURRENT VALUES               
         MVC   CUDATE,PRIDAT                                                    
         B     VREC108             AND ADD ELEMENT                              
         DROP  R3                                                               
         SPACE 1                                                                
VREC094  TM    DELIND,CHGDATQ                                                   
         BNZ   VREC095                                                          
         CLC   PDHIDAT,CUDATE                                                   
         BNL   ERREND                                                           
         SPACE 1                                                                
VREC095  MVI   ERROR,DATNOFUT      NEW MUST BE IN THE FUTURE (DATNOTGT)         
         CLI   CUDTORYN,C'Y'                                                    
         BE    VREC096                                                          
         CLC   TODAY,CUDATE                                                     
         BNL   ERREND                                                           
         SPACE 1                                                                
VREC096  TM    DELIND,CHGDATQ                                                   
         BNZ   VREC102                                                          
         AHI   R2,PROPRI1H-PRODAT1H                                             
         XR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   DMCB,HEXFF                                                       
         BNE   VREC100                                                          
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VREC100  L     R3,DMCB+4                                                        
         LTR   R3,R3               PRICE CANNOT BE NEGATIVE                     
         JNM   VREC101                                                          
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
VREC101  CVD   R3,DUB                                                           
         SPACE 1                                                                
         USING PRIELD,R3                                                        
VREC102  LA    R3,ELEMENT          NOW BUILD ELEMENT                            
         TM    DELIND,CHGAMTQ      CHANGE ACTION?                               
         BZ    VREC104                                                          
         ZAP   PRIAMT,DUB          REPLACE PRICE ONLY                           
         BO    ERREND                                                           
         TM    CURSTAT,ARTKNOPQ    FOR 'NO PRICE' AMOUNT MUST BE ZERO           
         BZ    VREC102A                                                         
         ZAP   PRIAMT,DUB                                                       
         BZ    VREC102B                                                         
         B     ERREND                                                           
VREC102A ZAP   PRIAMT,DUB          ELSE IT CANNOT BE ZERO                       
         BZ    ERREND                                                           
VREC102B MVI   CUCNTR,1                                                         
         B     VREC108                                                          
         SPACE 1                                                                
VREC104  TM    DELIND,CHGDATQ      CHANGE ACTION?                               
         BZ    VREC106                                                          
         MVC   PRIDAT,CUDATE       REPLACE DATE ONLY                            
         MVI   CUCNTR,1                                                         
         B     VREC108                                                          
         SPACE 1                                                                
VREC106  CLI   PRIEL,PRIELQ        NEW ELEMENT?                                 
         BE    VREC110                                                          
         MVI   PRIEL,PRIELQ                                                     
         MVI   PRILN,PRILN1Q                                                    
*&&UK*&& BAS   RE,VALCUR           VALIDATE CURRENCY                            
*&&UK*&& MVC   PRICURR,CUCURR                                                   
*&&US*&& MVC   PRICURR,=C'USD'                                                  
         MVI   PRISTAT,0                                                        
         MVI   PRICNTR,1                                                        
         MVI   CUCNTR,1                                                         
         MVC   PRIDAT,CUDATE                                                    
         MVI   ERROR,INVALID                                                    
         ZAP   PRIAMT,DUB                                                       
         BO    ERREND                                                           
         TM    CURSTAT,ARTKNOPQ    FOR 'NO PRICE' AMOUNT MUST BE ZERO           
         BZ    VREC106A                                                         
         ZAP   PRIAMT,DUB                                                       
         BZ    VREC106B                                                         
         B     ERREND                                                           
VREC106A ZAP   PRIAMT,DUB          ELSE IT CANNOT BE ZERO                       
         BZ    ERREND                                                           
VREC106B MVI   PRIEL+PRILN1Q,0                                                  
         SPACE 1                                                                
VREC108  MVI   ERROR,MISSING                                                    
         CLI   ELEMENT,PRIELQ      ENSURE WE ADD A PRICE                        
         BNE   ERREND                                                           
         SPACE 1                                                                
         BAS   RE,MYADDEL                                                       
         B     VREC116                                                          
         SPACE 1                                                                
VREC110  DS    0H                                                               
*&&UK*&& FOUT  PROCURC,PDCURR,3    REPLACE BY EXISTING                          
         XR    R0,R0                                                            
         CLI   PDNUM,PRICMAXQ      AND REFRESH ELEMENT                          
         BE    VREC112             MAXIMUM REACHED?                             
         IC    R0,PDNUM                                                         
         AHI   R0,1                                                             
         SPACE 1                                                                
VREC112  STC   R0,PRICNTR                                                       
         MVC   CUCNTR,PRICNTR                                                   
         IC    R0,PRILN                                                         
         LR    R1,R0                                                            
         SHI   R1,PRINTRY-PRIELD+1                                              
         SHI   R1,PRINTRQ                                                       
         CLI   PDNUM,PRICMAXQ      AND REFRESH ELEMENT                          
         BE    VREC114             MAXIMUM REACHED?                             
         AHI   R1,PRINTRQ                                                       
         AHI   R0,PRINTRQ                                                       
         STC   R0,PRILN                                                         
         SPACE 1                                                                
VREC114  MVC   MYELEM,ELEMENT                                                   
         MVC   PRIDAT,CUDATE                                                    
         MVI   ERROR,INVALID                                                    
         ZAP   PRIAMT,DUB                                                       
         BO    ERREND                                                           
         TM    CURSTAT,ARTKNOPQ    FOR 'NO PRICE' AMOUNT MUST BE ZERO           
         BZ    VREC114A                                                         
         ZAP   PRIAMT,DUB                                                       
         BZ    VREC114B                                                         
         B     ERREND                                                           
VREC114A ZAP   PRIAMT,DUB          ELSE IT CANNOT BE ZERO                       
         BZ    ERREND                                                           
VREC114B EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRINTRY+PRINTRQ(0),MYELEM+PRINTRY-PRIELD                         
         BAS   RE,MYADDEL                                                       
         SPACE 1                                                                
VREC116  DS    0H                                                               
         CLI   CUCNTR,0            ENSURE ONE PRICE SET AT LEAST                
         BNE   VREC118                                                          
         MVI   ERROR,MISSING                                                    
         LA    R2,PROPRI1H                                                      
         B     ERREND                                                           
         SPACE 1                                                                
VREC118  LA    R2,PRODAT1H                                                      
         XR    RE,RE                                                            
         IC    RE,CUCNTR           PROTECT ALL VALIDATED INPUT FIELDS           
         LTR   RE,RE                                                            
         BZ    VREC130                                                          
         CHI   RE,9                                                             
         BL    *+8                                                              
         LHI   RE,8                                                             
         SPACE 1                                                                
VREC120  OI    FLDATB,FATBPROT                                                  
         AHI   R2,PROPRI1H-PRODAT1H                                             
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBPROT                                                  
         AHI   R2,PRODAT2H-PROPRI1H                                             
         OI    FLDOIND,FOUTTRN                                                  
         BCT   RE,VREC120                                                       
         SPACE 1                                                                
VREC130  BAS   RE,MYPERIN                                                       
         SPACE 1                                                                
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM                                                 
         MVC   RAPCPY,CUL                                                       
         MVI   RAPRTYP,RAPKRART                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOMFACS                                                 
         LA    RF,OREC1                                                         
         ST    RF,RAPAREC                                                       
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R3,ARTRFST          SCAN RECORD FOR LOWEST PRIDAT                
         USING PRIELD,R3                                                        
         XR    R0,R0                                                            
VREC132  CLI   PRIEL,PRIELQ                                                     
         BE    VREC136                                                          
         CLI   PRIEL,0                                                          
         BNE   VREC134                                                          
         DC    H'0'                                                             
VREC134  IC    R0,PRILN                                                         
         AR    R3,R0                                                            
         B     VREC132                                                          
         SPACE 1                                                                
VREC136  XR    R1,R1                                                            
         IC    R1,PRICNTR                                                       
         SHI   R1,1                                                             
         MHI   R1,PRINTRQ                                                       
         LA    R1,PRINTRY(R1)                                                   
         USING PRINTRY,R1                                                       
         MVC   CUSDAT,PRIDAT                                                    
         DROP  R1,R3                                                            
         SPACE 1                                                                
         USING ARTKSTA,R3                                                       
         LA    R3,MYSTAT           PREPARE NEW STATUS FOR RECORD                
         MVC   ARTKSTAT,CURSTAT    LOCKED STATUS                                
         CLC   ARTKSUP,MYSPACES    INT/EXT STATUS                               
         BNE   *+8                                                              
         OI    ARTKSTAT,ARTKTINT                                                
         MVC   ARTKSWC,CURWRKC     W/C                                          
         MVC   ARTKSFLT,CURFILT    FILTER                                       
         MVC   ARTKSDAT,CUSDAT                                                  
         MVC   ARTKSTA2,CURSTA2                                                 
         DROP  R3                                                               
         SPACE 1                                                                
         MVC   ARTRSTA,MYSTAT      AND SET STATUS IN RECORD                     
         SPACE 1                                                                
VRECX    MVC   AIO,AIO1            RESET OREC1 SETTING                          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO FILE (OREC1 WITH RECORD), SEE LPAS ROUTINE            *         
***********************************************************************         
         SPACE 1                                                                
AREC     NTR1                                                                   
         MVC   OKEY1,OREC1         AND COPY KEY                                 
         GOTO1 DATAMGR,DMCB,ADDREX,ACCMST,TEMP,OREC1,OWRK1                      
         BE    AREC02                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
AREC02   MVC   MYDA,TEMP           SAVE D/A FOR PASSIVES                        
         SPACE 1                                                                
ARECX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELET A RECORD AND ALL PASSIVES (KEY IN OKEY1)                      *         
***********************************************************************         
         SPACE 1                                                                
XREC     NTR1                                                                   
         MVC   OKEY2,OKEY1                                                      
         MVI   ERROR,0                                                          
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),ACCDIR,OKEY2,OKEY2,0                 
         BE    XREC02                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC02   CLC   OKEY2,OKEY1         SAME RECORD?                                 
         BE    XREC04                                                           
         MVI   ERROR,NOTFOUND                                                   
         B     XRECX                                                            
         SPACE 1                                                                
         USING ARTRECD,R4                                                       
XREC04   LA    R4,OKEY2                                                         
         MVC   MYDA,ARTKDA                                                      
         OI    ARTKSTAT,ARTKDELT   SET TO DELETED                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OKEY2,OKEY2,TEMP                       
         BE    XREC06                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC06   GOTO1 DATAMGR,DMCB,(X'80',GETREX),ACCMST,MYDA,OREC1,OWRK1              
         BE    XREC08                                                           
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
         USING AFDELD,R3                                                        
XREC08   LA    R4,OREC1            SCAN FOR SEQUENCE NUMBER                     
         LA    R3,ARTRFST                                                       
         XR    R0,R0                                                            
XREC10   CLI   AFDEL,AFDELQ                                                     
         BE    XREC12                                                           
         CLI   AFDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,AFDLN                                                         
         AR    R3,R0                                                            
         B     XREC10                                                           
         SPACE 1                                                                
XREC12   MVC   MYSEQNO,AFDSEQ      SAVE SEQUENCE NUMBER                         
         OI    ARTRSTA,ARTKDELT    SET TO DELETED                               
         GOTO1 DATAMGR,DMCB,PUTREX,ACCMST,MYDA,OREC1,OWRK1                      
         BE    XREC14                                                           
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
         USING PAFRECD,R3                                                       
XREC14   LA    R3,OKEY2                                                         
         XC    PAFKEY,PAFKEY                                                    
         MVI   PAFKTYP,PAFKTYPQ                                                 
         MVI   PAFKSUB,PAFKSQ                                                   
         MVC   PAFKCPY,ARTKCPY                                                  
         MVC   PAFKART,ARTKART                                                  
         MVC   PAFKSUP,ARTKSUP                                                  
         MVC   PAFKCLI,ARTKCLI                                                  
         MVC   PAFKOFF,ARTKOFF                                                  
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,OKEY2,OKEY2,0                 
         BE    XREC16                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC16   LA    R3,OKEY2                                                         
         CLC   PAFKDA,MYDA                                                      
         BE    XREC18                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC18   OI    PAFKSTAT,ARTKDELT   SET TO DELETED                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OKEY2,OKEY2,TEMP                       
         BE    XREC20                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
         USING PASRECD,R3                                                       
XREC20   LA    R3,OKEY2                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,ARTKCPY                                                  
         MVC   PASKSEQ,MYSEQNO                                                  
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,OKEY2,OKEY2,0                 
         BE    XREC22                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC22   LA    R3,OKEY2                                                         
         CLC   PASKDA,MYDA                                                      
         BE    XREC24                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC24   OI    PASKSTAT,ARTKDELT   SET TO DELETED                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OKEY2,OKEY2,TEMP                       
         BE    XREC26                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
XREC26   DS    0H                  ALL DELETED NOW                              
         SPACE 1                                                                
XRECX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SEARCH RECORD ALREADY EXISTS                                        *         
***********************************************************************         
         SPACE 1                                                                
SREC     NTR1                                                                   
         MVC   OKEY2,OKEY1                                                      
         MVI   ERROR,0                                                          
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),ACCDIR,OKEY2,OKEY2,0                 
         TM    8(R1),HEXFF-X'02'   ALLOW DELETES BUT NO OTHER ERRORS            
         BZ    SREC02                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
SREC02   CLC   OKEY2,OKEY1         SAME RECORD?                                 
         BNE   SREC04                                                           
         MVI   ERROR,RECEXIST                                                   
         TM    8(R1),X'02'         IS EXISTING RECORD DELETED                   
         BZ    SRECERR                                                          
         MVI   ERROR,DELEXIST                                                   
         B     SRECERR                                                          
         SPACE 1                                                                
SREC04   XC    SAVEDATV,SAVEDATV   SAVE VALUES IF INPUT                         
         XC    SAVEPRIV,SAVEPRIV                                                
         XC    SAVEDATL,SAVEDATL                                                
         XC    SAVEPRIL,SAVEPRIL                                                
         LA    R2,PROPRI1H                                                      
         TM    FLDIIND,FINPTHIS                                                 
         BNZ   SREC06                                                           
         LA    R2,PRODAT1H                                                      
         TM    FLDIIND,FINPTHIS                                                 
         BZ    SREC08                                                           
         SPACE 1                                                                
SREC06   MVC   SAVEDATV,PRODAT1                                                 
         MVC   SAVEPRIV,PROPRI1                                                 
         MVC   SAVEDATL,PRODAT1H+5                                              
         MVC   SAVEPRIL,PROPRI1H+5                                              
         SPACE 1                                                                
SREC08   LA    R1,9                ON ADD ENABLE ONLY FIRST FIELD1              
         LA    R2,PRODAT1H                                                      
         SPACE 1                                                                
SREC10   OI    FLDATB,FATBPROT                                                  
         MVI   FLDILEN,0                                                        
         XC    8(L'PRODAT1,R2),8(R2)                                            
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,PROPRI1H-PRODAT1H                                             
         OI    FLDATB,FATBPROT                                                  
         MVI   FLDILEN,0                                                        
         XC    8(L'PROPRI1,R2),8(R2)                                            
         OI    FLDOIND,FOUTTRN                                                  
         AHI   R2,PRODAT2H-PROPRI1H                                             
         BCT   R1,SREC10                                                        
         LA    R2,PRODAT1H                                                      
         MVC   PRODAT1,SAVEDATV                                                 
         MVC   FLDILEN,SAVEDATL                                                 
         NI    FLDATB,HEXFF-FATBPROT                                            
         LA    R2,PROPRI1H                                                      
         MVC   PROPRI1,SAVEPRIV                                                 
         MVC   FLDILEN,SAVEPRIL                                                 
         NI    FLDATB,HEXFF-FATBPROT                                            
         B     SRECX                                                            
         SPACE 1                                                                
SRECERR  LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
SRECX    CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LOCK AND READ PASSIVE + BUILD DUMMY RECORD FOR ADD/VREC             *         
***********************************************************************         
         SPACE 1                                                                
         USING PASRECD,R4                                                       
LPAS     NTR1                                                                   
         LA    R4,OKEY2            BUILD PASSIVE KEY                            
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUL                                                      
         XC    PASKSEQ,PASKSEQ     READ HIGH FOR PASSIVE                        
         MVC   TEMP(3),OKEY2                                                    
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),ACCDIR,OKEY2,OKEY2,0                 
         TM    8(R1),HEXFF-X'02'   ALLOW DELETES BUT NO OTHER ERRORS            
         BZ    LPAS02                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
LPAS02   CLC   TEMP(3),OKEY2       SAME RECORD TYPE?                            
         BE    LPAS04                                                           
         MVC   MYSEQNO,=X'FFFFFF'  SET TO FIRST                                 
         B     LPAS08                                                           
         SPACE 1                                                                
LPAS04   ICM   R1,7,PASKSEQ                                                     
         LTR   R1,R1               ENSURE SPACE FOR ANOTHER ONE                 
         BNZ   LPAS06                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
LPAS06   SHI   R1,1                                                             
         STCM  R1,7,MYSEQNO        AND SET TO NEXT ONE                          
         SPACE 1                                                                
LPAS08   DS    0H                  MYSEQNO: NEXT SEQ. NO. + REC. LOCKED         
         LA    R4,OREC1            NOW BUILD DUMMY RECORD FOR ADD/VREC          
         USING ARTRECD,R4                                                       
         MVC   ARTKEY(64),OKEY1    PASS KEY                                     
         LA    R1,ARTRFST-ARTRECD  SET LENGTH                                   
         AHI   R1,NAMLN1Q                                                       
         STH   R1,ARTRLEN                                                       
         LA    R4,ARTRFST                                                       
         USING NAMELD,R4                                                        
         MVI   NAMEL,NAMELQ        AND ADD DUMMY NAME                           
         MVI   NAMLN,NAMLN1Q+1                                                  
         MVI   NAMEREC,C'*'                                                     
         SPACE 1                                                                
LPASX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SEARCH FOR PASSIVES STILL EXISTING ON RESTORE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING PASRECD,R4                                                       
SPAS     NTR1                                                                   
         LA    R4,OKEY2            BUILD PASSIVE KEY                            
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUL                                                      
         MVC   PASKSEQ,MYSEQNO     READ HIGH FOR PASSIVE                        
         MVC   TEMP(42),OKEY2                                                   
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,OKEY2,OKEY2,0                 
         TM    8(R1),HEXFF-X'02'   ALLOW DELETES BUT NO OTHER ERRORS            
         BZ    SPAS02                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
SPAS02   CLC   TEMP(42),OKEY2      SAME RECORD?                                 
         BE    SPAS04                                                           
         MVI   ERROR,NOTFOUND      SET TO PASSIVE NOT EXISTING                  
         B     SPASERR                                                          
         SPACE 1                                                                
SPAS04   CLC   MYDA,PASKDA         ENSURE SAME DISK/ADDRESS                     
         BE    SPAS06                                                           
         MVI   ERROR,RECEXIST                                                   
         B     SPASERR                                                          
         SPACE 1                                                                
SPAS06   TM    PASKSTAT,ARTKDELT   ENSURE DELETED                               
         BZ    SPAS08                                                           
         MVI   ERROR,RECEXIST                                                   
         B     SPASERR                                                          
         SPACE 1                                                                
         USING PAFRECD,R4                                                       
         USING ARTRECD,R3                                                       
SPAS08   LA    R4,OKEY2            BUILD PASSIVE KEY                            
         L     R3,AIO                                                           
         XC    PAFKEY,PAFKEY                                                    
         MVI   PAFKTYP,PAFKTYPQ                                                 
         MVI   PAFKSUB,PAFKSQ                                                   
         MVC   PAFKCPY,ARTKCPY                                                  
         MVC   PAFKART,ARTKART                                                  
         MVC   PAFKSUP,ARTKSUP                                                  
         MVC   PAFKCLI,ARTKCLI                                                  
         MVC   PAFKOFF,ARTKOFF                                                  
         MVC   TEMP(42),OKEY2                                                   
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,OKEY2,OKEY2,0                 
         TM    8(R1),HEXFF-X'02'   ALLOW DELETES BUT NO OTHER ERRORS            
         BZ    SPAS10                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
SPAS10   CLC   TEMP(42),OKEY2      SAME RECORD?                                 
         BE    SPAS12                                                           
         MVI   ERROR,NOTFOUND      SET TO PASSIVE NOT EXISTING                  
         B     SPASERR                                                          
         SPACE 1                                                                
SPAS12   CLC   MYDA,PAFKDA         ESNURE SAME DISK/ADDRESS                     
         BE    SPAS14                                                           
         MVI   ERROR,RECEXIST                                                   
         B     SPASERR                                                          
         SPACE 1                                                                
SPAS14   TM    PAFKSTAT,ARTKDELT   ENSURE DELETED                               
         BZ    SPASX                                                            
         MVI   ERROR,RECEXIST                                                   
         B     SPASERR                                                          
         SPACE 1                                                                
SPASERR  LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPASX    CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET RECORD TO FILE (OKEY1/OREC1 WITH RECORD, READ UPDATIVE)         *         
***********************************************************************         
         SPACE 1                                                                
GREC     NTR1                                                                   
         LA    R4,OKEY1                                                         
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,(R4),(R4),0                   
         TM    8(R1),HEXFF-X'02'   ALLOW DELETES BUT NO OTHER ERRORS            
         BNZ   GRECERR                                                          
         CLI   ACTNUM,ACTNRES      RESTORE?                                     
         BE    GREC02                                                           
         CLI   8(R1),0                                                          
         BNE   GRECERR                                                          
         B     GREC04                                                           
         SPACE 1                                                                
GREC02   CLI   8(R1),0                                                          
         BE    GRECERR             ONLY IF DELETED                              
         SPACE 1                                                                
GREC04   MVC   MYDA,ARTKDA-ARTRECD(R4)        READ FULL RECORD                  
         GOTO1 DATAMGR,DMCB,(X'80',GETREX),ACCMST,MYDA,OREC1,OWRK1              
         BE    GRECX               OK AND EXIT                                  
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
GRECERR  MVI   ERROR,NOTFOUND                                                   
         LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
GRECX    CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD PASSIVES TO FILE (ACTRECD OKEY1/OREC1 WITH RECORD)              *         
***********************************************************************         
         SPACE 1                                                                
         USING PASRECD,R4                                                       
         USING ARTRECD,R3                                                       
APAS     NTR1                                                                   
         LA    R4,OKEY2            AND BUILD PASSIVE KEY                        
         LA    R3,OREC1            (ITEMS RECORD)                               
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUL                                                      
         MVC   PASKSEQ,MYSEQNO     PASS NEW SEQUENCE NUMBER,                    
         MVC   PASKSTA,ARTRSTA     STATUS                                       
         MVC   PASKDA,MYDA         AND DISK ADDRESS                             
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,PASKEY,PASKEY,OWRK2                    
         BE    APAS02                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
         USING PAFRECD,R4                                                       
APAS02   LA    R4,OKEY2            ADD PAFRECD PASSIVE NOW                      
         XC    PAFKEY,PAFKEY                                                    
         MVI   PAFKTYP,PAFKTYPQ                                                 
         MVI   PAFKSUB,PAFKSQ                                                   
         MVC   PAFKCPY,CUL                                                      
         MVC   PAFKART,ARTKART                                                  
         MVC   PAFKSUP,ARTKSUP                                                  
         MVC   PAFKCLI,ARTKCLI                                                  
         MVC   PAFKOFF,ARTKOFF                                                  
         MVC   PAFKSTA,ARTRSTA     STATUS                                       
         MVC   PAFKDA,MYDA         AND DISK ADDRESS                             
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,PAFKEY,PAFKEY,OWRK2                    
         BE    APAS04                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
APAS04   DS    0H                  THAT'S IT                                    
         SPACE 1                                                                
APASX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CHANGE PASSIVES ON FILE (ACTRECD OKEY1/OREC1 WITH RECORD)           *         
***********************************************************************         
         SPACE 1                                                                
         USING ARTRECD,R4                                                       
         USING PAFRECD,R3                                                       
CPAS     NTR1                                                                   
         LA    R4,OKEY1                                                         
         LA    R3,OKEY2                                                         
         XC    PAFKEY,PAFKEY                                                    
         MVI   PAFKTYP,PAFKTYPQ                                                 
         MVI   PAFKSUB,PAFKSQ                                                   
         MVC   PAFKCPY,ARTKCPY                                                  
         MVC   PAFKART,ARTKART                                                  
         MVC   PAFKSUP,ARTKSUP                                                  
         MVC   PAFKCLI,ARTKCLI                                                  
         MVC   PAFKOFF,ARTKOFF                                                  
         CLI   ACTNUM,ACTNRES      RESTORE: ALLOW DELETES                       
         BE    CPAS02                                                           
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,OKEY2,OKEY2,0                 
         BE    CPAS04                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CPAS02   GOTO1 DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,OKEY2,OKEY2,0                 
         CLI   8(R1),X'02'                                                      
         BE    CPAS04                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CPAS04   LA    R3,OKEY2                                                         
         CLC   PAFKDA,MYDA                                                      
         BE    CPAS06                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CPAS06   MVC   PAFKSTA,ARTKSTA     REFRESH STATUS                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OKEY2,OKEY2,TEMP                       
         BE    CPAS08                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
         USING PASRECD,R3                                                       
CPAS08   LA    R3,OKEY2                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,ARTKCPY                                                  
         MVC   PASKSEQ,MYSEQNO                                                  
         CLI   ACTNUM,ACTNRES      RESTORE: ALLOW DELETES                       
         BE    CPAS10                                                           
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,OKEY2,OKEY2,0                 
         BE    CPAS12                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CPAS10   GOTO1 DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,OKEY2,OKEY2,0                 
         CLI   8(R1),X'02'                                                      
         BE    CPAS12                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CPAS12   LA    R3,OKEY2                                                         
         CLC   PASKDA,MYDA                                                      
         BE    CPAS14                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CPAS14   MVC   PASKSTA,ARTKSTA     REFRESH STATUS                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OKEY2,OKEY2,TEMP                       
         BE    CPASX                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
CPASX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CHANGE A RECORD ON FILE (OKEY1/OREC1 WITH RECORD)                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ARTRECD,R4                                                       
CREC     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,PUTREX,ACCMST,MYDA,OREC1,OWRK1                      
         BE    CREC02                                                           
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
CREC02   MVC   OKEY1,OREC1         NOW CHANGE DIRECTORY RECORD                  
         CLI   ACTNUM,ACTNRES      RESTORE: ALLOW FOR DELETES                   
         BE    CREC04                                                           
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,OKEY1,OKEY1,0                 
         BE    CREC06                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CREC04   GOTO1 DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,OKEY1,OKEY1,0                 
         CLI   8(R1),X'02'                                                      
         BE    CREC06                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CREC06   LA    R4,OKEY1                                                         
         MVC   ARTKSTA,MYSTAT                                                   
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OKEY1,OKEY1,TEMP                       
         BE    CRECX                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
CRECX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CURRENCY CODE INPUT                                        *         
***********************************************************************         
*&&UK                                                                           
VALCUR   NTR1                                                                   
         SPACE 1                                                                
         XC    CURTENT,CURTENT                                                  
         MVI   ERROR,INVALID                                                    
         OC    PROCURC,MYSPACES                                                 
         LA    R2,PROCURCH                                                      
         CLI   5(R2),0                                                          
         BNE   VALCUR2                                                          
         MVC   CUCURR,MYSPACES                                                  
         B     VALCURX                                                          
         SPACE 1                                                                
VALCUR2  CLC   PROCURC,MYSPACES                                                 
         BNE   VALCUR4                                                          
         MVC   CUCURR,MYSPACES                                                  
         B     VALCURX                                                          
         SPACE 1                                                                
VALCUR4  CLC   PROCURC,COMPCURP                                                 
         BE    ERREND                                                           
         GOTO1 VBLDCUR,DMCB,PROCURC,(X'80',CURTENT),ACOMFACS                    
         CLI   0(R1),0                                                          
         BNE   ERREND                                                           
         L     RF,0(R1)            RF=A(CURRENCY RECORD)                        
         USING GCURD,RF                                                         
         TM    GCFSTAT,GCDSPROD    PRODUCTION CURRENCY?                         
         BZ    ERREND                                                           
         SPACE 1                                                                
         MVI   ERROR,INVREC        TAKE TWO DEC PLACES ONLY                     
         CLI   CURTENT+CURTDECP-CURTABD,2                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVI   ERROR,INVALID       MUST MATCH CREDITOR'S CURR                   
         CLC   CURSUPC,MYSPACES    CREDITOR SET?                                
         BE    VALCUR6                                                          
         OC    CURSUPF,CURSUPF                                                  
         BZ    ERREND                                                           
         CLI   CURSUPF,ASTCANY                                                  
         BE    VALCUR6                                                          
         CLC   CURSUPF,CURTENT+CURTCUR-CURTABD                                  
         BNE   ERREND                                                           
         SPACE 1                                                                
VALCUR6  MVC   CUCURR,CURTENT+CURTCUR-CURTABD                                   
         SPACE 1                                                                
VALCURX  B     XIT                                                              
         DROP  RF                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETOFF   NTR1                                                                   
         SPACE 1                                                                
         MVC   MYAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY        TUCK THE OFFICE KEY AWAY                     
         LA    R4,KEY                                                           
         USING OGRRECD,R4                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY(3),CUL                                                   
         MVC   OGRKOFC,PROOFFC                                                  
         MVI   ERROR,OFFNOXIS                                                   
         LA    R2,PROOFFCH                                                      
         GOTO1 HIGH                                                             
         CLC   OGRKEY,KEYSAVE      TEST IF CODE FOUND                           
         BNE   ERREND              REJECT CODE                                  
         SPACE 1                                                                
         MVC   CUROFFC,PROOFFC                                                  
         MVC   CUROFFN,MYSPACES                                                 
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         USING NAMELD,R3                                                        
GETO02   CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    GETO04                                                           
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETO02                                                           
         SPACE 1                                                                
GETO04   XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUROFFN(0),NAMEREC                                               
         SPACE 1                                                                
         MVC   KEY(42),SAVEKEY1    RESTORE OFFICE KEY                           
         MVC   AIO,MYAIO           RESTORE IO AREA POINTER                      
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETCLI   NTR1                                                                   
         SPACE 1                                                                
         MVC   MYAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY        TUCK THE OFFICE KEY AWAY                     
         USING ACTRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   ACTKEY,MYSPACES                                                  
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKULA(2),=C'SJ'                                                
         MVC   ACTKACT(5),PROCLIC                                               
         LA    R2,PROCLICH                                                      
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE      TEST IF FOUND                                
         BNE   ERREND              REJECT                                       
         SPACE 1                                                                
         MVC   CURCLIC,ACTKACT                                                  
         MVC   CURCLIN,MYSPACES                                                 
         MVC   CURCLIO,MYSPACES                                                 
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         MVI   TEMP,0                                                           
         USING NAMELD,R3                                                        
GETC02   CLI   NAMEL,0                                                          
         BE    GETC10                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    GETC06                                                           
         CLI   NAMEL,PPRELQ                                                     
         BE    GETC08                                                           
         SPACE 1                                                                
GETC04   IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETC02                                                           
         SPACE 1                                                                
GETC06   MVI   TEMP,1                                                           
         XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURCLIN(0),NAMEREC                                               
         B     GETC04                                                           
         SPACE 1                                                                
         USING PPRELD,R3                                                        
GETC08   MVC   CURCLIO,PPRGAOFF                                                 
         OC    CURCLIO,MYSPACES                                                 
         B     GETC04                                                           
         SPACE 1                                                                
GETC10   CLI   TEMP,1              NAME FOUND?                                  
         BE    GETC12                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
GETC12   MVC   KEY(42),SAVEKEY1    RESTORE OFFICE KEY                           
         MVC   AIO,MYAIO           RESTORE IO AREA POINTER                      
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETSUP   NTR1                                                                   
         SPACE 1                                                                
         OC    PROSUPC,MYSPACES                                                 
         LA    R2,PROSUPCH                                                      
         USING LDGRECD,R4                                                       
         LA    R4,KEY              READ LEDGER FIRST                            
         MVC   SAVEKEY1,KEY        TUCK THE OFFICE KEY AWAY                     
         MVC   MYAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         MVC   LDGKEY,MYSPACES                                                  
         MVC   LDGKCPY,CUL                                                      
         MVC   LDGKUNT(2),PROSUPC                                               
         MVI   ERROR,INVLDGR                                                    
         CLC   LDGKUNT(2),LDGSV                                                 
         BE    GETSUP0                                                          
         CLC   LDGKUNT(2),LDGSX                                                 
         BE    GETSUP0                                                          
         B     ERREND                                                           
         SPACE 1                                                                
GETSUP0  GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         USING LDGELD,R3                                                        
         L     R3,AIO                                                           
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         MVI   ERROR,INVLDGR                                                    
         SPACE 1                                                                
GETSUP1  CLI   LDGEL,LDGELQ                                                     
         BE    GETSUP2                                                          
         CLI   LDGEL,0                                                          
         BE    ERREND                                                           
         IC    R0,LDGLN                                                         
         AR    R3,R0                                                            
         B     GETSUP1                                                          
         SPACE 1                                                                
GETSUP2  CLI   LDGOPOS,LDGONONE    OFFICE OK?                                   
         BE    GETSUP4                                                          
         CLI   LDGOPOS,LDGONKHI                                                 
         BH    GETSUP4                                                          
         MVC   TEMP(1),LDGOPOS                                                  
         LA    RE,PROSUPC+1                                                     
         LA    RF,0                                                             
         TM    TEMP,LDGOKEY2       2CO?                                         
         BZ    GETSUP3                                                          
         LA    RF,1                                                             
         NI    TEMP,HEXFF-LDGOKEY2                                              
         SPACE 1                                                                
GETSUP3  IC    R0,TEMP                                                          
         AR    RE,R0                                                            
         MVC   TEMP+2(2),MYSPACES                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),0(RE)                                                  
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,TEMP+2                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BE    GETSUP4                                                          
         MVI   ERROR,SECLKOUT                                                   
         B     ERREND                                                           
         DROP  R1                                                               
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
GETSUP4  LA    R4,KEY                                                           
         MVC   ACTKEY,MYSPACES                                                  
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKULA,PROSUPC                                                  
         MVI   ERROR,NOTFOUND                                                   
         CLC   ACTKACT,MYSPACES                                                 
         BE    ERREND                                                           
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   ACTKEY,KEYSAVE      TEST IF FOUND                                
         BNE   ERREND              REJECT                                       
         SPACE 1                                                                
         MVC   CURSUPC,PROSUPC                                                  
         MVC   CURSUPN,MYSPACES                                                 
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         MVI   TEMP,0                                                           
         USING NAMELD,R3                                                        
GETSUP5  CLI   NAMEL,0                                                          
         BE    GETSUP9                                                          
         CLI   NAMEL,NAMELQ                                                     
         BE    GETSUP7                                                          
         CLI   NAMEL,ASTELQ                                                     
         BE    GETSUP8                                                          
         SPACE 1                                                                
GETSUP6  IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETSUP5                                                          
         SPACE 1                                                                
GETSUP7  OI    TEMP,X'10'                                                       
         XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURSUPN(0),NAMEREC                                               
         B     GETSUP6                                                          
         SPACE 1                                                                
         USING ASTELD,R3                                                        
GETSUP8  OI    TEMP,X'01'                                                       
         MVC   CURSUPF,ASTCUR                                                   
         B     GETSUP6                                                          
         SPACE 1                                                                
GETSUP9  TM    TEMP,X'10'          (ASTELD IS NOT REQUIRED, TCLE)               
         BNZ   *+6                                                              
         DC    H'0'                NO NAMELD ON RECORD                          
         SPACE 1                                                                
         MVC   KEY(42),SAVEKEY1    RESTORE OFFICE KEY                           
         MVC   AIO,MYAIO           RESTORE IO AREA POINTER                      
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORK CODE                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETWCD   NTR1                                                                   
         SPACE 1                                                                
         MVC   MYAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY        TUCK THE OFFICE KEY AWAY                     
         USING WCORECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   WCOKEY,MYSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUL                                                      
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,PROWRKC                                                  
         OC    WCOKWRK,MYSPACES                                                 
         LA    R2,PROWRKCH                                                      
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 HIGH                                                             
         CLC   WCOKEY,KEYSAVE      TEST IF FOUND                                
         BNE   ERREND              REJECT                                       
         SPACE 1                                                                
         MVC   CURWRKC,WCOKWRK                                                  
         MVI   CURWRKS,0                                                        
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         USING WCOELD,R3                                                        
GETWCD1  CLI   WCOEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   WCOEL,WCOELQ                                                     
         BE    GETWCD2                                                          
         IC    R0,WCOLN                                                         
         AR    R3,R0                                                            
         B     GETWCD1                                                          
         SPACE 1                                                                
GETWCD2  MVI   ERROR,NOTOK                                                      
*&&UK                                                                           
         CLI   AGYCNTRY,CTRYGER    UK WORKS DIFFERENTLY FROM GERMANY            
         BNE   GETWCD9                                                          
         SPACE 1                                                                
         MVI   CURWRKS,INTQ        - SEE WHETHER NUMERIC                        
         CLI   CURWRKC,C'0'                                                     
         BL    GETWCD9                                                          
         MVI   CURWRKS,EXTQ                                                     
*&&                                                                             
GETWCD9  MVC   KEY(42),SAVEKEY1    RESTORE OFFICE KEY                           
         MVC   AIO,MYAIO           RESTORE IO AREA POINTER                      
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ELEMENT AND PERSON ACTIVITY ROUTINES                                *         
***********************************************************************         
         SPACE 1                                                                
MYREMEL  NTR1  ,                                                                
         L     R3,AIO                                                           
         LA    R4,ACTRFST-ACTRECD(R3)                                           
         XR    R1,R1                                                            
MYREM02  IC    R1,1(R4)                                                         
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLC   ELCODE,0(R4)                                                     
         BE    MYREM04                                                          
         AR    R4,R1                                                            
         B     MYREM02                                                          
MYREM04  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R4)                                                 
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(ELCODE,AIO),0                          
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                FATAL ERROR                                  
         SPACE 1                                                                
MYADDEL  NTR1  ,                                                                
         CLI   ELEMENT,0                                                        
         BE    XIT                                                              
         XR    RF,RF                                                            
         CLI   ELEMENT,PRIELQ      ENSURE IT'S NOT FIRST                        
         BNE   *+8                                                              
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,ELEMENT,(RF)                        
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         MVI   ERROR,TOOLONG       DID RECORD GET TOO LONG                      
         CLI   DMCB+12,5                                                        
         BE    ERREND                                                           
         DC    H'0'                OTHER ERRORS UNACCEPTABLE                    
         SPACE 1                                                                
MYPERIN  NTR1  ,                                                                
         CLI   TWAALIAS,X'41'      ADD A PERSON ID ELEMENT                      
         BL    XIT                 IF AN ALIAS IS AVAILABLE                     
         MVI   ELCODE,PACELQ                                                    
         BAS   RE,MYREMEL                                                       
         LA    R4,ELEMENT                                                       
         USING PACELD,R4                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   PACEL,PACELQ                                                     
         MVI   PACLN,PACLNQ2                                                    
         MVC   PACPERS,TWAALIAS                                                 
         MVC   PACDATE,TODAYP                                                   
         BAS   RE,MYADDEL                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
MYPEROUT NTR1  ,                                                                
         MVC   WORK,MYSPACES       EXTRACT PERSON/DATE/PERSON ON DATE           
         L     R4,AIO                                                           
         USING PACELD,R6                                                        
         XR    R0,R0                                                            
         LA    R6,ARTRFST-ARTRECD(R4)                                           
MYPERO02 CLI   PACEL,0                                                          
         BE    XIT                                                              
         CLI   PACEL,PACELQ                                                     
         BE    MYPERO04                                                         
         IC    R0,PACLN                                                         
         AR    R6,R0                                                            
         B     MYPERO02                                                         
MYPERO04 MVC   WORK(8),PACPERS                                                  
         GOTO1 DATCON,DMCB,(1,PACDATE),(8,WORK+10)                              
         MVC   WORK+20(8),WORK                                                  
         MVC   WORK+29(3),AC@ON                                                 
         MVC   WORK+32(8),WORK+10                                               
         GOTO1 SQUASHER,DMCB,WORK+20,20                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE, TABLES, EQUATES AND LTORG                            *         
***********************************************************************         
         SPACE 1                                                                
MYCHARS  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         DC    X'FF'                                                            
         SPACE 1                                                                
GERLANGS DS    0XL4                LANGUAGES FOR GERMANY                        
         DC    AL1(LANGENG),C'ENG'                                              
         DC    X'FF'                                                            
         SPACE 1                                                                
ALLLANGS DS    0XL4                LANGUAGES FOR ALL OTHERS                     
         DC    X'FF'                                                            
         SPACE 1                                                                
INTQ     EQU   C'I'                                                             
EXTQ     EQU   C'E'                                                             
NOMATCHQ EQU   C'N'                                                             
         SPACE 1                                                                
HEXFF    EQU   X'FF'                                                            
         SPACE 1                                                                
LDGSV    DC    CL2'SV'                                                          
LDGSX    DC    CL2'SX'                                                          
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
DMRDHI   DC    CL8'DMRDHI'                                                      
ADDREX   DC    CL8'ADDREC'                                                      
GETREX   DC    CL8'GETREC'                                                      
PUTREX   DC    CL8'PUTREC'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
         SPACE 1                                                                
MYSPACES DC    64C' '                                                           
         SPACE 1                                                                
SAVEPRIV DS    CL(L'PROPRI1)                                                    
SAVEDATV DS    CL(L'PRODAT1)                                                    
SAVEPRIL DS    XL1                                                              
SAVEDATL DS    XL1                                                              
         SPACE 1                                                                
PDHIDAT  DS    XL3                                                              
PDNUM    DS    XL1                                                              
PDCURR   DS    CL3                                                              
         SPACE 1                                                                
DELIND   DS    XL1                                                              
DELETEQ  EQU   X'08'                                                            
CHGDATQ  EQU   X'10'                                                            
CHGAMTQ  EQU   X'01'                                                            
         SPACE 1                                                                
RELO     DS    F                                                                
VBLDCUR  DS    V                                                                
AACSRCHC DS    V                                                                
TODAY    DS    XL3                                                              
TEMP     DS    CL250                                                            
MYELEM   DS    CL(L'ELEMENT)                                                    
CURTENT  DS    CL(CURTABL+L'CURTSHRT+L'CURTLONG)                                
DATELEN  DS    CL1                 DATE LENGTH FOR PERVAL CALL                  
DATEFORM DS    CL1                 DATE FORMAT FOR PERVAL CALL                  
PERVBLK  DS    CL56                STORAGE BLOCK FOR PERVAL                     
         SPACE 1                                                                
DDIN     DS    0C                                                               
         DCDDL AC#YES,3                                                         
         DCDDL AC#ON,3                                                          
         DCDDL AC#DEL,10                                                        
         DCDDL AC#CHG,8                                                         
         DC    X'00'                                                            
         SPACE 1                                                                
DDOUT    DS    0C                                                               
AC@YES   DS    CL3                                                              
AC@ON    DS    CL3                                                              
AC@DEL   DS    CL10                                                             
AC@CHG   DS    CL8                                                              
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ADDIND   DS    XL1                                                              
ADD08    EQU   X'08'                                                            
ADD80    EQU   X'80'                                                            
OKEY3    DS    XL42                                                             
         DS    XL16                                                             
OKEY2    DS    XL42                                                             
         DS    XL16                                                             
OWRK2    DS    XL64                                                             
OKEY1    DS    XL42                                                             
         DS    XL16                                                             
OWRK1    DS    XL64                                                             
OREC1    DS    2000X                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS AND DSECTS                                           *         
***********************************************************************         
         SPACE 1                                                                
* ACPROWORKA                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* DDFLDIND                                                                      
* DDEBLOCK                                                                      
* DDPERVALD                                                                     
* DDFLDHDR                                                                      
* ACDDEQUS                                                                      
* DDLANGEQUS                                                                    
* DDCTRYEQUS                                                                    
* DDCURTABD                                                                     
* GEGENCUR                                                                      
* DDCOMFACSD                                                                    
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDEBLOCK                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE DDCURTABD                                                      
       ++INCLUDE GEGENCUR                                                       
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROABD                                                       
         SPACE 1                                                                
         DS    0F                                                               
       ++INCLUDE ACRAPPERD                                                      
         SPACE 1                                                                
OKEYX    DS    XL42                                                             
OKEYY    DS    XL42                                                             
CUROFFC  DS    CL2                                                              
CUROFFN  DS    CL36                                                             
CURCLIC  DS    CL5                                                              
CURCLIN  DS    CL36                                                             
CURCLIO  DS    CL2                                                              
CURSUPC  DS    CL14                                                             
CURSUPN  DS    CL36                                                             
CURSUPF  DS    CL3                                                              
CURWRKC  DS    CL2                                                              
CURWRKS  DS    XL1                                                              
CURFILT  DS    CL1                                                              
CURLANG  DS    CL3                                                              
CURLANQ  DS    XL1                                                              
CURSTAT  DS    XL1                                                              
CURSTA2  DS    XL1                                                              
CUCURR   DS    CL3                                                              
CUDATE   DS    XL3                                                              
CUSDAT   DS    XL3                                                              
CUCNTR   DS    XL1                                                              
CUDTORYN DS    CL1                                                              
*                                                                               
MYSEQNO  DS    XL3                                                              
MYSTAT   DS    XL8                                                              
MYMODE   DS    XL1                                                              
MYDA     DS    F                                                                
MYAIO    DS    A                                                                
MYAIO2   DS    A                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACPRO54   01/07/16'                                      
         END                                                                    
