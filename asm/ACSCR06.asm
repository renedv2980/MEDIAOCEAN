*          DATA SET ACSCR06    AT LEVEL 030 AS OF 09/02/15                      
*PHASE T60C06A,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 29 AS OF 12/16/11         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'PREVIEW REPORT'                                                 
T60C06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C06,RA,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         USING RESRECD,R2                                                       
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         CLI   SVACTION,ACTPRVW                                                 
         BE    SCR01                                                            
         CLI   SVACTION,0                                                       
         BNE   *+8                                                              
         MVI   SVACTION,ACTDIS     DEFAULT TO DISPLAY IF NONE                   
         MVC   PREVACT,SVACTION    SAVE SAVED ACTION                            
*                                                                               
SCR01    TM    SCRSRLH+4,FVITHIS                                                
         BNZ   SCR02                                                            
         MVC   SCRSRL,SRLPRVW                                                   
         OI    SCRSRLH+6,FVOXMT                                                 
*                                                                               
SCR02    MVC   SRLPRVW,SCRSRL                                                   
         EJECT ,                                                                
         MVC   DATE2DAY(2),ASEDAT+6      TODAYS DATE   (YY)                     
*&&US*&& MVC   DATE2DAY+2(2),ASEDAT      REMOVE SLASH  (MM)                     
*&&US*&& MVC   DATE2DAY+4(2),ASEDAT+3    REMOVE SLASH  (DD)                     
*&&UK*&& MVC   DATE2DAY+2(2),ASEDAT+3    REMOVE SLASH  (MM)                     
*&&UK*&& MVC   DATE2DAY+4(2),ASEDAT     REMOVE SLASH  (DD)                      
         MVC   APWORK(3),ASBDAT         BINARY DATE OF TODAY                    
         MVC   APWORK+1(1),FISCALMO     START WITH COMPANY FISCAL MONTH         
         MVI   APWORK+2,1               START WITH DAY ONE                      
         GOTO1 VDATCON,APPARM,(3,APWORK),(0,TEMPDATE)                           
*                                                                               
         LA    R0,4                                                             
         LA    R2,QTRTAB                                                        
         MVC   QTRTAB,TEMPDATE     SAVE OFF DATE                                
         LA    R3,QTRMONTH                                                      
         LA    R4,1                BUMP UP ONE MONTH AT A TIME                  
*                                                                               
SCR04    MVC   0(6,R2),TEMPDATE    SAVE OFF START OF QTR                        
         MVC   4(2,R2),=C'01'      SET TO FIRST DAY OF MONTH                    
         MVC   0(2,R3),TEMPDATE+2  SAVE OFF MONTH                               
         GOTO1 VADDAY,APPARM,(C'M',TEMPDATE),TEMPDATE,(R4)                      
         MVC   2(2,R3),TEMPDATE+2  SAVE OFF MONTH                               
         GOTO1 VADDAY,APPARM,(C'M',TEMPDATE),(X'80',TEMPDATE),(R4)              
         MVC   6(6,R2),TEMPDATE                                                 
         MVC   4(2,R3),TEMPDATE+2  SAVE OFF MONTH                               
         LA    R2,L'QTRTAB(,R2)    NEXT QTR PAIR                                
         LA    R3,L'QTRMONTH(,R3)  NEXT QTR MONTH GROUP                         
         GOTO1 VADDAY,APPARM,(C'M',TEMPDATE),TEMPDATE,(R4)                      
         BCT   R0,SCR04                                                         
*&&DO                                                                           
SCR04    MVC   0(2,R4),2(R2)       SAVE OF MONTH                                
         LA    R3,6(,R2)                                                        
         GOTO1 VADDAY,APPARM,(C'M',(R2)),TEMPDATE,F'1'                          
         LA    R4,2(,R4)                                                        
         MVC   0(2,R4),TEMPDATE+2  MOVE IN MONTH                                
         LA    R4,2(,R4)                                                        
         GOTO1 VADDAY,APPARM,(C'M',(R2)),(X'80',(R3)),F'2'                      
         MVC   0(2,R4),2(R3)       MOVE IN MONTH                                
         LA    R4,2(,R4)                                                        
         LR    R2,R3                                                            
         LA    R3,6(,R2)                                                        
         GOTO1 VADDAY,APPARM,(C'M',(R2)),6(R2),F'1'                             
         MVC   4(2,R3),=C'01'                                                   
         LR    R2,R3                                                            
         BCT   R0,SCR04                                                         
*&&                                                                             
         LA    R0,4                CHECK 4 QTRS                                 
         LA    R1,1                FIRST QTR                                    
         LA    R3,QTRMONTH              FIND QTR TODAY FALLS IN                 
SCR05    CLC   DATE2DAY+2(2),0(R3)      IS IT 1ST MONTH OF QTR ?                
         BE    SCR10                                                            
         CLC   DATE2DAY+2(2),2(R3)      IS IT 2ND MONTH OF QTR ?                
         BE    SCR10                                                            
         CLC   DATE2DAY+2(2),4(R3)      IS IT 3RD MONTH OF QTR ?                
         BE    SCR10                                                            
         LA    R3,L'QTRMONTH(,R3)  NEXT QTR GROUP                               
         LA    R1,1(,R1)           NEXT QTR NUMBER                              
         BCT   R0,SCR05                                                         
         DC    H'00'                                                            
*                                                                               
*CR10    STC   R1,CURQTR                                                        
*&&DO                                                                           
         MVI   FSCQTR,1                 SET FSCQTR (JAN FOR NOW)                
         LA    R1,4                                                             
         LA    R2,QTRTAB                                                        
         SR    R3,R3                                                            
         IC    R3,FSCQTR                                                        
         SR    R4,R4                                                            
         IC    R4,ASBDAT           CURRENT YEAR                                 
*                                                                               
SCR04    STC   R4,0(,R2)           SAVE YEAR OF QTR                             
         STC   R3,1(,R2)           SAVE START MONTH OF QTR                      
         LA    R2,2(,R2)                                                        
         LA    R3,3(,R3)                                                        
         CLM   R3,1,=AL1(12)                                                    
         BNH   SCR05                                                            
         SH    R3,=H'12'           SUBTRACT 12 MONTHS                           
         LA    R4,1(,R4)           ADD ONE YEAR                                 
*                                                                               
SCR05    BCT   R1,SCR04                                                         
*                                                                               
         LA    R0,3                                                             
         LA    R1,1                                                             
         LA    R2,QTRTAB                                                        
         SR    RF,RF                                                            
*                                                                               
SCR06    IC    RF,1(,R2)           M OF YMD (BINARY)                            
         CLM   RF,1,ASBDAT+1       DOES MONTH MATCH                             
         BH    SCR07               LOOP                                         
         BE    SCR10               FOUND IT                                     
         AH    RF,=H'03'                                                        
         CLM   RF,1,ASBDAT+1                                                    
         BH    SCR10               FOUND IT                                     
*                                                                               
SCR07    LA    R2,2(,R2)                                                        
         LA    R1,1(,R1)                                                        
         BCT   R0,SCR06                                                         
*&&                                                                             
*                                                                               
SCR10    STC   R1,CURQTR                                                        
         LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALIDATE KEY                                 
         B     EXIT                VALREC                                       
         B     DISKEY              DISKEY                                       
         B     DISREC              DISPLAY RECORD                               
         B     EXIT                DELREC                                       
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                PROCESS LIST/SELECT                          
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY RECORD                                  
         B     EXIT                                                             
         B     EXIT                RE-DISPLAY LIST ENTRY AFTER LFM              
         EJECT ,                                                                
EXIT     CLI   APMODE,APMVALK                                                   
         BE    EXIT95              TURN ON HOLD AND RETURN                      
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP                                                 
         BZ    EXIT95                                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP       SET TO SWAP                                  
         MVC   APPARM(1),TWASWPRE                                               
         CLI   APPFKEY,PFKREQ      SWAP TO REQUEST?                             
         BE    EXIT10              YES, SO DON'T USE PREVACT YET                
         MVC   TWASWPAC,PREVACT                                                 
         MVI   PREVACT,0                                                        
*                                                                               
EXIT10   MVC   APPARM+1(1),TWASWPAC                                             
         MVI   APPFKEY,0                                                        
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    EXIT999                                                          
         CLI   APMODE,APMDISR      IS   MODE DISPLAY   RECORD ?                 
         BNE   EXIT999             NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY   IS   HAPPY                    
*                                                                               
EXIT999  XIT1                                                                   
         EJECT ,                                                                
VALKEY   DS    0H                                                               
         GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   VALKEY05                                                         
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     EXIT                                                             
*                                                                               
VALKEY05 DS    0H                                                               
         L     R1,0(,R1)           GET   BIG  WORK AREA ADDRESS                 
         LA    R1,0(,R1)           CLEAR HIGH ORDER     BYTE                    
         ST    R1,AREPAREA         SAVE  BIG  WORK AREA ADDRESS                 
*                                                                               
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,PRVCODEH                                                   
         MVC   RESKFORM,FVIFLD     FORMAT                                       
         BE    VALKEY15                                                         
         TM    PRVCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   PRVCODE,SAVFORM                                                  
         OI    PRVCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         TM    TWAMODE,TWAMDFR                                                  
         BNZ   *+10                                                             
         MVC   SAVEKEY(L'RESKEY),RESKEY                                         
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORDD+IOACCFIL+IO1                                            
*        CLI   APACTN,ACTDIS                                                    
*        BE    *+8                                                              
*        LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BE    VALKEY97                                                         
         TM    IOERR,IOEDEL        DELETED RECORD?                              
         BZ    VALKEY99            NO SO OTHER PROBLEM                          
*                                  IT IS MARKED DELETED, PREVIEW ANYWAY         
VALKEY97 L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         MVI   APINDS,APIOKDIS                                                  
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
         USING RESRECD,R2                                                       
DISKEY   DS    0H                                                               
         LA    R2,APRECKEY                                                      
         MVC   PRVCODE,RESKFORM                                                 
         MVI   PRVWCOL,1                                                        
         MVI   PRVWROW,1                                                        
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY (PREVIEW REPORT)                                *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC PRVNMEH,PRVOWNH                                                  
         TWAXC PRVFRSTH,PRVTABH,PROT=Y                                          
         GOTO1 GETNAME,APPARM,(R2),PRVNMEH                                      
         GOTO1 GETPER,APPARM,(R2),PRVOWNH                                       
         GOTO1 AFVAL,SCRSRLH                                                    
         MVCDD AC@TFOR,AC#TFOR                                                  
         GOTO1 VDICTAT,APPARM,C'SL  ',AC@TFOR                                   
         MVCDD AC@TREQ,AC#TREQ                                                  
         GOTO1 VDICTAT,APPARM,C'SL  ',AC@TREQ                                   
         XC    SCROLL,SCROLL                                                    
         SR    R4,R4                                                            
         CLI   APPFKEY,PFKUP                                                    
         BNE   *+8                                                              
         LA    R4,PAGEUP                                                        
         CLI   APPFKEY,PFKDOWN                                                  
         BNE   *+8                                                              
         LA    R4,PAGEUP                                                        
         CLI   APPFKEY,PFKLEFT                                                  
         BNE   *+8                                                              
         LA    R4,PAGELEFT                                                      
         CLI   APPFKEY,PFKRIGHT                                                 
         BNE   *+8                                                              
         LA    R4,PAGELEFT                                                      
         CLI   FVILEN,0            ANY  SCROLL DATA ENTERED ?                   
         BNE   DISREC01            YES, SKIP                                    
         MVC   SCRSRL,AC@HALF      NO,  ASSUME HALF                             
         MVI   FVILEN,4            RESET       DATA LENGTH                      
         MVI   FVXLEN,3            RESET       EXMVC     LENGTH                 
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
*                                                                               
DISREC01 DS    0H                                                               
         ZIC   RF,FVXLEN           GET  EXECUTE     LENGTH                      
         EX    RF,*+8              COMPARE     FOR  INPUT     LENGTH            
         B     *+10                                                             
         CLC   SCRSRL(0),AC@PAGE                                                
         BNE   DISRC01A            NOT    PAGE                                  
*                                  FOUND  PAGE                                  
         MVC   SCRSRL,AC@PAGE      DISPLAY     PAGE                             
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         B     DISREC02                                                         
*                                                                               
DISRC01A DS    0H                                                               
         SRL   R4,1                DIVIDE BY TWO                                
         EX    RF,*+8              COMPARE     FOR  INPUT     LENGTH            
         B     *+10                                                             
         CLC   SCRSRL(0),AC@HALF                                                
         BNE   DISRC01B            NOT    HALF                                  
*                                  FOUND  HALF                                  
         MVC   SCRSRL,AC@HALF      DISPLAY     HALF                             
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         B     DISREC02                                                         
*                                                                               
DISRC01B DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,APPARM,(C'N',SCRSRL),(RF)                               
         CLI   APPARM,0                                                         
         BNE   IVALIPUT                                                         
         ICM   R4,3,APPARM+6                                                    
*                                                                               
DISREC02 DS    0H                                                               
         MVC   SRLPRVW,SCRSRL      SAVE   FOR  NEXT TIME                        
         STH   R4,SCROLL                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         CLEAR WITH X'40'                             
         L     RE,AREPAREA                                                      
         L     RF,=AL4(MAXLINES*MAXRPTWD)                                       
         MVCL  RE,R0                                                            
         MVI   PRTBOX,YES                                                       
         MVI   PRTLFJT,NO                                                       
         MVI   PRTSTYLE,C'L'       LANDSCAPE?                                   
         MVI   PRTCASE,C'U'        UPPER ONLY                                   
         MVI   PRTCWRAP,YES        COLUMN WRAP?                                 
*                                                                               
         USING RPFELD,R1                                                        
         MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC04                                                         
         TM    RPFDNOPT,RPFDDOWN   DOWN LOAD ONLY?                              
         BNZ   IVALREP                                                          
         MVI   PRTBOX,NO                                                        
         TM    RPFPOPT,RPFBOX      PRINT BOXES?                                 
         BZ    *+8                                                              
         MVI   PRTBOX,YES                                                       
         MVI   PRTLFJT,NO                                                       
         TM    RPFPOPT,RPFLFJT     LEFT JUSTIFY?                                
         BZ    *+8                                                              
         MVI   PRTLFJT,YES                                                      
         MVI   PRTSTYLE,C'L'       LANDSCAPE?                                   
         TM    RPFPOPT,RPFPORT     PRINT USING PORTRAIT?                        
         BZ    *+8                                                              
         MVI   PRTSTYLE,C'P'                                                    
         MVI   PRTCASE,C'U'        UPPER ONLY                                   
         TM    RPFPOPT,RPFMCASE    PRINT UPPERCASE ONLY?                        
         BZ    *+8                                                              
         MVI   PRTCASE,C'L'                                                     
         MVI   PRTCWRAP,YES        COLUMN WRAP?                                 
         TM    RPFPOPT2,RPFEXCOL                                                
         BZ    *+8                                                              
         MVI   PRTCWRAP,NO                                                      
         DROP  R1                                                               
         EJECT ,                                                                
*                                  ************************************         
*                                  * SAVE COLUMN INFORMATION          *         
*                                  ************************************         
         USING RCLELD,R4                                                        
*                                                                               
DISREC04 DS    0H                                                               
         MVI   ANYCOLS,C'N'        INIT  ANY   COLUMNS                          
         MVI   ANYNACMC,C'Y'       INIT  ANY   NON-ACCUMULATED COL              
         MVI   COL#,0              INIT  NO.   OF    COLS                       
         XC    MIDSTART,MIDSTART   INIT  MIDDLE      OF   LINE                  
         MVI   MIDSIZE,LEN110/2    INIT  MIDDLE      OF   LINE WIDTH            
         XC    BOXWDTH,BOXWDTH     INIT  BOXES       WIDTH                      
         XC    STKUNDMX,STKUNDMX   INIT  MAX   STACK UNDER     LEVEL            
         MVI   APELCODE,RCLELQ     X'C3'       ELEMENTS                         
         GOTO1 GETEL,(R2)                                                       
         BNE   DISRC07A            NONE,       SKIP  COLUMNS                    
         ST    R1,SAVER1           SAVE  R1                                     
*                                                                               
         LA    R0,COLARRAY         ADDR  OF    COLUMN     ARRAY                 
         LA    R1,L'COLARRAY       SIZE  OF    COLUMN     ARRAY                 
         SR    RF,RF               NO    FROM  AREA                             
         MVCL  R0,RE               CLEAR THE   ARRAY                            
*                                                                               
         L     R1,SAVER1           RESTORE     R1                               
*                                                                               
         LA    R3,COLARRAY+CILNQ   ->    2ND   ELEMENT                          
*                                                                               
         USING COLINFOD,R3                                                      
*                                                                               
DISREC05 LR    R4,R1                                                            
         MVC   COL#,RCLSEQ         NUMBER      OF    COLUMNS                    
         SR    R6,R6                                                            
         IC    R6,RCLWDTH          COL   WIDTH                                  
         STC   R6,CICOLWD          SAVE  THE   WIDTH                            
         MVC   CICOLOPT,RCLOPT     SAVE  THE   OPTIONS                          
         MVC   CICOLOP2,RCLOPT2    SAVE  THE   OPTIONS                          
         AH    R6,BOXWDTH                                                       
         LA    R6,1(,R6)                                                        
         TM    RCLOPT,RCLHIDE      A     HIDDEN      COLUMN ?                   
         BZ    DISRC05B            NO,   CONTINUE                               
         MVI   CICOLWD,0           SET   WIDTH TO    ZERO                       
         LH    R6,BOXWDTH          RESET TOTAL WIDTH                            
         B     DISRC05Z            SKIP  STACK UNDER LOGIC                      
*                                                                               
DISRC05B CLI   RCLSTACK,0          ANY   STACK UNDER ?                          
         BE    DISRC05S            NO,   SKIP                                   
*                                                                               
*                                  **    STACK UNDER INITIALIZATION             
*                                                                               
         MVI   CICOLWD,0           NO    WIDTH USED                             
         LH    R6,BOXWDTH          USE   OLD   RPT   WIDTH                      
         MVC   CISTKUWD,RCLWDTH    SAVE  STACK UNDER WIDTH                      
         MVC   CISTKUCL,RCLSTACK   SAVE  STACK UNDER COL  NUMBER                
         MVC   CISTKUHW,RCLHD1LN   SAVE  STACK UNDER HEAD1      WIDTH           
         MVC   CISTKUH1,SPACES     CLEAR STACK UNDER HEAD1                      
         SR    R1,R1               GET   STACK UNDER HEAD1      WIDTH           
         ICM   R1,1,RCLHD1LN       IS    IT    ZERO ?                           
         BZ    DISRC05D            YES,  SKIP                                   
         ZIC   RF,RCLDATLN         GET   KEYWORD     LENGTH                     
         LA    RE,RCLNDATA(RF)     ->    HEAD1 DATA                             
         BCTR  R1,0                SAVE  HEAD1 TEXT                             
         EXMVC R1,CISTKUH1,0(RE)                                                
*                                                                               
DISRC05D MVC   CISTKUKW,SPACES     ->    CLEAR       KEYWD/EQUN AREA            
         LA    RE,RCLNDATA         ->    RCLELD      DATA       AREA            
         LA    RF,CISTKUKW         ->    COLINFOD    KEYWD/EQUN AREA            
         ZIC   R0,RCLDATLN         GET   KEYWD/EQUN  LENGTH                     
         SR    R1,R1               NUM   OF    CHARACTERS                       
*                                                                               
DISRC05F CLI   0(RE),C' '          MOVE  KEYWORD     OR   EQUATION              
         BE    DISRC05H            WHERE SPACE OR    COMMA                      
         CLC   0(1,RE),SCCOMMA           END   THE   KEYWORD                    
         BE    DISRC05H                                                         
         MVC   0(1,RF),0(RE)                                                    
         LA    RE,1(,RE)           ADDR  OF    NEXT  AVAILABLE  SPACE           
         LA    RF,1(,RF)           ADDR  OF    NEXT  CHARACTER                  
         LA    R1,1(,R1)           NUM   OF    CHARS MOVED                      
         BCT   R0,DISRC05F                                                      
*                                                                               
DISRC05H CLM   R1,1,CISTKUWD       FEWER CHARS THAN  FLD  WIDTH ?               
         BNL   *+8                 NO,   SKIP                                   
         STC   R1,CISTKUWD         SAVE  NUM   OF    CHARS                      
*                                                                               
         ZIC   R1,RCLSTACK         GET   STACK UNDER COL  NUMBER'S              
*                                        ARRAY ELEMENT                          
         MH    R1,=Y(CILNQ)                                                     
         LA    RE,COLARRAY(R1)                                                  
*                                                                               
PREV     USING COLINFOD,RE                                                      
*                                  NOTE  RCLSTACK    CONTAINS   THE             
*                                        LOWEST      STACKED    UNDER           
*                                        COLUMN      NUMBER                     
*                                  WAS   PREVIOUS    COL   HIDDEN ?             
         TM    PREV.CICOLOPT,CIHIDE      WAS   PREVIOUS    HIDDEN ?             
         BZ    DISRC05J            NO,   SKIP                                   
         OI    CICOLOPT,CIHIDE     YES,  THEN  THIS  COL   IS   HIDDEN          
         B     DISRC05Z            CONTINUE                                     
*                                                                               
*                                  GET   NUM   OF    EL'S  PREVIOUSLY           
DISRC05J ZIC   R1,PREV.CISTKULV          STACKED     UNDER THIS FIELD           
         BCTR  R1,0                SUBTRACT    ONE   FOR   THIS FIELD           
         STC   R1,PREV.CISTKULV    SAVE EL'S  STACKED     UNDER                 
         O     R1,=XL4'FFFFFF00'   MAKE  POSITIVE                               
         LPR   R1,R1                                                            
         STC   R1,CISTKULV         SAVE  STACK UNDER LEVEL                      
         CLM   R1,1,STKUNDMX       HIGHER      THAN  PREVIOUS   HIGH ?          
         BNH   *+8                 NO,   SKIP                                   
         STC   R1,STKUNDMX         SAVE  MAX   STACK UNDER LEVEL                
*                                  OLD   HEAD1 SIZE  >=    NEW  SIZE            
         CLC   PREV.CISTKUHW,CISTKUHW                                           
         BNL   *+10                YES,  SKIP                                   
*                                  MOVE  NEW   HEAD1 SIZE  INTO OLD             
         MVC   PREV.CISTKUHW,CISTKUHW                                           
         B     DISRC05Z                                                         
*                                                                               
         DROP  PREV                                                             
*                                                                               
*                                  **    SAVE  COLUMN      WIDTH                
*                                                                               
DISRC05S MVI   ANYCOLS,C'Y'        FOUND A     COLUMN      TO   OUTPUT          
*                                                                               
DISRC05Z STH   R6,CIBOXCOL         SAVE  BOXES COLUMN                           
         STH   R6,BOXWDTH          SAVE  RPT   WIDTH SO    FAR                  
         LA    R3,CILNQ(,R3)       ->    NEXT  COL   INFORMATION                
         LR    R1,R4               ->    ELEMENT                                
         GOTO1 NEXTEL                                                           
         BE    DISREC05                                                         
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         USING COLINFOD,R3                                                      
NEXT     USING COLINFOD,R4                                                      
*                                                                               
*                                  **    FIND  START OF   MID-LINE              
*                                  **    I.E.  FIND  SPACE     FOR              
*                                  **          TOTAL FOR  REQUEST               
*                                                                               
         CLI   ANYCOLS,C'N'        ANY   COLS  TO    OUTPUT ?                   
         BE    DISRC07A            NO,   SKIP                                   
*                                                                               
         LA    R3,COLARRAY         ->    1ST   ARRAY ENTRY                      
         LA    R4,COLARRAY+CILNQ   ->    2ND   ARRAY ENTRY                      
         SR    R1,R1                                                            
         IC    R1,COL#                                                          
*                                                                               
DISREC06 CLI   NEXT.CICOLWD,0      HIDDEN/STACK      UNDER     COL ?            
         BE    DISRC06A            YES,  NEXT  ENTRY                            
         TM    NEXT.CICOLOPT,CIACCM      ACCUMULATED COLUMN ?                   
         BZ    DISREC07            NO,   CONTINUE                               
*                                                                               
DISRC06A LA    R3,CILNQ(,R3)       NEXT  ENTRY                                  
         LA    R4,CILNQ(,R4)       NEXT  ENTRY                                  
         BCT   R1,DISREC06                                                      
         MVI   ANYNACMC,C'N'       NO    NON-ACCUMULATED  COLUMNS               
*                                                                               
*                                  **    FIND  REPORT     WIDTH                 
*                                                                               
DISREC07 MVC   MIDSTART,CIBOXCOL   GET   BOXES COLUMN                           
         MVC   MIDSIZE,NEXT.CICOLWD                                             
*                                                                               
DISRC07A LH    RE,MIDSTART                                                      
         LA    RE,1(,RE)                                                        
         STH   RE,MIDSTART                                                      
         MVC   REPWDTH,=Y(LEN110)  REPORT      AS    110 (PITCH 10)             
*                                  LAST  ROW   FROM  TOP  OF   SCREEN           
         MVC   LASTROW,=AL1(NROW110-NROWS)                                      
         CLC   BOXWDTH,=Y(LEN110)                                               
         BNH   DISRC07B                                                         
         MVC   REPWDTH,=Y(LEN132)  REPORT      AS    132 (PITCH 12)             
*                                  LAST  ROW   FROM  TOP  OF   SCREEN           
         MVC   LASTROW,=AL1(NROW132-NROWS)                                      
         CLC   BOXWDTH,=Y(LEN132)                                               
         BNH   DISRC07B                                                         
         MVC   REPWDTH,=Y(LEN164)  REPORT      AS    164 (PITCH 15)             
*                                  LAST  ROW   FROM  TOP  OF   SCREEN           
         MVC   LASTROW,=AL1(NROW164-NROWS)                                      
         CLC   BOXWDTH,=Y(LEN164)                                               
         BNH   DISRC07B                                                         
         MVC   REPWDTH,=Y(LEN198)  REPORT      AS    198 (PITCH 18/20)          
*                                  LAST  ROW   FROM  TOP  OF   SCREEN           
         MVC   LASTROW,=AL1(NROW198-NROWS)                                      
*                                                                               
         DROP  R3,NEXT                                                          
*                                                                               
DISRC07B CLI   LASTROW,MAXLINES    TOO   MANY  ROWS ?                           
         BNH   *+8                 NO,   SKIP                                   
         MVI   LASTROW,MAXLINES    USE   MAX   LINES                            
*                                                                               
         LH    R1,REPWDTH                                                       
         SH    R1,=Y(NCOLS-1)                                                   
         STC   R1,LASTCOL          LAST  COL   FROM  LEFT OF   SCREEN           
*                                                                               
*                                  ************************************         
*                                  * PROCESS PFK UP/DOWN/LEFT/RIGHT   *         
*                                  * AND THE SCREEN'S LINE/COLUMN NO.S*         
*                                  ************************************         
*                                                                               
*                                  **    PFK   DOWN                             
         SR    RF,RF                                                            
         IC    RF,PRVWROW                                                       
         CLI   APPFKEY,PFKDOWN                                                  
         BNE   DISREC08                                                         
         MVI   APPFKEY,0                                                        
         AH    RF,SCROLL                                                        
         STC   RF,PRVWROW                                                       
         CLC   PRVWROW,LASTROW                                                  
         BL    DISREC12                                                         
         MVC   PRVWROW,LASTROW                                                  
         B     DISREC12                                                         
*                                                                               
*                                  **    PFK   UP                               
DISREC08 CLI   APPFKEY,PFKUP                                                    
         BNE   DISREC09                                                         
         MVI   APPFKEY,0                                                        
         SH    RF,SCROLL                                                        
         BP    *+8                                                              
         LA    RF,1                SET TO 1                                     
         STC   RF,PRVWROW                                                       
         B     DISREC12                                                         
*                                                                               
*                                  **    LINE  NUMBER                           
DISREC09 TM    PRVROWH+4,FVITHIS                                                
         BZ    DISREC10                                                         
         GOTO1 AFVAL,PRVROWH                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,APPARM,(C'N',PRVROW),(RF)                               
         CLI   APPARM,0                                                         
         BNE   IVALIPUT                                                         
         CLI   APPARM+7,MAXLINES                                                
         BNL   IVALHIGH                                                         
         CLC   LASTROW,APPARM+7                                                 
         BL    IVALHIGH                                                         
         MVC   PRVWROW,APPARM+7                                                 
*                                                                               
*                                  **    PFK   RIGHT                            
DISREC10 SR    RF,RF                                                            
         IC    RF,PRVWCOL                                                       
         CLI   APPFKEY,PFKRIGHT                                                 
         BNE   DISREC11                                                         
         MVI   APPFKEY,0                                                        
         AH    RF,SCROLL                                                        
         STC   RF,PRVWCOL                                                       
         CLC   PRVWCOL,LASTCOL                                                  
         BL    DISREC13                                                         
         MVC   PRVWCOL,LASTCOL                                                  
         B     DISREC13                                                         
*                                                                               
*                                  **    PFK   LEFT                             
DISREC11 CLI   APPFKEY,PFKLEFT                                                  
         BNE   DISREC12                                                         
         MVI   APPFKEY,0                                                        
         SH    RF,SCROLL                                                        
         BP    *+8                                                              
         LA    RF,1                SET TO 1                                     
         STC   RF,PRVWCOL                                                       
         B     DISREC13                                                         
*                                                                               
*                                  **    COL   NUMBER                           
DISREC12 TM    PRVCOLH+4,FVITHIS                                                
         BZ    DISREC13                                                         
         GOTO1 AFVAL,PRVCOLH                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,APPARM,(C'N',PRVCOL),(RF)                               
         CLI   APPARM,0                                                         
         BNE   IVALIPUT                                                         
         CLC   APPARM+6(2),=AL2(255)                                            
         BNL   IVALHIGH                                                         
         CLC   LASTCOL,APPARM+7                                                 
         BL    IVALHIGH                                                         
         MVC   PRVWCOL,APPARM+7                                                 
*                                                                               
DISREC13 SR    R1,R1                                                            
         IC    R1,PRVWROW                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'       MAKE IT A CHARACTER                          
         UNPK  PRVROW(L'PRVROW),APDUB                                           
         OI    PRVROWH+6,FVOXMT                                                 
         IC    R1,PRVWCOL                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'       MAKE IT A CHARACTER                          
         UNPK  PRVCOL(L'PRVCOL),APDUB                                           
         OI    PRVCOLH+6,FVOXMT                                                 
*                                                                               
*                                  ************************************         
*                                  * SET UP FOR CENTERING THE REPORT  *         
*                                  ************************************         
         CLI   PRTLFJT,YES         LEFT  JUSTIFY ?                              
         BE    DISREC14            YES,  SKIP                                   
         CLI   ANYCOLS,C'N'        ANY   COLS  TO    OUTPUT ?                   
         BE    DISREC14            NO,   SKIP                                   
         LH    RF,REPWDTH          (REPORT WIDTH - BOX WIDTH)/2                 
         SH    RF,BOXWDTH                                                       
         SRL   RF,1                DIVIDE BY TWO (TO CENTER BOX)                
         SR    R1,R1                                                            
         IC    R1,COL#                                                          
         LA    R1,1(,R1)                                                        
         LA    R3,COLARRAY         ->    COL   INFORMATION                      
*                                                                               
         USING COLINFOD,R3         COLUMN      INFORMATION                      
DISRC13A LH    RE,CIBOXCOL         GET   BASE  BOXES COLUMN                     
         AR    RE,RF               ADJUST      FOR   CENTERING                  
         STH   RE,CIBOXCOL         SAVE  BOXES COLUMN                           
         LA    R3,CILNQ(,R3)       NEXT  COLUMN                                 
         BCT   R1,DISRC13A                                                      
         DROP  R3                                                               
*                                                                               
         LH    RE,MIDSTART                                                      
         AR    RE,RF                                                            
         STH   RE,MIDSTART                                                      
*                                                                               
*                                  ************************************         
*                                  * SAVE ROW INFORMATION             *         
*                                  ************************************         
         USING RRWELD,R1                                                        
         USING ROWD,R4                                                          
DISREC14 MVI   HEADROW,HDLINE1     INIT  1ST   HEADING    ROW                   
         MVI   ROW#,0              INIT  NO.   OF    ROWS                       
         MVI   APELCODE,RRWELQ     X'C2' ROW   ELEMENT                          
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC18                                                         
         LA    R4,ROWINFO          SAVE  OFF   ROW   INFORMATION                
*                                                                               
DISREC15 MVC   ROWOPT,RRWOPT       MOVE  IN    ROWOPT                           
*&&US*&& MVC   ROWOPT3,RRWOPT3     MOVE  IN    ROWOPT3                          
         MVC   ROWTYPE,RRWTYPE     MOVE  IN    TYPE                             
         MVC   ROWDATA,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,RRWDATLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ROWDATA(0),RRWNDATA                                              
         LA    RE,RRWNDATA+1(RF)   POINT TO END OF RRWNDATA                     
         ICM   RF,1,RRWPFXLN                                                    
         BZ    DISREC16            NO PREFIX TO DISPLAY                         
         BCTR  RF,0                                                             
         EXMVC RF,ROWPRFX,0(RE)                                                 
*                                                                               
DISREC16 LA    R4,ROWLNQ(,R4)                                                   
         MVC   ROW#,RRWSEQ         SAVE NUMBER OF ROWS                          
         GOTO1 NEXTEL                                                           
         BE    DISREC15                                                         
         DROP  R1                                                               
*                                                                               
*                                  ************************************         
*                                  * CREATE HEADING ELEMENTS          *         
*                                  ************************************         
         USING RHDELD,R3                                                        
         SR    R0,R0                                                            
         IC    R0,ROW#                                                          
         LA    R3,APELEM                                                        
         LA    R4,ROWINFO                                                       
         LA    R6,HDLINE1          START SEQUENCE # AT 4                        
*                                                                               
DISREC17 XC    APELEM,APELEM                                                    
         MVI   RHDEL,RHDELQ        X'C1' HEADING ELEMENT                        
         MVI   RHDLN,RHDLNQ        DEFAULT LENGTH                               
         TM    ROWOPT,RRWPAGE      IS IT A HEADLINE?                            
         BZ    DISREC18            NO MORE                                      
         CLI   ROWTYPE,RRWLHEAD    LEFT HEADING                                 
         BNE   *+8                                                              
         MVI   RHDTYPE,RHDLFTH     YES                                          
         CLI   ROWTYPE,RRWRHEAD    RIGHT HEADING                                
         BNE   *+8                                                              
         MVI   RHDTYPE,RHDRHTH     YES                                          
         CLI   ROWTYPE,RRWCHEAD    CENTER HEADING                               
         BNE   *+8                                                              
         MVI   RHDTYPE,RHDCNTR     YES                                          
         STC   R6,RHDSEQ                                                        
         LA    R6,1(,R6)           INCREASE SEQUENCE                            
         STC   R6,HEADROW          SAVE NUMBER OF LAST HEAD ROW                 
         OI    RHDFRM,RHDROW                                                    
         TM    ROWOPT,RRWADR       IS ADDRESS USED? (ADR ATTR)                  
         BZ    *+8                                                              
         OI    RHDFRM,RHDADR       YES                                          
*&&US                                                                           
         TM    ROWOPT3,RRWBDR      IS BUSINESS ADDRESS USED? (BDR ATTR)         
         BZ    *+8                                                              
         OI    RHDFRM,RHDADR                                                    
*&&                                                                             
         CLC   ROWPRFX,SPACES      IS PREFIX USED?                              
         BNH   *+8                                                              
         OI    RHDFRM,RHDPRFX      YES                                          
         GOTO1 ADDEL,(R2)                                                       
         BNE   DISREC99                                                         
         LA    R4,ROWLNQ(,R4)                                                   
         BCT   R0,DISREC17                                                      
         DROP  R3                                                               
*                                                                               
*                                  ************************************         
*                                  * FORMAT HEADING LINES             *         
*                                  ************************************         
         USING RHDELD,R1                                                        
DISREC18 L     R6,AREPAREA                                                      
         ST    R6,CURRLINE         CURRENT REPORT LINE ON                       
         ST    R6,RGHTHEAD                                                      
         ST    R6,LEFTHEAD                                                      
*                                                                               
         LA    R6,HEADTAB                                                       
DISREC20 CLI   0(R6),EOT           END OF TABLE?                                
         BE    DISREC32            FINISHED WITH HEADINGS                       
         CLI   0(R6),RHDRHTH       RIGHT HEADING                                
         BNE   *+10                                                             
         MVC   CURRLINE,RGHTHEAD                                                
         CLI   0(R6),RHDLFTH       LEFT HEADING                                 
         BNE   *+10                                                             
         MVC   CURRLINE,LEFTHEAD                                                
         MVI   APELCODE,RHDELQ     HEADINGS                                     
         MVC   ELEMSEQ,0(R6)       GET SPECIFIC ELEMENT                         
         GOTO1 GETEL,(R2)                                                       
         BE    DISREC23                                                         
*                                                                               
DISREC22 LA    R6,1(,R6)                                                        
         B     DISREC20            CHECK OUT NEXT HEADING                       
*                                                                               
DISREC23 MVI   RHDLINE#,1          ROW  HDR  LINE NUM  1                        
         TM    RHDFRM,RHDROW       IS   IT   A    ROW  HEADER ?                 
         BZ    DISREC24            NO,  CONTINUE                                
*                                                                               
*                                  FORMAT ROW HEADER                            
         SR    R4,R4                                                            
         IC    R4,RHDSEQ           GET SEQUENCE NUMBER                          
         SH    R4,=H'03'           FIGURE OUT WHICH ROW                         
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(2),=C'(R'    FOR ROW                                      
         STC   R4,APWORK+2                                                      
         OI    APWORK+2,X'F0'      TURN IT INTO A CHARACTER                     
         MVI   APWORK+3,C')'                                                    
         BCTR  R4,0                                                             
         MH    R4,=Y(ROWLNQ)       BUMP TO THAT ROW                             
         LA    R4,ROWINFO(R4)                                                   
         LA    RE,APWORK+5                                                      
         TM    RHDFRM,RHDPRFX                                                   
         BZ    DISRC23A                                                         
         MVC   0(L'ROWPRFX,RE),ROWPRFX                                          
         LA    RE,L'ROWPRFX+1(,RE)                                              
*                                                                               
DISRC23A MVC   0(L'ROWDATA,RE),ROWDATA                                          
         LA    RE,APWORK                                                        
         LA    RF,36                                                            
         B     DISREC26                                                         
         DROP  R4                                                               
*                                                                               
*                                  **    EXPAND      DATA DEFINITIONS           
*                                  **    WHICH ARE   USER CONSTANTS             
*                                                                               
DISREC24 TM    RHDFRM,RHDDEF       DATA  DEFINITION  FORMAT ?                   
         BZ    DISREC25                                                         
*                                  CLEAR DUMMY FIELD                            
         XC    DUMMYFH(L'DUMMYFH+L'DUMMYF),DUMMYFH                              
         MVI   DUMMYFH,L'DUMMYFH+L'DUMMYF                                       
         ZIC   R3,RHDLN            GET   HDR   EL    LENGTH                     
         SH    R3,=Y(RHDLNQ)       GET   DATA  LENGTH                           
         STC   R3,DUMMYFH+5        SAVE  DATA  LENGTH                           
         SH    R3,=H'01'           MINUS ONE   FOR   EX                         
         BM    DISREC25            LOW,  SKIP  THE   CHECKS                     
         EXMVC R3,DUMMYF,RHDDATA   SET   UP    FOR   VALIDATION                 
         ST    R1,SAVER1           SAVE  HDR   EL    ADDR                       
         MVI   REPMODE,REPHEAD     VALIDATE    HEADING    KEYWORD               
         GOTO1 VALDEF,DUMMYFH                                                   
         LR    R8,R1               SAVE  TABLE ENTRY                            
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         BNE   DISREC25            NOT   A     KW,   SKIP                       
*                                                                               
         USING DEFTABD,R8                                                       
*                                                                               
*                                  ****  &BLK                                   
*                                                                               
*                                  KEYWORD     =     BLANK ?                    
         CLC   =AL2(AC#RSBLK),DEFDDNUM                                          
         BNE   DISRC24A            NO,   TRY   SPECIAL    CASES                 
*                                                                               
*        CLC   RHDDATA+1(L'AC@BLKN-1),AC@BLKN                                   
*        BNE   DISREC25                                                         
*                                                                               
         SR    R3,R3                                                            
         NI    RHDDATA+L'AC@BLKN,X'0F'   TURN  OFF   HOB'S                      
         IC    R3,RHDDATA+L'AC@BLKN                                             
         MH    R3,REPWDTH                                                       
         A     R3,CURRLINE                                                      
         ST    R3,CURRLINE                                                      
         B     DISREC29            GET   NEXT                                   
*                                                                               
*                                  ****  &OA   AND   &ON                        
*                                                                               
DISRC24A DS    0H                  OFFICE      NAME/ADDRESS ?                   
         CLC   =AL2(AC#RSONN),DEFDDNUM                                          
         BE    DISRC24B            YES,  PROCESS                                
         CLC   =AL2(AC#RSOFA),DEFDDNUM                                          
         BNE   DISRC24G            NO,   CONTINUE                               
*                                                                               
         USING CTIREC,R3                                                        
DISRC24B DS    0H                                                               
         LA    R3,IOKEY                                                         
         XC    CTIKEY,CTIKEY       CLEAR KEY   AREA                             
         MVI   CTIKTYP,CTIKTYPQ    ID    RCR   C'I'                             
         MVC   CTIKNUM,CUUSER      USER  ID    NUMBER                           
         GOTO1 AIO,IO2+IORD+IOCTFILE     READ  RECORD                           
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         BNE   DISREC25            INVALID,    CONTINUE                         
*                                                                               
         L     R3,AIOAREA2         ->    RECORD                                 
         LA    R3,CTIDATA          ->    FIRST ELEMENT                          
         SR    RF,RF               CLEAR REGISTER                               
*                                                                               
DISRC24C DS    0H                                                               
         CLI   0(R3),0             ANY   MORE  DATA ?                           
         BE    DISREC25            NO,   CONTINUE                               
         CLI   0(R3),CTDSTELQ      X'30' ELEMENT ?                              
         BE    DISRC24D            YES,  PROCESS     IT                         
         IC    RF,1(,R3)           GET   EL    LENGTH                           
         AR    R3,RF               ->    NEXT  EL                               
         B     DISRC24C            LOOP  TO    NEXT  ELEMENT                    
*                                                                               
         USING CTDSTD,R3                                                        
*                                                                               
DISRC24D DS    0H                  OFFICE      NAME ?                           
         CLC   =AL2(AC#RSONN),DEFDDNUM                                          
         BNE   DISRC24E            NO,   PROCESS     OFFICE    ADDRESS          
         LA    RF,L'CTDSTNAM-1     GET   LENGTH      OF   NAME                  
         LA    RE,CTDSTNAM         ->    OFFICE      NAME                       
         B     DISREC26            FORMAT      OFFICE     NAME                  
*                                                                               
DISRC24E DS    0H                  PROCESS     OFFICE     ADDRESS               
         LA    RF,L'CTDSTNAM-1     GET   LENGTH      OF   ADDR LINE             
         LA    RE,CTDSTADD         ->    OFFICE      ADDR LINE 1                
         CLI   CTDSTLEN,166        MORE  THAN  ONE   ADDR LINE ?                
         BNE   DISREC26            NO,   FORMAT      OFFC ADDR LINE             
         CLI   RHDLINE#,1          FIRST ADDR  LINE ?                           
         BNE   DISRC24F            NO,   SKIP                                   
         MVI   RHDLINE#,2          SAY   TWO   ADDR  LINES                      
         B     DISREC26            FORMAT      OFFICE     ADDR LINE             
*                                                                               
DISRC24F DS    0H                  2ND   ADDR  LINE                             
         MVI   RHDLINE#,1          SAY   LAST  ADDR  LINE                       
         LA    RE,CTDSTAD2         ->    OFFICE      ADDR LINE 2                
         B     DISREC26            FORMAT      OFFICE     ADDR LINE             
         DROP  R3                                                               
*                                                                               
*                                  ****  &AA   AND   &AN                        
*                                                                               
DISRC24G DS    0H                  AGENCY      NAME/ADDRESS ?                   
         CLC   =AL2(AC#RSAGN),DEFDDNUM                                          
         BE    DISRC24H            YES,  PROCESS                                
         CLC   =AL2(AC#RSAGA),DEFDDNUM                                          
         BNE   DISRC24J            NO,   CONTINUE                               
*                                                                               
DISRC24H DS    0H                                                               
         MVC   IOKEY,SPACES        CLEAR KEY   AREA                             
*                                  COMPANY     CODE                             
         MVC   IOKEY(L'CUABIN),CUABIN                                           
         GOTO1 AIO,IO2+IORD+IOACCFIL     READ  RECORD                           
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         BNE   DISREC25            INVALID,    CONTINUE                         
*                                                                               
         L     R3,AIOAREA2         ->    RECORD                                 
*                                  AGENCY      NAME                             
         CLC   =AL2(AC#RSAGN),DEFDDNUM                                          
         BNE   DISRC24I            NO,   PROCESS     AGENCY    ADDR             
         MVI   APELCODE,NAMELQ     X'20' NAME  ELEMENT                          
         GOTO1 GETEL,(R3)          GET   NAME  ELEMENT                          
         LR    R3,R1                                                            
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         MVI   APELCODE,RHDELQ     RESTORE     FOR   NEXTEL                     
         BNE   DISREC25            INVALID,    CONTINUE                         
*                                                                               
         USING NAMELD,R3                                                        
         LA    RE,NAMEREC          ->    AGENCY      NAME                       
         ZIC   RF,NAMLN            GET   ELEMENT     LENGTH                     
         SH    RF,=Y(NAMLN1Q+1)    GET   EXMVC LENGTH                           
         B     DISREC26            FORMAT      AGENCY     NAME LINE             
         DROP  R3                                                               
*                                                                               
DISRC24I DS    0H                  PROCESS     AGENCY     ADDRESS               
         MVI   APELCODE,ADRELQ     X'22' ADDR  ELEMENT                          
         GOTO1 GETEL,(R3)          GET   NAME  ELEMENT                          
         LR    R3,R1                                                            
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         MVI   APELCODE,RHDELQ     RESTORE     FOR   NEXTEL                     
         BNE   DISREC25            INVALID,    CONTINUE                         
*                                                                               
         USING ADRELD,R3                                                        
         LA    RE,ADRADD1          ->    AGENCY      ADDR LINE 1                
         ZIC   RF,RHDLINE#         GET   ADDR  LINE  NUM  TO   FORMAT           
         BCTR  RF,0                GET   ADDR  OF    ADDR LINE                  
         MH    RF,=Y(L'ADRADD1)                                                 
         AR    RE,RF                                                            
         ZIC   RF,RHDLINE#         GET   NEXT  ADDR  LINE NUMBER                
         LA    RF,1(,RF)                                                        
         STC   RF,RHDLINE#         SAVE  NEXT  ADDR  LINE NUMBER                
         CLM   RF,1,ADRNUM         LAST  ADDR  LINE ?                           
         BNH   *+8                 NO,   FORMAT      AGENCY    ADDR LIN         
         MVI   RHDLINE#,1          SAY   LAST  ADDR  LINE                       
         LA    RF,L'ADRADD1-1      GET   ELEMENT     LENGTH                     
         B     DISREC26            FORMAT      AGENCY     ADDR LINE             
*                                                                               
*                                  ****  &FMT  AND   &FMTN                      
*                                                                               
DISRC24J DS    0H                  FORMAT      CODE ?                           
         CLC   =AL2(AC#RSFTC),DEFDDNUM                                          
         BNE   DISRC24L            NO,   CONTINUE                               
         LA    RE,PRVCODE          ->    FORMAT      CODE                       
         LA    R3,PRVCODE+L'PRVCODE-1    LAST  CHAR  IN   CODE                  
         LA    RF,L'PRVCODE-1      MAX   LENGTH      OF   CODE                  
*                                                                               
DISRC24K DS    0H                                                               
         CLI   0(R3),C' '          SPACE OR    NULL ?                           
         BH    DISREC26            NO,   OUTPUT      FORMAT    CODE             
         BCTR  RF,0                TEST  THE   PREVIOUS   CHARACTER             
         BCT   R3,DISRC24K                                                      
*                                                                               
DISRC24L DS    0H                  FORMAT      NAME ?                           
         CLC   =AL2(AC#RSFTN),DEFDDNUM                                          
         BNE   DISRC24N            NO,   CONTINUE                               
         LA    RE,PRVNME           ->    FORMAT      NAME                       
         LA    R3,PRVNME+L'PRVNME-1      LAST  CHAR  IN   NAME                  
         LA    RF,L'PRVNME-1       MAX   LENGTH      OF   NAME                  
         LA    R0,L'PRVNME                                                      
*                                                                               
DISRC24M DS    0H                                                               
         CLI   0(R3),C' '          SPACE OR    NULL ?                           
         BH    DISREC26            NO,   OUTPUT      FORMAT    NAME             
         BCTR  RF,0                TEST  THE   PREVIOUS   CHARACTER             
         BCTR  R3,0                                                             
         BCT   R0,DISRC24M                                                      
         B     DISREC25            OUTPUT      &FMTN                            
*                                                                               
*                                  ****  &TD                                    
*                                                                               
DISRC24N DS    0H                  REQUEST     DATE ?                           
         CLC   =AL2(AC#RSTDY),DEFDDNUM                                          
         BNE   DISRC24O            NO,   CONTINUE                               
         MVC   TODAY,SPACES        CLEAR DATE                                   
         GOTO1 VDATCON,APPARM,(5,0),(8,TODAY)                                   
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         LA    RE,TODAY            ->    TODAY'S     DATE                       
         LA    RF,L'TODAY-1        LENGTH      OF    DATE FIELD                 
         B     DISREC26            OUTPUT      TODAY'S    DATE                  
*                                                                               
DISRC24O DS    0H                  REQUESTOR   NAME                             
         CLC   =AL2(AC#RSRQR),DEFDDNUM                                          
         BNE   DISRC24P            NO,   CONTINUE                               
         GOTO1 AFVAL,PRVOWNH       GET   PERSON      NAME                       
         L     R1,SAVER1           RESTORE     HDR   EL   ADDR                  
         BNE   DISREC25            NONE, CONTINUE                               
         LA    RE,PRVOWN           ->    REQUESTOR   NAME                       
         ZIC   RF,FVILEN           GET   LENGTH                                 
         B     DISREC26            OUTPUT      REQUESTOR  NAME                  
         DROP  R8                                                               
*                                                                               
DISRC24P DS    0H                  RESERVED    FOR   MORE HEADERS               
*                                  **    NOT   A     CONSTANT                   
*                                                                               
DISREC25 SR    RF,RF               GET LENGTH OF FREE FORM DATA                 
         IC    RF,RHDLN                                                         
         SH    RF,=Y(RHDLNQ+1)                                                  
         BM    DISREC29                                                         
         LA    RE,RHDDATA                                                       
*                                                                               
*                                  **    GENERAL     HEADING    LINES           
*                                                                               
DISREC26 L     R3,CURRLINE         GET CURRENT LINE TO PRINT ON                 
         LR    R4,R3               SAVE R3                                      
         CLI   RHDTYPE,RHDRHTH     RIGHT HEADING                                
         BNE   DISRC26A                                                         
         AH    R3,REPWDTH                                                       
         SH    R3,=H'36'                                                        
*                                                                               
DISRC26A EXMVC RF,0(R3),0(RE)                                                   
         CLI   PRTCASE,C'L'        LOWER CASE?                                  
         BE    DISREC27            YES, LEAVE AS IS                             
         L     R8,AUPCASE          MAKE UPPER CASE                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    0(0,R3),0(R8)                                                    
*                                                                               
DISREC27 LR    R3,R4               RESTORE R3                                   
         LH    R4,REPWDTH                                                       
         CLI   RHDTYPE,RHDTITL     TITLE                                        
         BE    *+8                                                              
         CLI   RHDTYPE,RHDCNTR     CENTER (1,2,3)                               
         BNE   DISREC28            DON'T CENTER                                 
         ST    R1,SAVER1           SAVE R1=ELEMENT LOCATION                     
         GOTO1 VCENTER,APPARM,(R3),(R4)                                         
         L     R1,SAVER1           RESTORE R1=ELEMENT LOCATION                  
*                                                                               
DISREC28 AH    R3,REPWDTH          BUMP DOWN A LINE                             
         ST    R3,CURRLINE                                                      
*                                                                               
DISREC29 CLI   RHDTYPE,RHDRHTH     RIGHT HEADING                                
         BNE   *+10                                                             
         MVC   RGHTHEAD,CURRLINE                                                
         CLI   RHDTYPE,RHDLFTH     LEFT HEADING                                 
         BNE   *+10                                                             
         MVC   LEFTHEAD,CURRLINE                                                
         CLI   RHDTYPE,RHDTITL     TITLE                                        
         BE    *+8                                                              
         CLI   RHDTYPE,RHDCNTR     CENTER (1,2,3)                               
         BNE   DISREC30                                                         
         MVC   RGHTHEAD,CURRLINE   MOVE UP RIGHT AND LEFT HEAD                  
         MVC   LEFTHEAD,CURRLINE                                                
*                                                                               
DISREC30 CLI   RHDLINE#,1          ONLY ROW  HDR  LINE TO   FORMAT ?            
         BNE   DISREC24            NO,  GET  NEXT LINE                          
         GOTO1 NEXTEL                                                           
         BNE   DISREC22                                                         
         CLC   RHDTYPE,0(R6)                                                    
         BNE   DISREC22                                                         
         B     DISREC23                                                         
*                                                                               
DISREC32 MVC   CURRLINE,LEFTHEAD                                                
         CLC   RGHTHEAD,LEFTHEAD                                                
         BL    *+10                                                             
         MVC   CURRLINE,RGHTHEAD                                                
*                                                                               
         L     R6,CURRLINE                                                      
         AH    R6,REPWDTH          BUMP UP A LINE                               
         ST    R6,TOPLINE                                                       
         AH    R6,REPWDTH          BUMP UP A LINE                               
         ST    R6,CURRLINE                                                      
*                                                                               
*                                  ************************************         
*                                  * FORMAT COLUMN HEADERS            *         
*                                  ************************************         
         USING RCLELD,R2                                                        
         USING COLINFOD,R3                                                      
*                                                                               
         CLI   ANYCOLS,C'N'        ANY   COLS  TO    OUTPUT ?                   
         BE    DISRC53A            NO,   SKIP                                   
         LA    R3,COLARRAY                                                      
         MVI   APELCODE,RCLELQ     X'C3' COLUMN      ELEMENT                    
         MVC   PREVHEAD,SPACES                                                  
         XC    PREVLOC,PREVLOC                                                  
         XC    PREVSIZE,PREVSIZE                                                
         MVI   CHUNK,NO                                                         
         GOTO1 GETEL,(R2)                                                       
         BNE   DISRC53A            NO    ROWS, SKIP                             
*                                                                               
DISREC35 LR    R2,R1                                                            
         MVC   HEAD1,SPACES                                                     
         MVC   HEAD2,SPACES                                                     
         TM    RCLOPT,RCLHIDE      HIDDEN      COLUMN ?                         
         BO    DISREC51            YES,  SKIP                                   
         CLI   RCLSTACK,0          STACKED     COLUMN ?                         
         BNE   DISREC51            YES,  SKIP                                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF RCLNDATA                           
         LA    RE,RCLNDATA(RF)     POINT TO START OF HEADINGS                   
         ICM   RF,1,RCLHD1LN       LENGTH OF HEADING 1                          
         BZ    DISREC36                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   HEAD1(0),0(RE)      MOVE IN HEADING ONE                          
         LA    RE,1(RF,RE)         POINT TO NEXT HEADING                        
*                                                                               
DISREC36 ICM   RF,1,RCLHD2LN       LENGTH OF HEADING 2                          
         BZ    DISREC38                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   HEAD2(0),0(RE)      MOVE IN HEADING ONE                          
*                                                                               
DISREC38 MVC   APHALF,CIBOXCOL     BOXES   COLUMN                               
         NI    APHALF,TURNOFF-X'80'                                             
         LH    R6,APHALF           GET     DISPLACEMENT                         
*                                                                               
DISREC40 A     R6,CURRLINE         POINT TO PRINT AREA                          
         LA    R6,1(,R6)                                                        
         SR    R4,R4                                                            
         ICM   R4,1,RCLWDTH        GET COLUMN WIDTH                             
         BZ    DISREC51            IF ZERO IGNORE                               
         ST    R4,COLSIZE          SAVE COLUMN SIZE                             
         CLI   RCLWDTH,L'HEAD1     MAX LENGTH OF HEADINGS                       
         BL    *+8                                                              
         LA    R4,L'HEAD1                                                       
         MVC   TMPCWDTH,RCLHD1LN                                                
         CLC   TMPCWDTH,RCLHD2LN   GET THE LARGER OF THE TWO                    
         BNL   *+10                                                             
         MVC   TMPCWDTH,RCLHD2LN                                                
         CLI   TMPCWDTH,0          IF NO HEADINGS THEN BRANCH                   
         BE    DISREC41                                                         
         CLC   RCLWDTH,TMPCWDTH    IS WIDTH < THEN DATA LENGTH                  
         BL    *+8                 NO SO OK                                     
         IC    R4,TMPCWDTH         YES SO TAKE DATA SIZE                        
         BCTR  R4,0                ONE FOR EXECUTED MOVE                        
         B     DISREC42                                                         
*                                                                               
DISREC41 BCTR  R4,0                                                             
         TM    RCLOPT,RCLACCM      MAKE HEADINGS FOR $ COLUMNS                  
         BZ    DISREC51                                                         
         TM    RCLOPT,RCLEQU       BUT NOT CALCULATED COLUMNS                   
         BO    DISREC51                                                         
         TM    RCLDTEFG,RCLDAY     IS PERIOD DAY RANGE?                         
         BZ    *+8                                                              
         BAS   RE,DAYBLD                                                        
         TM    RCLDTEFG,RCLMON                                                  
         BZ    *+8                                                              
         BAS   RE,MONBLD                                                        
         TM    RCLDTEFG,RCLQTR                                                  
         BZ    *+8                                                              
         BAS   RE,QTRBLD                                                        
         TM    RCLDTEFG,RCLYEAR                                                 
         BZ    *+8                                                              
         BAS   RE,YRBLD                                                         
*        TM    RCLDTEFG,RCLPERD                                                 
*        BZ    *+8                                                              
*        BAS   RE,PRDBLD                                                        
         CLI   HEAD1,ESCHIGHQ                                                   
         BNL   DISREC42                                                         
         GOTO1 VDICTAT,APPARM,C'SU  ',HEAD1,0                                   
*                                                                               
DISREC42 XC    ADJUST,ADJUST                                                    
         CLI   PRTBOX,NO                                                        
         BE    DISREC43                                                         
         CLC   HEAD1,SPACES                                                     
         BNH   DISREC43                                                         
         CLC   HEAD1,PREVHEAD                                                   
         BE    DISREC44                                                         
*                                                                               
DISREC43 MVI   CHUNK,NO            RESET CHUNK SIZE (FLAG)                      
         MVC   PREVSIZE,COLSIZE+2                                               
         ST    R6,PREVLOC          SAVE NEW ADDRESS LOACATION                   
         MVC   PREVHEAD,HEAD1                                                   
         MVC   TEMPHEAD,HEAD1                                                   
         OC    TEMPHEAD,HEAD2                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DISREC45                                                         
         CLI   PRTBOX,YES                                                       
         BE    DISREC45                                                         
*                                                                               
         LA    RE,TEMPHEAD+L'TEMPHEAD-1                                         
         LA    RF,L'TEMPHEAD                                                    
DISRC43A CLI   0(RE),C' '                                                       
         BNE   DISRC43B                                                         
         BCTR  RE,0                                                             
         BCT   RF,DISRC43A                                                      
*                                                                               
DISRC43B STH   RF,APHALF                                                        
         L     RF,COLSIZE                                                       
         SH    RF,APHALF                                                        
         BM    DISREC45                                                         
         STH   RF,ADJUST                                                        
         B     DISREC45                                                         
*                                                                               
DISREC44 LH    RF,PREVSIZE                                                      
         A     RF,COLSIZE                                                       
         L     RE,PREVLOC                                                       
         EXMVC RF,0(RE),SPACES                                                  
         LA    RF,1(RF)                                                         
         STH   RF,PREVSIZE                                                      
         OI    CIBOXCOL,X'80'      TURN OFF BOX FOR ONE PASS                    
         MVI   CHUNK,YES                                                        
*                                                                               
DISREC45 CLC   HEAD1,SPACES                                                     
         BNH   DISREC49            SPACES                                       
         LR    RE,R6                                                            
         L     RF,COLSIZE                                                       
         CLI   CHUNK,NO                                                         
         BE    DISREC46                                                         
         L     RE,PREVLOC                                                       
         LH    RF,PREVSIZE                                                      
*                                                                               
DISREC46 CLI   PRTCASE,C'U'                                                     
         BNE   DISRC46A                                                         
         L     R8,AUPCASE          MAKE UPPER CASE                              
         TR    HEAD1,0(R8)                                                      
*                                                                               
DISRC46A AH    RE,ADJUST                                                        
         EXMVC R4,0(RE),HEAD1                                                   
         CLI   PRTBOX,NO                                                        
         BE    DISREC49                                                         
         GOTO1 VCENTER,APPARM,(RE),(RF)                                         
         CLI   CHUNK,YES                                                        
         BNE   DISREC49                                                         
         LH    RF,PREVSIZE                                                      
         L     RE,PREVLOC                                                       
*                                                                               
DISREC47 CLI   0(RE),C' '                                                       
         BH    DISREC48                                                         
         MVI   0(RE),C'-'                                                       
         LA    RE,1(,RE)                                                        
         BCT   RF,DISREC47                                                      
         B     DISREC49                                                         
*                                                                               
DISREC48 LH    RF,PREVSIZE                                                      
         L     RE,PREVLOC                                                       
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
*                                                                               
DISRC48A CLI   0(RE),C' '                                                       
         BH    DISREC49                                                         
         MVI   0(RE),C'-'                                                       
         BCTR  RE,0                                                             
         BCT   RF,DISRC48A                                                      
*                                                                               
DISREC49 AH    R6,REPWDTH                                                       
         AH    R6,ADJUST                                                        
         CLC   HEAD2,SPACES                                                     
         BNH   DISREC50                                                         
         CLI   PRTCASE,C'U'                                                     
         BNE   DISRC49A                                                         
         L     R8,AUPCASE                                                       
         TR    HEAD2,0(R8)                                                      
*                                                                               
DISRC49A EXMVC R4,0(R6),HEAD2                                                   
         CLI   PRTBOX,NO                                                        
         BE    DISREC50                                                         
         GOTO1 VCENTER,APPARM,(R6),COLSIZE                                      
*                                                                               
DISREC50 CLI   PRTBOX,NO                                                        
         BNE   DISREC51                                                         
         CLC   TEMPHEAD,SPACES                                                  
         BNH   DISREC51                                                         
         AH    R6,REPWDTH                                                       
         L     RF,COLSIZE                                                       
         SH    RF,ADJUST                                                        
         CH    RF,=Y(L'TEMPHEAD)                                                
         BL    *+8                                                              
         LA    RF,L'TEMPHEAD                                                    
         GOTO1 VUNDERLN,APPARM,((RF),TEMPHEAD),(R6)                             
*                                                                               
DISREC51 LA    R3,CILNQ(,R3)       BUMP TO NEXT COLUMN                          
         GOTO1 NEXTEL,(R2)                                                      
         BE    DISREC35                                                         
         DROP  R2,R3                                                            
*                                                                               
*                                  ************************************         
*                                  * FORMAT BOXES                     *         
*                                  ************************************         
         L     R2,AIOAREA1                                                      
         L     R6,CURRLINE         TOP OF HEADINGS                              
         AH    R6,REPWDTH          BOUNCE DOWN ONE                              
         CLI   PRTBOX,NO           ANY BOXES NO SO SKIP                         
         BE    DISREC53                                                         
         L     R6,TOPLINE          ONE LINE ABOVE HEADINGS                      
         MVC   CURRLINE,TOPLINE                                                 
         BAS   RE,BOXTOP           TOP BOX LINE                                 
         AH    R6,REPWDTH                                                       
         ST    R6,CURRLINE                                                      
         BAS   RE,BOXIT                                                         
         AH    R6,REPWDTH                                                       
         ST    R6,CURRLINE                                                      
         LH    R1,REPWDTH                                                       
         EXCLC R1,0(R6),SPACES                                                  
         BE    DISREC52                                                         
         BAS   RE,BOXIT                                                         
         AH    R6,REPWDTH                                                       
         ST    R6,CURRLINE                                                      
*                                                                               
DISREC52 BAS   RE,BOXTOP                                                        
*                                                                               
*                                  ************************************         
*                                  * FIND NUMBER OF LINES FORMATTED   *         
*                                  ************************************         
DISREC53 AH    R6,REPWDTH                                                       
         ST    R6,CURRLINE                                                      
         ST    R6,TOPLINE                                                       
*                                                                               
DISRC53A L     R4,AREPAREA                                                      
         SR    R6,R4                                                            
         LA    R1,1                                                             
*                                                                               
DISREC54 SH    R6,REPWDTH                                                       
         BNP   DISREC55                                                         
         LA    R1,1(,R1)           COUNT NUMBER OF LINES USED                   
         B     DISREC54                                                         
*                                                                               
*                                  ************************************         
*                                  * FORMAT FOOTING LINES             *         
*                                  ************************************         
DISREC55 SR    R4,R4                                                            
         IC    R4,LASTROW                                                       
         AH    R4,=Y(NROWS-4)                                                   
         SR    R4,R1               NUMBER OF ROW LEFT                           
         STH   R4,NLINES           N LINES AVAILABLE                            
         LR    R1,R4               FIGURE OUT WHERE FOOT LINE GOES              
         LA    R1,1(,R1)                                                        
         MH    R1,REPWDTH                                                       
         A     R1,CURRLINE                                                      
         ST    R1,FOOTLINE                                                      
*                                                                               
         USING RHDELD,R1                                                        
DISREC56 L     R1,AIOAREA1                                                      
         L     R2,FOOTLINE                                                      
         ST    R2,CURRLINE                                                      
         MVI   APELCODE,RHDELQ     X'C1'                                        
         MVI   ELEMSEQ,RHDFTLN     05 FOOT LINE                                 
         GOTO1 GETEL                                                            
*                                                                               
DISREC57 BNE   DISREC60                                                         
         CLI   RHDTYPE,RHDFTLN                                                  
         BNE   DISREC60                                                         
         SR    RF,RF                                                            
         IC    RF,RHDLN                                                         
         SH    RF,=Y(RHDLNQ+1)                                                  
         BM    DISREC58                                                         
         EXMVC RF,0(R2),RHDDATA                                                 
         AH    R2,REPWDTH                                                       
         STH   R2,CURRLINE                                                      
*                                                                               
DISREC58 GOTO1 NEXTEL                                                           
         BE    DISREC57                                                         
         DROP  R1                                                               
*                                                                               
*                                  ************************************         
*                                  * ADJUST BOX COLUMNS               *         
*                                  ************************************         
DISREC60 CLI   ANYCOLS,C'N'        ANY   COLS  TO    OUTPUT ?                   
         BE    DISREC65            NO,   SKIP                                   
         SR    R1,R1                                                            
         IC    R1,COL#                                                          
         LR    R0,R1                                                            
         BCTR  R1,0                ->    LAST  USED  BOX  COL  -1               
         MH    R1,=Y(CILNQ)                                                     
         LA    R3,COLARRAY(R1)                                                  
         LA    R4,CILNQ(,R3)       ->    LAST  USED  COL  WIDTH                 
*                                                                               
LCOLM1   USING COLINFOD,R3         LAST  COL   MINUS 1                          
LCOL     USING COLINFOD,R4         LAST  COLUMN                                 
*                                                                               
         XC    APHALF,APHALF                                                    
DISREC62 SR    RF,RF                                                            
         TM    LCOL.CICOLOPT,CIACCM      CALCULATED  COLUMN ?                   
         BO    DISREC63            YES,  SKIP                                   
         IC    RF,LCOL.CICOLWD     GET   COL   WIDTH                            
         AH    RF,APHALF           ADD   PREVIOUS    COLUMN   (MAYBE)           
         STC   RF,LCOL.CICOLWD                                                  
         LA    RF,1(,RF)                                                        
         TM    LCOLM1.CIBOXCOL,X'80'     IS    IT    CHUNKED ?                  
         BO    DISREC63                  YES,  SKIP                             
         CLI   PRTCWRAP,YES                                                     
         BNE   DISREC63                                                         
         SR    RF,RF                                                            
*                                                                               
DISREC63 STH   RF,APHALF                                                        
*                                                                               
         SH    R3,=Y(CILNQ)        ->    PREVIOUS    COLUMN                     
         SH    R4,=Y(CILNQ)        ->    PREVIOUS    COLUMN                     
         BCT   R0,DISREC62                                                      
         DROP  LCOL,LCOLM1                                                      
*                                                                               
         USING COLINFOD,R4                                                      
         ZIC   R0,COL#                                                          
         LA    R4,COLARRAY         ->    1ST   COLUMN                           
*                                                                               
DISREC64 LA    R4,CILNQ(,R4)       ->    NEXT  COLUMN                           
         CLI   CICOLWD,0           HIDDEN/STACKED   COLUMN ?                    
         BE    DISRC64A            YES,  SKIP                                   
         TM    CICOLOPT,CIACCM     ACCUMULATED COLUMN ?                         
         BZ    DISRC64B            NO,   MAKE  ADJUSTMENT                       
*                                                                               
DISRC64A BCT   R0,DISREC64         TRY   NEXT  COLUMN                           
         B     DISREC65            NONE, SKIP                                   
*                                                                               
DISRC64B MVC   MIDSIZE,CICOLWD     SAVE  COL   WIDTH                            
         DROP  R4                                                               
*                                                                               
*                                  ************************************         
*                                  * FORMAT MID-LINES                 *         
*                                  ************************************         
         USING ROWD,R4                                                          
DISREC65 L     R6,TOPLINE                                                       
         AH    R6,REPWDTH                                                       
         ST    R6,CURRLINE                                                      
         SR    R4,R4                                                            
         IC    R4,HEADROW          GO    TO    FIRST POSSIBLE MID-LINE          
         SH    R4,=H'04'                                                        
         LR    R0,R4                                                            
         MH    R4,=Y(ROWLNQ)       BUMP  TO    THAT  ROW                        
         LA    R4,ROWINFO(R4)                                                   
*                                                                               
DISREC68 CLI   ROWTYPE,0                                                        
         BE    DISREC69            NONE  OR    NO    MORE                       
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(2),=C'(R'    FOR   ROW                                    
         AH    R0,=H'01'                                                        
         CLM   R0,1,ROW#           ALL   ROWS  COMPLETED                        
         BH    DISREC69            YES,  CONTINUE                               
         STC   R0,APWORK+2                                                      
         OI    APWORK+2,X'F0'      TURN IT INTO A CHARACTER                     
         MVI   APWORK+3,C')'                                                    
         MVC   APWORK+5(L'ROWDATA),ROWDATA                                      
         GOTO1 VSQUASH,APPARM,APWORK,L'APWORK                                   
         L     R2,APPARM+4         GET SIZE                                     
         L     R3,CURRLINE                                                      
         AH    R3,MIDSTART                                                      
         GOTO1 VCHOPPER,APPARM,((R2),APWORK),(MIDSIZE,(R3)),           X        
               (REPWDTH+1,3),0,0                                                
         L     R2,APPARM+8                                                      
         LH    R1,NLINES           TOTAL LINES LEFT - LINES TO USE              
         SR    R1,R2                                                            
         SH    R1,=H'02'                                                        
         BM    DISREC90            NO MORE ROOM                                 
         STH   R1,NLINES                                                        
*                                                                               
         MH    R2,REPWDTH          R2= # OF LINES TO BUMP FOR UNDERLINE         
         L     R6,CURRLINE                                                      
         AR    R6,R2               STORE WHERE NEXT LINE IS TO BE               
         AH    R6,REPWDTH                                                       
         AH    R6,REPWDTH                                                       
         ST    R6,CURRLINE                                                      
*                                                                               
         AR    R3,R2               POINT TO LAST LINE (UNDERLINE)               
         LR    R2,R3               SAVE IT IN R2                                
         SH    R2,REPWDTH          UNDERLINE LAST LINE                          
         GOTO1 VUNDERLN,APPARM,(MIDSIZE,(R2)),(R3)                              
*                                                                               
         SR    R1,R1               INDENT NEXT MID-LINE                         
         IC    R1,MIDSIZE                                                       
         CLM   R1,1,=AL1(10)                                                    
         BL    DISRC68A                                                         
         BCTR  R1,0                                                             
         STC   R1,MIDSIZE                                                       
         LH    R1,MIDSTART                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,MIDSTART                                                      
*                                                                               
DISRC68A LA    R4,ROWLNQ(,R4)                                                   
         B     DISREC68                                                         
         DROP  R4                                                               
*                                                                               
*                                  ************************************         
*                                  * FORMAT STACK UNDER COLUMNS       *         
*                                  ************************************         
DISREC69 CLI   STKUNDMX,0          STACK UNDER USED ?                           
         BE    DISREC70            NO,   SKIP                                   
         LA    R1,1                START WITH  LEVEL ONE                        
*                                                                               
         USING COLINFOD,R2         MAP   COLUMNS     INFORMATION                
PREV     USING COLINFOD,R4         MAP   COLUMNS     INFORMATION                
*                                                                               
DISRC69A DS    0H                                                               
         L     R6,CURRLINE         ->    FORMAT      AREA                       
         LA    R2,COLARRAY+CILNQ   ->    COLUMN      INFORMATION                
         SR    R3,R3               GET   NUM   OF    COLUMNS                    
*                                                                               
DISRC69C DS    0H                                                               
         LA    R3,1(,R3)           NEXT  COLUMN                                 
         CLM   R3,1,COL#           ALL   COLS  CHECKED ?                        
         BH    DISRC69Q            YES,  BUMP  TO    NEXT LINE                  
         TM    CICOLOPT,CIHIDE     HIDDEN      COLUMN ?                         
         BO    DISRC69I            YES,  LOOP  TO    NEXT COLUMN                
         CLM   R1,1,CISTKULV       STACK UNDER LEVEL =    LINE NUMBER ?         
         BNE   DISRC69I            NO,   LOOP  TO    NEXT COLUMN                
*                                                                               
         ZIC   R4,CISTKUCL         GET   STACK UNDER COL  INFORMATION           
         MH    R4,=Y(CILNQ)                                                     
         LA    R4,COLARRAY(R4)                                                  
*                                                                               
         LH    RE,PREV.CIBOXCOL    GET   ADDR  OF    AREA TO   BUILD            
         ZIC   RF,PREV.CICOLWD           THE   STACKED    COL  DATA             
         SR    RE,RF                                                            
         AR    RE,R6                                                            
*                                                                               
         SR    RF,RF               GET   HEAD1 WIDTH                            
         ICM   RF,1,CISTKUHW       IS    IT    ZERO ?                           
         BZ    DISRC69E            YES,  SKIP                                   
         CLM   RF,1,PREV.CICOLWD   WILL  HEAD1 FIT ?                            
         BNH   *+8                 YES,  CONTINUE                               
         IC    RF,PREV.CICOLWD     NO,   USE   AS    MANY AS   FITS             
         BCTR  RF,0                MOVE  HEAD1                                  
         EXMVC RF,0(RE),CISTKUH1         DATA                                   
*                                                                               
DISRC69E ZIC   R8,PREV.CISTKUHW    GET   MAX   HEAD1 SIZE                       
         LA    R8,1(,R8)           PLUS  1     SPACE                            
         CLM   R8,1,PREV.CICOLWD   WILL  ANY   MORE  FIT ?                      
         BNL   DISRC69I            NO,   DONE                                   
*                                                                               
         ZIC   RF,CISTKUWD         GET   KEYWD/EQUN  WIDTH                      
         LA    RE,0(R8,RF)         PLUS  HEAD1                                  
         CLM   RE,1,PREV.CICOLWD   WILL  KEYWD/EQUN  FIT ?                      
         BNH   DISRC69G            YES,  CONTINUE                               
         IC    RF,PREV.CICOLWD     GET   MAX   SPACE AVAILABLE                  
*                                  MINUS SPACE USED  GIVING                     
         SR    RF,R8                     SPACE TO    BE   USED                  
*                                                                               
DISRC69G LH    RE,PREV.CIBOXCOL    GET   OFFSET      FOR  THIS                  
         ZIC   R0,PREV.CICOLWD           STACKED     COL  DATA                  
         SR    RE,R0                                                            
         AR    RE,R8               ADD   OFFSET FOR  MAX  HEAD1                 
         AR    RE,R6               ADD   ADDR   OF   LINE                       
         BCTR  RF,0                MOVE  KEYWORD/EQUATION                       
         EXMVC RF,0(RE),CISTKUKW         DATA                                   
*                                                                               
DISRC69I DS    0H                                                               
         LA    R2,CILNQ(,R2)       NEXT  COL   ELEMENT                          
         B     DISRC69C            LOOP  TO    NEXT  COLUMN                     
         DROP  PREV                                                             
*                                                                               
DISRC69Q AH    R6,REPWDTH          BUMP  TO    NEXT  LINE                       
         ST    R6,CURRLINE         SAVE  NEXT  LINE  ADDRESS                    
         LH    RF,NLINES           GET   NUM   OF    LINES     LEFT             
         SH    RF,=H'01'           ACCOUNT     FOR   LINE                       
         STH   RF,NLINES           SAVE  NUM   OF    LINES     LEFT             
         BNP   DISREC90            NONE  LEFT, STOP  ADDING    LINES            
         LA    R1,1(,R1)           GET   NEXT  LEVEL                            
         CLM   R1,1,STKUNDMX       ANY   MORE  TO    STACK ?                    
         BNH   DISRC69A            YES,  LOOP  FOR   NEXT LINE                  
         AH    R6,REPWDTH          BUMP  AN    EXTRA LINE                       
         ST    R6,CURRLINE         SAVE  NEXT  LINE  ADDRESS                    
         DROP  R2                                                               
*                                                                               
*                                  ************************************         
*                                  * FORMAT COLUMN TOTAL LINES        *         
*                                  ************************************         
DISREC70 CLI   ANYCOLS,C'N'        ANY   COLS  TO    OUTPUT ?                   
         BE    DISREC80            NO,   SKIP                                   
         LH    R0,NLINES                                                        
         SR    R3,R3                                                            
         IC    R3,COL#                                                          
         LR    R4,R3               ->    LAST  USED  COL  WIDTH                 
         MH    R4,=Y(CILNQ)                                                     
         LA    R4,COLARRAY(R4)     ->    LAST  USED  COL  INFORMATION           
*                                                                               
LCOL     USING COLINFOD,R4         LAST  COLUMN                                 
*                                                                               
DISREC72 TM    LCOL.CICOLOP2,CITOT TOTAL ON    COLUMN ?                         
         BZ    DISREC75            NO,   SKIP                                   
         LR    RF,R4               ->    COL   MINUS 1                          
         SH    RF,=Y(CILNQ)                                                     
*                                                                               
LCOLM1   USING COLINFOD,RF         LAST  COL   MINUS 1                          
         L     R6,CURRLINE                                                      
         MVC   APHALF,LCOLM1.CIBOXCOL    BOXES COLUMN                           
         DROP  LCOLM1                                                           
*                                                                               
         NI    APHALF,TURNOFF-X'80'      CHUNKING    BIT                        
         AH    R6,APHALF                                                        
         LA    R6,1(,R6)                                                        
         SR    RF,RF                                                            
         IC    RF,LCOL.CICOLWD     GET   COL   WIDTH                            
         GOTO1 TOTFOR,APPARM,((RF),(R6)),(C'C',(R3))                            
         BNE   DISREC90                                                         
         L     R6,APPARM           NUM   OF    LINES USED                       
         MH    R6,REPWDTH                                                       
         A     R6,CURRLINE                                                      
         ST    R6,CURRLINE                                                      
*                                                                               
DISREC75 SH    R4,=Y(CILNQ)        GET   PREVIOUS    COLUMN                     
         BCT   R3,DISREC72         ONE   LESS  COLUMN                           
         DROP  LCOL                                                             
*                                                                               
*                                  ************************************         
*                                  * FORMAT TOTAL FOR ROW LINES       *         
*                                  ************************************         
         USING ROWD,R4                                                          
*                                                                               
DISREC80 CLI   ANYNACMC,C'N'       ANY   NON-ACCUMULATED  COLUMNS ?             
         BE    DISREC90            NO,   SKIP  TOTALS                           
         SR    R3,R3                                                            
         ICM   R3,1,ROW#           NUM   OF    ROWS                             
         BZ    DISREC90                                                         
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(ROWLNQ)       ->    LAST  ROW                              
         LA    R4,ROWINFO(R1)                                                   
*                                                                               
DISREC82 LH    R6,MIDSTART                                                      
         SR    RF,RF                                                            
         IC    RF,MIDSIZE          SIZE OF PRINT AREA                           
         CLI   ROWTYPE,RRWMID      MID-LINE ?                                   
         BNE   DISREC84                                                         
         LR    R1,R6                                                            
         BCTR  R1,0                SUBTRACT ONE                                 
         STH   R1,MIDSTART                                                      
         LR    R1,RF               ADD ONE TO SIZE                              
         LA    R1,1(,R1)                                                        
         STC   R1,MIDSIZE                                                       
*                                                                               
DISREC84 TM    ROWOPT,RRWTOT       DO WE WANT TOTAL?                            
         BZ    DISREC85            NO                                           
         A     R6,CURRLINE                                                      
         GOTO1 TOTFOR,APPARM,((RF),(R6)),(C'R',(R3))                            
         BNE   DISREC90                                                         
         L     R6,APPARM           NUMBER OF LINES USED                         
         MH    R6,REPWDTH                                                       
         A     R6,CURRLINE                                                      
         ST    R6,CURRLINE                                                      
*                                                                               
DISREC85 SH    R4,=Y(ROWLNQ)                                                    
         BCT   R3,DISREC82                                                      
         DROP  R4                                                               
*                                                                               
         L     R6,CURRLINE                                                      
         AH    R6,MIDSTART                                                      
         GOTO1 TOTFOR,APPARM,(MIDSIZE,(R6)),(C'R',(R3))                         
*                                                                               
*                                  ************************************         
*                                  * FINAL BOX FORMAT                 *         
*                                  ************************************         
DISREC90 CLI   PRTBOX,NO                                                        
         BE    DISREC95                                                         
         CLI   ANYCOLS,C'N'        ANY   COLS  TO    OUTPUT ?                   
         BE    DISREC95            NO,   SKIP                                   
         L     R6,TOPLINE                                                       
         L     R2,FOOTLINE                                                      
         SH    R2,REPWDTH                                                       
*                                                                               
DISREC92 ST    R6,CURRLINE                                                      
         BAS   RE,BOXIT                                                         
         AH    R6,REPWDTH                                                       
         CR    R6,R2                                                            
         BL    DISREC92                                                         
         BAS   RE,BOXTOP                                                        
         EJECT ,                                                                
***********************************************************************         
*              CALCULATE WHAT PART OF REPORT TO DISPLAY               *         
***********************************************************************         
         SPACE 1                                                                
DISREC95 DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,PRVWROW                                                       
         BCTR  R6,0                                                             
         MH    R6,REPWDTH                                                       
         A     R6,AREPAREA                                                      
         ST    R6,CURRLINE                                                      
         TM    INOPT1,INOCBAR                                                   
         BZ    *+8                                                              
         BAS   RE,COLBAR                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PRVWCOL                                                       
         BCTR  R1,0                                                             
         AR    R6,R1                                                            
*                                                                               
         LA    R0,NROWS            # OF ROWS TO DISPLAY                         
         LA    R4,PRVFRSTH         FIRST FIELD TO PLACE DATA                    
*                                                                               
DISREC96 OI    6(R4),FVOXMT        TRANSMIT                                     
         MVC   8(L'PRVFRST,R4),0(R6)                                            
         SR    R1,R1                                                            
         IC    R1,0(,R4)           BUMP TO NEXT HEADER                          
         AR    R4,R1                                                            
         AH    R6,REPWDTH                                                       
         BCT   R0,DISREC96                                                      
*                                                                               
DISREC99 B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        PRINT TOTAL FOR LINE OF COLUMN OR ROW                        *         
*        PARM1 = AL1     SIZE OF PRINT AREA                           *         
*                AL3     A(PRINT AREA)                                *         
*        PARM2 = AL1     "C" - COLUMN, "R" - ROW                      *         
*                AL3     COLUMN OR ROW NUMBER                         *         
***********************************************************************         
         SPACE 1                                                                
TOTFOR   NTR1                                                                   
         L     R3,0(,R1)           A(PRINT AREA)                                
         SR    R4,R4                                                            
         ICM   R4,1,0(R1)          PRINT AREA SIZE                              
         L     R5,4(,R1)           NUMBER OF COLUMN OR ROW                      
         LA    R5,0(,R5)           CLEAR HOB                                    
         LR    R0,R1                                                            
         MVC   APBYTE,4(R1)                                                     
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(20),AC@TREQ                                               
         LTR   R5,R5                                                            
         BZ    TOTFOR05                                                         
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(12),AC@TFOR                                               
         MVI   APWORK+13,C'('                                                   
         MVC   APWORK+14(1),APBYTE                                              
         CVD   R5,APDUB                                                         
         OI    APDUB+7,X'0F'       MAKE A CHARACTER NUMBER                      
         UNPK  APWORK+15(2),APDUB                                               
         MVI   APWORK+17,C')'                                                   
         CLI   APWORK+15,C'0'                                                   
         BNE   TOTFOR05                                                         
         MVC   APWORK+15(2),APWORK+16                                           
         MVI   APWORK+17,C' '                                                   
*                                                                               
TOTFOR05 GOTO1 VSQUASH,APPARM,APWORK,L'APWORK                                   
         L     RF,APPARM+4         SIZE OF DATA                                 
         LR    R2,RF                                                            
         LA    RE,APWORK                                                        
         CLI   PRTCASE,C'U'                                                     
         BNE   TOTFOR10                                                         
         L     R8,AUPCASE                                                       
         TR    APWORK,0(R8)                                                     
*                                                                               
TOTFOR10 CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),X'42'         MASK, CAN PRINT BOX ON TOP OF HERE           
         LA    RE,1(,RE)                                                        
         BCT   RF,TOTFOR10                                                      
         LH    R1,NLINES                                                        
         C     R1,=F'3'                                                         
         BNH   TOTFOR50                                                         
         GOTO1 VCHOPPER,APPARM,((R2),APWORK),((R4),(R3)),              X        
               (REPWDTH+1,3),0,0                                                
         L     R2,APPARM+8         LINES USED                                   
         LA    R2,1(,R2)           FOR SKIP A LINE                              
         ST    R2,0(,R1)                                                        
         LH    R1,NLINES                                                        
         SR    R1,R2                                                            
         STH   R1,NLINES                                                        
         BM    TOTFOR50                                                         
         SR    R1,R1                                                            
*                                                                               
TOTFOR50 B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  BUILD COLUMN BAR TO DISPLAY ON TOP OF PREVIEW                      *         
***********************************************************************         
         SPACE 1                                                                
COLBAR   NTR1                                                                   
         L     R6,CURRLINE         ADDRESS OF REPORT BLOCK                      
         AH    R6,REPWDTH                                                       
         LH    R1,REPWDTH                                                       
         SH    R1,=H'02'                                                        
         MVI   0(R6),C'-'                                                       
         EXMVC R1,1(R6),0(R6)                                                   
         LR    RF,R6                                                            
         AH    RF,REPWDTH                                                       
         LA    R1,1                                                             
         LA    R6,4(,R6)                                                        
*                                                                               
COLBAR10 MVI   0(R6),C'+'                                                       
         LA    R6,5(,R6)                                                        
         CR    R6,RF                                                            
         BH    XIT                                                              
         STC   R1,0(,R6)                                                        
         OI    0(R6),X'F0'         MAKE A CHARACTER NUMBER                      
         LA    R1,1(,R1)                                                        
         CH    R1,=H'10'                                                        
         BL    *+8                                                              
         SH    R1,=H'10'                                                        
         LA    R6,5(,R6)                                                        
         CR    R6,RF                                                            
         BH    XIT                                                              
         B     COLBAR10                                                         
         EJECT ,                                                                
         SPACE 1                                                                
         USING COLINFOD,R3                                                      
         SPACE 1                                                                
BOXTOP   NTR1                                                                   
         LA    R3,COLARRAY         ->    COLUMN ARRAY                           
         L     R6,CURRLINE         ADDRESS OF REPORT BLOCK                      
         AH    R6,CIBOXCOL         BOXES COLUMN                                 
         LH    R1,BOXWDTH                                                       
         SH    R1,=H'02'                                                        
         MVI   0(R6),C'-'                                                       
         EXMVC R1,1(R6),0(R6)                                                   
         MVI   BOXCHAR,C'+'                                                     
         B     BOX10                                                            
         DROP  R3                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         USING COLINFOD,R3                                                      
         SPACE 1                                                                
BOXIT    NTR1                                                                   
         MVI   BOXCHAR,C':'                                                     
*                                                                               
BOX10    SR    R1,R1                                                            
         IC    R1,COL#                                                          
         LA    R1,1(,R1)                                                        
         LA    R3,COLARRAY         ->    COLUMN ARRAY                           
*                                                                               
BOX20    LH    R6,CIBOXCOL         GET   BOXES  COLUMN                          
         A     R6,CURRLINE                                                      
         TM    CIBOXCOL,X'80'      DON'T EVER   PRINT THIS  COL  LINE           
         BNZ   BOX40                                                            
         CLI   BOXCHAR,C'+'                                                     
         BE    BOX30                                                            
         CLI   0(R6),C' '                                                       
         BNE   BOX40                                                            
*                                                                               
BOX30    MVC   0(1,R6),BOXCHAR                                                  
*                                                                               
BOX40    LA    R3,CILNQ(,R3)       NEXT  COLUMN                                 
         BCT   R1,BOX20                                                         
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  DAY PARAMETER BUILD HEADINGS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
         SPACE 1                                                                
DAYBLD   NTR1                                                                   
         LA    R6,HEAD1                                                         
         MVC   APHALF,RCLSTDT                                                   
         CLC   RCLSTDT,=XL2'8000'                                               
         BE    DAYBLD10                                                         
         BAS   RE,DAYCALC                                                       
         LA    R6,HEAD2                                                         
         MVC   APHALF,RCLENDT                                                   
         B     DAYBLD20                                                         
*                                                                               
DAYBLD10 MVCDD HEAD1,AC#CUR                                                     
         OC    RCLENDT,RCLENDT                                                  
         BZ    DAYBLDX                                                          
         MVC   HEAD2(2),=C'&& '                                                 
         MVC   HEAD2+2(L'AC@PRIOR),AC@PRIOR                                     
         MVC   APHALF,RCLENDT                                                   
         LA    R6,HEAD1                                                         
         BAS   RE,DAYCALC                                                       
         B     DAYBLDX                                                          
*                                                                               
DAYBLD20 CLC   RCLENDT,=XL2'8000'                                               
         BNE   DAYBLD25                                                         
         MVCDD HEAD1,AC#FUTUR                                                   
         OC    RCLSTDT,RCLSTDT                                                  
         BZ    DAYBLDX                                                          
         MVC   HEAD1,SPACES                                                     
         MVC   HEAD2(2),=C'&& '                                                 
         MVC   HEAD2+2(L'AC@AFTER),AC@AFTER                                     
         MVC   APHALF,RCLSTDT                                                   
         LA    R6,HEAD1                                                         
*                                                                               
DAYBLD25 BAS   RE,DAYCALC                                                       
*                                                                               
DAYBLDX  B     XIT                                                              
*                                                                               
DAYCALC  ST    RE,SAVERE                                                        
         LH    R0,APHALF                                                        
         GOTO1 VADDAY,APPARM,DATE2DAY,TEMPDATE,(R0)                             
         GOTO1 VDATCON,APPARM,(0,TEMPDATE),(8,(R6))                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATE BASIS PARAMETERS ==> MON OR M1-M12                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
         SPACE 1                                                                
MONBLD   NTR1                                                                   
         SR    R0,R0                                                            
         TM    RCLDTEFG,RCLNROLL   NO ROLLING MONTHS (M1-M12)                   
         BZ    MONBLD10                                                         
         MVC   APHALF,RCLENDT      GET MONTH NUMBER (BINARY)                    
         LH    R0,APHALF           R0 = MONTHS                                  
         BCTR  R0,0                                                             
         MVC   APHALF,RCLSTDT      GET # OF YEAR FORWARD OR BACK                
         LH    R1,APHALF           R1 = YEARS                                   
         B     MONBLD20                                                         
*                                  DATE PARMETERS MON                           
MONBLD10 MVC   APHALF,RCLSTDT      GET MONTH NUMBER (BINARY)                    
         CLC   RCLSTDT,=XL2'8000'                                               
         BNE   *+10                                                             
         MVC   APHALF,RCLENDT      GET MONTH NUMBER (BINARY)                    
         SR    R1,R1               R1 = YEARS                                   
         LH    R0,APHALF           R0 = MONTHS                                  
*                                                                               
MONBLD20 MVC   APWORK(3),ASBDAT    BINARY DATE                                  
         TM    RCLDTEFG,RCLNROLL   NOT ROLLING                                  
         BZ    *+10                                                             
         MVC   APWORK+1(1),FISCALMO    CHANGE MONTH TO FISCAL START             
         LR    R6,R1                                                            
         GOTO1 VDATCON,APPARM,(3,APWORK),(0,TEMPDATE)                           
         GOTO1 VADDAY,APPARM,(C'M',TEMPDATE),TEMPDATE,(R0)                      
         LTR   R6,R6                                                            
         BZ    MONBLD30            NO YEAR TO ADJUST FOR                        
         GOTO1 VADDAY,APPARM,(C'Y',TEMPDATE),TEMPDATE,(R6)                      
*                                                                               
MONBLD30 GOTO1 VDATCON,APPARM,(0,TEMPDATE),(9,HEAD1)                            
         CLC   RCLSTDT,=XL2'8000'                                               
         BNE   MONBLD40                                                         
         MVI   HEAD2,C'&&'                                                      
         MVC   HEAD2+2(L'AC@PRIOR),AC@PRIOR                                     
*                                                                               
MONBLD40 CLC   RCLENDT,=XL2'8000'                                               
         BNE   MONBLD50                                                         
         MVI   HEAD2,C'&&'                                                      
         MVC   HEAD2+2(L'AC@AFTER),AC@AFTER                                     
*                                                                               
MONBLD50 B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* BUILD HEADINGS FOR Q1-Q4 OR QTR OR QTD                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
         SPACE 1                                                                
QTRBLD   NTR1                                                                   
         SR    R0,R0                                                            
         MVC   APHALF,RCLSTDT      QTR                                          
         LH    R4,APHALF           R4 = YEARS                                   
         IC    R0,CURQTR           R0 = CURRENT QTR                             
         TM    RCLDTEFG,RCLNROLL   NO ROLLING MONTHS                            
         BZ    QTRBLD10                                                         
         MVC   APHALF,RCLENDT      GET QUARTER NUMBER (BINARY)                  
         LH    R0,APHALF           R0 = QUARTER                                 
*                                                                               
QTRBLD10 BCTR  R0,0                                                             
         MH    R0,=Y(L'QTRTAB)                                                  
         LA    R6,QTRTAB                                                        
         AR    R6,R0                                                            
         MVC   TEMPDATE,0(R6)                                                   
         LTR   R4,R4                                                            
         BZ    QTRBLD20                                                         
         GOTO1 VADDAY,APPARM,(C'Y',TEMPDATE),TEMPDATE,(R4)                      
*                                                                               
QTRBLD20 GOTO1 VDATCON,APPARM,(0,TEMPDATE),(8,HEAD1)                            
         MVC   TEMPDATE,6(R6)                                                   
         LTR   R4,R4                                                            
         BZ    QTRBLD25                                                         
         GOTO1 VADDAY,APPARM,(C'Y',TEMPDATE),TEMPDATE,(R4)                      
*                                                                               
QTRBLD25 GOTO1 VDATCON,APPARM,(0,TEMPDATE),(8,HEAD2)                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* YEAR BASIS                                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
         SPACE 1                                                                
YRBLD    NTR1                                                                   
         TM    RCLDTEFG,RCLTODTE                                                
         BZ    *+10                                                             
         MVCDD HEAD1,AC#RSRY2                                                   
         GOTO1 VDATCON,APPARM,(3,ASBDAT),(0,TEMPDATE)                           
         MVC   APHALF,RCLSTDT                                                   
         LH    R6,APHALF                                                        
         GOTO1 VADDAY,APPARM,(C'Y',TEMPDATE),TEMPDATE,(R6)                      
         GOTO1 VDATCON,APPARM,(0,TEMPDATE),(20,APWORK)                          
         MVC   HEAD2(4),APWORK                                                  
*                                                                               
XIT      XIT1                                                                   
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  ERROR MESSAGES TO DISPLAY ON TOP OF SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
IVALEKEY MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALHIGH MVC   FVMSGNO,=AL2(81)                                                 
         B     IVALEXIT                                                         
         SPACE 1                                                                
*                                  MESSAGE SAYS:   REPORT  TOO  WIDE            
*                                  EVEN THOUGH THE PROBLEM IS   THAT            
*                                  THE  FORMAT CAN EXCEED  198  CHARS;          
*                                  AND  THEREFORE, THE DOWNLOAD ONLY            
*                                  BIT  IS     ON                               
IVALREP  DS    0H                  REPORT      IS  TOO WIDE                     
         TM    TWASWPST,TWASWAP    ARE  WE     SWAPPING ?                       
         BNZ   IVALEXIT            YES, JUST   SWAP                             
         MVC   FVMSGNO,=AL2(1413)  REPORT      TOO WIDE                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
HEADTAB  DC    AL1(RHDTITL)                                                     
         DC    AL1(RHDCNTR)                                                     
         DC    AL1(RHDRHTH)                                                     
         DC    AL1(RHDLFTH)                                                     
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
MAXLINES EQU   67                                                               
LEN198   EQU   198                                                              
LEN164   EQU   164                                                              
LEN132   EQU   132                                                              
LEN110   EQU   110                                                              
NROW198  EQU   MAXLINES                                                         
NROW164  EQU   (MAXLINES*MAXRPTWD)/LEN164                                       
NROW132  EQU   (MAXLINES*MAXRPTWD)/LEN132                                       
NROW110  EQU   (MAXLINES*MAXRPTWD)/LEN110                                       
NROWS    EQU   15                                                               
NCOLS    EQU   L'PRVFRST                                                        
PAGEUP   EQU   NROWS                                                            
PAGELEFT EQU   NCOLS                                                            
HDLINE1  EQU   4                   FIRST HEADING LINE                           
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
AREPAREA DS    A                                                                
CURRLINE DS    A                   CURRENT LINE ON REPORT                       
TOPLINE  DS    A                   TOP OF BOXES, PRINTING ADDRESS               
LEFTHEAD DS    A                   ADDRESS OF LEFT SIDE HEADING LINE            
RGHTHEAD DS    A                   ADDRESS OF RIGHT SIDE HEADING LINE           
FOOTLINE DS    A                   ADDRESS OF FOOT LINE                         
SAVER1   DS    A                   SAVE OFF R1                                  
SAVERE   DS    A                   SAVE OFF RE                                  
COLSIZE  DS    A                                                                
PREVLOC  DS    A                   PREVIOUS COLUMN LOCATION                     
*                                                                               
BOXWDTH  DS    H                                                                
REPWDTH  DS    H                                                                
PREVSIZE DS    H                                                                
ADJUST   DS    H                                                                
SCROLL   DS    H                                                                
MIDSTART DS    H                                                                
NLINES   DS    H                                                                
*                                                                               
COL#     DS    AL1                                                              
ROW#     DS    AL1                                                              
LASTROW  DS    AL1                                                              
LASTCOL  DS    AL1                                                              
HEADROW  DS    AL1                 LAST  ROW HEADING                            
BOXCHAR  DS    CL1                                                              
STKUNDMX DS    AL1                 MAX   STACK UNDER LEVEL                      
ANYCOLS  DS    CL1                 ANY   COLUMNS TO PRINT (Y/N)                 
ANYNACMC DS    CL1                 ANY   NON-ACCUMULATED COLUMN (Y/N)           
MIDSIZE  DS    AL1                                                              
CHUNK    DS    AL1                                                              
TMPCWDTH DS    XL1                                                              
RHDLINE# DS    AL1                 FOR   ROW HEADER - LINE NUMBER               
*                                                                               
PRTBOX   DS    CL1                 PRINT BOXES (Y/N)                            
PRTLFJT  DS    CL1                 PRINT LEFT JUSTIFIED (Y/N)                   
PRTSTYLE DS    CL1                 PRINT STYLE (L/P) PORTRIAT/LANDSCAPE         
PRTCASE  DS    CL1                 PRINT CASE (U/L) UPPER/LOWER                 
PRTCWRAP DS    CL1                 PRINT COLUMN WRAP (Y/N)                      
*                                                                               
CURQTR   DS    AL1                                                              
FSCQTR   DS    AL1                                                              
*                                                                               
TEMPDATE DS    CL6                                                              
DATE2DAY DS    CL6                 YYMMDD EBCDIC                                
*                                                                               
TODAY    DS    CL8                 TODAY                                        
*                                                                               
QTRTAB   DS    4CL12               4 START/END MONTHS - YYMMDD/YYMMDD           
QTRMONTH DS    4CL6                4 QTRS OF   MONTHS IN QTR MM/MM/MM           
*                                                                               
AC@TFOR  DS    CL12                                                             
AC@TREQ  DS    CL20                                                             
*                                                                               
DUMMYFH  DS    XL8                 DUMMY FIELD HEADER                           
DUMMYF   DS    CL7                 DUMMY FIELD                                  
*                                                                               
PREVHEAD DS    CL(L'RCLHDL1)                                                    
HEAD1    DS    CL(L'RCLHDL1)                                                    
HEAD2    DS    CL(L'RCLHDL1)                                                    
TEMPHEAD DS    CL(L'RCLHDL1)                                                    
*                                                                               
ROWINFO  DS    (MAXROWS)CL(ROWLNQ)       SEE   ROWD  DSECT                      
*                                                                               
         DS    0H                        COLARRAY NEEDS HALF WORD BWD           
COLARRAY DS    XL(CILNQ*(MAXCOLS+1))     SEE   COLINFOD DSECT                   
LWSX     DS    0C                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROW INFORMATION SAVED                                              *         
***********************************************************************         
         SPACE 1                                                                
ROWD     DSECT                                                                  
ROWOPT   DS    AL1                                                              
ROWTYPE  DS    AL1                                                              
ROWDATA  DS    CL12                                                             
ROWPRFX  DS    CL14                                                             
ROWSTART DS    AL2                                                              
ROWSIZE  DS    AL1                                                              
*&&US                                                                           
ROWOPT3  DS    AL1                                                              
*&&                                                                             
ROWLNQ   EQU   *-ROWD                                                           
         EJECT ,                                                                
***********************************************************************         
*  COLUMN INFORMATION SAVED                                           *         
***********************************************************************         
         SPACE 1                                                                
COLINFOD DSECT                     COLUMN INFORMATION                           
*                                                                               
*                                  NOTE:  KEEP  ON   HALF WORD BOUNDRY          
*                                                                               
CIBOXCOL DS    H                   BOXES  COLUMN                                
CICOLWD  DS    X                   COLUMN WIDTH                                 
CICOLOPT DS    X                   COLUMN OPTIONS                               
CIACCM   EQU   RCLACCM             .X'80' ACCUMULATED     COLUMN                
CIHIDE   EQU   RCLHIDE             .X'20' HIDDEN     COLUMN                     
CICOLOP2 DS    X                   COLUMN OPTIONS                               
CITOT    EQU   RCLTOT              .X'80' TOTAL ON   COLUMN                     
CISTKUCL DS    AL1                 STACK  UNDER COLUMN                          
CISTKULV DS    AL1                 STACK  UNDER LEVEL                           
CISTKUWD DS    AL1                 STACK  UNDER WIDTH                           
CISTKUHW DS    AL1                 STACK  UNDER HEAD1     WIDTH                 
CISTKUKW DS    CL(MXCDATLN)        STACK  UNDER KEYWORD                         
*                                         OR    EQUATION (LEN= 12)              
CISTKUH1 DS    CL(L'RCLNHDL1)      STACK  UNDER HEAD1    (LEN= 12)              
         DS    0H                  SPARE                                        
CILNQ    EQU   *-COLINFOD          ENTRY  LENGTH                                
         EJECT ,                                                                
* ACSCRWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF7D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACSCR06   09/02/15'                                      
         END                                                                    
