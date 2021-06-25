*          DATA SET ACSCR0E    AT LEVEL 047 AS OF 09/03/15                      
*PHASE T60C0EA,+0                                                               
*&&ONLIN SET   Y                                                                
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 46 AS OF 07/23/13         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 049 10NOV11 PR000122 Estimate Phase 1                                    
* SMAN 050 16DEC11 PR002242 UK/US MERGE                                         
* JFOS 051 04JAN13 PR003402 ESHRAT/C KWORDS NEED CAC OR CAN IN FORMAT           
*                                                                               
         TITLE 'FORMAT MAINTENANCE'                                             
T60C0E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C0E,RA,R9,RR=RE,CLEAR=YES                                    
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         L     RE,=A(DELWARN)                                                   
         A     RE,APRELO                                                        
         ST    RE,ADELWARN                                                      
*                                                                               
         MVC   ERRORMSG,SPACES                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVXTRA,SPACES                                                    
         MVI   DSRNMTYP,C' '       CLEAR DELAYED SCREEN  MESSAGE TYPE           
         XC    DSRNMNUM,DSRNMNUM   CLEAR DELAYED SCREEN  MESSAGE NUMBER         
         XC    DSRNDATA,DSRNDATA   CLEAR DELAYED SCREEN  MESSAGE DATA           
         MVI   MSGELSW,X'00'       CLEAR MESSAGE ELEMENT SWITCH                 
         MVI   WARNSW,X'00'        CLEAR WARNING MESSAGE SWITCH                 
         MVC   DRMSGNO,=AL2(FVFOK) CLEAR DISPLAY ERROR   NUMBER                 
         XC    DRCURSOR,DRCURSOR   CLEAR DISPLAY CURSOR                         
         MVI   PASTESEQ,X'00'      CLEAR PASTE   INSERT  SEQ     NUMBER         
         MVC   RQBLOCK(RQSTR-RQBLOCK),SPACES                                    
         XC    RQSTR(RQCURCY-RQSTR),RQSTR                                       
         MVC   RQNAME,SPACES                                                    
*                                                                               
         USING COLD,R2             MAP  COL  FIELDS                             
         CLI   APMODE,APMDISR      ARE  WE   IN   DISPLAY   RECORD ?            
         BE    SCR000              YES, CONTINUE                                
         CLI   APMODE,APMVALR      ARE  WE   IN   VALIDATE  RECORD ?            
         BNE   SCR035              NO,  SKIP                                    
*                                                                               
SCR000   CLI   TWALREC,RECCOL      WERE WE   ON   COLUMN    SCREEN ?            
         BE    SCR020              YES, SPECIAL   PROCESSING                    
         CLI   TWALREC,RECCOLF     WERE WE   ON   COLUMN    FILTERS ?           
         BE    SCR020              YES, SPECIAL   PROCESSING                    
*                                                                               
         CLC   ATWA,ACURSOR        IS   THE  CURSOR    IN   A    FIELD?         
         BE    SCR001              NO,  SET  CURSOR                             
         CLI   TWALREC,RECFRM      1ST  TIME IN ?                               
         BNE   SCR001              YES, SET  CURSOR                             
         CLI   TWALACT,ACTLST      COMING    FROM LIST SCREEN ?                 
         BNE   SCR002              NO,  KEEP CURSOR                             
*                                                                               
SCR001   LA    RE,FRMCODEH         SET  DEFAULT   CURSOR    TO                  
         ST    RE,ACURSOR               NAME FIELD                              
*                                                                               
SCR002   L     R4,ACURSOR          IS   CURSOR    IN   COLS LIST ?              
*                                                                               
         LA    RE,FRMCODEH         SET  DEFAULT   CURSOR    TO                  
         ST    RE,ACURSOR               NAME FIELD                              
                                                                                
         LA    R2,FRMCOL1H         LESS THAN COLUMN 1  FIELD ?                  
         CR    R4,R2                                                            
         BL    SCR006              CURSOR    IS   BEFORE    COLUMNS             
         LA    R2,FRMFUTXH         PAST LAST COLUMN    FIELD ?                  
         CR    R4,R2                                                            
         BNL   SCR013              CURSOR    IS   PAST COLUMNS                  
*                                                                               
         LA    R1,1                DEFAULT TO COLUMN ONE                        
         LA    R2,FRMCOL1H                                                      
SCR004   LA    RE,COLDATAH         ->   1ST  COLUMN                             
         ST    RE,ACURSOR                                                       
         LA    R2,COLLNQ(,R2)                                                   
         CR    R4,R2               IS   IT   LESS THAN END  OF   LINE ?         
         BL    SCR025              YES, DONE                                    
         LA    R1,1(,R1)           ->   NEXT COLUMN    LINE                     
         B     SCR004              TRY  NEXT COLUMN    LINE                     
*                                                                               
SCR006   MVI   CURRCOLN,0          RESET     COLUMN    NUMBER                   
         MVI   COLUMN#,0                                                        
         LA    R3,FRMBGNH          ->   UNIT AND  LEDGER    HEADER              
         CR    R4,R3               CURSOR    BEFORE    UNIT+LEDGER ?            
         BL    SCR035              YES, DEFAULT   CURSOR    IS   SET            
         LA    RE,FRMTYPE+L'FRMTYPE                                             
         CR    R4,RE               CURSOR    PAST UNIT AND  LEDGER ?            
         BNL   SCR007              YES, TRY  TITLE                              
         LA    R1,FRMTYPEH         ->   UNIT AND  LEADGER   HEADER              
         B     SCR015              SET  CURSOR                                  
*                                                                               
         USING HEADSCRD,R3         MAP  HEADERS   AREA                          
SCR007   LA    R3,FRMTITXH         ->   1ST  HEADER   (TITLE)                   
         CR    R4,R3               CURSOR    BEFORE    1ST  HEADER ?            
         BL    SCR035              YES, DEFAULT   CURSOR    IS   SET            
*                                  ->   PAST HEADER    AREA                     
         LA    RE,FRMRHED+L'FRMRHED                                             
         CR    R4,RE               CURSOR    PAST HEADERS   AREA ?              
         BNL   SCR010              YES, TRY  ROWS                               
         LA    R1,HEADDATH         ->   1ST  DATA HEADER                        
*                                                                               
SCR008   LA    R3,HEADSCRQ(,R3)    ->   PAST END  OF   FIELD                    
         CR    R4,R3               CURSOR    PAST END  OF   FIELD ?             
         BL    SCR015              NO,  SET  CURSOR                             
         LA    R1,HEADSCRQ(,R1)    ->   NEXT HEADER    AREA                     
         B     SCR008              TRY  NEXT HEADER    AREA                     
*                                                                               
         USING ROWD,R3             MAP  ROW  FIELDS                             
SCR010   LA    R3,FRMROW1H         ->   1ST  ROW  LINE                          
         CR    R4,R3               CURSOR    BEFORE    ROW  HEADER ?            
         BL    SCR035              YES, DEFAULT   CURSOR    IS   SET            
         LA    RE,FRMROW3H+ROWLNQ  ->   PAST ROW  AREA                          
         CR    R4,RE               CURSOR    PAST HEADERS   AREA ?              
         BNL   SCR035              YES, DEFAULT   CURSOR    IS   SET            
         LA    R1,ROWDATAH         ->   1ST  ROW  DATA AREA                     
*                                                                               
SCR011   LA    R3,ROWLNQ(,R3)      ->   PAST END  OF   FIELD                    
         CR    R4,R3               CURSOR    PAST END  OF   FIELD ?             
         BL    SCR015              NO,  SET  CURSOR                             
         LA    R1,ROWLNQ(,R1)      ->   NEXT ROW  LINE                          
         B     SCR011              TRY  NEXT ROW  LINE                          
         DROP  R3                                                               
*                                  CURSOR    PAST COLUMN    FIELDS              
SCR013   MVI   CURRCOLN,0          RESET     COLUMN    NUMBER                   
         MVI   COLUMN#,0                                                        
         LA    R3,FRMFUTXH         ->   FOOTING   TEXT                          
         CR    R4,R3               BEFORE    FOOTING   HEADER ?                 
         BL    SCR035              YES, DEFAULT   CURSOR    IS   SET            
*                                  ->   PAST FOOTING   AREA                     
         LA    RE,FRMFOOT+L'FRMFOOT                                             
         CR    R4,RE               CURSOR    PAST FOOTING   AREA ?              
         BNL   SCR035              YES, DEFAULT   CURSOR    IS   SET            
*                                                                               
         LA    R1,FRMFOOTH         ->   FOOTING   DATA HEADER                   
SCR015   ST    R1,ACURSOR          SET  CURSOR                                  
         B     SCR035              CONTINUE                                     
*                                                                               
*                                  1ST  TIME IN   FROM COLUMNS   OR             
*                                       FROM COLUMN    FILTERS                  
SCR020   CLI   CURRCOLN,0          ANY  COLUMN    SPECIFIED ?                   
         BE    SCR030              NO,  SET  TO   FORMAT    CODE                
*                                                                               
         LA    R2,FRMCOL1H                                                      
         CLI   CURRCOLN,12                                                      
         BNH   *+8                                                              
         MVI   CURRCOLN,1          DEFAULT TO COLUMN 1                          
         ZIC   R1,CURRCOLN         GET TO COLUMN                                
         SHI   R1,1                                                             
         MHI   R1,COLLNQ                                                        
         AR    R2,R1                                                            
         LA    RE,COLDATAH         DEFAULT TO FIRST FIELD OF COL                
         ST    RE,ACURSOR          STORE LOCATION TO PUT CURSOR                 
         SR    R1,R1                                                            
         IC    R1,CURRCOLN                                                      
         DROP  R2                                                               
*                                                                               
SCR025   STC   R1,COLUMN#                                                       
         STC   R1,CURRCOLN                                                      
         B     SCR035                                                           
*                                                                               
SCR030   MVI   CURRCOLN,0          RESET     COLUMN    NUMBER                   
         MVI   COLUMN#,0                                                        
         LA    RE,FRMCODEH         SET  DEFAULT   CURSOR    TO                  
         ST    RE,ACURSOR               NAME FIELD                              
*                                                                               
SCR035   CLI   APACTN,ACTCHA       ARE WE CHANGING RECORD?                      
         BNE   SCR040                                                           
         CLI   APMODE,APMVALK      VALKEY MODE ONLY                             
         BNE   SCR040                                                           
         CLI   APPFKEY,0                                                        
         BNE   SCR040                                                           
         TM    SCROPTH+4,FVITHIS                                                
         BZ    SCR040                                                           
         TM    TWAMODE,TWAMLSM     LIST SELECT MODE ?                           
         BO    SCR040              YES, SKIP                                    
         XC    SAVRECK,SAVRECK     CLEAR TO REDISPLAY RECORD                    
*                                                                               
SCR040   CLI   APMODE,APMVALR      IN   VALIDATE  RECORD ?                      
         BE    SCR045              YES, CONTINUE                                
         CLI   APMODE,APMDISR      IN   DISPLAY   RECORD ?                      
         BNE   SCR200                                                           
*                                                                               
SCR045   CLI   APPFKEY,PFK13       GOING     TO   COL  FILTER   SCREEN?         
         BNE   SCR050              NO,  SKIP                                    
         CLI   CURRCOLN,0          MUST HAVE A    VALUE                         
         BE    IVALCOLF                                                         
*                                                                               
SCR050   CLI   APPFKEY,PFKHLP                                                   
         BNE   SCR100                                                           
         MVI   SVINSSEQ,0          CLEAR SAVE INSSEQ                            
         L     R2,ACURSOR          HAVE  TO   DECIDE WHICH HELP TO USE          
         LA    RF,FRMTITLH                                                      
         CR    R2,RF                                                            
         BL    IVALHLP                                                          
         LA    RF,FRMRHEDH                                                      
         CR    R2,RF                                                            
         BH    SCR055                                                           
         MVI   HLPREPT,REPHEAD     HELP  FOR  HEADERS                           
         B     SCR070                                                           
*                                                                               
SCR055   LA    RF,FRMWDTHH                                                      
         CR    R2,RF                                                            
         BH    SCR060                                                           
         MVI   HLPREPT,REPROW      HELP  FOR  ROWS                              
         B     SCR070                                                           
*                                                                               
SCR060   LA    RF,FRMFUTXH                                                      
         CR    R2,RF                                                            
         BH    SCR065                                                           
         MVI   HLPREPT,REPCOL      HELP  FOR  COLUMNS                           
         B     SCR070                                                           
*                                                                               
SCR065   MVI   HLPREPT,REPHEAD     HELP  FOR  HEADERS                           
*                                                                               
SCR070   CLI   APACTN,ACTCHA       ARE   WE   UPDATING  THE  RECORD ?           
         BE    SCR072              YES,  CONTINUE                               
         CLI   APACTN,ACTADD       ARE   WE   ADDING    A    RECORD ?           
         BNE   SCR100              NO,   SKIP                                   
*                                                                               
SCR072   DS    0H                                                               
         GOTO1 AFVAL,(R2)          ANY   DATA IN   FIELD ?                      
         BE    SCR100              YES,  SKIP                                   
         LR    RE,R2               NO,   SAVE OFFSET                            
         S     RE,ATWA                   FROM START     OF   TWA                
         ST    RE,CUR@RHLP               TO   ENABLE    PASTE                   
*                                                                               
*                                  NOTE, HEADERS   ARE  ALL  SET  UP            
         CLI   HLPREPT,REPHEAD     HELP  FOR  HEADERS ?                         
         BE    SCR100              YES,  CONTINUE                               
*                                                                               
         CLI   HLPREPT,REPROW      HELP  FOR  ROWS ?                            
         BNE   SCR075              NO,   TRY  COLUMNS                           
         LR    R1,R2               GET   CURSOR    LOCATION                     
         LA    RE,FRMROW1H         ->    1ST  ROW  AREA                         
*                                                                               
         USING ROWD,RE             MAP   ROW AREA                               
         LA    RE,ROWDATAH         ->    1ST  ROW  DATA FIELD                   
         SR    R1,RE               GET   OFFSET    FROM 1ST  DATA FIELD         
         SR    R0,R0               CLEAR REGISTER                               
         D     R0,=A(ROWLNQ)       GET   ROW  NUM  (1ST =    0)                 
         LA    R1,1(,R1)           GET   ROW  NUM  (1ST =    1)                 
         STC   R1,SVINSSEQ         SET   INSERT    SEQUENCE  NUMBER             
         B     SCR100              CONTINUE                                     
         DROP  RE                                                               
*                                                                               
SCR075   CLI   HLPREPT,REPCOL      HELP  FOR  COLUMNS ?                         
         BNE   SCR100              NO,   SKIP                                   
         MVC   SVINSSEQ,INSSEQ     YES,  SAVE INSSEQ                            
         CLI   INSSEQ,0            ARE  WE   IN   AN    INSERT ?                
         BNE   SCR100              YES, CONTINUE                                
         LA    RF,FRMCOL1H         ->   1ST  COLUMN     LINE                    
*                                  ->   1ST  COLUMN     DATA AREA               
         LA    RF,COLDATAH-COLD(,RF)                                            
         LR    R1,R2               ->   CURRENT   CURSOR                        
         SR    R1,RF               GET  OFFSET    FROM 1ST  COLUMN              
         SR    R0,R0               CLEAR     REGISTER                           
         D     R0,=A(COLLNQ)       GET  COL  NUM  (1ST =    0)                  
         LA    R1,1(,R1)           GET  COL  NUM  (1ST =    1)                  
         STC   R1,SVINSSEQ         SET  INSERT    SEQUENCE  NUMBER              
*                                                                               
         USING RESRECD,R2                                                       
SCR100   CLI   APPFKEY,PFKDEL      PFKEY =    DELETE ?                          
         BE    SCR110              YES,  VALIDATE                               
         CLI   APPFKEY,PFKINS      PFKEY =    INSERT ?                          
         BNE   SCR200              NO,   SKIP                                   
*                                                                               
SCR110   CLI   CURRCOLN,0          MUST  HAVE A    VALUE                        
         BE    IVALCOLF            INVALID    COLUMN    NUMBER                  
         EJECT ,                                                                
SCR200   LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALIDATE KEY                                 
         B     VALREC              VALIDATE RECORD                              
         B     DISKEY              DISPLAY KEY                                  
         B     DISREC              DISPLAY RECORD                               
         B     DELREC              DELETE RECORD                                
         B     RESREC              RESTORE RECORD                               
         B     VALSEL              VALSEL                                       
         B     RGETSEL             RGETSEL                                      
         B     DISSEL              DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     PROCLST             PROCESS LIST/SELECT                          
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     CPYREC              COPY RECORD                                  
         B     EXIT                                                             
         B     REDIS               RE-DISPLAY LIST ENTRY AFTER LFM              
         EJECT ,                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK      VALIDATE KEY?                                
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK      DISPLAY KEY?                                 
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP                                                 
         BZ    EXIT95                                                           
*                                                                               
         CLI   APPFKEY,PFKHLP                                                   
         BNE   EXIT50                                                           
         XC    ACURDEF,ACURDEF     SET  TO   BEGINING OF HELP                   
         MVC   REPMODE,HLPREPT                                                  
         CLI   HLPREPT,0           HELP REPORT    TYPE SET ?                    
         BNE   EXIT50              YES, CONTINUE                                
         MVI   APPFKEY,0           CLEAR     PF   KEY                           
         B     EXIT95              STOP SWAP                                    
*                                                                               
EXIT50   CLC   FVMSGNO,=AL2(FVFOK) ANY  ERROR OCCURED ?                         
         BE    EXIT80              NO,  TEST FOR  PF04/PF13                     
         CLI   APPFKEY,PFK14       REQUEST   PF   KEY  ?                        
         BNE   EXIT55              NO,  SKIP                                    
*                                  ANY  ERROR     MESSAGES ?                    
         GOTO1 =A(CKMSGERR),RR=APRELO                                           
         BE    EXIT95              YES, STOP SWAP                               
         B     EXIT70              NO,  ALLOW     SWAP                          
*                                                                               
EXIT55   TM    MSGELSW,MSGELERR    MESSAGE   ELEMENT   ERROR ?                  
         BO    EXIT70              YES, ALLOW     SWAP                          
         CLI   APPFKEY,PFK04       COLUMN    FILTER    PF   KEY  ?              
         BE    EXIT60              YES, CHK  TYPE OF   ERROR                    
         CLI   APPFKEY,PFK13       FORMAT    PF   KEY  ?                        
         BNE   EXIT70              NO,  ALLOW     SWAP                          
*                                                                               
EXIT60   CLI   FVOMTYP,X'00'       MESSAGE   TYPE ERROR ?                       
         BE    EXIT95              YES, STOP SWAP                               
         CLI   FVOMTYP,GTMERR      MESSAGE   TYPE ERROR ?                       
         BE    EXIT95              YES, STOP SWAP                               
*                                                                               
EXIT70   MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
EXIT80   CLI   APPFKEY,PFK13       COLUMN    FILTER    PF   KEY  ?              
         BE    EXIT90              YES, CONTINUE                                
         CLI   APPFKEY,PFK04       COLUMN    PF   KEY  ?                        
         BNE   EXIT92              NO,  SKIP                                    
*                                                                               
EXIT90   MVC   CURRCOLN,COLUMN#                                                 
*                                                                               
EXIT92   XC    APCURSOR,APCURSOR   DON'T    SET  CURSOR ON WRONG SCREEN         
         MVI   APMODE,APMSWP                                                    
         CLI   TWASWPAC,ACTADD     ACTION = ADD ?                               
         BNE   EXIT93              NO,      CONTINUE                            
         MVI   TWASWPAC,ACTCHA     ACTION = CHANGE                              
         CLI   APPFKEY,PFKHLP      HELP     PF   KEY ?                          
         BNE   *+8                 NO,      SKIP                                
         MVI   TWASWPAC,ACTHLP     ACTION = HELP                                
         CLI   APPFKEY,PFKREQ      REQUEST  PF   KEY ?                          
         BNE   *+8                 NO,      SKIP                                
         MVI   TWASWPAC,ACTREQ     ACTION = REQUEST                             
         CLI   APPFKEY,PFKPRVW     PREVIEW  PF   KEY ?                          
         BNE   *+8                 NO,      SKIP                                
         MVI   TWASWPAC,ACTPRVW    ACTION = PREVIEW                             
                                                                                
EXIT93   MVC   APPARM(1),TWASWPRE  SWAP     TO   NEW    RECORD                  
         MVC   APPARM+1(1),TWASWPAC         TO   NEW    ACTION                  
         MVI   APPFKEY,0           CLEAR    PF   KEY                            
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         CLI   APMODE,APMDISR      MODE IS   DISPLAY RECORD                     
         BNE   XIT                 NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY IS HAPPY                        
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
*=====================================================================*         
*  VALKEY                                                             *         
*=====================================================================*         
VALKEY   DS    0H                                                               
         MVI   HAVE5TH,NO                                                       
         MVCDD FRMBGN,AC#TYPRP                                                  
         OI    FRMBGNH+6,FVOXMT                                                 
         MVCDD FRMBGN,AC#UNTLD                                                  
*                                                                               
VALKEY01 GOTO1 VDICTAT,APPARM,C'SL  ',FRMBGN                                    
         MVCDD FRMCHD1,AC#RANGE                                                 
         OI    FRMCHD1H+6,FVOXMT                                                
         OI    FRMCHD2H+6,FVOXMT                                                
         TM    INOPT1,INOBUD       DO WE WANT BUDGET HEADING?                   
         BZ    *+10                                                             
         MVCDD FRMCHD1,AC#BGTS                                                  
         TM    INOPT1,INOTYPE      DO WE WANT TYPE HEADING?                     
         BZ    *+10                                                             
         MVCDD FRMCHD1,AC#TYPE                                                  
         TM    INOPT1,INOFCUR      DO WE WANT FOREIGN CURRENCY?                 
         BZ    *+10                                                             
         MVCDD FRMCHD1(8),AC#CURRC                                              
         MVC   FRMCHD2,FRMCHD1                                                  
         GOTO1 VDICTAT,APPARM,C'SL  ',FRMCHD1                                   
         GOTO1 VDICTAT,APPARM,C'SL  ',FRMCHD2                                   
*                                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,FRMCODEH                                                   
         MVC   RESKFORM,FVIFLD     FORMAT                                       
         BE    VALKEY15                                                         
         TM    FRMCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   FRMCODE,SAVFORM                                                  
         OI    FRMCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         TM    TWAMODE,TWAMDFR     SECOND PASS ?                                
         BO    *+10                YES, SKIP                                    
         MVC   SAVEKEY(L'RESKEY),RESKEY                                         
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORDD+IOACCFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         TM    IOERR,IOERNF        RECORD NOT FOUND ON FILE ?                   
         BO    VALKEY28            YES, SO OK TO ADD RECORD                     
*                                                                               
*&&UK*&& CLI   APACTN,ACTADD       ACTION ADD                                   
*&&UK*&& BE    VALKEY20            YES - DON'T SWAP TYPE                        
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT    TRANSMIT                                     
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         MVI   APINDS,APIOKDIS                                                  
*                                                                               
VALKEY20 CLI   APACTN,ACTDIS                                                    
         BE    VALKEY99                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY30                                                         
         TM    TWAMODE,TWAMDFR     SECOND PASS OF COPY                          
         BO    VALKEY25            YES, PROCESS SECOND PASS                     
         MVC   FVXTRA(L'SCRACT),SCRACT                                          
         TM    IOERR,IOEDEL        IS RECORD MARKED DELETED?                    
         BZ    VALKEY98            NO SO OK TO COPY                             
         B     IVALCDEL            YES CAN'T COPY A DELETED RECORD              
*                                                                               
VALKEY25 TM    IOERR,IOERNF        RECORD ON FILE?                              
         BZ    IVALNADD            YES, CAN'T ADD RECORD                        
*                                                                               
VALKEY28 MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         OI    FRMCODEH+6,FVOXMT   TRANSMIT FORMAT FIELD                        
         MVI   NEWKEY,YES                                                       
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
         B     VALKEY98                                                         
*                                                                               
VALKEY30 CLI   APACTN,ACTADD                                                    
         BE    IVALRECX                                                         
         OI    APINDS,APIOKCHA                                                  
         CLI   APACTN,ACTCHA                                                    
         BE    VALKEY40                                                         
         MVI   APINDS,APIOKDIS+APIOKDEL+APIOKRES                                
         B     VALKEY98                                                         
*                                                                               
VALKEY40 LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKS5TH                                                 
         MVC   IOADDR,AIOAREA0                                                  
         GOTO1 AIO,IORD+IOACCFIL+IOLOCK                                         
         BNE   *+8                                                              
         MVI   HAVE5TH,YES                                                      
         LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKSREG    RESTORE AS NORMAL                            
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
*=====================================================================*         
*        ROUTINE TO DISPLAY KEY                                       *         
*=====================================================================*         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVI   SVSELACT,ACTSEL                                                  
         MVC   FRMCODE,RESKFORM                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVI   APPFKEY,0                                                        
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO VALIDATE RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   RESKEY,APRECKEY                                                  
         MVI   DOWNOPT,NO          DEFAULT                                      
         XC    RANKON,RANKON       INIT RANK ON                                 
         XC    ORANKON,ORANKON     INIT RANK ON (OLD)                           
         MVI   RANKCOL,0           INIT RANK COL                                
*                                                                               
         GOTO1 AFVAL,FRMNMEH                                                    
         BNE   VALREC10                        NAME HAS NOT BEEN INPUT          
         GOTO1 ADDNAME,APPARM,(R2),FRMNMEH     ADD  FORMAT  NAME                
         BNE   VALREC99                        ON   ERROR,  EXIT                
*                                                                               
VALREC10 GOTO1 AFVAL,FRMTYPEH                                                   
         BE    VALREC15            DATA INPUT SO VERIFY                         
         OI    FRMTYPEH+6,FVOXMT                                                
         CLI   NEWKEY,YES          IS A NEW RECORD?                             
         BNE   IVALIPUT            NOT ADDING RECORD, SO ERROR                  
         MVC   FRMTYPE(L'APREPUL),APREPUL             SET DEFAULTS              
         CLI   APMLDGR,YES                                                      
         BE    VALREC15                                                         
         MVC   FRMTYPE(L'APREPTYP),APREPTYP                                     
         GOTO1 AFVAL,FRMTYPEH                                                   
*                                                                               
VALREC15 CLI   APMLDGR,YES         MULTIPLE LEDGER TYPE ?                       
         BE    VALREC30            YES, USE FIELD DIRECTLY FROM SCREEN          
         L     R3,ACTYPTAB         LOAD REPORT INFO TABEL ADDRESS               
*                                                                               
         USING REPTABD,R3                                                       
VALREC20 CLI   REPCODE,EOT         END OF TABLE                                 
         BE    IVALTYPE            NO GOOD                                      
         GOTO1 =A(EXPRPTY),(R3),RR=APRELO                                       
         CLC   APREPCDE,FRMRPCDE                                                
         BNE   VALREC22                                                         
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXCLC R1,FRMRPTYP,FVIFLD                                               
         BNE   VALREC22                                                         
         MVI   LEDGTYPH,10                                                      
         MVC   LEDGTYP,REPUL                                                    
         GOTO1 MAKELIST,APPARM,('RFLLDG',LEDGTYPH),AIOAREA1,           X        
               ('MAXPARM',BLOCK),APELEM                                         
         MVC   APREPNUM,REPNUM     SAVE POSSIBLE REPORT NUM                     
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALREC50                                                         
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BO    VALREC50            YES, SO OK                                   
*                                                                               
VALREC22 AHI   R3,REPLNQ           BUMP UP IN TABLE                             
         B     VALREC20                                                         
         DROP  R3                                                               
*                                                                               
VALREC30 GOTO1 MAKELIST,APPARM,('RFLLDG',FRMTYPEH),AIOAREA1,           X        
               ('MAXPARM',BLOCK),APELEM                                         
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    IVALCULR            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         CLI   NPARMS,MAXLDG       TOO MANY LEDGERS ?                           
         BH    IVAL2MNY            YES, ERROR TOO MANY PARAMETERS               
*                                                                               
         USING RFLELD,R1                                                        
         LA    R1,APELEM           ->   FILTER DATA                             
         TM    RFLIND,RFLXCLD      EXCLUDE REQUESTED ?                          
         BO    IVALCULR            EXCLUDE NOT ALLOWED                          
         DROP  R1                                                               
*                                                                               
         XC    MULTLDG,MULTLDG     CLEAR                                        
         SR    R0,R0                                                            
         IC    R0,NPARMS                                                        
         LA    R4,BLOCK            BLOCK HAS UP TO 10 UNIT/LEDGERS              
         L     R3,ACTYPTAB         LOAD REPORT INFO TABEL ADDRESS               
*                                                                               
         USING REPTABD,R3                                                       
VALREC36 CLI   REPCODE,EOT                                                      
         BE    IVALCUL0                                                         
         GOTO1 =A(EXPRPTY),(R3),RR=APRELO                                       
         CLC   APREPCDE,FRMRPCDE   MATCH REPORT TYPE ?                          
         BNE   VALREC38            NO,  SO GET NEXT REPORT TYPE                 
         CLI   0(R4),2             MUST BE LENGTH TWO FOR NOW                   
         BNE   IVALCUL0            NO,  INVALID UNIT/LEDGER                     
         CLI   REPNUM,C'M'         MULTIPLE LEDGER ?                            
         BE    VALREC38            YES, SKIP THIS TYPE                          
         CLC   REPUL,12(R4)                                                     
         BNE   VALREC38            NO,  SO GET NEXT REPORT TYPE                 
         TM    REPFLAG,REPDDS      DDS  ONLY ?                                  
         BZ    VALREC40            NO,  SO OK                                   
         TM    CUSTAT,CUSDDS       IS   IT A DDS TERMINAL ?                     
         BO    VALREC40            YES, SO OK                                   
*                                                                               
VALREC38 AHI   R3,REPLNQ           BUMP TO NEXT REPORT TYPE                     
         B     VALREC36                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALREC40 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),REPUL    UNIT/LEDGER                                  
         GOTO1 AIO,IOREAD+IOACCFIL+IO2                                          
         BNE   IVALCUL0            NOT  A VALID LEDGER FOR COMPANY              
         DROP  R2                                                               
*                                                                               
         MVC   TEMPLDG,REPIND      GET   THIS LEDGER'S BIT NUMBER               
         NC    TEMPLDG,MULTLDG     'AND' WITH CURRENT MULTI-LEDGER BITS         
         BNZ   IVALEDUP            NOT   ZERO, ERROR DUPLICATE LEDGER           
         OC    MULTLDG,REPIND      GOOD, SAVE THIS LEDGER'S BIT NUMBER          
*                                                                               
         LA    R4,32(,R4)          BUMP TO NEXT PARAMETER                       
         MVC   APREPNUM,REPNUM     SAVE POSSIBLE REPORT NUM                     
         L     R3,ACTYPTAB         RE-LOAD REPORT INFO TABEL ADDRESS            
         BCT   R0,VALREC36         LOOP TO NEXT LEDGER                          
         DROP  R3                                                               
*                                                                               
VALREC50 L     R2,AIOAREA1         ADD LIST TYPE X'C5' ELEMENT                  
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON  ERROR, EXIT                              
         CLI   APMLDGR,YES         CAN IT BE MORE THAN ONE LEDGER ?             
         BNE   VALREC52            NO                                           
         CLI   NPARMS,1            ONLY ONE LEDGER INPUT ?                      
         BE    VALREC52            YES                                          
         MVI   APREPNUM,C'M'       NO, SO MARK AS MULTIPLE LEDGER               
*                                                                               
VALREC52 GOTO1 ADDREPTY,APPARM,(R2)                                             
         BNE   VALREC99            ON    ERROR, EXIT                            
         GOTO1 GETTYPE,(R2)        RESET TYPE                                   
*                                                                               
         MVI   DWNTYPE,0                                                        
         USING RPFELD,R2                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ     X'C4'  PROFILE ELEMENT                       
         GOTO1 GETEL                                                            
         BNE   VALREC55                                                         
         LR    R2,R1                                                            
         MVC   ORANKON,RPFRKON     SAVE RANK ON (OLD)                           
         MVC   RANKON,RPFRKON      SAVE RANK ON                                 
         MVC   RANKCOL,RPFRKCL     SAVE RANK COLUMN                             
         TM    RPFDNOPT,RPFDDOWN   DOWNLOADING ONLY REPORT?                     
         BZ    *+8                 NO                                           
         MVI   DOWNOPT,YES                                                      
                                                                                
         TM    RPFXMIT,RPFXACNT    ACCENT ENABLED ?                             
         BZ    *+12                                                             
         OI    DWNTYPE,DWNTACNT                                                 
         B     VALREC55                                                         
         TM    RPFXMIT,RPFXQREP    QREPORTS ENABLED ?                           
         BZ    *+8                                                              
         OI    DWNTYPE,DWNTQREP                                                 
                                                                                
         DROP  R2                                                               
*                                                                               
VALREC55 DS    0H                                                               
         EJECT ,                                                                
***********************************************************************         
*  BUILD HEADING ELEMENTS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RHDELD,R2                                                        
         USING HEADD,R4                                                         
         MVI   REPMODE,REPHEAD                                                  
         L     R2,AIOAREA1                                                      
         AH    R2,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
VALHED10 IC    RF,RHDLN                                                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    VALHED20                                                         
         CLI   0(R2),RHDELQ        HEADING ELEMENT                              
         BNE   VALHED18                                                         
*                                                                               
         L     R4,=A(HEADTAB)      TABLE OF HEADING FIELDS                      
         A     R4,APRELO           RELOCATE IT                                  
*                                                                               
VALHED14 ICM   RE,15,HEADFLD       END OF TABLE                                 
         BZ    VALHED18            YES SO GET NEXT ELEMENT                      
         CLC   HEADTYPE,RHDTYPE    IS IT A CENTER HEADING?                      
         BNE   VALHED16            DON'T DELETE                                 
         CLC   HEADSEQ,RHDSEQ      IS IT SEQUENCE NUMBER?                       
         BNE   VALHED16            DON'T DELETE                                 
         LR    R1,RF                                                            
         SHI   R1,(RHDLNQ+1)                                                    
         BM    VALHED17            IF HEADING > FIELD SIZE THEN                 
         CLM   R1,1,HEADLN         DON'T DELETE THIS ONE                        
         BNH   VALHED17            ELSE DELETE ELEMENT                          
         A     RE,ATWA             POINT TO FIELD                               
         OI    1(RE),X'20'         MAKE LIKE PROTECTED FOR 1ST TIME IN          
*                                                                               
VALHED16 LA    R4,HEADLNQ(,R4)                                                  
         B     VALHED14                                                         
*                                                                               
VALHED17 MVI   RHDEL,X'FF'         MARK FOR DELETION                            
*                                                                               
VALHED18 AR    R2,RF                                                            
         B     VALHED10                                                         
*                                                                               
VALHED20 L     R2,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE THOSE MARKED                          
         GOTO1 DELEL,(R2)                                                       
*                                                                               
         L     R4,=A(HEADTAB)      TABLE OF HEADING FIELDS                      
         A     R4,APRELO           RELOCATE IT                                  
*                                                                               
VALHED30 ICM   R3,15,HEADFLD                                                    
         BZ    VALROWS             FINISHED                                     
         AR    R3,R5               ADD BASE OF SCREEN                           
         TM    1(R3),X'20'         IS IT PROTECTED?                             
         BO    VALHED40            YES, SO SKIP IT                              
         GOTO1 AFVAL,(R3)                                                       
         BNE   VALHED40                                                         
         XC    APELEM,APELEM                                                    
         LA    R2,APELEM                                                        
         MVI   RHDEL,RHDELQ        ELEMENT CODE                                 
         MVC   RHDTYPE,HEADTYPE    TYPE OF DATA                                 
         MVC   RHDSEQ,HEADSEQ      SEQUENCE                                     
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FVXLEN           INPUT DATA LENGTH                            
         EXMVC R1,RHDDATA,FVIFLD                                                
         LA    R1,RHDLNQ+1(,R1)    ADD STANDARD LENGTH TO DATA LENGTH           
         STC   R1,RHDLN            ELEMENT LENGTH                               
         CLI   FVIFLD,C'&&'        DATA DEFINITION                              
         BE    *+12                                                             
         OI    RHDFRM,RHDFREE      FREE FORM                                    
         B     VALHED35                                                         
*                                                                               
         OC    RHDDATA,SPACES             CAPS ON                               
         OI    RHDFRM,RHDDEF              DEFINITION                            
*                                                                               
         USING DEFTABD,R6                                                       
         GOTO1 VALDEF,(R3)                INVALID INPUT                         
         BNE   IVALKYWD                   CODE DOESN'T MATCH                    
         LR    R6,R1                                                            
         CLI   HEADTYPE,RHDFTLN    IS IT A FOOTLINE                             
         BNE   VALHED35                                                         
         TM    DEFIND,DEFNOFUT     NOT VALID AS FOOTLINE                        
         BO    IVALFOOT                                                         
         B     VALHED35                                                         
         DROP  R6                                                               
*                                                                               
VALHED35 L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99                                                         
*                                                                               
VALHED40 LA    R4,HEADLNQ(,R4)     R3 TO NEXT HEADFLD ENTRY                     
         B     VALHED30            NO, SO LOOP                                  
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  BUILD ROW ELEMENTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RRWELD,R2                                                        
         USING ROWD,R3                                                          
         USING DEFTABD,R4                                                       
VALROWS  MVI   REPMODE,REPROW                                                   
         MVC   SAV@RHLP,CUR@RHLP   SAVE RETURN    FROM HELP ADDR                
         MVI   MIDLNFG,NO          INITALIZE TO NO MIDLINE ENCOUNTERED          
         MVI   NROWS,0             ZERO ROWS SO FAR                             
         MVI   STRROW#,1           NUMBER OF ROW (UP TO 3)                      
         MVI   MIXESTK,0                                                        
         LR    RF,R5                                                            
         AHI   RF,SAVOIND-TWAD                                                  
         MVI   0(RF),0                                                          
         LA    R3,FRMROW1H                                                      
*                                                                               
VALROW20 CLI   NROWS,3                                                          
         BNL   VALROW70            FINISHED                                     
         ZIC   RF,NROWS                                                         
         AHI   RF,1                                                             
         STC   RF,ELEMSEQ          ROW NUMBER                                   
         MVI   APELCODE,RRWELQ     ROW ELEMENT                                  
         L     R1,AIOAREA1                                                      
         GOTO1 GETEL                                                            
         BNE   VALROW23            ROW ELEMENT DOESN'T EXISTS                   
*                                                                               
         USING RRWELD,R2                                                        
         LR    R2,R1                                                            
         TM    RRWOPT2,RRWPGNO     NEW PAGE NUMBER ON ROW CHANGE?               
         BZ    *+10                                                             
         MVC   PAGEROW,RRWSEQ      SAVE ROW SEQUENCE                            
         MVI   0(R2),X'FF'         MARK DELETED                                 
         GOTO1 AFVAL,ROWDATAH      ANY INPUT ?                                  
         BE    VALROW21            YES                                          
         ZIC   R6,RRWSEQ           NO, ADJUST 5TH RECORD ELEMENTS               
         GOTO1 =A(FIXXTREL),APPARM,(R6),-1,RR=APRELO                            
         GOTO1 =A(FIX5TH),APPARM,(R6),-1,RR=APRELO  DELETE ADJUST               
         B     VALROW22                                                         
*                                                                               
VALROW21 GOTO1 =A(MATCHON),APPARM,(RRWDATLN,RRWNDATA),ROWDATA,RR=APRELO         
         BE    VALROW22            KEYWORD HAS NOT CHANGED                      
         ZIC   R6,RRWSEQ                                                        
         GOTO1 =A(FIX5TH),APPARM,(R6),0,RR=APRELO  DELETE ONLY                  
*                                                                               
VALROW22 L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE ROW MARKED FOR DELETION               
         GOTO1 DELEL                                                            
*                                                                               
VALROW23 GOTO1 AFVAL,ROWDATAH                                                   
         BNE   VALROW63                    BUMP TO NEXT FIELD                   
         XC    DEFENTRY,DEFENTRY                                                
         GOTO1 VALDEF,ROWDATAH                                                  
         BNE   IVALKYWD            INVALID KEYWORD                              
         ST    R1,DEFENTRY         SAVE ADDRESS OF DEFINITION FOUND             
         LR    R4,R1               R1=POINTS TO DEFINITION ENTRY                
         XC    APELEM,APELEM                                                    
         LA    R2,APELEM                                                        
         MVI   RRWEL,RRWELQ       ELEMENT CODE                                  
         MVI   RRWLN,RRWNLNQ      ELEMENT LENGTH                                
         SR    RF,RF                                                            
         IC    RF,STRROW#                                                       
         SR    R1,R1                                                            
         IC    R1,NROWS                                                         
         AR    R1,RF                                                            
         STC   R1,RRWSEQ                                                        
         IC    RF,FVXLEN           EXECUTE LENGTH                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RRWNDATA(0),FVIFLD  ROW DATA                                     
         AHI   RF,RRWNLNQ+1        NEW SIZE OF RRWEL                            
         STC   RF,RRWLN                                                         
         MVC   RRWDATLN,FVILEN     LENGTH OF DATA                               
         MVI   RRWOPT,CDE+NME      DEFAULT ROW INDICATOR VALUES                 
         NC    RRWOPT,DEFRWIND     MAKE SURE NME OR CDE IS VALID                
         OI    RRWOPT,RRWNEWEL                                                  
         NI    RRWOPT2,TURNOFF-RRWAIDX     Reset                                
*                                                                               
         CLC   PAGEROW,RRWSEQ      IS IT SAME ROW?                              
         BNE   *+8                                                              
         OI    RRWOPT2,RRWPGNO     TURN ON OPTION                               
*&&UK                                                                           
         TM    DEFTYPE,DEFPRD      PERIOD TYPE KEYWORD                          
         BZ    *+8                                                              
         OI    RRWOPT,RRWPRDTY     YES - MUST ENTER PERIOD RANGE                
*&&                                                                             
         TM    DEFTYP2,DEFCACI     TEST CONTRA A/C KEYWORD                      
         BZ    *+14                                                             
         LR    RF,R5                                                            
         AHI   RF,SAVOIND-TWAD     SAVED STORAGE                                
         OI    0(RF),SAVOCAC       SET CONTRA A/C KEYWORD IN USE                
         LA    RF,0                                                             
         TM    MIXESTK,DEFOEST+DEFNEST                                          
         BNM   *+8                                                              
         LA    RF,1                HAVE OLD *OR* NEW EST KEYWORD                
*                                                                               
         OC    MIXESTK,DEFTYP2                                                  
         TM    DEFTYP2,DEFCACR     TEST ROW REQUIRES C/A KEYWORD                
         BZ    *+12                                                             
         LA    RE,ROWDATAH                                                      
         ST    RE,FULL             SAVE CURSOR POS. IN CASE OF ERROR            
*                                                                               
         CHI   RF,0                TEST HAD OLD OR NEW EST KEYWORDS             
         BE    VALROW24            NO                                           
         TM    MIXESTK,DEFOEST+DEFNEST  TEST NOW HAVE OLD *AND* NEW             
         BNO   VALROW24                                                         
         LA    RE,ROWDATAH                                                      
         ST    RE,FULL             SAVE CURSOR POS. FOR ERROR                   
*                                                                               
*                                                                               
VALROW24 CLI   DEFDATES,0          ANY  VALID DATA BASES ?                      
         BE    VALROW25            NO,  SKIP                                    
         MVI   RRWDATES,RRWTRDT    DEFAULT TO TRANSACTION DATE BASIS            
         TM    DEFDATES,DEFTRDT    TRANSACTION BASIS VALID ?                    
         BO    VALROW25            YES, CONTINUE                                
         MVI   RRWDATES,RRWMODT    DEFAULT TO MOA DATE BASIS                    
         TM    DEFDATES,DEFMODT    MOA  BASIS VALID ?                           
         BO    VALROW25            YES, CONTINUE                                
         DC    H'00'               NO,  KEYWORD DEFINITION TABLE ERROR          
*                                                                               
VALROW25 GOTO1 VSCANNER,APPARM,ROWDATAH,(6,BLOCK),SCNP3NEQ                      
         SR    R0,R0                                                            
         IC    R0,APPARM+4                                                      
         BCTR  R0,0                SUBTRACT ONE FOR KEYWORD                     
         STC   R0,NPARMS                                                        
         CLC   NPARMS,DEFRWMIN     TOO FEW PARAMETERS                           
         BL    IVALLOW                                                          
         CLC   NPARMS,DEFRWMAX     TOO MANY PARAMETERS                          
         BH    IVALHIGH                                                         
         CLI   NPARMS,0            ANY PARAMETERS                               
         BE    VALROW34            NO, FINISHED                                 
         LA    R6,BLOCK+32         POINT TO FIRST PARAMETER TO VALIDATE         
*                                                                               
*&&US*&& CLC   DEFDDNUM,=AL2(AC#RSUSC)  USER FIELD (SPECIAL) ?                  
*&&US*&& BE    VALROW28                 NO,  PROCESS PARAMETERS                 
         CLC   DEFDDNUM,=AL2(AC#RSUSF)                                          
         BNE   VALROW30                 NO,  PROCESS PARAMETERS                 
*                                       YES, VALIDATE USER FIELDS               
VALROW28 GOTO1 =A(VALRUF),RR=APRELO                                             
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS?                                 
         BE    VALROW34            NO,  CONTINUE                                
         B     VALREC99                                                         
*                                                                               
VALROW30 DS    0H                  VALIDATE THE PARAMETERS                      
         GOTO1 VALPARM,APPARM,(R2),(NPARMS,BLOCK),DEFENTRY,0                    
         BNE   VALREC99                                                         
*                                                                               
VALROW34 GOTO1 AFVAL,ROWTYPEH                                                   
         BE    VALROW38                                                         
         MVI   ROWTYPE,RRWLHEAD    DEFAULT TO LEFT HEADING                      
         CLI   MIDLNFG,YES         DID WE ENCOUNTER A MID YET?                  
         BNE   VALROW38            NO                                           
         MVI   ROWTYPE,RRWMID      DEFAULT TO MID THEN                          
*                                                                               
VALROW38 MVC   RRWTYPE,ROWTYPE     SAVE ROW TYPE IN ELEMENT                     
         CLI   ROWTYPE,RRWMID      WAS THERE A MIDLINE?                         
         BNE   VALROW42            NO                                           
         MVI   MIDLNFG,YES         YES, SO SET FLAG                             
         TM    RRWOPT,RRWADR                                                    
         BO    VALROW40                                                         
*&&US*&& TM    RRWOPT3,RRWBDR                                                   
         BZ    VALROW50                                                         
VALROW40 GOTO1 AFVAL,ROWDATAH      RESET CURSOR TO DATA FIELD                   
         B     IVALADR                                                          
*                                                                               
VALROW42 CLI   MIDLNFG,NO          DID WE HAVE A MIDLINE YET?                   
         BNE   IVALHEAD                                                         
         CLI   RRWTYPE,RRWLHEAD    PRINT AS LEFT HEAD?                          
         BE    *+8                                                              
         CLI   RRWTYPE,RRWCHEAD    PRINT AS CENTER HEAD?                        
         BE    *+8                                                              
         CLI   RRWTYPE,RRWRHEAD    PRINT AS RIGHT HEAD?                         
         BE    *+8                                                              
         CLI   RRWTYPE,NO          Suppress row?                                
         BNE   IVALTYPE            No such type                                 
         OI    RRWOPT,RRWPAGE      TURN ON BIT TO PAGE ON HEADINGS              
*                                                                               
VALROW50 GOTO1 AFVAL,ROWTOTLH                                                   
         BE    *+10                                                             
         MVC   ROWTOTL,APNO        SET DEFAULT NOT TO TOTAL                     
*&&US                                                                           
         CLI   ROWTOTL,C'I'        Accent indexing                              
         BNE   VALROW51                                                         
         OI    RRWOPT2,RRWAIDX     Row indexing on for Accent                   
         B     VALROW55                                                         
*&&                                                                             
*        CLI   ROWTOTL,C'I'        Accent indexing                              
*        BNE   VALROW51                                                         
*        OI    RRWOPT2,RRWAIDX     Row indexing on for Accent                   
*        B     VALROW55                                                         
                                                                                
VALROW51 CLC   ROWTOTL,APNO        TOTAL ON ROW?                                
         BE    VALROW55            NO                                           
         OI    RRWOPT,RRWTOT       SET BIT TO TOTAL ON ROW                      
         CLC   ROWTOTL,APYES       WAS IT ACTUALLY SET TO YES                   
         BE    VALROW55            YES                                          
         CLI   ROWTOTL,C'S'        TOTAL ON SEPERATE PAGE?                      
         BNE   VALROW52                                                         
         OI    RRWOPT,RRWTOTSP     SET BIT TO PAGE FOR TOTALS                   
         B     VALROW55                                                         
*                                                                               
VALROW52 CLI   ROWTOTL,C'B'        TOTAL ON BOTTOM OF PAGE?                     
         BNE   IVALTOT             NOT A VALID OPTION LETTER                    
         OI    RRWOPT2,RRWBTM      YES                                          
*                                                                               
VALROW55 GOTO1 AFVAL,ROWPRFXH      ROW PREFIX                                   
         BNE   VALROW60                                                         
         CLI   ROWTYPE,RRWMID      WAS THERE A MIDLINE?                         
         BE    IVALIPUT                                                         
         SR    R1,R1                                                            
         IC    R1,RRWDATLN         GET LENGTH OF DATA                           
         LA    RE,RRWNDATA(R1)                                                  
         MVC   RRWPFXLN,FVILEN     SAVE LENGTH OF PREFIX                        
         IC    R1,FVXLEN           GET LENGTH OF PREFIX                         
         EXMVC R1,0(RE),FVIFLD                                                  
         LA    RE,1(R1,RE)         RE = END OF ELEMENT                          
         LA    RF,RRWEL            RF = START OF ELEMENT                        
         SR    RE,RF                                                            
         STC   RE,RRWLN                                                         
*                                                                               
VALROW60 L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99                                                         
         B     VALROW65            CONTINUE                                     
*                                                                               
VALROW63 CLI   APPFKEY,PFKHLP      HELP PF   KEY ?                              
         BNE   VALROW64            NO,  SKIP                                    
         CLI   HLPREPT,REPROW      HELP FOR  ROWS ?                             
         BNE   VALROW64            NO,  SKIP                                    
         LA    RF,ROWDATAH         ->   HEADER    FOR  DATA                     
         S     RF,ATWA             IS   THE  DELETED   LINE PRIOR    TO         
         C     RF,SAV@RHLP              THE  PASTE     LINE ?                   
         BNL   VALROW64            NO,  SKIP                                    
         L     RF,CUR@RHLP         YES, SUBTRACT  ONE  LINE                     
         SHI   RF,ROWLNQ           FROM THE                                     
         ST    RF,CUR@RHLP                             PASTE     LINE           
         ZIC   RF,SVINSSEQ         AND  SUBTRACT  ONE                           
         BCTR  RF,0                               FROM THE                      
         STC   RF,SVINSSEQ                             LINE NUMBER              
*                                  NULL LINE                                    
VALROW64 CLI   RANKON,C'R'         RANK ON   ROWS ?                             
         BNE   VALROW65            NO,  SKIP TO   NEXT ROW                      
         GOTO1 =A(FIXRPROF),APPARM,-1,RR=APRELO                                 
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
VALROW65 LA    R3,ROWLNQ(,R3)      BUMP TO   NEXT ROW                           
         SR    R1,R1                                                            
         IC    R1,NROWS                                                         
         LA    R1,1(,R1)           BUMP ROW  NUMBER                             
         STC   R1,NROWS            SAVE NEXT NUMBER                             
         B     VALROW20                                                         
*                                                                               
VALROW70 L     R2,AIOAREA1         RE-SEQUENCE THE ROWS                         
         MVI   APELCODE,RRWELQ                                                  
         LA    R6,1                START WITH 1                                 
         GOTO1 GETEL,(R2)                                                       
*                                                                               
VALROW72 LR    R2,R1                                                            
         BNE   VALROW99                                                         
         STC   R6,RRWSEQ                                                        
         LA    R6,1(,R6)                                                        
         GOTO1 NEXTEL                                                           
         B     VALROW72                                                         
*                                                                               
VALROW99 STC   R6,NROWS            SAVE NUMBER OF ROWS                          
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
*=====================================================================*         
*  BUILD COLUMN ELEMENTS                                              *         
*=====================================================================*         
         SPACE 1                                                                
         USING COLD,R3                                                          
VALCOLS  MVI   REPMODE,REPCOL      VALIDATE REPORT COLUMNS                      
         LA    R3,FRMCOL1H         FIRST COLUMN                                 
         MVI   STSEQ,1                                                          
         MVI   SECLVL,0                                                         
*&&UK*&& MVI   KWDINDS,0                                                        
         MVC   COL#,STSEQ                                                       
*                                                                               
VALCOL01 GOTO1 =A(SETLAST),RR=APRELO  FIND LAST COLUMN NUMBER (LSTCOL#)         
         CLI   APPFKEY,PFKINS         INSERT COLUMN ?                           
         BNE   VALCOL02                                                         
         CLI   LSTCOL#,MAXCOLS                                                  
         BE    IVALCMAX                                                         
         SPACE 1                                                                
***********************************************************************         
*  INSERT A SEQUENCE NUMBER IF PFKEY TO INSERT WAS USED               *         
***********************************************************************         
         SPACE 1                                                                
VALCOL02 DS    0H                                                               
         CLI   INSSEQ,0            WAS   A COLUMN INSERTED?                     
         BE    VALCOL04            NO                                           
         SR    R1,R1                                                            
         IC    R1,INSSEQ                                                        
         GOTO1 =A(INSCOLMN),RR=APRELO                                           
         BNE   VALREC99            ON ERRROR, EXIT                              
*                                                                               
VALCOL04 DS    0H                                                               
         LA    R0,MAXDISP          NUMBER OF COLUMNS ON SCREEN                  
         LA    R3,(MAXDISP-1)*COLLNQ                                            
         LA    R3,FRMCOL1H(R3)     POINT TO LAST COLUMN ON SCREEN               
*                                                                               
VALCOL05 DS    0H                                                               
         CLI   APPFKEY,PFKDEL      ARE WE GOING TO DELETE A LINE                
         BNE   VALCOL08                                                         
         L     RE,ACURSOR          CURRENT CURSOR LOCATION                      
         CR    RE,R3               IS CURSOR BEFORE THIS COLUMN                 
         BL    VALCOL07            NOT PROCESSED YET                            
         LA    RF,COLLNQ(,R3)                                                   
         CR    RE,RF               IS CURSOR BEFORE END OF END OF LINE          
         BH    VALCOL07            NO MUST HAVE ALREADY PROCESSED               
         STC   R0,APBYTE           SAVE CURRENT COLUMN NUMBER                   
*                                  CHECK COLUMN RANKING FOR ERRORS              
         GOTO1 =A(CHKCRANK),RR=APRELO                                           
         BNE   VALREC99            ON ERROR, STOP THE DELETE                    
         MVC   COLDATA,SPACES      CLEAR FIELD TO DELETE LINE                   
*                                                                               
VALCOL07 SHI   R3,COLLNQ                                                        
         BCT   R0,VALCOL05                                                      
*                                                                               
VALCOL08 XC    COLSORT#,COLSORT#   STORE SORT SEQUENCE, INDEX = SORT#           
         MVI   COLSORT#,C'*'       INITIALIZE SORT SEQUENCE                     
         XC    COLARRY2,COLARRY2   TO STORE FLAGS                               
         LA    R3,FRMCOL1H                                                      
*                                                                               
         USING RCLELD,R8                                                        
OLD      USING RCLELD,R1                                                        
*                                                                               
VALCOL10 XC    ELEMENT,ELEMENT                                                  
         LA    R8,ELEMENT                                                       
         L     R1,AIOAREA1                                                      
         MVI   NEWELEM,YES                                                      
         MVI   SVRCLOPT,0          CLEAR  OLD RCLOPT  BYTE                      
         MVI   SVRCLOP2,0          CLEAR  OLD RCLOPT2 BYTE                      
         MVI   SVRCLSPC,0          CLEAR  OLD RCLSPCL BYTE                      
         XC    SVRCLHD1,SVRCLHD1   CLEAR  OLD RCLHDL1 BYTES                     
         XC    SVRCLHD2,SVRCLHD2   CLEAR  OLD RCLHDL2 BYTES                     
         MVI   APELCODE,RCLELQ     X'C3' COLUMN ELEMENT                         
         MVC   ELEMSEQ,COL#        GET   ONLY THIS COLUMN ELEMENT               
         MVI   RCLLN,RCLNLNQ       DEFAULT LENGTH                               
         GOTO1 GETEL                                                            
         BNE   VALCOL18                                                         
*                                                                               
         TM    COLDATAH+4,FVITHIS  ANY  INPUT - DATA ?                          
         BO    VALCOL11            YES, UPDATED                                 
         TM    COLDATEH+4,FVITHIS  ANY  INPUT - DATE ?                          
         BO    VALCOL11            YES, UPDATED                                 
         TM    COLWIDEH+4,FVITHIS  ANY  INPUT - WIDTH ?                         
         BO    VALCOL11            YES, UPDATED                                 
         TM    COLTOTLH+4,FVITHIS  ANY  INPUT - TOTAL ?                         
         BO    VALCOL11            YES, UPDATED                                 
         TM    COLPRNTH+4,FVITHIS  ANY  INPUT - PRINT ?                         
         BZ    VALCOL12            NO,  CONTINUE                                
*----------------------------------------------------------------*              
*        REMOVE WARNINGS FROM COLUMNS FOR THE FOLLOWING          *              
*----------------------------------------------------------------*              
VALCOL11 DS    0H                                                               
         ST    R1,SAVER1                                                        
*                                  1 = EQUATION TRUNCATED                       
         GOTO1 ADELWARN,LWPARM,(COL#,ACWTRUNC),('MNOMTWRN',0)                   
*                                                                               
         LHI   RE,ACWCDELD                                                      
         STH   RE,LWPARM+2                                                      
         BASR  RE,RF               2 = COLUMN POINTED TO WAS DELETED            
*                                                                               
         LHI   RE,ACWCSUD                                                       
         STH   RE,LWPARM+2                                                      
         BASR  RE,RF               3 = COLUMN STACKED UNDER WAS DELETED         
*                                                                               
         LHI   RE,ACEPRM                                                        
         STH   RE,LWPARM+2                                                      
         MVI   LWPARM+4,MNOMTERR   ERROR TYPE                                   
         BASR  RE,RF               1402 = INVALID PARAMETER &T                  
*                                                                               
         L     R1,SAVER1           RESTORE REGISTER 1                           
*---------------------------------------------------------------------*         
*     NOTE: SINCE X'C9' ELEMENTS FOLLOW  X'C3' ELEMENTS, ANY DELETE   *         
*           WOULD NOT AFFECT THE FOLLOWING LOGIC                      *         
*---------------------------------------------------------------------*         
VALCOL12 MVC   SVRCLOPT,OLD.RCLOPT      SAVE OLD   RCLOPT                       
         MVC   SVRCLOP2,OLD.RCLOPT2     SAVE OLD   RCLOPT2                      
         MVC   SVRCLSPC,OLD.RCLSPCL     SAVE OLD   RCLSPCL                      
*        MVC   SVRCLLN,OLD.RCLLN        SAVE OLD   RCLLN                        
         SR    RF,RF                                                            
         IC    RF,OLD.RCLDATLN     GET LENGTH (VARIABLE NOW)                    
         LA    RE,OLD.RCLNDATA(RF) POINT TO HEADLINE 1                          
         ICM   RF,1,OLD.RCLHD1LN   LENGTH OF HEADING 1                          
         BZ    VALCL12A                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVRCLHD1(0),0(RE)                                                
         LA    RE,1(RF,RE)         POINT TO NEXT HEADING                        
*                                                                               
VALCL12A ICM   RF,1,OLD.RCLHD2LN   LENGTH OF HEADING 2                          
         BZ    VALCL12B            NONE                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVRCLHD2(0),0(RE)                                                
*                                                                               
VALCL12B IC    RF,OLD.RCLDATLN     GET LENGTH (VARIABLE NOW)                    
         LA    R4,OLD.RCLNDATA     ELEMENT DATA                                 
*                                                                               
VALCOL13 ST    R1,SAVER1                                                        
         GOTO1 =A(MATCHON),APPARM,((RF),(R4)),COLDATA,RR=APRELO                 
         BNE   VALCOL18                                                         
         L     R1,SAVER1           RESTORE REGISTER 1                           
         MVI   NEWELEM,NO                                                       
         SR    RF,RF                                                            
         MVC   RCLHD1LN(RCLNDATA-RCLHD1LN),OLD.RCLHD1LN                         
         MVC   RCLOPT2,OLD.RCLOPT2      COPY OPTIONS                            
         MVC   RCLSORTN,OLD.RCLSORTN    COPY SORT FIELD PARAMETER               
         MVC   RCLPARA,OLD.RCLPARA      COPY PARMETER DATA                      
         DROP  OLD                                                              
*                                                                               
VALCOL18 GOTO1 AFVAL,COLDATAH      ANY  COLUMN DATA ?                           
         BNE   VALCOL84            NO, SO REMOVE COLUMN                         
*                                                                               
         CLI   NEWELEM,NO          NEW KEYWORD USED ?                           
         BE    VALCOL20            NO                                           
         L     R1,AIOAREA1         YES, SO DELETE OLD ONE                       
         MVI   APELCODE,RFLELQ     X'C5' FILTER DATA                            
         MVC   ELEMSEQ,COL#        DELETE ALL COLUMN FILTER FOR COL#            
         GOTO1 DELEL                                                            
*                                                                               
         ZIC   RF,COL#                                                          
         GOTO1 =A(FIX5TH),APPARM,(RF),0,RR=APRELO  DELETE ONLY                  
*                                                                               
VALCOL20 MVI   RCLEL,RCLELQ        X'C3'                                        
         MVC   RCLSEQ,COL#         CURRENT SEQUENCE NUMBER                      
         MVC   RCLDATLN,FVILEN     SAVE LENGTH OF INPUT                         
         SR    RF,RF                                                            
         IC    RF,FVXLEN               EXECUTE LENGTH                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RCLNDATA(0),FVIFLD      COLUMN DATA                              
         LA    RF,RCLNLNQ+1(,RF)       LENGTH OF ELEMENT SO FAR                 
         STC   RF,RCLLN                SAVE OF LENGTH SO FAR                    
*                                                                               
         CLI   NEWELEM,NO          NEW KEYWORD USED ?                           
         BNE   VALCOL25            YES                                          
         IC    RF,RCLDATLN                                                      
         LA    RE,RCLNDATA(RF)     POINT TO END OF RCLNDATA                     
         ICM   RF,1,RCLHD1LN       1ST HEADING                                  
         BZ    VALCOL22            USE SAVED LENGTH                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SVRCLHD1                                                 
         LA    RE,1(RF,RE)         POINT TO HEADING 2                           
*                                                                               
VALCOL22 ICM   RF,1,RCLHD2LN       2ND HEADING LENGTH                           
         BZ    VALCOL24                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SVRCLHD2                                                 
         LA    RE,1(RF,RE)         POINT TO END OF ELEMENT                      
*                                                                               
VALCOL24 LA    RF,RCLEL            RF = BEGINING OF ELEMENT                     
         SR    RE,RF               RE = END      OF ELEMENT                     
         STC   RE,RCLLN            SAVE OFF LENGTH OF ELEMENT                   
*---------------------------------------------------------------------*         
*   IF NONE OF THESE OPTIONS ARE ON THEN CLEAR AND RESET FROM SCREEN  *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
VALCOL25 TM    INOPT1,INOBUD+INOTYPE+INOFCUR                                    
         BNZ   VALCOL28                                                         
         MVI   RCLDTEFG,0          DATE PARAMETER INDICATOR                     
         MVI   RCLDATES,0          DATE BASIS     DATA                          
         XC    RCLSTDT,RCLSTDT     START DATE RANGE VALUES                      
         XC    RCLENDT,RCLENDT     END   DATE RANGE VALUES                      
*                                                                               
         USING DEFTABD,R4                                                       
VALCOL28 XC    DEFENTRY,DEFENTRY                                                
         GOTO1 VALDEF,COLDATAH                                                  
         BNE   VALCOL44                                                         
*                                                                               
* SET DEFAULT VALUES FROM KEYWORD DEFINITIONS                                   
*                                                                               
         LR    R4,R1               R4 POINTS AT DEFINITION AREA                 
         ST    R1,DEFENTRY         SAVE ADDRESS                                 
         MVC   RCLOPT,DEFCLOTH     TURN ON  COLUMN 1 INDICATORS                 
         NI    RCLOPT,RCLACCM+RCLEQU    SET DEFAULT VALUES                      
         OI    RCLOPT,RCLNEWEL                                                  
         MVI   RCLOPT2,0                INITIALIZE TO ZERO                      
         OC    RCLOPT2,DEFCLIND         TURN ON  DEFAULT VALUES                 
         NI    RCLOPT2,CDE+NME+RCLJOBR  ONLY ONES VALID                         
         NI    RCLOPT4,TURNOFF-RCLAIDX  Reset                                   
         TM    DEFTYP2,DEFNESH                                                  
         BZ    *+8                                                              
         OI    RCLOPT5,RCLNJESR    SET NEW ESTIMATE HOURS/RATE KEYWORD          
*                                                                               
         MVI   RCLSPCL,0           RESET SPECIAL VALUE                          
*&&UK                                                                           
         TM    DEFCLOTH,DEFCLNAC   NON ACCUMULATIVE COLFILT                     
         BZ    *+8                                                              
         OI    RCLOPT4,RCLNACCU                                                 
*                                                                               
         TM    DEFTYPE,DEFPRD      PERIOD TYPE KEYWORD                          
         BZ    *+8                                                              
         OI    RCLOPT4,RCLPRDTY    YES - MUST ENTER PERIOD RANGE                
*&&                                                                             
         TM    DEFCLIND,DEFCLEST                                                
         BZ    *+8                                                              
         OI    RCLOPT4,RCLNJEST    NON-JOBBER ESTIMATE TYPE KEYWORD             
*&&US                                                                           
         TM    DEFTYP2,DEFCACI     TEST CONTRA A/C KEYWORD                      
         BZ    *+14                                                             
         LR    RF,R5                                                            
         AHI   RF,SAVOIND-TWAD     SAVED STORAGE                                
         OI    0(RF),SAVOCAC       SET CONTRA A/C KEYWORD IN USE                
*&&                                                                             
         LA    RF,0                                                             
         TM    MIXESTK,DEFOEST+DEFNEST                                          
         BNM   *+8                                                              
         LA    RF,1                HAVE OLD *OR* NEW EST KEYWORD                
*                                                                               
         OC    MIXESTK,DEFTYP2                                                  
         TM    DEFTYP2,DEFCACR     TEST COL REQUIRES C/A KEYWORD                
         BZ    *+12                                                             
         LA    RE,COLDATAH                                                      
         ST    RE,FULL             SAVE CURSOR POS. IN CASE OF ERROR            
*                                                                               
         CHI   RF,0                TEST HAD OLD OR NEW EST KEYWORDS             
         BE    VALCOL30            NO                                           
         TM    MIXESTK,DEFOEST+DEFNEST  TEST NOW HAVE OLD *AND* NEW             
         BNO   VALCOL30                                                         
         LA    RE,COLDATAH                                                      
         ST    RE,FULL             SAVE CURSOR POS. FOR ERROR                   
*                                                                               
VALCOL30 TM    RCLOPT,RCLACCM      CANDIDATE FOR BUDGET OR TYPE ?               
         BO    VALCOL32            YES, SKIP                                    
         MVI   RCLDATES,0          DATE BASIS DATA                              
         MVI   RCLDTEFG,0          DATE PARAMETER INDICATOR                     
         XC    RCLENDT,RCLENDT     END   DATE RANGE VALUES                      
         MVI   RCLPDYR,0           1ST BYTE OF RCLSTDT                          
         CLC   DEFDDNUM,=AL2(AC#RSPPD) THIS IS AN EXCEPTION                     
         BE    VALCOL31                                                         
         MVI   RCLPDN1,0           2ND BYTE OF RCLSTDT                          
         B     VALCOL32                                                         
*                                                                               
VALCOL31 CLI   RCLPDN1,0           IS   VALUE   SET ?                           
         BNE   VALCOL32            YES, ALREADY SET                             
         MVI   RCLPDN1,1           NO,  SO DEFAULT TO ONE                       
*                                                                               
VALCOL32 DS    0H                                                               
*        CLI   DEFSPCL,DEFLVL1     DO    NOT DO FOR ACCT LEVEL                  
*        BNL   *+10                                                             
         MVC   RCLSPCL,DEFSPCL          SPECIAL KEYWORD                         
         OC    SECLVL,DEFSEC#                                                   
*&&UK                                                                           
         CLC   DEFDDNUM,=AL2(AC#RSDTE)   KEYWORD "TSDTE" ?                      
         BNE   *+8                                                              
         OI    RCLOPT4,RCLTSDTE                                                 
         CLC   DEFDDNUM,=AL2(AC#RSTSD)   KEYWORD "TSDAY" ?                      
         BNE   *+12                                                             
         OI    KWDINDS,KWDTSDAY                                                 
         ST    R3,SVDAYCOL               SAVE COLUMN FIELD                      
*                                                                               
         CLC   DEFDDNUM,=AL2(AC#BILLT)   KEYWORD "BILLT" ?                      
         BNE   *+12                                                             
         OI    KWDINDS,KWDBILLT                                                 
         ST    R3,SVBILCOL               SAVE COLUMN FIELD                      
*&&                                                                             
         CLI   DEFDATES,0                                                       
         BE    VALCOL34                                                         
         MVI   RCLDATES,RCLTRDT    SET DEFAULT TRANACTION DATE BASED            
         TM    DEFDATES,DEFTRDT    TRANSACTION BASIS ?                          
         BO    VALCOL34            YES                                          
         MVI   RCLDATES,RCLMODT    SET DEFAULT MOA        DATE BASED            
         TM    DEFDATES,DEFMODT    MOA         BASIS ?                          
         BO    VALCOL34            YES                                          
         DC    H'00'               SHOULD BE AT LEAST ONE OF THESE              
*                                                                               
VALCOL34 MVC   APPARM+8(2),=C',='                                               
         MVC   APPARM+10(1),SCCOMMA        GENERAL C','                         
         MVC   APPARM+11(1),APOPNPRN       GENERAL C'('                         
         GOTO1 VSCANNER,APPARM,COLDATAH,(6,BLOCK)                               
         SR    R0,R0                                                            
         IC    R0,APPARM+4                                                      
         BCTR  R0,0                                                             
         STC   R0,NPARMS                                                        
         OC    RCLSPCL,RCLSPCL     SPECIAL KEYWORD                              
         BZ    VALCOL35                                                         
         GOTO1 VALSPCL,APPARM,(R4),('MAXCOLS',BLOCK),(COL#,COLARRY2)            
         BNE   VALREC99                                                         
*                                                                               
VALCOL35 CLC   NPARMS,DEFCLMIN     TOO FEW PARAMS                               
         BL    IVALLOW                                                          
         CLC   NPARMS,DEFCLMAX     TOO MANY PARAMS                              
         BH    IVALHIGH                                                         
         CLI   NPARMS,0            ANY PARAMETERS                               
         BE    VALCOL38            NO, FINISHED                                 
         LA    R6,BLOCK+32         POINT TO FIRST PARAMETER TO VALIDATE         
*MN                                                                             
*&&US*&& CLC   DEFDDNUM,=AL2(AC#RSUSC)     USER FIELD (SPECIAL) ?               
*&&US*&& BE    VALCL35A                    NO,  PROCESS PARAMETERS              
         CLC   DEFDDNUM,=AL2(AC#RSUSF)     USER FIELD (SPECIAL) ?               
         BNE   VALCOL36                    NO,  PROCESS PARAMETERS              
*                                                                               
VALCL35A GOTO1 =A(VALRUF),RR=APRELO        YES, VALIDATE USER FIELDS            
         CLC   FVMSGNO,=AL2(FVFOK)         ANY  ERRORS?                         
         BE    VALCOL38                    NO,  CONTINUE                        
         B     VALREC99                                                         
*                                          VALIDATE THE PARAMETERS              
VALCOL36 GOTO1 VALPARM,APPARM,(R8),(NPARMS,BLOCK),DEFENTRY,0                    
         BNE   VALREC99                                                         
*&&UK                                                                           
         CLC   DEFDDNUM,=AL2(AC#RSDRA)   KEYWORD "DR" ?                         
         BNE   VALCOL38                                                         
         CLI   RCLDATES,RCLTRDT          PARAMETER "BILL" ?                     
         BNE   VALCOL38                                                         
         OI    KWDINDS,KWDDRBIL                                                 
*&&                                                                             
VALCOL38 MVC   FVMSGNO,=AL2(FVFOK)         RESET                                
*                                                                               
         MVI   NPARMS,0                                                         
         GOTO1 AFVAL,COLDATEH                                                   
         BNE   VALCOL42                   NO INPUT                              
         CLC   DEFDDNUM,=AL2(AC#RSPPD)    PDAY KEYWORD ?                        
         BE    VALCOL40                   YES                                   
         TM    RCLOPT,RCLACCM      IS   IT AN ACCUMULATED COLUMN ?              
         BZ    IVALIPUT            NO,  SO INVALID INPUT                        
*                                                                               
VALCOL40 GOTO1 VSCANNER,APPARM,COLDATEH,(4,BLOCK),SCNP3NEQ                      
         MVC   NPARMS,APPARM+4                                                  
*                                                                               
         CLC   DEFDDNUM,=AL2(AC#RSPPD)        PDAY KEYWORD ?                    
         BNE   VALCOL42                                                         
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         CLI   NPARMS,1            ONLY ONE INPUT ALLOWED                       
         BH    VALREC99                                                         
         TM    COLDATEH+4,FVINUM   WAS IT NUMERIC INPUT ?                       
         BZ    IVALNNUM                                                         
         LA    RE,BLOCK                                                         
         MVC   FVMSGNO,=AL2(1767)                                               
         CLC   4(4,RE),=AL4(1)                                                  
         BL    VALREC99                                                         
         CLC   4(4,RE),=AL4(MAXPDAY#)                                           
         BH    VALREC99                                                         
         MVC   RCLPDN1,7(RE)                                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALCOL50                                                         
*                                                                               
VALCOL42 GOTO1 =A(VALRANGE),APPARM,RR=APRELO      VALIDATE DATE RANGE           
         BE    VALCOL50                                                         
         B     VALREC99                                                         
*                                                                               
VALCOL44 LTR   R1,R1                                                            
         BNZ   IVALKYWD            KEYWORD IS NOT VALID                         
         GOTO1 =A(CHKEXP),RR=APRELO                                             
         BNE   VALREC99                                                         
*                                                                               
         MVI   RCLSPCL,0                                                        
         MVI   RCLOPT,RCLEQU+RCLNEWEL   DEFAULT TO THESE OPTIONS                
         MVI   RCLOPT2,0                                                        
         SR    R0,R0               SET FOR PARENTHESIS CHECK                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 =A(VALEXP),APPARM,FVIFLD,(RF),RCLEL,RR=APRELO                    
         BNE   VALCOL45                                                         
         LTR   R0,R0                                                            
         BNZ   IVALUBPR            R0=0 FOR BALANCED PARENTHESES                
         OI    RCLOPT,RCLACCM      TRUE ACCUMULATED COLUMN                      
         B     VALCOL48                                                         
*                                  DATE CALCULATIONS                            
VALCOL45 GOTO1 =A(DTECOMP),RR=APRELO                                            
         BNE   IVALEXP             NO,  INVALID EXPRESSION                      
         MVI   RCLSPCL,RCLSPDTE    MARK AS DATE1-DATE2 COLUMN                   
*                                  DATE CALCULATIONS                            
VALCOL48 GOTO1 AFVAL,COLDATEH      MAKE SURE NO DATA IN DATE RANGE              
         BE    IVALIPUT            YES, INVALID INPUT WITH CALCULATIONS         
*                                                                               
VALCOL50 GOTO1 AFVAL,COLWIDEH                                                   
         BNE   VALCOL53            NO INPUT, USE DEFAULT WIDTH                  
         TM    COLWIDEH+4,FVITHIS  INPUT THIS TIME IN ?                         
         BO    VALCOL55            YES, USE WIDTH                               
         CLI   NEWELEM,NO          NEW  ELEMENT ?                               
         BE    VALCOL52            NO,  SO USE WHAT WE HAD BEFORE               
         CLI   APACTN,ACTADD       ACTION ADD ?                                 
         BNE   VALCOL53            NO,  CONTINUE                                
         TM    COLDATAH+4,FVITHIS  NEW  COLUMN DATA ?                           
         BO    VALCOL53            YES, USE DEFAULT WIDTH                       
         B     VALCOL55            NO,  USE OLD WIDTH                           
*                                                                               
*                                  MAKE SURE NAME AND CODE ARE STILL            
*                                       THE  SAME                               
VALCOL52 MVC   TEMP1,RCLOPT2       GET  CURR NAME AND CODE                      
         NI    TEMP1,RCLNAME+RCLCODE                                            
         MVC   TEMP2,SVRCLOP2      GET  OLD  NAME AND CODE                      
         NI    TEMP2,RCLNAME+RCLCODE                                            
         CLC   TEMP1,TEMP2         ARE  THEY THE  SAME ?                        
         BE    VALCOL55            YES, USE  OLD WIDTH                          
*                                  NO,  USE  DEFAULT WIDTH                      
*                                                                               
VALCOL53 MVI   RCLWDTH,13          DEFAULT WIDTH                                
         ICM   R4,15,DEFENTRY                                                   
         BZ    *+10                                                             
         MVC   RCLWDTH,DEFWDTH                                                  
*                                                                               
         TM    RCLOPT2,NME         IS IT NAME ENTRY ON?                         
         BZ    VALCOL58                                                         
         MVI   RCLWDTH,36                                                       
         LTR   R4,R4                                                            
         BZ    VALCOL58                                                         
         CLI   DEFNWDTH,0          DEFAULT NAME WIDTH ZERO ?                    
         BE    *+10                YES, SKIP                                    
         MVC   RCLWDTH,DEFNWDTH    USE  DEFAULT NAME WIDTH                      
         B     VALCOL58                                                         
         DROP  R4                  DEFTABD                                      
*                                                                               
VALCOL55 GOTO1 =A(GETNUM),APPARM,COLWIDEH,0,RR=APRELO                           
         CLI   APPARM,X'00'        VALID     INPUT ?                            
         BNE   IVALNNUM            NO,  NOT  NUMERIC                            
         TM    APPARM+4,X'80'      NEGATIVE  VALUE ?                            
         BO    IVALIGE0            YES, NOT  >=    0                            
         MVC   RCLWDTH,APPARM+7    SAVE COL  WIDTH                              
*                                                                               
VALCOL58 GOTO1 AFVAL,COLDATAH                                                   
         CLC   FVILEN,RCLDATLN     DID LENGTH CHANGE ?                          
         BE    VALCOL64            NO                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN           LENGTH OF DATA                               
         EXMVC RF,RCLNDATA,FVIFLD                                               
         LA    RE,RCLNDATA+1(RF)   POINT PAST RCLNDATA                          
         CLI   NEWELEM,YES         NEW KEYWORD USED ?                           
         BE    VALCOL64            YES                                          
         SR    RF,RF                                                            
         ICM   RF,1,RCLHD1LN       HEADING 1 LENGTH                             
         BZ    VALCOL60                                                         
         BCTR  RF,0                                                             
         EXMVC RF,0(RE),SVRCLHD1   ATTACH HEADING 1                             
         LA    RE,1(RF,RE)         POINT TO END OF HEADING 1                    
*                                                                               
VALCOL60 ICM   RF,1,RCLHD2LN       HEADING 2 LENGTH                             
         BZ    VALCOL62                                                         
         EXMVC RF,0(RE),SVRCLHD2                                                
         LA    RE,1(RF,RE)                                                      
*                                                                               
VALCOL62 LA    RF,RCLEL            RF = START OF ELEMENT                        
         SR    RE,RF                                                            
         STC   RE,RCLLN            SAVE NEW LENGTH                              
*                                                                               
         USING DEFTABD,R4                                                       
VALCOL64 GOTO1 AFVAL,COLTOTLH                                                   
         BNE   VALCOL70                                                         
         TM    RCLOPT,RCLACCM      Can we total on this column ?                
         BO    VALCOL65            No                                           
         ICM   R4,15,DEFENTRY                                                   
         BZ    VALCOL66                                                         
         TM    DEFTYPE,DEFPACK     Specail case of pack but not accm            
         BZ    VALCOL66                                                         
         DROP  R4                  DEFTABD                                      
*                                                                               
VALCOL65 CLC   COLTOTL,APONLY      ONLY PRINT TOTAL NO DETAIL                   
         BNE   *+12                                                             
         OI    RCLOPT,SUPPRESS     SUPRESS DETAIL                               
         B     VALCOL70            NO                                           
*                                                                               
         CLI   COLTOTL,C'S'        ONLY  SHOW    DETAILS NO TOTALS ?            
         BNE   VALCOL68            NO,   PERFORM MORE  TESTS                    
         OI    RCLOPT,RCLNOTOT     SUPRESS TOTAL                                
         B     VALCOL70            CONTINUE                                     
                                                                                
VALCOL66 DS    0H                                                               
*&&US                                                                           
         CLI   COLTOTL,C'I'        Row indexing (Accent)                        
         BNE   VALCOL67                                                         
         OI    RCLOPT4,RCLAIDX     Accent indexing                              
         B     VALCOL70                                                         
*&&                                                                             
*        CLI   COLTOTL,C'I'        Row indexing (Accent)                        
*        BNE   VALCOL67                                                         
*        OI    RCLOPT4,RCLAIDX     Accent indexing                              
*        B     VALCOL70                                                         
                                                                                
VALCOL67 CLC   COLTOTL,APNO        TOTAL = 'NO'  SPECIFIED ?                    
         BE    VALCOL70            YES,  SKIP                                   
         CLC   COLTOTL,APYES       TOTAL = 'YES' SPECIFIED ?                    
         BNE   VALCOL68            NO,   PERFORM MORE  TESTS                    
         OI    RCLOPT2,RCLTOT      TURN  ON      INDICATOR TO TOTAL             
         B     VALCOL70            CONTINUE                                     
*                                                                               
VALCOL68 CLI   NEWELEM,YES         NEW   ELEMENT ?                              
         BNE   IVALTOT             NO,   INVALID TOTAL OPTION                   
         TM    COLTOTLH+4,FVITHIS  ANY   NEW     INPUT DATA ?                   
         BO    IVALTOT             YES,  INVALID TOTAL OPTION                   
*                                  NO,   IGNORE  LEFT  OVER DATA                
*                                                                               
VALCOL70 GOTO1 AFVAL,COLPRNTH                                                   
         BNE   VALCOL76                                                         
         CLC   COLPRNT,APYES       PRINT COLUMN?                                
         BE    VALCOL76            YES                                          
         CLC   COLPRNT,APNO        HIDE COLUMN?                                 
         BNE   VALCOL72            YES                                          
         OI    RCLOPT,RCLHIDE      DON'T PRINT COLUMN                           
         B     VALCOL76                                                         
*                                                                               
VALCOL72 DS    0H                                                               
*&&US                                                                           
         CLI   COLPRNT,C'X'        PROCESS COL BUT HIDE AND NO SORT             
         BNE   VALCOL74                                                         
         OI    RCLOPT,RCLHIDE                                                   
         OI    RCLOPT4,RCLXCOL                                                  
         B     VALCOL76                                                         
*&&                                                                             
VALCOL74 CLI   COLPRNT,C'M'        MERGE DATA IF SELECTED $COLS=ZERO            
         BNE   VALCL74D                                                         
         OI    RCLOPT,RCLMERGE                                                  
         TM    RCLOPT,RCLACCM                                                   
         BZ    VALCOL76                                                         
         OI    RCLOPT2,RCLCZERO    TURN ON OPTION                               
         B     VALCOL76                                                         
*                                                                               
VALCL74D TM    RCLOPT,RCLACCM                                                   
         BZ    IVALIPUT                                                         
         CLI   COLPRNT,C'C'        ELIMINATE RECS IF COL=ZERO                   
         BNE   VALCOL75                                                         
         OI    RCLOPT2,RCLCZERO    TURN ON OPTION                               
         B     VALCOL76                                                         
*                                                                               
VALCOL75 CLI   COLPRNT,C'H'        COL=ZERO AND HIDE COL                        
         BNE   IVALIPUT            INVALID PRINT OPTION                         
         OI    RCLOPT,HIDE         DON'T PRINT COLUMN                           
         OI    RCLOPT2,RCLCZERO    TURN ON OPTION                               
*                                                                               
VALCOL76 CLI   NEWELEM,NO          OLD   ELEMENT ?                              
         BE    VALCOL81            YES,  USE  HIDDEN    HEADINGS                
*                                                                               
         TM    RCLOPT,RCLEQU       EQUATION ?                                   
         BZ    VALCOL77            NO,  SKIP                                    
         TM    SVRCLOPT,RCLEQU     WAS  OLD  ALSO EQUATION ?                    
         BO    VALCOL81            YES, USE  HIDDEN    HEADINGS                 
*                                                                               
VALCOL77 CLI   RCLSPCL,RCLSPEXR    SPECIAL ?                                    
         BE    VALCL78A            NO,  SKIP                                    
         CLI   RCLSPCL,RCLSPCME    SPECIAL ?                                    
         BNE   VALCOL78            NO,  SKIP                                    
*                                  WAS  OLD  ALSO SPECIAL ?                     
VALCL78A CLI   SVRCLSPC,RCLSPEXR                                                
         BE    VALCOL81            YES, USE  HIDDEN    HEADINGS                 
         CLI   SVRCLSPC,RCLSPCME                                                
         BE    VALCOL81            YES, USE  HIDDEN    HEADINGS                 
*                                                                               
VALCOL78 CLI   APREPJCL,REPJCLV    PRODUCTION ?                                 
         BE    VALCOL80            YES, USE  DEFAULT   HEADINGS                 
         TM    RCLOPT,RCLACCM      AN   ACCUM     COLUMN ?                      
         BZ    VALCOL80            NO,  USE  DEFAULT   HEADINGS                 
         TM    SVRCLOPT,RCLACCM    WAS  OLD  ALSO ACCUM ?                       
         BZ    VALCOL80            NO,  USE  DEFAULT   HEADINGS                 
         CLI   RCLSPCL,RCLSPEXR    ACCUM     ->   SPECIAL ?                     
         BE    VALCOL80            YES, USE  DEFAULT   HEADINGS                 
         CLI   RCLSPCL,RCLSPCME    ACCUM     ->   SPECIAL ?                     
         BE    VALCOL80            YES, USE  DEFAULT   HEADINGS                 
         CLI   SVRCLSPC,RCLSPEXR   SPECIAL   ->   ACCUM   ?                     
         BE    VALCOL80            YES, USE  DEFAULT   HEADINGS                 
         CLI   SVRCLSPC,RCLSPCME                                                
         BNE   VALCOL81            YES, USE  DEFAULT   HEADINGS                 
*                                                                               
VALCOL80 GOTO1 MKHEAD,APPARM,(R8)  MAKE HEADLINES                               
*                                                                               
VALCOL81 OC    RANKON,RANKON       ANY  RANKING ?                               
         BZ    VALCOL83            NO,  SKIP                                    
         CLI   RANKON,C'C'         RANK ON COLUMNS                              
         BNE   VALCOL82            NO,  SKIP                                    
         CLC   COL#,RANKON+1       IS   THIS THE RANK ON COLUMN ?               
         BNE   VALCOL82            NO,  SKIP                                    
         TM    RCLOPT,RCLACCM      ACCUMULATED COLUMN ?                         
         BO    IVALCNNU            YES, RANK ON COL MUST BE NON-NUMERIC         
*                                                                               
VALCOL82 CLC   COL#,RANKCOL        IS   THIS THE RANK COLUMN ?                  
         BNE   VALCOL83            NO,  SKIP                                    
         TM    RCLOPT,RCLACCM      ACCUMULATED COLUMN ?                         
         BZ    IVALCNU             YES, RANK COL MUST BE NUMERIC                
*                                                                               
VALCOL83 L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     X'C3'                                        
         MVC   ELEMSEQ,COL#        DELETE ELEMENT                               
         GOTO1 DELEL                                                            
         MVC   APELEM,ELEMENT      ADD  THE NEW ELEMENT                         
         GOTO1 ADDEL                                                            
         BNE   VALREC99                                                         
*                                                                               
VALCOL84 SR    RF,RF                                                            
         IC    RF,COL#                                                          
         LA    RF,1(,RF)                                                        
         STC   RF,COL#                                                          
         LA    RF,FRMFUTXH         ->   FIELD AFTER COLUMNS                     
         LA    R3,COLLNQ(,R3)      BUMP TO NEXT FIELD                           
         CR    R3,RF               ARE  WE AFTER COLUMN FIELDS ?                
         BL    VALCOL10            LOOP TO DO ANOTHER                           
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
*  FIND ANY ADDITIONAL ERRORS THAT WOULD CAUSE ERRORS DURING DISPLAY  *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
         TM    KWDINDS,KWDBILLT    KEYWORD "BILLT" ?                            
         BZ    VALCL84A                                                         
         TM    KWDINDS,KWDDRBIL    KEYWORD "DR,BILL" MUST PRESENT               
         BO    VALCL84A                                                         
         MVC   FVMSGNO,=AL2(ACEDRREQ)  DR,BILL REQUIRED                         
         L     R3,SVBILCOL             SET CURSOR TO BILLT FIELD                
         B     IVALCNU1                                                         
*                                                                               
*&&                                                                             
***********************************************************************         
*  ACTUALLY DELETE THE COLUMNS                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCL84A LA    R0,MAXDISP          NUMBER OF  COLUMNS ON SCREEN                 
         LA    R3,(MAXDISP-1)*COLLNQ                                            
         LA    R3,FRMCOL1H(R3)     POINT TO   LAST COLUMN ON SCREEN             
         MVC   SVSVINSQ,SVINSSEQ   SAVE  INSERT    SEQUENCE   NUMBER            
         MVC   SVACURSR,ACURSOR    SAVE  CURRENT   CURSOR     ADDRESS           
*                                                                               
VALCOL85 GOTO1 AFVAL,COLDATAH      ANY   DATA FOUND ?                           
         BE    VALCOL90            YES,  SO   CHECK NEXT COLUMN                 
         SR    R1,R1                                                            
         IC    R1,STSEQ                                                         
         AR    R1,R0               R0 =  STARTING  COLUMN #                     
         BCTR  R1,0                                                             
         GOTO1 =A(DELCOLMN),RR=APRELO                                           
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
         CLI   APPFKEY,PFKINS      INSERT    PF   KEY ?                         
         BNE   VALCOL87            NO,  SKIP                                    
         LA    RE,COLDATAH         ->   DATA AREA                               
         C     RE,SVACURSR         AFTER     CURRENT   CURSOR ?                 
         BNL   VALCOL87            YES, SKIP                                    
         L     RE,ACURSOR          SUBTRACT  ONE  LINE                          
         AHI   RE,-COLLNQ          FROM                                         
         ST    RE,ACURSOR          CURRENT   CURSOR                             
*                                                                               
VALCOL87 CLI   APPFKEY,PFKHLP      HELP  PF   KEY ?                             
         BNE   VALCOL90            NO,   SKIP                                   
         CLI   SVINSSEQ,0          DURING     AN   INSERT ?                     
         BE    VALCOL90            NO,   SKIP                                   
         CLM   R1,1,SVSVINSQ       AFTER SAVE INSSEQ ?                          
         BNL   VALCOL90            YES,  SKIP                                   
         IC    R1,SVINSSEQ         SUBTRACT ONE FROM SVINSSEQ                   
         BCTR  R1,0                                                             
         STC   R1,SVINSSEQ                                                      
         L     RE,CUR@RHLP         SUBTRACT ONE LINE FROM CUR@RHLP              
         AHI   RE,-COLLNQ                                                       
         ST    RE,CUR@RHLP                                                      
*                                                                               
VALCOL90 AHI   R3,-COLLNQ                                                       
         BCT   R0,VALCOL85                                                      
         TM    MIXESTK,DEFOEST+DEFNEST  TEST BOTH NEW+OLD EST KEYWORDS          
         BO    IVALMIX                  YES - INVALID                           
         TM    MIXESTK,DEFCACR     TEST KEYWORD REQUIRES CONTRA A/C             
         BZ    VALCOL91                                                         
         TM    MIXESTK,DEFCACI     TEST CONTRA A/C KEYWORD PRESENT              
         BZ    IVALNOCA            NO - INVALID                                 
VALCOL91 DS    0H                                                               
         EJECT ,                                                                
***********************************************************************         
*  FINISHED BUILDING COLUMNS NOW RE-SEQUENCE AND EXTRA VALIDATION     *         
***********************************************************************         
                                                                                
         GOTO1 =A(CCOLSEQ),RR=APRELO                                            
         GOTO1 =A(CHKCOLS),RR=APRELO                                            
         BNE   IVALCOLC                                                         
         GOTO1 =A(DELXTRM),RR=APRELO                                            
***********************************************************************         
*  OUTPUT THE RECORD TO THE FILE                                      *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON   ERROR, EXIT                             
*&&UK                                                                           
         TM    KWDINDS,KWDTSDAY    KEYWORD "TSDAY" ?                            
         BZ    VALREC92                                                         
         TM    KWDINDS,KWDTSDTE    TSDTE KEYWORD REQUIRED                       
         BO    VALREC92                                                         
         MVC   FVMSGNO,=AL2(ACEDTREQ)                                           
         L     RE,SVDAYCOL             SET CURSOR TO TSDAY FIELD                
         ST    RE,FVADDR                                                        
         B     VALREC99                                                         
*&&                                                                             
VALREC92 TM    DWNTYPE,DWNTACNT+DWNTQREP   ACCENT OR QREPORT ENABLED?           
         BZ    VALREC95                    NO - OK                              
*&&UK                                                                           
         USING RRWELD,R1                                                        
         L     R1,AIOAREA1                 A(FORMAT RECORD)                     
         MVI   APELCODE,RRWELQ             X'C2', row element                   
         GOTO1 GETEL                                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(ACEM1R1C)                                           
         B     VALREC99            MUST DEFINE AT LEASE 1 ROW                   
*                                                                               
         GOTO1 NEXTEL                                                           
         BE    *-2                                                              
         MVC   TOT#ROWS,RRWSEQ                                                  
         DROP  R1                                                               
*&&                                                                             
         USING RCLELD,R1                                                        
         L     R1,AIOAREA1         A(FORMAT RECORD)                             
         MVI   APELCODE,RCLELQ     X'C3' COLUMN ELEMENT                         
         GOTO1 GETEL                                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(ACEM1R1C)                                           
         B     VALREC99            MUST DEFINE AT LEASE 1 COLUMN                
                                                                                
         TM    DWNTYPE,DWNTQREP       QREPORT ENABLED?                          
         BZ    VALREC93                                                         
         XR    RE,RE                                                            
         IC    RE,TOT#ROWS                                                      
         XR    RF,RF                                                            
         IC    RF,TOT#COLS                                                      
         AR    RE,RF                                                            
         CHI   RE,20                                                            
         BNH   VALREC93                                                         
         MVC   FVMSGNO,=AL2(ACEQR20)  MORE THAN 20 ROWS AND COLUMNS             
         B     VALREC99                                                         
                                                                                
VALREC93 TM    RCLOPT,RCLACCM                                                   
         BO    VALREC95                                                         
*&&UK                                                                           
         MVC   FVMSGNO,=AL2(ACENAMKW) MUST BE AMOUNT KEYWORD FOR ACCENT         
         B     VALREC99                                                         
*&&                                                                             
         TM    DWNTYPE,DWNTQREP       QREPORT ?                                 
         BZ    VALREC95               NO - OK                                   
         MVC   FVMSGNO,=AL2(ACENMKWQ) MUST BE AMOUNT KEYWORD FOR QREPS          
         B     VALREC99                                                         
         DROP  R1                                                               
*                                                                               
VALREC95 MVI   APELCODE,STYELQ     X'25'                                        
         L     R1,AIOAREA1                                                      
         GOTO1 GETEL,(R1)                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING STYELD,R1                                                        
         MVC   STYSEC#1,SECLVL                                                  
         MVC   APSECKYW,SECLVL                                                  
         LR    RE,R1                                                            
         SR    R2,R2                                                            
         LA    RF,1                                                             
*                                                                               
         USING RCLELD,RE                                                        
VALRC95A CLI   0(RE),0             EOR                                          
         BE    VALRC95C                                                         
         CLI   0(RE),RCLELQ        X'C3' COLUMN ELEMENT                         
         BNE   VALRC95B                                                         
         TM    RCLOPT,RCLHIDE                                                   
         BO    VALRC95B            HIDDEN COLUMN                                
         CLI   RCLSTACK,0                                                       
         BNE   VALRC95B            STACKED UNDER ANOTHER COLUMN                 
         CLI   RCLWDTH,0           COLUMN  WIDTH ZERO                           
         BE    VALRC95B                                                         
         IC    R2,RCLWDTH          WIDTH OF COLUMN                              
         LA    RF,1(R2,RF)                                                      
*                                                                               
VALRC95B IC    R2,RCLLN            LENGTH OF ELEMENT                            
         AR    RE,R2                                                            
         B     VALRC95A                                                         
         DROP  RE                                                               
*                                                                               
VALRC95C STH   RF,STYWIDTH         SAVE REPORT WIDTH                            
         DROP  R1                                                               
*                                                                               
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADDING   A RECORD ?                          
         BO    VALREC96                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGING A RECORD ?                          
         BO    VALREC96                                                         
         DC    H'00'                                                            
*                                                                               
VALREC96 GOTO1 AIO                                                              
         BE    VALREC97                                                         
         TM    IOERR,IOEDUP        DELETED RECORD (DUPLICATE) ON FILE           
         BO    *+6                                                              
         DC    H'00'                                                            
         MVC   FVMSGNO,=AL2(FVFCCDR)       CANNOT MODIFY A DELETED RCD          
*                                                                               
VALREC97 CLI   HAVE5TH,YES                                                      
         BNE   VALREC98            NO                                           
         CLI   ISLOCKED,YES                                                     
         BE    VALREC98                                                         
*                                                                               
         USING RESRECD,R2                                                       
         L     R2,AIOAREA0                                                      
         LR    RE,R2                                                            
         AH    RE,DATADISP                                                      
         CLI   0(RE),EOR           ANY ELEMENTS ON RECORD ?                     
         BNE   *+8                                                              
         OI    RESRSTA,X'80'       MARK DELETED, NO ELEMENT ON RECORD           
         MVC   IOADDR,AIOAREA0                                                  
         GOTO1 AIO,IOWRITE+IOACCFIL                                             
         DROP  R2                                                               
*                                                                               
VALREC98 CLI   APACTN,ACTDEL       DELETE  ONLY                                 
         BE    DISREC                                                           
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   IVALEX                                                           
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    DISREC                                                           
         CLI   APMODE,APMNEWK      ACTION COPY?                                 
         BNE   DISREC              NO                                           
         B     EXIT                ADD NEW RECORD WITH HEADING ELEMENTS         
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISREC   BAS   RE,DISRSUB          CALL  DISPLAY RECORD SUBROUTINE              
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECORD SUBROUTINE                                          *         
***********************************************************************         
DISRSUB  NTR1                                                                   
*                                  TURN  OFF  DELETE ELEMENT SWITCH             
         NI    MSGELSW,TURNOFF-MSGELDEL                                         
         OI    MSGELSW,MSGELIDR    TURN  ON  'IN     DISPLAY ROUTINE'           
         L     R2,AIOAREA1                                                      
         TWAXC FRMNMEH,FRMTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),FRMNMEH                                      
         GOTO1 GETPER,APPARM,(R2),FRMOWNH                                       
         SPACE 1                                                                
***********************************************************************         
*  DISPLAY U/L OR REPORT TYPE                                         *         
***********************************************************************         
         SPACE 1                                                                
         CLI   APMLDGR,YES                                                      
         BE    DR050                                                            
         MVC   FRMTYPE(L'APREPTYP),APREPTYP                                     
         B     DR100                                                            
*                                                                               
         USING RFLELD,R1                                                        
DR050    MVI   APELCODE,RFLELQ     X'C5'                                        
         GOTO1 GETEL,(R2)                                                       
*                                                                               
DR060    BNE   DR100                                                            
         CLI   RFLTYPE,RFLLDG      GET LEDGER LIST                              
         BNE   *+8                                                              
         CLI   RFLSEQ,0                                                         
         BE    DR080                                                            
         GOTO1 NEXTEL                                                           
         B     DR060                                                            
*                                                                               
DR080    SR    RF,RF                                                            
         IC    RF,RFLLN            GET ELEMENT LENGTH                           
         SH    RF,=Y(RFLLNQ+1)                                                  
         BM    DR100               NO LIST, NOT GOOD                            
         CLM   RF,1,=AL1(L'FRMTYPE-1)                                           
         BNH   *+6                                                              
         DC    H'00'                                                            
         EXMVC RF,FRMTYPE,RFLDATA                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY HEADINGS                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RHDELD,R1                                                        
         USING HEADD,R4                                                         
DR100    OC    KWDPASTE,KWDPASTE   ANY  KEYWORD   FROM HELP FACILITY ?          
         BZ    DR105               NO,  SKIP                                    
         CLI   REPMODE,REPHEAD     KEYWORD   FOR  HEADER    AREA ?              
         BNE   DR105               NO,  SKIP                                    
         L     R3,CUR@RHLP         ->   ADDR RETURNED  FROM HELP                
         A     R3,ATWA                  PLUS ADDR OF   TWA                      
         ST    R3,ACURSOR          SET  CURSOR    LOCATION                      
         MVI   8(R3),C'&&'         INSERT    "&"                                
*                                  INSERT    KEYWORD                            
         MVC   9(L'KWDPASTE,R3),KWDPASTE                                        
         OI    4(R3),FVITHIS       SAY  DATA INPUTED                            
         MVI   5(R3),L'KWDPASTE+1  LENGTH    OF   KEYWORD                       
         OI    6(R3),FVOXMT        TRANSMIT                                     
*                                                                               
         XC    KWDPASTE,KWDPASTE   CLEAR     KEYWORD                            
*                                                                               
DR105    MVI   APELCODE,RHDELQ     HEADING ELEMENT                              
         L     R2,AIOAREA1                                                      
         GOTO1 GETEL,(R2)                                                       
*                                                                               
DR110    BNE   DR200                                                            
*                                                                               
         L     R4,=A(HEADTAB)      TABLE OF HEADING FIELDS                      
         A     R4,APRELO           RELOCATE IT                                  
*                                                                               
DR120    ICM   R3,15,HEADFLD                                                    
         BZ    DR180                                                            
         AR    R3,R5                                                            
         CLC   HEADSEQ,RHDSEQ                                                   
         BNE   DR140                                                            
         CLC   HEADTYPE,RHDTYPE                                                 
         BNE   DR140                                                            
         NI    6(R3),TURNOFF-X'20' UNPROTECT FIELD                              
         OI    6(R3),FVOXMT        TRANSMIT IT                                  
         SR    RF,RF                                                            
         IC    RF,RHDLN            ELEMENT LENGTH                               
         SH    RF,=Y(RHDLNQ+1)                                                  
         BM    DR180                                                            
         CLM   RF,1,HEADLN                                                      
         BNH   DR130                                                            
         IC    RF,HEADLN                                                        
         LA    RE,8(RF,R3)         POINT TO END ON FIELD                        
         MVI   0(RE),C'>'                                                       
         OI    6(R3),X'20'         PROTECT FIELD                                
         BCTR  RF,0                MINUS ONE EXTRA                              
*                                                                               
DR130    EXMVC RF,8(R3),RHDDATA    MOVE IN DATA                                 
         B     DR180                                                            
*                                                                               
DR140    LA    R4,HEADLNQ(,R4)     BUMP TO NEXT FIELD                           
         B     DR120                                                            
*                                                                               
DR180    GOTO1 NEXTEL                                                           
         B     DR110               LOOP TO CONDITION                            
         DROP  R1,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY ROWS                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RRWELD,R1                                                        
         USING ROWD,R3                                                          
DR200    OC    KWDPASTE,KWDPASTE   ANY  KEYWORD   FROM HELP FACILITY ?          
         BZ    DR210               NO,  SKIP                                    
         CLI   REPMODE,REPROW      KEYWORD   FOR  ROWS AREA ?                   
         BNE   DR210               NO,  SKIP                                    
         L     R3,CUR@RHLP         ->   ADDR RETURNED  FROM HELP                
         A     R3,ATWA                  PLUS ADDR OF   TWA                      
         ST    R3,ACURSOR          SET  CURSOR    LOCATION                      
*                                  INSERT    KEYWORD                            
         MVC   8(L'KWDPASTE,R3),KWDPASTE                                        
         OI    4(R3),FVITHIS       SAY  DATA INPUTED                            
         MVI   5(R3),L'KWDPASTE    LENGTH    OF   KEYWORD                       
         OI    6(R3),FVOXMT        TRANSMIT                                     
*                                                                               
         XC    KWDPASTE,KWDPASTE   CLEAR     KEYWORD                            
*                                                                               
DR210    MVI   APELCODE,RRWELQ     ROW ELEMENT                                  
         L     R2,AIOAREA1                                                      
         MVI   STRROW#,1           START ROW NUMBER                             
         SR    R1,R1                                                            
         IC    R1,STRROW#                                                       
         LA    R3,FRMROW1H         START ROW 1                                  
         LA    R6,3                                                             
*                                                                               
DR220    MVC   ROWNUM,SPACES       CLEAR                                        
         CVD   R1,APDUB            CONVERT TO PACKED                            
         OI    APDUB+7,X'0F'       IGNORE SIGN                                  
         UNPK  ROWNUM+1(2),APDUB                                                
         CLI   ROWNUM+1,C'0'       BLANK OUT FOR SINGLE DIGITS                  
         BNE   *+8                                                              
         MVI   ROWNUM+1,C' '                                                    
         OI    ROWNUMH+6,FVOXMT                                                 
         LA    R3,ROWLNQ(,R3)                                                   
         LA    R1,1(,R1)                                                        
         BCT   R6,DR220                                                         
*                                                                               
         LA    R3,FRMROW1H         START ROW 1                                  
         LA    R6,3                                                             
         GOTO1 GETEL,(R2)                                                       
*                                                                               
DR250    BNE   DR400                                                            
         CLC   STRROW#,RRWSEQ      IS IT ROW ONE?                               
         BE    DR300               NO MATCH GET NEXT                            
         GOTO1 NEXTEL                                                           
         B     DR250                                                            
*                                                                               
DR300    CLI   PASTESEQ,0          JUST PASTED    A    KEYWORD ?                
         BE    DR320               NO,  SKIP                                    
         CLM   R3,1,PASTESEQ       WAS  THIS LINE PASTED ?                      
         BNE   DR320               NO,  SKIP                                    
         MVI   PASTESEQ,0          CLEAR     SWITCH                             
         LA    R3,ROWLNQ(,R3)      NEXT ROW                                     
         BCT   R6,DR320            DECREMENT ROW  COUNTER                       
*                                  IF   NO   MORE ROWS SPACE     LEFT,          
         B     DR400                    THEN DISPLAY   COLUMNS                  
*                                                                               
DR320    SR    RF,RF                                                            
         IC    RF,RRWDATLN         LENGTH OF DATA                               
         CLI   RRWDATLN,L'ROWDATA                                               
         BNH   *+8                                                              
         MVI   RRWDATLN,L'ROWDATA                                               
*        BNH   *+6                                                              
*        DC    H'00'               DATA TOO BIG FOR FIELD, UT OH                
*                                                                               
         BCTR  RF,0                                                             
         EXMVC RF,ROWDATA,RRWNDATA                                              
         MVC   ROWTYPE,RRWTYPE     MOVE IN TYPE                                 
         TM    RRWOPT2,RRWHIDE     ONLY SET BY STEREO                           
         BZ    *+8                                                              
         MVI   ROWTYPE,C'N'                                                     
*                                                                               
         TM    RRWOPT2,RRWPGNO     REPAGE ON ROW?                               
         BZ    *+8                                                              
         MVI   ROWNUM+1,C'*'       MARK AS ROW PAGEING                          
*&&US                                                                           
         MVC   ROWTOTL,APNO        DO NOT TOTAL ON (DEFAULT)                    
         TM    RRWOPT2,RRWAIDX     Row indexing on for Accent                   
         BZ    *+8                                                              
         MVI   ROWTOTL,C'I'        Accent indexing                              
*&&                                                                             
*        MVC   ROWTOTL,APNO        DO NOT TOTAL ON (DEFAULT)                    
*        TM    RRWOPT2,RRWAIDX     Row indexing on for Accent                   
*        BZ    *+8                                                              
*        MVI   ROWTOTL,C'I'        Accent indexing                              
                                                                                
         TM    RRWOPT,RRWTOT                                                    
         BZ    DR345                                                            
         MVC   ROWTOTL,APYES       TOTAL ON                                     
         TM    RRWOPT,RRWTOTSP                                                  
         BZ    *+8                                                              
         MVI   ROWTOTL,C'S'        TOTAL ON BUT ON SEPERATE PAGE                
         TM    RRWOPT2,RRWBTM                                                   
         BZ    *+8                                                              
         MVI   ROWTOTL,C'B'        TOTAL ON BUT ON SEPERATE PAGE                
*                                                                               
DR345    SR    RF,RF                                                            
         ICM   RF,1,RRWPFXLN                                                    
         BZ    DR350                                                            
         SR    RE,RE                                                            
         IC    RE,RRWDATLN         LENGTH OF   RRWNDATA                         
         LA    RE,RRWNDATA(RE)     POINT TO PAST RRNDATA                        
         CHI   RF,L'ROWPRFX                                                     
         BNH   *+6                                                              
         DC    H'00'               TOO BIG FOR SCREEN FIELD                     
*                                                                               
         SHI   RF,1                                                             
         BM    DR350               NO PREFIX                                    
         EXMVC RF,ROWPRFX,0(RE)                                                 
*                                                                               
DR350    LA    R3,ROWLNQ(,R3)      BUMP TO ROW                                  
         BCT   R6,DR380                                                         
         B     DR400               NO MORE ROWS                                 
*                                                                               
DR380    GOTO1 NEXTEL                                                           
         BE    DR300                                                            
         DROP  R1,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY COLUMNS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R4                                                        
         USING COLD,R3                                                          
DR400    LA    R3,FRMCOL1H         START COLUMN 1                               
         L     R4,AIOAREA1                                                      
         MVI   STSEQ,1                                                          
         MVI   INSSEQ,0            INIT TO ZERO                                 
*                                                                               
         SR    R1,R1                                                            
         IC    R1,STSEQ                                                         
         STC   R1,COL#             SAVE AS CURRENT COLUMN NUMBER                
         LA    R6,MAXDISP          MAX  COLUMNS DISPLAYED                       
*                                                                               
DR420    MVC   COLNUM,SPACES       CLEAR                                        
         CVD   R1,APDUB            CONVERT TO PACKED                            
         OI    APDUB+7,X'0F'       IGNORE SIGN                                  
         UNPK  COLNUM+1(2),APDUB                                                
         CLI   COLNUM+1,C'0'       BLANK OUT FOR SINGLE DIGITS                  
         BNE   *+8                                                              
         MVI   COLNUM+1,C' '                                                    
         OI    COLNUMH+6,FVOXMT                                                 
         LA    R3,COLLNQ(,R3)                                                   
         LA    R1,1(,R1)                                                        
         BCT   R6,DR420                                                         
*                                                                               
         LA    R3,FRMCOL1H         START COLUMN 1                               
*                                                                               
         OC    KWDPASTE,KWDPASTE   ANY  KEYWORD   FROM HELP FACILITY ?          
         BZ    DR430               NO,  SKIP                                    
*                                                                               
         CLI   SVINSSEQ,0          HELP REQUESTED DURING    INSERT ?            
         BE    DR425               NO,  SKIP                                    
         ZIC   R3,SVINSSEQ         LOAD THE  INSERT    SEQUENCE  NUMBER         
         STC   R3,COLINSRT         SAVE THE  INSERT    SEQUENCE  NUMBER         
         STC   R3,PASTESEQ         SAVE THE  INSERT    SEQUENCE  NUMBER         
*                                  FIX  UP   COLUMN    CALCULATIONS             
         GOTO1 =A(FIXCALC),APPARM,(R3),1,RR=APRELO                              
*                                  FIX  UP   PROFILE   ELEMENTS                 
         GOTO1 =A(FIXPROF),APPARM,(R3),1,RR=APRELO                              
*                                  FIX  UP   MSGS      DISPLAY                  
         GOTO1 =A(FIXMSGS),APPARM,(R3),1,NO,RR=APRELO                           
         OI    MSGELSW,MSGELINS    TURN ON   INSERT    SWITCH                   
         LA    R3,FRMCOL1H         ->   1ST  COLUMN                             
*                                                                               
DR425    L     RE,CUR@RHLP         ->   ADDR RETURNED  FROM HELP                
         A     RE,ATWA                  PLUS ADDR OF   TWA                      
         ST    RE,ACURSOR          SET  CURSOR    LOCATION                      
*                                  INSERT    KEYWORD                            
         MVC   8(L'KWDPASTE,RE),KWDPASTE                                        
         OI    4(RE),FVITHIS       SAY  DATA INPUTED                            
         MVI   5(RE),L'KWDPASTE    LENGTH    OF   KEYWORD                       
         OI    6(RE),FVOXMT        TRANSMIT                                     
*                                                                               
DR430    XC    KWDPASTE,KWDPASTE   CLEAR     KEYWORD                            
         CLI   APPFKEY,PFKHLP      HELP PF   KEY  ?                             
         BE    DR435               YES, SKIP                                    
*                                  CLEAR     CURSOR    ADDR FOR  RETURN         
         XC    CUR@RHLP,CUR@RHLP             FROM HELP                          
         MVI   SVINSSEQ,0          CLEAR     SAVE INSSEQ                        
*                                                                               
DR435    MVI   APELCODE,RCLELQ     X'C3'     COLUMN    ELEMENT                  
         GOTO1 GETEL,(R4)                                                       
*                                                                               
DR450    BNE   DR950                                                            
         LR    R4,R1                                                            
         CLC   STSEQ,RCLSEQ        IS IT COLUMN ONE?                            
         BE    DR455               NO MATCH GET NEXT                            
         GOTO1 NEXTEL                                                           
         B     DR450                                                            
*                                                                               
         USING RCLELD,R4                                                        
DR455    CLI   APPFKEY,PFKINS      INSERT  COLUMN ?                             
         BNE   DR460               NO,     SKIP                                 
         SR    R0,R0                                                            
         L     R1,ACURSOR          CURRENT CURSOR  ADDRESS                      
         LA    R2,FRMCOL1H         FIRST   COLUMN  ADDRESS                      
         SR    R1,R2               FIGURE  OUT THE COLUMN NUMBER                
         BM    DR460               NOT     COLUMN  INSERT, SKIP                 
         D     R0,=A(COLLNQ)               WHERE   INSERT WAS REQUESTED         
         CHI   R0,(MAXDISP-1)      PAST    LAST    COLUMN ?                     
         BH    DR460               YES,    SKIP                                 
         ZIC   R2,STSEQ            FIRST   COLUMN  ON     SCREEN                
         AR    R2,R1               INSERT  BEFORE  COLUMN NUMBER                
*                                                                               
*                                  FIX     UP      COLUMN  CALCULATIONS         
         GOTO1 =A(FIXCALC),APPARM,(R2),1,RR=APRELO                              
*                                  FIX     UP      PROFILE ELEMENTS             
         GOTO1 =A(FIXPROF),APPARM,(R2),1,RR=APRELO                              
*                                  FIX     UP      MSGS    DISPLAY              
         GOTO1 =A(FIXMSGS),APPARM,(R2),1,NO,RR=APRELO                           
         OI    MSGELSW,MSGELINS    TURN    ON      INSERT  SWITCH               
         B     DR460                                                            
         SPACE 1                                                                
***********************************************************************         
*  INSERT A COLUMN LINE                                               *         
***********************************************************************         
         SPACE 1                                                                
DR460    CLI   APACTN,ACTCHA       ACTION CHANGE?                               
         BNE   DR465                                                            
         CLI   APPFKEY,PFKINS      INSERT COLUMN?                               
         BNE   DR465                                                            
         L     R2,ACURSOR          CURRENT CURSOR LOCATION                      
         CR    R2,R3               INSERT ON THIS COLUMN LINE                   
         BL    DR465                                                            
         LA    RF,COLLNQ(,R3)      BUMP TO END OF COLUMN LINE                   
         CR    R2,RF                                                            
         BH    DR465                                                            
         MVC   INSSEQ,COL#         SAVE CURRENT SEQUENCE NUMBER                 
         MVC   COLINSRT,COL#       SAVE CURRENT SEQUENCE NUMBER                 
         LA    R0,FRMFUTXH         DID WE PASS LAST COLUMN ?                    
         CR    RF,R0                                                            
         BNL   DR900                                                            
         LA    RF,COLDATAH         SET TO FIRST FIELD                           
         ST    RF,ACURSOR                                                       
         LA    R3,COLLNQ(,R3)      BUMP TO NEXT LINE ON SCREEN                  
*                                                                               
DR465    CLI   PASTESEQ,0          JUST PASTED    A    KEYWORD ?                
         BE    DR470               NO,  SKIP                                    
         CLC   PASTESEQ,COL#       WAS  THIS LINE PASTED ?                      
         BNE   DR470               NO,  SKIP                                    
         MVC   INSSEQ,PASTESEQ     SAVE THE  PASTE     SEQUENCE  NUMBER         
         MVI   PASTESEQ,0          CLEAR     SWITCH                             
         LA    R3,COLLNQ(,R3)      BUMP TO   NEXT LINE ON   THE  SCREEN         
         LA    RF,FRMFUTXH         ->   END  OF   SCREEN                        
         CR    R3,RF               ON   LAST LINE OF   SCREEN ?                 
         BNL   DR900               YES, NO   MORE LINES                         
*                                                                               
DR470    CLC   RCLSEQ,COL#                                                      
         BNE   DR890                                                            
         SR    R2,R2                                                            
         IC    R2,COL#                                                          
         CLI   APPFKEY,PFKINS                                                   
         BNE   DR475                                                            
         CLI   INSSEQ,0                                                         
         BE    DR475                                                            
         MVI   APPFKEY,0           CLEAR OUT PFKEY                              
*                                                                               
DR475    LA    R2,1(,R2)           BUMP UP CURRENT SEQUENCE NUMBER              
         STC   R2,COL#                                                          
         CLI   RCLDATLN,L'COLDATA                                               
         BNH   *+6                                                              
         DC    H'00'               TOO BIG TO FIT IN FIELD                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RCLDATLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   COLDATA(0),RCLNDATA                                              
         XC    APWORK,APWORK                                                    
*                                                                               
         USING RFLELD,R1                                                        
         SR    RF,RF                                                            
         L     R1,AIOAREA1                                                      
         AH    R1,DATADISP                                                      
*                                                                               
DR480    CLI   RFLEL,0             END OF RECORD                                
         BE    DR650                                                            
         CLI   RFLEL,RFLELQ        X'C5' FILTER ELEMENT                         
         BNE   DR482                                                            
         CLC   RFLSEQ,RCLSEQ       MATCH ON COLUMN SEQUENCE                     
         BNE   DR482                                                            
         MVI   COLNUM,C'*'         DEFAULT IF A FILTER                          
         CLI   RFLTYPE,RFLTTYPE    MATCH ON TRANSACTION TYPE                    
         BE    DR486                                                            
         CLI   RFLTYPE,RFLBUDGT    MATCH ON BUDGET                              
         BE    DR488                                                            
         CLI   RFLTYPE,RFLFCUR     MATCH ON FOREIGN CURRENCY TYPE               
         BE    DR484                                                            
*                                                                               
DR482    IC    RF,RFLLN            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         B     DR480                                                            
*                                                                               
DR484    MVI   COLNUM,C'C'         MARK AS FOREIGN CURRENCY                     
         TM    INOPT1,INOFCUR      DISPLAY CURRENCY ?                           
         BZ    DR650               NO,     SKIP                                 
         MVC   COLDATE(3),RFLDATA  DISPLAY CURRENCY                             
         B     DR800                                                            
*                                                                               
DR486    MVI   COLNUM,C'T'         MARK AS TRANSACTION TYPE                     
         TM    INOPT1,INOTYPE                                                   
         BO    DR550                                                            
         B     DR650                                                            
*                                                                               
DR488    MVI   COLNUM,C'B'         MARK AS BUDGET                               
         TM    INOPT1,INOBUD                                                    
         BO    DR530                                                            
         B     DR650                                                            
         DROP  R1                                                               
*                                                                               
         USING RFLELD,R2                                                        
         USING BUDRECD,R1                                                       
DR530    MVC   SVKEY,IOKEY                                                      
         MVC   APWORK,SPACES                                                    
         LR    R2,R1                                                            
         LA    R1,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY                                      
         MVC   BUDKNO1,RFLDATA                                                  
         MVC   APHALF,RFLDATA      SAVE    DESIRED   BUDGET   NUMBER            
         GOTO1 AIO,IO2+IOACCFIL+IOHID                                           
         BL    DR535                                                            
         L     R1,AIOAREA2                                                      
         BE    DR540                                                            
*                                                                               
DR535    MVC   FVMSGNO,=AL2(ACEBUD)        INVALID BUDGET TYPE                  
         TM    IOERR,IOEDEL                IS IT DELETED                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFRDEL)       RECORD DELETED                       
         ST    R3,DRCURSOR                 SET  DISPLAY CURSOR                  
*                                                                               
*                                  THIS    BUDGET    NUMBER   MIGHT             
*                                          NOT  EXIST    BECAUSE IT             
*                                          WAS  OBSOLETED     -                 
*                                  DISPLAY THE  BUDGET   AS   A  NUMBER         
*                                          INSTEAD   OF  AS   A                 
*                                          BUDGET    ID  NAME                   
         LA    RF,APWORK           ->      WORK AREA                            
         SR    R0,R0               CLEAR   REGISTER                             
         ICM   R0,3,APHALF         GET     THE  BUDGET   NUMBER                 
         CVD   R0,APDUB            CONVERT IT   TO       PACKED DECIMAL         
         OI    APDUB+7,X'0F'       SETUP   FOR  DISPLAY                         
         UNPK  0(5,RF),APDUB       CONVERT TO   DISPLAY                         
         MVC   5(2,RF),=C' ?'      SAY     SOMETHING IS  WRONG                  
         B     DR545               DISPLAY IT                                   
*                                                                               
DR540    LA    RF,APWORK                                                        
         MVC   0(L'BUDKCOD,RF),BUDKCOD                                          
*                                                                               
DR545    MVC   COLDATE,APWORK                                                   
         MVC   IOKEY,SVKEY                                                      
         OI    COLDATEH+6,FVOXMT   TRANSMIT                                     
         B     DR800                                                            
         DROP  R1,R2                                                            
*                                                                               
DR550    MVC   APWORK,SPACES                                                    
         LA    R8,APWORK                                                        
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',(R8))                         
         LR    R1,R0                                                            
         MVC   COLDATE,APWORK                                                   
         OI    COLDATEH+6,FVOXMT                                                
         B     DR800                                                            
*                                                                               
DR650    MVC   APWORK,SPACES                                                    
         TM    INOPT1,INOFCUR+INOBUD+INOTYPE                                    
         BNZ   DR800                                                            
*                                                                               
         CLI   RCLSPCL,RCLSPKY1    PDAY KEYWORD ?                               
         BNE   DR655                                                            
         SR    RF,RF                                                            
         ICM   RF,1,RCLPDN1        GET PERIOD NUMBER                            
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         MVC   COLDATE,SPACES                                                   
         UNPK  COLDATE(2),APDUB                                                 
         CLI   COLDATE,C'0'                                                     
         BNE   DR655                                                            
         MVC   COLDATE,SPACES                                                   
         UNPK  COLDATE(1),APDUB                                                 
*                                                                               
DR655    CLI   RCLDATES,0          IS   DATE RANGE     USED ?                   
         BE    DR800               NO,  SKIP                                    
         TM    RCLOPT,RCLACCM      IS   THIS AN   ACCUMULATED COLUMN ?          
         BZ    DR800               NO,  SKIP                                    
*                                                                               
         TM    RCLDTEFG,X'FF'-RCLTODTE                                          
         BZ    DR750               MUST BE   PERIOD   RANGE                     
         CLC   RCLSTDT,=XL2'8000'  DELIMETER FOR  "PRIOR"                       
         BNE   DR660                                                            
         MVC   APWORK(5),AC@PRIOR                                               
         B     DR700                                                            
*                                                                               
DR660    MVC   APHALF,RCLSTDT      MOVE INTO APHALF WORD                        
         LH    R2,APHALF                                                        
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BNO   DR664                                                            
         SR    R2,R2                                                            
         ICM   R2,1,RCLPDN1        GET PERIOD NUMBER                            
         TM    RCLPDYR,X'80'                                                    
         BZ    *+6                                                              
         LNR   R2,R2                                                            
*                                                                               
DR664    CVD   R2,APDUB                                                         
         MVC   APWORK(7),=XL7'40202020202060'                                   
         LA    R1,APWORK+5                                                      
         EDMK  APWORK(7),APDUB+5                                                
         CLI   APWORK+5,C' '                                                    
         BNE   *+8                                                              
         MVI   APWORK+5,C'0'       FORCE A ZERO TO PRINT                        
         BCTR  R1,0                                                             
         MVC   0(1,R1),APWORK+6                                                 
         MVI   APWORK+6,C' '                                                    
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BNO   DR668                                                            
         CLI   RCLSTDT,X'01'       FPRWARD ONE YEAR?                            
         BNE   *+8                                                              
         MVI   0(R1),C'+'          YES SO MOVE IN A PLUS SIGN                   
         TM    RCLDTEFG,RCLNROLL   PM1-PM12                                     
         BO    DR750                                                            
         SR    R2,R2                                                            
         OC    RCLENDT,RCLENDT                                                  
         BNZ   DR700                                                            
         B     DR750                                                            
*                                                                               
DR668    TM    RCLSTDT,X'80'       NEGATIVE HALF WORD?                          
         BO    DR670               YES, SO DISPLAY END DATA                     
         TM    RCLDTEFG,RCLDAY     IS IT DAY?                                   
         BO    DR680               YES, SO DISPLAY BOTH PARAMETERS              
         OC    RCLENDT,RCLENDT     ANY END DATE?                                
         BZ    DR750               NO SO DON'T WORRY                            
*                                                                               
DR670    TM    RCLDTEFG,RCLPERD                                                 
         BNO   DR680                                                            
         TM    RCLDTEFG,RCLMON+RCLNROLL                                         
         BNZ   DR680                                                            
         CLC   RCLENDT,RCLSTDT                                                  
         BE    DR750               IF EQUAL DON'T DISPLAY AGAIN                 
*                                                                               
DR680    SR    R2,R2                                                            
         CLC   RCLENDT,=XL2'8000'  DELIMETER FOR "AFTER"                        
         BNE   DR690                                                            
         MVC   APWORK+7(1),SCCOMMA                                              
         MVC   APWORK+8(5),AC@AFTER                                             
         B     DR750                                                            
*                                                                               
DR690    TM    RCLDTEFG,RCLMON+RCLNROLL                                         
         BNZ   DR750                                                            
         TM    RCLDTEFG,RCLPERD                                                 
         BO    DR700                                                            
         TM    RCLDTEFG,RCLDAY                                                  
         BZ    DR750                                                            
*                                                                               
DR700    MVC   APWORK+7(1),SCCOMMA                                              
         MVC   APHALF,RCLENDT                                                   
         LH    R2,APHALF                                                        
         CVD   R2,APDUB                                                         
         MVC   APWORK+8(7),=XL7'40202020202060'                                 
         LA    R1,APWORK+13                                                     
         EDMK  APWORK+8(7),APDUB+5                                              
         CLI   APWORK+13,C' '                                                   
         BNE   *+8                                                              
         MVI   APWORK+13,C'0'      FORCE A ZERO TO PRINT                        
         BCTR  R1,0                                                             
         MVC   0(1,R1),APWORK+14                                                
         MVI   APWORK+14,C' '                                                   
*                                                                               
DR750    MVC   COLDATE,SPACES      INITIALIZE  COLDATE                          
         LA    R0,15                                                            
         LA    R1,APWORK           START  OF   STRING                           
         LA    R2,COLDATE                                                       
*                                                                               
DR760    CLI   0(R1),C' '          SQUISH TOGETHER                              
         BNH   DR770               BLANK, NEXT CHARACTER                        
         MVC   0(1,R2),0(R1)       INSERT THE  CHARACTER                        
         LA    R2,1(,R2)           BUMP   UP   OUTPUT POINTER                   
*                                                                               
DR770    LA    R1,1(,R1)           BUMP   UP   INPUT  POINTER                   
         BCT   R0,DR760                                                         
*                                                                               
DR800    SR    R1,R1                                                            
         IC    R1,RCLWDTH          CURRENT COLUMN WIDTH                         
         CVD   R1,APDUB            DISPLAY COLUMN WIDTH                         
         OI    APDUB+7,X'0F'       CHANGE SIGN TO UNPACK                        
         UNPK  COLWIDE,APDUB       UNPACK TO SCREEN                             
         CLI   COLWIDE,C'0'        SUPPRESS LEADING ZEROS                       
         BNE   *+8                                                              
         MVI   COLWIDE,C' '        CLEAR    LEADING ZERO                        
         OI    COLWIDEH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         MVI   COLTOTL,C' '                                                     
         TM    RCLOPT,RCLACCM      IS IT AN ACCUMULATED COLUMN?                 
         BO    DR830               YES SO GET OUT                               
         MVC   COLTOTL,APNO        SET OPTION TO NO                             
*&&US                                                                           
         TM    RCLOPT4,RCLAIDX     Accent indexing                              
         BZ    *+12                                                             
         MVI   COLTOTL,C'I'        Yes                                          
         B     DR840                                                            
*&&                                                                             
*        TM    RCLOPT4,RCLAIDX     Accent indexing                              
*        BZ    *+12                                                             
*        MVI   COLTOTL,C'I'        Yes                                          
*        B     DR840                                                            
*                                                                               
         TM    RCLOPT2,RCLTOT      TOTAL OPTION ON?                             
         BZ    DR840                                                            
         MVC   COLTOTL,APYES       SET OPTION TO YES                            
         B     DR840                                                            
*                                                                               
DR830    TM    RCLOPT,RCLNOTOT                                                  
         BZ    *+8                                                              
         MVI   COLTOTL,C'S'        SET TO SUPRESS TOTAL IN COLUMN               
         TM    RCLOPT,SUPPRESS                                                  
         BZ    *+10                                                             
         MVC   COLTOTL,APONLY      SET TO SUPRESS DETIAL IN COLUMN              
*                                                                               
DR840    MVC   COLPRNT,APYES       DEFAULT TO PRINT COLUMN                      
         TM    RCLOPT,HIDE         HIDDEN COLUMN?                               
         BZ    DR845                                                            
         MVC   COLPRNT,APNO                                                     
*&&US                                                                           
         TM    RCLOPT4,RCLXCOL     PROCESS COLUMN + HIDE + DON'T SORT           
         BZ    *+12                                                             
         MVI   COLPRNT,C'X'                                                     
         B     DR850                                                            
*&&                                                                             
         TM    RCLOPT2,RCLCZERO                                                 
         BZ    DR845                                                            
         MVI   COLPRNT,C'H'        HIDDEN COL AND COL=ZERO                      
         B     DR850                                                            
*                                                                               
DR845    TM    RCLOPT2,RCLCZERO    ELIMINATE RECS IF COL=ZERO                   
         BZ    *+8                                                              
         MVI   COLPRNT,C'C'                                                     
*                                                                               
         TM    RCLOPT,RCLMERGE    MERGE DATA IF SELECTED $COLS=ZERO             
         BZ    *+8                                                              
         MVI   COLPRNT,C'M'                                                     
*                                                                               
         USING MNOELD,R2                                                        
DR850    OI    COLPRNTH+6,FVOXMT                                                
         SR    RF,RF               CLEAR REGISTER                               
         L     R2,AIOAREA1         ->    RECORD                                 
         AH    R2,DATADISP         ->    1ST  ELEMENT                           
*                                                                               
DR860    DS    0H                                                               
         CLI   0(R2),0             END   OF   RECORD ?                          
         BE    DR880               YES,  GET  NEXT COLUMN                       
         CLI   MNOEL,MNOELQ        MESSAGE    ELEMENT ?                         
         BH    DR880               HIGH, GET  NEXT COLUMN                       
         BNE   DR870               NO,   GET  NEXT ELEMENT                      
         OI    MSGELSW,MSGELFND    MESSAGE    ELEMENT   FOUND                   
         CLC   MNOSEQ,RCLSEQ       MATCH ON   COLUMN ?                          
         BH    DR880               HIGH, GET  NEXT COLUMN                       
         BNE   DR870               NO,   GET  NEXT ELEMENT                      
         CLI   MNOSTYPE,MNOSTCOL   COLUMN     TYPE MESSAGE ?                    
         BNE   DR870               NO,   GET  NEXT ELEMENT                      
*                                                                               
*                                  JUST  DISPLAY   THE  1ST  MSG  FOUND         
*                                  WHICH MESSAGE   TYPE ?                       
         CLI   MNOMTYPE,MNOMTERR   ERROR ?                                      
         BNE   *+10                                                             
         MVC   COLNUM(1),APERROR                                                
         CLI   MNOMTYPE,MNOMTWRN   WARNING ?                                    
         BNE   *+10                                                             
         MVC   COLNUM(1),APWARN                                                 
         CLI   DSRNMTYP,C' '       ANY   PREVIOUS  MSG  TYPE                    
         BNE   DR880               YES,  GET  NEXT COLUMN                       
         MVC   DSRNMTYP,MNOMTYPE   NO,   SAVE MSG  TYPE                         
         MVC   DSRNMNUM,MNOMNUM          SAVE MSG  NUMBER                       
         MVC   DSRNCOL,MNOSEQ            SAVE COLUMN    NUMBER                  
         MVI   DSRNDTYP,0                ASSUME    NO   MSG  DATA               
         MVC   DSRNDATA,SPACES           CLEAR     MSG  DATA                    
         IC    RF,MNOMLN           GET   MSG  LENGTH                            
         SHI   RF,(MNOMELQ+L'MNOMDTYP)                                          
         LTR   RF,RF               ANY   MSG  DATA ?                            
         BM    DR880               NO,   GET  NEXT COLUMN                       
         STC   RF,DSRNDLNG         YES,  SAVE DATA LENGTH                       
         MVC   DSRNDTYP,MNOMDTYP         SAVE DATA TYPE                         
         BCTR  RF,0                                                             
         EXMVC RF,DSRNDATA,MNOMDATA      SAVE MSG  DATA                         
         B     DR880               GET   NEXT COLUMN                            
*                                                                               
DR870    DS    0H                                                               
         IC    RF,MNOLN            ELEMENT    LENGTH                            
         AR    R2,RF               ->    NEXT ELEMENT                           
         B     DR860                                                            
         DROP  R2                                                               
*                                                                               
DR880    DS    0H                                                               
         LA    R3,COLLNQ(,R3)      NEXT COLFLD ENTRY                            
*                                                                               
DR890    GOTO1 NEXTEL,(R4)                                                      
         BNE   DR900                                                            
         LR    R4,R1                                                            
         LA    RF,FRMFUTXH         ->   AFTER COLUMN AREA                       
         CR    R3,RF               ARE  WE AT END OF COLUMN DISPLAY ?           
         BL    DR460               NO,  LOOP TO DISPLAY NEXT                    
         DROP  R3,R4                                                            
         EJECT ,                                                                
         USING RCLELD,R1                                                        
DR900    DS    0H                                                               
         MVI   APELCODE,RCLELQ     X'C3'                                        
         CLI   APPFKEY,PFK13       COLUMN FILTER REQUESTED ?                    
         BNE   DR905                                                            
         L     R1,AIOAREA1                                                      
         MVC   ELEMSEQ,COLUMN#                                                  
         GOTO1 GETEL               GET  COLUMN DATA ELEMENT                     
         BNE   IVALCOL1            NONE, INVALID COLUMN NUMBER                  
         CLC   COLUMN#,RCLSEQ                                                   
         BNE   IVALCOL1                                                         
*&&UK                                                                           
         TM    RCLOPT4,RCLNACCU    NON ACCUMULATIVE COLFILT                     
         BO    DR905                                                            
         TM    RCLOPT,RCLACCM+RCLEQU                                            
         BZ    IVALCOL1                                                         
*&&                                                                             
*&&US                                                                           
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALCOL1                                                         
         TM    RCLOPT,RCLEQU                                                    
         BO    IVALCOL1                                                         
*&&                                                                             
DR905    SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVI   APELCODE,RCLELQ     X'C3'                                        
         L     R1,AIOAREA1                                                      
         GOTO1 GETEL                                                            
         BNE   DR930                                                            
         LA    R2,1(,R2)           FIRST BOX LINE WIDTH                         
*                                                                               
DR910    SR    RF,RF                                                            
         LA    R3,0(,R3)           COUNT THE NUM OF COLUMNS IN REPORT           
         ICM   RF,1,RCLWDTH        CURRENT COLUMN WIDTH                         
         BZ    DR920               NO WIDTH                                     
         TM    RCLOPT,HIDE         HIDDEN COLUMN ?                              
         BO    DR920               YES, COLUMN TAKES NO SPACE                   
         CLI   RCLSTACK,0          STACKED UNDER ANOTHER COLUMN ?               
         BNE   DR920               YES, COLUMN TAKES NO NEW SPACE               
         LA    R2,1(RF,R2)         ADD UP WIDTHS + ONE FOR COLUMN LINE          
*                                                                               
DR920    GOTO1 NEXTEL                                                           
         BE    DR910               LOOP TO ADD IN NEXT COLUMN WIDTH             
*                                                                               
DR930    XC    FRMWDTH,FRMWDTH                                                  
         OI    FRMWDTHH+6,FVOXMT                                                
         MVC   FRMWDTH(1),APOPNPRN INSERT OPEN   PARENTHESIS AND                
*                                         CLOSED PARENTHESIS                    
         MVC   FRMWDTH+L'FRMWDTH-1(1),APCLSPRN                                  
         CVD   R2,APDUB                                                         
         OI    APDUB+7,X'0F'       CHANGE SIGN FOR UNPACK                       
         UNPK  FRMWDTH+1(L'FRMWDTH-2),APDUB                                     
*                                                                               
         LA    RE,L'FRMWDTH-2      CHANGE LEADING ZEROS TO                      
         LA    RF,FRMWDTH+1               BLANKS                                
*                                                                               
DR935    CLI   0(RF),C'0'                                                       
         BNE   DR950                                                            
         MVI   0(RF),C' '                                                       
         LA    RF,1(,RF)                                                        
         BCT   RE,DR935                                                         
*                                                                               
DR950    MVC   FVMSGNO,DRMSGNO     PICK UP   ANY  DISPLAY   ERRORS              
         MVC   FVADDR,DRCURSOR                                                  
*                                                                               
         XC    APCURSOR,APCURSOR   CLEAR CURSOR LOCATION                        
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERROR OCCURED ?                         
         BNE   DR990               YES, USE FVADDR                              
         MVC   APCURSOR,ACURSOR    MOVE CURSOR  INTO APPLICATION CURSOR         
*                                                                               
DR990    DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERROR     OCCURED ?                     
         BNE   DR995               YES, SKIP                                    
         TM    MSGELSW,MSGELFND    ANY  MESSAGE   ELEMENT   FOUND ?             
         BZ    DR995               NO,  SKIP                                    
         CLI   DSRNMTYP,C' '       ANY  PREVIOUS  MSG  TYPE                     
         BNE   DR993               YES, GENERATE  MSG                           
         MVI   DSRNMTYP,MNOMTWRN   SET  WARNING   MSG                           
*                                  SAY  MESSAGE(S)     GENERATED                
         MVC   DSRNMNUM,=AL2(ACWMGEN)                                           
         MVI   DSRNCOL,1           SET  FIRST     COLUMN                        
         MVI   DSRNDLNG,0          NO   EXTRA     DATA                          
*                                                                               
DR993    DS    0H                                                               
         OI    MSGELSW,MSGELERR    SET  MESSAGE   EL   ERROR                    
         MVC   FVMSGNO,DSRNMNUM    GET  DELAYED   MSG  NUMBER                   
         MVC   ERRCOL#,DSRNCOL     GET  DELAYED   MSG  COLUMN                   
         TM    MSGELSW,MSGELINS    IS   INSERT    ON   ?                        
         BZ    DR994               NO,  SKIP                                    
         CLC   ERRCOL#,COLINSRT    WAS  INSERT    BEFORE    THIS COL ?          
         BL    DR994               NO,  USE       COLUMN    NUMBER              
         ZIC   RE,ERRCOL#          GET  COLUMN    NUM                           
         LA    RE,1(,RE)           BUMP COLUMN    NUM                           
         STC   RE,ERRCOL#          SAVE COLUMN    NUM                           
*                                                                               
DR994    DS    0H                                                               
         CLI   DSRNDLNG,0          ANY  DELAYED   MSG  DATA ?                   
         BNE   IVALWDAT            YES, OUTPUT    WITH DATA                     
         CLI   DSRNMTYP,MNOMTERR   IS   IT   MSG  TYPE ERROR ?                  
         BE    *+10                YES, SKIP                                    
         MVC   FVOMTYP,DSRNMTYP    SET  MSG  TYPE                               
         B     IVALCOLC            SET  CURSOR                                  
*                                                                               
DR995    DS    0H                                                               
*                                  TURN  OFF 'IN     DISPLAY ROUTINE'           
         NI    MSGELSW,TURNOFF-MSGELIDR                                         
         SR    RE,RE               SET CONCODE TO YES                           
         B     XIT                 RETURN                                       
         DROP  R1                                                               
         EJECT ,                                                                
*=====================================================================*         
*  ROUTINE TO COPY RECORD                                             *         
*=====================================================================*         
         SPACE 1                                                                
CPYREC   GOTO1 =A(COPYREC),RR=APRELO                                            
         B     EXIT97                                                           
*=====================================================================*         
*  ROUTINE TO RESTORE RECORD                                          *         
*=====================================================================*         
         SPACE 1                                                                
RESREC   CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT                                                             
         GOTO1 =A(RESTORE),RR=APRELO                                            
         B     EXIT97                                                           
         EJECT ,                                                                
*=====================================================================*         
*  ROUTINE TO DELETE RECORD                                           *         
*=====================================================================*         
         SPACE 1                                                                
DELREC   CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT                                                             
         GOTO1 =A(DELETE),RR=APRELO                                             
         B     EXIT97                                                           
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO VALIDATE SELECT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
VALSEL   MVC   APRECKEY,SPACES                                                  
         MVC   LSTCDE,AC@FRMAT                                                  
         OI    LSTCDEH+6,FVOXMT                                                 
         CLC   FLTFORM,SPACES                                                   
         BE    VALSEL10                                                         
         MVC   LSTCODE,FLTFORM                                                  
         OI    LSTCODEH+6,FVOXMT                                                
         MVC   FLTFORM,SPACES                                                   
*                                                                               
VALSEL10 LA    R0,LSTSEL1H                                                      
         ST    R0,APPARM                                                        
         LA    R1,LSTSEL2H                                                      
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         STH   R1,APHALF           SAVE VALUE                                   
         MVI   APPARM+4,15         ONLY FIFTEEN LINES OF SELECT                 
         TM    INOPT3,INOPXDET     EXTRA DETAILS ?                              
         BZ    VALSEL20                                                         
         SLL   R1,1                MULTIPLY BY TWO                              
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,7                                                       
*                                                                               
VALSEL20 LA    R0,7                                                             
         LA    RF,LSTSEL1H                                                      
*                                                                               
VALSEL22 AH    RF,APHALF                                                        
         NI    FVATRB-FVIHDR(RF),TURNOFF-FVAPROT                                
         TM    INOPT3,INOPXDET                                                  
         BZ    *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAPROT                                        
         AH    RF,APHALF                                                        
         BCT   R0,VALSEL22                                                      
*                                                                               
         TM    INOPT3,INOPXDET     PROTECT LAST LINE OR NOT ?                   
         BZ    VALSEL90                                                         
         OI    6(RF),FVOXMT        TRANSMIT                                     
         OI    FVATRB-FVIHDR(RF),FVAPROT                                        
         SR    R1,R1               CLEAR LAST LINE IF OPTION ON                 
         IC    R1,0(,RF)                                                        
         SH    R1,=Y(L'FVIHDR+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,RF),SPACES                                            
         IC    R1,0(,RF)                                                        
         AR    RF,R1                                                            
         OI    6(RF),FVOXMT        TRANSMIT                                     
         IC    R1,0(,RF)                                                        
         SH    R1,=Y(L'FVIHDR+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,RF),SPACES                                            
*                                                                               
VALSEL90 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET SELECT RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
RGETSEL  GOTO1 =A(GETSEL),RR=APRELO                                             
         B     EXIT                                                             
***********************************************************************         
*  ROUTINE TO DISPLAY SELECT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING LISTD,R4                                                         
REDIS    DS    0H                                                               
         MVI   ACPFCUR,PFKNEXT     GO TO NEXT RECORD                            
*                                                                               
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM           R4=A(LIST/SELECT LINE)                       
         TWAXC LISTDATH,LISTDATH                                                
         TM    INOPT3,INOPXDET                                                  
         BZ    DISSEL02                                                         
         TWAXC LISTXDH,LISTXDH                                                  
*                                                                               
DISSEL02 MVC   LISTFMT,RESKFORM                                                 
         MVC   LISTTYP,SCRTYP                                                   
         TM    INOPT3,INOTRN       TRANSLATE  ALL  REQUEST ?                    
         BZ    DISSEL05            NO,  SKIP                                    
         MVI   APELCODE,STYELQ     X'25' FREE FORM ELEMENT                      
         GOTO1 GETEL,(R2)          FIND  REPORT    TYPE                         
         BE    *+6                 FOUND,     CONTINUE                          
         DC    H'00'               NOT   FOUND,    ABEND                        
*                                  INSERT     REPORT    TYPE                    
         MVC   LISTTYP,STYNAME-STYELD(R1)                                       
*                                                                               
DISSEL05 GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         XC    APBYTE,APBYTE                                                    
         TM    INOPT1,INORDTE      WANT REQUEST DATE                            
         BZ    *+8                 NO                                           
         MVI   APBYTE,C'R'                                                      
         GOTO1 GETACT,APPARM,(APBYTE,(R2)),(L'LISTADTE,LISTADTE)                
         TM    RESRSTA,X'80'                                                    
         BZ    DISSEL10                                                         
         OI    LISTDATH+1,FVAHIGH                                               
         GOTO1 TEXTGET,APPARM,1600,(L'LISTNME,LISTNME),0                        
         B     EXIT                                                             
*                                                                               
DISSEL10 GOTO1 GETNAME,APPARM,(R2),(L'LISTNME,LISTNME)                          
         TM    INOPT3,INOPXDET                                                  
         BZ    DISSEL90                                                         
         LA    R6,LISTXD                                                        
         AH    R2,DATADISP                                                      
         MVC   0(1,R6),APOPNPRN                                                 
         LA    R6,1(,R6)                                                        
*                                                                               
DISSEL15 CLI   0(R2),0             EOR                                          
         BE    DISSEL80                                                         
         CLI   0(R2),RPFELQ        X'C4' PROFILE ELEMENT                        
         BE    DISSEL20                                                         
         CLI   0(R2),RFLELQ        X'C5' FILTER ELEMENT                         
         BE    DISSEL30                                                         
         CLI   0(R2),RCPELQ        X'C8' RECAP  ELEMENT                         
         BE    DISSEL40                                                         
         CLI   0(R2),DTSELQ        X'FB' REQUEST DATE                           
         BE    DISSEL50                                                         
DISSEL18 SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     DISSEL15                                                         
*                                                                               
         USING RPFELD,R2                                                        
DISSEL20 TM    RPFPOPT2,RPFDOWN    FORCED DOWN-LOAD                             
         BZ    DISSEL22                                                         
         MVCDD 0(4,R6),AC#DL                                                    
         GOTO1 VDICTAT,APPARM,C'SL  ',(4,(R6))                                  
         GOTO1 =A(FINDEND),4,RR=APRELO    R6 IS ASSUMED                         
         BNE   DISSEL22                                                         
         MVC   0(1,R6),SCCOMMA                                                  
         LA    R6,1(,R6)                                                        
*                                                                               
DISSEL22 CLI   RPFLN,RPFLNQ                                                     
         BNH   DISSEL24                                                         
         TM    RPFXMIT,RPFXFTP     FTP IN USE ?                                 
         BZ    DISSEL24                                                         
         MVC   0(3,R6),=C'FTP'                                                  
         MVC   3(1,R6),SCCOMMA                                                  
         LA    R6,4(,R6)                                                        
*                                                                               
DISSEL24 B     DISSEL18            GET NEXT ELEMENT                             
*                                                                               
         USING RFLELD,R2                                                        
DISSEL30 CLI   RFLTYPE,RFLBUDGT    BUDGET COLUMN ?                              
         BNE   DISSEL18                                                         
         MVCDD 0(8,R6),AC#BGTS                                                  
         GOTO1 VDICTAT,APPARM,C'SL  ',(8,(R6))                                  
         GOTO1 =A(FINDEND),8,RR=APRELO    R6 IS ASSUMED                         
         BNE   DISSEL18                                                         
         MVC   0(1,R6),SCCOMMA                                                  
         LA    R6,1(,R6)                                                        
         B     DISSEL18                                                         
*                                                                               
         USING RCPELD,R2                                                        
DISSEL40 MVCDD 0(8,R6),AC#RECAP                                                 
         GOTO1 VDICTAT,APPARM,C'SL  ',(8,(R6))                                  
         GOTO1 =A(FINDEND),8,RR=APRELO    R6 IS ASSUMED                         
         BNE   DISSEL18                                                         
         MVC   0(1,R6),SCCOMMA                                                  
         LA    R6,1(,R6)                                                        
         SR    R1,R1                                                            
*                                                                               
DISSEL42 IC    R1,1(,R2)           GET PAST LAST RCPELQ ELEMENT                 
         CLI   0(R2),RCPELQ        X'C9'                                        
         BNE   DISSEL15                                                         
         AR    R2,R1                                                            
         B     DISSEL42                                                         
*                                                                               
         USING DTSELD,R2                                                        
DISSEL50 CLI   DTSTYPE,DTSTREQ                                                  
         BNE   DISSEL18                                                         
         MVCDD 0(5,R6),AC#REQ                                                   
         GOTO1 VDICTAT,APPARM,C'SL  ',(5,(R6))                                  
         GOTO1 =A(FINDEND),5,RR=APRELO    R6 IS ASSUMED                         
         BNE   DISSEL18                                                         
         MVI   0(R6),C'='                                                       
         LA    R6,1(,R6)                                                        
         GOTO1 VDATCON,APPARM,(2,DTSDATE),(10,(R6))                             
         LA    R6,8(,R6)                                                        
         MVC   0(1,R6),SCCOMMA                                                  
         LA    R6,1(,R6)                                                        
         B     DISSEL18                                                         
*                                                                               
DISSEL80 BCTR  R6,0                                                             
         CLC   SCCOMMA,0(R6)                                                    
         BE    *+8                                                              
         LA    R6,1(,R6)                                                        
         MVC   0(1,R6),APCLSPRN                                                 
*                                                                               
DISSEL90 B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROCESS FROM FORMAT LIST SCREEN                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING LISTD,R4                                                         
PROCLST  L     R4,APLSTADD         LOAD LIST LINE                               
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         MVC   RESKFORM,LISTFMT                                                 
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IORDD+IOACCFIL+IO2                                           
         BL    PROCLST9            HARDWARE ERROR                               
         BE    PROCLST2                                                         
         CLI   APACTN,ACTDEL       ACTION DELETE                                
         BNE   PROCLST2                                                         
         TM    RESRSTA,X'80'                                                    
         BZ    PROCLST8            NO SO DELETE THE RECORD                      
         MVC   FVMSGNO,=AL2(77)    RECORD DELETED ALREADY                       
         B     PROCLST9                                                         
*                                                                               
PROCLST2 DS    0H                                                               
         CLI   APACTN,ACTRES       ACTION RESTORE                               
         BNE   PROCLST3                                                         
         TM    RESRSTA,X'80'                                                    
         BO    PROCLST8            EXIT                                         
         MVC   FVMSGNO,=AL2(59)                                                 
         B     PROCLST9                                                         
*                                                                               
         USING STYELD,R1                                                        
PROCLST3 CLI   APACTN,ACTREQ                                                    
         BNE   PROCLST8                                                         
         OC    TWASAGN,TWASAGN     NEW SECURITY                                 
         BZ    PROCLST8                                                         
         MVI   APELCODE,STYELQ     X'25' SCRIBE REPORT TYPE                     
         L     R1,AIOAREA2                                                      
         GOTO1 GETEL                                                            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         SR    R3,R3               KEYWORD SECURITY (1-8)                       
         ICM   R3,1,STYSEC#1       ANY SECURITY SET UP?                         
         GOTO1 AFMTSEC,APPARM,(R3),1                                            
         CLI   APPARM,0                                                         
         BNE   IVALSEC                                                          
*                                                                               
         SR    R3,R3               PROFILE SECURITY (33-40)                     
         ICM   R3,1,STYSEC#5       ANY SECURITY SET UP?                         
         GOTO1 AFMTSEC,APPARM,(R3),33                                           
         CLI   APPARM,0                                                         
         BNE   IVALSEC                                                          
*                                                                               
PROCLST8 MVC   FVMSGNO,=AL2(FVFOK) RESET TO BE OK                               
*                                                                               
PROCLST9 B     EXIT99                                                           
         DROP  R1,R2,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  ERRORS TO DISPLAY ON TOP OF SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
IVALSEC  MVC   FVMSGNO,=AL2(1447)  AE$RQACD                                     
         XC    APCURSOR,APCURSOR           CLEAR APCURSOR                       
         B     EXIT99                                                           
         SPACE 1                                                                
IVALCMAX LA    R1,MAXCOLS                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  FVXTRA(2),APDUB                                                  
         LHI   R0,1448             AE$CIMXC                                     
         B     IVALEXSC                    SET  CURRENT CURSOR                  
         SPACE 1                                                                
IVALEXP  MVC   FVXTRA(12),APWORK           INVALID EXPRESSION                   
         B     IVALEX                                                           
         SPACE 1                                                                
IVALUBPR LHI   R0,ACEUNBL          UNBALANCE PARENTHESIS                        
         MVC   FVXTRA(12),APWORK                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALKYWD LHI   R0,ACEKYWDI         KEYWORD NOT RECOGNIZED                       
         MVC   FVXTRA(L'APKEYWRD),APKEYWRD                                      
         LTR   R1,R1                                                            
         BZ    IVALEXIT                                                         
         LHI   R0,ACEKYWDI         KEYWORD NOT VALID                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVAL2MNY LHI   R0,ACE2MANY         TOO MANY PARAMETERS                          
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALADR  LHI   R0,1443                                                          
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT LHI   R0,ACEINPT                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALFOOT LHI   R0,ACEINPT                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEDUP LHI   R0,ACEDUPR                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCUL0 MVC   FVXTRA(10),12(R4)          INVALID UNIT/LEDGER                   
*                                                                               
IVALCULR LHI   R0,ACEIVUL                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALLOW  LHI   R0,ACE2FEW                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALHIGH LHI   R0,ACE2MANY                                                      
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALHEAD LHI   R0,ACEIVHD                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALTYPE LHI   R0,ACEIVTY                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALTOT  LHI   R0,ACEIVTO                                                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEKEY SR    R0,R0                                                            
         ICM   R0,3,=AL2(FVFEKEY)                                               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALHLP  LHI   R0,ACEIVHLP                                                      
         B     IVALEXSC                    SET  CURRENT CURSOR                  
         SPACE 1                                                                
IVALCOLF MVC   FVADDR,ACURSOR                                                   
         MVI   COLUMN#,0                                                        
         LHI   R0,ACEIVCN                  INVALID COLUMN FILTER                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCDEL LHI   R0,ACEXDEL                  CAN NOT COPY OVER A RECORD           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALNADD LHI   R0,1443                     CAN NOT ADD RECORD                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRECX LHI   R0,ACERECEX                                                      
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALNNUM SR    R0,R0                       NOT NUMERIC                          
         ICM   R0,3,=AL2(FVFNOTN)                                               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIGE0 LHI   R0,ACEIGE0                  INPUT NOT >= 0                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALMIX  LHI   R0,ACECMXE                  CANNOT MIX OLD WITH NEW              
         L     R1,FULL                     SAVED A(COLDATAH)                    
         ST    R1,FVADDR                                                        
         B     IVALEXIT                    ESTIMATE KEYWORDS                    
         SPACE 1                                                                
IVALNOCA LHI   R0,ACEFMICA                 FORMAT MUST INCLUDE CONTRA           
         L     R1,FULL                     SAVED A(COLDATAH)                    
         ST    R1,FVADDR                                                        
         B     IVALEXIT                    A/C WITH THIS KEYWORD                
         SPACE 1                                                                
IVALCNNU LHI   R0,ACERONNM                 RANK ON MUST BE NON-NUMERIC          
         B     IVALCNU1                                                         
*                                                                               
IVALCNU  LHI   R0,ACERCNUM                 RANK COLUMN MUST BE NUMERIC          
*                                                                               
         USING COLD,R3                                                          
IVALCNU1 LA    R1,COLDATAH                 ->   COLUMN DATA FIELD               
         ST    R1,FVADDR                   SET  CURSOR ADDRESS                  
         B     IVALEXIT                                                         
         DROP  R3                                                               
         SPACE 3                                                                
IVALWDAT DS    0H                          INSERT WITH DATA                     
         MVC   FVMSGNO,=AL2(FVFGTSET)      OVER-RIDE FVERR CALL                 
         SR    RF,RF                                                            
         IC    RF,ERRCOL#                  INSERT ERROR COLUMN NUMBER           
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         XC    APXTRA,APXTRA                                                    
         MVI   APXTRA,3                                                         
         UNPK  APXTRA+1(2),APDUB                                                
         CLI   DSRNDTYP,MNOMDCOL           COLUMN TYPE DATA                     
         BNE   IVALWDA1                    NO,    SKIP                          
         MVI   APXTRA+3,3                  INSERT EXTRA COLUMN NUMBER           
         IC    RF,DSRNDATA                                                      
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APXTRA+4(2),APDUB                                                
         B     IVALWDA2                    GENERATE THE MESSAGE                 
*                                                                               
IVALWDA1 DS    0H                                                               
         IC    RF,DSRNDLNG                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,APXTRA+3                 EXTRA DATA LENGTH + 1                
         SH    RF,=H'02'                                                        
         EXMVC RF,APXTRA+4,DSRNDATA        INSERT EXTRA TEXT DATA               
*                                                                               
         USING GETTXTD,R1                                                       
IVALWDA2 LA    R1,APPARM                                                        
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTAOUT,AMSGHDR+1                                                 
         MVC   GTMSYS,ASSYSO               NO,  SET NATIVE SYSTEM               
         MVC   GTMSGNO,DSRNMNUM            MESSAGE NUMBER                       
         MVC   GTMTYP,DSRNMTYP             MESSAGE TYPE                         
         LA    RE,APXTRA                                                        
         STCM  RE,7,GTASUBST               APPEND &1 AND &2 CALL                
         B     IVALCOLC                    SET  CURSOR                          
         DROP  R1                                                               
*                                                                               
*VALSOR2 MVC   FVMSGNO,=AL2(ACEIVSS)       INVALID SORT SEQUENCE                
*        LR    R1,R3                       COLUMN NUMBER IN R3                  
*        BCTR  R1,0                                                             
*        B     IVALCAL1                                                         
*                                                                               
         USING RCLELD,R1                                                        
*VALDSRT LHI   R0,ACEDUSN                  DUPLICATE SORT NUMBER                
*        B     IVALJOB0                                                         
*                                                                               
*VALJOBR LHI   R0,ACEMJBR                  MAX EST KEYWORDS REACHED             
*                                                                               
*VALJOB0 ZIC   R3,RCLSEQ                   COLUMN NUMBER IN RCLSEQ              
*        LR    R1,R3                                                            
*        BCTR  R1,0                                                             
*        B     IVALCAL1                                                         
*        DROP  R1                                                               
*                                                                               
IVALCOL1 LHI   R0,ACEIVCN                  INVALID  COLUMN  NUMBER              
         ZIC   R1,COLUMN#                  POINT TO CORRECT COLUMN              
         ZIC   RE,STSEQ                    SUBTRACT START   SEQUENCE            
         SR    R1,RE                                NUMBER                      
         B     IVALCAL1                                                         
*                                                                               
*VALVERT LHI   R0,ACEVERT                  INVALID  VERTICAL                    
*        B     IVALCAL0                                                         
*                                                                               
*VALCALE LHI   R0,ACEIVCN                  INVALID  COLUMN  NUMBER              
*        ZIC   R1,COL#                     GET      CURRENT SEQUENCE            
*        CVD   R1,APDUB                    CONVERT  TO      DECIMAL             
*        OI    APDUB+7,X'0F'               CLEAR    SIGN                        
*        UNPK  FVXTRA(2),APDUB             UNPACK                               
*        B     IVALCAL0                                                         
*                                                                               
IVALCOLC SR    R1,R1                                                            
         IC    R1,ERRCOL#                                                       
         BCTR  R1,0                                                             
         ZIC   R3,STSEQ                    MAKE     SURE    WE                  
         BCTR  R3,0                                 END     UP                  
         SR    R1,R3                                ON      SCREEN              
         BNM   *+6                         IF       LOW,    USE                 
         SR    R1,R1                                START   SEQUENCE            
         CHI   R1,MAXDISP-1                IF       HIGH,   USE                 
         BNH   *+8                                  LAST    COLUMN              
         LA    R1,MAXDISP-1                         ON      SCREEN              
*                                          NOW      ON      SCREEN              
         MHI   R1,COLLNQ                   FIND     CURSOR  ADDRESS             
         LA    R1,FRMCOL1H+COLDATAH-COLD(R1)                                    
         ST    R1,FVADDR                                                        
         B     IVALEX                                                           
*                                                                               
*IVALCALC LHI   R0,ACEIVCN                  INVALID  COLUMN  NUMBER             
*                                                                               
*IVALCAL0 ZIC   R1,0(RF)                    GET      COLUMN  NUMBER             
*         BCTR  R1,0                        MINUS    ONE                        
*                                                                               
IVALCAL1 ZIC   R3,STSEQ                    MAKE     SURE    WE                  
         BCTR  R3,0                                 END     UP                  
         SR    R1,R3                                ON      SCREEN              
         BNM   *+6                         IF       LOW,    USE                 
         SR    R1,R1                                START   SEQUENCE            
         CHI   R1,MAXDISP-1                IF       HIGH,   USE                 
         BNH   *+8                                  LAST    COLUMN              
         LA    R1,MAXDISP-1                         ON      SCREEN              
*                                          NOW      ON      SCREEN              
         MHI   R1,COLLNQ                   FIND     CURSOR  ADDRESS             
         LA    R1,FRMCOL1H+COLDATAH-COLD(R1)                                    
         ST    R1,FVADDR                                                        
         B     IVALEXIT                                                         
         SPACE 3                                                                
IVALEXSC MVC   FVADDR,ACURSOR              POINT TO CURRENT CURSOR              
         SPACE 1                                                                
IVALEXIT STCM  R0,3,FVMSGNO        R0=MSG NO. SET BY CALLER                     
*                                  ELSE FVMSGNO SET BY CALLER                   
IVALEX   XC    APCURSOR,APCURSOR           CLEAR APPLICATION CURSOR             
         TM    MSGELSW,MSGELIDR            IN    DISPLAY     ROUTINE ?          
         BO    XIT                         YES,  SUBROUTINE  EXIT               
         B     EXIT                        NO,   ROUTINE     EXIT               
         EJECT ,                                                                
         LTORG                                                                  
         SPACE 1                                                                
         DROP  R5,R9,RA                                                         
         EJECT ,                                                                
HEADTAB  DC    AL4(FRMTITLH-TWAD),AL1(RHDTITL,1,L'FRMTITL-1)                    
         DC    AL4(FRMCNTRH-TWAD),AL1(RHDCNTR,1,L'FRMCNTR-1)                    
         DC    AL4(FRMLHEDH-TWAD),AL1(RHDLFTH,1,L'FRMLHED-1)                    
         DC    AL4(FRMRHEDH-TWAD),AL1(RHDRHTH,1,L'FRMRHED-1)                    
         DC    AL4(FRMFOOTH-TWAD),AL1(RHDFTLN,1,L'FRMFOOT-1)                    
         DC    AL4(0),AL1(0,0,0)                                                
         EJECT ,                                                                
***********************************************************************         
*  FIND END OF DATA STRING AND RESET R6, R6 IS STARTING LOCATION      *         
*       CC <> IF NO DATA                                              *         
***********************************************************************         
FINDEND  NTR1  BASE=*,LABEL=*      R6 IS ASSUMED, STARTING LOCATION             
         LR    RF,R1               R1 HAS LENGTH OF POSSIBLE DATA               
         BCTR  R1,0                LESS ONE TO CHECK LAST CHARACTER             
         EXCLC R1,0(R6),SPACES                                                  
         BNH   FINDENDN                                                         
         LA    RE,0(R1,R6)         POINT TO LAST CHARACTER                      
FINDEN10 CLI   0(RE),C' '                                                       
         BH    FINDEN20                                                         
         BCTR  RE,0                                                             
         BCT   RF,FINDEN10                                                      
         DC    H'00'                                                            
*                                                                               
FINDEN20 LA    R6,1(,RE)           SET R6 TO NEW LOCATION                       
*                                                                               
FINDENDY SR    RE,RE                                                            
FINDENDN LTR   RE,RE                                                            
         XIT1  REGS=(R6)                                                        
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*    GET RECORDS TO PASS TO SCREEN. FILTER OUT WITH OPTIONS           *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING LISTD,R4                                                         
         USING TWAD,R5                                                          
GETSEL   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         MVC   IOKEY,APRECKEY                                                   
         CLI   APINDS,APILFLST     FIRST ENTRY ?                                
         BE    GETSEL05                                                         
         CLI   APINDS,APILNLST     LAST  ENTRY ?                                
         BNE   GETSEL06                                                         
*                                                                               
GETSEL05 MVC   SAVLSTKY,APRECKEY                                                
*                                                                               
GETSEL06 MVC   RESKEY,APRECKEY                                                  
         TM    SCRTYPH+4,X'80'     SAME AS BEFORE?                              
         BO    GETSEL08                                                         
         TM    LSTCODEH+4,X'80'    SAME AS BEFORE?                              
         BO    GETSEL08                                                         
         TM    LSTOWNH+4,X'80'     SAME AS BEFORE?                              
         BZ    GETSEL09                                                         
*                                                                               
GETSEL08 MVI   RESKEY,0            RESET FROM BEGINING                          
*                                                                               
GETSEL09 NI    SCRTYPH+4,TURNOFF-X'80'                                          
         NI    LSTCODEH+4,TURNOFF-X'80'                                         
         NI    LSTOWNH+4,TURNOFF-X'80'                                          
*                                                                               
         CLI   RESKEY,RESKTYPQ     X'2D'                                        
         BNE   GETSEL10                                                         
         CLI   RESKSUB,RESKSUBQ    X'02'                                        
         BNE   GETSEL10                                                         
         CLC   RESKCPY,CUABIN      SAME COMPANY CODE?                           
         BE    GETSEL20                                                         
*                                                                               
GETSEL10 MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         MVI   SCRLPAGE,0                                                       
         GOTO1 AFVAL,LSTCODEH      ANY     FORMAT CODE ?                        
         BNE   GETSEL60            READ    FIRST  RECORD                        
         SR    R6,R6                                                            
         IC    R6,FVXLEN           GET     FORMAT CODE EX   LENGTH              
         LA    RE,FVIFLD(R6)       ->      FORMAT CODE LAST CHARACTER           
         CLI   0(RE),C'*'          USER    WANTS  STARTING  FROM CODE ?         
         BNE   *+6                 NO,     SKIP                                 
         BCTR  R6,0                YES,    DON'T  MOVE THE  C"*"                
         EXMVC R6,RESKFORM,FVIFLD  MOVE    FORMAT CODE                          
         B     GETSEL60                                                         
*                                                                               
GETSEL20 TM    APINDS,APILRERD     RE-READ RECORD, IO OCCURED                   
         BZ    GETSEL40                                                         
         GOTO1 AIO,IORDD+IOACCFIL+IO1                                           
         BE    GETSEL80                                                         
         TM    IOERR,IOEDEL                                                     
         BO    GETSEL80                                                         
         B     GETSEL96                                                         
*                                                                               
GETSEL40 TM    APINDS,APILNSEQ     CONTINUE WITH SEQUENTIAL READ?               
         BO    GETSEL80                                                         
*                                                                               
GETSEL60 LA    R1,IOHID+IOACCFIL+IO1                                            
         B     GETSEL81                                                         
*                                                                               
GETSEL80 LA    R1,IOSQD+IOACCFIL+IO1                                            
*                                                                               
GETSEL81 GOTO1 AIO                                                              
         BE    GETSEL82                                                         
         TM    IOERR,IOEDEL                                                     
         BZ    GETSEL96                                                         
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE THE RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
GETSEL82 L     R2,AIOAREA1                                                      
         CLI   RESKEY,RESKTYPQ     X'2D'                                        
         BNE   GETSEL96                                                         
         CLI   RESKSUB,RESKSUBQ    X'02'                                        
         BNE   GETSEL96                                                         
         CLC   RESKCPY,CUABIN      SAME   COMPANY CODE ?                        
         BNE   GETSEL96                                                         
         OC    RESKFORM,RESKFORM   ANY    FORMAT  NAME ?                        
         BZ    GETSEL80            NO,    SKIP                                  
         CLI   RESKSEQ,RESKSREG    NORMAL FORMAT  TYPE ?                        
         BNE   GETSEL80            NO,    SKIP                                  
         SPACE 1                                                                
***********************************************************************         
*  REPORT TYPE FILTER                                                 *         
***********************************************************************         
         SPACE 1                                                                
         L     R6,AIOAREA1                                                      
         MVI   APELCODE,STYELQ     X'25' FREE FORM ELEMENT                      
         GOTO1 GETEL,(R6)          FIND  REPORT    TYPE                         
         BNE   GETSEL80                                                         
         ST    R1,SAVER1           SAVE    R1                                   
         TM    INOPT3,INOTRN       TRANSLATE ALL REQUESTED ?                    
         BO    GETSEL84            YES, SKIP                                    
         L     R1,SAVER1           RESTORE R1                                   
*                                  DESIRED REPORT TYPE ?                        
         CLC   APREPCDE,STYNAME-STYELD(R1)                                      
         BNE   GETSEL80            NO,  SKIP THIS RECORD                        
         SPACE 1                                                                
***********************************************************************         
*  FILTER ON FORMAT                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETSEL84 GOTO1 AFVAL,LSTCODEH                                                   
         BNE   GETSEL86            NO INPUT, SKIP                               
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         LA    R1,FVIFLD(R6)       POINT TO END                                 
         LA    RF,X'70'            THIS IS A BRANCH NOT EQUAL (BNE)             
         CLI   0(R1),C'*'                                                       
         BNE   *+10                                                             
         LA    RF,X'40'            THIS IS A BRANCH LOW (BL)                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   RESKFORM(0),FVIFLD                                               
         EX    RF,*+6                                                           
         NOPR  0                                                                
         BC    0,GETSEL80                                                       
         SPACE 1                                                                
***********************************************************************         
*  FILTER ON PERSON                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETSEL86 GOTO1 AFVAL,LSTOWNH                                                    
         BNE   GETSEL88                                                         
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         GOTO1 GETPER,APPARM,(R2),(L'TEMPOWN,TEMPOWN)                           
         BNE   GETSEL80                                                         
         EXCLC R6,TEMPOWN,FVIFLD                                                
         BNE   GETSEL80                                                         
         SPACE 1                                                                
***********************************************************************         
*  FILTER ON REQUESTED DATE                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING DTSELD,R3                                                        
GETSEL88 TM    INOPT1,INORDTE      OPTION RQD= INPUT ?                          
         BZ    GETSEL92                                                         
         TM    INOPTA,INOADTES     WERE FILTERING DATES USED ?                  
         BZ    GETSEL92                                                         
         XC    RQDATE,RQDATE                                                    
         L     R6,AIOAREA1                                                      
         MVI   APELCODE,DTSELQ     X'FB' REQUESTED DATE STAMP                   
         GOTO1 GETEL,(R6)                                                       
         BNE   GETSEL90                                                         
         LR    R3,R1                                                            
         GOTO1 VDATCON,APPARM,(2,DTSDATE),(1,RQDATE)                            
         DROP  R3                                                               
*                                                                               
GETSEL90 CLC   RQDATE,OPTDTE1                                                   
         BL    GETSEL80            FILTER OUT THIS ONE                          
         CLC   RQDATE,OPTDTE2                                                   
         BH    GETSEL80            FILTER OUT THIS ONE                          
         SPACE 1                                                                
***********************************************************************         
*  LIST THE RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETSEL92 TM    INOPT3,INOTRN       TRANSLATE ALL REQUESTED ?                    
         BZ    GETSEL94            NO,  SKIP                                    
         L     R6,AIOAREA1                                                      
         GOTO1 GETTYPE,(R6)        RESET    TYPE FIELDS                         
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R6)                                                      
*                                  RE-READ  WITH LOCK                           
         GOTO1 AIO,IORDD+IOACCFIL+IO1+IOLOCK                                    
         BE    GETSEL93            OKAY, CONTINUE                               
         TM    IOERR,IOEDEL        DELETED       RECORD ?                       
         BO    GETSEL93            YES,  CONTINUE                               
         DC    H'00'               NO,   ABEND                                  
*                                                                               
GETSEL93 GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA1                                                      
*                                                                               
GETSEL94 MVC   APRECKEY(L'RESKEY),RESKEY                                        
         B     GETSEL99                                                         
*                                                                               
GETSEL96 MVI   APMODE,APMEOFS      END OF SELECT RECORDS                        
         TM    INOPT3,INOTRN       TRANSLATE ALL REQUEST ?                      
         BZ    GETSEL99            NO,  SKIP                                    
         XC    SCROPT,SCROPT       CLEAR OPTION FIELD                           
         OI    SCROPTH+6,FVOXMT    TRANSMIT                                     
*                                                                               
GETSEL99 MVC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         EJECT ,                                                                
*=====================================================================*         
*  COPY FORMAT RECORD AND RELATED RECORDS                             *         
*=====================================================================*         
         USING RESRECD,R2                                                       
COPYREC  NMOD1 0,**COPY**              COPY WHOLE REOCORD                       
         L     RC,APALOCAL                                                      
         MVC   RESKEY,SAVEKEY          COPY WHOLE REOCORD                       
         GOTO1 AIO,IORDD+IOACCFIL+IO1  RE-ESTABLISH ORIGINAL RECORD             
         BE    CPYREC05                READ OKAY, CONTINUE                      
         MVC   FVXTRA(L'SCRACT),SCRACT SAVE ACTION FOR ERROR MESSAGE            
         TM    IOERR,IOERNF            RECORD NOT FOUND ERROR ?                 
         BO    *+6                     YES, CANNOT COPY A DELETED RCD           
         DC    H'00'                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(ACEXDEL)   CAN NOT COPY OVER A RECORD               
         B     CPYREC90                                                         
*                                                                               
CPYREC05 L     R1,AIOAREA1                                                      
         MVI   APELCODE,PTRELQ         X'FA' ELEMENT                            
         GOTO1 DELEL                   DELETE POINTER ELEMENT                   
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,DTSELQ         X'FB' ELEMENT                            
         GOTO1 DELEL                   DELETE DATE/TIME STAMP ELEMENT           
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,FFTELQ         X'DB' ELEMENT                            
         MVI   ELEMSEQ,FFTTEEXT        Delete extension type                    
         GOTO1 DELEL                   DELETE DATE/TIME STAMP ELEMENT           
*                                                                               
         USING RPFELD,R1                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ         X'C4' ELEMENT                            
         GOTO1 GETEL                   FIND  FIRST RECAP ELEMENT                
         BNE   CPYREC08                NONE, SKIP                               
         NI    RPFXMIT,TURNOFF-(RPFXACNT+RPFXQREP)                              
         DROP  R1                                                               
*                                                                               
         USING STYELD,R1                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,STYELQ         X'25' ELEMENT                            
         GOTO1 GETEL                   FIND  FIRST RECAP ELEMENT                
         BNE   CPYREC08                NONE, SKIP                               
         NI    STYSTAT,TURNOFF-(STYSACNT+STYSQREP)                              
         DROP  R1                                                               
*                                                                               
CPYREC08 L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCPELQ         X'C8' ELEMENT                            
         GOTO1 GETEL                   FIND  FIRST RECAP ELEMENT                
         BNE   CPYREC10                NONE, SKIP                               
*                                                                               
         USING RCPELD,R1               MAP   RECAP ELEMENT                      
         CLI   RCPSEQ,0                MAIN  PROGRAM     ELEMENT ?              
         BNE   CPYREC10                NO,   SKIP                               
         MVC   RCPCODE,APRECKEY+RESKFORM-RESKEY                                 
         DROP  R1                                                               
*                                                                               
CPYREC10 LA    R2,IOKEY                                                         
         MVC   RESKEY,APRECKEY         Set IOKEY with new key                   
         L     R2,AIOAREA1                                                      
         MVC   RESKEY,APRECKEY         New key to add                           
         GOTO1 ADDID,APPARM,(R2)                                                
         GOTO1 AIO,IOADD+IOACCFIL+IO1                                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SAVEKEY       See if this record is there                  
         MVI   RESKSEQ,RESKS5TH    New condition code record                    
         GOTO1 AIO,IOACCDIR+IORDD                                               
         BNE   CPYREC20            No so finished with this one                 
         GOTO1 AIO,IOACCMST+IOGET+IO3                                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   RESKEY,APRECKEY     NEW KEY TO ADD                               
         MVI   RESKSEQ,RESKS5TH    NEW CONDITION CODE RECORD                    
         L     R2,AIOAREA3                                                      
         MVC   RESKEY,IOKEY        NEW KEY TO ADD                               
         GOTO1 AIO,IOADD+IO3+IOACCMST                                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CPYREC20 LA    R2,IOKEY                                                         
         MVC   IOKEY,SAVEKEY       See if this record is there                  
         MVI   RESKSEQ,RESKSTXT    Text record                                  
         GOTO1 AIO,IOACCDIR+IORDD                                               
         BE    CPYREC30            Finshed                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CPYREC90                                                         
*                                                                               
CPYREC30 GOTO1 AIO,IOACCMST+IOGET+IO3                                           
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LA    R2,IOKEY                                                         
         MVC   RESKEY,APRECKEY     NEW KEY TO ADD                               
         MVI   RESKSEQ,RESKSTXT    SET TO TEXT RECORD                           
         L     R2,AIOAREA3                                                      
         MVC   RESKEY,IOKEY        NEW KEY TO ADD                               
         GOTO1 AIO,IOADD+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
CPYREC90 XIT1                                                                   
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
*=====================================================================*         
*  RESTORE DELETED RECORD AND ASSOCITIATED RECORDS                    *         
*=====================================================================*         
         USING RESRECD,R2                                                       
         USING LISTD,R4                                                         
         USING LSMD,R6                                                          
RESTORE  NMOD1 0,*RESTORE                                                       
         L     RC,APALOCAL                                                      
         L     R2,AIOAREA1                                                      
         TM    RESRSTA,X'80'                                                    
         BO    RESREC10                                                         
         MVC   FVMSGNO,=AL2(59)          RECORD IS RESTORED ALREADY             
         B     RESREC99                                                         
*                                                                               
RESREC10 NI    RESRSTA,TURNOFF-X'80'     TURN OFF DELETE RECORD                 
         GOTO1 ADDID,APPARM,(R2)                                                
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKS5TH                                                 
         GOTO1 AIO,IORDD+IOACCFIL+IO3                                           
         BE    RESREC20            RECORD NOT DELETED?                          
         TM    IOERR,IOEDEL                                                     
         BZ    RESREC99            SOME ERROR?                                  
         L     R2,AIOAREA3                                                      
         NI    RESRSTA,TURNOFF-X'80'       UN-DELETED                           
         AH    R2,DATADISP                                                      
         CLI   0(R2),0             END OF RECORD                                
         BE    RESREC20            DON'T RESTORE, NO ELEMENTS ON RECORD         
         GOTO1 AIO,IOWRITE+IOACCFIL+IO3                                         
*                                                                               
RESREC20 LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKSTXT                                                 
         GOTO1 AIO,IORDD+IOACCFIL+IO3                                           
         BE    RESREC30            RECORD NOT DELETED?                          
         TM    IOERR,IOEDEL                                                     
         BO    RESREC25            RESTORE THIS RECORD                          
         MVC   FVMSGNO,=AL2(FVFOK) IGNORE ERROR, PROBABLY NOT FOUND             
         B     RESREC30                                                         
*                                                                               
RESREC25 L     R2,AIOAREA3                                                      
         NI    RESRSTA,TURNOFF-X'80'       UN-DELETED                           
         AH    R2,DATADISP                                                      
         CLI   0(R2),0             END OF RECORD                                
         BE    RESREC30            DON'T RESTORE, NO ELEMENTS ON RECORD         
         GOTO1 AIO,IOWRITE+IOACCFIL+IO3                                         
*                                                                               
RESREC30 DS    0H                                                               
         LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKSREG    RESET                                        
*=====================================================================*         
*  TAMPER WITH LIST LINE IF COMING FROM LIST                          *         
*=====================================================================*         
RESREC80 TM    TWAMODE,TWAMLSM                                                  
         BZ    RESREC99                                                         
         L     R2,AIOAREA1                                                      
         L     R6,ALSM                                                          
         LA    R4,LSMTWSV                                                       
         AH    R4,LSMTWDSP                                                      
         OI    LISTDATH+1,FVAHIGH                                               
         OI    LISTDATH+6,FVOXMT                                                
         MVC   LISTNME,SPACES                                                   
         GOTO1 GETNAME,APPARM,(R2),(L'LISTNME,LISTNME)                          
         GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         XC    APBYTE,APBYTE                                                    
         TM    INOPT1,INORDTE      WANT REQUEST DATE                            
         BZ    *+8                 NO                                           
         MVI   APBYTE,C'R'                                                      
         GOTO1 GETACT,APPARM,(APBYTE,(R2)),(L'LISTADTE,LISTADTE)                
*                                                                               
RESREC99 XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
         DROP  R2,R4,R6                                                         
         EJECT ,                                                                
*=====================================================================*         
* DELETE FORMAT RECORD AND ASSOCIATED RECORDS                         *         
*=====================================================================*         
         USING RESRECD,R2                                                       
         USING LISTD,R4                                                         
         USING LSMD,R6                                                          
DELETE   NMOD1 0,*DELETE*                                                       
         L     RC,APALOCAL                                                      
         L     R2,AIOAREA1                                                      
         TM    RESRSTA,X'80'                                                    
         BZ    DELREC10                                                         
         MVC   FVMSGNO,=AL2(77)    ALREADY DELETED                              
         B     DELREC99                                                         
*                                                                               
DELREC10 OI    RESRSTA,X'80'       TURN ON DELETE RECORD                        
         GOTO1 ADDID,APPARM,(R2)                                                
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   IOKEY,0(R2)                                                      
         LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKS5TH    NEW CONDITIONAL CODE RECORD                  
         GOTO1 AIO,IORD+IOACCFIL+IO3                                            
         BNE   DELREC20            RECORD NOT THERE OR DELETED                  
         L     R2,AIOAREA3                                                      
         OI    RESRSTA,X'80'       MARK DELETED WITH ELEMENTS INTACT            
         GOTO1 AIO,IOWRITE+IOACCFIL+IO3                                         
*                                                                               
DELREC20 LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKSTXT    TEXT RECORD                                  
         GOTO1 AIO,IORD+IOACCFIL+IO3                                            
         BNE   DELREC30            RECORD NOT THERE OR DELETED                  
         L     R2,AIOAREA3                                                      
         OI    RESRSTA,X'80'       MARK DELETED WITH ELEMENTS INTACT            
         GOTO1 AIO,IOWRITE+IOACCFIL+IO3                                         
*                                                                               
DELREC30 LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKSREG    RESET                                        
*=====================================================================*         
* TAMPER WITH LIST SELECT LINE TO REFECT STATUS OF RECORD                       
*=====================================================================*         
DELREC80 L     R2,AIOAREA1                                                      
         TM    TWAMODE,TWAMLSM                                                  
         BZ    DELREC99                                                         
         L     R6,ALSM                                                          
         LA    R4,LSMTWSV                                                       
         AH    R4,LSMTWDSP                                                      
         OI    LISTDATH+1,FVAHIGH                                               
         GOTO1 TEXTGET,APPARM,1600,(L'LISTNME,LISTNME),0                        
         GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         XC    APBYTE,APBYTE                                                    
         TM    INOPT1,INORDTE      WANT REQUEST DATE                            
         BZ    *+8                 NO                                           
         MVI   APBYTE,C'R'                                                      
         GOTO1 GETACT,APPARM,(APBYTE,(R2)),(L'LISTADTE,LISTADTE)                
*                                                                               
DELREC99 XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
         DROP  R2,R4,R6                                                         
         EJECT ,                                                                
*=====================================================================*         
*        SEE IF THE OLD KEYWORD MATCHES WHAT IS NOW ON SCREEN         *         
*=====================================================================*         
MATCHON  NMOD1 0,*MATCH**                                                       
         L     R3,0(,R1)           OLD KEYWORD                                  
         ZIC   RF,0(,R1)           LENGTH OF DATA TO SCAN                       
         L     R4,4(,R1)           NEW KEYWORD                                  
*                                                                               
MATCH10  CLC   0(1,R3),0(R4)       MATCH OLD/NEW KEYWORD                        
         BE    MATCH80                                                          
         CLC   0(1,R3),SCCOMMA     AT END OF OLD KEYWORD                        
         BE    *+8                 YES                                          
         CLI   0(R3),C' '          AT END OF OLD KEYWORD                        
         BH    MATCHNO             NO, SO NO MATCH                              
         CLC   0(1,R4),SCCOMMA     AT END OF NEW KEYWORD?                       
         BE    MATCHED                                                          
         CLI   0(R3),C' '          AT END OF OLD KEYWORD                        
         BNH   MATCHED                                                          
         B     MATCHNO             NO MATCH                                     
*                                                                               
MATCH80  CLC   0(1,R3),SCCOMMA     AT END OF KEYWORD?                           
         BE    MATCHED                                                          
         CLI   0(R3),C' '                                                       
         BNH   MATCHED             MATCHED                                      
         LA    R4,1(,R4)                                                        
         LA    R3,1(,R3)                                                        
         BCT   RF,MATCH10                                                       
                                                                                
         CLC   0(1,R4),SCCOMMA     IS NEW KEYWORD FINISHED ?                    
         BE    MATCHED             YES                                          
         CLI   0(R4),C' '                                                       
         BH    MATCHNO             NO                                           
*                                                                               
MATCHED  SR    RE,RE               SET TO INDICATE     MATCHED                  
MATCHNO  LTR   RE,RE               SET TO INDICATE NOT MATCHED                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE COLUMN CALCULATIONS                                       *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                     RECURSIVE SUBROUTINE                            *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        R0 = SET TO 0 FIRST TIME IN                                  *         
*        R2 = COLUMN ELEMENT                                          *         
*        P1 = CURRENT PLACE IN DATA                                   *         
*        P2 = LENGTH OF EXPRESSION                                    *         
*        RETURN                                                       *         
*             CC SET                                                  *         
*             R0 = LEVEL OF PARENTHESIS                               *         
*             R1 = POINT TO NEXT PLACE IN EXPRESSION                  *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
VALEXP   NMOD1 0,**VEXP**                                                       
         L     RC,APALOCAL                                                      
         L     R4,0(,R1)           DATA                                         
         L     R8,4(,R1)           LENGTH OF DATA                               
         L     R2,8(,R1)           RCLEL                                        
         AR    R8,R4               END OF EXPRESSION                            
         MVC   FVMSGNO,=AL2(ACEEXP)    INVALID EXPRESSION                       
         MVI   TYPEIND,0           SET TO ZERO                                  
         CR    R4,R8               PAST END OF EXPERSSION?                      
         BH    VALEXPX             YES                                          
         CLI   0(R4),C' '          END OF EXPERSSION?                           
         BE    VALEXPOK                                                         
*                                                                               
VALEXP05 NI    TYPEIND,TURNOFF-TYPECOL                                          
         MVI   NDECIMAL,4          CONTROL DECIMAL                              
*        MVI   NDECIMAL,0          CONTROL DECIMAL                              
         CLI   0(R4),C'+'          OPERAND SIGN (NOT A OPERATOR)                
         BE    VALEXP10            YES                                          
         CLI   0(R4),C'-'          OPERAND SIGN (NOT A OPERATOR)                
         BE    VALEXP10            NO                                           
         CLI   0(R4),C'C'          COLUMN NUMBER TYPE OPERAND?                  
         BE    VALEXP15            EVALUATE NUMBER                              
         B     VALEXP20            NO MUST BE A CONSTANT OR PARENTHESIS         
*                                                                               
VALEXP10 LA    R4,1(,R4)           BUMP PAST CHARACTER                          
         CLI   0(R4),C'C'          NOW IS IT A COL# TYPE EXPRESSION?            
         BNE   VALEXP30                                                         
*                                                                               
VALEXP15 OI    TYPEIND,TYPECOL                                                  
         MVI   NDECIMAL,C'N'       CONTROL DECIMAL                              
         LA    R4,1(,R4)           BUMP BAST "C"                                
         B     VALEXP30            EVALUATE NUMBER                              
*                                                                               
VALEXP20 CLI   0(R4),C'('                                                       
         BNE   VALEXP30                                                         
         LA    R4,1(,R4)                                                        
         AH    R0,=H'01'           BALANCE PARENTHISIS +1                       
         SR    R8,R4                                                            
         BNP   VALEXPX                                                          
*                                  ************************************         
*                                  *    ***** RECURSIVE CALL *****    *         
*                                  ************************************         
         GOTO1 =A(VALEXP),APPARM,(R4),(R8),RR=APRELO                            
         BNE   VALEXPX             NOT OK, EXIT                                 
         LR    R4,R1               RESET R4 TO NEW LOCATION                     
         CLI   0(R4),C' '                                                       
         BE    VALEXPOK                                                         
         MVC   FVMSGNO,=AL2(ACEOPTR)       INVALID OPERATOR                     
         LA    R6,OPTABLE                                                       
*                                                                               
VALEXP25 CLI   0(R6),EOT           END OF OPERATOR TABLE?                       
         BE    VALEXPX             NO GOOD MUST HAVE OPERATOR                   
         CLC   0(1,R4),0(R6)                                                    
         BE    VALEXP45                                                         
         LA    R6,1(,R6)           BUMP UP TO NEXT OPERATOR                     
         B     VALEXP25                                                         
*                                                                               
VALEXP30 LR    R5,R4               POINT TO START OF EXPRESSION                 
*                                                                               
VALEXP32 LA    R6,OPTABLE          TABLE OF OPERATORS                           
*                                                                               
VALEXP33 CLI   0(R6),EOT                                                        
         BE    VALEXP35                                                         
         CLC   0(1,R5),0(R6)       MATCH OPERATOR                               
         BE    VALEXP40                                                         
         LA    R6,1(,R6)           BUMP TO NEXT OPERATOR                        
         B     VALEXP33                                                         
*                                                                               
VALEXP35 LA    R5,1(,R5)           BUMP TO NEXT VALULE IN EXPRESSION            
         CLI   0(R5),C' '          END OF EXPRESSION?                           
         BNE   VALEXP32            LOOP TO CHECK NEXT CHARATER                  
         B     VALEXP42                                                         
*                                                                               
VALEXP40 CLI   0(R6),C'%'             IS IT A PERCENT?                          
         BNE   VALEXP42                                                         
         OI    RCLOPT,RCLPCT          TURN ON % FLAG                            
*                                                                               
VALEXP42 SR    R5,R4                  GET LENGTH SCANNED                        
         MVC   FVMSGNO,=AL2(ACEOPND)  INVALID OPERAND                           
         BNP   VALEXPX                NO NUMBER, NO GOOD                        
         GOTO1 VCASHVAL,APPARM,(NDECIMAL,(R4)),(R5)                             
         MVC   FVMSGNO,=AL2(ACECNST)  INVALID CONSTANT, 2 DECIMAL               
         TM    TYPEIND,TYPECOL                                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(ACEIVCN)  INVALID COLUMN NUMBER                     
         CLI   APPARM,0                                                         
         BNE   VALEXPX                NOT A  VALID NUMBER                       
         TM    TYPEIND,TYPECOL                                                  
         BZ    VALEXP44                                                         
         SR    RE,RE                                                            
         ICM   RE,1,APPARM+7       GET COLUMN NUMBER                            
         BZ    VALEXPX             C0 NOT A VALID NUMBER                        
         LA    RF,COLARRY2(RE)                                                  
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         TM    0(RF),X'80'         IS IT A VERTICAL % COLUMN?                   
         BO    VALEXPX             CAN'T BE CALC COL AND NON CALC COL           
         MVC   0(1,RF),COL#        SAVE OFF COLUMN NUMBER                       
*                                                                               
VALEXP44 AR    R4,R5               POINT TO END OF EXPRESSION                   
*                                                                               
VALEXP45 CLI   0(R4),C')'                                                       
         BNE   VALEXP50                                                         
         LA    R4,1(,R4)                                                        
         BCTR  R0,0                BALANCE PARENTHESES -1                       
         B     VALEXPOK                                                         
*                                                                               
VALEXP50 CLI   0(R4),C'V'          VERTICAL PERCENT?                            
         BNE   VALEXP52                                                         
         CLI   2(R4),C'0'          VERT PERCENT MUST BE VC# OR VR#              
         BL    VALEXP52             NOT A NUMBER SO NOT A VERT %                
         MVC   FVMSGNO,=AL2(1441)                                               
         CLC   COL#,RANKCOL                                                     
         BE    VALEXPX                                                          
         B     VALEXP55                                                         
*                                                                               
VALEXP52 CLI   0(R4),C' '          END OF EXPRESSION?                           
         BE    VALEXPOK                                                         
         LA    R4,1(,R4)           BUMP TO NEXT OPERAND                         
         B     VALEXP05            LOOP TO CHECK NEXT                           
*                                                                               
VALEXP55 MVC   FVMSGNO,=AL2(ACEVERT)                                            
         TM    TYPEIND,TYPEVRT     VERTICAL %                                   
         BO    VALEXPX             ALREADY HAVE VERTICAL %                      
         OI    TYPEIND,TYPEVRT+TYPECOL     SET ON                               
         MVI   NDECIMAL,C'N'       CONTROL DECIMAL                              
         MVC   FVMSGNO,=AL2(ACE1CRC)                                            
         CLI   1(R4),C'C'          COLUMN NUMBER?                               
         BE    VALEXP60                                                         
         CLI   1(R4),C'R'          ROW NUMBER?                                  
         BNE   VALEXPX             WAS NIETHER SO NO GOOD                       
         OI    TYPEIND,TYPEROW                                                  
         NI    TYPEIND,TURNOFF-TYPECOL                                          
*                                                                               
VALEXP60 LA    R4,2(,R4)           BUMP TO LEVEL NUMBER                         
         LR    R5,R4                                                            
*                                                                               
VALEXP62 LA    R6,OPTABLE                                                       
*                                                                               
VALEXP64 CLI   0(R6),EOT                                                        
         BE    VALEXP68                                                         
         CLI   0(R5),C'C'          COLUMN NUMBER                                
         BE    VALEXP70                                                         
*                                                                               
VALEXP65 MVC   FVMSGNO,=AL2(ACEVERT)                                            
         CLI   0(R5),C'V'          COLUMN NUMBER                                
         BNE   VALEXP66            NOT ANOTHER VERTICAL %                       
         CLI   2(R5),C'0'          VERT PERCENT MUST BE VC# OR VR#              
         BNL   VALEXPX              NOT A NUMBER SO NOT A VERT %                
VALEXP66 CLC   0(1,R5),0(R6)       MATCH OPERATOR                               
         BE    VALEXP70                                                         
         LA    R6,1(,R6)                                                        
         B     VALEXP64            LOOP TO CHECK NEXT OPERATOR                  
*                                                                               
VALEXP68 LA    R5,1(,R5)           BUMP TO NEXT CHAR IN EXPRESSION              
         CLI   0(R5),C' '          END OF EXPRESSION?                           
         BNE   VALEXP62                                                         
         B     VALEXP71                                                         
*                                                                               
VALEXP70 CLI   0(R6),C'%'          IS IT A PERCENT?                             
         BNE   VALEXP71            NO                                           
         OI    RCLOPT,RCLPCT       TURN ON % FLAG                               
*                                                                               
VALEXP71 SR    R5,R4                                                            
         MVC   FVMSGNO,=AL2(ACEOPND)                                            
         BNP   VALEXPX             NOTHING HERE TO BE A NUMBER                  
         GOTO1 VCASHVAL,APPARM,(NDECIMAL,(R4)),(R5)                             
         MVC   FVMSGNO,=AL2(ACEIVRN)                                            
         TM    TYPEIND,TYPECOL                                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(ACEIVCN)  INVALID COLUMN NUMBER                     
         CLI   APPARM,0                                                         
         BNE   VALEXPX                NOT A VALID NUMBER                        
         MVC   FVMSGNO,=AL2(ACEIVRN)  INVALID ROW NUMBER                        
         TM    TYPEIND,TYPEROW                                                  
         BZ    VALEXP72                                                         
         CLC   NROWS,APPARM+7                                                   
         BL    VALEXPX             NOT A VALID NUMBER                           
*                                                                               
VALEXP72 TM    TYPEIND,TYPECOL                                                  
         BZ    VALEXP73                                                         
         SR    RE,RE                                                            
         ICM   RE,1,APPARM+7       GET COLUMN NUMBER                            
         BZ    VALEXPX             C0 NOT A VALID NUMBER                        
         LA    RF,COLARRY2(RE)                                                  
         MVC   FVMSGNO,=AL2(ACEIVCN)  INVALID COLUMN NUMBER                     
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         TM    0(RF),X'80'         IS IT ALREADY MARKED AS VERTICAL%            
         BZ    VALEXPX             NO, MUST BE MARKED AS AMOUNT COLUMN          
         MVC   0(1,RF),COL#                                                     
         OI    0(RF),X'80'         MARK AS VERTICAL %                           
*                                                                               
VALEXP73 NI    TYPEIND,TURNOFF-TYPEROW-TYPECOL                                  
         AR    R4,R5                                                            
         CLI   0(R4),C'C'          COLUMN NUMBER?                               
         BE    VALEXP05            LOOP                                         
         CLI   0(R4),C' '          END OF EXPRESSION?                           
         BNE   VALEXP30            LOOP                                         
*                                                                               
VALEXPOK MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALEXPX  CLC   FVMSGNO,=AL2(FVFOK)                                              
         LR    R1,R4                                                            
         XIT1  REGS=(R0,R1)                                                     
         DROP  R2                                                               
*                                                                               
OPTABLE  DC    C'+-*X/%V)',AL1(EOT)                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO GET REPORT TYPE CODE                                     *         
*                                                                     *         
* NTRY - R1=A(REPTYPE TABLE)                                          *         
* EXIT - FRMRPCDE CONTAINS EXPANDED REPORT CODE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REPTABD,R1                                                       
EXPRPTY  NMOD1 0,**XPRC**                                                       
         L     RC,APALOCAL                                                      
         MVC   FRMRPCDE,REPCODE                                                 
         MVC   FRMRPTYP,REPSTYPE                                                
         CLI   FRMRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNL   EXPRPTYX                                                         
         DROP  R1                                                               
         GOTO1 VDICTAT,APPARM,C'TU  ',('FRMRPLNQ',FRMRPCDE),0                   
*                                                                               
EXPRPTYX XMOD1 ,                   RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE USER FIELD ENTRY                                          *         
***********************************************************************         
         SPACE 1                                                                
VALRUF   NMOD1 0,**VLUF**                                                       
         L     RC,APALOCAL                                                      
         SR    RF,RF                                                            
         IC    RF,NPARMS                                                        
*                                                                               
VALRUF10 CLC   12(2,R6),SPACES     USER FIELD OF SPACES ?                       
         BE    VALRUF15            YES, ERROR                                   
         CLI   0(R6),2             LENGTH OF PARAMETER > 2 ?                    
         BNH   VALRUF20            NO,  OKAY                                    
*                                                                               
VALRUF15 MVC   FVMSGNO,=AL2(ACEIVUF)    INVALID USER FIELD                      
         MVC   FVXTRA,12(R6)                                                    
         B     VALRUF90                                                         
*                                                                               
VALRUF20 LA    R6,32(,R6)          BUMP TO NEXT PARAMETER                       
         BCT   RF,VALRUF10                                                      
*                                                                               
VALRUF90 XMOD1 ,                   RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE DATE COLUMN CALCULATION                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R1                                                       
DTECOMP  NMOD1 0,**DTEC**                                                       
         L     RC,APALOCAL                                                      
         MVC   FVXTRA,SPACES                                                    
         MVI   NPARMS,1            NUMBER OF PARAMETERS                         
         LA    R6,FVIFLD                                                        
*                                                                               
DTECMP05 CLI   0(R6),C' '          END OF DATA                                  
         BE    DTECMP99            NOT A DATE COMP                              
         CLI   0(R6),C'-'          MINUS TYPE OPERATOR ONLY                     
         BE    DTECMP06                                                         
         LA    R6,1(,R6)           BUMP UP BY ONE AND TRY AGAIN                 
         B     DTECMP05                                                         
*                                                                               
DTECMP06 LA    R6,FVIFLD                                                        
*                                                                               
DTECMP08 CLI   0(R6),C' '          END OF DATA                                  
         BNE   DTECMP09                                                         
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         CLI   NPARMS,2                                                         
         BH    DTECMP99            ERROR                                        
         MVC   FVMSGNO,=AL2(ACE2FEW)                                            
         BL    DTECMP99            ERROR                                        
         MVI   NPARMS,X'FF'        SIGNAL FINISHED                              
         B     DTECMP12                                                         
*                                                                               
DTECMP09 CLI   0(R6),C'-'          MINUS TYPE OPERATOR ONLY                     
         BE    DTECMP10                                                         
         LA    R6,1(,R6)           BUMP UP BY ONE AND TRY AGAIN                 
         B     DTECMP08                                                         
*                                                                               
DTECMP10 LA    R3,FVIFLD                                                        
*                                                                               
DTECMP12 LR    R2,R6               SAVE LOCATION OF MINUS                       
         SR    R2,R3               LENGTH OF DATA                               
         STC   R2,DUMFLDH+5                                                     
         MVC   DUMFLD,SPACES                                                    
         MVC   FVMSGNO,=AL2(ACE2FEW)                                            
         SH    R2,=H'01'                                                        
         BM    DTECMP99            ERROR NO DATA                                
         EXMVC R2,DUMFLD,0(R3)                                                  
         GOTO1 VALDEF,DUMFLDH                                                   
         MVC   FVMSGNO,=AL2(FVFKYWD)                                            
         BNE   DTECMP85            ERROR NOT A VALID KEYWORD                    
         TM    DEFTYPE,DEFDTE1     DATE TYPE?                                   
         BZ    DTECMP99            NO SO ERROR                                  
         LA    R6,1(,R6)                                                        
         LR    R3,R6                                                            
         CLI   NPARMS,X'FF'                                                     
         BE    DTECMP90            OK                                           
         SR    R1,R1                                                            
         IC    R1,NPARMS           NUMBER OF PARAMETERS SO FAR                  
         LA    R1,1(,R1)           INCREASE BY ONE                              
         STC   R1,NPARMS           SAVE                                         
         B     DTECMP08                                                         
*                                                                               
DTECMP85 LTR   R1,R1               IS   THIS A KEYWORD ?                        
         BZ    DTECMP99            NO,  EXIT                                    
         MVC   FVMSGNO,=AL2(ACEKYWDI)                                           
         B     DTECMP99            KEYWORD NOT VALID                            
*                                                                               
DTECMP90 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DTECMP99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         XMOD1 ,                   RETURN                                       
         DROP  R1                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CHARACTER SET USED FOR AN EXPRESSION                      *         
***********************************************************************         
         SPACE 1                                                                
CHKEXP   NMOD1 0,**CEXP**                                                       
         L     RC,APALOCAL                                                      
         LA    R1,L'COLDATA        LENGTH TO SQUISH                             
         LA    R2,FVIFLD                                                        
         LA    R3,APWORK                                                        
         MVC   APWORK,SPACES                                                    
         MVC   FVMSGNO,=AL2(FVFKYWD)                                            
         MVC   FVXTRA,SPACES                                                    
*                                                                               
CHKEXP10 CLI   0(R2),C' '          ELIMINATE BLANKS                             
         BE    CHKEXP40                                                         
         MVC   0(1,R3),0(R2)       MOVE CHARACTER IN                            
         LA    R3,1(,R3)           MOVE UP BY ONE                               
*                                                                               
         LA    R6,CHARSET                                                       
CHKEXP20 CLI   0(R6),EOT                                                        
         BE    CHKEXP40            BAD EXPRESSION                               
         CLC   0(1,R2),0(R6)       LOOK FOR ANY OPERATOR                        
         BNE   CHKEXP30            POSSIBLE EXPRESSION                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
CHKEXP30 LA    R6,1(,R6)           BUMP UP BY ONE                               
         B     CHKEXP20                                                         
*                                                                               
CHKEXP40 LA    R2,1(,R2)                                                        
         BCT   R1,CHKEXP10                                                      
         B     CHKEXP90                                                         
*                                                                               
CHKEXP90 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    CHKEXPEX                                                         
         MVC   FVXTRA(L'COLDATA),APWORK                                         
         B     CHKEXPNE                                                         
*                                                                               
CHKEXPEX LA    RE,APWORK                                                        
         SR    R3,RE                                                            
         STC   R3,FVILEN           SAVE NEW LENGTH                              
         BCTR  R3,0                                                             
         STC   R3,FVXLEN                                                        
         MVC   FVIFLD,SPACES                                                    
         EXMVC R3,FVIFLD,APWORK                                                 
         SR    RE,RE               SET CC TO OK                                 
*                                                                               
CHKEXPNE XMOD1 ,                   RETURN                                       
*                                                                               
CHARSET  DC    C'-+/*XV%0123456789()',AL1(EOT)                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE TRANSACTION TYPE INPUT                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
VALTYPE  NMOD1 0,**VTYP**                                                       
         L     RC,APALOCAL                                                      
         MVC   ELEMSAVE,APELEM                                                  
         MVI   CURTYP#,RFLTTYPE                                                 
         GOTO1 =A(DELRFL),APPARM,RR=APRELO                                      
         BNE   VALTYP99                                                         
*                                                                               
         CLI   FVILEN,0            NOTHING INPUTTED?                            
         BE    VALTYP90            OK FINISHED                                  
         LA    R2,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RFLEL,RFLELQ        HAVE TO PUT IN ELEMENT INFO                  
         MVC   RFLSEQ,COL#                                                      
         MVI   RFLTYPE,RFLTTYPE                                                 
         CLI   FVIFLD,C'*'         EXCLUSION?                                   
         BNE   *+8                                                              
         OI    RFLIND,RFLXCLD                                                   
*                                                                               
         ZIC   R1,NPARMS                                                        
         LA    R1,RFLLNQ(,R1)                                                   
         STC   R1,RFLLN                                                         
*                                                                               
         GOTO1 CNVTTYPE,APPARM,(C'N',BLOCK),(NPARMS,RFLDATA)                    
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   VALTYP99                                                         
*                                                                               
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALTYP99                                                         
         MVI   CURTYP#,RFLBUDGT                                                 
         GOTO1 =A(DELRFL),APPARM,RR=APRELO                                      
*                                                                               
VALTYP90 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALTYP99 MVC   APELEM,ELEMSAVE                                                  
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALTYPEX                                                         
         MVC   FVXTRA(12),12(R6)                                                
*                                                                               
VALTYPEX XMOD1 ,                   RETURN                                       
         DROP  R2                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BUDGET TYPE INPUT                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
         USING BUDRECD,R4                                                       
VALBUD   NMOD1 0,**VBUD**                                                       
         L     RC,APALOCAL                                                      
         MVC   ELEMSAVE,APELEM                                                  
         MVI   CURTYP#,RFLBUDGT                                                 
         GOTO1 =A(DELRFL),APPARM,RR=APRELO                                      
         BNE   VALBUD99                                                         
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    VALBUD90            NO, LEAVE                                    
         MVC   FVMSGNO,=AL2(ACEBUD)                                             
         MVC   SVKEY,IOKEY                                                      
         LA    R2,APELEM                                                        
         XC    APELEM,APELEM                                                    
         LA    R4,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN                                                   
*                                                                               
         TM    FVIIND,FVINUM       IS IT NUMERIC                                
         BZ    VALBUD08                                                         
         CLI   FVILEN,LBUDG#       IS   LENGTH > 5                              
         BH    VALBUD99            YES, INVALID BUDGET                          
         ZIC   RF,FVXLEN           GET  LENGTH - 1                              
         EX    RF,VALBUDPK         PACK INTO APDUB                              
         CVB   RF,APDUB            CONVERT TO BINARY                            
         C     RF,=A(32767)        >    MAX HALFWORD NUMBER ?                   
         BH    VALBUD99            YES, INVALID BUDGET                          
         STH   RF,APHALF           SAVE BUDGET NUMBER                           
         MVC   BUDKNO1,APHALF      INSERT INTO RECORD                           
         GOTO1 AIO,IO2+IOACCFIL+IOHI                                            
         BL    VALBUD99            HARDWARE ERROR, EXIT                         
         CLC   BUDKNO1,APHALF      BUDKNO CHANGED ?                             
         BNE   VALBUD99            YES, INVALID BUDGET                          
         L     R4,AIOAREA2         ->   RECORD AREA                             
         SR    RF,RF                                                            
         IC    RF,0(,R6)           GET  LENGTH OF SCREEN FIELD                  
         SHI   RF,9                                                             
         TM    1(R6),FVAXTND                                                    
         BZ    *+8                                                              
         SHI   RF,8                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),BUDKCOD     MOVE IN NAME                                 
         OI    6(R6),FVOXMT        TRANSMIT NAME                                
         B     VALBUD10                                                         
*                                                                               
VALBUD08 MVC   BUDKCOD,FVIFLD                                                   
         GOTO1 AIO,IO2+IOACCFIL+IOHIGH                                          
         BL    VALBUD99            HARDWARE ERROR, EXIT                         
         L     R4,AIOAREA2                                                      
         CLC   BUDKEY(BUDKNO2-BUDKEY),IOKEY                                     
         BNE   VALBUD99                                                         
         MVC   APHALF,BUDKNO2      SAVE BUDGET NUMBER                           
*                                                                               
         USING BIVELD,R1                                                        
VALBUD10 LR    R1,R4                                                            
         MVI   APELCODE,BIVELQ                                                  
         GOTO1 GETEL                                                            
         MVC   FVMSGNO,=AL2(ACEBUDA)                                            
         BNE   VALBUD99                                                         
*                                                                               
VALBUD15 CLC   BIVAUNT(2),APREPUL                                               
         BE    VALBUD20                                                         
         GOTO1 NEXTEL                                                           
         BE    VALBUD15                                                         
         MVC   FVMSGNO,=AL2(ACEBUDA)                                            
         B     VALBUD99                                                         
*                                                                               
VALBUD20 MVC   IOKEY,SVKEY                                                      
         MVI   RFLEL,RFLELQ        X'C5' ELEMENT                                
         MVI   RFLLN,RFLLNQ+2      LENGTH                                       
         MVC   RFLSEQ,COL#         COLUMN #                                     
         MVI   RFLTYPE,RFLBUDGT    BUDGET TYPE                                  
         MVC   RFLDATA(L'BUDKNO2),APHALF          SAVE BUDGET NUMBER            
*                                                                               
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALBUD99                                                         
         MVI   CURTYP#,RFLTTYPE    DELETE TYPE BECAUSE OF BUDGET                
         GOTO1 =A(DELRFL),APPARM,RR=APRELO                                      
*                                                                               
VALBUD90 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALBUD99 MVC   APELEM,ELEMSAVE                                                  
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         XMOD1 ,                   RETURN                                       
*                                                                               
VALBUDPK PACK  APDUB,FVIFLD(0)                                                  
*                                                                               
LBUDG#   EQU   5                   MAXIMUM LENGTH FOR NUMERIC BUDGET #          
         DROP  R2,R4                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE FOREIGN CURRENCY TYPE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
VALCURR  NMOD1 0,**VCUR**                                                       
         L     RC,APALOCAL                                                      
         MVC   ELEMSAVE,APELEM                                                  
         MVI   CURTYP#,RFLFCUR                                                  
         GOTO1 =A(DELRFL),APPARM,RR=APRELO                                      
         L     R1,AIOAREA1                                                      
         BNE   VALCURR9            ERROR IF BRANCH TAKEN                        
*                                                                               
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         CLI   NPARMS,1                                                         
         BL    VALCURR8            NOTHING TO BUILD                             
         BH    VALCURR9                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   BLOCK,3                                                          
         BH    VALCURR9                                                         
         GOTO1 VALFCUR,BLOCK+12                                                 
         BNE   VALCURR9                                                         
         LA    R1,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RFLEL,RFLELQ        X'C5' FILTER TYPE ELEMENT                    
         MVI   RFLLN,RFLLNQ+3                                                   
         MVC   RFLSEQ,COL#         SAVE SEQUENCE NUMBER                         
         MVI   RFLTYPE,RFLFCUR     MARK AS FOREIGN CURR. FILTER                 
         MVC   RFLDATA(3),BLOCK+12                                              
         OC    RFLDATA(3),SPACES   MAKE SURE THERE ARE 3 BYTES OF DATA          
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALCURR9                                                         
*                                                                               
VALCURR8 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALCURR9 MVC   APELEM,ELEMSAVE     RESTORE ELEMENT                              
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         XMOD1 ,                   RETURN                                       
         DROP  R1                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALRANGE DATE RANGE                                                *         
*---------------------------------------------------------------------*         
*        DAY       RCLDAY                 ** DAY1,DAY2                *         
*        MON       RCLMON                 ** + OR - A MONTH           *         
*        M1-M12    RCLMON+RCLNTOLL           + OR - A YEAR(S)         *         
*        PER       ZERO                      NONE                     *         
*        QTR       RCLQTR                    + OR - A QUARTER         *         
*        QTD       RCLQTR+RCLTODTE           + OR - A QUARTER         *         
*        Q1-Q4     RCLQTR+RCLNROLL           + OR - A YEAR(S)         *         
*        YR,YEAR   RCLYEAR                   + OR - A YEAR(S)         *         
*        YTD       RCLYEAR+RCLTODTE          + OR - A YEAR(S)         *         
*        PED       RCLPERD                   + OR - # PERIODS         *         
*        P1-P99    RCLPERD+RCLROLL         * PERIOD# IN YEAR          *         
*        PMON      RCLPERD+RCLMON          * PERIOD#, +/- MONTH       *         
*        PM1-PM12  RCLPERD+RCLMON+RCLNROLL * PERIOD#, +/- MONTH       *         
*                                                                     *         
*  (*)  - FOR LAST YEAR, + FOR NEXT YEAR, NOTHING FOR CURRENT YEAR    *         
*  (**) - "AFTER" OR "PRIOR" ALSO VALID                               *         
***********************************************************************         
         EJECT 1                                                                
         USING RCLELD,R8                                                        
VALRANGE NMOD1 0,**VRNG**                                                       
         L     RC,APALOCAL                                                      
         MVI   PARM_N,0                                                         
         LA    R4,RCLSTDT                                                       
         CLI   FVILEN,0                                                         
         BNE   VALRNG05                                                         
         MVC   FVMSGNO,=AL2(ACEPRM)                                             
         TM    RCLOPT,RCLACCM                                                   
         BZ    VALRNG90            IT DOESN'T NEED ANY DATA                     
         MVC   FVMSGNO,=AL2(ACE2FEW)                                            
         TM    RCLDTEFG,RCLPERD                                                 
         BZ    *+8                                                              
         TM    RCLDTEFG,RCLMON                                                  
         BO    VALRNG99                                                         
         TM    RCLDTEFG,RCLDAY     NO INPUT                                     
         BO    VALRNG99            DAY MUST HAVE TWO PARMS                      
         TM    RCLDTEFG,RCLNROLL                                                
         BZ    VALRNG90            NO INPUT OK EXCEPT FOR DAY RANGE             
         LA    R4,RCLENDT                                                       
         B     VALRNG50            ENTER NUMBER OF MON OR QTR                   
*                                                                               
VALRNG05 TM    RCLOPT,RCLACCM                                                   
         BZ    VALRNG90                     NO INPUT NEEDED                     
         MVC   FVMSGNO,=AL2(FVFNOTV)        NOT VALID INPUT                     
         TM    RCLDTEFG,TURNOFF-RCLTODTE    PERIOD HAS NO INPUT                 
         BZ    VALRNG99                                                         
         TM    RCLDTEFG,RCLDAY     ARE WE GETTING AMOUNT BY DAY?                
         BZ    VALRNG06            NO                                           
         MVC   ELEMSAVE,APELEM                                                  
         MVI   CURTYP#,RFLBUDGT                                                 
         MVC   NPARMS,APPARM+4                                                  
         GOTO1 =A(DELRFL),APPARM,RR=APRELO                                      
         MVC   APELEM,ELEMSAVE                                                  
*                                                                               
VALRNG06 LA    R6,BLOCK                                                         
         MVC   NPARMS,APPARM+4                                                  
         CLI   NPARMS,0            NO PARAMETERS (NO INPUT)                     
         BE    VALRNG90                                                         
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         CLI   NPARMS,2                                                         
         BH    VALRNG99                                                         
         BL    VALRNG08                                                         
         TM    RCLDTEFG,RCLMON     PM1-PM12 OR PMON?                            
         BZ    VALRNG07                                                         
         TM    RCLDTEFG,RCLPERD                                                 
         BNO   VALRNG07                                                         
         TM    RCLDTEFG,RCLNROLL   PM1-PM12 IN ON                               
         BZ    VALRNG08            PMON CAN HAVE 2 PARMS                        
         B     VALRNG99            PM1-PM12 HAS 1 PARM                          
*                                                                               
VALRNG07 TM    RCLDTEFG,RCLNROLL                                                
         BO    *+12                                                             
         TM    RCLDTEFG,RCLPERD                                                 
         BO    VALRNG08                   THIS IS PED, PERIOD RANGE             
         TM    RCLDTEFG,RCLDAY+RCLMON     DAY & MON CAN HAVE 2 PARMS            
         BZ    VALRNG99                                                         
         TM    RCLDTEFG,RCLNROLL+RCLMON   IF MON THEN CAN'T BE M1-M12           
         BO    VALRNG99                                                         
*                                                                               
VALRNG08 MVI   PARM_N,1                                                         
         MVI   ONEXONLY,NO                                                      
*                                                                               
VALRNG10 CLC   PARM_N,NPARMS                                                    
         BH    VALRNG60            FINISHED                                     
         CLI   PARM_N,2            2ND TIME AROUND?                             
         BE    VALRNG12                                                         
         TM    RCLDTEFG,RCLMON+RCLPERD                                          
         BO    VALRNG20                                                         
*                                                                               
VALRNG12 TM    2(R6),X'80'         IS IT A POSITIVE NUMBER?                     
         BZ    VALRNG20            NO                                           
         MVC   0(2,R4),6(R6)       MOVE IN HALF WORD BINARY FIELD               
         LA    R4,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG20 MVC   FVMSGNO,=AL2(ACEPRM)                                             
         SR    R1,R1                                                            
         IC    R1,0(,R6)           LENGTH OF PARAMETER                          
         BCTR  R1,0                                                             
         TM    RCLDTEFG,RCLPERD                                                 
         BO    VALRNG40                                                         
         TM    RCLDTEFG,RCLYEAR+RCLQTR+RCLNROLL                                 
         BNZ   VALRNG40                                                         
*        BO    VALRNG40                                                         
         EXCLC R1,12(R6),AC@PRIOR                                               
         BNE   VALRNG30                                                         
         CLI   ONEXONLY,YES                                                     
         BE    VALRNG99                                                         
         CLI   NPARMS,1                                                         
         BE    VALRNG99                                                         
         MVI   ONEXONLY,YES                                                     
         CLI   PARM_N,2            2ND TIME IN                                  
         BNE   *+10                                                             
         MVC   RCLENDT,RCLSTDT     SWAP VALUE FROM START TO END                 
         MVC   RCLSTDT,=XL2'8000'                                               
         LA    R4,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG30 MVC   FVMSGNO,=AL2(ACEPRM)                                             
         EXCLC R1,12(R6),AC@AFTER                                               
         BNE   VALRNG40                                                         
         CLI   ONEXONLY,YES                                                     
         BE    VALRNG99                                                         
         MVI   ONEXONLY,YES                                                     
         CLI   NPARMS,1                                                         
         BE    VALRNG99                                                         
         MVC   RCLENDT,=XL2'8000'                                               
         LA    R4,RCLSTDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG40 LA    R1,1(,R1)                                                        
         ST    R1,APPARM+4         SET UP 2ND PARAMETER (LENGTH)                
         LA    R1,12(,R6)          SET UP 1ST PARAMETER (FIELD FOR VAL)         
         ST    R1,APPARM                                                        
         MVI   APPARM,C'N'         SET TO NO DEC                                
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         GOTO1 VCASHVAL,APPARM                                                  
         CLI   APPARM,0                                                         
         BNE   VALRNG99                                                         
         L     RF,APPARM+4         BINARY VALUE                                 
         LPR   RF,RF                                                            
         TM    RCLDTEFG,RCLPERD                                                 
         BNO   VALRNG48                                                         
         TM    RCLDTEFG,RCLMON                                                  
         BO    VALRNG41                                                         
         TM    RCLDTEFG,RCLNROLL   IS IT P#                                     
         BO    VALRNG48                                                         
         MVC   FVMSGNO,=AL2(1758)  PERIOD RANGE -10 TO 10 ONLY                  
         C     RF,=F'10'                                                        
         BH    VALRNG99                                                         
         B     VALRNG48                                                         
*                                                                               
VALRNG41 CLI   PARM_N,2            2ND TIME AROUND?                             
         BE    VALRNG48                                                         
         MVC   FVMSGNO,=AL2(1758)  PERIOD # 1 THROUGH 12 ONLY                   
         C     RF,=F'10'                                                        
         BH    VALRNG99                                                         
         MVC   FVMSGNO,=AL2(1759)  ZERO PERIOD NUMBER NOT VALID                 
         LTR   RF,RF                                                            
         BZ    VALRNG99                                                         
         STC   RF,RCLPDN1                                                       
         MVI   RCLPDYR,0           CURRENT YEAR                                 
         TM    APPARM+4,X'80'                                                   
         BZ    VALRNG42                                                         
         MVI   RCLPDYR,X'FF'       PREVIOUS YEAR                                
         LA    R4,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG42 CLI   12(R6),C'+'                                                      
         BNE   *+8                                                              
         MVI   RCLPDYR,01          FORWARD ONE YEAR                             
         LA    R4,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG48 MVC   0(2,R4),APPARM+6    MOVE IN BINARY FIELD                         
         LA    R4,RCLENDT                                                       
*                                                                               
VALRNG50 TM    RCLDTEFG,RCLPERD                                                 
         BNO   VALRNG52                                                         
         TM    RCLDTEFG,RCLMON+RCLNROLL                                         
         BNZ   VALRNG52                                                         
         CLI   PARM_N,1                                                         
         BH    VALRNG55                                                         
         BL    VALRNG90                                                         
         MVC   RCLENDT,RCLSTDT     DEFAULT TO SAME VALUE                        
         B     VALRNG55                                                         
*                                                                               
VALRNG52 TM    RCLDTEFG,RCLNROLL                                                
         BO    VALRNG90                                                         
*                                                                               
VALRNG55 TM    RCLDTEFG,RCLYEAR+RCLQTR                                          
         BNZ   VALRNG90                                                         
*        BO    VALRNG90                                                         
         LA    R6,32(,R6)          BUMP TO SECOND PARAMETER                     
         IC    R1,PARM_N                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,PARM_N                                                        
         B     VALRNG10                                                         
*                                                                               
VALRNG60 MVC   FVMSGNO,=AL2(ACEPRM)                                             
         CLI   ONEXONLY,YES                                                     
         BE    VALRNG90                                                         
         CLI   NPARMS,2                                                         
         BNE   VALRNG90                                                         
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BO    VALRNG90                                                         
         TM    RCLDTEFG,RCLMON     MONTH ONLY ALLOWED ONE NUMBER                
         BO    VALRNG99                                                         
         TM    RCLSTDT,X'80'       IS IT NEGATIVE                               
         BZ    VALRNG70            IT IS POSITIVE                               
         TM    RCLENDT,X'80'       IS IT NEGATIVE                               
         BZ    VALRNG90            IT IS POSITIVE                               
         B     VALRNG72                                                         
*                                                                               
VALRNG70 TM    RCLENDT,X'80'       IS IT NEGATIVE                               
         BO    VALRNG80            SWAP DATES                                   
*                                                                               
VALRNG72 CLC   RCLSTDT,RCLENDT                                                  
         BL    VALRNG90                                                         
*                                                                               
VALRNG80 XC    RCLSTDT,RCLENDT                                                  
         XC    RCLENDT,RCLSTDT                                                  
         XC    RCLSTDT,RCLENDT                                                  
*                                                                               
VALRNG90 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALRNG99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         XMOD1 ,                   RETURN                                       
         DROP  R8                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
**********************************************************************          
*  DELRFL - DELETE X'C5' COLUMN FILTER ELEMENT FOR TYPE/COLUMN       *          
**********************************************************************          
         SPACE 1                                                                
DELRFL   NMOD1 0,**DRFL**                                                       
         L     RC,APALOCAL                                                      
         L     R1,AIOAREA1                                                      
         AH    R1,DATADISP                                                      
*                                                                               
         USING RFLELD,R1                                                        
DELRFL05 CLI   RFLEL,0             END OF RECORD?                               
         BE    DELRFL30                                                         
*                                                                               
DELRFL10 CLI   RFLEL,RFLELQ        DELETE FILTER LIST X'C5' ELEM.               
         BNE   DELRFL20            LOOP TO NEXT ELEMENT                         
         CLC   RFLSEQ,COL#         MAKE SURE IT MATCHES COLUMN #                
         BNE   DELRFL20            LOOP TO NEXT ELEMENT                         
         CLC   RFLTYPE,CURTYP#     MATCH TYPE OF FILTER                         
         BNE   DELRFL20            LOOP TO NEXT ELEMENT                         
         MVI   0(R1),X'FF'         MARK FOR DELETION                            
*                                                                               
DELRFL20 SR    RF,RF                                                            
         IC    RF,RFLLN            GET LENGTH                                   
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     DELRFL05            LOOP                                         
*                                                                               
DELRFL30 MVI   APELCODE,X'FF'      DELETE X'FF' ELEMENTS                        
         L     R2,AIOAREA1                                                      
         GOTO1 DELEL,(R2)                                                       
         BNE   DELRFLEX                                                         
         CR    RE,RE                                                            
*                                                                               
DELRFLEX XMOD1 ,                   RETURN                                       
         DROP  R1                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
**********************************************************************          
*  SETLAST  - FIND LAST COLUMN SEQUENCE NUMBER                       *          
**********************************************************************          
         SPACE 1                                                                
         USING RCLELD,R1                                                        
SETLAST  NMOD1 0,**SETL**                                                       
         L     RC,APALOCAL                                                      
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     COLUMN ELEMENTS X'C3'                        
         MVI   LSTCOL#,0           INITIALIZE TO ZERO COLUMNS                   
         GOTO1 GETEL                                                            
*                                                                               
SETLAST3 BNE   SETLAST9                                                         
         MVC   LSTCOL#,RCLSEQ                                                   
         GOTO1 NEXTEL                                                           
         B     SETLAST3                                                         
*                                                                               
SETLAST9 XMOD1 ,                   RETURN                                       
         DROP  R1                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  INSERT COLUMN AND MESSAGE ELEMENTS AND ADJUST COLUMN CALCULATION   *         
*  TYPE COLUMNS                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R1                                                        
INSCOLMN NMOD1 0,**ICOL**                                                       
         L     RC,APALOCAL                                                      
         STC   R1,INSCOL#          COLUMN NUMBER INSERTING BETWEEN              
         SR    R2,R2                                                            
         IC    R2,INSSEQ                                                        
         GOTO1 FIXCALC,APPARM,(R2),1                                            
         GOTO1 =A(FIX5TH),APPARM,(R2),1,RR=APRELO                               
         GOTO1 =A(FIXPROF),APPARM,(R2),1,RR=APRELO                              
         BNE   INSCOLEX                                                         
         GOTO1 FIXMSGS,APPARM,(R2),1,NO                                         
*                                                                               
         CLC   INSCOL#,LSTCOL#                                                  
         BH    INSCOL90            NOTHING TO ADJUST                            
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     COLUMN ELEMENTS X'C3'                        
         MVC   ELEMSEQ,INSCOL#     START WITH THIS COLUMN #                     
         GOTO1 GETEL                                                            
         BNE   INSCOL90                                                         
         SR    R2,R2                                                            
*                                                                               
INSCOL10 IC    R2,RCLSEQ           BUMP UP COLUMN #S                            
         LA    R2,1(,R2)                                                        
         STC   R2,RCLSEQ           NEW NUMBER                                   
         GOTO1 NEXTEL                                                           
         BE    INSCOL10                                                         
*                                                                               
         USING RFLELD,R1                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RFLELQ     COLUMN FILTER ELEMENTS X'C5'                 
         GOTO1 GETEL                                                            
         BNE   INSCOL30                                                         
         SR    R2,R2                                                            
*                                                                               
INSCOL20 CLC   INSCOL#,RFLSEQ      SHOULD WE UPDATE THIS ELEMENT ?              
         BH    INSCOL25            NO,  NEXT ELEMENT                            
         IC    R2,RFLSEQ           BUMP UP COLUMN #S                            
         LA    R2,1(,R2)                                                        
         STC   R2,RFLSEQ           NEW  NUMBER                                  
*                                                                               
INSCOL25 GOTO1 NEXTEL              GET  NEXT ELEMENT                            
         BE    INSCOL20                                                         
*                                                                               
         USING XTRELD,R1                                                        
INSCOL30 L     R1,AIOAREA1                                                      
         MVI   APELCODE,XTRELQ     EXTRACT ELEMENTS X'C6'                       
         GOTO1 GETEL                                                            
         BNE   INSCOL60                                                         
         SR    R2,R2                                                            
*                                                                               
INSCOL42 CLI   XTRTYPE,XTRCTOT     EXTRACT TOTAL                                
         BH    INSCOL43                                                         
         BL    INSCOL58                                                         
         CLC   INSCOL#,XTRNUM      SHOULD WE UPDATE THIS VALUE ?                
         BH    INSCOL43                                                         
         IC    R2,XTRNUM           YES                                          
         AHI   R2,1                                                             
         STC   R2,XTRNUM                                                        
*                                                                               
INSCOL43 ZIC   RF,XTRSUB#          NUMBER OF SUB-ELEMENT                        
         LA    RE,XTRSUBEL         START OF SUB-ELEMENTS                        
*                                                                               
         USING XTRSUBEL,RE                                                      
INSCOL45 TM    XTRSIND,XTRSCOL     COLUMN TYPE SUB-ELEMENT ?                    
         BZ    INSCOL48            NEXT SUB-ELEMENT                             
         CLC   INSCOL#,XTRSNUM     SHOULD WE UPDATE THIS ELEMENT ?              
         BH    INSCOL48            NO, NEXT SUB-ELEMENT                         
         IC    R2,XTRSNUM                                                       
         AHI   R2,1                                                             
         STC   R2,XTRSNUM                                                       
*                                                                               
INSCOL48 IC    R2,XTRSUBLN                                                      
         AR    RE,R2                                                            
         BCT   RF,INSCOL45                                                      
         DROP  RE                                                               
*                                                                               
INSCOL58 GOTO1 NEXTEL              GET  NEXT ELEMENT                            
         BE    INSCOL42                                                         
*                                                                               
         USING MNOELD,R1           MESSAGE ELEMENTS                             
INSCOL60 L     R1,AIOAREA1                                                      
         MVI   APELCODE,MNOELQ     MESSAGE ELEMENTS X'C9'                       
         GOTO1 GETEL                                                            
         BNE   INSCOL80                                                         
         SR    R2,R2                                                            
*                                                                               
INSCOL65 CLC   INSCOL#,MNOSEQ      SHOULD WE UPDATE THIS ELEMENT ?              
         BH    INSCOL70            NO,  NEXT ELEMENT                            
         IC    R2,MNOSEQ           BUMP UP COLUMN #S                            
         LA    R2,1(,R2)                                                        
         STC   R2,MNOSEQ           NEW  NUMBER                                  
*                                                                               
INSCOL70 GOTO1 NEXTEL              GET  NEXT ELEMENT                            
         BE    INSCOL65                                                         
*                                                                               
INSCOL80 IC    RF,LSTCOL#          NUMBER  OF      COLUMNS  ON   RECORD         
         LA    RF,1(,RF)           ONE     MORE    COLUMN   LEFT                
         STC   RF,LSTCOL#          SAVE    NUMBER  OF       COLUMNS             
*                                                                               
INSCOL90 MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
*                                                                               
INSCOLEX CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION        CODE                
         XMOD1 ,                   RETURN                                       
         DROP  R1                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DELETE COLUMN AND MESSAGE ELEMENTS AND ADJUST COLUMN CALCULATION   *         
*  TYPE COLUMNS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DELCOLMN NMOD1 0,**DCOL**                                                       
         L     RC,APALOCAL                                                      
         STC   R1,DELCOL#          COLUMN  NUMBER  ABOUT TO DELETE              
         CLC   DELCOL#,LSTCOL#                                                  
         BH    DELCOL80            NOTHING LEFT    TO       ADJUST              
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     COLUMN  ELEMENTS         X'C3'               
         MVC   ELEMSEQ,DELCOL#     THIS    COLUMN  ONLY                         
         GOTO1 DELEL                                                            
         MVI   APELCODE,RFLELQ     FILTER  ELEMENTS         X'C5'               
         MVC   ELEMSEQ,DELCOL#     THIS    COLUMN  ONLY                         
         GOTO1 DELEL                                                            
         MVI   APELCODE,MNOELQ     MESSAGE ELEMENTS         X'C9'               
         MVC   ELEMSEQ,DELCOL#     THIS    COLUMN  ONLY                         
         GOTO1 GETEL                                                            
*                                                                               
         USING MNOELD,R1           MESSAGE ELEMENT                              
DELCOL10 BNE   DELCOL20            NOT     FOUND,  CONTINUE                     
         CLC   MNOSEQ,DELCOL#      FOUND   RIGHT   COLUMN   NUMBER ?            
         BNE   DELCOL20            NO,     CONTINUE                             
         CLI   MNOSTYPE,MNOSTCOL   COLUMN  TYPE    ELEMENT ?                    
         BNE   *+8                 NO,     TRY     AGAIN                        
         MVI   MNOEL,X'FF'         YES,    DELETE  IT                           
         GOTO1 NEXTEL              GET     NEXT    ELEMENT                      
         B     DELCOL10            CHECK   NEXT    ELEMENT                      
*                                                                               
DELCOL20 L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE  X'FF'   ELEMENTS                     
         GOTO1 DELEL                                                            
         OI    MSGELSW,MSGELDEL    TURN    ON      COLUMN   DELETED SW          
         DROP  R1                                                               
*                                                                               
         CLC   DELCOL#,LSTCOL#                                                  
         BH    DELCOL50            NOTHING LEFT    TO       ADJUST              
DELCOL40 SR    R3,R3                                                            
         IC    R3,DELCOL#          COLUMN  DELETED                              
         GOTO1 =A(FIXXTREL),APPARM,(R3),-1,RR=APRELO                            
         GOTO1 =A(FIX5TH),APPARM,(R3),-1,RR=APRELO                              
         GOTO1 FIXCALC,APPARM,(R3),-1                                           
         GOTO1 =A(FIXPROF),APPARM,(R3),-1,RR=APRELO                             
         BNE   DELCOL90                                                         
         GOTO1 FIXMSGS,APPARM,(R3),-1,YES                                       
*                                                                               
DELCOL50 DS    0H                                                               
         ZIC   RF,LSTCOL#          NUMBER  OF      COLUMNS  ON   RECORD         
         BCTR  RF,0                ONE     LESS    COLUMN   LEFT                
         STC   RF,LSTCOL#                                                       
*                                                                               
DELCOL80 MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
*                                                                               
DELCOL90 CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION        CODE                
         XMOD1 ,                   RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
*=====================================================================*         
*        SEE IF THERE IS A RESKS5TH RECORD TO ADJUST                  *         
*=====================================================================*         
                                                                                
         USING RESRECD,R2                                                       
FIX5TH   NMOD1 0,**5REC**                                                       
         L     RC,APALOCAL                                                      
         CLI   HAVE5TH,YES                                                      
         BNE   FIX5TH90            NO                                           
         L     R3,0(,R1)           X'C3'  ELEMENT TO START FROM                 
         STC   R3,APBYTE           COLUMN NUMBER WORKING ON                     
         L     R3,4(,R1)           +1 FOR INSERT -1 FOR DELETE & RENUM          
*                                  0  TO DELETE ONLY                            
         MVC   SVELEM1,APELEM      SAVE    APELEM                               
*                                                                               
         USING CONELD,R2                                                        
         SR    RF,RF                                                            
         L     R2,AIOAREA0                                                      
         AH    R2,DATADISP                                                      
FIX5TH10 CLI   0(R2),EOR                                                        
         BE    FIX5TH80                                                         
         CLI   0(R2),CONELQ        X'A1'  CONDITIONAL ELEMENT                   
         BNE   FIX5TH30            NEXT ELEMENT                                 
         CLI   REPMODE,REPROW      PROCESSING ROWS ?                            
         BNE   FIX5TH12                                                         
         CLI   CONTYPE,CONTROW     ROW TYPE ?                                   
         BE    FIX5TH14            YES                                          
         B     FIX5TH30            NO, GET NEXT ELEMENT                         
*                                                                               
FIX5TH12 CLI   REPMODE,REPCOL      PROCESSING COLUMNS ?                         
         BNE   FIX5TH30            NO, GET NEXT ELEMENT                         
         CLI   CONTYPE,CONTCOL     COLUMN TYPE ?                                
         BNE   FIX5TH30            NO, GET NEXT ELEMENT                         
*                                                                               
FIX5TH14 LTR   R3,R3               YES, DELETE ONLY OPTION ?                    
         BNZ   FIX5TH15            NO                                           
         CLC   APBYTE,CONNUM       YES. DELETE ONLY THIS ENTRY MATCH            
         BNE   FIX5TH30            NEXT ELEMENT                                 
         MVI   CONEL,X'FF'                                                      
         B     FIX5TH30            ARE THERE OTHERS TO DELETE                   
*                                                                               
FIX5TH15 CLC   APBYTE,CONNUM                                                    
         BH    FIX5TH30            NEXT ELEMENT                                 
         BL    FIX5TH20                                                         
         LTR   R3,R3               MINUS FOR DELETE                             
         BP    FIX5TH20                                                         
         MVI   CONEL,X'FF'         DELETE ELEMENT                               
         B     FIX5TH30            NEXT ELEMENT                                 
*                                                                               
FIX5TH20 IC    RF,CONNUM                                                        
         AR    RF,R3                                                            
         STC   RF,CONNUM           ADD OR SUBTRACT                              
*                                                                               
FIX5TH30 IC    RF,1(,R2)           NEXT ELEMENT                                 
         AR    R2,RF                                                            
         B     FIX5TH10                                                         
*                                                                               
FIX5TH80 L     R1,AIOAREA0         DELETE ANY THING MARKED                      
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL                                                            
*                                                                               
FIX5TH90 MVC   APELEM,SVELEM1      RESTORE APELEM                               
         XMOD1 ,                   RETURN                                       
         DROP  R2                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
*=====================================================================*         
*        Adjust XTRELD based on row or column instert or delete       *         
*=====================================================================*         
                                                                                
FIXXTREL NMOD1 0,**XTRL**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       Save APELEM                                  
         L     R3,0(,R1)           Row or column number to start with           
         STC   R3,APBYTE           Save row/column working on                   
         L     R2,4(,R1)           +1 or -1 for insert or delete                
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,XTRELQ     Extra data elements, X'C6'                   
         GOTO1 GETEL                                                            
         BNE   FIXXTR90            Nothing to fix                               
*                                                                               
EL       USING XTRELD,APELEM                                                    
                                                                                
FIXXTR10 SR    R3,R3                                                            
         IC    R3,1(,R1)           Length of element                            
         BCTR  R3,0                                                             
         EXMVC R3,APELEM,0(R1)     Copy element                                 
*                                                                               
         CLI   EL.XTRTYPE,XTRRTOT  Yes. Row total type ?                        
         BL    FIXXTR60            No, next element                             
         MVI   0(R1),X'FF'         Mark deleted on record                       
         CLI   EL.XTRTYPE,XTRCTOT  Yes. Column total type ?                     
         BH    FIXXTR20            Must be trailer record                       
         BE    FIXXTR12            Is column type                               
         CLI   REPMODE,REPROW      Row type ?                                   
         BNE   FIXXTR20            No. Adjust columns                           
         B     FIXXTR14                                                         
*                                                                               
FIXXTR12 CLI   REPMODE,REPCOL      Column type ?                                
         BNE   FIXXTR20            No. Adjust rows                              
*                                                                               
FIXXTR14 LTR   R2,R2               Insert or delete ?                           
         BM    FIXXTR16            Delete                                       
         CLC   APBYTE,EL.XTRNUM    Insert, where is row/col relative            
         BH    FIXXTR20            This is ok, check sub-elements               
         B     FIXXTR18            Increase row/column                          
*                                                                               
FIXXTR16 CLC   APBYTE,EL.XTRNUM    Match on row or column                       
         BH    FIXXTR20            This is ok, check sub-elements               
         BE    FIXXTR60            Remove element                               
*                                  Decrease row/column                          
FIXXTR18 IC    R3,EL.XTRNUM                                                     
         AR    R3,R2               +1 or -1                                     
         STC   R3,EL.XTRNUM        Replace with adjusted number                 
*                                                                               
FIXXTR20 SR    RF,RF                                                            
         IC    RF,EL.XTRSUB#       Number or sub-elements                       
         LA    RE,EL.XTRSUBEL      Point  to sub-element                        
*                                                                               
         USING XTRSUBEL,RE                                                      
FIXXTR25 TM    XTRSIND,XTRSROW     Row type                                     
         BZ    FIXXTR28            No, try column                               
         CLI   REPMODE,REPROW                                                   
         BE    FIXXTR30                                                         
*                                                                               
FIXXTR28 TM    XTRSIND,XTRSCOL     Column type                                  
         BZ    FIXXTR45            No, next sub-element                         
         CLI   REPMODE,REPCOL                                                   
         BNE   FIXXTR45            No, next sub-element                         
*                                                                               
FIXXTR30 CLC   APBYTE,XTRSNUM      See where it is relative to                  
         BH    FIXXTR45            This ok. Get next sub-element                
         BL    FIXXTR40            Adjust sub-element                           
         LTR   R2,R2               Insert or delete                             
         BP    FIXXTR40            Insert, adjust sub-element                   
         IC    R3,EL.XTRSUB#       Delete, remove sub-element                   
         SHI   R3,1                Adjust number of sub-elem's in elem          
         BNP   FIXXTR60            No sub-elements left, so delete elem         
*                                                                               
         STC   R3,EL.XTRSUB#       New number of sub-elements                   
         ZIC   R1,XTRSUBLN         Remove sub-element                           
         IC    R3,EL.XTRLN         Length of element                            
         SR    R3,R1               Less length of sub-element                   
         STC   R3,EL.XTRLN         New length of element                        
         LA    R4,APELEM           Start of element                             
         LR    R5,RE               RE = start of sub-element                    
         SR    R5,R4               R5 = length upto in element so far           
         SR    R3,R5               R3 = length to move                          
         BZ    FIXXTR48            Nothing to move                              
         BCTR  R3,0                                                             
         LA    R4,0(R1,RE)         Point to end of sub-element                  
         EXMVC R3,0(RE),0(R4)      Move data over old sub-element               
         B     FIXXTR48            RE = now points to next sub-elem             
*                                                                               
FIXXTR40 IC    R3,XTRSNUM                                                       
         AR    R3,R2               Adjust up or down, R2 = +1 or -1             
         STC   R3,XTRSNUM                                                       
*                                                                               
FIXXTR45 IC    R3,XTRSUBLN         Length of sub-element                        
         AR    RE,R3               Point to next sub-element                    
*                                                                               
FIXXTR48 BCT   RF,FIXXTR25         Process next                                 
         DROP  RE                                                               
*                                                                               
         L     R1,AIOAREA1         Delete any unwanted elements                 
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL                                                            
*                                                                               
         L     R1,AIOAREA1         Add -rebuilt X'C6' element                   
         MVI   APELCODE,XTRELQ                                                  
         GOTO1 ADDEL                                                            
*                                                                               
         L     R1,AIOAREA1         Find element just added                      
         GOTO1 GETEL                                                            
FIXXTR50 BNE   FIXXTR90                                                         
         CLC   EL.XTREL(XTRLNQ),0(R1)                                           
         BE    FIXXTR60            Found                                        
         GOTO1 NEXTEL              Try again                                    
         B     FIXXTR50                                                         
         DROP  EL                                                               
*                                                                               
FIXXTR60 MVI   APELCODE,XTRELQ     Next XTREL, X'C6'                            
         GOTO1 NEXTEL                                                           
         BE    FIXXTR10                                                         
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      remove all x'FF' elements                    
         GOTO1 DELEL                                                            
*                                                                               
FIXXTR90 MVC   APELEM,SVELEM       Restore element                              
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
*&&DO                                                                           
*=====================================================================*         
*        ADJUST XTRELD BASED ON COLUMN DELETED                        *         
*=====================================================================*         
                                                                                
FIXXTREL NMOD1 0,**XTRL**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM1,APELEM      SAVE   APELEM                                
         L     R3,0(,R1)           X'C3'  ELEMENT TO START FROM                 
         STC   R3,APBYTE           COLUMN NUMBER WORKING ON                     
         L     R2,4(,R1)           +1 OR -1 FOR INSERT OR DELETE                
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,XTRELQ     EXTRA DATA ELEMENTS      X'C6'               
         GOTO1 GETEL                                                            
         BNE   FIXXTR90            NOTHING TO FIX                               
*                                                                               
EL       USING XTRELD,APELEM                                                    
                                                                                
FIXXTR10 SR    R3,R3                                                            
         IC    R3,1(,R1)           LENGTH OF ELEMENT                            
         BCTR  R3,0                                                             
         EXMVC R3,APELEM,0(R1)     COPY ELEMENT                                 
*                                                                               
         CLI   EL.XTRTYPE,XTRCTOT  TOTAL TYPE ?                                 
         BL    FIXXTR60            NO, SO SKIP                                  
         MVI   0(R1),X'FF'         DELETE ELEMENT                               
         BH    FIXXTR20            YES, BUT NOT COLUMN TOTAL TYPE               
         CLC   APBYTE,EL.XTRNUM    MATCH ON COLUMN                              
         BE    FIXXTR60            ADJUST COLUMN NUMBER                         
         BH    FIXXTR20            THIS IS OK, CHECK SUB-ELEMENTS               
*                                                                               
FIXXTR15 IC    R3,EL.XTRNUM                                                     
         BCTR  R3,0                                                             
         STC   R3,EL.XTRNUM        REPLACE WITH ADJUSTED COLUMN NUMBER          
*                                                                               
FIXXTR20 SR    RF,RF                                                            
         IC    RF,EL.XTRSUB#       NUMBER OF SUB-ELEMENTS                       
         LA    RE,EL.XTRSUBEL      POINT  TO SUB-ELEMENTS                       
*                                                                               
         USING XTRSUBEL,RE                                                      
FIXXTR25 TM    XTRSIND,XTRSCOL     COLUMN TYPE                                  
         BZ    FIXXTR34            NEXT SUB-ELEMENT                             
         CLC   DELCOL#,XTRSNUM                                                  
         BH    FIXXTR34            NEXT   SUB-ELEMENT                           
         BL    FIXXTR30            ADJUST SUB-ELEMENT                           
         IC    R3,EL.XTRSUB#                                                    
         SHI   R3,1                                                             
         BP    FIXXTR28            MORE THAN 1 SUB-ELEMENT                      
         MVI   0(R1),X'FF'         NO SUB-ELEMENTS LEFT, SO DELETE ELEM         
         B     FIXXTR60                                                         
*                                                                               
FIXXTR28 STC   R3,EL.XTRSUB#       NEW NUMBER OF SUB-ELEMENTS                   
         ZIC   R2,XTRSUBLN         REMOVE SUB-ELEMENT                           
         IC    R3,EL.XTRLN                                                      
         SR    R3,R2                                                            
         STC   R3,EL.XTRLN         NEW LENGTH OF ELEMENT                        
         LA    R4,APELEM                                                        
         LR    R5,RE                                                            
         SR    R5,R4               LENGTH ACCOUNT FOR SO FAR                    
         SR    R3,R5               LENGTH  TO MOVE                              
         BZ    FIXXTR36            NOTHING TO MOVE                              
         BCTR  R3,0                                                             
         LA    R4,0(R2,RE)         POINT TO END OF SUB-ELEMENT                  
         EXMVC R3,0(RE),0(R4)      MOVE DATA OVER OLD SUB-ELEMENT               
         B     FIXXTR36                                                         
*                                                                               
FIXXTR30 IC    R3,XTRSNUM                                                       
         BCTR  R3,0                                                             
         STC   R3,XTRSNUM          ADJUSTED COLUMN NUMBER                       
*                                                                               
FIXXTR34 IC    R3,XTRSUBLN         LENGTH OF SUB-ELEMENT                        
         AR    RE,R3                                                            
*                                                                               
FIXXTR36 BCT   RF,FIXXTR25         NEXT SUB-ELEMENT                             
         DROP  RE                                                               
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL                                                            
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,XTRELQ     RE-BUILT C'C6' ELEMENT                       
         GOTO1 ADDEL                                                            
*                                                                               
         L     R1,AIOAREA1         FIND ELEMENT JUST ADDED                      
         GOTO1 GETEL                                                            
FIXXTR38 BNE   FIXXTR90                                                         
         CLC   EL.XTREL(XTRLNQ),0(R1)                                           
         BE    FIXXTR60            FOUND                                        
         GOTO1 NEXTEL              TRY AGAIN                                    
         B     FIXXTR38                                                         
         DROP  EL                                                               
*                                                                               
FIXXTR60 GOTO1 NEXTEL                                                           
         BE    FIXXTR10                                                         
         MVI   APELCODE,X'FF'      DELETE  X'FF'   ELEMENTS                     
         GOTO1 DELEL                                                            
*                                                                               
FIXXTR90 MVC   APELEM,SVELEM1      RESTORE ELEMENT                              
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
*  FIX CNN REFERENCES TO POINT TO THE UPDATED COLUMN NUMBERS          *         
***********************************************************************         
         SPACE 1                                                                
FIXCALC  NMOD1 0,**FIXC**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM1,APELEM      SAVE   APELEM                                
         L     R3,0(,R1)           X'C3'  ELEMENT TO START FROM                 
         STC   R3,APBYTE           COLUMN NUMBER WORKING ON                     
         L     R2,4(,R1)           +1 OR -1 FOR INSERT OR DELETE                
*                                                                               
         L     R1,AIOAREA1                                                      
         LA    R3,1                CHECK  FROM FIRST COLUMN                     
         MVI   APELCODE,RCLELQ     ADJUST COLUMN ELEMENT                        
*                                                                               
FIXCAL05 STC   R3,ELEMSEQ          SAVE   COLUMN NUMBER                         
         GOTO1 GETEL                                                            
         BE    FIXCAL12            ELEMENT FOUND, CONTINUE                      
         LTR   R2,R2               WAS IT DELETE ?                              
         BP    FIXCAL90            NO,  EXIT                                    
         LTR   R3,R3               DID  WE CHECK ALL ELEMENTS ?                 
         BZ    FIXCAL90            YES, EXIT                                    
         SR    R3,R3               FIND FIRST COLUMN DATA ELEMENT               
         B     FIXCAL05            TEST FIRST COLUMN DATA ELEMENT               
*                                                                               
         USING RCLELD,R3                                                        
FIXCAL10 BNE   FIXCAL90            FINISHED PROCESS ELEMENTS                    
*                                                                               
FIXCAL12 LR    R3,R1               GETEL/NEXTEL SET R1 = A(ELEMENT)             
         LTR   R2,R2               WAS IT INSERT?                               
         BP    FIXCAL15            YES, SKIP                                    
         CLC   RCLSEQ,APBYTE       IS THIS THE DELETED COLUMN ?                 
         BE    FIXCAL80            YES, GET NEXT ELEMENT                        
*                                                                               
FIXCAL15 TM    RCLOPT,RCLACCM      AN   ACCUMULATED COLUMN ?                    
         BZ    FIXCAL80            NO,  SO DON'T CARE ABOUT IT                  
         TM    RCLOPT,RCLEQU       AN   EQUATION TYPE COLUMN ?                  
         BO    FIXCAL18            YES, MODIFIY IT                              
         CLI   RCLSPCL,RCLSPEXR    A SPECIAL COLUMN ?                           
         BE    FIXCAL18            NO,  SO DON'T CARE ABOUT IT                  
         CLI   RCLSPCL,RCLSPCME    A SPECIAL COLUMN ?                           
         BNE   FIXCAL80            NO,  SO DON'T CARE ABOUT IT                  
*                                                                               
FIXCAL18 MVC   APWORK,SPACES                                                    
         LA    R8,APWORK                                                        
         LA    R4,RCLNDATA         POINT TO EQUATION                            
         SR    R0,R0                                                            
         IC    R0,RCLDATLN         LENGTH OF INPUT                              
*                                                                               
FIXCAL20 CLI   0(R4),C'C'          FIND "C" FOR COLUMN NUMBER                   
         BNE   FIXCAL55                                                         
         CLI   1(R4),C'0'          VERIFY "C#" WHERE "#" = A NUMBER             
         BL    FIXCAL55                                                         
         MVC   0(1,R8),0(R4)       MOVE IN   C'C'                               
         LA    R8,1(,R8)           BUMP PAST C'C' IN APWORK                     
         LA    R4,1(,R4)           BUMP PAST C'C' IN RCLNDATA                   
         LA    R6,1                START LENGTH OF NUMBER (AT LEAST 1)          
         BCTR  R0,0                R0 = # OF CHARACTER LEFT TO SCAN             
         CLM   R0,1,=AL1(2)                                                     
         BL    FIXCAL22            ONLY ROOM FOR 1 NUMBER NOT 2                 
         CLI   1(R4),C'0'          LOOK  ONE BEYOUND FIRST NUMBER               
         BL    *+8                 NOT   A   NUMBER  SO LENGTH ONE              
         LA    R6,2                MUST  BE  LENGTH  TWO                        
*                                                                               
FIXCAL22 GOTO1 VCASHVAL,APPARM,(C'N',(R4)),(R6)                                 
         CLI   APPARM,X'00'        VALID NUMBER                                 
         BNE   FIXCAL55            NO,  COPY THE CHARACTER                      
         CLC   APBYTE,APPARM+7                                                  
         BH    FIXCAL25                                                         
         BL    FIXCAL30                                                         
         LTR   R2,R2               INSERT OR DELETE                             
         BP    FIXCAL30            INSERT, SO OK TO ADJUST                      
*                                  DELETE, GENERATE WARNING MESSAGE -           
*                                        COLUMN POINTED TO WAS DELETED          
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACWCDELD)),              X        
               (RCLSEQ,AIOAREA1),('MNOMTWRN',0),0                               
*                                                                               
FIXCAL25 BCTR  R6,0                DON'T CHANGE THE COLUMN NUMBER               
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
         LA    R6,1(,R6)           ADD BACK THE ONE WE TOOK AWAY                
         AR    R4,R6               BUMP UP IN RCLNDATA                          
         AR    R8,R6               BUMP UP IN APWORK                            
         SR    R0,R6               COUNT DOWN WHAT'S LEFT IN RCLNDATA           
         BP    FIXCAL20            MORE DATA IN RCLNDATA                        
         B     FIXCAL60            FINISHED ADJUSTING THIS ONE                  
*                                                                               
FIXCAL30 AR    R4,R6               POINT  TO NEXT DATA                          
         SR    R0,R6               COUNT  DOWN                                  
         L     RF,APPARM+4         GET    BINARY VALUE OF COLUMN NUMBER         
         AR    RF,R2               -1 FOR DELETE, +1 FOR INSERT                 
         CVD   RF,APDUB            CONVERT TO CHARACTERS                        
         OI    APDUB+7,X'0F'       TO IGNORE SIGN                               
         UNPK  0(2,R8),APDUB                                                    
         LA    R6,2                TWO DIGITS                                   
         CLI   0(R8),C'0'          WAS IT A SINGLE DIGIT?                       
         BNE   FIXCAL35                                                         
         MVC   0(1,R8),1(R8)       REMOVE C'0'                                  
         MVI   1(R8),C' '          CLEAR NUMBER WE MOVED UP                     
         LA    R6,1                ONE   DIGIT                                  
*                                                                               
FIXCAL35 AR    R8,R6               R8 = NEXT POSITION IN APWORK                 
         LTR   R0,R0               RAN  OUT OF DATA IN RCLNDATA?                
         BNZ   FIXCAL20            MORE DATA IN RCLNDATA                        
         B     FIXCAL60            FINISHED ADJUSTING THIS ONE                  
*                                                                               
FIXCAL55 DS    0H                                                               
         CLI   0(R4),C' '          CHARACTER SPACE ?                            
         BE    FIXCAL57            YES,  DON'T MOVE                             
         MVC   0(1,R8),0(R4)       RE-BUILDING RCLNDATA, R8=APWORK              
         LA    R8,1(,R8)           NEXT  POSTION IN APWORK                      
*                                                                               
FIXCAL57 DS    0H                                                               
         LA    R4,1(,R4)           CHECK NEXT CHARACTER IN RCLNDATA             
         BCT   R0,FIXCAL20                                                      
         SPACE 1                                                                
*---------------------------------------------------------------------*         
*  DETERMINE IF THE EQUATION WILL BE TRUNCATED                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING COLD,R4                                                          
FIXCAL60 LA    R4,APWORK           ->    WORK AREA                              
         SR    R8,R4               LENGTH OF EQUATION                           
         CLM   R8,1,=AL1(MXCDATLN) LENGTH > MAXIMUM DATA LENGTH ?               
         BNH   FIXCAL65            NO,   SKIP                                   
*                                  YES   GENERATE WARNING MESSAGE -             
*                                        EQUATION TRUNCATED                     
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACWTRUNC)),              X        
               (RCLSEQ,AIOAREA1),('MNOMTWRN',0),0                               
         LA    R8,MXCDATLN         MAX LENGTH OF DATA (TRUNCATE DATA)           
*                                                                               
FIXCAL65 CLM   R8,1,RCLDATLN       DID LENGTH CHANGE ?                          
         BNE   FIXCAL68                                                         
         BCTR  R8,0                                                             
         EXMVC R8,RCLNDATA,APWORK                                               
         B     FIXCAL80                                                         
*                                                                               
NEW      USING RCLELD,APELEM                                                    
*                                                                               
FIXCAL68 MVC   NEW.RCLEL(RCLNLNQ),RCLEL                                         
         STC   R8,NEW.RCLDATLN     SAVE NEW LENGTH                              
         BCTR  R8,0                                                             
         EXMVC R8,NEW.RCLNDATA,APWORK                                           
         LA    RE,NEW.RCLNDATA+1(R8)   POINT TO END OF DATA                     
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,RCLHD1LN         LENGTH OF HEADING ONE                        
         IC    RF,RCLHD2LN         LENGTH OF HEADING TWO                        
         AR    R1,RF                                                            
         SH    R1,=H'01'                                                        
         BM    FIXCAL70                                                         
         IC    RF,RCLDATLN                                                      
         LA    RF,RCLNDATA(RF)                                                  
         EXMVC R1,0(RE),0(RF)      MOVE IN HEADINGS                             
         LA    RE,1(R1,RE)         POINT TO END OF ELEMENT                      
*                                                                               
FIXCAL70 LA    RF,NEW.RCLEL        RF = START OF  ELEMENT                       
         SR    RE,RF               RE = END OF ELEMENT                          
         STC   RE,NEW.RCLLN        SAVE OFF LENGTH                              
         MVI   RCLEL,X'FF'         DELETE ELEMENT                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE THOSE MARKED                          
         GOTO1 DELEL                                                            
         MVI   APELCODE,RCLELQ     RESTORE ELEMENT CODE                         
         GOTO1 ADDEL                                                            
         MVC   ELEMSEQ,RCLSEQ                                                   
         GOTO1 GETEL               GET A(ELEMENT ADDED)                         
         LR    R3,R1                                                            
*                                                                               
FIXCAL80 CLC   APBYTE,RCLSTACK     ADJUST STACK UNDER NUMBER ?                  
         BH    FIXCAL85            HIGH   SKIP                                  
         BL    FIXCAL83            LOW,   ADJUST                                
*                                  EQUAL                                        
         LTR   R2,R2               WAS    IT    INSERT ?                        
         BP    FIXCAL83            YES,   ADJUST                                
*                                  DELETE, GENERATE WARNING MESSAGE -           
*                                         COL STACKED UNDER WAS DELETED         
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACWCSUD)),               X        
               (RCLSEQ,AIOAREA1),('MNOMTWRN',0),0                               
*                                                                               
FIXCAL83 ZIC   R6,RCLSTACK         GET    STACK UNDER NUMBER                    
         AR    R6,R2               ADJUST STACK UNDER NUMBER                    
         STC   R6,RCLSTACK         SAVE   STACK UNDER NUMBER                    
*                                                                               
FIXCAL85 GOTO1 NEXTEL,(R3)         RETURNS ADDRESS IN R1                        
         B     FIXCAL10                                                         
*                                                                               
FIXCAL90 MVC   APELEM,SVELEM1      RESTORE APELEM                               
         XMOD1 ,                   RETURN                                       
         DROP  NEW                                                              
         DROP  R3,R4                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  FIX UP PROFILE DATA FIELDS THAT CONTAIN COLUMN NUMBER DATA         *         
*                                                                     *         
*  PARAMETERS:                                                        *         
*    PARM 1 BYTES 0-3 COLUMN  NUMBER TO START UPDATING FROM           *         
*    PARM 2 BYTES 0-3 +1 FOR  INSERT                                  *         
*                     -1 FOR  DELETE                                  *         
*                                                                     *         
*  INPUT AREA:                                                        *         
*    IOAREA1  - RECORD  AREA                                          *         
*    RANKON   - PROFILE RANK    ON     FIELD                          *         
*    RANKCOL  - PROFILE RANK    COL    FIELD                          *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    FVMSGNO  - ERROR   MESSAGE NUMBER SET  OR   CLEARED              *         
*    RANKON   - PROFILE RANK    ON     FIELD                          *         
*    RANKCOL  - PROFILE RANK    COL    FIELD                          *         
*    ON ERROR:  CONDITION       CODE   SET  TO   NOT  EQUAL           *         
*                                                                     *         
*  USES:                                                              *         
*    APBYTE   - SAVE    START   NUMBER                                *         
*    APELCODE - MESSAGE ELEMENT                                       *         
*    SVELEM   - SAVE    AREA    FOR    APELEM                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1           PROFILE DATA    ELEMENT                      
FIXPROF  NMOD1 0,**FPRF**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE    APELEM                               
         L     R3,0(,R1)           FIRST   COLUMN  NUM     TO   UPDATE          
         STC   R3,APBYTE           SAVE    COLUMN  NUMBER                       
         L     R2,4(,R1)           +1      OR      -1      FOR  UPDATE          
         SR    R6,R6               CLEAR   REGISTER                             
*                                                                               
         CHI   R2,1                INSERT ?                                     
         BE    FIXP10              YES,    SKIP                                 
*                                  CHECK   COLUMN  RANKING FOR  ERRORS          
         GOTO1 =A(CHKCRANK),RR=APRELO                                           
         BNE   FIXPROFX            ON      ERROR,  EXIT                         
*                                                                               
FIXP10   DS    0H                                                               
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,RPFELQ     GET     PROFILE ELEMENT                      
         GOTO1 GETEL                                                            
         BNE   FIXPRFOK            NONE,   EXIT                                 
         OC    RPFRKON,RPFRKON     ANY     RANK    ON      COLUMN               
         BZ    FIXP30              NO,     SKIP    RANKING TESTS                
         CLI   RPFRKON,C'C'        RANK    ON      COLUMN  NUMBER ?             
         BNE   FIXP20              NO,     SKIP                                 
         CLC   APBYTE,RPFRKON+1    DO      WE      NEED    AN   UPDATE?         
         BH    FIXP20              HIGH,   SKIP    UPDATING                     
         IC    R6,RPFRKON+1        GET     THE     NUMBER                       
         AR    R6,R2               UPDATE  THE     NUMBER                       
         STC   R6,RPFRKON+1                                                     
         STC   R6,RANKON+1                                                      
*                                                                               
FIXP20   DS    0H                  CHECK   RANK    USING   COLUMN               
         CLC   APBYTE,RPFRKCL      DO      WE      NEED    AN   UPDATE?         
         BH    FIXP30              HIGH,    SKIP    UPDATING                    
         IC    R6,RPFRKCL          GET     THE     NUMBER                       
         AR    R6,R2               UPDATE  THE     NUMBER                       
         STC   R6,RPFRKCL                                                       
         STC   R6,RANKCOL                                                       
*                                                                               
FIXP30   DS    0H                  LABEL   FOR     FUTURE  TESTS                
*                                                                               
FIXPRFOK DS    0H                  EXIT    OKAY                                 
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
         MVC   APELEM,SVELEM       RESTORE APELEM                               
*                                                                               
FIXPROFX DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION       CODE                 
         XMOD1 ,                                                                
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CHECK THE PROFILE RANK FIELDS THAT CONTAIN COLUMN NUMBER DATA.     *         
*  THIS ROUTINE IS ONLY CALLED WHEN A COLUMN IS TO BE DELETED.        *         
*                                                                     *         
*  INPUT:                                                             *         
*    APBYTE   - COLUMN  TO      BE     DELETED                        *         
*    RANKON   - PROFILE RANK    ON     FIELD                          *         
*    RANKCOL  - PROFILE RANK    COL    FIELD                          *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    FVMSGNO  - ERROR   MESSAGE NUMBER SET  OR   CLEARED              *         
*                                                                     *         
*    ON ERROR:  CONDITION       CODE   SET  TO   NOT  EQUAL           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
CHKCRANK NMOD1 0,**CRNK**                                                       
         L     RC,APALOCAL                                                      
*                                                                               
         OC    RANKON,RANKON       ANY     PROFILE RANKING ?                    
         BZ    CHKCROK             NONE,   EXIT                                 
         CLI   RANKON,C'C'         RANK    ON      COLUMN  NUMBER ?             
         BNE   CHKCR20             NO,     SKIP                                 
         CLC   APBYTE,RANKON+1     DELETE  COLUMN  =       COL  NUMBER?         
         BE    CHKCRNG             YES,    ERROR                                
*                                                                               
CHKCR20  DS    0H                  CHECK   RANK    USING   COLUMN               
         CLC   APBYTE,RANKCOL      DELETE  COLUMN  =       COL  NUMBER?         
         BNE   CHKCROK             NO,     RANKING IS      OKAY                 
*                                                                               
CHKCRNG  DS    0H                  ERROR                                        
*                                  DELETE  OF      A       RANKING              
*                                          FIELD   INVALID -                    
         MVC   FVMSGNO,=AL2(ACEDRNKF)      FIX     PROFILE DATA                 
*                                                                               
         USING COLD,R1             MAP     COLUMN  AREA                         
         LA    R1,FRMCOL1H         ->      COLUMN  AREA                         
         ZIC   R2,APBYTE           GET     COLUMN  NUMBER                       
         BCTR  R2,0                MINUS   1                                    
         MH    R2,=Y(COLLNQ)       FIND    COLUMN  OFFSET                       
         AR    R1,R2               FIND    COLUMN  ADDRESS                      
         LA    R2,COLDATAH         ->      COLUMN  DATA                         
         ST    R2,FVADDR           SET     CURSOR                               
         DROP  R1                                                               
*                                                                               
         CLI   APPFKEY,PFKDEL      DELETE  PF      KEY ?                        
         BNE   CHKCREX             NO,     SKIP                                 
         MVI   APPFKEY,0           CLEAR   DELETE  PF      KEY                  
         B     CHKCREX             EXIT                                         
*                                                                               
CHKCROK  DS    0H                  EXIT    OKAY                                 
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
*                                                                               
CHKCREX  DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION       CODE                 
         XMOD1 ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  FIX UP PROFILE DATA FIELDS THAT CONTAIN ROW NUMBER DATA            *         
*                                                                     *         
*  PARAMETERS:                                                        *         
*    PARM 1 BYTES 0-3 +1 FOR  INSERT                                  *         
*                     -1 FOR  DELETE                                  *         
*                                                                     *         
*  INPUT AREA:                                                        *         
*    IOAREA1  - RECORD  AREA                                          *         
*    RANKON   - PROFILE RANK    ON     FIELD                          *         
*    ORANKON  - PROFILE RANK    ON     FIELD (OLD)                    *         
*    NROWS    - ROW     SEQUENCE       NUMBER                         *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    RANKON   - PROFILE RANK    ON     FIELD                          *         
*    FVMSGNO  - ERROR   MESSAGE NUMBER SET  OR   CLEARED              *         
*                                                                     *         
*    ON ERROR:  CONDITION       CODE   SET  TO   NOT  EQUAL           *         
*                                                                     *         
*  USES:                                                              *         
*    APBYTE   - SAVE    START   NUMBER                                *         
*    APELCODE - MESSAGE ELEMENT                                       *         
*    SVELEM   - SAVE    AREA    FOR    APELEM                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1           PROFILE DATA ELEMENT                         
FIXRPROF NMOD1 0,**RPRF**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE    APELEM                               
         L     R2,0(,R1)           +1      OR   -1   FOR  UPDATE                
         SR    R6,R6               CLEAR   REGISTER                             
*                                                                               
         ZIC   R3,NROWS            GET     ROW  NUMBER                          
         LA    R3,1(,R3)           BUMP    TO   CURR ROW  NUMBER                
         STC   R3,APBYTE           SAVE    CURR ROW  NUMBER                     
         CLC   APBYTE,ORANKON+1    RANK    ON   THIS ROW ?                      
*                                  YES,    DELETE    OF   A                     
         BE    FIXRPRNG                    RANKING FLD                          
*                                                                               
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,RPFELQ     GET     PROFILE   ELEMENT                    
         GOTO1 GETEL                                                            
         BNE   FIXRPROK            NONE,   EXIT OKAY                            
         OC    RPFRKON,RPFRKON     ANY     RANK ON   ROW ?                      
         BZ    FIXR20              NO,     SKIP RANKING   TESTS                 
         CLI   RPFRKON,C'R'        RANK    ON   ROW  NUMBER ?                   
         BNE   FIXR20              NO,     SKIP                                 
         CLC   APBYTE,ORANKON+1    DO      WE   NEED AN   UPDATE?               
         BH    FIXR20              LOW,    SKIP UPDATING                        
         IC    R6,RPFRKON+1        GET     THE  NUMBER    IN   RCD              
         AR    R6,R2               UPDATE  IT                                   
         STC   R6,RPFRKON+1                                                     
         STC   R6,RANKON+1                                                      
*                                                                               
         CHI   R2,1                INSERT  ?                                    
         BNE   FIXR20              NO,     SKIP                                 
         IC    R6,ORANKON+1        GET     OLD  NUMBER                          
         AR    R6,R2               UPDATE  IT                                   
         STC   R6,ORANKON+1                                                     
*                                                                               
FIXR20   DS    0H                  LABEL   FOR  FUTURE    TESTS                 
*                                                                               
FIXRPROK DS    0H                  EXIT    OKAY                                 
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
         B     FIXRPREX            EXIT                                         
*                                                                               
FIXRPRNG DS    0H                  DELETE  OF   A    ROW  RANKING               
*                                          FIELD     INVALID -                  
         MVC   FVMSGNO,=AL2(ACEDRNKF)      FIX  PROFILE   DATA                  
         LA    R1,FRMROW1H         ->      1ST  ROW                             
         ZIC   R2,ORANKON+1        ROW     NUMBER                               
         BCTR  R2,0                                                             
         MH    R2,=Y(ROWLNQ)       ROW     OFFSET                               
         AR    R1,R2               ROW     IN   ERROR                           
*                                  GET     TO   DATA FIELD                      
         LA    R1,ROWDATAH-ROWD(,R1)                                            
         ST    R1,FVADDR           ->      CURSOR    LOCATION                   
*                                                                               
FIXRPREX DS    0H                                                               
         MVC   APELEM,SVELEM       RESTORE APELEM                               
         CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION       CODE                 
         XMOD1 ,                                                                
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  FIX UP MESSAGE ELEMENTS THAT CONTAIN COLUMN NUMBER DATA            *         
*                                                                     *         
*  PARAMETERS:                                                        *         
*    PARM 1 BYTES 0-3 COLUMN  NUMBER TO START UPDATING FROM           *         
*    PARM 2 BYTES 0-3 +1 FOR  INSERT                                  *         
*                     -1 FOR  DELETE                                  *         
*    PARM 3 BYTES 3   YES     UPDATE        MNOSEQ                    *         
*                     NO      DO NOT UPDATE MNOSEQ                    *         
*                                                                     *         
*  INPUT AREA:                                                        *         
*    IOAREA1  - RECORD  AREA                                          *         
*                                                                     *         
*  USES:                                                              *         
*    APBYTE   - SAVE    START   NUMBER                                *         
*    APELCODE - MESSAGE ELEMENT                                       *         
*    SVELEM   - SAVE    AREA    FOR    APELEM                         *         
*    FIXSEQ#  - SAVE    UPDATE  MNOSEQ PARAMETER                      *         
***********************************************************************         
         SPACE 1                                                                
         USING MNOELD,R1           MESSAGE ELEMENT                              
WARNP    USING MNOMLN,R3           MESSAGE NUMBER  PORTION                      
         SPACE 1                                                                
FIXMSGS  NMOD1 0,**FIXM**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE    APELEM                               
         L     R3,0(,R1)           FIRST   COLUMN  NUM     TO   UPDATE          
         STC   R3,APBYTE           SAVE    COLUMN  NUMBER                       
         L     R2,4(,R1)           +1      OR      -1      FOR  UPDATE          
         MVC   FIXSEQ#,11(R1)      SAVE    FIX     UP      MNOSEQ               
         SR    R6,R6               CLEAR   REGISTER                             
         SR    R8,R8               CLEAR   REGISTER                             
*                                                                               
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,MNOELQ     GET     MESSAGE ELEMENT                      
         GOTO1 GETEL                                                            
         B     FIXM15                                                           
*                                                                               
FIXM10   DS    0H                                                               
         GOTO1 NEXTEL              GET     NEXT    ELEMENT                      
*                                                                               
FIXM15   DS    0H                                                               
         BNE   FIXMSGSX            NOT     FOUND,  EXIT                         
         CLI   MNOSTYPE,MNOSTCOL   COLUMN  ELEMENT ?                            
         BNE   FIXM10              NO,     NEXT    ELEMENT                      
         CLI   FIXSEQ#,YES         FIX     UP      MNOSEQ ?                     
         BNE   FIXM20              NO,     SKIP                                 
         CLC   MNOSEQ,APBYTE       COLUMN  NUMBER  <       1ST  NUMBER?         
         BL    FIXM20              YES,    SKIP                                 
         ZIC   RF,MNOSEQ           GET     COLUMN  NUMBER                       
         AR    RF,R2               UPDATE  COLUMN  NUMBER                       
         STC   RF,MNOSEQ           SAVE    COLUMN  NUMBER                       
*                                                                               
FIXM20   DS    0H                                                               
         LA    R3,MNOLN1Q(,R1)     ->      MESSAGE ELEMENT                      
         ZIC   R4,MNOLN            ELEMENT LENGTH                               
         AR    R4,R1               ->      NEXT    ELEMENT                      
*                                                                               
FIXM30   DS    0H                                                               
         IC    R6,WARNP.MNOMLN     GET     MESSAGE LENGTH                       
*                                  ONE     BYTE    OF      MSG  DATA ?          
         CLI   WARNP.MNOMLN,MNOMELQ+L'MNOMDTYP+1                                
         BNE   FIXM40              NO,     NOT     COLUMN  NUM  DATA            
*                                  COLUMN  NUMBER  DATA ?                       
         CLI   WARNP.MNOMDTYP,MNOMDCOL                                          
         BNE   FIXM40              NO,     NOT     COLUMN  NUM  DATA            
*                                  NUMBER  <       FIRST   NUM  WANTED?         
         CLC   WARNP.MNOMDATA,APBYTE                                            
         BL    FIXM40              YES,    GET     NEXT    MESSAGE              
         IC    R8,WARNP.MNOMDATA   GET     COLUMN  NUMBER                       
         AR    R8,R2               ADJUST  BY      +/-     ONE                  
         STC   R8,WARNP.MNOMDATA   SAVE    UPDATED COLUMN  NUMBER               
*                                                                               
FIXM40   DS    0H                                                               
         AR    R3,R6               ->      NEXT    MESSAGE                      
         CR    R3,R4               FOUND   NEXT    MESSAGE ?                    
         BL    FIXM30              YES,    ADJUST  IT                           
         B     FIXM10              NO,     NEXT    ELEMENT                      
*                                                                               
FIXMSGSX DS    0H                                                               
         MVC   APELEM,SVELEM       RESTORE APELEM                               
         XMOD1 ,                   RETURN                                       
         DROP  R1,WARNP                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BUILD TABLE OF COLUMNS, SAVE OF TYPE OF COLUMN AND COLUMN #S       *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2                                                        
         USING RFLELD,R4                                                        
         USING CSEQTABD,R3                                                      
CCOLSEQ  NMOD1 0,**CCOL**                                                       
         L     RC,APALOCAL                                                      
*                                        Clear duplicate sort # used            
         GOTO1 ADELWARN,LWPARM,ACEDUSN,('MNOMTERR',0)                           
*                                                                               
         LHI   RE,ACEMJBR                Clear too many jobber keywords         
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         MVI   LWPARM+4,MNOMTWRN         Set message type to warnning           
         LHI   RE,ACWHSUC                Clear hidden col being stacked         
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         LHI   RE,ACWCSUH                Clear stack under hidden col           
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         LA    R6,1                START AT ONE                                 
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     COLUMN ELEMENTS X'C3'                        
         GOTO1 GETEL                                                            
CCOLSQ04 BNE   CCOLSQ08            NO MORE COLUMNS                              
         LR    R2,R1                                                            
*                                                                               
CCOLSQ05 MVI   APELCODE,RFLELQ     COLUMN FILTERS X'C5'                         
         MVC   ELEMSEQ,RCLSEQ      GET SAME NUMBER AS X'C3' HAS                 
         GOTO1 NEXTEL,(R1)                                                      
         BNE   CCOLSQ06                                                         
         LR    R4,R1                                                            
         STC   R6,RFLSEQ           RE-SEQUENCE COLUMN FILTER                    
         B     CCOLSQ05                                                         
*                                                                               
CCOLSQ06 STC   R6,RCLSEQ           RE-SEQUENCE COLUMN                           
         LA    R6,1(,R6)           NEXT NUMBER                                  
         MVI   APELCODE,RCLELQ     RESET FOR COLUMN ELEMENTS X'C3'              
         GOTO1 NEXTEL,(R2)                                                      
         B     CCOLSQ04                                                         
         DROP  R4                                                               
*                                                                               
CCOLSQ08 BCTR  R6,0                ONE LESS                                     
         STC   R6,TOT#COLS         TOTAL NUMBER OF COLUMNS                      
*                                                                               
         LA    R0,TABOFCOL         ADDR OF COLUMN ARRAY                         
         LA    R1,MAXCOLS*CSEQLNQ  SIZE OF COLUMN ARRAY                         
         SR    RF,RF               NO   FROM AREA                               
         MVCL  R0,RE               CLEAR THE TABLE                              
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     COLUMN ELEMENTS X'C3'                        
         GOTO1 GETEL                                                            
*                                                                               
CCOLSQ10 BNE   CCOLSQ90                                                         
         LR    R2,R1                                                            
         SR    RF,RF                                                            
         IC    RF,RCLSEQ           COLUMN SEQUENCE NUMBER                       
         BCTR  RF,0                                                             
         MHI   RF,CSEQLNQ                                                       
         LA    R3,TABOFCOL(RF)                                                  
         MVC   CSEQNUM,RCLSEQ      SAVE OF COLUMN NUMBER                        
         TM    RCLOPT,RCLHIDE      ANY  PRINT  COLUMN ?                         
         BO    CCOLSQ11            NO,  IT IS  HIDDEN                           
         MVC   CSEQWDTH,RCLWDTH    SAVE COLUMN WIDTH                            
         B     CCOLSQ12            CONTINUE                                     
*                                                                               
CCOLSQ11 OI    CSEQIND,CSEQHIDE    SAY  HIDDEN COLUMN                           
*                                                                               
CCOLSQ12 SR    R6,R6                                                            
         ICM   R6,1,RCLSORTN                                                    
         BZ    CCOLSQ16                                                         
         SLL   R6,1                MULTIPLY BY TWO                              
         LA    R6,COLSORT#(R6)                                                  
         CLI   0(R6),C'*'                                                       
         BE    CCOLSQ14            ERROR                                        
         MVI   0(R6),C'*'          MARK AS USED                                 
         MVC   1(1,R6),RCLSEQ      SAVE COLUMN NUMBER                           
         B     CCOLSQ16            CONTINUE                                     
*                                                                               
CCOLSQ14 DS    0H                  DUPLICATE SORT NUMBER USED                   
*                                       MARK BOTH USAGES                        
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACEDUSN)),               X        
               (RCLSEQ,AIOAREA1),('MNOMTERR',0),0                               
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACEDUSN)),               X        
               (1(R6),AIOAREA1),('MNOMTERR',0),0                                
*                                                                               
CCOLSQ16 TM    RCLOPT2,RCLJOBR     JOBBER KEYWORD USED?                         
         BZ    CCOLSQ18            NO                                           
         LA    R6,1                                                             
         AH    R6,JOBRNUM          CURRENT NUMBER+1 OF JOBBER KEYWORDS          
         STH   R6,JOBRNUM                                                       
         CHI   R6,MAXJOBR          REACHED LIMIT?                               
         BNH   CCOLSQ18            NO,  CONTINUE                                
*                                  YES, TOO MANY JOBBER KEYWORDS                
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACEMJBR)),               X        
               (RCLSEQ,AIOAREA1),('MNOMTERR',0),0                               
*                                                                               
CCOLSQ18 DS    0H                                                               
*&&UK                                                                           
         TM    RCLOPT4,RCLTSDTE    TSDTE KEYWORD USED?                          
         BZ    *+8                 NO                                           
         OI    KWDINDS,KWDTSDTE                                                 
*&&                                                                             
         CLI   RCLSPCL,RCLSPEXR    EXCHANGE    RATE    ?                        
         BE    CCOLSQ19            YES,        PROCESS                          
         CLI   RCLSPCL,RCLSPCME    CUME        COLUMN  ?                        
         BE    CCOLSQ19            YES,        PROCESS                          
         TM    RCLOPT,RCLEQU       AN EQUATION COLUMN  ?                        
         BO    CCOLSQ21                                                         
         TM    RCLOPT,RCLACCM      AN AMOUNT   COLUMN  ?                        
         BZ    *+8                                                              
         OI    CSEQIND,CSEQAMT                                                  
         TM    RCLOPT2,RCLTOT      A TOTAL     COLUMN  ?                        
         BZ    *+8                                                              
         OI    CSEQIND,CSEQTOT                                                  
         B     CCOLSQ50                                                         
*                                                                               
CCOLSQ19 OI    CSEQIND,CSEQEQU     EQUATION TYPE                                
         SR    R0,R0                                                            
         IC    R0,RCLDATLN         GET LENGTH OF DATA                           
         LA    R4,RCLNDATA         ->   DATA FIELD                              
*                                                                               
CCOLSQ20 CLC   0(1,R4),APOPNPRN    FIND OPEN PARENTHESIS                        
         BE    CCOLSQ22            YES, CONTINUE  EQUATION  LOGIC               
         LA    R4,1(,R4)           TRY  NEXT CHARACTER                          
         BCT   R0,CCOLSQ20         TEST NEXT CHARACTER                          
*                                                                               
CCOLSQ21 OI    CSEQIND,CSEQEQU     EQUATION TYPE                                
*                                                                               
         SR    R0,R0                                                            
         IC    R0,RCLDATLN         GET LENGTH OF DATA                           
         LA    R4,RCLNDATA         POINT TO BEGIN OF DATA                       
CCOLSQ22 CLI   0(R4),C'C'          FIND "C" FOR COLUMN#                         
         BE    CCOLSQ25                                                         
         LA    R4,1(,R4)           BUMP UP                                      
         BCT   R0,CCOLSQ22         COUNT DOWN                                   
         B     CCOLSQ50            FINISHED, GET NEXT ELEMENT                   
*                                                                               
CCOLSQ25 LR    RF,R0                                                            
         CLM   RF,1,=AL1(MXCDATLN)   AT BEGINING OF ELEMENT DATA ?              
         BE    CCOLSQ26              YES, SO DON'T CHECK FOR VERTICAL%          
         LR    R5,R4                                                            
         BCTR  R5,0                BUMP BACK ONE TO FIND C'V'                   
         CLI   0(R5),C'V'          CHECK VERTICAL ?                             
         BNE   CCOLSQ26                                                         
         CLI   2(R5),C'0'          VERT PERCENT MUST BE VC# OR VR#              
         BL    CCOLSQ26             NOT A NUMBER SO NOT A VERT %                
         MVI   CSEQVPCT,C'V'       MARK TO SAY FOUND AND TO REPLACE             
*                                                                               
CCOLSQ26 LA    R4,1(,R4)           POINT TO NUMBER                              
         BCTR  R0,0                COUNT DOWN                                   
         LA    R5,1                START LENGTH OF NUMBER (AT LEAST 1)          
         CLI   1(R4),C'0'          LOOK  ONE BEYOUND FIRST NUMBER               
         BL    *+8                 NOT   A   NUMBER  SO LENGTH ONE              
         LA    R5,2                MUST  BE  LENGTH  TWO                        
*                                                                               
         GOTO1 VCASHVAL,APPARM,(C'N',(R4)),(R5)                                 
         AR    R4,R5               POINT TO NEXT DATA                           
         SR    R0,R5               COUNT DOWN                                   
         BM    CCOLSQ50            MUST  BE FINISHED, GET NEXT ELEMENT          
         L     RF,APPARM+4         GET   BINARY VALUE OF COLUMN NUMBER          
*                                                                               
         LA    R1,1                                                             
         LA    R5,4                MAX   OF FOUR COLUMN NUMBERS                 
         LA    RE,CSEQCOL1         FIND  UN-USED ENTRY                          
CCOLSQ28 CLI   0(RE),0                                                          
         BE    CCOLSQ30                                                         
         LA    RE,1(,RE)           BUMP  UP                                     
         LA    R1,1(,R1)           COUNT UP                                     
         BCT   R5,CCOLSQ28                                                      
         DC    H'00'               SHOULD NEVER GET HERE                        
*                                                                               
CCOLSQ30 CLI   CSEQVPCT,C'V'       THIS COLUMN IS VERTICAL% ?                   
         BNE   *+8                 REPLACE C'V' WITH INDEX #                    
         STC   R1,CSEQVPCT         SAVE INDEX 1 TO 4, INTO CSEQCOL1             
         STC   RF,0(,RE)           SAVE  OF COLUMN NUMBER                       
         LTR   R0,R0                                                            
         BNZ   CCOLSQ22            CHECK FOR ANOTHER COLUMN NUMBER              
*                                                                               
CCOLSQ50 MVC   CSEQCSTK,RCLSTACK   SAVE STACK UNDER COLUMN                      
         SR    R4,R4                                                            
         ICM   R4,1,RCLSTACK       IS   STACK UNDER COLUMN = 0 ?                
         BZ    CCOLSQ70            YES, SKIP                                    
         MVI   CSEQWDTH,0          NO,  COL   TAKES NO     NEW   SPACE          
         TM    CSEQIND,CSEQHIDE    IS   THIS  COL   HIDDEN ?                    
         BZ    CCOLSQ55            NO,  SKIP                                    
*                                  HIDDEN     COL   BEING  STACKED              
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACWHSUC)),               X        
               (RCLSEQ,AIOAREA1),('MNOMTWRN',0),0                               
*                                                                               
CCOLSQ55 DS    0H                                                               
         BCTR  R4,0                FIND STACK UNDER COLUMN ENTRY                
         MHI   R4,CSEQLNQ                                                       
         LA    R4,TABOFCOL(R4)                                                  
         SR    R5,R5               DID  COL   HAVE  STACK  UNDER NUM ?          
         ICM   R5,1,CSEQCSTK-CSEQTABD(R4)                                       
         BZ    CCOLSQ60            NO,  SKIP                                    
         STC   R5,CSEQCSTK         YES, SAY   OLD   STACK  UNDER NUMBER         
         STC   R5,RCLSTACK              IN    TABLE AND    IN    RECORD         
*                                                                               
         LR    R4,R5               FIND STACK UNDER COLUMN ENTRY                
         BCTR  R4,0                                                             
         MHI   R4,CSEQLNQ                                                       
         LA    R4,TABOFCOL(R4)                                                  
*                                                                               
CCOLSQ60 DS    0H                  IS   STACK UNDER COLUMN HIDDEN ?             
         TM    CSEQIND-CSEQTABD(R4),CSEQHIDE                                    
         BZ    CCOLSQ70            NO,  SKIP                                    
*                                  STACKING   UNDER HIDDEN COLUMN               
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACWCSUH)),               X        
               (RCLSEQ,AIOAREA1),('MNOMTWRN',0),0                               
*                                                                               
CCOLSQ70 GOTO1 NEXTEL,(R2)                                                      
         B     CCOLSQ10                                                         
*                                                                               
CCOLSQ90 XMOD1 ,                   RETURN                                       
         DROP  R2,R3                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  THIS CHECKS FOR COLUMN EQUATIONS THAT MAY CAUSE LOOPING, I.E.      *         
*       IF  COL2 = C2+C3 AND COL3 = C2+C5 THEN                        *         
*           C2 POINTS TO C3 AND C3 POINTS BACK TO C2 (BAD EQUATIONS)  *         
*                                                                     *         
*  TABOFCOL IS AN ARRAY 1 TO 32 FOR EACH COLUMN                       *         
*       IT HAS AN INDICATOR TO SAY IT IS AN AMOUNT OR EQUATION        *         
*       IF IT IS AN EQUATION THEN THE COLUMN NUMBERS IT USES ARE      *         
*       STORED IN BINARY VALUE. THERE ARE AT MOST 4 ENTRIES, I.E      *         
*           C1+C2+C3+C4 ==> AL1(1,2,3,4)                              *         
*                                                                     *         
*  TO CHECK WE TRAVERSE THE EQUATION TO SEE WHERE EACH COLUMN LEADS   *         
*     I.E.  IF  C2 = C3+C4 THEN WE LOOK INTO TABOFCOL TO SEE WHAT     *         
*               C3 IS, IF C3 = C5+C6 WE THEN LOOK AT C5.              *         
*           WE ONLY STOP WHEN THE COLUMN IS AN AMOUNT COLUMN.         *         
*           IF NEITHER AN EQUATION OR AMOUNT THEN IT IS AN ERROR      *         
*           IF THE COLUMN WE PROCESS MATCH ANY OF THE PREVIOUS ONES   *         
*              IN THE CHAIN THEN IT IS A LOOP                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CSEQTABD,R3                                                      
NXT      USING CSEQTABD,R6                                                      
         SPACE 1                                                                
CHKCOLS  NMOD1 0,**CHKC**                                                       
         L     RC,APALOCAL                                                      
*                                        Clear invalid sort sequence            
         GOTO1 ADELWARN,LWPARM,ACEIVSS,('MNOMTERR',0)                           
*                                                                               
         LHI   RE,ACEVERT                Clear invalid vertical %               
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         LHI   RE,ACEIVCN                Clear invalid column number            
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         LHI   RE,1449                   Clear looping errors                   
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         LHI   RE,1451                                                          
         ST    RE,LWPARM                                                        
         BASR  RE,RF                                                            
*                                                                               
         SR    R3,R3                                                            
         MVI   ERRCOL#,0                                                        
         ICM   R3,1,TOT#COLS                                                    
         BZ    CHKCOL90            NO COLUMNS                                   
         MHI   R3,CSEQLNQ                                                       
         LA    R3,TABOFCOL(R3)                                                  
         ST    R3,LASTNTRY         FIGURE OUT END OF TABLE AND SAVE             
*                                                                               
         LA    R3,TABOFCOL         POINT TO FIRST ENTRY TO PROCESS              
CHKCOL10 C     R3,LASTNTRY         ARE WE FINISHED?                             
         BNL   CHKCOL60            YES                                          
         L     R2,AIOAREA3                                                      
         XC    0(2,R2),0(R2)                                                    
         TM    CSEQIND,CSEQEQU     EQUATION TYPE                                
         BZ    CHKCOL50            NOT AN EQUATION SO SKIP                      
         MVC   0(1,R2),CSEQNUM     SAVE OFF COLUMN NUMBER PROCESSING            
*                                  R8 = # OF ENTRIES (LEVELS) IN AIO3           
         LA    R8,1                FIRST ENTRY (LEVEL)                          
         ST    R3,CHKCOLR3         BASE R3 FOR CHKCOL IN CASE OF ERROR          
         SR    RF,RF               VALIDATE VERTICAL PERCENT                    
         ICM   RF,1,CSEQVPCT       DID CURRENT COL CALC HAVE A V%?              
         BZ    CHKCOL15            NO, CONTINUE                                 
         BCTR  RF,0                                                             
         SR    R6,R6                                                            
         LA    RF,CSEQCOL1(RF)                                                  
         ICM   R6,1,0(RF)          GET COLUMN NUMBER OF V%                      
         BZ    CHKCOLE1            ERROR FOR V%                                 
         BCTR  R6,0                                                             
         MHI   R6,CSEQLNQ                                                       
         LA    R6,TABOFCOL(R6)     POINT TO COL DATA FROM EQUATION              
         TM    NXT.CSEQIND,CSEQAMT+CSEQEQU                                      
         BNZ   CHKCOLE1            ERROR FOR V%                                 
*                                                                               
CHKCOL15 SR    R4,R4                                                            
         IC    R4,1(,R2)           WORK ON CSEQCOL1 THRU CSEQCOL4               
         CLM   R4,1,=AL1(3)        NO MORE TO PROCESS, 4 MAX                    
         BH    CHKCOL40            IF > 3 THEN THE COLS MUST OF BEEN OK         
         LA    R4,CSEQCOL1(R4)                                                  
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,1,0(R4)          GET COLUMN IT POINT TO                       
         BZ    CHKCOL40            IF = 0 THEN THE COLS MUST OF BEEN OK         
         BCTR  R6,0                                                             
         MHI   R6,CSEQLNQ                                                       
         LA    R6,TABOFCOL(R6)     POINT TO COL DATA FROM EQUATION              
*                                                                               
         TM    NXT.CSEQIND,CSEQAMT IS IT AN AMOUNT?                             
         BO    CHKCOL45            THIS IS OK, CAN'T GO FURTHER HERE            
         TM    NXT.CSEQIND,CSEQEQU IS IT AN EQUATION?                           
         BO    CHKCOL28            TRYING TO ADD NONE DOLLAR COLUMN             
         SR    RF,RF                                                            
         ICM   RF,1,CSEQVPCT       DID CURRENT COL CALC HAVE A V% ?             
         BZ    CHKCOLE2            NO, SO ERROR                                 
*        CLM   RF,1,NXT.CSEQNUM    IS  IT THIS COLUMN                           
         LA    R4,CSEQCOL1(RF)     ->  COLUMN THAT HAS THE VERTICAL %           
         BCTR  R4,0                SUB 1 SINCE V% COL SEQ # IS A 1-4            
         CLC   0(1,R4),NXT.CSEQNUM IS  NEXT COLUMN THE V% COLUMN ?              
         BE    CHKCOL45            THIS IS OK, CAN'T GO FURTHER HERE            
         B     CHKCOLE2            NOT A DOLLAR COLUMN, ERROR                   
*                                                                               
CHKCOL28 LR    R0,R8               NUMBER OF LEVELS TO CHECK                    
         L     R5,AIOAREA3         CHECK FOR LOOPING                            
CHKCOL30 CLC   NXT.CSEQNUM,0(R5)                                                
         BE    CHKCOLE3            LOOP ERROR                                   
         LA    R5,2(,R5)           NEXT ENTRY IN AIO3                           
         BCT   R0,CHKCOL30                                                      
*                                                                               
         LA    R8,1(,R8)           R8 = ADD A LEVEL                             
         LA    R2,2(,R2)           NEXT SPOT IN WORKAREA                        
         XC    0(2,R2),0(R2)       CLEAR                                        
         MVC   0(1,R2),NXT.CSEQNUM SAVE  COLUMN NUMBER IN NODE                  
*                                                                               
*                                  ****  SWITCH R3 ****                         
         LR    R3,R6               START LOOKING HERE NOW                       
         B     CHKCOL15            START CHECK THESE COLUMNS                    
*                                                                               
*                                  ****  RESTORE R3 ONE LEVEL ****              
CHKCOL40 SHI   R8,1                R8 = REMOVE A LEVEL                          
         BNP   CHKCOL50            MUST BE FINISHED WITH THIS COLUMN            
         SHI   R2,2                BUMP BACK ONE ENTRY IN AIO3                  
         SR    R3,R3                                                            
         IC    R3,0(,R2)           GET COLUMN NUMBER PROCESSING                 
         BCTR  R3,0                                                             
         MHI   R3,CSEQLNQ                                                       
         LA    R3,TABOFCOL(R3)     POINT TO COL DATA FROM EQUATION              
*                                                                               
CHKCOL45 SR    R4,R4                                                            
         IC    R4,1(,R2)           GET  INDEX INTO CSEQCOL1                     
         LA    R4,1(,R4)           BUMP UP TO LOOK AT NEXT COLUMN               
         STC   R4,1(,R2)           SAVE NEW NUMBER                              
         B     CHKCOL15                                                         
*                                                                               
CHKCOLE1 DS    0H                  INVALID VERTICAL %                           
         MVC   TEMPMNUM,=AL2(ACEVERT)                                           
         B     CHKCOLEM                                                         
*                                                                               
CHKCOLE2 DS    0H                  INVALID COLUMN NUMBER                        
         MVC   TEMPMNUM,=AL2(ACEIVCN)                                           
         B     CHKCOLEM                                                         
*                                                                               
CHKCOLEM DS    0H                  ADD  ERROR TO MESSAGE NUMBER TABLE           
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',TEMPMNUM),                    X        
               (CSEQNUM,AIOAREA1),('MNOMTERR',0),0                              
         B     CHKCOLEX                                                         
*                                                                               
CHKCOLE3 DS    0H                  LOOPING ERROR                                
*                                  FLAG  THE 'TO'   COLUMN NUMBER               
*                                  PASS  THE 'FROM' COLUMN NUMBER               
         MVC   TEMPMNUM,=AL2(1449)                                              
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',TEMPMNUM),                    X        
               (CSEQNUM,AIOAREA1),('MNOMTERR',0(R5)),(1,MNOMDCOL)               
*                                  FLAG  THE 'FROM' COLUMN NUMBER               
*                                  PASS  THE 'TO'   COLUMN NUMBER               
         MVC   TEMPMNUM,=AL2(1451)                                              
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',TEMPMNUM),                    X        
               (0(R5),AIOAREA1),('MNOMTERR',CSEQNUM),(1,MNOMDCOL)               
*                                                                               
CHKCOLEX L     R3,CHKCOLR3         RESET R3                                     
*                                                                               
CHKCOL50 LA    R3,CSEQLNQ(,R3)     NEXT ENTRY                                   
         B     CHKCOL10                                                         
*                                                                               
CHKCOL60 CLI   DOWNOPT,YES         DOWNLOAD OPTION                              
         BE    CHKCOL70            YES, SO DON'T CARE ABOUT WIDTH               
         SR    R0,R0                                                            
         IC    R0,TOT#COLS                                                      
         SR    R6,R6               R6 =  WIDTH OF REPORT                        
         SR    RF,RF                                                            
*                                                                               
         LA    R3,TABOFCOL         POINT TO FIRST ENTRY TO PROCESS              
CHKCOL62 ICM   RF,1,CSEQWDTH       COLUMN WIDTH                                 
         BZ    CHKCOL68                                                         
         LA    R6,1(RF,R6)         ADD IN WIDTH                                 
CHKCOL68 LA    R3,CSEQLNQ(,R3)     NEXT ENTRY                                   
         BCT   R0,CHKCOL62                                                      
         LA    R6,1(,R6)           ONE OTHER SIDE OF BOX                        
*                                                                               
         CHI   R6,MAXRPTWD         REPORT TOO WIDE ?                            
         BH    CHKCOLE4            YEP                                          
*                                                                               
CHKCOL70 LA    R3,COLSORT#         CHECK SORT OVER-RIDE SEQUENCE                
         LA    RF,MAXCOLS                                                       
         LA    R1,1                                                             
*                                                                               
*        NOTE: COLSORT# HAS THE FOLLOWING FORMAT:                               
*                                                                               
*              *         0  * COLUMN-NUMBER . . . 0             0               
*              º         º  º             º       º  FOR EACH   º               
*              º INITIAL º  º  FOR EACH   º       º   UNUSED    º               
*              º__ENTRY__º  º_SORT-NUMBER_º       º_SORT-NUMBER_º               
*                                                                               
CHKCOL72 CLC   0(1,R3),2(R3)       MUST BE CONTIGUOUS NULL OR C'*'              
         BE    CHKCOL74                                                         
         CLI   0(R3),C'*'          WAS  IT MARKED?                              
         BE    CHKCOL74            YES, SO OKAY                                 
*                                                                               
         STM   RE,R1,SAVEREGS      SAVE REGISTERS                               
*                                  INVALID SORT SEQUENCE                        
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACEIVSS)),               X        
               (3(R3),AIOAREA1),('MNOMTERR',0),0                                
*                                                                               
         LM    RE,R1,SAVEREGS      RESTORE REGISTERS                            
*                                                                               
CHKCOL74 LA    R3,2(,R3)           NEXT COLUMN IN ARRAY                         
         LA    R1,1(,R1)           COUNT THE COLUMNS                            
         BCT   RF,CHKCOL72                                                      
         B     CHKCOL90                                                         
*                                                                               
CHKCOLE4 MVC   ERRCOL#,STSEQ       REPORT TOO WIDE                              
         MVC   FVMSGNO,=AL2(ACERTWD)                                            
         CVD   R6,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  FVXTRA(3),APDUB                                                  
*                                                                               
CHKCOL90 CLC   FVMSGNO,=AL2(FVFOK)                                              
         XMOD1 ,                   RETURN                                       
         DROP  NXT                                                              
         DROP  R3                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DELETE ANY EXTRA WARNING/ERROR MESSAGES                            *         
*                                                                     *         
*  INPUT AREA:                                                        *         
*    IOAREA1  - RECORD  AREA                                          *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT (IN    APELEM)                        *         
*    APBYTE   - SWITCH  FOR      FND   DELETE                         *         
***********************************************************************         
         SPACE 1                                                                
         USING MNOELD,R1           MESSAGE ELEMENT                              
DELXTRM  NMOD1 0,***DLXM***        DELETE  EXTRA   MESSAGE ELEMENTS             
         L     RC,APALOCAL         ->      LWSD                                 
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APBYTE,X'00'        CLEAR   SWITCH                               
         MVI   APELCODE,MNOELQ     GET     MESSAGE ELEMENTS                     
         GOTO1 GETEL               GET     THE     FIRST   ELEMENT              
         BNE   DELXTREX            NONE,   RETURN                               
*                                                                               
DELXTR10 DS    0H                  CHECK   THE     ELEMENT                      
         CLC   MNOSEQ,TOT#COLS     PAST    TOTAL   NUMBER  OF   COLS ?          
         BNH   DELXTR20            NO,     SKIP                                 
         CLI   MNOSTYPE,MNOSTCOL   COLUMN  SEQ     NUMBER  TYPE ?               
         BNE   DELXTR20            NO,     SKIP                                 
         MVI   MNOEL,X'FF'         DELETE  EXTRA   ELEMENT                      
         MVI   APBYTE,X'01'        SAY     FOUND   EL.S    TO   DELETE          
*                                                                               
DELXTR20 DS    0H                                                               
         GOTO1 NEXTEL              GET     NEXT    ELEMENT                      
         BE    DELXTR10            FOUND,  CHECK   IT                           
         CLI   APBYTE,X'00'        ANY     EXTRA   ELEMENTS ?                   
         BE    DELXTREX            NO,     RETURN                               
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,X'FF'      DELETE  EXTRA   ELEMENTS                     
         GOTO1 DELEL                                                            
*                                                                               
DELXTREX DS    0H                                                               
         XMOD1 ,                   RETURN                                       
         DROP  R1                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DELETE WARNING MESSAGE ELEMENT(S)                                  *         
*                                                                     *         
*  PARAMETERS:                                                        *         
*    PARM 1 BYTE  0   COLUMN NUMBER OR 0 MEANING ALL COLUMNS          *         
*           BYTES 2-3 TWO BYTE MESSAGE NUMBER                         *         
*                                                                     *         
*  INPUT AREA:                                                        *         
*    IOAREA1  - RECORD  AREA                                          *         
*                                                                     *         
*  USES:                                                              *         
*    SVELEM   - SAVE    AREA     FOR   APELEM                         *         
*    WARNMTYP - MESSAGE TYPE                                          *         
*    WARNMNUM - MESSAGE NUMBER                                        *         
*    WARNCOL  - COLUMN  NUMBER   OR    ZERO                           *         
*    WARNSW   - FLAG -  X'80' =  DELEL REQUIRED                       *         
*    APELEM   - ELEMENT BUILD    AREA                                 *         
*    APELCODE - MESSAGE ELEMENT (IN    APELEM)                        *         
*    ELEMSEQ  - COLUMN  NUMBER                                        *         
*    SAVECOL# - SAVE    AREA     FOR   COLUMN   NUMBER                *         
***********************************************************************         
         SPACE 1                                                                
         USING MNOELD,R1           MESSAGE ELEMENT                              
WARNP    USING MNOMLN,R2           MESSAGE NUMBER  PORTION                      
         SPACE 1                                                                
DELWARN  NMOD1 0,**DELW**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE    APELEM                               
         MVC   WARNCOL,0(R1)       SAVE    COLUMN  NUMBER                       
         MVC   WARNMNUM,2(R1)      SAVE    MESAGE NUMBER                        
         MVC   WARNMTYP,4(R1)      SAVE    MESSAGE TYPE                         
*                                  CLEAR   FLAG                                 
         NI    WARNSW,TURNOFF-WARNDREQ                                          
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,MNOELQ     GET     MESSAGE ELEMENT                      
         MVC   ELEMSEQ,WARNCOL     USE     COLUMN  NUMBER                       
         GOTO1 GETEL               FIND    MESSAGE ELEMENT                      
         B     DELWPRC             PROCESS MESSAGE ELEMENT                      
*                                                                               
DELWNEXT DS    0H                  FIND    NEXT    MESSAGE ELEMENT              
         GOTO1 NEXTEL              FIND    IT                                   
*                                                                               
DELWPRC  DS 0H                                                                  
         BNE   DELWARNX            ELEMENT NOT     FOUND,  EXIT                 
         CLI   MNOSTYPE,MNOSTCOL   COLUMN  TYPE    ELEMENT ?                    
         BNE   DELWNEXT            NO,     GET     NEXT    ELEMENT              
         CLI   WARNCOL,0           DELETE  ONE     COLUMN  ONLY ?               
         BE    DELW05              YES,    FOUND   GOOD    ELEMENT              
         CLC   MNOSEQ,WARNCOL      FOUND   RIGHT   COLUMN  NUMBER               
         BNE   DELWARNX            NO,     EXIT                                 
*                                                                               
DELW05   DS    0H                                                               
         LA    R2,MNOLN1Q(,R1)     SET     UP      WARNP                        
         LA    R6,MNOLN1Q          OFFSET  FROM    START   OF   ELEMENT         
         SR    R3,R3               CLEAR   REGISTER                             
         LR    R4,R1               ->      MESSAGE ELEMENT                      
         ZIC   RF,MNOLN            MESSAGE ELEMENT LENGTH                       
         AR    R4,RF               ->      NEXT    ELEMENT                      
*                                                                               
DELW10   DS    0H                  RIGHT   MESSAGE FOUND ?                      
*                                  CHECK   FOR     MESSAGE TYPE                 
         CLC   WARNMTYP,WARNP.MNOMTYPE                                          
         BNE   DELW20              NO,     GET     NEXT    MESSAGE ID           
*                                  RIGHT   MESSAGE NUMBER ?                     
         CLC   WARNMNUM,WARNP.MNOMNUM                                           
         BE    DELW30              YES,    DELETE  THE     MESSAGE              
*                                                                               
DELW20   DS    0H                                                               
         IC    R3,WARNP.MNOMLN     GET     MESSAGE LENGTH                       
         AR    R2,R3               ->      NEXT    MESSAGE                      
         AR    R6,R3               OFFSET  FROM    START   OF   ELEMENT         
         CR    R2,R4               FOUND   NEXT    MESSAGE                      
         BL    DELW10              YES,    CHECK   IT                           
         B     DELW80              NOT     FOUND,  PROCESS NEXT ELEMENT         
*                                                                               
DELW30   DS    0H                  FOUND   MESSAGE NUMBER                       
         MVC   SAVECOL#,MNOSEQ     SAVE    COLUMN  NUMBER                       
*                                  ONLY    ONE     MESSAGE IN ELEMENT ?         
         ZIC   RF,MNOMLN           GET     TEXT    LENGTH                       
         LA    RF,MNOLN1Q(,RF)     PLUS    ELEMENT HEADER  LENGTH               
         CLM   RF,1,MNOLN          SAME    AS      ELEMENT LENGTH ?             
         BNE   DELW40              NO,     PROCESS MANY    MESSAGES             
         MVI   MNOELD,X'FF'        YES,    DELETE  ELEMENT                      
         OI    WARNSW,WARNDREQ     TURN    ON      DELEL   REQUIRED             
         B     DELW80              PROCESS NEXT    ELEMENT                      
*                                                                               
DELW40   DS    0H                  MANY    MSGS    IN      ELEMENT              
         ZIC   R8,WARNP.MNOMLN     SAVE    MESSAGE LENGTH                       
         XC    APELEM,APELEM       CLEAR   APELEM                               
         ZIC   RE,MNOLN            LENGTH  OF      ELEMENT                      
         BCTR  RE,R0               MINUS   ONE     FOR     EXECUTE              
         EX    RE,DELWMVCE         MOVE    ELEMENT TO      APELEM               
         LA    R2,APELEM           ->      APELEM                               
         ZIC   RF,MNOLN            GET     MESSAGE ELEMENT LENGTH               
         LA    R4,0(RF,R2)         ->      PAST    ELEMENT INFORMATION          
         SR    RF,R8               MINUS   LENGTH  OF      MESSAGE              
*                                  FIX     MESSAGE LENGTH                       
         STC   RF,MNOLN-MNOELD(,R2)                                             
         AR    R2,R6               ->      CURRENT MESSAGE NUMBER               
         LA    R3,0(R8,R2)         ->      NEXT    MESSAGE NUMBER               
*                                                                               
NEXTMSG  USING MNOMLN,R3           MESSAGE NUMBER  PORTION                      
*                                                                               
DELW50   DS    0H                  MOVE    NEXT    MESSAGE INFORMATION          
*                                          TO      CURRENT MESSAGE              
         CR    R3,R4               AT      PAST    MESSAGE INFORMATION?         
         BNL   DELW55              NO,     DONE                                 
         ZIC   R6,NEXTMSG.MNOMLN   LENGTH  OF      NEXT    MESSAGE              
         BCTR  R6,R0               MINUS   ONE     FOR     EXECUTE              
         EX    R6,DELWMVCM         MOVE    NEXT    MESSAGE INFORMATION          
         EX    R6,DELWCLRM         CLEAR   NEXT    MESSAGE INFORMATION          
         LA    R2,1(R6,R2)         UPDATE  CURRENT MESSAGE ADDRESS              
         LA    R3,1(R6,R3)         UPDATE  NEXT    MESSAGE ADDRESS              
         B     DELW50              PROCESS NEXT    MESSAGE                      
*                                                                               
DELW55   DS    0H                                                               
         MVI   MNOELD,X'FF'        DELETE  ELEMENT                              
*                                  TURN    OFF     DELEL   REQUIRED             
         NI    WARNSW,TURNOFF-WARNDREQ                                          
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE  X'FF'   ELEMENTS                     
         GOTO1 DELEL                                                            
         MVI   APELCODE,MNOELQ     RESTORE ELEMENT CODE                         
*                                                                               
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL               ADDEL   UPDATED MESSAGE ELEMENT              
*                                  SINCE   THIS    ELEMENT IS   SHORTER         
*                                          THERE   IS      NO   NEED            
*                                          TO      CHECK   FOR  ERRORS          
*                                                                               
         CLI   WARNCOL,0           DELETE  ONE     ELEMENT ONLY                 
         BNE   DELWARNX            YES,    EXIT                                 
*                                                                               
*                                  POINT   BACK    TO      SAME ELEMENT         
         MVI   APELCODE,MNOELQ     GET     MESSAGE ELEMENT                      
         MVC   ELEMSEQ,SAVECOL#    CURRENT COLUMN  NUMBER                       
         GOTO1 GETEL               FIND    MESSAGE ELEMENT                      
*                                                                               
DELW60   DS    0H                                                               
         CLI   MNOSTYPE,MNOSTCOL   COLUMN  TYPE    ELEMENT ?                    
         BE    DELW80              YES,    GET     NEXT    ELEMENT              
         GOTO1 NEXTEL              TRY     NEXT    ELEMENT                      
         B     DELW60              CHECK   FOR     SAME    ELEMENT              
*                                                                               
DELW80   DS    0H                  PROCESS NEXT    ELEMENT                      
         CLI   WARNCOL,0           DELETE  ONE     ELEMENT ONLY                 
         BE    DELWNEXT            NO,     FIND    NEXT    ELEMENT              
*        B     DELWARNX            YES,    EXIT                                 
*                                                                               
DELWARNX DS    0H                  EXIT    CODE                                 
         TM    WARNSW,WARNDREQ     IS      DELEL   REQUIRED ?                   
         BZ    DELWARXX            NO,     EXIT                                 
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE  X'FF'   ELEMENTS                     
         GOTO1 DELEL                                                            
*                                                                               
DELWARXX DS    0H                                                               
         MVC   APELEM,SVELEM       RESTORE APELEM                               
         XMOD1 ,                   RETURN                                       
*                                                                               
DELWMVCE MVC   APELEM(0),MNOEL     MOVE    ELEMENT TO      APELEM               
*                                  MOVE    NEXT    MESSAGE TO                   
*                                          CURRENT MESSAGE                      
DELWMVCM MVC   WARNP.MNOMLN(0),NEXTMSG.MNOMLN                                   
*                                  CLEAR   NEXT    MESSAGE                      
DELWCLRM XC    NEXTMSG.MNOMLN(0),NEXTMSG.MNOMLN                                 
         DROP  R1,WARNP,NEXTMSG                                                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CHECK MESSAGE ELEMENTS FOR ERROR TYPE MESSAGES                     *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT (IN    APELEM)                        *         
*    ELEMSEQ  - COLUMN  NUMBER                                        *         
*    SVMSGNO  - SAVE    FVMSGNO                                       *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    CONDITION CODE SET:                                              *         
*      EQUAL     - 'E' TYPE MESSAGE FOUND                             *         
*      NOT EQUAL - 'E' TYPE MESSAGE NOT   FOUND                       *         
***********************************************************************         
         SPACE 1                                                                
         USING MNOELD,R1           MESSAGE ELEMENT                              
WARNP    USING MNOMLN,R2           MESSAGE NUMBER  PORTION                      
         SPACE 1                                                                
CKMSGERR NMOD1 0,**CMNO**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVMSGNO,FVMSGNO     SAVE    FVMSGNO                              
         SR    R6,R6               NO      ERRORS  FOUND                        
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,MNOELQ     GET     MESSAGE ELEMENT                      
         GOTO1 GETEL               FIND    MESSAGE ELEMENT                      
         B     CKMSG20             CHECK   MESSAGE ELEMENT                      
*                                                                               
CKMSG10  DS    0H                  FIND    NEXT    MESSAGE ELEMENT              
         GOTO1 NEXTEL              FIND    IT                                   
*                                                                               
CKMSG20  DS    0H                                                               
         BNE   CKMSGEX             ELEMENT NOT     FOUND,  EXIT                 
         CLI   MNOSTYPE,MNOSTCOL   ELEMENT TYPE =  COLUMN ?                     
         BE    CKMSG30             YES,    TEST    MESSAGE TYPE                 
         CLI   MNOSTYPE,MNOSTROW   ELEMENT TYPE =  ROW    ?                     
         BNE   CKMSG10             NO,     GET     NEXT    ELEMENT              
*                                                                               
CKMSG30  DS    0H                                                               
         LA    R2,MNOLN1Q(,R1)     SET     UP      WARNP                        
         SR    R3,R3               CLEAR   REGISTER                             
         LA    R4,R1               ->      MESSAGE ELEMENT                      
         ZIC   RF,MNOLN            MESSAGE ELEMENT LENGTH                       
         AR    R4,RF               ->      NEXT    ELEMENT                      
*                                                                               
CKMSG40  DS    0H                  CHECK   FOR     MESSAGE TYPE ERROR           
         CLI   WARNP.MNOMTYPE,MNOMTERR                                          
         BNE   CKMSG50             NO,     GET     NEXT    MSG  ID              
         LA    R6,1                SAY     ERRORS  FOUND                        
         B     CKMSGEX             EXIT                                         
*                                                                               
CKMSG50  DS    0H                                                               
         IC    R3,WARNP.MNOMLN     GET     MESSAGE LENGTH                       
         AR    R2,R3               ->      NEXT    MESSAGE                      
         CR    R2,R4               FOUND   NEXT    MESSAGE                      
         BL    CKMSG40             YES,    CHECK   IT                           
         B     CKMSG10             NOT     FOUND,  PROCESS NEXT ELEMENT         
*                                                                               
CKMSGEX  DS    0H                                                               
         MVC   FVMSGNO,SVMSGNO     RESTORE FVMSGNO                              
         C     R6,=F'1'            SET     CONDITION       CODE                 
         XMOD1 ,                   RETURN                                       
         DROP  R1,WARNP                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GET NUMERIC INPUT - CONVERT FROM INPUT FORMAT TO BINARY            *         
*                                                                     *         
*  INPUT:                                                             *         
*    R1 = ADDRESS OF THE PARAMETER LIST                               *         
*                                                                     *         
*    INPUT  PARMLIST:                                                 *         
*       PARM1:                                                        *         
*         BYTE  0   X'00' = BYTES 1-3 CONTAINS THE ADDRESS OF A FIELD *         
*                           HEADER                                    *         
*                   ELSE  = LENGTH OF THE FIELD (LENGTH <= 11 SINCE   *         
*                           THE FIELD MAY BE SIGNED) IN THIS CASE     *         
*                           BYTES 1-3 CONTAINS THE ADDRESS OF THE     *         
*                           FIELD                                     *         
*         BYTES 1-3 ADDRESS OF THE FIELD OR                           *         
*                   ADDRESS OF THE FIELD HEADER                       *         
*                   (SEE BYTE 0)                                      *         
*                                                                     *         
*    OUTPUT PARMLIST:                                                 *         
*       PARM1:                                                        *         
*         BYTE  0   ERROR  INDICATOR                                  *         
*                    0 = GOOD VALUE                                   *         
*                    4 = OVERFLOW                                     *         
*                    8 = NOT  NUMERIC                                 *         
*                   16 = BAD  PARAMETER                               *         
*       PARM2:                                                        *         
*         BYTES 0-3 BINARY VALUE OF INPUT FIELD                       *         
*                                                                     *         
*       USES:                                                         *         
*         APWORK                                                      *         
*         APBYTE                                                      *         
*         APDUB                                                       *         
***********************************************************************         
         SPACE 1                                                                
GETNUM   NMOD1 0,**GETN**                                                       
         L     RC,APALOCAL                                                      
         L     R2,0(,R1)           ->    THE  FLD  OR   FLD  HDR                
         LA    R2,0(,R2)           REMOVE     HIGH ORDER     BYTE               
         SR    R3,R3               CLEAR REGISTER                               
         CLI   0(R1),X'00'         FIELD LNG  IN   BYTE 0 ?                     
         BE    GETNUM03            YES,  SKIP                                   
         CLI   0(R1),GETNMAXD      INPUT SIZE >    11 ?                         
         BH    GETNUMER            YES,  INVALID   INPUT                        
         IC    R3,0(,R1)           GET   FIELD     LENGTH                       
         B     GETNUM06            CONTINUE                                     
*                                                                               
GETNUM03 DS    0H                                                               
         CLI   5(R2),GETNMAXD      INPUT DATA SIZE >    11 ?                    
         BH    GETNUMER            YES,  INVALID   INPUT                        
         IC    R3,5(,R2)           GET   DATA LENGTH                            
         LA    R2,8(,R2)           ->    FIELD                                  
*                                                                               
*                                  AT    THIS TIME                              
*                                        R2 = ADDR OF   FIELD                   
*                                        R3 = LNG  OF   FIELD                   
GETNUM06 DS    0H                                                               
         AR    R2,R3               FIND  LAST BYTE                              
         BCTR  R2,0                                                             
         LA    R5,APWORK+GETNMXM1  POINT LAST CHARACTER OF WORK AREA            
         MVI   APWORK,C'0'         INITIALISE WORK AREA TO C'0'S                
         MVC   APWORK+1(GETNMAXD-1),APWORK                                      
         MVI   APBYTE,X'00'        CLEAR SWITCHES                               
*                                                                               
GETNUM10 DS    0H                  CHECK THIS CHARACTER                         
         CLI   0(R2),X'40'         SPACE ?                                      
         BNH   GETNUM20            YES,  CHECK     STARTED                      
         CLI   0(R2),C'+'          PLUS  SIGN ?                                 
         BE    GETNUM30            YES,  VALIDATE  SIGN                         
         CLI   0(R2),C'-'          MINUS SIGN ?                                 
         BE    GETNUM30            YES,  VALIDATE  SIGN                         
         TM    APBYTE,GETNEND      FIELD ENDED ?                                
         BO    GETNUMNG            YES,  NOT  NUMERIC                           
         CLI   0(R2),C'0'          IS    IT   NUMERIC ?                         
         BL    GETNUMNG                                                         
         CLI   0(R2),C'9'                                                       
         BH    GETNUMNG                                                         
*                                  NUMERIC                                      
         OI    APBYTE,GETNDIG      SAY   FOUND     DIGIT                        
         MVC   0(1,R5),0(R2)       SAVE  THIS BYTE                              
         BCTR  R5,0                PREV  SAVE BYTE                              
         B     GETNUM40            GET   PREV CHARACTER                         
*                                                                               
GETNUM20 DS    0H                  FOUND      SPACE                             
         TM    APBYTE,GETNDIG      ANY   DIGITS    FOUND ?                      
         BZ    GETNUM25            NO,   SKIP                                   
         OI    APBYTE,GETNEND      YES,  FIELD     ENDED                        
         B     GETNUM40            GET   PREV CHARACTER                         
*                                                                               
GETNUM25 DS    0H                  SPACE FOUND,    CHK  IF   SIGN FOUND         
         TM    APBYTE,GETNSIGN     ANY   SIGN FOUND ?                           
         BO    GETNUMNG            YES,  NOT  NUMERIC                           
         B     GETNUM40            GET   PREV CHARACTER                         
*                                                                               
GETNUM30 DS    0H                  FOUND C'+' OR   C'-'                         
*                                  END   OF   INPUT     FOUND                   
         TM    APBYTE,GETNEND+GETNSIGN   OR   SIGN      FOUND ?                 
         BNZ   GETNUMNG            YES,  NOT  NUMERIC                           
         OI    APBYTE,GETNSIGN     TURN  ON   FOUND     SIGN                    
         TM    APBYTE,GETNDIG      DIGIT FOUND ?                                
         BZ    *+8                 NO,   SKIP                                   
         OI    APBYTE,GETNEND      TURN  ON   END  OF   INPUT                   
         CLI   0(R2),C'-'          MINUS SIGN ?                                 
         BNE   *+8                 NO,   SKIP                                   
         OI    APBYTE,GETNNEG      TURN  ON   MINUS                             
*                                                                               
GETNUM40 DS    0H                  GET   PREV CHARACTER                         
         BCTR  R2,0                BUMP  TO   PREVIOUS                          
         BCT   R3,GETNUM10         TEST  PREV CHARACTER                         
*                                                                               
*                                  END   OF   INPUT                             
         TM    APBYTE,GETNDIG      DIGIT FOUND ?                                
         BZ    GETNUMNG            NO,   NOT  NUMERIC                           
*                                  TURN  OFF  BITS 2,3 (X'CN')                  
         NI    APWORK+GETNMXM1,X'CF'                                            
         TM    APBYTE,GETNNEG      MINUS SIGN FOUND ?                           
         BZ    *+8                 NO,   SKIP                                   
*                                  TURN  ON   MINUS    (X'DN')                  
         OI    APWORK+GETNMXM1,X'10'                                            
*                                  PACK  THE  NUMBER                            
         PACK  APDUB,APWORK(GETNMAXD)                                           
*                                  MAX   PACKED    NUMBER                       
         CP    APDUB,=P'2147483647'                                             
         BH    GETNUMOV            HIGH, OVERFLOW                               
*                                  MIN   PACKED    NUMBER                       
         CP    APDUB,=P'-2147483648'                                            
         BL    GETNUMOV            LOW,  OVERFLOW                               
         CVB   R2,APDUB            CONVERT    TO   BINARY                       
         ST    R2,4(,R1)           SAVE  THE  NUMBER                            
         SR    RF,RF               GOOD  RETURN    CODE                         
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMOV DS    0H                  OVERFLOW                                     
         LA    RF,4                RETURN     CODE                              
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMNG DS    0H                  NOT   NUMERIC                                
         LA    RF,8                RETURN     CODE                              
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMER DS    0H                  OVERFLOW                                     
         LA    RF,16               RETURN     CODE                              
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMEX DS    0H                  RETURN     TO   CALLER                       
         STC   RF,0(,R1)           STORE RETURN    CODE                         
         XMOD1 ,                   EXIT                                         
         DROP  RC                                                               
         EJECT ,                                                                
**********************************************************************          
*        EQUATES FOR BITS IN APBYTE                                             
**********************************************************************          
GETNDIG  EQU   X'80'               DIGIT FOUND                                  
GETNEND  EQU   X'40'               END   OF   VALUE     FOUND                   
GETNSIGN EQU   X'08'               SIGN  FOUND                                  
GETNNEG  EQU   X'04'               MINUS FOUND                                  
*                                                                               
GETNMAXD EQU   11                  MAX   NUM  OF   DIGITS                       
GETNMXM1 EQU   GETNMAXD-1          MAX   NUM  OF   DIGS MINUS 1                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*              DSECT FOR LOCAL WORKING STORAGE                        *         
***********************************************************************         
LWSD     DSECT                                                                  
FULL     DS    F                                                                
MAXPARM  EQU   12                                                               
MAXLDG   EQU   10                  MAXIMUM NUMBER OF LEDGERS                    
*                                  MAXIMUM NUMBER OF COLUMNS                    
MAXDISP  EQU   (FRMFUTXH-FRMCOL1H)/COLLNQ                                       
*                                                                               
SAVEREGS DS    4A                  SAVE REGISTERS                               
SAVER1   DS    A                   SAVE AREA FOR R1                             
DEFENTRY DS    A                   CURRENT ENTRY FOR DEFTAB                     
PARENTRY DS    A                   CURRENT ENTRY FOR PARMTAB                    
CHKCOLR3 DS    A                   SAVE R3 FOR CHKCOL IN CASE OF ERROR          
LASTNTRY DS    A                                                                
SAV@RHLP DS    A                   SAVE CUR@RHLP                                
SVACURSR DS    A                   SAVE ADDR OF CURRENT CURSOR                  
SVBILCOL DS    A                   SAVE ADDR OF BILLT COLUMN FIELD (UK)         
SVDAYCOL DS    A                   SAVE ADDR OF TSDAY COL. FLD (UK)             
LWPARM   DS    6A                  LOCAL PARAMETER LIST                         
ADELWARN DS    A                                                                
*                                                                               
JOBRNUM  DS    H                                                                
*                                                                               
TEMPOWN  DS    CL8                 TEMPORAY STORAGE FOR OWNER                   
LEDGTYPH DS    CL8                                                              
LEDGTYP  DS    CL2                                                              
DUMFLDH  DS    CL8                 DUMMY FIELD HEADER                           
DUMFLD   DS    CL12                DUMMY FIELD                                  
ERRCOL#  DS    XL1                 COLUMN NUMBER IN ERROR                       
DELCOL#  DS    XL1                 CURRENT COLUMN DELETED                       
INSCOL#  DS    XL1                 CURRENT COLUMN INSERTED                      
COLINSRT DS    XL1                 COL INSERTED TO COMPARE WITH ERRCOL#         
LSTCOL#  DS    XL1                 LAST COLUMN OR NUMBER OF COLUMNS             
TOT#COLS DS    XL1                 TOTAL NUMBER OF COLUMNS ON FORMAT            
TOT#ROWS DS    XL1                 TOTAL NUMBER OF ROWS ON FORMAT               
NEWELEM  DS    CL1                 YES/NO                                       
CURWIDTH DS    XL1                 REPORT WIDTH                                 
TYPEIND  DS    AL1                                                              
HAVE5TH  DS    CL1                 HAVE RESKS5TH RECORD (YES/NO)                
DOWNOPT  DS    CL1                 YES/NO                                       
PAGEROW  DS    AL1                 REPAGEING ON ROW NUMBER                      
TYPECOL  EQU   X'80'                                                            
TYPEROW  EQU   X'40'                                                            
TYPEVRT  EQU   X'20'                                                            
TYPEPARM DS    AL1                                                              
TYPECDE  DS    CL4                                                              
MIXESTK  DS    XL1                 MIXED ESTIMATE KEYWORDS INDICATOR            
*                                                                               
KWDINDS  DS    XL1                 KEYWORD INDICATOR                            
KWDDRBIL EQU   X'80'               DR,BILL                                      
KWDBILLT EQU   X'40'               BILLT                                        
KWDTSDTE EQU   X'20'               TSDTE                                        
KWDTSDAY EQU   X'10'               TSDAY                                        
*                                                                               
DWNTYPE  DS    XL1                 DOWNLOAD SCREEN INDICATOR                    
DWNTACNT EQU   X'80'               ACCENT ENABLED                               
DWNTQREP EQU   X'40'               QUICK REPORT ENABLED                         
*                                                                               
FRMRPCDE DS    CL(L'REPCODE)                                                    
FRMRPTYP DS    CL(L'REPSTYPE)                                                   
FRMRPLNQ EQU   *-FRMRPCDE                                                       
*                                                                               
PARM_N   DS    CL1                 CURRENT PARAMETER WORKING ON                 
MOASTR   DS    CL4                 YYMM      MOA START                          
MOAEND   DS    CL4                 YYMM      MOA END                            
STRDTE   DS    CL6                 YYMMDD    START DATE                         
ENDDTE   DS    CL6                 YYMMDD    END DATE                           
ACTSDTE  DS    CL6                 YYMMDD    ACTIVITY START                     
ACTEDTE  DS    CL6                 YYMMDD    ACTIVITY END                       
TEMPDATE DS    CL6                                                              
RQDATE   DS    XL3                 LAST REQUESTED DATE PACKED                   
ONEXONLY DS    AL1                 FOR  ONE  TIME PASS FLAG (Y/N)               
STRROW#  DS    AL1                                                              
COL#     DS    AL1                                                              
CURTYP#  DS    AL1                                                              
NROWS    DS    AL1                                                              
NDECIMAL DS    AL1                                                              
MIDLNFG  DS    AL1                 WAS  MIDLINE   USED ?    (Y/N)               
USEDTYPE DS    AL1                 FLAG IF   PARAMETER  USED YET                
ORANKON  DS    CL2                 OLD  RANK ON   IF   ANY                      
RANKON   DS    CL2                 RANK ON        IF   ANY                      
RANKCOL  DS    AL1                 RANK COLUMN    IF   ANY                      
SECLVL   DS    XL1                                                              
MULTLDG  DS    AL4                 MULTIPLE  LEDGER    BIT  NUMBERS             
TEMPLDG  DS    AL4                 TEMPORARY LEDGER    BIT  NUMBER              
ERRORMSG DS    CL60                                                             
*                                                                               
SVRCLOPT DS    XL1                 SAVE OLD  RCLOPT                             
SVRCLOP2 DS    XL1                 SAVE OLD  RCLOPT2                            
SVRCLSPC DS    XL1                 SAVE OLD  RCLSPCL                            
SVRCLHD1 DS    CL(L'RCLNHDL1)      SAVE OLD  RCLNHDL1                           
SVRCLHD2 DS    CL(L'RCLNHDL2)      SAVE OLD  RCLNHDL2                           
*                                                                               
TEMP1    DS    XL1                 TEMP BYTE 1                                  
TEMP2    DS    XL1                 TEMP BYTE 2                                  
*                                                                               
COLUMN#  DS    XL1                 COLUMN    NUMBER                             
HLPREPT  DS    XL1                 HELP REPORT    TYPE                          
SAVECOL# DS    XL1                 SAVE AREA FOR  COLUMN    NUMBER              
FIXSEQ#  DS    CL1                 SAVE AREA FOR  FIX  UP   MNOSEQ              
PASTESEQ DS    XL1                 PASTE     SEQUENCE  NUMBER                   
SVSVINSQ DS    XL1                 SAVE THE  SAVE INSERT    SEQ  NUMBER         
*                                                                               
MSGELSW  DS    XL1                 MESSAGE   ELEMENT   SWITCH                   
MSGELFND EQU   X'80'               MESSAGE   ELEMENT   FOUND                    
MSGELERR EQU   X'40'               MESSAGE   ELEMENT   ERROR                    
MSGELINS EQU   X'08'               MESSAGE   ELEMENT   INSERT                   
MSGELDEL EQU   X'04'               MESSAGE   ELEMENT   DELETE                   
MSGELIDR EQU   X'02'               MESSAGE   ELEMENT   IN   DELETE  RTN         
*                                                                               
TEMPMNUM DS    AL2                 TEMPORARY MESSAGE   NUMBER                   
TEMPMTYP DS    CL1                 TEMPORARY MESSAGE   TYPE                     
TEMPDATA DS    CL2                 TEMPORARY MESSAGE   DATA                     
*                                                                               
*                                  FOR  ADDWARN   AND       DELWARN             
WARNCOL  DS    XL1                 .    COLUMN    NUMBER    OR   ZERO           
WARNMNUM DS    AL2                 .    MESSAGE   NUMBER                        
WARNMTYP DS    CL1                 .    MESSAGE   TYPE                          
WARNADAT DS    XL4                 .    ADDRESS   MESSAGE   DATA                
WARNDTYP DS    XL1                 .    MESSAGE   DATA      TYPE                
WARNDLNG DS    XL1                 .    MESSAGE   DATA      LENGTH              
WARNSW   DS    XL1                 .    SWITCH                                  
WARNDREQ EQU   X'80'                    ..        DELEL     REQUIRED            
*                                                                               
*                                  FOR  DELAYED   SCREEN    MESSAGES            
DSRNCOL  DS    XL1                 .    COLUMN    NUMBER                        
DSRNMNUM DS    AL2                 .    MESSAGE   NUMBER                        
DSRNMTYP DS    XL1                 .    MESSAGE   TYPE                          
DSRNDTYP DS    XL1                 .    MESSAGE   DATA      TYPE                
DSRNDLNG DS    XL1                 .    MESSAGE   DATA      LENGTH              
DSRNDATA DS    CL20                .    MESSAGE   DATA                          
*                                                                               
         DS    0F                  FOR  DISPLAY   RECORD    ROUTINE             
DRCURSOR DS    XL(L'FVADDR)        .    SAVE      CURSOR    POINTER             
DRMSGNO  DS    XL(L'FVMSGNO)       .    SAVE      MESSAGE   NUMBER              
*                                                                               
BLOCK    DS    (MAXPARM)CL32       DATA BLOCK     FOR  SCANNER                  
*                                  AN  ARRAY OF COLUMNS CALULATION COLS         
TABOFCOL DS    (MAXCOLS)XL(CSEQLNQ)                                             
COLSORT# DS    CL(2*(MAXCOLS+1))   OVER-RIDE SORT, 1 THRU N, CONTIGUOUS         
COLARRY2 DS    CL(MAXCOLS)         ONE BYTE ARRAY TO CHECK CALCULATIONS         
*                                                                               
ELEMENT  DS    CL(L'APELEM)                                                     
*                                                                               
SVKEY    DS    CL(L'IOKEY)                                                      
ELEMSAVE DS    XL(L'APELEM)                                                     
SVELEM   DS    CL(L'APELEM)        SAVE APELEM                                  
SVELEM1  DS    CL(L'APELEM)        SAVE APELEM                                  
LWSX     DS    0C                                                               
         EJECT ,                                                                
HEADD    DSECT                                                                  
HEADFLD  DS    AL4                 DISPLACEMENT TO FIELD HEADER                 
HEADTYPE DS    AL1                 TYPE OF HEADER                               
HEADSEQ  DS    AL1                 SEQUENCE NUMBER                              
HEADLN   DS    AL1                 LENGTH OF FIELD -1                           
HEADLNQ  EQU   *-HEADD                                                          
         SPACE  2                                                               
HEADSCRD DSECT                     HEADER AREA ON THE SCREEN                    
HEADTXTH DS    CL8                 TEXT HEADER                                  
HEADTXT  DS    CL10                TEXT                                         
HEADDATH DS    CL8                 DATA HEADER                                  
HEADDAT  DS    CL25                DATA                                         
HEADSCRQ EQU   *-HEADSCRD          LENGTH OF SCREEN AREA                        
         SPACE  2                                                               
ROWD     DSECT                                                                  
ROWNUMH  DS    CL8                                                              
ROWNUM   DS    CL3                                                              
ROWDATAH DS    CL8                                                              
ROWDATA  DS    CL12                                                             
ROWTYPEH DS    CL8                                                              
ROWTYPE  DS    CL1                                                              
ROWTOTLH DS    CL8                                                              
ROWTOTL  DS    CL1                                                              
ROWPRFXH DS    CL8                                                              
ROWPRFX  DS    CL14                                                             
ROWLNQ   EQU   *-ROWD                                                           
         SPACE  2                                                               
COLD     DSECT                                                                  
COLNUMH  DS    CL8                 HEADER FOR NUMBER                            
COLNUM   DS    CL3                 COLUMN NUMBER                                
COLDATAH DS    CL8                 HEADER FOR DATA                              
COLDATA  DS    CL12                DATA                                         
COLDATEH DS    CL8                 HEADER DATE PARAMETER                        
COLDATE  DS    CL10                DATE PARAMETER                               
COLWIDEH DS    CL8                 HEADER FOR WIDTH                             
COLWIDE  DS    CL2                 WIDTH OF COLUMN                              
COLTOTLH DS    CL8                 HEADER TO TOTAL FIELD                        
COLTOTL  DS    CL1                 TOTAL ON THIS COLUMN? (Y/N)                  
COLPRNTH DS    CL8                 HEADER TO HIDDEN COLUMN FLAG                 
COLPRNT  DS    CL1                 HIDDEN COLUMN (Y/N)                          
COLLNQ   EQU   *-COLD                                                           
         EJECT ,                                                                
LISTD    DSECT                                                                  
LISTACTH DS    CL8                 HEADER FOR NUMBER                            
LISTACT  DS    CL3                 SELECTION FIELD                              
LISTDATH DS    CL8                 HEADER FOR DATA                              
LISTDAT  DS    CL74                DATA                                         
         ORG   LISTDAT                                                          
         DS    CL2                                                              
LISTTYP  DS    CL3                 TYPE                                         
         DS    CL2                                                              
LISTFMT  DS    CL8                 FORMAT                                       
         DS    CL2                                                              
LISTNME  DS    CL36                NAME                                         
         DS    CL2                                                              
LISTOWN  DS    CL8                 OWNER                                        
         DS    CL2                                                              
LISTADTE DS    CL9                 ACTIVITY DATE                                
         ORG                                                                    
LISTLNQ  EQU   *-LISTD                                                          
LIST2ND  DS    CL8                 HEADER FOR SELECT FIELD 2ND LINE             
         DS    CL3                 SELECTION FIELD                              
LISTXDH  DS    CL8                 HEADER FOR EXTRA DATA                        
LISTXD   DS    CL74                EXTRA DATA DISPLAY                           
LISTLN2Q EQU   *-LISTD                                                          
                                                                                
         EJECT ,                                                                
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
*              DSECT FOR HEADLINE DEFINITIONS                                   
*                                                                               
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCREFD                                                       
         EJECT ,                                                                
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF9D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047ACSCR0E   09/03/15'                                      
         END                                                                    
