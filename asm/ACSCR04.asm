*          DATA SET ACSCR04    AT LEVEL 030 AS OF 04/11/19                      
*PHASE T60C04C                                                                  
*&&ONLIN SET   Y                                                                
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 26 AS OF 08/05/13         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 024 10NOV11 PR000122 ESTIMATE PHASE 1                                    
* SMAN 025 16DEC11 PR002242 UK/US MERGE                                         
* JFOS 026 04JAN13 PR003402 ESHRAT/C KWORDS NEED CAC OR CAN IN FORMAT           
*                                                                               
* GHOA 030 11APR19 SPEC-28645 CHANGE BRANCHES (BEST PRACTICE)                   
*                                                                               
         TITLE 'COLUMN ELEMENTS MAINTAINANCE'                                   
T60C04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C04,RA,R9,RR=RE                                              
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
         L     R2,=A(EXITRTN)                                                   
         A     R2,APRELO                                                        
         ST    R2,AEXITRTN                                                      
         L     R2,=A(FLDTAB)       RELOCATE TABLE ADDRESS                       
         A     R2,APRELO                                                        
         ST    R2,AFLDTAB                                                       
         MVI   REPMODE,REPCOL                                                   
         MVC   FVXTRA,SPACES                                                    
         MVI   DSRNMTYP,C' '       CLEAR DELAYED SCREEN  MESSAGE TYPE           
         XC    DSRNMNUM,DSRNMNUM   CLEAR DELAYED SCREEN  MESSAGE NUMBER         
         XC    DSRNDATA,DSRNDATA   CLEAR DELAYED SCREEN  MESSAGE DATA           
         MVI   MSGELSW,0           CLEAR MESSAGE ELEMENT SWITCH                 
         MVI   WARNSW,0            CLEAR WARNING MESSAGE SWITCH                 
         MVC   DRMSGNO,=AL2(FVFOK) CLEAR DISPLAY ERROR   NUMBER                 
         XC    DRCURSOR,DRCURSOR   CLEAR DISPLAY CURSOR                         
         MVI   PASTESEQ,0          CLEAR PASTE   INSERT  SEQ NUMBER             
*                                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR100                                                           
         MVC   SVREPJCL,APREPJCL                                                
         SR    RF,RF                                                            
         LA    R4,COLTAGH          SEE IF SCREEN IS BUILT ?                     
SCR022   ICM   RF,1,0(R4)          LENGTH TO NEXT FIELD                         
         BZ    SCR030              END OF SCREEN (NOT BUILT)                    
         TM    1(R4),FVAXTND       HAS FIELD # ?                                
         BZ    SCR028                                                           
         LR    R1,RF                                                            
         SHI   R1,8                LENGTH OF EXTENDED HEADER                    
         AR    R1,R4               R1 = START OF EXTENDED HEADER                
         CLI   0(R1),SFDCOLN       DID WE FIND COLUMN #1                        
         BE    SCR040              YES, SCREEN IS BUILT                         
SCR028   AR    R4,RF               BUMP TO NEXT FIELD                           
         B     SCR022              LOOP                                         
*                                                                               
SCR030   MVI   COL#FRST,0                                                       
         GOTO1 =A(BLDSCR),RR=APRELO                                             
*                                                                               
SCR040   GOTO1 =A(SETSCR),RR=APRELO                                             
         EJECT ,                                                                
***********************************************************************         
*  CHECK PFKEYS FOR INSERT/DELETE                                     *         
***********************************************************************         
         SPACE 1                                                                
SCR100   CLI   APACTN,ACTCHA       ARE WE CHANGING RECORD?                      
         BNE   SCR600                                                           
         CLI   APMODE,APMVALK      ONLY VALIDATE PFKEYS IN VALKEY               
         BNE   SCR600                                                           
         CLI   APPFKEY,PFKDEL      DELETE COLUMN?                               
         BE    *+8                                                              
         CLI   APPFKEY,PFKINS      INSERT COLUMN?                               
         BNE   SCR420                                                           
         CLI   FXDCURSR,YES        IS CURSOR ON A COLUMN LINE ?                 
         BE    IVALPFK             IT IS BUT WE PUT IT THERE                    
         B     SCR600                                                           
*                                                                               
SCR420   CLI   APPFKEY,PFKUP       SCROLL UP?                                   
         BE    SCR450                                                           
         CLI   APPFKEY,PFKDOWN     SCROLL DOWN?                                 
         BE    SCR450                                                           
         TM    SCROPTH+4,FVITHIS   ANY INPUT?                                   
         BZ    SCR600                                                           
*                                                                               
SCR450   TM    TWAMODE,TWAMLSM     IN LIST/SELECT MODE?                         
         BO    SCR600                                                           
         XC    SAVRECK,SAVRECK     CLEAR TO FORCE APMDISR MODE AGAIN            
*                                                                               
         USING RESRECD,R2                                                       
SCR600   MVC   RESETSEQ,STSEQ      START AT COLUMN ELEMENT SEQUENCE             
         CLI   APMODE,APMVALR      VALREC    ?                                  
         BE    *+8                 YES, TEST CURSOR                             
         CLI   APMODE,APMDISR      DISREC    ?                                  
         BNE   SCR900              NO,  DO   NOT  TEST CURSOR                   
*                                                                               
         CLI   APPFKEY,PFK13       GOING TO FORMAT SCREEN ?                     
         BE    *+8                                                              
         CLI   APPFKEY,PFK04       ARE WE TRYING TO SEE COLUMN FILTER?          
         BNE   SCR750                                                           
         CLI   FXDCURSR,YES        DID WE FORCE IT THERE ?                      
         BE    SCR900              YES, DO NOT SET COLUMN#                      
         ZIC   R1,STSEQ            INITIALIZE TO FIRST COLUMN                   
         ZIC   RF,CRSRCOL#         CURRENT LINE # CURSOR IS ON                  
         AR    R1,RF                                                            
         BCTR  R1,0                                                             
*                                                                               
SCR730   STC   R1,COLUMN#                                                       
         L     R4,ACURCOL                                                       
         ZIC   RF,0(,R4)                                                        
         AR    R4,RF                                                            
         ST    R4,APCURSOR                                                      
         B     SCR900                                                           
*                                                                               
SCR750   CLI   APPFKEY,PFKHLP      HELP PF   KEY  ?                             
         BNE   SCR800              NO,  SKIP                                    
         MVI   SVINSSEQ,0          CLEAR     SAVE INSSEQ                        
         CLI   APACTN,ACTCHA       ARE  WE   UPDATING  THE  RECORD ?            
         BE    SCR755              YES, CONTINUE                                
         CLI   APACTN,ACTADD       ARE  WE   ADDING    A    RECORD ?            
         BNE   SCR800              NO,  SKIP                                    
*                                                                               
SCR755   DS    0H                                                               
*                                                                               
         L     R4,ACURCOL          ->   CURRENT CURSOR LINE                     
         ZIC   RF,0(,R4)                                                        
         AR    R4,RF                                                            
         ST    R4,APCURSOR         SET  CURSOR    TO   FRONT OF LINE            
         GOTO1 AFVAL,(R4)          ANY  DATA ?                                  
         BE    SCR900              YES, SKIP                                    
*                                                                               
SCR762   XC    CUR@RHLP,CUR@RHLP   NO,  ENABLE    PASTE                         
         MVC   CUR@RHLP+3(1),CRSRCOL# SAVE COLUMN NUMBER NOT DISP               
         MVC   SVINSSEQ,INSSEQ     SAVE INSSEQ                                  
*                                                                               
         CLI   INSSEQ,0            ARE  WE   IN   AN    INSERT ?                
         BNE   SCR900              YES, CONTINUE                                
         ZIC   R1,STSEQ            BASE COLUMN ON SCREEN                        
         ZIC   RF,CRSRCOL#         CURRENT LINE # CURSOR IS SET TO              
         AR    R1,RF                                                            
         BCTR  R1,0                                                             
*                                                                               
SCR780   STC   R1,SVINSSEQ         SET  INSERT    SEQUENCE  NUMBER              
*                                                                               
SCR800   DS    0H                                                               
         EJECT ,                                                                
SCR900   LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELCLEL             DELETE COLUMN ELEMENT'S ONLY                 
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     CPYCLEL             COPY COLUMN ELEMENT'S                        
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
EXIT     GOTO1 AEXITRTN                                                         
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE KEY AND INITIALIZE SOME DATA                              *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   NEWKEY,NO           RESET TO NO                                  
         MVI   HAVE5TH,NO                                                       
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,COLCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    COLCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY            ENTER KEY                                    
         MVC   COLCODE,SAVFORM                                                  
         OI    COLCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         TM    ACLFMIND,ACLFMIFK   1ST PASS FOR ACTION COPY                     
         BZ    *+10                NO                                           
         MVC   SAVEKEY(L'RESKEY),RESKEY                                         
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
*                                                                               
         LA    R1,IORD+IOACCFIL+IO1                                             
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    VALKEY17                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY17                                                         
         LA    R1,IORD+IOACCFIL+IO1+IORDEL                                      
*                                                                               
VALKEY17 CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         L     RF,ACUCASTB         ->   UPPER CASE TRANSLATE TABLE              
         TR    COLCODE,0(RF)       TRANSLATE TO UPPER CASE                      
         OI    COLCODEH+6,FVOXMT   TRANSMIT                                     
         CLC   SAVCCODE,COLCODE    ARE WE WORKING ON THE SAME FORMAT?           
         BE    VALKEY19            YES, SO LEAVE WELL ENOUGH ALONE              
         MVC   SAVCCODE,COLCODE    SAVE NEW FORMAT CODE                         
         MVI   STSEQ,1             NEW FORMAT, SET TO START ON COL 1            
*                                                                               
VALKEY19 MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    VALKEY30                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY30                                                         
         TM    IOERR,IOEDEL        IS RECORD DELETED?                           
         BO    IVALCCDL            YES CAN'T COPY TO A DELETED RECORD           
         OI    APINDS,APIOKADD     TURN ON TO TRICK ACTION COPY                 
         B     VALKEY30                                                         
*                                                                               
VALKEY20 MVI   STSEQ,1                                                          
         TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         MVI   NEWKEY,YES          WE ADDING A NEW RECORD                       
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY30 LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKS5TH                                                 
         MVC   IOADDR,AIOAREA0                                                  
         GOTO1 AIO,IORD+IOACCFIL+IOLOCK                                         
         BNE   *+8                                                              
         MVI   HAVE5TH,YES                                                      
         LA    R2,IOKEY                                                         
         MVI   RESKSEQ,RESKSREG    RESTORE AS NORMAL                            
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   SVREPJCL,APREPJCL   NEED TO RE-BUILD SCREEN ?                    
         BE    VALKEY99                                                         
         MVI   COL#FRST,0                                                       
         GOTO1 =A(BLDSCR),RR=APRELO                                             
         GOTO1 =A(SETSCR),RR=APRELO                                             
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE COLUMN INPUT OR DELETE COLUMNS                            *         
***********************************************************************         
         SPACE 1                                                                
DELCLEL  DS    0H                  ONLY DELETE COLUMN ELEMENTS                  
*                                                                               
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,COLNMEH                                                    
         BNE   VALREC00            NAME HAS NOT BEEN INPUT                      
         GOTO1 ADDNAME,APPARM,(R2),COLNMEH    GET FORMAT NAME                   
         BNE   VALREC99            ON   ERROR, EXIT                             
*                                                                               
         USING RPFELD,R1                                                        
VALREC00 L     R1,AIOAREA1                                                      
*        MVI   SECLVL,0                                                         
         XC    RANKON,RANKON       INIT  RANK ON                                
         MVI   RANKCOL,0           INIT  RANK COL                               
         MVI   DOWNOPT,NO                                                       
*&&UK*&& MVI   KWDINDS,0           INIT  KEYWORD INDICATOR                      
         MVI   DWNTYPE,0                                                        
         MVI   MIXESTK,0                                                        
         MVI   APELCODE,RPFELQ     X'C4' PROFILE ELEMENT                        
         GOTO1 GETEL                                                            
         BNE   VALREC01                                                         
         MVC   RANKON,RPFRKON      SAVE RANK ON                                 
         MVC   RANKCOL,RPFRKCL     SAVE RANK COLUMN                             
         TM    RPFDNOPT,RPFDDOWN    DOWNLOADING ONLY?                           
         BZ    *+8                                                              
         MVI   DOWNOPT,YES                                                      
*                                                                               
         TM    RPFXMIT,RPFXACNT    ACCENT ENABLED ?                             
         BZ    *+12                                                             
         OI    DWNTYPE,DWNTACNT                                                 
         B     VALREC01                                                         
         TM    RPFXMIT,RPFXQREP    QREPORTS ENABLED ?                           
         BZ    *+8                                                              
         OI    DWNTYPE,DWNTQREP                                                 
*                                                                               
         USING RRWELD,R1                                                        
VALREC01 L     R1,AIOAREA1                                                      
         MVI   NROWS,0                                                          
         MVI   APELCODE,RRWELQ     ROW ELEMENT X'C2'                            
         GOTO1 GETEL               GET AN ELEMENT                               
         BNE   VALREC02            NO ROWS                                      
VREC01C  GOTO1 NEXTEL              FIND LAST ROW                                
         BE    VREC01C                                                          
         MVC   NROWS,RRWSEQ                                                     
*                                                                               
VALREC02 TM    APINDS,APIOKADD                                                  
         BZ    *+10                                                             
         MVC   RESKEY,APRECKEY                                                  
         DROP  R2                                                               
*                                                                               
         USING RCLELD,R1                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     X'C3' COLUMN ELEMENT                         
         MVI   LSTCOL#,0           SET TO ZERO                                  
         GOTO1 GETEL                                                            
         BNE   VALREC03            NONE ON RECORD                               
VREC02C  GOTO1 NEXTEL                                                           
         BE    VREC02C                                                          
         MVC   LSTCOL#,RCLSEQ                                                   
*                                                                               
VALREC03 CLI   APPFKEY,PFKINS      INSERT COLUMN?                               
         BNE   VALREC04                                                         
         CLI   LSTCOL#,MAXCOLS     IS IT LAST COLUMN ?                          
         BE    IVALCMAX            CAN NOT INSERT PAST LAST COLUMN              
*                                                                               
VALREC04 CLI   APACTN,ACTDEL       DELETE ONLY                                  
         BNE   VALREC05                                                         
         SR    R1,R1                                                            
         ICM   R1,1,LSTCOL#        LAST COLUMN NUMBER                           
         BZ    VALREC92            NO   COLUMNS, OUTPUT THE RECORD              
         OC    RANKON,RANKON       ANY  RANKING ?                               
         BNZ   IVALCRNK            YES, DELETE OF A RANKING FIELD               
*                                                                               
         L     RF,=A(DELCOLMN)                                                  
         A     RF,APRELO                                                        
*                                                                               
VALREC4C BASR  RE,RF               DELETE ALL COLUMNS                           
         BNE   VALREC99            ON ERROR, EXIT                               
         BCT   R1,VALREC4C         DELETE COLUMNS UNTIL R1=0                    
*                                                                               
         B     VALREC92            OUTPUT THE RECORD                            
*                                                                               
VALREC05 CLI   NEWKEY,YES          ADDING A NEW RECORD?                         
         BNE   VALREC06            NO                                           
********** ????                                                                 
*        MVC   APREPNUM,APREPUL+1  RESET TO SINGLE TYPE LEDGER                  
********** ????                                                                 
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON    ERROR, EXIT                            
*                                                                               
         USING RFLELD,R1                                                        
         XC    APELEM,APELEM       BUILD LEDGER ELEMENT FILTER                  
         LA    R1,APELEM                                                        
         MVI   RFLEL,RFLELQ        X'C5' FILTER TYPE                            
         MVI   RFLLN,RFLLNQ+2                                                   
         MVI   RFLTYPE,RFLLDG      SET LEDGER ON RECORD                         
         MVC   RFLDATA,APREPUL     DEFAULT UNIT AND LEDGER                      
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON    ERROR, EXIT                            
***********************************************************************         
*  INSERT A SEQUENCE NUMBER IF PFKEY TO INSERT WAS USED               *         
***********************************************************************         
         SPACE 1                                                                
VALREC06 MVC   CURCOL,STSEQ        START SEQUENCING                             
         CLI   INSSEQ,0            WAS   A COLUMN INSERTED?                     
         BE    VALREC07            NO                                           
         SR    R1,R1                                                            
         IC    R1,INSSEQ                                                        
         GOTO1 =A(INSCOLMN),RR=APRELO                                           
         BNE   VALREC99            ON ERRROR, EXIT                              
*                                                                               
VALREC07 LA    R0,CLIMIT           NUMBER OF COLUMNS ON SCREEN                  
         L     R4,ALASTCOL         LAST COLUMN                                  
         SR    RE,RE               MAINTAIN COLUMN NUMBER                       
         IC    RE,CURCOL           LAST COLUMN NUMBER =                         
         AR    RE,R0                    CURCOL + CLIMIT - 1                     
         BCTR  RE,0                                                             
*                                                                               
VALREC09 CLI   APPFKEY,PFKDEL      ARE WE GOING TO DELETE A LINE                
         BNE   VALREC11            NO                                           
         CLI   FXDCURSR,YES        DID WE MOVE THE CURSOR TO A LINE ?           
         BE    IVALCOLF            YES, INVALID COLUMN NUMBER                   
         C     R4,ACURCOL          IS CURSOR ON THIS LINE ?                     
         BNE   VALREC10            NO                                           
         STC   RE,APBYTE           SAVE CURRENT COLUMN NUMBER                   
*                                  CHECK COLUMN RANKING FOR ERRORS              
         GOTO1 =A(CHKCRANK),RR=APRELO                                           
         BNE   VALREC99            ON ERROR, STOP THE DELETE                    
         SR    RF,RF                                                            
         IC    RF,0(,R4)                                                        
         AR    R4,RF               POINT TO DATA FIELD INPUT                    
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         SHI   RF,9                                                             
         EX    RF,*+8                                                           
         B     VALREC11            FINISHED                                     
         MVC   8(0,R4),SPACES      CLEAR FIELD                                  
*                                                                               
VALREC10 SH    R4,NEXTCOL          BUMP BACKWARDS                               
         BCTR  RE,0                                                             
         BCT   R0,VALREC09                                                      
                                                                                
*                                                                               
VALREC11 MVC   RESETSEQ,CURCOL     SET RESET VALUE TO START SEQUENCE            
         XC    COLSORT#,COLSORT#   STORE SORT SEQUENCE, INDEX = SORT#           
         MVI   COLSORT#,C'*'       INITIALIZE SORT SEQUENCE                     
         XC    COLARRY2,COLARRY2   TO STORE FLAGS                               
*                                                                               
         USING RCLELD,R8                                                        
VALREC12 XC    ELEMENT,ELEMENT                                                  
         LA    R8,ELEMENT                                                       
         L     R1,AIOAREA1                                                      
         MVI   NEWELEM,YES         SAY IT IS A NEW BUILT ELEMENT                
         MVI   APELCODE,RCLELQ     COLUMN ELEMENTS X'C3'                        
         MVC   ELEMSEQ,CURCOL      GET CUR ELEMENT WITH THAT SEQUENCE           
         MVC   SVHEAD#1,SPACES                                                  
         MVC   SVHEAD#2,SPACES                                                  
         MVI   SVRCLOPT,0          SAVE OLD RCLOPT                              
         MVI   SVRCLOP2,0          SAVE OLD RCLOPT2                             
         MVI   SVRCLOP4,0          SAVE OLD RCLOPT4                             
         MVI   SVRCLSPC,0          SAVE OLD RCLSPCL                             
         GOTO1 GETEL,(R1)          GET ELEMENT WITH SEQUENCE NUMBER             
         BNE   VALREC15            NO ELEMENT TO COPY FROM                      
         SR    RF,RF                                                            
         IC    RF,1(,R1)           GET ELEMENT LENGTH                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RCLEL(0),0(R1)      SAVE ELEMENT DATA                            
*                                  ERASE DATA THAT IS ALWAYS SET                
         MVC   SVRCLOPT,RCLOPT     SAVE  RCLOPT                                 
         MVC   SVRCLOP2,RCLOPT2    SAVE  RCLOPT2                                
         MVC   SVRCLOP4,RCLOPT4    SAVE  RCLOPT4                                
         MVC   SVRCLSPC,RCLSPCL    SAVE  RCLSPCL                                
         MVI   RCLDTEFG,0          DATE  PARAMETER INDICATOR                    
         MVI   RCLDATES,0          DATE  BASIS DATA                             
         XC    RCLSTDT,RCLSTDT     START DATE RANGE VALUES                      
         XC    RCLENDT,RCLENDT     END   DATE RANGE VALUES                      
*                                  SAVE OFF HEADINGS IN SAVE FIELDS             
         LA    RE,RCLNDATA         RE = POINT TO DATA                           
         IC    RF,RCLDATLN         LENGTH OF DATA                               
         AR    RE,RF               RE = POINT TO HEAD 1                         
         ICM   RF,1,RCLHD1LN       LENGTH OF HEADING 1                          
         BZ    VALRC12A                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVHEAD#1(0),0(RE)   SAVE OFF HEADING 1                           
         LA    RE,1(RE,RF)                                                      
*                                                                               
VALRC12A ICM   RF,1,RCLHD2LN       LENGTH OF HEADING 2                          
         BZ    VALRC12B                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVHEAD#2(0),0(RE)   SAVE OFF HEADING 2                           
*                                                                               
VALRC12B BAS   RE,ANYINPUT         DID WE INPUT DATA ?                          
         BNE   VALREC13            NO, NO DATA  INPUT ON THIS LINE              
*---------------------------------------------------------------------*         
*   NOTE: SINCE X'C9' ELEMENTS FOLLOW  X'C3' ELEMENTS, ANY DELETES    *         
*         WOULD NOT AFFECT THE FOLLOWING LOGIC                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
*                                  REMOVE  TRUNCATED WARNING IF ANY             
         GOTO1 =A(DELWARN),APPARM,                                     X        
               (CURCOL,=AL2(ACWTRUNC)),('MNOMTWRN',0),RR=APRELO                 
*                                  REMOVE  DELETED   COLUMN WARNINGS            
         GOTO1 =A(DELWARN),APPARM,                                     X        
               (CURCOL,=AL2(ACWCDELD)),('MNOMTWRN',0),RR=APRELO                 
         GOTO1 =A(DELWARN),APPARM,                                     X        
               (CURCOL,=AL2(ACWCSUD)),('MNOMTWRN',0),RR=APRELO                  
*                                  REMOVE  INVALID   PARAMETER ERROR            
*                                         (FROM ACSCR00)                        
         GOTO1 =A(DELWARN),APPARM,                                     X        
               (CURCOL,=AL2(ACEPRM)),('MNOMTERR',0),RR=APRELO                   
*                                                                               
VALREC13 L     R2,ACOLDATA         HEADER OF DATA FIELD                         
         LA    R2,8(,R2)           INPUT  OF DATA FIELD                         
         GOTO1 =A(MATCHON),APPARM,(RCLDATLN,RCLNDATA),(R2),RR=APRELO            
         BNE   *+8                                                              
         MVI   NEWELEM,NO          ELEMENT ALREADY EXIST SO SAY NO              
*                                                                               
VALREC15 L     R4,ACOLDATA                                                      
         GOTO1 AFVAL,(R4)          COLUMN DATA                                  
         BNE   VALREC78            IF NO DATA THEN DELETE THIS LINE             
*                                                                               
         CLI   NEWELEM,NO          NEW KEYWORD FOR THIS COLUMN#?                
         BE    VALREC17            NO                                           
         L     R1,AIOAREA1         YES, SO DELETE FILTERS                       
         MVI   APELCODE,RFLELQ     FILTER ELEMENTS X'C5'                        
         MVC   ELEMSEQ,CURCOL      DELETE COLUMN FILTERS FOR COL#               
         GOTO1 DELEL                                                            
*                                                                               
         ZIC   RF,CURCOL                                                        
         GOTO1 =A(FIX5TH),APPARM,(RF),0,RR=APRELO  DELETE ONLY                  
*                                                                               
         MVC   SVHEAD#1,SPACES     NEW ELEMENT SO DELETED VALUES SAVED          
         MVC   SVHEAD#2,SPACES                                                  
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
VALREC17 MVI   RCLEL,RCLELQ        ELEMENT CODE X'C3'                           
         MVC   RCLSEQ,CURCOL       SEQUENCE NUMBER                              
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RCLNDATA(0),FVIFLD      COLUMN DATA                              
         MVC   RCLDATLN,FVILEN                                                  
*                                                                               
         USING DEFTABD,R6                                                       
         XC    DEFENTRY,DEFENTRY                                                
         L     R4,ACOLDATA                                                      
         GOTO1 VALDEF,(R4)                                                      
         BNE   VALREC32            NOT IN TABLE                                 
*                                                                               
* SET DEFAULT VALUES FROM KEYWORD DEFINITION                                    
*                                                                               
         LR    R6,R1               R6 POINTS AT DEFINITION DATA                 
         ST    R6,DEFENTRY         SAVE ADDRESS                                 
         CLI   NEWELEM,YES         IS NEW ELEMENT ?                             
         BNE   VALREC18            NO SO USED OLD VALUES                        
         MVC   RCLOPT,DEFCLOTH             TURN ON COLUMN 1 INDICATORS          
         NI    RCLOPT,RCLACCM+RCLEQU       DEFAULT VALUES                       
         OI    RCLOPT,RCLNEWEL             MARK AS NEW ELEMENT TYPE             
         MVI   RCLOPT2,0                   CLEAR OPTION OUT                     
         OC    RCLOPT2,DEFCLIND            SET   DEFAULT VALUES ON              
         NI    RCLOPT2,CDE+NME+RCLJOBR                                          
         NI    RCLOPT4,TURNOFF-RCLAIDX                                          
         MVI   RCLSPCL,0                 RESET SPECIAL VALUE                    
         TM    DEFTYP2,DEFNESH                                                  
         BZ    *+8                                                              
         OI    RCLOPT5,RCLNJESR    SET NEW ESTIMATE HOURS/RATE KEYWORD          
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
         OI    RCLOPT4,RCLNJEST    NON-JOBBER ESTIMATE TYPE COLUMN              
*                                                                               
VALREC18 LA    RF,0                                                             
         TM    MIXESTK,DEFOEST+DEFNEST                                          
         BNM   *+8                                                              
         LA    RF,1                HAVE OLD *OR* NEW EST KEYWORD                
*                                                                               
         OC    MIXESTK,DEFTYP2                                                  
         TM    DEFTYP2,DEFCACR     TEST COL REQUIRES C/A KEYWORD                
         BZ    *+10                                                             
         MVC   FULL,ACOLDATA       SAVE CURSOR POS. IN CASE OF ERROR            
*                                                                               
         CHI   RF,0                TEST HAD OLD OR NEW EST KEYWORDS             
         BE    VALREC19            NO                                           
         TM    MIXESTK,DEFOEST+DEFNEST  TEST NOW HAVE OLD *AND* NEW             
         BNO   *+10                                                             
         MVC   FULL,ACOLDATA       SAVE CURSOR POS. FOR ERROR MSG               
*                                                                               
VALREC19 CLC   DEFDDNUM,=AL2(AC#RSPPD)   KEYWORD "PDAY" ?                       
         BNE   *+8                                                              
         MVI   RCLPDN1,1           SET DEFAULT TO ONE                           
*&&UK                                                                           
         TM    DEFCLOTH,DEFCLHED                                                
         BZ    *+8                                                              
         OI    KWDINDS,KWDSHEAD    ALWAYS SHOW HEADINGS                         
*                                                                               
         CLC   DEFDDNUM,=AL2(AC#RSDTE)   KEYWORD "TSDTE" ?                      
         BNE   *+8                                                              
         OI    RCLOPT4,RCLTSDTE                                                 
         CLC   DEFDDNUM,=AL2(AC#RSTSD)   KEYWORD "TSDAY" ?                      
         BNE   *+12                                                             
         OI    KWDINDS,KWDTSDAY                                                 
         ST    R3,SVDAYCOL               SAVE COLUMN FIELD                      
*                                                                               
         CLC   DEFDDNUM,=AL2(AC#BILLT)   KEYWORD "BILLT" ?                      
         BNE   *+14                                                             
         OI    KWDINDS,KWDBILLT                                                 
         MVC   SVBILCOL,ACOLDATA   SAVE COLUMN FIELD                            
*&&                                                                             
         MVC   RCLSPCL,DEFSPCL     SPECIAL KEYWORD                              
*        OC    SECLVL,DEFSEC#      SET SECURITY LEVEL FOR KEYWORD               
*                                                                               
         CLI   RCLDATES,0                                                       
         BNE   VALREC20                                                         
         CLI   DEFDATES,0          SET DATE BASIS DEFAULT                       
         BE    VALREC20                                                         
         MVI   RCLDATES,RCLTRDT    SET DEFAULT TRANSACTION DATE BASED           
         TM    DEFDATES,DEFTRDT    TRANSACTION BASIS ?                          
         BO    VALREC20            YES SO OK                                    
         MVI   RCLDATES,RCLMODT    SET DEFAULT TRANSACTION DATE BASED           
         TM    DEFDATES,DEFMODT    TRANSACTION BASIS ?                          
         BO    VALREC20            YES SO OK                                    
         DC    H'00'               SHOULD BE ONE OF THESE                       
*                                                                               
VALREC20 MVC   APPARM+8(2),=C',='                                               
         MVC   APPARM+10(1),SCCOMMA        GENERAL C','                         
         MVC   APPARM+11(1),APOPNPRN       GENERAL C'('                         
         GOTO1 VSCANNER,APPARM,ACOLDATA,(6,BLOCK)                               
         SR    R0,R0                                                            
         IC    R0,APPARM+4                                                      
         BCTR  R0,0                                                             
         STC   R0,CPARMS                                                        
         OC    RCLSPCL,RCLSPCL     SPECIAL KEYWORD?                             
         BZ    VALREC21                                                         
         GOTO1 VALSPCL,APPARM,(R6),('MAXCOLS',BLOCK),(CURCOL,COLARRY2)          
         BNE   VALREC99                                                         
*                                                                               
VALREC21 CLC   CPARMS,DEFCLMIN                                                  
         BL    IVALLOW             **ERROR** TOO FEW PARAMETERS                 
         CLC   CPARMS,DEFCLMAX                                                  
         BH    IVALHIGH            **ERROR** TOO MANY PARAMETERS                
         CLI   CPARMS,0            NO VALUES NEEDED                             
         BE    VALREC26            PARAMETER CHECK NEEDED, SO BRANCH            
         LA    R3,BLOCK+32         SKIP FIRST BLOCK (KEYWORD)                   
*                                                                               
*&&US*&& CLC   DEFDDNUM,=AL2(AC#RSUSC)   USER FIELD (SPECIAL) ?                 
*&&US*&& BE    VALREC23                  NO,  PROCESS PARAMETERS                
         CLC   DEFDDNUM,=AL2(AC#RSUSF)   USER FIELD (SPECIAL) ?                 
         BNE   VALREC24                  NO,  PROCESS PARAMETERS                
VALREC23 BAS   RE,VALRUF                 YES, VALIDATE USER FIELDS              
         CLC   FVMSGNO,=AL2(FVFOK)       ANY  ERRORS?                           
         BE    VALREC26                  NO,  CONTINUE                          
         B     VALREC99                  YES                                    
*                                                                               
VALREC24 GOTO1 VALPARM,APPARM,(R8),(CPARMS,BLOCK),DEFENTRY,0                    
         BNE   VALREC99                                                         
*&&UK                                                                           
         CLC   DEFDDNUM,=AL2(AC#RSDRA)   KEYWORD "DR" ?                         
         BNE   VALREC26                                                         
         CLI   RCLDATES,RCLTRDT          PARAMETER "BILL" ?                     
         BNE   VALREC26                                                         
         OI    KWDINDS,KWDDRBIL                                                 
*&&                                                                             
VALREC26 MVC   FVMSGNO,=AL2(FVFOK) RESET TO OK                                  
*                                                                               
         MVI   CPARMS,0                                                         
         L     R4,ACOLDTER                                                      
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC31                  NO INPUT                               
         CLC   DEFDDNUM,=AL2(AC#RSPPD)   PDAY KEYWORD                           
         BE    VALREC29                                                         
         TM    RCLOPT,RCLACCM      IS IT AN ACCUMULATED COLUMN?                 
         BZ    IVALINPT            NO, SO INVALID INPUT                         
*                                                                               
VALREC29 GOTO1 VSCANNER,APPARM,ACOLDTER,(4,BLOCK),SCNP3NEQ                      
         MVC   CPARMS,APPARM+4                                                  
*                                                                               
VALREC30 DS    0H                                                               
         CLC   DEFDDNUM,=AL2(AC#RSPPD)   PDAY KEYWORD                           
         BNE   VALREC31                                                         
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         CLI   CPARMS,1            ONLY ONE INPUT ALLOWED                       
         BH    VALREC99                                                         
         TM    4(R4),FVINUM        WAS IT NUMERIC INPUT ?                       
         BZ    IVALNNUM            NO,  NOT  NUMERIC                            
         LA    RE,BLOCK            SCAN BLOCK                                   
         MVC   FVMSGNO,=AL2(1767)                                               
         CLC   4(4,RE),=AL4(1)                                                  
         BL    VALREC99                                                         
         CLC   4(4,RE),=AL4(MAXPDAY#)                                           
         BH    VALREC99                                                         
         MVC   RCLPDN1,7(RE)       MOVE IN BINARY                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALREC35                                                         
*                                                                               
VALREC31 BAS   RE,VALRANGE                                                      
         BE    VALREC35                                                         
         B     VALREC99                                                         
*----------------------------------------------------------------------         
*  SEE IF IT IS AN MATHEMATICAL EXPRESSION                                      
*----------------------------------------------------------------------         
         SPACE 1                                                                
VALREC32 LTR   R1,R1               WAS R1 SET AND CC WAS <>                     
         BNZ   IVALKYWD            YES,   KEYWORD IS NOT VALID                  
         GOTO1 =A(CHKEXP),RR=APRELO                                             
         BNE   VALREC99                                                         
*                                                                               
         MVI   RCLSPCL,0           RESET SPECIAL VALUE                          
         MVI   RCLOPT,RCLEQU+RCLNEWEL                                           
         MVI   RCLOPT2,0                                                        
         SR    R0,R0               CLEAR R0 FOR VALEXP                          
         SR    RF,RF                                                            
         IC    RF,RCLDATLN                                                      
         GOTO1 =A(VALEXP),APPARM,FVIFLD,(RF),RCLEL,RR=APRELO                    
         BNE   VALREC33            NOT AN EQUATION                              
         LTR   R0,R0               DID WE HAVE PAIRED PARENTHESIS ?             
         BNZ   IVALUBPR            NO,     UNBALANCED PARENTHESES               
         OI    RCLOPT,RCLACCM                                                   
         B     VALREC34                                                         
*----------------------------------------------------------------------         
*  SEE IF IT IS DATE KEYWORD MINUS ANOTHER DATE KEYWORD                         
*----------------------------------------------------------------------         
         SPACE 1                                                                
VALREC33 GOTO1 =A(DTECOMP),RR=APRELO   POSSIBLE DATE COMPUTATION                
         BNE   IVALEXP                 INVALID EXPRESSION                       
         MVI   RCLSPCL,RCLSPDTE        INDICATE DATE1-DATE2 COLUMN              
*                                                                               
VALREC34 L     R4,ACOLDTER         DATE RANGE FIELD                             
         GOTO1 AFVAL,(R4)          ANY DATA ?                                   
         BE    IVALINPT            YES, INVALID INPUT                           
*                                                                               
VALREC35 L     R4,ACOLWDTH         COLUMN WIDTH FIELD                           
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC37            NO DATA, SO SET DEFAULTS                     
         TM    4(R4),FVITHIS       INPUT FROM USER ?                            
         BO    VALREC38            YES, SO DON'T CHANGE                         
         CLI   NEWELEM,NO          IS   THIS A NEW  ELEMENT ?                   
         BE    VALREC36            NO,  CONTINUE                                
         CLI   APACTN,ACTADD       ACTION ADD ?                                 
         BNE   VALREC37            NO,  SO SET UP DEFAULTS                      
         L     RF,ACOLDATA         YES, ->  DATA FIELD                          
         TM    4(RF),FVITHIS       INPUT FROM USER ?                            
         BZ    VALREC38            NO,  SO DON'T CHANGE                         
         B     VALREC37            YES, SO SET UP DEFAULTS                      
*                                                                               
*                                  MAKE SURE NAME AND CODE ARE STILL            
*                                       THE  SAME                               
VALREC36 MVC   TEMP1,RCLOPT2       GET  CURR NAME AND CODE                      
         NI    TEMP1,RCLNAME+RCLCODE                                            
         MVC   TEMP2,SVRCLOP2      GET  OLD  NAME AND CODE                      
         NI    TEMP2,RCLNAME+RCLCODE                                            
         CLC   TEMP1,TEMP2         ARE  THEY THE  SAME ?                        
         BE    VALREC38            YES, USE  OLD  WIDTH                         
*                                  NO,  USE  DEFAULT  WIDTH                     
*                                                                               
VALREC37 MVI   RCLWDTH,13          DEFAULT WIDTH                                
         ICM   R6,15,DEFENTRY                                                   
         BZ    *+10                                                             
         MVC   RCLWDTH,DEFWDTH     USE TABLE ENTRY WIDTH, (CODE WIDTH)          
*                                                                               
         TM    RCLOPT2,RCLNAME     IF NAME ATTRIBUTE IS USED DEFAULT 36         
         BZ    VALREC39                                                         
         MVI   RCLWDTH,36                                                       
         LTR   R6,R6                                                            
         BZ    VALREC39                                                         
         CLI   DEFNWDTH,0          DEFAULT NAME WIDTH ZERO ?                    
         BE    *+10                YES, SKIP                                    
         MVC   RCLWDTH,DEFNWDTH    USE  DEFAULT NAME WIDTH                      
         B     VALREC39                                                         
         DROP  R6                                                               
*                                  CONVERT   WIDTH TO BINARY                    
VALREC38 GOTO1 =A(GETNUM),APPARM,ACOLWDTH,0,RR=APRELO                           
         CLI   APPARM,X'00'        VALID     INPUT ?                            
         BNE   IVALNNUM            NO,  NOT  NUMERIC                            
         TM    APPARM+4,X'80'      NEGATIVE  VALUE ?                            
         BO    IVALIGE0            YES, NOT  >=    0                            
         MVC   RCLWDTH,APPARM+7    SAVE COL  WIDTH                              
*                                                                               
VALREC39 ICM   R4,15,ACOLTOTL      TOTAL ON SCREEN ?                            
         BNZ   VALREC40            YES,  PROCESS INPUT                          
         CLI   NEWELEM,YES         WAS   IT  A NEW KEYWORD ?                    
         BE    VALREC45            YES,  SKIP                                   
         MVC   APBYTE,SVRCLOPT     GET   OLD RCLOPT                             
         NI    APBYTE,RCLSUPP+RCLNOTOT                                          
         OC    RCLOPT,APBYTE       RESTORE   TOTAL OPTION                       
                                                                                
         MVC   APBYTE,SVRCLOP2     GET   OLD RCLOPT2                            
         NI    APBYTE,RCLTOT                                                    
         OC    RCLOPT2,APBYTE      RESTORE   TOTAL OPTION                       
                                                                                
         MVC   APBYTE,SVRCLOP4                                                  
         NI    APBYTE,RCLAIDX                                                   
         OC    RCLOPT4,APBYTE                                                   
         B     VALREC45            CONTINUE                                     
*                                                                               
VALREC40 NI    RCLOPT,TURNOFF-RCLSUPP-RCLNOTOT                                  
         NI    RCLOPT2,TURNOFF-RCLTOT                                           
         NI    RCLOPT4,TURNOFF-RCLAIDX                                          
                                                                                
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC45            NO INPUT                                     
         TM    RCLOPT,RCLACCM      IS  IT A AMOUNT COLUMN?                      
         BZ    VALREC42            NO, SO CHECK OPTION FOR NON-$$               
         CLC   APONLY,FVIFLD       ONLY SHOW TOTALS NO DETAILS                  
         BNE   VALREC41                                                         
         OI    RCLOPT,RCLSUPP      SUPRESS DETAIL                               
         B     VALREC45                                                         
*                                                                               
VALREC41 CLI   FVIFLD,C'S'         ONLY  SHOW    DETAILS NO TOTALS ?            
         BNE   VALREC43            NO,   NOT A VALID OPTION USED                
         OI    RCLOPT,RCLNOTOT     SUPRESS TOTAL                                
         B     VALREC45            CONTINUE                                     
*                                                                               
VALREC42 CLC   APNO,FVIFLD         TOTAL = 'NO'  SPECIFIED ?                    
         BE    VALREC45            YES,  SKIP                                   
         CLI   FVIFLD,C'I'         ACCENT INDEXING ?                            
         BNE   *+12                                                             
         OI    RCLOPT4,RCLAIDX     YES                                          
         B     VALREC45                                                         
*                                                                               
         CLC   APYES,FVIFLD        TOTAL = 'YES' SPECIFIED ?                    
         BNE   VALREC43            NO,   PERFORM MORE  TESTS                    
         OI    RCLOPT2,RCLTOT      TURN  ON      INDICATOR TO TOTAL             
         B     VALREC45            CONTINUE                                     
*                                                                               
VALREC43 TM    4(R4),FVITHIS       ANY  NEW INPUT ?                             
         BO    IVALTOT             YES, INVALID TOTAL OPTION                    
         CLI   NEWELEM,YES         NEW  ELEMENT                                 
         BE    IVALTOT             YES, INVALID OPTION                          
*                                  NO,  IGNORE  LEFT  OVER DATA                 
VALREC45 ICM   R4,15,ACOLPRNT      PRINT ON  SCREEN ?                           
         BNZ   VALRC45A            YES,  PROCESS INPUT                          
         CLI   NEWELEM,YES         WAS   IT  A NEW KEYWORD ?                    
         BE    VALREC50            YES,  SKIP                                   
         MVC   APBYTE,SVRCLOPT     GET   OLD RCLOPT                             
         NI    APBYTE,RCLHIDE+RCLMERGE                                          
         OC    RCLOPT,APBYTE       RESTORE   TOTAL OPTION                       
                                                                                
         MVC   APBYTE,SVRCLOP2     GET   OLD RCLOPT2                            
         NI    APBYTE,RCLCZERO                                                  
         OC    RCLOPT2,APBYTE      RESTORE   TOTAL OPTION                       
         B     VALREC50                                                         
*                                                                               
VALRC45A NI    RCLOPT,TURNOFF-RCLHIDE-RCLMERGE                                  
         NI    RCLOPT2,TURNOFF-RCLCZERO                                         
*&&US*&& NI    RCLOPT4,TURNOFF-RCLXCOL                                          
                                                                                
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC50                                                         
         CLC   APYES,FVIFLD        PRINT COLUMN?                                
         BE    VALREC50            YES SO OK AS IS                              
         CLC   APNO,FVIFLD         HIDE COLUMN INFORMATION                      
         BNE   VALREC46                                                         
         OI    RCLOPT,HIDE         DON'T SHOW COLUMN                            
         B     VALREC50            YES SO OK AS IS                              
*                                                                               
VALREC46 DS    0H                                                               
*&&US                                                                           
         CLI   FVIFLD,C'X'        HIDE AND DON'T ADD TO COL TO SORT             
         BNE   VREC46A                                                          
         OI    RCLOPT,RCLHIDE                                                   
         OI    RCLOPT4,RCLXCOL                                                  
         B     VALREC50                                                         
*&&                                                                             
VREC46A  CLI   FVIFLD,C'M'        MERGE DATA IF SELECTED $COLS=ZERO             
         BNE   VALREC47                                                         
         OI    RCLOPT,RCLMERGE                                                  
         TM    RCLOPT,RCLACCM      MUST BE ACCUM COLUMN                         
         BZ    VALREC50                                                         
         OI    RCLOPT2,RCLCZERO                                                 
         B     VALREC50            YES SO OK AS IS                              
*                                                                               
VALREC47 TM    RCLOPT,RCLACCM      MUST BE ACCUM COLUMN                         
         BZ    IVALINPT                                                         
         CLI   FVIFLD,C'C'         IF COL=ZERO THEN ELIMINATE REC.              
         BNE   VALREC48                                                         
         OI    RCLOPT2,RCLCZERO                                                 
         B     VALREC50                                                         
*                                                                               
VALREC48 CLI   FVIFLD,C'H'         COL=ZERO AND HIDE COLUMN                     
         BNE   IVALINPT                                                         
         OI    RCLOPT,HIDE                                                      
         OI    RCLOPT2,RCLCZERO                                                 
*                                                                               
VALREC50 ICM   R4,15,ACOLSORD                                                   
         BZ    VALREC51                                                         
         MVI   RCLSORTN,0                                                       
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC51                                                         
         TM    RCLOPT,RCLACCM      IT IS AN ACCUMULATED COLUMN?                 
         BO    IVALSORT                                                         
*                                  CONVERT   SORT NUMBER TO BINARY              
         GOTO1 =A(GETNUM),APPARM,ACOLSORD,0,RR=APRELO                           
         CLI   APPARM,X'00'        VALID     INPUT ?                            
         BNE   IVALNNUM            NO,  NOT  NUMERIC                            
         TM    APPARM+4,X'80'      NEGATIVE  VALUE ?                            
         BO    IVALIGE0            YES, NOT  >=    0                            
         CLI   APPARM+7,0          IS   SORT NO.   0 ?                          
         BE    IVALIGT0            YES, NOT  >     0                            
         CLI   APPARM+7,MAXCOLS    NUM  >    NUM   OF   COLUMNS ?               
         BH    IVALCOLN            YES, INVALID    COLUMN    NUMBER             
         MVC   RCLSORTN,APPARM+7   SAVE SORT NUMBER                             
*                                                                               
VALREC51 ICM   R4,15,ACOLCSTK      ->   COL  STACK UNDER                        
         BZ    VALREC52                                                         
         MVI   RCLSTACK,0          INIT COL  STACK UNDER                        
         GOTO1 AFVAL,(R4)          ANY  COL  STACK UNDER      INPUT ?           
         BNE   VALREC52            NO,  SKIP                                    
         CLI   FVILEN,1            MORE THAN ONE   CHARACTER ?                  
         BNH   IVALINPT            NO,  INVALID    INPUT                        
         CLC   8(1,R4),APCOLCHR    COLUMN    CHARACTER ?                        
         BNE   IVALINPT            NO,  INVALID    INPUT                        
         CLI   9(R4),C'+'                                                       
         BE    IVALINPT                                                         
         ZIC   RF,FVXLEN           GET  EX   LENGTH                             
*                                  GET  COL  STACK UNDER      NUMBER            
         GOTO1 =A(GETNUM),APPARM,((RF),9(R4)),0,RR=APRELO                       
         CLI   APPARM,X'00'        VALID     INPUT ?                            
         BNE   IVALNNUM            NO,  NOT  NUMERIC                            
         TM    APPARM+4,X'80'      NEGATIVE  VALUE ?                            
         BO    IVALIGE0            YES, NOT  >=    0                            
         CLI   APPARM+7,0          IS   COL  STACK UNDER      NUM  =0 ?         
         BE    IVALIGT0            YES, NOT  >     0                            
         ZIC   R4,APPARM+7         SAVE COL  STACK UNDER      NUMBER            
         L     RF,ACOLNUM          GET  ADDR OF    COL  NUM   FIELD             
         GOTO1 =A(GETNUM),APPARM,('SFDCOLM1',9(RF)),0,RR=APRELO                 
         CLM   R4,1,APPARM+7       STACK     UNDER NUM  TOO   HIGH ?            
         BNL   IVALCOLN            YES, INVALID    COLUMN     NUMBER            
         STC   R4,RCLSTACK         SAVE IN   ELEMENT                            
*                                                                               
VALREC52 ICM   R4,15,ACOLDECM      ->   # OF DECIMAL PLACES                     
         BZ    VALREC53                                                         
                                                                                
         CLI   8(R4),C'('                                                       
         BE    VALREC53                                                         
         MVI   RCLDCMLS,0                                                       
*        MVI   RCLDCMLS,C' '                                                    
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC53                                                         
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC53                                                         
*                                                                               
VALR52D  L     R3,=A(DECMTAB)                                                   
         A     R3,APRELO                                                        
VALR52G  CLI   0(R3),X'FF'                                                      
         BE    IVALINPT                                                         
         CLC   FVIFLD(2),0(R3)                                                  
         BE    VALR52J                                                          
         CLC   FVIFLD(2),3(R3)                                                  
         BE    VALR52J                                                          
         LA    R3,DECMLEN(,R3)                                                  
         B     VALR52G                                                          
*                                                                               
VALR52J  MVC   RCLDCMLS,2(R3)                                                   
                                                                                
VALREC53 ICM   R4,15,ARPTCNSD      ->   REPEAT CONSTANT                         
         BZ    VALREC54                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC54                                                         
         TM    RCLOPT,RCLACCM                                                   
         BZ    VALREC54                                                         
         NI    RCLOPT3,X'FF'-RCLRDATY-RCLRDATN                                  
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC54                                                         
**       TM    RCLOPT,RCLACCM                                                   
**       BZ    IVALINPT                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC54                                                         
*                                                                               
VALR53A  CLC   FVIFLD(L'APNO),APNO                                              
         BNE   VALR53B                                                          
         OI    RCLOPT3,RCLRDATN                                                 
         B     VALREC54                                                         
VALR53B  CLC   FVIFLD(L'APYES),APYES                                            
         BNE   IVALINPT                                                         
         OI    RCLOPT3,RCLRDATY                                                 
*                                                                               
VALREC54 ICM   R4,15,APRTRDND      ->   PRINT REDUNDANT                         
         BZ    VALREC55                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC55                                                         
         TM    RCLOPT,RCLACCM                                                   
         BO    VALREC55                                                         
         NI    RCLOPT3,X'FF'-RCLRDATY-RCLRDATN                                  
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC55                                                         
**       TM    RCLOPT,RCLACCM                                                   
**       BO    IVALINPT                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC55                                                         
*                                                                               
VALR54A  CLC   FVIFLD(L'APNO),APNO                                              
         BNE   VALR54B                                                          
         OI    RCLOPT3,RCLRDATN                                                 
         B     VALREC55                                                         
*                                                                               
VALR54B  CLC   FVIFLD(L'APYES),APYES                                            
         BNE   IVALINPT                                                         
         OI    RCLOPT3,RCLRDATY                                                 
*                                                                               
VALREC55 ICM   R4,15,ACOLNGAM      ->   NEGATIVE AMOUNTS                        
         BZ    VALREC56                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC56                                                         
         NI    RCLEDOPT,X'FF'-RCLEDTRL-RCLEDBKT-RCLEDLED-RCLEDCR                
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC56                                                         
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC56                                                         
*                                                                               
VALR55B  CLI   FVIFLD,C'T'                                                      
         BNE   *+12                                                             
         OI    RCLEDOPT,RCLEDTRL                                                
         B     VALREC56                                                         
         CLI   FVIFLD,C'B'                                                      
         BNE   *+12                                                             
         OI    RCLEDOPT,RCLEDBKT                                                
         B     VALREC56                                                         
         CLI   FVIFLD,C'L'                                                      
         BNE   *+12                                                             
         OI    RCLEDOPT,RCLEDLED                                                
         B     VALREC56                                                         
         CLI   FVIFLD,C'S'                                                      
         BNE   IVALINPT                                                         
         OI    RCLEDOPT,RCLEDCR                                                 
*                                                                               
VALREC56 ICM   R4,15,ACOLPRCM      ->   PRINT COMMAS                            
         BZ    VALREC57                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC57                                                         
         NI    RCLEDOPT,X'FF'-RCLEDCMY-RCLEDCMN                                 
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC57                                                         
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC57                                                         
*                                                                               
VALR56A  CLC   FVIFLD(L'APNO),APNO                                              
         BNE   VALR56C                                                          
         OI    RCLEDOPT,RCLEDCMN                                                
         B     VALREC57                                                         
*                                                                               
VALR56C  CLC   FVIFLD(L'APYES),APYES                                            
         BNE   IVALINPT                                                         
         OI    RCLEDOPT,RCLEDCMY                                                
*                                                                               
VALREC57 ICM   R4,15,ACOLPRZR      ->   PRINT ZERO AMOUNTS                      
         BZ    VALREC58                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC58                                                         
         NI    RCLEDOPT,X'FF'-RCLEDZRY-RCLEDZRN                                 
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC58                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC58                                                         
*                                                                               
VALR57A  TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
         CLC   FVIFLD(L'APNO),APNO                                              
         BNE   VALR57C                                                          
         OI    RCLEDOPT,RCLEDZRN                                                
         B     VALREC58                                                         
*                                                                               
VALR57C  CLC   FVIFLD(L'APYES),APYES                                            
         BNE   IVALINPT                                                         
         OI    RCLEDOPT,RCLEDZRY                                                
*                                                                               
VALREC58 ICM   R4,15,ACOLUNLN      ->   UNDERLINE AMOUNTS                       
         BZ    VALREC59                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC59                                                         
         NI    RCLOPT3,X'FF'-RCLUNLNA-RCLUNLNB-RCLUNLNN-RCLUNLDB                
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC59                                                         
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC59                                                         
*                                                                               
         CLI   FVIFLD,C'A'         SINGLE UNDERLINE ABOVE                       
         BNE   VALR58B                                                          
         OI    RCLOPT3,RCLUNLNA                                                 
         B     VALREC59                                                         
*                                                                               
VALR58B  CLI   FVIFLD,C'B'         SINBLE UNDERLINE BELOW                       
         BNE   VALR58C                                                          
         OI    RCLOPT3,RCLUNLNB                                                 
         B     VALREC59                                                         
*                                                                               
VALR58C  CLI   FVIFLD,C'D'         DOUBLE UNDERLINE ABOVE                       
         BNE   VALR58D                                                          
         OI    RCLOPT3,RCLUNLNA+RCLUNLDB                                        
         B     VALREC59                                                         
*                                                                               
VALR58D  CLI   FVIFLD,C'E'         DOUBLE UNDERLINE BELOW                       
         BNE   VALR58E                                                          
         OI    RCLOPT3,RCLUNLNB+RCLUNLDB                                        
         B     VALREC59                                                         
*                                                                               
VALR58E  CLC   FVIFLD(1),APNO      NO UNDERLINE                                 
         BNE   IVALINPT                                                         
         OI    RCLOPT3,RCLUNLNN                                                 
*                                                                               
VALREC59 ICM   R4,15,ACOLPZTT      ->   PRINT ZERO TOTALS                       
         BZ    VALREC60                                                         
         CLI   8(R4),C'('                                                       
         BE    VALREC60                                                         
         NI    RCLOPT4,X'FF'-RCLZRTTY-RCLZRTTN                                  
         GOTO1 AFVAL,(R4)                                                       
         BNE   VALREC60                                                         
*                                                                               
         BAS   RE,LFTJFLD                                                       
         BNH   VALREC60                                                         
*                                                                               
VALR59A  TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
         CLC   FVIFLD(L'APNO),APNO                                              
         BNE   VALR59C                                                          
         OI    RCLOPT4,RCLZRTTN                                                 
         B     VALREC60                                                         
*                                                                               
VALR59C  CLC   FVIFLD(L'APYES),APYES                                            
         BNE   IVALINPT                                                         
         OI    RCLOPT4,RCLZRTTY                                                 
*                                                                               
VALREC60 SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH  OF DATA                              
         AHI   RF,RCLNLNQ                                                       
         STC   RF,RCLLN            FIX ELEMENT LENGTH                           
***********************************************************************         
*        ADD COLUMN HEADINGS FOR NEW OR CHANGE IN KEYWORD             *         
***********************************************************************         
         SPACE 1                                                                
         CLI   NEWELEM,YES         WAS IT A NEW KEYWORD ?                       
         BNE   VALREC63            NO                                           
         ICM   R4,15,ACOLHD#1      IS THIS FIELD ON SCREEN                      
         BZ    *+8                                                              
         TM    4(R4),FVITHIS       INPUT THIS TIME IN ?                         
         BO    VALREC63            YES,  SO   GET  WHAT WAS  INPUT              
         ICM   R4,15,ACOLHD#2      IS THIS FIELD ON SCREEN                      
         BZ    *+8                                                              
         TM    4(R4),FVITHIS       INPUT THIS TIME IN ?                         
         BO    VALREC63            YES,  SO   GET  WHAT WAS  INPUT              
*                                                                               
         TM    RCLOPT,RCLEQU       EQUATION ?                                   
         BZ    *+8                 NO,  SKIP                                    
         TM    SVRCLOPT,RCLEQU     WAS  OLD  ALSO EQUATION ?                    
         BO    VALREC63            YES, USE  DISPLAYED HEADINGS                 
*                                                                               
         CLI   RCLSPCL,RCLSPEXR                                                 
         BE    *+8                                                              
         CLI   RCLSPCL,RCLSPCME    CUME() KEYWORD  ?                            
         BNE   VALREC61            NO,  NEITHER ONE OF THESE                    
*                                  WAS  OLD  ALSO SPECIAL ?                     
         CLI   SVRCLSPC,RCLSPEXR                                                
         BE    VALREC63            YES, USE  DISPLAYED HEADINGS                 
         CLI   SVRCLSPC,RCLSPCME                                                
         BE    VALREC63            YES, USE  DISPLAYED HEADINGS                 
*                                                                               
VALREC61 CLI   APREPJCL,REPJCLV    PRODUCTION ?                                 
         BE    VALREC62            YES, USE  DEFAULT   HEADINGS                 
*&&UK*&& TM    KWDINDS,KWDSHEAD    ALWAYS SHOW HEADINGS                         
*&&UK*&& BO    VALREC62            YES, USE  DEFAULT   HEADINGS                 
*                                                                               
         TM    RCLOPT,RCLACCM      AN   ACCUM     COLUMN ?                      
         BZ    VALREC62            NO,  USE  DEFAULT   HEADINGS                 
*                                                                               
*                                  YES, ACCUM     COLUMN                        
         TM    SVRCLOPT,RCLACCM    WAS  OLD  ALSO ACCUM ?                       
         BZ    VALREC62            NO,  USE  DEFAULT   HEADINGS                 
*                                                                               
*                                  YES, ACCUM     ->   ACCUM                    
         CLI   RCLSPCL,RCLSPEXR                                                 
         BE    VALREC62            YES, USE  DEFAULT   HEADINGS                 
         CLI   RCLSPCL,RCLSPCME                                                 
         BE    VALREC62            YES, USE  DEFAULT   HEADINGS                 
*                                  SPECIAL   ->   ACCUM   ?                     
         CLI   SVRCLSPC,RCLSPEXR                                                
         BE    VALREC62            YES, USE  DEFAULT   HEADINGS                 
         CLI   SVRCLSPC,RCLSPCME                                                
         BNE   VALREC63            YES, USE  DEFAULT   HEADINGS                 
*                                                                               
VALREC62 GOTO1 MKHEAD,APPARM,(R8)  MAKE HEADLINES                               
         B     VALREC70                                                         
***********************************************************************         
*        ADD COLUMN HEADINGS FOR EXISING COLUMN                       *         
***********************************************************************         
         SPACE 1                                                                
VALREC63 LA    R4,SVHEAD#1         START WITH SAVED HEADING # 1                 
         OC    ACOLHD#1,ACOLHD#1   ON  THE SCREEN ?                             
         BZ    VALREC64            YES, SO USE INPUTED HEADING                  
         MVI   RCLHD1LN,0                                                       
         L     R1,ACOLHD#1                                                      
         OI    6(R1),FVOXMT        RE-TRANSMIT                                  
         GOTO1 AFVAL                                                            
         BNE   VALREC64            NO HEADING 1                                 
         MVC   RCLHD1LN,FVILEN     LENGTH OF DATA                               
         LA    R4,FVIFLD           POINT TO NEW DATA                            
*                                                                               
VALREC64 SR    RF,RF                                                            
         IC    RF,RCLLN                                                         
         LA    RE,RCLEL(RF)        POINT TO END OF ELEMENT                      
         SR    R1,R1                                                            
         ICM   R1,1,RCLHD1LN     * LENGTH SHOULD STILL BE THERE                 
         LA    RF,0(R1,RF)       * ADD AND DON'T CHANGE CONDITION CODE          
         STC   RF,RCLLN          * SAVE NEW LENGTH                              
         BZ    VALREC65          * NO HEADING 1                                 
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(R4)                                                   
*                                                                               
VALREC65 LA    R4,SVHEAD#2         CHECK OUT HEADING #2                         
         OC    ACOLHD#2,ACOLHD#2   ON  THE SCREEN ?                             
         BZ    VALREC66            YES, SO USE INPUTED HEADING                  
         MVI   RCLHD2LN,0                                                       
         L     R1,ACOLHD#2                                                      
         OI    6(R1),FVOXMT        RE-TRANSMIT                                  
         GOTO1 AFVAL                                                            
         BNE   VALREC66            NO HEADING 2                                 
         MVC   RCLHD2LN,FVILEN     LENGTH OF DATA                               
         LA    R4,FVIFLD           POINT TO NEW DATA                            
*                                                                               
VALREC66 SR    RF,RF                                                            
         IC    RF,RCLLN                                                         
         LA    RE,RCLEL(RF)        POINT TO END OF ELEMENT                      
         SR    R1,R1                                                            
         ICM   R1,1,RCLHD2LN       LENGTH SHOULD STILL BE THERE                 
         LA    RF,0(R1,RF)         ADD AND DON'T CHANGE CONDITION CODE          
         STC   RF,RCLLN            SAVE NEW LENGTH                              
         BZ    VALREC70            NO HEADING 2                                 
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(R4)                                                   
*                                                                               
VALREC70 ICM   R4,15,ACOLTTYP      TRANSACION TYPE COLUMN ?                     
         BZ    VALREC71            NO                                           
         GOTO1 AFVAL,(R4)                                                       
         BNE   *+12                NO INPUT                                     
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
         BAS   RE,VALTYPE                                                       
         BNE   VALREC99                                                         
*                                                                               
VALREC71 ICM   R4,15,ACOLBUDG      BUDGET COLUMN ?                              
         BZ    VALREC72            NO                                           
         GOTO1 AFVAL,(R4)                                                       
         BNE   *+12                NO INPUT                                     
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
         BAS   RE,VALBUD                                                        
         BNE   VALREC99                                                         
*                                                                               
VALREC72 ICM   R4,15,ACOLFCUR      FOREIGN CURRENCY COLUMN ?                    
         BZ    VALREC74                                                         
         GOTO1 AFVAL,(R4)                                                       
         BNE   *+12                NO INPUT                                     
         TM    RCLOPT,RCLACCM                                                   
         BZ    IVALINPT                                                         
         BAS   RE,VALCURR                                                       
         BNE   VALREC99                                                         
*                                                                               
VALREC74 OC    RANKON,RANKON       ANY  RANKING ?                               
         BZ    VALREC76            NO,  SKIP                                    
         CLI   RANKON,C'C'         RANK ON COLUMNS                              
         BNE   VALREC75            NO,  SKIP                                    
         CLC   CURCOL,RANKON+1     IS   THIS THE RANK ON COLUMN ?               
         BNE   VALREC75            NO,  SKIP                                    
         TM    RCLOPT,RCLACCM      ACCUMULATED COLUMN ?                         
         BO    IVALCNNU            YES, RANK ON COL MUST BE NON-NUMERIC         
*                                                                               
VALREC75 CLC   CURCOL,RANKCOL      IS   THIS THE RANK COLUMN ?                  
         BNE   VALREC76            NO,  SKIP                                    
         TM    RCLOPT,RCLACCM      ACCUMULATED COLUMN ?                         
         BZ    IVALCNU             YES, RANK COL MUST BE NUMERIC                
*                                                                               
VALREC76 L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     X'C3'                                        
         MVC   ELEMSEQ,CURCOL      DELETE ELEMENT                               
         GOTO1 DELEL                                                            
         MVC   APELEM,ELEMENT      ADD THE NEW ELEMENT WITH CUR SEQ.            
         GOTO1 ADDEL                                                            
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
VALREC78 SR    RF,RF                                                            
         IC    RF,CURCOL           INCREMENT CURRENT SEQUENCE                   
         LA    RF,1(,RF)                                                        
         STC   RF,CURCOL                                                        
         BRAS  RE,BUMPLINE                                                      
         BE    VALREC12            PASSED END OF COLUMNS                        
         TM    MIXESTK,DEFOEST+DEFNEST                                          
         BO    IVALMIX                                                          
         TM    MIXESTK,DEFCACR     TEST KEYWORD REQUIRES CONTRA A/C             
         BZ    VALREC80                                                         
         TM    MIXESTK,DEFCACI     TEST CONTRA A/C KEYWORD PRESENT              
         BNZ   VALREC80                                                         
         LR    RF,R5                                                            
         AHI   RF,SAVOIND-TWAD                                                  
         TM    0(RF),SAVOCAC       TEST CONTRA A/C IN ROW                       
         BZ    IVALNOCA                                                         
VALREC80 BRAS  RE,BUMPBACK         RESET TO LAST LINE                           
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
*  FIND ANY ADDITIONAL ERRORS THAT WOULD CAUSE ERRORS DURING DISPLAY  *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
         TM    KWDINDS,KWDBILLT    KEYWORD "BILLT" ?                            
         BZ    VALREC82                                                         
         TM    KWDINDS,KWDDRBIL    KEYWORD "DR,BILL" MUST PRESENT               
         BO    VALREC82                                                         
         MVC   FVMSGNO,=AL2(ACEDRREQ)  DR,BILL REQUIRED                         
         L     RE,SVBILCOL             SET CURSOR TO BILLT FIELD                
         ST    RE,FVADDR                                                        
         B     VALREC99                                                         
*&&                                                                             
***********************************************************************         
*  FIND ANY ADDITIONAL ERRORS THAT WOULD CAUSE ERRORS DURING DISPLAY  *         
***********************************************************************         
         SPACE 1                                                                
VALREC82 CLI   APACTN,ACTCHA       ACTION CHANGE?                               
         BNE   VALREC84                                                         
         CLI   APPFKEY,PFKINS      INSERT COLUMN?                               
         BNE   VALREC84                                                         
         CLI   FXDCURSR,YES        FIXED THE CURSOR ?                           
         BE    IVALCOLF            YES, INVALID COLUMN NUMBER                   
         EJECT ,                                                                
***********************************************************************         
*  ACTUALLY DELETE THE COLUMNS                                        *         
***********************************************************************         
         SPACE 1                                                                
VALREC84 LA    R0,CLIMIT           NUMBER OF COLUMNS ON SCREEN                  
         MVC   SVSVINSQ,SVINSSEQ   SAVE  INSERT    SEQUENCE   NUMBER            
*                                                                               
VALREC85 L     R4,ACOLDATA                                                      
         GOTO1 AFVAL,(R4)          ANY   DATA FOUND ?                           
         BE    VALREC90            YES,  SO   CHECK NEXT COLUMN                 
         SR    R1,R1                                                            
         IC    R1,STSEQ                                                         
         AR    R1,R0               R0 =  STARTING  COLUMN #                     
         BCTR  R1,0                                                             
         GOTO1 =A(DELCOLMN),RR=APRELO                                           
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
         CLI   APPFKEY,PFKINS      INSERT    PF   KEY ?                         
         BNE   VALREC87            NO,  SKIP                                    
         CLC   ACURCOL,ACOLNUM     AFTER     CURRENT   CURSOR ?                 
         BNE   VALREC87                                                         
         L     RE,ACURCOL                                                       
         SH    RE,NEXTCOL                                                       
         C     RE,ACOLFRST         DID WE BUMP TOO FAR ?                        
         BL    VALREC90            YES                                          
         ZIC   RF,0(,RE)           RE = COL NUMBER FIELD                        
         AR    RE,RF               RE = COL DATA   FIELD                        
         ST    RE,APCURSOR         SAVE NEW LOCATION                            
         B     VALREC90            NO,  SKIP                                    
*                                                                               
VALREC87 CLI   APPFKEY,PFKHLP      HELP PF   KEY ?                              
         BNE   VALREC90            NO,  SKIP                                    
         CLI   SVINSSEQ,0          DURING    AN   INSERT ?                      
         BE    VALREC90            NO,  SKIP                                    
         CLM   R1,1,SVSVINSQ       AFTER     SAVE INSSEQ ?                      
         BNL   VALREC90            YES, SKIP                                    
         IC    R1,SVINSSEQ         DECREMENT SAVED INSERT COLUMN #              
         BCTR  R1,0                                                             
         STC   R1,SVINSSEQ                                                      
         IC    R1,CUR@RHLP+3       DECREMENT ADDRESS TO PUT DATA                
         BCTR  R1,0                                                             
         STC   R1,CUR@RHLP+3                                                    
*                                                                               
VALREC90 BRAS  RE,BUMPBACK         BUMP BACK ONE  COLUMN ROW                    
         BCT   R0,VALREC85                                                      
         EJECT ,                                                                
***********************************************************************         
*  FINISHED BUILDING COLUMNS NOW RE-SEQUENCE AND EXTRA VALIDATION     *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 =A(CCOLSEQ),RR=APRELO                                            
         BNE   IVALEXIT                                                         
         GOTO1 =A(CHKCOLS),RR=APRELO                                            
         BNE   IVALCOLC                                                         
         GOTO1 =A(DELXTRM),RR=APRELO                                            
***********************************************************************         
*  OUTPUT THE RECORD TO THE FILE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALREC92 GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON     ERROR, EXIT                           
*&&UK                                                                           
         TM    KWDINDS,KWDTSDAY    KEYWORD "TSDAY" ?                            
         BZ    VALREC94                                                         
         TM    KWDINDS,KWDTSDTE    TSDTE KEYWORD REQUIRED                       
         BO    VALREC94                                                         
         MVC   FVMSGNO,=AL2(ACEDTREQ)  TSDTE KEYWORD REQUIRED                   
         L     RE,SVDAYCOL             SET CURSOR TO TSDAY FIELD                
         ST    RE,FVADDR                                                        
         B     VALREC99                                                         
*&&                                                                             
         USING RCLELD,R1                                                        
VALREC94 TM    DWNTYPE,DWNTACNT+DWNTQREP   ACCENT OR QREPORT SPEC ?             
         BZ    VALREC95            NO - OK                                      
         L     R1,AIOAREA1         A(FORMAT RECORD)                             
         MVI   APELCODE,RCLELQ     X'C3' COLUMN ELEMENT                         
         GOTO1 GETEL                                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(ACEM1R1C)                                           
         B     VALREC99            MUST DEFINE AT LEASE 1 COLUMN                
         TM    RCLOPT,RCLACCM                                                   
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
VALREC95 MVI   APELCODE,STYELQ     SCRIBE TYPE X'25'                            
         L     R1,AIOAREA1                                                      
         GOTO1 GETEL,(R1)                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING STYELD,R1                                                        
*        MVC   STYSEC#1,SECLVL                                                  
*        MVC   APSECKYW,SECLVL                                                  
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
         TM    APINDS,APIOKADD           ADD A RECORD?                          
         BO    VALREC96                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA           CHANGE A RECORD?                       
         BO    VALREC96                                                         
         DC    H'00'                     WHAT THE HELL?                         
*                                                                               
VALREC96 GOTO1 AIO                                                              
         BE    VALREC97                                                         
         TM    IOERR,IOEDUP        DOES RECORD EXIST BUT DELETED                
         BNZ   *+6                                                              
         DC    H'00'               BAD WRITE OR SOMETHING DUDE                  
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
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
         CLI   0(RE),EOR           ANY ELEMENTS ON RECORD?                      
         BNE   *+8                                                              
         OI    RESRSTA,X'80'       MARK DELETED, NO ELEMENT ON RECORD           
         MVC   IOADDR,AIOAREA0                                                  
         GOTO1 AIO,IOWRITE+IOACCFIL                                             
         DROP  R2                                                               
*                                                                               
VALREC98 CLI   APACTN,ACTDEL       DELETE ONLY                                  
         BE    DISREC                                                           
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   IVALEXIT                                                         
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    DISREC                                                           
         CLI   APMODE,APMNEWK      ACTION COPY?                                 
         BNE   DISREC              NO                                           
         CLI   NEWKEY,YES          IS IT REALLY A NEW KEY?                      
         BNE   IVALCCPY            NO, UPDATE RECORD WITH COLUMNS ONLY          
         B     EXIT                ADD NEW RECORD WITH THE COLUMNS ONLY         
         EJECT ,                                                                
***********************************************************************         
*  COPY COLUMN ELEMENTS                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
CPYCLEL  DS    0H                  ONLY COPY COLUMN ELEMENTS                    
         MVC   RESKEY,SAVEKEY      RESTORE ORIGINAL RECORD                      
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         L     R2,AIOAREA1                                                      
         CLI   NEWKEY,YES          ARE WE ADDING COL.S TO A NEW RECORD?         
         BE    CPYCLEL4 YES,  SKIP                                              
         NI    APINDS,TURNOFF-APIOKADD   TURN OFF TO TRICK ACTION COPY          
         MVI   APELCODE,RCLELQ           REMOVE ANY COLUMNS FROM RECORD         
         GOTO1 DELEL,(R2)                                                       
*                                                                               
         USING RFLELD,R1                                                        
         LR    R1,R2               DELETE FILTERS ON COLUMNS                    
         MVI   APELCODE,RFLELQ                                                  
         GOTO1 GETEL                                                            
         BNE   CPYCLEL4            NONE TO DELETE                               
*                                                                               
CPYCLEL2 CLI   0(R1),0             END OF RECORD                                
         BE    CPYCLEL3                                                         
         CLI   0(R1),RFLELQ        X'C5'                                        
         BNE   CPYCLE2A                                                         
         CLI   RFLSEQ,0            DON'T DELETE PROFILE FILTERS                 
         BE    *+8                                                              
         MVI   RFLEL,X'FF'         MARK FOR DELETION                            
         B     CPYCLE2B                                                         
*                                                                               
CPYCLE2A CLI   0(R1),MNOELQ        X'C9'  ELEMENT ?                             
         BH    CPYCLEL3            HIGH,  DONE                                  
         BNE   CPYCLE2B            NO,    CONTINUE                              
*                                  COLUMN MESSAGE ?                             
         CLI   MNOSTYPE-MNOELD(R1),MNOSTCOL                                     
         BNE   CPYCLE2B            NO,    SKIP                                  
         MVI   0(R1),X'FF'         MARK   FOR DELETION                          
*                                                                               
CPYCLE2B SR    RF,RF                                                            
         IC    RF,RFLLN                                                         
         AR    R1,RF                                                            
         B     CPYCLEL2                                                         
*                                                                               
CPYCLEL3 MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL,(R2)                                                       
         DROP  R1                                                               
*                                                                               
CPYCLEL4 TM    APINDS,APIOKADD                                                  
         BZ    *+10                                                             
         MVC   RESKEY,APRECKEY                                                  
         L     R3,AIOAREA2         GET  FIRST ELEMENT                           
         MVI   APELCODE,RCLELQ     ADD  COLUMNS TO RECORD                       
         GOTO1 GETEL,(R3)                                                       
         BNE   CPYCLEL8            NO   ELEMENT TO COPY                         
*                                                                               
         SR    R4,R4                                                            
CPYCLEL5 LR    R3,R1               R1   RETURNED POINTING TO ELEMENT            
         XC    APELEM,APELEM                                                    
         IC    R4,1(,R3)           GET  ELEMENT LENGTH                          
         BCTR  R4,0                ONE  FOR EXECUTE                             
         EXMVC R4,APELEM,0(R3)                                                  
         GOTO1 ADDEL,(R2)                                                       
         BNE   CPYCLELE            ON   ERROR, EXIT                             
         GOTO1 NEXTEL,(R3)                                                      
         BE    CPYCLEL5                                                         
*                                  ADD  THE NEW ELEMENT AND GET NEXT            
         L     R3,AIOAREA2                                                      
         MVI   APELCODE,RFLELQ     ADD  FILTER COLUMNS TO RECORD                
         GOTO1 GETEL,(R3)                                                       
         BNE   CPYCLE7A            NO   ELEMENT TO COPY                         
*                                                                               
         USING RFLELD,R3                                                        
         SR    R4,R4                                                            
CPYCLEL6 LR    R3,R1               R1   RETURNED POINTING TO ELEMENT            
         CLI   RFLSEQ,0                                                         
         BE    CPYCLEL7                                                         
         XC    APELEM,APELEM                                                    
         IC    R4,1(,R3)           GET  ELEMENT LENGTH                          
         BCTR  R4,0                ONE  FOR EXECUTE                             
         EXMVC R4,APELEM,0(R3)                                                  
         GOTO1 ADDEL,(R2)                                                       
         BNE   CPYCLELE            ON   ERROR, EXIT                             
*                                                                               
CPYCLEL7 GOTO1 NEXTEL,(R3)                                                      
         BE    CPYCLEL6                                                         
         DROP  R3                                                               
*                                                                               
CPYCLE7A L     R3,AIOAREA2         GET  ANY MESSAGE  ELEMENTS                   
         MVI   APELCODE,MNOELQ     ADD  MESSAGE ELEMENTS TO RECORD              
         GOTO1 GETEL,(R3)                                                       
         BNE   CPYCLEL8            NO   ELEMENT TO COPY                         
*                                                                               
         USING MNOELD,R3                                                        
         SR    R4,R4                                                            
CPYCLE7B LR    R3,R1               R1   RETURNED POINTING TO ELEMENT            
         CLI   MNOSTYPE,MNOSTCOL   COL  MESSAGE ?                               
         BNE   CPYCLE7C            NO,  SKIP                                    
         XC    APELEM,APELEM                                                    
         IC    R4,1(,R3)           GET  ELEMENT LENGTH                          
         BCTR  R4,0                ONE  FOR EXECUTE                             
         EXMVC R4,APELEM,0(R3)                                                  
         GOTO1 ADDEL,(R2)          ADD  THE NEW ELEMENT                         
         BNE   CPYCLELE            ON   ERROR, EXIT                             
*                                                                               
CPYCLE7C GOTO1 NEXTEL,(R3)         GET  NEXT ELEMENT                            
         BE    CPYCLE7B                                                         
         DROP  R3                                                               
*                                                                               
CPYCLEL8 CLI   NEWKEY,YES          ARE  WE ADDING A NEW RECORD?                 
         BNE   CPYCLEL9                                                         
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
*                                                                               
CPYCLEL9 B     VALREC92                                                         
*                                                                               
CPYCLELE B     VALREC99            ERROR, EXIT                                  
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY HEADLINE DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISKEY   LA    R2,APRECKEY                                                      
         MVC   COLCODE,RESKFORM                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVI   APPFKEY,0                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISREC   GOTO1 =A(DISRSUB),RR=APRELO     DISPLAY RECORD SUBROUTINE              
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        CHECK COLUMNS ON SCREEN FOR INPUT                            *         
***********************************************************************         
         SPACE 1                                                                
ANYINPUT NTR1                                                                   
         LA    RF,(ACOLLNQ/4)-1                                                 
         LA    RE,ACOLDATA         START WITH THIS COLUMN                       
ANYINP10 ICM   R1,15,0(RE)         IS THIS COLUMN ACTIVE ?                      
         BZ    ANYINP20            NO, CHECK NEXT COLUMN                        
         TM    4(R1),FVITHIS       DID USER INPUT DATA  ?                       
         BO    ANYYES              YES                                          
ANYINP20 LA    RE,4(,RE)           TEST NEXT COLUMN                             
         BCT   RF,ANYINP10                                                      
         B     ANYNO               NO INPUT ON THIS COLUMN                      
ANYYES   SR    RE,RE               SET CC = YES, =                              
ANYNO    LTR   RE,RE               SET CC = NO,  <>                             
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE USER FIELD INPUT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALRUF   SR    RF,RF                                                            
         IC    RF,CPARMS                                                        
*                                                                               
VALRUF10 CLC   12(2,R3),SPACES     USER FIELD OF SPACES ?                       
         BE    VALRUF15            YES, ERROR                                   
         CLI   0(R3),2             LENGTH OF PARAMETER > 2 ?                    
         BNH   VALRUF20            NO,  OKAY                                    
*                                                                               
VALRUF15 MVC   FVMSGNO,=AL2(ACEIVUF)    INVALID USER FIELD                      
         MVC   FVXTRA(12),12(R3)                                                
         B     VALRUF90                                                         
*                                                                               
VALRUF20 LA    R3,32(,R3)          BUMP TO NEXT BLOCK PARAMETER                 
         BCT   RF,VALRUF10                                                      
*                                                                               
VALRUF90 BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE FOREIGN CURRENCY TYPE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
VALCURR  NTR1                                                                   
         MVC   SVELEM,APELEM                                                    
         MVI   CURTYP#,RFLFCUR                                                  
         BAS   RE,DELRFL           DELETE X'C5', CURRENCY                       
         BNE   VALCURR9                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALCURR8                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   FVILEN,3                                                         
         BH    VALCURR9                                                         
         GOTO1 VALFCUR,FVIFLD                                                   
         BNE   VALCURR9                                                         
         LA    R1,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RFLEL,RFLELQ        X'C5'                                        
         MVI   RFLLN,RFLLNQ+3                                                   
         MVC   RFLSEQ,CURCOL                                                    
         MVI   RFLTYPE,RFLFCUR                                                  
         MVC   RFLDATA(3),FVIFLD                                                
         OC    RFLDATA(3),SPACES   MAKE SURE THERE ARE 3 BYTES OF DATA          
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALCURR9                                                         
*                                                                               
VALCURR8 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALCURR9 CLC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APELEM,SVELEM                                                    
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE TRANSACTION TYPES                                         *         
*      R4 = FIELD HEADER                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R8                                                        
VALTYPE  NTR1                                                                   
         MVC   SVELEM,APELEM                                                    
         MVI   CURTYP#,RFLTTYPE                                                 
         BAS   RE,DELRFL           DELETE X'C5', TRANS. TYPES                   
         BNE   VTYPE99                                                          
*                                                                               
         CLI   FVILEN,0            ANY INPUT ?                                  
         BE    VTYPE90             NO                                           
         ICM   RF,15,ACOLBUDG      BUDGET COLUMN ?                              
         BZ    VTYPE10             NO                                           
         SR    R1,R1                                                            
         IC    R1,0(,RF)                                                        
         SHI   R1,9                                                             
         EXCLC R1,8(RF),SPACES                                                  
         BNH   VTYPE14                                                          
*                                                                               
VTYPE10  MVI   CURTYP#,RFLBUDGT                                                 
         BAS   RE,GETRFL                                                        
         MVC   FVMSGNO,=AL2(1321)  INVALID INPUT, CONFLICT W/BUDGET             
         BE    VTYPE99             FOUND ONE, SO CAN'T ADD TYPE                 
*                                                                               
VTYPE14  GOTO1 VSCANNER,APPARM,(R4),(13,BLOCK),SCNP3NEQ                         
         MVC   CPARMS,APPARM+4                                                  
*                                                                               
         LA    R8,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RFLEL,RFLELQ                                                     
         MVC   RFLSEQ,CURCOL                                                    
         MVI   RFLTYPE,RFLTTYPE                                                 
         CLI   FVIFLD,C'*'         EXCLUSION?                                   
         BNE   *+8                                                              
         OI    RFLIND,RFLXCLD                                                   
*                                                                               
         GOTO1 CNVTTYPE,APPARM,(C'N',BLOCK),(CPARMS,RFLDATA)                    
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   VTYPE99                                                          
*                                                                               
         ZIC   R1,CPARMS                                                        
         LA    R1,RFLLNQ(,R1)                                                   
         STC   R1,RFLLN                                                         
*                                                                               
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VTYPE99                                                          
*        MVI   CURTYP#,RFLBUDGT    DELETE BUDGET CUZ OF TYPE                    
*        BAS   RE,DELRFL                                                        
*                                                                               
VTYPE90  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VTYPE99  MVC   APELEM,SVELEM                                                    
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         MVC   FVXTRA(12),BLOCK+12                                              
         CLC   FVMSGNO,=AL2(1321)  INVALID INPUT, CONFLICT W/BUDGET             
         BNE   XIT                                                              
         MVCDD FVXTRA(20),AC#BGT                                                
         LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BUDGET INPUT                                              *         
*      R4 = FIELD HEADER                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING BUDRECD,R2                                                       
         USING RFLELD,R8                                                        
VALBUD   NTR1                                                                   
         MVC   SVELEM,APELEM                                                    
         MVI   CURTYP#,RFLBUDGT                                                 
         BAS   RE,DELRFL                                                        
         BNE   VALBUD99                                                         
*                                                                               
         CLI   FVILEN,0            ANY INPUT ?                                  
         BE    VALBUD90            NO                                           
         ICM   RF,15,ACOLTTYP      BUDGET COLUMN ?                              
         BZ    VALBUD05            NO                                           
         SR    R1,R1                                                            
         IC    R1,0(,RF)                                                        
         SHI   R1,9                                                             
         EXCLC R1,8(RF),SPACES                                                  
         BNH   VALBUD07                                                         
*                                                                               
VALBUD05 MVI   CURTYP#,RFLTTYPE    CAN'T HAVE TRANS TYPE                        
         MVCDD FVXTRA(20),AC#TRNTF                                              
         BAS   RE,GETRFL                                                        
         MVC   FVMSGNO,=AL2(1321)  INVALID INPUT, CONFLICT W/TYPE               
         BE    VALBUD99            FOUND ONE, SO CAN'T ADD BUDGET               
         MVC   FVXTRA,SPACES       RE-CLEAR AREA                                
*                                                                               
VALBUD07 MVC   FVMSGNO,=AL2(ACEBUD)    SET  INVALID BUDGET CODE                 
         MVC   SVKEY,IOKEY             SAVE IOKEY                               
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         LA    R2,IOKEY            BUILD KEY TO READ FOR                        
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN                                                   
*                                                                               
         TM    FVIIND,FVINUM       IS IT NUMERIC                                
         BZ    VALBUD08            NO                                           
         CLI   FVILEN,5            ALLOW ONLY 99999 MAX                         
         BH    VALBUD99            YES, INVALID BUDGET                          
         ZIC   RF,FVXLEN           GET  LENGTH - 1                              
         EX    RF,*+8              PACK INTO APDUB                              
         B     *+10                                                             
         PACK  APDUB,FVIFLD(0)                                                  
*                                                                               
         CVB   RF,APDUB            CONVERT TO BINARY                            
         STH   RF,APHALF           SAVE BUDGET NUMBER                           
         MVC   BUDKNO1,APHALF      INSERT INTO RECORD                           
         GOTO1 AIO,IO2+IOACCFIL+IOHI                                            
         BL    VALBUD99            HARDWARE ERROR, EXIT                         
         MVC   FVMSGNO,=AL2(ACEBUD)    SET  INVALID BUDGET CODE                 
         L     R2,AIOAREA2         A(BUDGET RECORD READ)                        
         CLC   BUDKNO1,APHALF      BUDKNO CHANGED ?                             
         BNE   VALBUD99            YES, INVALID BUDGET                          
         SR    RF,RF                                                            
         IC    RF,0(,R4)           GET  LENGTH OF SCREEN FIELD                  
         SHI   RF,9                                                             
         TM    1(R4),FVAXTND                                                    
         BZ    *+8                                                              
         SHI   RF,8                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),BUDKCOD     MOVE IN NAME                                 
         OI    6(R4),FVOXMT        TRANSMIT NAME                                
         B     VALBUD10                                                         
*                                                                               
VALBUD08 MVC   BUDKCOD,FVIFLD                                                   
         GOTO1 AIO,IO2+IOACCFIL+IOHI                                            
         BL    VALBUD99            HARDWARE ERROR, EXIT                         
         MVC   FVMSGNO,=AL2(ACEBUD)    SET  INVALID BUDGET CODE                 
*        BH    VALBUD99                RECORD NOT FOUND                         
         L     R2,AIOAREA2                                                      
         CLC   BUDKEY(BUDKNO2-BUDKEY),IOKEY                                     
         BNE   VALBUD99                                                         
         MVC   APHALF,BUDKNO2      SAVE BUDGET NUMBER                           
*                                                                               
         USING BIVELD,R1                                                        
VALBUD10 LR    R1,R2                                                            
         MVI   APELCODE,BIVELQ                                                  
         GOTO1 GETEL                                                            
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
         MVI   RFLEL,RFLELQ                                                     
         MVI   RFLLN,8                                                          
         MVC   RFLSEQ,CURCOL                                                    
         MVI   RFLTYPE,RFLBUDGT                                                 
         MVC   RFLDATA(L'BUDKNO2),APHALF       SAVE BUDGET NUMBER               
*                                                                               
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALBUD99                                                         
*        MVI   CURTYP#,RFLTTYPE    DELETE TYPE BECAUSE OF BUDGET                
*        BAS   RE,DELRFL                                                        
*                                                                               
VALBUD90 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALBUD99 MVC   APELEM,SVELEM                                                    
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R1,R2,R8                                                         
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE DATE RANGE COLUMN                   DATE RANGE            *         
*---------------------------------------------------------------------*         
*           DAY        RCLDAY                  ** DAY1,DAY2           *         
*           MON        RCLMON                  ** + OR - A MONTH      *         
*           M1-M12     RCLMON+RCLNROLL            + OR - A YEAR       *         
*           PER        ZERO                       NONE                *         
*           QTR        RCLQTR                     + OR - A QUARTER    *         
*           QTD        RCLQTR+RCLTODTE            + OR - A QUARTER    *         
*           Q1-Q4      RCLQTR+RCLNROLL            + OR - A YEAR       *         
*           YR,YEAR    RCLYEAR                    + OR - A YEAR       *         
*           YTD        RCLYEAR+RCLTODTE           + OR - A YEAR       *         
*           PED        RCLPERD                    + OR - # PERIODS    *         
*           P1-P99     RCLPERD+RCLNROLL         * PERIOD# IN YEAR     *         
*           PMON       RCLPERD+RCLMON           * PERIOD#, +/-MONTH   *         
*           PM1-PM12   RCLPERD+RCLMON+RCLNROLL  * PERIOD# IN MONTH    *         
*                                                                     *         
*   (*)  - FOR LAST YEAR, + FOR NEXT YEAR, NOTHING FOR CURRENT YEAR   *         
*   (**) - "AFTER" OR "PRIOR" ALSO VALID                              *         
***********************************************************************         
         EJECT 1                                                                
         USING RCLELD,R8                                                        
VALRANGE NTR1                                                                   
         LA    R2,RCLSTDT                                                       
         CLI   FVILEN,0            INPUT LENGTH?                                
         BNE   VALRNG05                                                         
         MVC   FVMSGNO,=AL2(ACEPRM)                                             
         TM    RCLOPT,RCLACCM                                                   
         BZ    VALRNG90                                                         
         MVC   FVMSGNO,=AL2(ACE2FEW)                                            
         TM    RCLDTEFG,RCLPERD    CHECK PMON OR PM1-PM12                       
         BZ    VALRNG03                                                         
         TM    RCLDTEFG,RCLMON                                                  
         BO    VALRNG99            NEED SOME INPUT                              
*                                                                               
VALRNG03 TM    RCLDTEFG,RCLDAY     NO INPUT                                     
         BO    VALRNG99            DAY MUST HAVE TWO PARMS                      
         TM    RCLDTEFG,RCLNROLL                                                
         BZ    VALRNG90            NO INPUT OK EXCEPT FOR DAY RANGE             
         LA    R2,RCLENDT                                                       
         B     VALRNG50            ENTER NUMBER OF MON OR QTR                   
*                                                                               
VALRNG05 TM    RCLOPT,RCLACCM                                                   
         BZ    VALRNG90                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         TM    RCLDTEFG,TURNOFF-RCLTODTE        PERIOD HAS NO INPUT             
         BZ    VALRNG99                                                         
         TM    RCLDTEFG,RCLDAY     ARE WE GETTING AMOUNT BY DAY?                
         BZ    VALRNG06            NO                                           
         MVC   SVELEM,APELEM                                                    
         MVI   CURTYP#,RFLBUDGT                                                 
         BAS   RE,DELRFL           DELETE BUDGET IF EXISTS                      
         MVC   APELEM,SVELEM                                                    
*                                                                               
VALRNG06 LA    R6,BLOCK                                                         
         MVC   CPARMS,APPARM+4                                                  
         CLI   CPARMS,0                                                         
         BE    VALRNG90            NO PARAMETERS (NO INPUT)                     
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         CLI   CPARMS,2                                                         
         BH    VALRNG99                                                         
         BL    VALRNG09                                                         
         TM    RCLDTEFG,RCLMON+RCLPERD   PMON OR  PM1-PM12 ?                    
         BNO   VALRNG07                  NO                                     
         TM    RCLDTEFG,RCLNROLL         PMON OR  PM1-PM12 ?                    
         BZ    VALRNG09                  PMON     CAN HAVE 2 PARMS              
         B     VALRNG99                  PM1-PM12 HAS ONLY 1 PARM               
*                                                                               
VALRNG07 TM    RCLDTEFG,RCLNROLL         PM1-PM12,M1-M12,P1-P99,Q1-Q4 ?         
         BO    VALRNG08                  YES                                    
         TM    RCLDTEFG,RCLPERD          IS PED OR PMON ?                       
         BO    VALRNG09                  YES                                    
*                                                                               
VALRNG08 TM    RCLDTEFG,RCLDAY+RCLMON    DAY & MON CAN HAVE 2 PARMS             
         BZ    VALRNG99                                                         
         TM    RCLDTEFG,RCLNROLL+RCLMON  M1-M12 ONLY HAVE 1 PARM                
         BO    VALRNG99                                                         
*                                                                               
VALRNG09 MVI   PARM_N,1                                                         
         MVI   ONEXONLY,NO                                                      
*                                                                               
VALRNG10 CLC   PARM_N,CPARMS                                                    
         BH    VALRNG60            FINISHED                                     
         CLI   PARM_N,2            2ND TIME AROUND?                             
         BE    VALRNG12                                                         
         TM    RCLDTEFG,RCLMON+RCLPERD      PMON OR PM1-PM12 ?                  
         BO    VALRNG20                     YES                                 
*                                                                               
VALRNG12 TM    2(R6),X'80'         IS IT A POSITIVE NUMBER?                     
         BZ    VALRNG20            NO                                           
         MVC   0(2,R2),6(R6)       MOVE IN HALF WORD BINARY FIELD               
         LA    R2,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG20 MVC   FVMSGNO,=AL2(ACEPRM)                                             
         ZIC   R1,0(,R6)           LENGTH OF PARAMETER                          
         BCTR  R1,0                                                             
         TM    RCLDTEFG,RCLPERD    ANY PERIOD TYPE ?                            
         BO    VALRNG40            YES, SO SKIP                                 
*                                                                               
         TM    RCLDTEFG,RCLYEAR+RCLQTR+RCLNROLL                                 
         BNZ   VALRNG40                                                         
         EXCLC R1,12(R6),AC@PRIOR                                               
         BNE   VALRNG30                                                         
         CLI   ONEXONLY,YES                                                     
         BE    VALRNG99                                                         
         CLI   CPARMS,1                                                         
         BE    VALRNG99                                                         
         MVI   ONEXONLY,YES                                                     
         CLI   PARM_N,2            2ND TIME IN                                  
         BNE   *+10                                                             
         MVC   RCLENDT,RCLSTDT     SWAP VALUE FROM START TO END                 
         MVC   RCLSTDT,=XL2'8000'                                               
         LA    R2,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG30 MVC   FVMSGNO,=AL2(ACEPRM)                                             
         EXCLC R1,12(R6),AC@AFTER                                               
         BNE   VALRNG40                                                         
         CLI   ONEXONLY,YES                                                     
         BE    VALRNG99                                                         
         MVI   ONEXONLY,YES                                                     
         CLI   CPARMS,1                                                         
         BE    VALRNG99                                                         
         MVC   RCLENDT,=XL2'8000'                                               
         LA    R2,RCLSTDT                                                       
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
         TM    RCLDTEFG,RCLPERD    ANY PERIOD TYPE ?                            
         BZ    VALRNG48            NO                                           
         TM    RCLDTEFG,RCLMON     IS IT PM1-PM12 OR PMON ?                     
         BO    VALRNG41            YES                                          
         TM    RCLDTEFG,RCLNROLL   IS IT P1-P99 ?                               
         BO    VALRNG48            YES                                          
         MVC   FVMSGNO,=AL2(1758)  OUT SIDE OF PERIOD RANGE ?                   
         C     RF,=AL4(MAXPED#)                                                 
         BNH   VALRNG48                                                         
         B     VALRNG99            ERROR                                        
*                                                                               
VALRNG41 CLI   PARM_N,2            2ND TIME AROUND?                             
         BE    VALRNG48                                                         
         MVC   FVMSGNO,=AL2(1758)                                               
         C     RF,=AL4(MAXPED#)                                                 
         BH    VALRNG99                                                         
         MVC   FVMSGNO,=AL2(1759)                                               
         LTR   RF,RF                                                            
         BZ    VALRNG99                                                         
         STC   RF,RCLPDN1          SAVE PERIOD NUMBER                           
         MVI   RCLPDYR,0           SET TO CURRENT YEAR                          
         TM    APPARM+4,X'80'      IS IT POS OR NEG                             
         BZ    VALRNG42            POSTIVE                                      
         MVI   RCLPDYR,X'FF'       SET TO PREVIOUS YEAR                         
         LA    R2,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG42 CLI   12(R6),C'+'         DOES IT HAVE A + SIGN                        
         BNE   *+8                                                              
         MVI   RCLPDYR,01          SET TO NEXT YEAR                             
         LA    R2,RCLENDT                                                       
         B     VALRNG50                                                         
*                                                                               
VALRNG48 MVC   0(2,R2),APPARM+6    MOVE IN BINARY FIELD                         
         LA    R2,RCLENDT                                                       
*                                                                               
VALRNG50 TM    RCLDTEFG,RCLPERD    ANY PERIOD TYPE ?                            
         BZ    VALRNG52            NO                                           
         TM    RCLDTEFG,RCLMON+RCLNROLL                                         
         BNZ   VALRNG52                                                         
         CLI   PARM_N,1                                                         
         BH    VALRNG55                                                         
         BL    VALRNG90                                                         
         MVC   RCLENDT,RCLSTDT                                                  
         B     VALRNG55                                                         
*                                                                               
VALRNG52 TM    RCLDTEFG,RCLNROLL                                                
         BO    VALRNG90                                                         
*                                                                               
VALRNG55 TM    RCLDTEFG,RCLYEAR+RCLQTR                                          
         BNZ   VALRNG90                                                         
         LA    R6,32(,R6)          BUMP TO SECOND PARAMETER                     
         IC    R1,PARM_N                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,PARM_N                                                        
         B     VALRNG10                                                         
*                                                                               
VALRNG60 MVC   FVMSGNO,=AL2(ACEPRM)                                             
         CLI   ONEXONLY,YES                                                     
         BE    VALRNG90                                                         
         CLI   CPARMS,2                                                         
         BNE   VALRNG90                                                         
         TM    RCLDTEFG,RCLPERD+RCLMON   PMON OR PM1-PM12                       
         BO    VALRNG90                  YES                                    
         TM    RCLDTEFG,RCLMON     MUST BE MON OR M1-M12                        
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
         B     XIT                                                              
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO LEFT JUSTIFY DATA IN FVIFLD                             *         
***********************************************************************         
         SPACE 1                                                                
LFTJFLD  CLC   FVIFLD,SPACES       ANY DATA ?                                   
         BNHR  RE                  NO                                           
*                                                                               
         LA    RF,L'FVIFLD                                                      
LFTJFLD2 CLI   FVIFLD,C' '                 IS THERE A BLANK INFRONT ?           
         BHR   RE                          NO                                   
         MVC   FVIFLD(L'FVIFLD-1),FVIFLD+1 REMOVES BLANK                        
         MVI   FVIFLD+L'FVIFLD-1,C' '      ONLY NEED TO DO ONCE FOR             
         BCT   RF,LFTJFLD2                 REPLICATION OF BLANK ON END          
         DC    H'00'                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO FIND EXISTING X'C5' ELEMENTS FOR CURTYP#/COLSEQ         *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
GETRFL   NTR1                                                                   
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RFLELQ     X'C5' FILTER ELEMENT                         
         MVC   ELEMSEQ,CURCOL      GET COLUMN NUMBER                            
         GOTO1 GETEL                                                            
         BNE   GETRFLNO            NONE FOUND                                   
*                                                                               
GETRFL01 CLI   RFLEL,0             END OF RECORD?                               
         BE    GETRFLNO                                                         
         CLI   RFLEL,RFLELQ        NEW ELEM TYPE OR END OF REC?                 
         BNE   GETRFL02            YES, SO NONE FOUND                           
         CLC   RFLSEQ,CURCOL       MATCH FOR THIS COLUMN                        
         BNE   GETRFL02            NONE FOR THIS COLUMN                         
         CLC   RFLTYPE,CURTYP#     MATCH FILTER TYPE                            
         BE    GETRFLOK                                                         
*                                                                               
GETRFL02 SR    RF,RF                                                            
         IC    RF,RFLLN            BUMP UP TO NEXT ELEMENT                      
         AR    R1,RF                                                            
         B     GETRFL01                                                         
*                                                                               
GETRFLOK SR    RE,RE                                                            
         B     GETRFLX                                                          
*                                                                               
GETRFLNO SR    R1,R1                                                            
*                                                                               
GETRFLX  LTR   RE,RE                                                            
         XIT1  REGS=(R1)                                                        
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO DELETE EXISTING X'C5' ELEMENTS FOR CURTYP#/COLSEQ       *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DELRFL   NTR1                                                                   
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RFLELQ     X'C5' FILTER ELEMENT                         
         MVC   ELEMSEQ,CURCOL      GET COLUMN NUMBER                            
         GOTO1 GETEL                                                            
         BNE   DELRFL04            NONE FOUND                                   
*                                                                               
DELRFL01 CLI   RFLEL,0             END OF RECORD?                               
         BE    DELRFL03                                                         
         CLI   RFLEL,RFLELQ        NEW ELEM TYPE OR END OF REC?                 
         BNE   DELRFL02            YES, SO NONE FOUND                           
         CLC   RFLSEQ,CURCOL       MATCH FOR THIS COLUMN                        
         BNE   DELRFL02            NONE FOR THIS COLUMN                         
         CLC   RFLTYPE,CURTYP#     MATCH FILTER TYPE                            
         BNE   DELRFL02                                                         
         MVI   RFLEL,X'FF'         FOUND SO DELETE                              
*                                                                               
DELRFL02 SR    RF,RF                                                            
         IC    RF,RFLLN            BUMP UP TO NEXT ELEMENT                      
         AR    R1,RF                                                            
         B     DELRFL01                                                         
*                                                                               
DELRFL03 MVI   APELCODE,X'FF'                                                   
         L     R1,AIOAREA1                                                      
         GOTO1 DELEL                                                            
         BNE   XIT                                                              
*                                                                               
DELRFL04 CR    RE,RE                                                            
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  ERROR TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
IVALEXP  MVC   FVXTRA(12),FVIFLD   MOVE BAD DATA IN                             
         B     IVALEXIT                                                         
                                                                                
IVALCCDL MVC   FVMSGNO,=AL2(ACEXDEL)      CAN NOT COPY OVER A RECORD            
         MVC   FVXTRA(8),=C'COPY TO '                                           
         B     IVALEXIT                                                         
                                                                                
IVALKYWD MVC   FVXTRA(12),APKEYWRD                                              
         LHI   R4,ACEKYWDI         KEYWORD NOT VALID                            
         B     IVALMSG                                                          
                                                                                
IVALCMAX LA    R1,MAXCOLS                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  FVXTRA(2),APDUB                                                  
         LHI   R4,1448                                                          
         B     IVALMSG                                                          
                                                                                
IVALUBPR LHI   R4,ACEUNBL          UNBALANCED PARENTHESES                       
         MVC   FVXTRA(12),FVIFLD                                                
         B     IVALMSG                                                          
                                                                                
IVALLOW  LHI   R4,ACE2FEW          TOO FEW PARAMETERS                           
         B     IVALMSG                                                          
                                                                                
IVALHIGH LHI   R4,ACE2MANY         TOO MANY PARAMETERS                          
         B     IVALMSG                                                          
                                                                                
IVALTOT  LHI   R4,ACEIVTO          INVALID TOTAL OPTION                         
         B     IVALMSG                                                          
                                                                                
IVALEKEY MVI   STSEQ,1                                                          
         LHI   R4,-(X'FFFF'-FVFEKEY+1)                                          
         B     IVALMSG                                                          
                                                                                
IVALINPT LHI   R4,-(X'FFFF'-FVFNOTV+1)                                          
         B     IVALMSG                                                          
                                                                                
IVALNNUM LHI   R4,-(X'FFFF'-FVFNOTN+1)     NOT NUMERIC                          
         B     IVALMSG                                                          
                                                                                
IVALIGE0 LHI   R4,ACEIGE0          INPUT NOT >= 0                               
         B     IVALMSG                                                          
                                                                                
IVALIGT0 LHI   R4,ACEIGT0          INPUT NOT >  0                               
         B     IVALMSG                                                          
                                                                                
IVALPFK  LHI   R4,ACEIVCN          INVALID COLUMN NUMBER                        
         MVC   FVADDR,AACTHDR      PUT  CURSOR AT ACTION FIELD                  
         B     IVALMSG                                                          
                                                                                
IVALSORT LHI   R4,ACEIVSS          INVALID SORT SEQUENCE                        
         B     IVALMSG                                                          
                                                                                
IVALCOLF MVI   COLUMN#,0                                                        
         LHI   R4,ACEIVCN          INVALID COLUMN NUMBER                        
         MVC   FVADDR,AACTHDR      PUT  CURSOR AT ACTION FIELD                  
         B     IVALMSG                                                          
                                                                                
IVALWDTH LHI   R4,ACERTWD          REPORT TOO WIDE                              
         ICM   R2,15,ACOLWDTH                                                   
         BZ    *+8                                                              
         L     R2,ACOLDATA                                                      
         ST    R2,FVADDR                                                        
         B     IVALMSG                                                          
*                                  DELETE OF A RANKING FIELD                    
IVALCRNK LHI   R4,ACEDRNKF         INVALID - FIX PROFILE DATA                   
         L     R2,ACOLDATA                                                      
         ST    R2,FVADDR                                                        
         B     IVALMSG                                                          
                                                                                
IVALCOLN LHI   R4,ACEIVCN          INVALID COLUMN NUMBER                        
         B     IVALMSG                                                          
                                                                                
IVALMIX  LHI   R4,ACECMXE          CANNOT MIX OLD WITH NEW ESTIMATE             
         L     R0,FULL                                                          
         ST    R0,FVADDR                                                        
         B     IVALMSG             KEYWORDS                                     
                                                                                
IVALNOCA LHI   R4,ACEFMICA         FORMAT MUST INCLUDE CONTRA                   
         L     R0,FULL                                                          
         ST    R0,FVADDR                                                        
         B     IVALMSG             A/C WITH THIS KEYWORD                        
                                                                                
IVALCCPY LHI   R4,ACICLCPY         COPIED COLUMN ELEMENTS                       
         MVI   FVOMTYP,GTMINF      INFORMATION TYPE                             
         MVC   FVADDR,AACTHDR      PUT CURSOR AT ACTION FIELD                   
         B     IVALMSG                                                          
                                                                                
IVALSOR2 LHI   R4,ACEIVSS          INVALID SORT SEQUENCE                        
         LR    R1,R3               COLUMN NUMBER IN R3                          
         BCTR  R1,0                                                             
         B     IVALCAL1                                                         
                                                                                
         USING RCLELD,R1                                                        
IVALDSRT LHI   R4,ACEDUSN          DUPLICATE SORT NUMBER                        
         B     IVALJOB0                                                         
                                                                                
IVALJOBR LHI   R4,ACEMJBR          MAX EST KEYWORDS REACHED                     
*                                                                               
IVALJOB0 ZIC   R3,RCLSEQ           COLUMN NUMBER IN RCLSEQ                      
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         B     IVALCAL1                                                         
         DROP  R1                                                               
                                                                                
IVALCNNU LHI   R4,ACERONNM         RANK ON MUST BE NON-NUMERIC                  
         B     IVALCNU1                                                         
*                                                                               
IVALCNU  LHI   R4,ACERCNUM         RANK COLUMN MUST BE NUMERIC                  
*                                                                               
IVALCNU1 ZIC   R1,CURCOL           GET  COLUMN NUMBER                           
         BCTR  R1,0                        MINUS    ONE                         
         B     IVALCAL1                                                         
*                                                                               
IVALVERT LHI   R4,ACEVERT          INVALID  VERTICAL                            
         B     IVALCAL0                                                         
*                                                                               
IVALCOLC SR    R1,R1                                                            
         IC    R1,ERRCOL#                                                       
         BCTR  R1,0                                                             
         B     IVALCAL1                                                         
*                                                                               
IVALCAL0 ZIC   R1,0(,RF)           GET COLUMN NUMBER                            
         BCTR  R1,0                                                             
*                                                                               
IVALCAL1 ZIC   R3,STSEQ            MAKE SURE WE ENDUP ON SCREEN                 
         BCTR  R3,0                                                             
         SR    R1,R3                                                            
         BNM   *+6                 IF LOW USE START SEQUENCE                    
         SR    R1,R1                                                            
         CHI   R1,(CLIMIT-1)       IF HIGH USE LAST COLUMN                      
         BNH   *+8                                                              
         LA    R1,CLIMIT-1                                                      
*                                                                               
         MH    R1,NEXTCOL          NOW ON SCREEN FIND CURSOR  ADDRESS           
         A     R1,ACOLFRST                                                      
         SR    RF,RF                                                            
         IC    RF,0(,R1)           BUMP TO COLUMN DATA FIELD                    
         AR    R1,RF                                                            
         ST    R1,FVADDR                                                        
*                                                                               
IVALMSG  CLC   FVMSGNO,=AL2(FVFOK) WAS ERROR SET ALREADY ?                      
         BNE   *+8                 YES                                          
         STCM  R4,3,FVMSGNO        NO, SO SET ERROR STORED IN R9                
*                                                                               
IVALEXIT DS    0H                                                               
         XC    APCURSOR,APCURSOR   CLEAR APPLICATION CURSOR                     
         B     EXIT                NO,  SO EXIT VALREC, RB OK                   
         DROP  R9,RA,RC                                                         
         LTORG                                                                  
         EJECT ,                                                                
DECMTAB  DS    0C                                                               
         DC    CL2'4 ',XL1'05',CL2' 4'                                          
DECMLEN  EQU   *-DECMTAB                                                        
         DC    CL2'3 ',XL1'04',CL2' 3'                                          
         DC    CL2'2 ',XL1'03',CL2' 2'                                          
         DC    CL2'P ',XL1'03',CL2' 2'                                          
         DC    CL2' P',XL1'03',CL2' 2'                                          
         DC    CL2'1 ',XL1'02',CL2' 1'                                          
         DC    CL2'0 ',XL1'01',CL2' 0'                                          
         DC    CL2'D ',XL1'01',CL2' 0'                                          
         DC    CL2' D',XL1'01',CL2' 0'                                          
         DC    CL2'-1',XL1'FF',CL2'-1'                                          
         DC    CL2'-2',XL1'FE',CL2'-2'                                          
         DC    CL2'H ',XL1'FE',CL2'-2'                                          
         DC    CL2' H',XL1'FE',CL2'-2'                                          
         DC    CL2'-3',XL1'FD',CL2'-3'                                          
         DC    CL2'T ',XL1'FD',CL2'-3'                                          
         DC    CL2' T',XL1'FD',CL2'-3'                                          
         DC    CL2'-4',XL1'FC',CL2'-4'                                          
         DC    CL2'-5',XL1'FB',CL2'-5'                                          
         DC    CL2'-6',XL1'FA',CL2'-6'                                          
         DC    CL2'M ',XL1'FA',CL2'-6'                                          
         DC    CL2' M',XL1'FA',CL2'-6'                                          
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*    BUMP UP OR DOWN ONE ROW OF ADDRESS FOR ALL COLUMNS DISPLAYED     *         
***********************************************************************         
         SPACE 1                                                                
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
BUMPLINE LH    RF,NEXTCOL                                                       
         J     BUMPLN                                                           
*                                                                               
BUMPBACK LH    RF,NEXTCOL                                                       
         LNR   RF,RF                                                            
*                                                                               
BUMPLN   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,(ACOLLNQ/4)-1                                                 
         LA    RE,ACOLNUM                                                       
*                                                                               
BUMPLN10 ICM   R1,15,0(RE)         ADDRESS TO ADJUST                            
         BZ    BUMPLN20                                                         
         AR    R1,RF               LENGTH OF LINE                               
         ST    R1,0(,RE)           ADJUST ENTRY                                 
*                                                                               
BUMPLN20 LA    RE,4(,RE)           NEXT ADDRESS                                 
         BCT   R0,BUMPLN10                                                      
*                                                                               
         CLC   ACOLNUM,ACOLFRST    COLUMN  BEFORE  FIRST LINE ?                 
         BL    BUMPLNNO            YES,    INVALID LINE                         
         CLC   ACOLNUM,AENDCOL     COLUMN  AFTER   LAST  LINE ?                 
         BNL   BUMPLNNO            YES,    INVALID LINE                         
*                                                                               
BUMPLNOK SR    RE,RE               SAY     VALID   LINE                         
*                                                                               
BUMPLNNO LTR   RE,RE               VALID   LINE ?  SETS  COND CODE              
         XIT1                      RETURN                                       
         DROP  R7,RC                                                            
         EJECT ,                                                                
*=====================================================================*         
*        SEE IF THE OLD KEYWORD MATCHES WHAT IS NOW ON SCREEN         *         
*=====================================================================*         
         USING WORKD,R7                                                         
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
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* THIS IS NOW OFFICIALLY A MESS ! MUST RE-DO AT SOME POINT            *         
***********************************************************************         
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         USING TWAD,R5                                                          
EXITRTN  NMOD1 0,**EXIT**                                                       
         L     RC,APALOCAL                                                      
         L     R5,ATWA                                                          
         CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT95              NO                                           
*                                                                               
         CLI   APPFKEY,PFKHLP      HELP PF   KEY ?                              
         BNE   EXIT40              NO,  SKIP                                    
         XC    ACURDEF,ACURDEF     SET  TO   BEGINING  OF   HELP                
*                                                                               
EXIT40   CLC   FVMSGNO,=AL2(FVFOK) ANY  ERROR     OCCURED ?                     
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
EXIT80   CLI   APPFKEY,PFK13       FORMAT    PF   KEY  ?                        
         BE    EXIT90              YES, CONTINUE                                
         CLI   APPFKEY,PFK04       COLUMN    FILTER    PF   KEY  ?              
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
         BE    EXITOUT                                                          
         CLI   APMODE,APMDISR      MODE IS   DISPLAY RECORD                     
         BNE   EXITOUT             NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY IS HAPPY                        
*                                                                               
EXITOUT  XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R7,RC                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECORD SUBROUTINE                                          *         
***********************************************************************         
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
DISRSUB  NMOD1 0,**DISR**,RA,R9                                                 
         L     RC,APALOCAL                                                      
         NI    MSGELSW,TURNOFF-MSGELDEL  TURN OFF DELETE ELEMENT SWITCH         
         OI    MSGELSW,MSGELIDR          TURN ON 'IN DISPLAY ROUTINE'           
         XC    SVCURSOR,SVCURSOR                                                
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL                                                            
         ST    R1,ARPFELM                                                       
         SPACE 1                                                                
***********************************************************************         
*   SET UP TO FIND STARTING COLUMN IF LEFT OR RIGHT PFK WAS HIT       *         
***********************************************************************         
         SPACE 1                                                                
         SR    RF,RF                                                            
         IC    RF,COL#FRST                                                      
*                                                                               
DR000    LR    R1,RF                                                            
         CLI   APPFKEY,PFKRGHT2    RIGHT ?                                      
         BNE   DR001                                                            
         AHI   R1,1                                                             
         CHI   R1,COLHIGHQ         MAX NUMBER OF COLUMNS                        
         BNH   DR002               NOT TO HIGH                                  
         B     DR008               TOO HIGH SO IGNORE                           
*                                                                               
DR001    CLI   APPFKEY,PFKLEFT2    LEFT ?                                       
         BNE   DR008                                                            
         SHI   R1,1                                                             
         CHI   R1,COLLOWQ          LOWEST ALLOWED COLUMN                        
         BL    DR008                                                            
*                                                                               
         USING SFDD,RE                                                          
DR002    L     RE,AFLDTAB          ->    FIELD TABLE                            
         LR    RF,R1                                                            
         BCTR  R1,0                ADJUST FOR ZERO INDEXING                     
         MHI   R1,SFDLNQ                                                        
         AR    RE,R1               POINT TO POSSIBLE ENTRY                      
         MVC   FULL,APREPIND       GET REPORT TYPE                              
         NC    FULL,SFDREPS        'AND' WITH VALID REPORT TYPES                
         BZ    DR000               NOT VALID COLUMN TRY AGAIN                   
         TM    SFDIND1,SFDDDS      DDS ONLY COLUMN ?                            
         BZ    DR004               DON'T CARE SO COLUMN OK                      
         TM    CUSTAT,CUSDDS       DDS TERMINAL ?                               
         BZ    DR000               NO, SO TRY NEXT COLUMN                       
         DROP  RE                                                               
*                                                                               
DR004    STC   RF,COL#FRST                                                      
*                                                                               
DR005    LA    R0,CLIMIT           MAX NUMBER OF COLUMNS                        
         LA    R2,1                                                             
         L     R4,ACOLFRST                                                      
*                                                                               
DR006    C     R4,ACURCOL          CURRENT COLUMN ON ?                          
         BE    DR007                                                            
         AH    R4,NEXTCOL                                                       
         LA    R2,1(,R2)           COUNTER OF COLUMN ON                         
         BCT   R0,DR006                                                         
         LA    R2,1                                                             
*                                                                               
DR007    GOTO1 =A(BLDSCR),RR=APRELO                                             
         GOTO1 =A(SETSCR),RR=APRELO                                             
         BCTR  R2,0                                                             
         MH    R2,NEXTCOL                                                       
         A     R2,ACOLFRST                                                      
         ST    R2,ACURCOL          RESET CURSOR                                 
         B     DR010                                                            
*                                                                               
DR008    CLI   APMODE,APMVALR      VALREC ?                                     
         BNE   DR010                                                            
         GOTO1 =A(SETSCR),RR=APRELO                                             
*                                                                               
DR010    L     R2,AENDCOL                                                       
         TWAXC COLNMEH,(R2)                                                     
         BAS   RE,SCROLLIT                                                      
         MVI   INSSEQ,0            INITIALIZE TO ZERO                           
*                                                                               
         L     R2,AIOAREA1         COLUMN ELEMENTS                              
         GOTO1 GETNAME,APPARM,(R2),COLNMEH                                      
         GOTO1 GETPER,APPARM,(R2),COLOWNH                                       
*                                                                               
         LA    R0,CLIMIT           # OF COLUMNS DISPLAYED ON SCREEN             
         SR    R1,R1                                                            
         IC    R1,RESETSEQ         DISPLAY SEQUENCE AS COLUMN NUMBER            
         STC   R1,STSEQ            RESET START SEQUENCE FOR NEXT TIME           
         L     R4,ACOLNUM          LOAD ADDRESS ON COLUMN NUM FIELD             
*                                                                               
*        ------------------------  SET COLUMN NUMBERS ON SCREEN                 
DR020    CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  8(SFDCOLNQ,R4),APDUB                                             
         MVI   8(R4),C' '          BLANK OUT                                    
         CLI   9(R4),C'0'                                                       
         BNE   *+8                                                              
         MVI   9(R4),C' '          BLANK OUT FOR SINGLE DIGITS                  
         OI    6(R4),FVOXMT        TRANSMIT                                     
         AH    R4,NEXTCOL          BUMP TO NEXT ROW (COLUMN)                    
         LA    R1,1(,R1)                                                        
         BCT   R0,DR020                                                         
*        ------------------------  END                                          
*                                                                               
         L     R4,ACOLNUM          A(FIRST COLUMN)                              
         MVC   CURCOL,RESETSEQ     START   SEQUENCING                           
         OC    KWDPASTE,KWDPASTE   ANY  KEYWORD   FROM HELP FACILITY ?          
         BZ    DR030               NO,  SKIP                                    
         CLI   SVINSSEQ,0          HELP REQUESTED DURING    INSERT ?            
         BE    DR025               NO,  SKIP                                    
         ZIC   R4,SVINSSEQ         LOAD THE  INSERT    SEQUENCE  NUMBER         
         STC   R4,COLINSRT         SAVE THE  INSERT    SEQUENCE  NUMBER         
         STC   R4,PASTESEQ         SAVE THE  INSERT    SEQUENCE  NUMBER         
*                                                                               
*                                          FIX  UP COLUMN  CALCULATIONS         
         GOTO1 =A(FIXCALC),APPARM,(R4),1,RR=APRELO                              
*                                          FIX  UP PROFILE ELEMENTS             
         GOTO1 =A(FIXPROF),APPARM,(R4),1,RR=APRELO                              
*                                          FIXUP MESSAGES DISPLAY               
         GOTO1 =A(FIXMSGS),APPARM,(R4),1,NO,RR=APRELO                           
         OI    MSGELSW,MSGELINS            TURN ON INSERT SWITCH                
*                                                                               
DR025    SR    R4,R4               SAVED OFF COLUMN NUMBER FOR HELP             
         IC    R4,CUR@RHLP+3       GET COLUMN NUMBER FOR HELP                   
         BCTR  R4,0                                                             
         MH    R4,NEXTCOL                                                       
         A     R4,ACOLDATA         R4 = A(DATA COLUMN INPUT FIELD)              
         ST    R4,SVCURSOR                 SET CURSOR LOCATION                  
         MVC   8(L'KWDPASTE,R4),KWDPASTE   INSERT    KEYWORD                    
         OI    4(R4),FVITHIS               SAY FIELD INPUTED                    
         MVI   5(R4),L'KWDPASTE            SET LENGTH OF DATA                   
         OI    6(R4),FVOXMT                TRANSMIT                             
*                                                                               
DR030    XC    KWDPASTE,KWDPASTE   KEYWORD DATA        -- CLEAR                 
         CLI   APPFKEY,PFKHLP      PF1, HELP PF KEY USED ?                      
         BE    DR035               YES, SKIP                                    
         XC    CUR@RHLP,CUR@RHLP   A(RETURN FROM HELP) -- CLEAR                 
         MVI   SVINSSEQ,0          SAVE INSSEQ         -- CLEAR                 
*                                                                               
DR035    MVI   APELCODE,RCLELQ     X'C3'                                        
         MVC   ELEMSEQ,CURCOL                                                   
         GOTO1 GETEL,(R2)                                                       
         BNE   DR900                                                            
*                                                                               
         USING RCLELD,R8                                                        
         LR    R8,R1                                                            
         CLI   APPFKEY,PFKINS      INSERT  COLUMN ?                             
         BNE   DR050               NO,     SKIP                                 
         SR    R0,R0                                                            
         L     R1,ACURSOR          A(CURRENT CURSOR LOCATION)                   
         L     R2,ACOLFRST         A(FIRST COLUMN)                              
         SR    R1,R2               FIGURE  OUT THE COLUMN NUMBER                
         BM    DR050               BAD     COLUMN  INSERT, SKIP                 
         LH    RF,NEXTCOL                                                       
         DR    R0,RF               WHERE   INSERT WAS REQUESTED                 
         CHI   R0,(CLIMIT-1)      PAST    LAST    COLUMN ?                      
         BH    DR050               YES,    SKIP                                 
         ZIC   R2,STSEQ            FIRST   COLUMN  ON     SCREEN                
         AR    R2,R1               INSERT  BEFORE  COLUMN NUMBER                
*                                                                               
*        ------------------------  ADJUST COLUMN CALCULATIONS                   
         GOTO1 =A(FIXCALC),APPARM,(R2),1,RR=APRELO                              
*        ------------------------  ADJUST PROFILE ELEMENTS                      
         GOTO1 =A(FIXPROF),APPARM,(R2),1,RR=APRELO                              
*        ------------------------  ADJUST COLUMN MESSAGES                       
         GOTO1 =A(FIXMSGS),APPARM,(R2),1,NO,RR=APRELO                           
         OI    MSGELSW,MSGELINS    TURN ON INSERT  SWITCH                       
         SPACE 1                                                                
***********************************************************************         
*  INSERT A COLUMN LINE                                               *         
***********************************************************************         
         SPACE 1                                                                
DR050    CLI   APACTN,ACTCHA       ACTION CHANGE?                               
         BNE   DR055                                                            
         CLI   APPFKEY,PFKINS      INSERT COLUMN?                               
         BNE   DR055                                                            
         CLI   FXDCURSR,YES        WAS CURSOR WAS ON A COLUMN LINE ?            
         BE    DR055               NO, WE FORCED THE CURSOR HERE                
         CLC   ACOLNUM,ACURCOL     CURRENT CURSOR LOCATION                      
         BNE   DR055               NOT THIS COLUMN                              
         MVC   INSSEQ,CURCOL       SAVE CURRENT SEQUENCE NUMBER                 
         MVC   COLINSRT,CURCOL     SAVE CURRENT SEQUENCE NUMBER                 
         L     RF,ACOLDATA         SET TO DATA FIELD ON COLUMN LINE             
         ST    RF,APCURSOR         RETURN CURSOR TO THIS AREA ON SCREEN         
         BRAS  RE,BUMPLINE         BUMP TO NEXT LINE ON SCREEN                  
         BNE   DR900               PAST LAST LINE, SKIP                         
*                                                                               
DR055    CLI   PASTESEQ,0          JUST PASTED    A    KEYWORD ?                
         BE    DR060               NO,  SKIP                                    
         CLC   PASTESEQ,CURCOL     WAS  THIS LINE PASTED ?                      
         BNE   DR060               NO,  SKIP                                    
         MVC   INSSEQ,PASTESEQ     SAVE THE  PASTE     SEQUENCE  NUMBER         
         MVI   PASTESEQ,0          CLEAR     SWITCH                             
         BRAS  RE,BUMPLINE         BUMP TO   NEXT LINE ON   SCREEN              
         BNE   DR900               PAST LAST LINE,     SKIP THIS LINE           
*                                                                               
DR060    CLC   RCLSEQ,CURCOL       RESET SEQUENCE TO ELEMENT SEQ                
         BNE   DR700                                                            
         SR    R2,R2                                                            
         IC    R2,CURCOL                                                        
         CLI   APPFKEY,PFKINS      INSERT COLUMN?                               
         BNE   DR062                                                            
         CLI   INSSEQ,0            WAS THIS SET YET?                            
         BE    DR062               NO                                           
         MVI   APPFKEY,0           CLEAR OUT PFKEY                              
*                                                                               
DR062    LA    R2,1(,R2)           BUMP UP SEQUENCE                             
         STC   R2,CURCOL                                                        
         CLI   RCLDATLN,SFDDATAQ   LENGTH OF DATA FIELD                         
         BNH   *+8                                                              
         MVI   RCLDATLN,SFDDATAQ                                                
*        BNH   *+6                                                              
*        DC    H'00'               TOO BIG FOR FIELD                            
         L     R4,ACOLDATA                                                      
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),RCLNDATA                                                 
         XC    APWORK,APWORK                                                    
*                                                                               
         USING RFLELD,R2                                                        
         SR    R2,R2                                                            
         L     R2,AIOAREA1                                                      
         AH    R2,DATADISP                                                      
*                                                                               
DR070    CLI   RFLEL,0                                                          
         BE    DR200                                                            
         CLI   RFLEL,RFLELQ        X'C5' FILTER ELEMENT?                        
         BNE   DR080               NEXT  ELEMENT                                
         CLC   RFLSEQ,RCLSEQ       MATCH ON COLUMN                              
         BNE   DR080               NEXT  ELEMENT                                
         L     R4,ACOLNUM                                                       
         MVI   8(R4),C'*'          DEFAULT TO ANY COLUMN FILTER                 
*                                                                               
         CLI   RFLTYPE,RFLBUDGT    BUDGET TYPE?                                 
         BNE   DR072                                                            
         ICM   R4,15,ACOLBUDG      IS BUDGET FIELD ON SCREEN ?                  
         BZ    DR072               NO                                           
         BAS   RE,DISBUD                                                        
*                                                                               
DR072    CLI   RFLTYPE,RFLTTYPE    TRANSACTN TYPE?                              
         BNE   DR073                                                            
         ICM   R4,15,ACOLTTYP      IS TRANS TYPE FIELD ON SCREEN ?              
         BZ    DR073               NO                                           
         MVC   APWORK,SPACES                                                    
         LA    R3,APWORK                                                        
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R2)),(X'00',(R3))                         
         LR    R1,R0                                                            
         MVC   8(SFDTTYPQ,R4),APWORK                                            
         OI    6(R4),FVOXMT         TRANSMIT                                    
*                                                                               
DR073    CLI   RFLTYPE,RFLFCUR     CURRENCY TYPE?                               
         BNE   DR080                                                            
         ICM   R4,15,ACOLFCUR      IS FOREIGN CURRENCY ON SCREEN ?              
         BZ    DR080               NO                                           
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),RFLDATA     CURRENCY DISPLAY                             
*                                                                               
DR080    SR    RF,RF                                                            
         IC    RF,RFLLN            BUMP UP IN RECORD                            
         AR    R2,RF                                                            
         B     DR070                                                            
         DROP  R2                                                               
*                                                                               
DR200    MVC   APWORK,SPACES       CLEAR OUT VALUES                             
         ICM   R4,15,ACOLDTER      DATE RANGE (FIXED)                           
         BZ    DR350                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         CLI   RCLSPCL,RCLSPKY1    PDAY KEYWORD                                 
         BNE   DR201                                                            
         SR    RF,RF                                                            
         ICM   RF,1,RCLPDN1        GET PERIOD NUMBER                            
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         MVC   8(SFDDTERQ,R4),SPACES                                            
         UNPK  8(2,R4),APDUB                                                    
         CLI   8(R4),C'0'                                                       
         BNE   DR201                                                            
         MVC   8(SFDDTERQ,R4),SPACES                                            
         UNPK  8(1,R4),APDUB                                                    
*                                                                               
DR201    CLI   RCLDATES,0          IS   DATE RANGE     USED ?                   
         BE    DR350               NO,  SKIP                                    
         TM    RCLOPT,RCLACCM      IS   THIS AN   ACCUMULATED COLUMN ?          
         BZ    DR350               NO,  SKIP                                    
*                                                                               
DR202    TM    RCLDTEFG,X'FF'-RCLTODTE                                          
         BZ    DR300               MUST BE PERIOD RANGE                         
         CLC   RCLSTDT,=XL2'8000'  DELIMETER FOR "PRIOR"                        
         BNE   DR210                                                            
         MVC   APWORK(5),AC@PRIOR                                               
         B     DR270                                                            
*                                                                               
DR210    MVC   APHALF,RCLSTDT      MOVE INTO APHALF WORD                        
         LH    R2,APHALF                                                        
         TM    RCLDTEFG,RCLPERD+RCLMON   PMON OR PM1-PM12 VALUES ?              
         BNO   DR222                     NO                                     
         SR    R2,R2                                                            
         ICM   R2,1,RCLPDN1        GET PERIOD NUMBER                            
         TM    RCLPDYR,X'80'                                                    
         BZ    *+6                                                              
         LNR   R2,R2                                                            
*                                                                               
DR222    CVD   R2,APDUB                                                         
         MVC   APWORK(7),=XL7'40202020202060'                                   
         LA    R1,APWORK+5                                                      
         EDMK  APWORK(7),APDUB+5                                                
         CLI   APWORK+5,C' '                                                    
         BNE   *+8                                                              
         MVI   APWORK+5,C'0'       FORCE A ZERO TO PRINT                        
         BCTR  R1,0                                                             
         MVC   0(1,R1),APWORK+6    MOVE POSSIBLE MINUS SIGN IN FRONT            
         MVI   APWORK+6,C' '                                                    
         TM    RCLDTEFG,RCLPERD+RCLMON     PMON OR PM1-PM12 ?                   
         BNO   DR225                       NO                                   
         CLI   RCLSTDT,X'01'       FORWARD ONE YEAR?                            
         BNE   *+8                                                              
         MVI   0(R1),C'+'          YES SO MOVE IN A PLUS SIGN                   
         TM    RCLDTEFG,RCLNROLL   PM1-PM12 IN ON                               
         BO    DR300                                                            
         SR    R2,R2                                                            
         OC    RCLENDT,RCLENDT                                                  
         BNZ   DR270                                                            
         B     DR300                                                            
*                                                                               
DR225    TM    RCLSTDT,X'80'       NEGATIVE HALF WORD?                          
         BO    DR230               YES, SO DISPLAY END DATA                     
         TM    RCLDTEFG,RCLDAY     IS IT DAY?                                   
         BO    DR250               YES, SO DISPLAY BOTH PARAMETERS              
         OC    RCLENDT,RCLENDT     ANY END DATE?                                
         BZ    DR300               NO SO DON'T WORRY                            
*                                                                               
DR230    TM    RCLDTEFG,RCLPERD    ANY PERIOD TYPE ?                            
         BNO   DR250               NO                                           
         TM    RCLDTEFG,RCLNROLL+RCLMON    PMON,PM1-PM12 OR P1-P99              
         BNZ   DR250                       YES                                  
         CLC   RCLSTDT,RCLENDT     MUST BE PED                                  
         BE    DR300               IF EQUAL DON'T DISPLAY                       
*                                                                               
DR250    SR    R2,R2                                                            
         CLC   RCLENDT,=XL2'8000'  DELIMETER FOR "AFTER"                        
         BNE   DR260                                                            
         MVC   APWORK+7(1),SCCOMMA                                              
         MVC   APWORK+8(5),AC@AFTER                                             
         B     DR300                                                            
*                                                                               
DR260    TM    RCLDTEFG,RCLMON+RCLNROLL                                         
         BNZ   DR300                                                            
         TM    RCLDTEFG,RCLPERD     IF PED THEN BRANCH                          
         BO    DR270                                                            
         TM    RCLDTEFG,RCLDAY                                                  
         BZ    DR300                                                            
*                                                                               
DR270    MVC   APWORK+7(1),SCCOMMA                                              
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
DR300    MVC   8(SFDDTERQ,R4),SPACES      INITIALIZE  COLDATE                   
*        _______________________   START                                        
         LA    R0,15                                                            
         LA    R1,APWORK           START OF STRING                              
         LA    R2,8(,R4)           START OF FIELD ON SCREEN                     
DR310    CLI   0(R1),C' '          SQUISH TOGETHER                              
         BNH   DR320               BLANK, NEXT CHARACTER                        
         MVC   0(1,R2),0(R1)       INSERT THE  CHARACTER                        
         LA    R2,1(,R2)           BUMP   UP   OUTPUT POINTER                   
DR320    LA    R1,1(,R1)           BUMP   UP   INPUT  POINTER                   
         BCT   R0,DR310                                                         
*        -----------------------   END                                          
*                                                                               
DR350    ICM   R4,15,ACOLWDTH      COLUMN WIDTH                                 
         BZ    DR360               NOT ON SCREEN                                
         OI    6(R4),FVOXMT               TRANSMIT                              
         SR    R1,R1                                                            
         IC    R1,RCLWDTH          CURRENT COLUMN WIDTH                         
         CVD   R1,APDUB            DISPLAY COLUMN WIDTH                         
         OI    APDUB+7,X'0F'              CHANGE  SIGN TO UNPACK                
         UNPK  8(SFDWDTHQ,R4),APDUB       UNPACK  TO SCREEN                     
         CLI   8(R4),C'0'                 SUPPRESS LEADING ZEROS                
         BNE   *+8                                                              
         MVI   8(R4),C' '                 CLEAR    LEADING ZERO                 
*                                                                               
DR360    ICM   R4,15,ACOLTOTL      TOTAL OPTION                                 
         BZ    DR500               NOT ON SCREEN                                
         OI    6(R4),FVOXMT               TRANSMIT                              
         MVI   8(R4),C' '                                                       
         TM    RCLOPT,RCLACCM      IS IT AN ACCUMULATED COLUMN?                 
         BO    DR460               YES SO GET OUT                               
         MVC   8(1,R4),APNO        SET OPTION TO NO                             
         TM    RCLOPT4,RCLAIDX     ACCENT INDEXING                              
         BZ    *+8                 NO                                           
         MVI   8(R4),C'I'          YES                                          
                                                                                
         TM    RCLOPT2,RCLTOT      TOTAL OPTION ON?                             
         BZ    DR500                                                            
         MVC   8(1,R4),APYES       SET OPTION TO YES                            
         B     DR500                                                            
*                                                                               
DR460    TM    RCLOPT,RCLNOTOT                                                  
         BZ    *+8                                                              
         MVI   8(R4),C'S'          SET TO SUPPRESS TOTAL IN COLUMN              
         TM    RCLOPT,SUPPRESS     SUPPRESSED DETAIL?                           
         BZ    *+10                                                             
         MVC   8(1,R4),APONLY                                                   
*                                                                               
DR500    ICM   R4,15,ACOLSORD      SORT ORDER OPTION                            
         BZ    DR520               NOT ON SCREEN                                
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         SR    R1,R1                                                            
         ICM   R1,1,RCLSORTN       VALUE TO DISPLAY ?                           
         BZ    DR520               NO                                           
         CVD   R1,APDUB            DISPLAY SORT OPTION                          
         OI    APDUB+7,X'0F'       CHANGE SIGN FOR UNPACK                       
         UNPK  8(SFDSORDQ,R4),APDUB                                             
         CLI   8(R4),C'0'          SUPPRESS LEADING ZEROS                       
         BNE   *+8                                                              
         MVI   8(R4),C' '          CLEAR    LEADING ZERO                        
*                                                                               
DR520    ICM   R4,15,ACOLCSTK      COL      STACK  UNDER                        
         BZ    DR550               NOT      ON     SCREEN                       
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         SR    R1,R1                                                            
         ICM   R1,1,RCLSTACK       VALUE    TO     DISPLAY ?                    
         BZ    DR550               NO,      SKIP                                
         MVC   8(1,R4),APCOLCHR    INSERT   COLUMN CHARACTER                    
         CVD   R1,APDUB            CONVERT  COLUMN STACK UNDER                  
         OI    APDUB+7,X'0F'       CHANGE   SIGN   FOR   UNPACK                 
         UNPK  9(SFDCSTKQ-1,R4),APDUB                                           
         CLI   9(R4),C'0'          REMOVE   LEADING      ZEROS                  
         BNE   DR550                                                            
*                                  MOVE     TRAILING     BYTE                   
         MVC   9(SFDCSTKQ-2,R4),10(R4)                                          
         MVI   7+SFDCSTKQ(R4),C' ' CLEAR    TRAILING     BYTE                   
*                                                                               
DR550    SR    RF,RF                                                            
         IC    RF,RCLDATLN                                                      
         LA    RE,RCLNDATA(RF)     POINT TO START OF HEADING 1                  
         IC    RF,RCLHD1LN         ANY COLUMN HEADINGS?                         
         ICM   R4,15,ACOLHD#1      HEADING COLUMN ON SCREEN                     
         BZ    DR552                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         SHI   RF,1                                                             
         BM    DR555               MUST NOT BE ANY HEADING                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),0(RE)                                                    
         AHI   RF,1                ADD BACK ONE                                 
*                                                                               
DR552    AR    RE,RF               POINT TO END OF ELEMENT                      
*                                                                               
DR555    ICM   R4,15,ACOLHD#2      HEADING COLUMN ON SCREEN                     
         BZ    DR600                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         ICM   RF,1,RCLHD2LN       IS THERE A 2ND COLUMN HEADING?               
         BZ    DR600               NO                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),0(RE)                                                    
*                                                                               
DR600    ICM   R4,15,ACOLPRNT      PRINT FIELD ON SCREEN ?                      
         BZ    DR620                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(1,R4),APYES       DEFAULT TO PRINT COLUMN                      
         TM    RCLOPT,HIDE         HIDDEN COLUMN?                               
         BZ    DR610                                                            
         MVC   8(1,R4),APNO                                                     
*&&US                                                                           
         TM    RCLOPT4,RCLXCOL                                                  
         BZ    *+12                                                             
         MVI   8(R4),C'X'                                                       
         B     DR620                                                            
*&&                                                                             
         TM    RCLOPT2,RCLCZERO                                                 
         BZ    DR610                                                            
         MVI   8(R4),C'H'                                                       
         B     DR620                                                            
*                                                                               
DR610    TM    RCLOPT2,RCLCZERO    IF COL=ZERO ELIMINATE RECORD                 
         BZ    *+8                                                              
         MVI   8(R4),C'C'                                                       
*                                                                               
         TM    RCLOPT,RCLMERGE     MERGE DATA IF SELECTED $COLS=ZERO            
         BZ    *+8                                                              
         MVI   8(R4),C'M'                                                       
*                                                                               
*        DECIMAL ROUNDING                                                       
DR620    ICM   R4,15,ACOLDECM      DECIMAL ROUNDING                             
         BZ    DR621                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(4,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DR621                                                            
*        CLI   RCLDCMLS,C' '                                                    
         CLI   RCLDCMLS,0                                                       
         BE    DR620C                                                           
         L     R3,=A(DECMTAB)                                                   
         A     R3,APRELO                                                        
DR620A   CLI   0(R3),X'FF'                                                      
         BE    DR620C                                                           
         CLC   RCLDCMLS(1),2(R3)                                                
         BE    DR620B                                                           
         LA    R3,DECMLEN(,R3)                                                  
         B     DR620A                                                           
DR620B   MVC   9(2,R4),3(R3)                                                    
         B     DR621                                                            
*                                                                               
DR620C   MVI   8(R4),C'('                                                       
         MVI   11(R4),C')'                                                      
         USING RPFELD,R1                                                        
         L     R1,ARPFELM                                                       
         MVC   9(1,R4),RPFRND                                                   
         TM    RCLOPT,RCLPCT                                                    
         BNO   DR621                                                            
         MVC   9(1,R4),RPFPCTS                                                  
*                                                                               
*        REPEAT CONSTANT                                                        
DR621    ICM   R4,15,ARPTCNSD      REPEAT CONSTANT                              
         BZ    DR622                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DR622                                                            
         TM    RCLOPT3,RCLRDATY                                                 
         BZ    DR621B                                                           
         MVC   9(L'APYES,R4),APYES                                              
         B     DR622                                                            
DR621B   TM    RCLOPT3,RCLRDATN                                                 
         BZ    DR621C                                                           
         MVC   9(L'APNO,R4),APNO                                                
         B     DR622                                                            
DR621C   MVC   9(L'APNO,R4),APNO                                                
*                                                                               
*        PRINT REDUNDANT INFO                                                   
DR622    ICM   R4,15,APRTRDND      PRINT REDUNDANT INFO                         
         BZ    DR623                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BO    DR623                                                            
         TM    RCLOPT3,RCLRDATY                                                 
         BZ    DR622B                                                           
         MVC   9(L'APYES,R4),APYES                                              
         B     DR623                                                            
*                                                                               
DR622B   TM    RCLOPT3,RCLRDATN                                                 
         BZ    DR622C                                                           
         MVC   9(L'APNO,R4),APNO                                                
         B     DR623                                                            
*                                                                               
DR622C   DS    0H                                                               
         MVI   8(R4),C'('                                                       
         MVC   9(L'APNO,R4),APNO                                                
         MVI   10(R4),C')'                                                      
*                                                                               
         USING RPFELD,R1                                                        
         L     R1,ARPFELM                                                       
         TM    RPFPOPT,RPFRDAT                                                  
         BNO   DR623                                                            
         MVC   9(L'APYES,R4),APYES                                              
*                                                                               
DR623    ICM   R4,15,ACOLNGAM      NEGATIVE AMOUNTS                             
         BZ    DR624                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DR624                                                            
         MVI   9(R4),C'T'                                                       
         TM    RCLEDOPT,RCLEDTRL                                                
         BO    DR624                                                            
         MVI   9(R4),C'B'                                                       
         TM    RCLEDOPT,RCLEDBKT                                                
         BO    DR624                                                            
         MVI   9(R4),C'L'                                                       
         TM    RCLEDOPT,RCLEDLED                                                
         BO    DR624                                                            
         MVI   9(R4),C'S'                                                       
         TM    RCLEDOPT,RCLEDCR                                                 
         BO    DR624                                                            
*                                                                               
         MVI   8(R4),C'('                                                       
         MVC   9(L'APNO,R4),APNO                                                
         MVI   10(R4),C')'                                                      
*                                                                               
         USING RPFELD,R1                                                        
         L     R1,ARPFELM                                                       
         MVI   9(R4),C'T'                                                       
         TM    RPFEDOPT,RPFEDTRL                                                
         BO    DR624                                                            
         MVI   9(R4),C'B'                                                       
         TM    RPFEDOPT,RPFEDBKT                                                
         BO    DR624                                                            
         MVI   9(R4),C'L'                                                       
         TM    RPFEDOPT,RPFEDLED                                                
         BO    DR624                                                            
         MVI   9(R4),C'S'                                                       
         TM    RPFEDOPT,RPFEDCR                                                 
         BO    DR624                                                            
         MVI   9(R4),C' '                                                       
*                                                                               
DR624    ICM   R4,15,ACOLPRCM      PRINT COMMAS                                 
         BZ    DR625                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DR625                                                            
         TM    RCLEDOPT,RCLEDCMY                                                
         BZ    DR624B                                                           
         MVC   9(L'APYES,R4),APYES                                              
         B     DR625                                                            
*                                                                               
DR624B   TM    RCLEDOPT,RCLEDCMN                                                
         BZ    DR624D                                                           
         MVC   9(L'APNO,R4),APNO                                                
         B     DR625                                                            
*                                                                               
DR624D   DS    0H                                                               
         MVI   8(R4),C'('                                                       
         MVC   9(L'APNO,R4),APNO                                                
         MVI   10(R4),C')'                                                      
*                                                                               
         USING RPFELD,R1                                                        
         L     R1,ARPFELM                                                       
         TM    RPFEDOPT,RPFEDCMA                                                
         BNO   DR625                                                            
         MVC   9(L'APYES,R4),APYES                                              
*                                                                               
DR625    ICM   R4,15,ACOLPRZR      PRINT ZERO AMOUNTS                           
         BZ    DR626                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DR626                                                            
         TM    RCLEDOPT,RCLEDZRY                                                
         BZ    DR625B                                                           
         MVC   9(L'APYES,R4),APYES                                              
         B     DR626                                                            
DR625B   TM    RCLEDOPT,RCLEDZRN                                                
         BZ    DR625D                                                           
         MVC   9(L'APNO,R4),APNO                                                
         B     DR626                                                            
*                                                                               
DR625D   DS    0H                                                               
         MVI   8(R4),C'('                                                       
         MVC   9(L'APNO,R4),APNO                                                
         MVI   10(R4),C')'                                                      
*                                                                               
         USING RPFELD,R1                                                        
         L     R1,ARPFELM                                                       
         TM    RPFEDOPT,RPFEDZRO                                                
         BNO   DR626                                                            
         MVC   9(L'APYES,R4),APYES                                              
*                                                                               
DR626    ICM   R4,15,ACOLUNLN      UNDERLINE                                    
         BZ    DR627               NOT ON SCREEN                                
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES      CLEAR FIELD                                  
         TM    RCLOPT,RCLACCM      AMOUNT COLUMN ?                              
         BZ    DR627               NO                                           
         TM    RCLOPT3,RCLUNLNA                                                 
         BZ    DR626B                                                           
         MVI   9(R4),C'A'                                                       
         TM    RCLOPT3,RCLUNLDB    DOUBLE UNDERLINE ?                           
         BZ    DR627               NO                                           
         MVI   9(R4),C'D'                                                       
         B     DR627                                                            
*                                                                               
DR626B   TM    RCLOPT3,RCLUNLNB                                                 
         BZ    DR626C                                                           
         MVI   9(R4),C'B'                                                       
         TM    RCLOPT3,RCLUNLDB    DOUBLE UNDERLINE ?                           
         BZ    DR627               NO                                           
         MVI   9(R4),C'E'                                                       
         B     DR627                                                            
*                                                                               
DR626C   TM    RCLOPT3,RCLUNLNN                                                 
         BZ    DR626D                                                           
         MVC   9(1,R4),APNO                                                     
         B     DR627                                                            
*                                                                               
         USING RPFELD,R1                                                        
DR626D   DS    0H                                                               
         L     R1,ARPFELM                                                       
         MVC   8(3,R4),=C'(B)'     SET UP DEFAULTS                              
         TM    RPFPOPT3,RPFDBUL    DOUBLE UNDERLINE ?                           
         BZ    *+8                 NO                                           
         MVI   9(R4),C'D'          YES                                          
*                                                                               
         TM    RPFPOPT3,RPFNOUL                                                 
         BNO   DR626E                                                           
         MVC   9(1,R4),APNO                                                     
         B     DR627                                                            
*                                                                               
DR626E   TM    RPFPOPT3,RPFTPUL                                                 
         BNO   DR627                                                            
         MVI   9(R4),C'A'                                                       
         TM    RPFPOPT3,RPFDBUL    DOUBLE UNDERLINE ?                           
         BZ    *+8                 NO                                           
         MVI   9(R4),C'E'          YES                                          
*                                                                               
DR627    ICM   R4,15,ACOLPZTT      PRINT ZERO TOTALS                            
         BZ    DR629                                                            
         OI    6(R4),FVOXMT        RE-TRANSMIT                                  
         MVC   8(3,R4),SPACES                                                   
         TM    RCLOPT,RCLACCM                                                   
         BZ    DR629                                                            
         TM    RCLOPT4,RCLZRTTY                                                 
         BZ    DR627B                                                           
         MVC   9(L'APYES,R4),APYES                                              
         B     DR629                                                            
DR627B   TM    RCLOPT4,RCLZRTTN                                                 
         BZ    DR627D                                                           
         MVC   9(L'APNO,R4),APNO                                                
         B     DR629                                                            
*                                                                               
DR627D   DS    0H                                                               
         MVI   8(R4),C'('                                                       
         MVC   9(L'APNO,R4),APNO                                                
         MVI   10(R4),C')'                                                      
*                                                                               
         USING RPFELD,R1                                                        
         L     R1,ARPFELM                                                       
         TM    RPFPOPT,RPFZEROT                                                 
         BNO   DR629                                                            
         MVC   9(L'APYES,R4),APYES                                              
*                                                                               
         USING MNOELD,R2                                                        
DR629    SR    RF,RF               CLEAR REGISTER                               
         L     R2,AIOAREA1         ->    RECORD                                 
         AH    R2,DATADISP         ->    1ST  ELEMENT                           
*                                                                               
DR630    CLI   0(R2),0             END   OF   RECORD ?                          
         BE    DR650               YES,  GET  NEXT COLUMN                       
         CLI   MNOEL,MNOELQ        MESSAGE    ELEMENT ?                         
         BH    DR650               HIGH, GET  NEXT COLUMN                       
         BNE   DR640               NO,   GET  NEXT ELEMENT                      
         OI    MSGELSW,MSGELFND    MESSAGE    ELEMENT   FOUND                   
         CLC   MNOSEQ,RCLSEQ       MATCH ON   COLUMN ?                          
         BH    DR650               HIGH, GET  NEXT COLUMN                       
         BNE   DR640               NO,   GET  NEXT ELEMENT                      
         CLI   MNOSTYPE,MNOSTCOL   COLUMN     TYPE MESSAGE ?                    
         BNE   DR640               NO,   GET  NEXT ELEMENT                      
*                                                                               
*                                  JUST  DISPLAY   THE  1ST  MSG  FOUND         
*                                  WHICH MESSAGE   TYPE ?                       
         L     R4,ACOLNUM                                                       
         CLI   MNOMTYPE,MNOMTERR   ERROR ?                                      
         BNE   *+10                                                             
         MVC   8(1,R4),APERROR     MOVE ERROR   INDICATION INTO COL NUM         
         CLI   MNOMTYPE,MNOMTWRN   WARNING ?                                    
         BNE   *+10                                                             
         MVC   8(1,R4),APWARN      MOVE WARNING INDICATION INTO COL NUM         
         CLI   DSRNMTYP,C' '       ANY   PREVIOUS  MSG  TYPE                    
         BNE   DR650               YES,  GET  NEXT COLUMN                       
         MVC   DSRNMTYP,MNOMTYPE   NO,   SAVE MSG  TYPE                         
         MVC   DSRNMNUM,MNOMNUM          SAVE MSG  NUMBER                       
         MVC   DSRNCOL,MNOSEQ            SAVE COLUMN    NUMBER                  
         MVI   DSRNDTYP,0                ASSUME    NO   MSG  DATA               
         MVC   DSRNDATA,SPACES           CLEAR     MSG  DATA                    
         IC    RF,MNOMLN           GET   MSG  LENGTH                            
         SHI   RF,(MNOMELQ+L'MNOMDTYP)                                          
         LTR   RF,RF               ANY   MSG  DATA ?                            
         BM    DR650               NO,   GET  NEXT COLUMN                       
         STC   RF,DSRNDLNG         YES,  SAVE DATA LENGTH                       
         MVC   DSRNDTYP,MNOMDTYP         SAVE DATA TYPE                         
         BCTR  RF,0                                                             
         EXMVC RF,DSRNDATA,MNOMDATA      SAVE MSG  DATA                         
         B     DR650               GET   NEXT COLUMN                            
*                                                                               
DR640    IC    RF,MNOLN            ELEMENT    LENGTH                            
         AR    R2,RF               ->    NEXT ELEMENT                           
         B     DR630                                                            
         DROP  R2                                                               
*                                                                               
DR650    BRAS  RE,BUMPLINE         NEXT COLFLD ENTRY                            
*                                                                               
DR700    GOTO1 NEXTEL,(R8)                                                      
         BNE   DR900                                                            
         LR    R8,R1                                                            
         CLC   ACOLNUM,AENDCOL     ARE WE AT END OF COLUMN SCREEN               
         BL    DR050               LOOP TO DISPLAY NEXT                         
         DROP  R8                                                               
         EJECT ,                                                                
         USING RCLELD,R1                                                        
DR900    DS    0H                                                               
         MVI   APELCODE,RCLELQ     X'C3'                                        
         CLI   APPFKEY,PFK04       COLUMN FILTER REQUESTED?                     
         BNE   DR905                                                            
         CLI   FXDCURSR,YES        FIXED THE CURSOR ?                           
         BE    IVALNCLM            YES, INVALID COLUMN NUMBER                   
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
         L     R1,AIOAREA1                                                      
         GOTO1 GETEL                                                            
         BNE   DR930                                                            
         LA    R2,1(,R2)           FIRST BOX LINE WIDTH                         
*                                                                               
DR910    SR    RF,RF                                                            
         LA    R3,1(,R3)           COUNT THE NUM OF COLUMNS IN REPORT           
         ICM   RF,1,RCLWDTH        CURRENT COLUMN WIDTH                         
         BZ    DR920               NO WIDTH                                     
         TM    RCLOPT,RCLHIDE      HIDDEN COLUMN ?                              
         BO    DR920               YES, COLUMN TAKES NO SPACE                   
         CLI   RCLSTACK,0          STACKED UNDER ANOTHER COLUMN ?               
         BNE   DR920               YES, COLUMN TAKES NO NEW SPACE               
         LA    R2,1(RF,R2)         ADD UP WIDTHS + ONE FOR COLUMN LINE          
*                                                                               
DR920    GOTO1 NEXTEL                                                           
         BE    DR910               LOOP TO ADD IN NEXT COLUMN WIDTH             
*                                                                               
DR930    ICM   R4,15,ANUMCOLS      NUMBER OF COLUMNS                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         XC    8(SFD#COLQ,R4),8(R4)                                             
         OI    6(R4),FVOXMT                                                     
         CVD   R3,APDUB                                                         
         OI    APDUB+7,X'0F'              CHANGE SIGN FOR UNPACK                
         UNPK  8(SFD#COLQ,R4),APDUB       UNPACK TO SCREEN                      
*                                                                               
         CLI   8(R4),C'0'                                                       
         BNE   DR940                                                            
         MVI   8(R4),C' '                                                       
*                                                                               
DR940    ICM   R4,15,AREPWDTH      REPORT WIDTH                                 
         BNZ   *+6                                                              
         DC    H'00'                                                            
         XC    8(SFDRPWDQ,R4),8(R4)                                             
         OI    6(R4),FVOXMT                                                     
         CVD   R2,APDUB                                                         
         OI    APDUB+7,X'0F'             CHANGE SIGN FOR UNPACK                 
         UNPK  8(SFDRPWDQ,R4),APDUB       UNPACK TO SCREEN                      
*                                                                               
         LA    RE,SFDRPWDQ-1             CHANGE LEADING ZEROS                   
         LA    RF,8(,R4)                  TO BLANKS                             
*                                                                               
DR945    CLI   0(RF),C'0'                                                       
         BNE   DR950                                                            
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         BCT   RE,DR945                                                         
*                                                                               
DR950    MVC   FVMSGNO,DRMSGNO     PICK UP   ANY  DISPLAY   ERRORS              
         MVC   FVADDR,DRCURSOR                                                  
*                                                                               
         CLI   APACTN,ACTCPY       DON'T SET CURSOR FOR ACTION COPY             
         BE    DR990                                                            
         MVC   APCURSOR,SVCURSOR                                                
         OC    SVCURSOR,SVCURSOR   WAS CURSOR SAVED OFF                         
         BNZ   DR990                                                            
         L     R4,ACURCOL          CURRENT LINE CURSOR IS ON                    
         ZIC   RF,0(,R4)                                                        
         AR    R4,RF               POINT TO DATA FIELD                          
         ST    R4,APCURSOR         PUT CURSOR HERE                              
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERROR OCCURED ?                         
         BE    DR990               YES, USE FVADDR                              
         XC    APCURSOR,APCURSOR   CLEAR CURSOR LOCATION, USE FVADDR            
*                                                                               
DR990    CLI   APMODE,APMDELR      DELETED COLUMN ELEMENTS?                     
         BE    IVALCDEL                                                         
*                                                                               
DR992    CLC   FVMSGNO,=AL2(FVFOK) ANY  ERROR     OCCURED ?                     
         BNE   DR995               YES, SKIP                                    
         TM    MSGELSW,MSGELFND    ANY  MESSAGE   ELEMENT   FOUND ?             
         BZ    DR995               NO,  SKIP                                    
         CLI   DSRNMTYP,C' '       ANY  PREVIOUS  MSG  TYPE                     
         BNE   DR993               YES, GENERATE  MSG                           
         MVI   DSRNMTYP,MNOMTWRN   SET  WARNING   MSG                           
         MVC   DSRNMNUM,=AL2(ACWMGEN)   MESSAGE   GENERATED                     
         MVI   DSRNCOL,1           SET  FIRST     COLUMN                        
         MVI   DSRNDLNG,0          NO   EXTRA     DATA                          
*                                                                               
DR993    OI    MSGELSW,MSGELERR    SET  MESSAGE   EL   ERROR                    
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
DR994    CLI   DSRNDLNG,0          ANY  DELAYED   MSG  DATA ?                   
         BNE   IVALWDAT            YES, OUTPUT    WITH DATA                     
         CLI   DSRNMTYP,MNOMTERR   IS   IT   MSG  TYPE ERROR ?                  
         BE    *+10                YES, SKIP                                    
         MVC   FVOMTYP,DSRNMTYP    SET  MSG  TYPE                               
         B     IVALCOL0            SET  CURSOR                                  
*                                                                               
DR995    NI    MSGELSW,TURNOFF-MSGELIDR   TURN OFF 'IN DISPLAY ROUTINE'         
         SR    RE,RE                      SET CONCODE TO YES                    
DRXIT    XIT1                             RETURN                                
         DROP  R1                                                               
         EJECT ,                                                                
         USING RFLELD,R2                                                        
         USING BUDRECD,R3                                                       
DISBUD   NTR1                                                                   
         MVC   SVKEY,IOKEY                                                      
         MVC   APWORK,SPACES                                                    
         LA    R3,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY                                      
         MVC   BUDKNO1,RFLDATA                                                  
         MVC   APHALF,RFLDATA      SAVE    DESIRED   BUDGET   NUMBER            
         L     R3,AIOAREA2                                                      
         GOTO1 AIO,IO2+IOACCFIL+IOHID                                           
         CLC   APHALF,BUDKNO1                                                   
         BE    DISBUD20            FOUND BUDGET                                 
*                                                                               
DISBUD10 MVC   DRMSGNO,=AL2(ACEBUD)        INVALID BUDGET TYPE                  
         TM    IOERR,IOEDEL                IS IT DELETED                        
         BZ    *+10                                                             
         MVC   DRMSGNO,=AL2(FVFRDEL)       RECORD DELETED                       
         ST    R4,DRCURSOR                 SET  DISPLAY CURSOR                  
**********************************************************************          
* IF BUDGET WAS DELETED THEN DISPLAY NUMBER INSTEAD OF CODE          *          
**********************************************************************          
         SPACE 1                                                                
         LA    RF,APWORK           ->      WORK AREA                            
         SR    R0,R0               CLEAR   REGISTER                             
         ICM   R0,3,APHALF         GET     THE  BUDGET   NUMBER                 
         CVD   R0,APDUB            CONVERT IT   TO       PACKED DECIMAL         
         OI    APDUB+7,X'0F'       SETUP   FOR  DISPLAY                         
         UNPK  0(5,RF),APDUB       CONVERT TO   DISPLAY                         
         MVC   5(2,RF),=C' ?'      SAY     SOMETHING IS  WRONG                  
         B     DISBUD30            DISPLAY IT                                   
*                                                                               
DISBUD20 LA    RF,APWORK                                                        
         MVC   0(L'BUDKCOD,RF),BUDKCOD                                          
*                                                                               
DISBUD30 L     R4,ACOLBUDG         BUDGET AREA                                  
         MVC   8(SFDBUDGQ,R4),APWORK                                            
         OI    6(R4),FVOXMT        TRANSMIT                                     
         MVC   IOKEY,SVKEY                                                      
         B     DRXIT                                                            
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  CHECK PFKEYS FOR FOR COLUMN SCROLLING                              *         
***********************************************************************         
         SPACE 1                                                                
SCROLLIT NTR1                                                                   
         TM    SCRSRLH+4,FVITHIS   ANY INPUT THIS TIME?                         
         BNZ   SCIT10              YES                                          
         MVC   SCRSRL,SRLCOL       RESET FROM PREVIOUS TIME                     
         OI    SCRSRLH+6,FVOXMT                                                 
*                                                                               
SCIT10   GOTO1 AFVAL,SCRSRLH                                                    
         CLI   FVILEN,0            ANY  DATA ?                                  
         BNE   SCIT12              YES, SKIP                                    
         MVC   SCRSRL,=C'1   '     NO,  DEFAULT TO 1                            
         MVI   FVILEN,1            LNG= 1                                       
         MVI   FVXLEN,0            EXMVC LENGTH 0                               
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
*                                                                               
SCIT12   DS    0H                                                               
         ZIC   R3,FVXLEN           GET   EXECUTE LENGTH                         
         MVC   SCROLL,=Y(CLIMIT/2)                                              
         EX    R3,*+8              COMPARE FOR INPUT LENGTH                     
         B     *+10                IS    IT   HALF ?                            
         CLC   SCRSRL(0),AC@HALF                                                
         BNE   SCIT14              NO,   TRY  AGAIN                             
         MVC   SCRSRL,AC@HALF      DISPLAY    HALF                              
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         B     SCIT30                                                           
*                                                                               
SCIT14   DS    0H                                                               
         MVC   SCROLL,=Y(CLIMIT)                                                
         EX    R3,*+8              COMPARE FOR INPUT LENGTH                     
         B     *+10                IS    IT   PAGE ?                            
         CLC   SCRSRL(0),AC@PAGE                                                
         BNE   SCIT16              NO,   TRY  AGAIN                             
         MVC   SCRSRL,AC@PAGE      DISPLAY    PAGE                              
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         B     SCIT30                                                           
*                                                                               
SCIT16   DS    0H                  VALIDATE NUMBER                              
         SR    R3,R3                                                            
         IC    R3,FVILEN                                                        
         GOTO1 VCASHVAL,APPARM,(C'N',SCRSRL),(R3)                               
         CLI   APPARM,0                                                         
         BNE   SCITER                                                           
         CLC   APPARM+4(4),=A(CLIMIT)                                           
         BH    SCITER                                                           
         MVC   SCROLL,APPARM+6                                                  
         B     SCIT30                                                           
*                                                                               
SCITER   MVC   DRMSGNO,=AL2(FVFNOTV)                                            
         LA    R1,SCRSRLH          ->   SCROLL HEADER                           
         ST    R1,DRCURSOR         SAVE IN DISPLAY CURSOR ADDRESS               
         MVC   SCRSRL,=C'1   '     USE  DEFAULT VALUE                           
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         MVC   SCROLL,=H'01'                                                    
*                                                                               
SCIT30   MVC   SRLCOL,SCRSRL       SAVE CURRENT COLUMN SCROLL VALUE             
         SR    R1,R1                                                            
         ICM   R1,1,RESETSEQ       GET START SEQUENCE                           
         BNZ   SCIT60                                                           
*                                                                               
SCIT40   LA    R1,1                START A SEQUENCE 1                           
         B     SCIT90                                                           
*                                                                               
SCIT60   CLI   APPFKEY,PFKDOWN     SCROLL DOWN?                                 
         BNE   SCIT80                                                           
         AH    R1,SCROLL           BUMP UP BY ONE                               
         B     SCIT90                                                           
*                                                                               
SCIT80   CLI   APPFKEY,PFKUP       SCROLL UP?                                   
         BNE   DRXIT               DON'T DO ANY SCROLLING                       
         SH    R1,SCROLL           BUMP DOWN BY ONE                             
*                                                                               
SCIT90   CHI   R1,1                                                             
         BL    SCIT40              FORCE ONE INTO R1                            
         CHI   R1,MAXSCRLL         GREATEST SCROLL NUMBER                       
         BL    *+8                                                              
         LA    R1,MAXSCRLL                                                      
         MVI   APPFKEY,0           CLEAR PFKEY                                  
         STC   R1,RESETSEQ         START AT COLUMN ELEMENT SEQUENCE             
         B     DRXIT                                                            
         EJECT ,                                                                
IVALWDAT DS    0H                          INSERT WITH DATA                     
         LHI   R4,-(X'FFFF'-FVFGTSET+1)    OVER-RIDE FVERR CALL                 
         SR    RF,RF                                                            
         IC    RF,ERRCOL#          INSERT ERROR COLUMN NUMBER                   
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         XC    APXTRA,APXTRA                                                    
         MVI   APXTRA,3                                                         
         UNPK  APXTRA+1(2),APDUB                                                
         CLI   DSRNDTYP,MNOMDCOL   COLUMN TYPE DATA                             
         BNE   IVALWDA1            NO,    SKIP                                  
         MVI   APXTRA+3,3          INSERT EXTRA COLUMN NUMBER                   
         IC    RF,DSRNDATA                                                      
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APXTRA+4(2),APDUB                                                
         B     IVALWDA3            GENERATE THE MESSAGE                         
*                                                                               
IVALWDA1 DS    0H                                                               
         IC    RF,DSRNDLNG                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,APXTRA+3         EXTRA DATA LENGTH + 1                        
         SHI   RF,1                                                             
         LA    RE,APXTRA+4         CAN'T USE EX INSTRUCTION HERE                
         LA    R1,DSRNDATA                                                      
*                                                                               
IVALWDA2 MVC   0(1,RE),0(R1)       INSTERT EXTRA TEST DATA                      
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BRCT  RF,IVALWDA2                                                      
*                                                                               
         USING GETTXTD,R1                                                       
IVALWDA3 LA    R1,APPARM                                                        
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTAOUT,AMSGHDR+1                                                 
         MVC   GTMSYS,ASSYSO       NO,  SET NATIVE SYSTEM                       
         MVC   GTMSGNO,DSRNMNUM    MESSAGE NUMBER                               
         MVC   GTMTYP,DSRNMTYP     MESSAGE TYPE                                 
         LA    RE,APXTRA                                                        
         STCM  RE,7,GTASUBST       APPEND &1 AND &2 CALL                        
         B     IVALCOL0            SET  CURSOR                                  
         DROP  R1                                                               
                                                                                
IVALNCLM MVI   COLUMN#,0                                                        
         LHI   R4,ACEIVCN          INVALID COLUMN NUMBER                        
         MVC   FVADDR,AACTHDR      PUT  CURSOR AT ACTION FIELD                  
         B     IVALMSG2                                                         
                                                                                
IVALCDEL LHI   R4,ACICLDEL         DELETED COLUMN ELEMENTS                      
         MVI   FVOMTYP,GTMINF      INFORMATION TYPE                             
         MVC   FVADDR,AACTHDR      PUT CURSOR AT ACTION FIELD                   
         B     IVALMSG2                                                         
                                                                                
IVALCOL0 SR    R1,R1                                                            
         IC    R1,ERRCOL#                                                       
         BCTR  R1,0                                                             
         B     IVALCOL2                                                         
                                                                                
IVALCOL1 LHI   R4,ACEIVCN          INVALID  COLUMN  NUMBER                      
         ZIC   R1,COLUMN#          POINT TO CORRECT COLUMN                      
         ZIC   R0,STSEQ            SUBTRACT START   SEQUENCE NUMBER             
         SR    R1,R0                                                            
                                                                                
IVALCOL2 ZIC   R3,STSEQ            MAKE SURE WE ENDUP ON SCREEN                 
         BCTR  R3,0                                                             
         SR    R1,R3                                                            
         BNM   *+6                 IF LOW USE START SEQUENCE                    
         SR    R1,R1                                                            
         CHI   R1,(CLIMIT-1)       IF HIGH USE LAST COLUMN                      
         BNH   *+8                                                              
         LA    R1,CLIMIT-1                                                      
*                                                                               
         MH    R1,NEXTCOL          NOW ON SCREEN FIND CURSOR  ADDRESS           
         A     R1,ACOLFRST                                                      
         SR    RF,RF                                                            
         IC    RF,0(,R1)           BUMP TO COLUMN DATA FIELD                    
         AR    R1,RF                                                            
         ST    R1,FVADDR                                                        
*                                                                               
IVALMSG2 CLC   FVMSGNO,=AL2(FVFOK) WAS ERROR SET ALREADY ? (FVFOK)              
         BNE   *+8                 YES                                          
         STCM  R4,3,FVMSGNO        NO, SO SET ERROR STORED IN R9                
         XC    APCURSOR,APCURSOR   CLEAR APPLICATION CURSOR                     
         B     DRXIT               NO,  SO EXIT VALREC, RB OK                   
         EJECT ,                                                                
         LTORG                                                                  
         DROP  R5,R9,RA,RC                                                      
         EJECT ,                                                                
***********************************************************************         
*  INSERT COLUMN AND MESSAGE ELEMENTS AND ADJUST COLUMN CALCULATION   *         
*  TYPE COLUMNS                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R1                                                        
         USING LWSD,RC                                                          
INSCOLMN NMOD1 0,**ICOL**                                                       
         L     RC,APALOCAL                                                      
         STC   R1,INSCOL#          COLUMN NUMBER INSERTING BETWEEN              
         SR    R2,R2                                                            
         IC    R2,INSSEQ                                                        
         GOTO1 =A(FIXCALC),APPARM,(R2),1,RR=APRELO                              
         GOTO1 =A(FIXXTREL),APPARM,(R2),1,RR=APRELO                             
         GOTO1 =A(FIX5TH),APPARM,(R2),1,RR=APRELO                               
         GOTO1 =A(FIXPROF),APPARM,(R2),1,RR=APRELO                              
         BNE   INSCOLEX                                                         
         GOTO1 =A(FIXMSGS),APPARM,(R2),1,NO,RR=APRELO                           
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
         USING MNOELD,R1           MESSAGE ELEMENTS                             
INSCOL30 L     R1,AIOAREA1                                                      
         MVI   APELCODE,MNOELQ     MESSAGE ELEMENTS X'C9'                       
         GOTO1 GETEL                                                            
         BNE   INSCOL80                                                         
         SR    R2,R2                                                            
*                                                                               
INSCOL35 CLC   INSCOL#,MNOSEQ      SHOULD WE UPDATE THIS ELEMENT ?              
         BH    INSCOL40            NO,  NEXT ELEMENT                            
         IC    R2,MNOSEQ           BUMP UP COLUMN #S                            
         LA    R2,1(,R2)                                                        
         STC   R2,MNOSEQ           NEW  NUMBER                                  
*                                                                               
INSCOL40 GOTO1 NEXTEL              GET  NEXT ELEMENT                            
         BE    INSCOL35                                                         
*                                                                               
INSCOL80 IC    RF,LSTCOL#          NUMBER  OF      COLUMNS  ON   RECORD         
         LA    RF,1(,RF)           ONE     MORE    COLUMN   LEFT                
         STC   RF,LSTCOL#          SAVE    NUMBER  OF       COLUMNS             
*                                                                               
INSCOL90 MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
*                                                                               
INSCOLEX CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION        CODE                
         XIT1  ,                   EXIT                                         
         DROP  R1,RC                                                            
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
         USING LWSD,RC                                                          
         USING RCLELD,R2                                                        
VALEXP   NMOD1 0,**VEXP**                                                       
         L     RC,APALOCAL                                                      
         L     R4,0(,R1)           DATA                                         
         L     R8,4(,R1)           LENGTH OF DATA                               
         L     R2,8(,R1)           COLUMN ELEMENT                               
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
         AHI   R0,1                BALANCE PARENTHESIS +1                       
         SR    R8,R4                                                            
         BNP   VALEXPX                                                          
*                                                                               
         GOTO1 VALEXP,APPARM,(R4),(R8)     RECURSIVE CALL                       
         BNE   VALEXPX                     NOT OK SO EXIT                       
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
VALEXP35 LA    R5,1(,R5)           BUMP TO NEXT VALUE IN EXPRESSION             
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
         BNZ   VALEXPX             CAN'T BE CALC COL AND NON CALC COL           
         MVC   0(1,RF),CURCOL      SAVE OFF COLUMN NUMBER                       
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
         CLC   CURCOL,RANKCOL                                                   
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
         BNZ   VALEXPX             ALREADY HAVE VERTICAL %                      
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
         LA    R6,1(R6)                                                         
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
         MVC   0(1,RF),CURCOL                                                   
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
*                                                                               
OPTABLE  DC    C'+-*X/%V)'                                                      
         DC    AL1(EOT)                                                         
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DELETE COLUMN AND MESSAGE ELEMENTS AND ADJUST COLUMN CALCULATION   *         
*  TYPE COLUMNS                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LWSD,RC                                                          
DELCOLMN NMOD1 0,**DELC**                                                       
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
DELCOL10 DS    0H                                                               
         BNE   DELCOL20            NOT     FOUND,  CONTINUE                     
         CLC   MNOSEQ,DELCOL#      FOUND   RIGHT   COLUMN   NUMBER ?            
         BNE   DELCOL20            NO,     CONTINUE                             
         CLI   MNOSTYPE,MNOSTCOL   COLUMN  TYPE    ELEMENT ?                    
         BNE   *+8                 NO,     TRY     AGAIN                        
         MVI   MNOEL,X'FF'         YES,    DELETE  IT                           
         GOTO1 NEXTEL              GET     NEXT    ELEMENT                      
         B     DELCOL10            CHECK   NEXT    ELEMENT                      
*                                                                               
DELCOL20 DS    0H                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      DELETE  X'FF'   ELEMENTS                     
         GOTO1 DELEL                                                            
         OI    MSGELSW,MSGELDEL    TURN    ON      COLUMN   DELETED SW          
         DROP  R1                                                               
*                                                                               
         CLC   DELCOL#,LSTCOL#                                                  
         BH    DELCOL30            NOTHING LEFT    TO       ADJUST              
         SR    R3,R3                                                            
         IC    R3,DELCOL#          COLUMN  DELETED                              
         GOTO1 =A(FIXXTREL),APPARM,(R3),-1,RR=APRELO                            
         GOTO1 =A(FIX5TH),APPARM,(R3),-1,RR=APRELO                              
         GOTO1 =A(FIXCALC),APPARM,(R3),-1,RR=APRELO                             
         GOTO1 =A(FIXPROF),APPARM,(R3),-1,RR=APRELO                             
         BNE   DELCOL90                                                         
         GOTO1 =A(FIXMSGS),APPARM,(R3),-1,YES,RR=APRELO                         
*                                                                               
DELCOL30 DS    0H                                                               
         ZIC   RF,LSTCOL#          NUMBER  OF      COLUMNS  ON   RECORD         
         BCTR  RF,0                ONE     LESS    COLUMN   LEFT                
         STC   RF,LSTCOL#                                                       
*                                                                               
DELCOL80 MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
*                                                                               
DELCOL90 CLC   FVMSGNO,=AL2(FVFOK) SET     CONDITION        CODE                
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE COLUMN DATE CALCULATIONS                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R1                                                       
         USING LWSD,RC                                                          
         SPACE 1                                                                
DTECOMP  NMOD1 0,**DTEC**                                                       
         L     RC,APALOCAL                                                      
         MVC   FVXTRA,SPACES                                                    
         MVI   CPARMS,1            NUMBER OF PARAMETERS                         
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
         CLI   CPARMS,2                                                         
         BH    DTECMP99            ERROR                                        
         MVC   FVMSGNO,=AL2(ACE2FEW)                                            
         BL    DTECMP99            ERROR                                        
         MVI   CPARMS,X'FF'        SIGNAL FINISHED                              
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
         SHI   R2,1                                                             
         BM    DTECMP99            ERROR NO DATA                                
         EXMVC R2,DUMFLD,0(R3)                                                  
         GOTO1 VALDEF,DUMFLDH                                                   
         MVC   FVMSGNO,=AL2(FVFKYWD)                                            
         BNE   DTECMP85            ERROR NOT A VALID KEYWORD                    
         TM    DEFTYPE,DEFDTE1     DATE TYPE?                                   
         BZ    DTECMP99            NO SO ERROR                                  
         LA    R6,1(,R6)                                                        
         LR    R3,R6                                                            
         CLI   CPARMS,X'FF'                                                     
         BE    DTECMP90            OK                                           
         SR    R1,R1                                                            
         IC    R1,CPARMS           NUMBER OF PARAMETERS SO FAR                  
         LA    R1,1(,R1)           INCREASE BY ONE                              
         STC   R1,CPARMS           SAVE                                         
         B     DTECMP08                                                         
*                                                                               
DTECMP85 LTR   R1,R1               IS   THIS A KEYWORD                          
         BZ    DTECMP99            NO,  EXIT                                    
         MVC   FVMSGNO,=AL2(ACEKYWDI)                                           
         B     DTECMP99            KEYWORD NOT VALID                            
*                                                                               
DTECMP90 SR    RC,RC               OK                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DTECMP99 LTR   RC,RC               NOT OK                                       
*                                                                               
         XMOD1 ,                                                                
         DROP  R1,RC                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CHARACTER SET USED FOR AN EXPRESSION                      *         
***********************************************************************         
         SPACE 1                                                                
         USING LWSD,RC                                                          
         SPACE 1                                                                
CHKEXP   NMOD1 0,**CEXP**                                                       
         L     RC,APALOCAL                                                      
         LA    R1,SFDDATAQ         LENGTH TO SQUISH                             
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
*        ________________________                                               
         LA    R6,CHARSET                                                       
CHKEXP20 CLI   0(R6),EOT                                                        
         BE    CHKEXP40            BAD EXPRESSION                               
         CLC   0(1,R2),0(R6)       LOOK FOR ANY OPERATOR                        
         BNE   CHKEXP30            POSSIBLE EXPRESSION                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
CHKEXP30 LA    R6,1(,R6)           BUMP UP BY ONE                               
         B     CHKEXP20                                                         
*        ------------------------                                               
CHKEXP40 LA    R2,1(,R2)                                                        
         BCT   R1,CHKEXP10                                                      
         B     CHKEXP90                                                         
*                                                                               
CHKEXP90 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    CHKEXPX                                                          
         MVC   FVXTRA(SFDDATAQ),APWORK                                          
         B     CHKEXPNE                                                         
*                                                                               
CHKEXPX  LA    RE,APWORK                                                        
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
*        SEE IF THERE IS A RESKS5TH RECORD TO ADJUST                  *         
***********************************************************************         
         SPACE 1                                                                
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
***********************************************************************         
*        ADJUST XTRELD BASED ON ROW OR COLUMN INSTERT OR DELETE       *         
***********************************************************************         
                                                                                
FIXXTREL NMOD1 0,**XTRL**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE APELEM                                  
         L     R3,0(,R1)           ROW OR COLUMN NUMBER TO START WITH           
         STC   R3,APBYTE           SAVE ROW/COLUMN WORKING ON                   
         L     R2,4(,R1)           +1 OR -1 FOR INSERT OR DELETE                
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,XTRELQ     EXTRA DATA ELEMENTS, X'C6'                   
         GOTO1 GETEL                                                            
         BNE   FIXXTR90            NOTHING TO FIX                               
*                                                                               
EL       USING XTRELD,APELEM                                                    
                                                                                
FIXXTR10 SR    R3,R3                                                            
         IC    R3,1(,R1)           LENGTH OF ELEMENT                            
         BCTR  R3,0                                                             
         EXMVC R3,APELEM,0(R1)     COPY ELEMENT                                 
*                                                                               
         CLI   EL.XTRTYPE,XTRRTOT  YES. ROW TOTAL TYPE ?                        
         BL    FIXXTR60            NO, NEXT ELEMENT                             
         MVI   0(R1),X'FF'         MARK DELETED ON RECORD                       
         CLI   EL.XTRTYPE,XTRCTOT  YES. COLUMN TOTAL TYPE ?                     
         BH    FIXXTR20            MUST BE TRAILER RECORD                       
         BE    FIXXTR12            IS COLUMN TYPE                               
         CLI   REPMODE,REPROW      ROW TYPE ?                                   
         BNE   FIXXTR20            NO. ADJUST COLUMNS                           
         B     FIXXTR14                                                         
*                                                                               
FIXXTR12 CLI   REPMODE,REPCOL      COLUMN TYPE ?                                
         BNE   FIXXTR20            NO. ADJUST ROWS                              
*                                                                               
FIXXTR14 LTR   R2,R2               INSERT OR DELETE ?                           
         BM    FIXXTR16            DELETE                                       
         CLC   APBYTE,EL.XTRNUM    INSERT, WHERE IS ROW/COL RELATIVE            
         BH    FIXXTR20            THIS IS OK, CHECK SUB-ELEMENTS               
         B     FIXXTR18            INCREASE ROW/COLUMN                          
*                                                                               
FIXXTR16 CLC   APBYTE,EL.XTRNUM    MATCH ON ROW OR COLUMN                       
         BH    FIXXTR20            THIS IS OK, CHECK SUB-ELEMENTS               
         BE    FIXXTR60            REMOVE ELEMENT                               
*                                  DECREASE ROW/COLUMN                          
FIXXTR18 IC    R3,EL.XTRNUM                                                     
         AR    R3,R2               +1 OR -1                                     
         STC   R3,EL.XTRNUM        REPLACE WITH ADJUSTED NUMBER                 
*                                                                               
FIXXTR20 SR    RF,RF                                                            
         IC    RF,EL.XTRSUB#       NUMBER OR SUB-ELEMENTS                       
         LA    RE,EL.XTRSUBEL      POINT  TO SUB-ELEMENT                        
*                                                                               
         USING XTRSUBEL,RE                                                      
FIXXTR25 TM    XTRSIND,XTRSROW     ROW TYPE                                     
         BZ    FIXXTR28            NO, TRY COLUMN                               
         CLI   REPMODE,REPROW                                                   
         BE    FIXXTR30                                                         
*                                                                               
FIXXTR28 TM    XTRSIND,XTRSCOL     COLUMN TYPE                                  
         BZ    FIXXTR45            NO, NEXT SUB-ELEMENT                         
         CLI   REPMODE,REPCOL                                                   
         BNE   FIXXTR45            NO, NEXT SUB-ELEMENT                         
*                                                                               
FIXXTR30 CLC   APBYTE,XTRSNUM      SEE WHERE IT IS RELATIVE TO                  
         BH    FIXXTR45            THIS OK. GET NEXT SUB-ELEMENT                
         BL    FIXXTR40            ADJUST SUB-ELEMENT                           
         LTR   R2,R2               INSERT OR DELETE                             
         BP    FIXXTR40            INSERT, ADJUST SUB-ELEMENT                   
         IC    R3,EL.XTRSUB#       DELETE, REMOVE SUB-ELEMENT                   
         SHI   R3,1                ADJUST NUMBER OF SUB-ELEM'S IN ELEM          
         BNP   FIXXTR60            NO SUB-ELEMENTS LEFT, SO DELETE ELEM         
*                                                                               
         STC   R3,EL.XTRSUB#       NEW NUMBER OF SUB-ELEMENTS                   
         ZIC   R1,XTRSUBLN         REMOVE SUB-ELEMENT                           
         IC    R3,EL.XTRLN         LENGTH OF ELEMENT                            
         SR    R3,R1               LESS LENGTH OF SUB-ELEMENT                   
         STC   R3,EL.XTRLN         NEW LENGTH OF ELEMENT                        
         LA    R4,APELEM           START OF ELEMENT                             
         LR    R5,RE               RE = START OF SUB-ELEMENT                    
         SR    R5,R4               R5 = LENGTH UPTO IN ELEMENT SO FAR           
         SR    R3,R5               R3 = LENGTH TO MOVE                          
         BZ    FIXXTR48            NOTHING TO MOVE                              
         BCTR  R3,0                                                             
         LA    R4,0(R1,RE)         POINT TO END OF SUB-ELEMENT                  
         EXMVC R3,0(RE),0(R4)      MOVE DATA OVER OLD SUB-ELEMENT               
         B     FIXXTR48            RE = NOW POINTS TO NEXT SUB-ELEM             
*                                                                               
FIXXTR40 IC    R3,XTRSNUM                                                       
         AR    R3,R2               ADJUST UP OR DOWN, R2 = +1 OR -1             
         STC   R3,XTRSNUM                                                       
*                                                                               
FIXXTR45 IC    R3,XTRSUBLN         LENGTH OF SUB-ELEMENT                        
         AR    RE,R3               POINT TO NEXT SUB-ELEMENT                    
*                                                                               
FIXXTR48 BCT   RF,FIXXTR25         PROCESS NEXT                                 
         DROP  RE                                                               
*                                                                               
         L     R1,AIOAREA1         DELETE ANY UNWANTED ELEMENTS                 
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL                                                            
*                                                                               
         L     R1,AIOAREA1         ADD -REBUILT X'C6' ELEMENT                   
         MVI   APELCODE,XTRELQ                                                  
         GOTO1 ADDEL                                                            
*                                                                               
         L     R1,AIOAREA1         FIND ELEMENT JUST ADDED                      
         GOTO1 GETEL                                                            
FIXXTR50 BNE   FIXXTR90                                                         
         CLC   EL.XTREL(XTRLNQ),0(R1)                                           
         BE    FIXXTR60            FOUND                                        
         GOTO1 NEXTEL              TRY AGAIN                                    
         B     FIXXTR50                                                         
         DROP  EL                                                               
*                                                                               
FIXXTR60 MVI   APELCODE,XTRELQ     NEXT XTREL, X'C6'                            
         GOTO1 NEXTEL                                                           
         BE    FIXXTR10                                                         
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,X'FF'      REMOVE ALL X'FF' ELEMENTS                    
         GOTO1 DELEL                                                            
*                                                                               
FIXXTR90 MVC   APELEM,SVELEM       RESTORE ELEMENT                              
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  FIX CNN REFERENCES TO POINT TO THE UPDATED COLUMN NUMBERS          *         
***********************************************************************         
         SPACE 1                                                                
         USING LWSD,RC                                                          
         SPACE 1                                                                
FIXCALC  NMOD1 0,**FCAL**                                                       
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
         BO    FIXCAL18            YES, MODIFY IT                               
         CLI   RCLSPCL,RCLSPEXR    EXRT()                                       
         BE    FIXCAL18            YES                                          
         CLI   RCLSPCL,RCLSPCME    CUME()                                       
         BNE   FIXCAL80            NO,  SO DON'T CARE ABOUT IT                  
*                                                                               
FIXCAL18 MVC   APWORK,SPACES                                                    
         LA    R8,APWORK                                                        
         LA    R4,RCLNDATA         POINT TO EQUATION                            
         SR    R0,R0               LENGTH OF INPUT                              
         IC    R0,RCLDATLN                                                      
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
*                                       COLUMN POINTED TO WAS DELETED           
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
*                                                                               
FIXCAL60 DS    0H                                                               
*                                  DETERMINE IF THE EQUATION WILL BE            
*                                        TRUNCATED                              
         LA    R4,APWORK           ->    WORK AREA                              
         SR    R8,R4               LENGTH OF EQUATION                           
         CLM   R8,1,=AL1(MXCDATLN) LENGTH > MAXIMUM DATA LENGTH ?               
         BNH   FIXCAL65            NO,   SKIP                                   
*                                  YES   GENERATE WARNING MESSAGE -             
*                                        EQUATION TRUNCATED                     
         GOTO1 ADDMSG,APPARM,('MNOSTCOL',=AL2(ACWTRUNC)),              X        
               (RCLSEQ,AIOAREA1),('MNOMTWRN',0),0                               
         LA    R8,MXCDATLN         MAX LENGTH (TRUNCATE DATA)                   
*                                                                               
FIXCAL65 CLM   R8,1,RCLDATLN       DID LENGTH CHANGE ?                          
         BNE   FIXCAL68            YES                                          
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   RCLNDATA(0),APWORK                                               
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
         SHI   R1,1                                                             
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
         XMOD1 ,                                                                
*                                                                               
         DROP  NEW                                                              
         DROP  R3,RC                                                            
*                                                                               
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
         USING LWSD,RC                                                          
         SPACE 1                                                                
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
         BH    FIXP30              HIGH,   SKIP    UPDATING                     
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
*                                                                               
         DROP  R1,RC                                                            
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
         USING LWSD,RC                                                          
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
         L     R1,ACOLFRST         ->      1ST     COLUMN  FIELD                
         ZIC   R2,APBYTE           GET     COLUMN  NUMBER                       
         ZIC   R3,STSEQ            GET     START   COLUMN                       
         SR    R2,R3                                                            
         MH    R2,NEXTCOL          GET     COLUMN  OFFSET                       
         AR    R1,R2               ->      COLUMN  FIELD                        
         ZIC   R2,0(,R1)           GET     LENGTH  OF      FIELD                
         AR    R1,R2               ->      COLUMN  DATA    FIELD                
         ST    R1,FVADDR           SET     CURSOR  ADDRESS                      
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
         DROP  RC                                                               
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
         USING LWSD,RC                                                          
         SPACE 1                                                                
FIXMSGS  NMOD1 0,**FMSG**                                                       
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
         XMOD1 ,                                                                
*                                                                               
         DROP  WARNP                                                            
         DROP  R1,RC                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BUILD TABLE OF COLUMNS, SAVE OF TYPE OF COLUMN AND COLUMN #S       *         
***********************************************************************         
         SPACE  1                                                               
         USING RCLELD,R2                                                        
         USING RFLELD,R4                                                        
         USING CSEQTABD,R3                                                      
         USING LWSD,RC                                                          
         SPACE  1                                                               
CCOLSEQ  NMOD1 0,**CCOL**                                                       
         L     RC,APALOCAL                                                      
         MVI   TEMPMTYP,MNOMTERR      SET   MESSAGE   TYPE TO     ERROR         
         MVC   TEMPMNUM,=AL2(ACEDUSN) CLEAR DUPLICATE SORT NUMBER USED          
         GOTO1 =A(CLRMEMNO),RR=APRELO                                           
         MVC   TEMPMNUM,=AL2(ACEMJBR) CLEAR TOO MANY JOBBER KEYWORDS            
         BASR  RE,RF                                                            
         MVI   TEMPMTYP,MNOMTWRN      SET   MESSAGE   TYPE TO     WARN          
         MVC   TEMPMNUM,=AL2(ACWHSUC) CLEAR HIDDEN    COL  BEING  STCKD         
         BASR  RE,RF                                                            
         MVC   TEMPMNUM,=AL2(ACWCSUH) CLEAR STACK UNDER    HIDDEN COL           
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
         TM    DWNTYPE,DWNTQREP    QREPORT TRANSMISSION TYPE?                   
         BZ    CCOLSQ09                                                         
         LR    R0,R6                                                            
         XR    R1,R1                                                            
         ICM   R1,1,NROWS                                                       
         AR    R0,R1                                                            
         CHI   R0,20                                                            
         BNH   CCOLSQ09                                                         
         MVC   FVMSGNO,=AL2(ACEQR20)                                            
         B     CCOLSQN                                                          
*                                                                               
CCOLSQ09 LA    R0,TABOFCOL         ADDR OF COLUMN ARRAY                         
         LA    R1,MAXCOLS*CSEQLNQ  SIZE OF COLUMN ARRAY                         
         SR    RF,RF               NO   FROM AREA                               
         MVCL  R0,RE               CLEAR THE TABLE                              
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     COLUMN ELEMENTS X'C3'                        
         GOTO1 GETEL                                                            
CCOLSQ10 BNE   CCOLSQY                                                          
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
         SR    R0,R0               LENGTH    OF   DATA FIELD                    
         IC    R0,RCLDATLN                                                      
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
         IC    R0,RCLDATLN                                                      
         LA    R4,RCLNDATA         POINT TO BEGIN OF DATA                       
CCOLSQ22 CLI   0(R4),C'C'          FIND "C" FOR COLUMN#                         
         BE    CCOLSQ25                                                         
         LA    R4,1(,R4)           BUMP UP                                      
         BCT   R0,CCOLSQ22         COUNT DOWN                                   
         B     CCOLSQ50            FINISHED, GET NEXT ELEMENT                   
*                                                                               
CCOLSQ25 LR    RF,R0                                                            
         CLM   RF,1,RCLDATLN       AT BEGINING OF ELEMENT DATA ?                
         BE    CCOLSQ26            YES, SO DON'T CHECK FOR VERTICAL%            
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
CCOLSQN  LTR   RB,RB                                                            
         B     *+6                                                              
CCOLSQY  CR    RB,RB                                                            
         XMOD1 ,                   RETURN                                       
         DROP  R2,R3,RC                                                         
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
         USING LWSD,RC                                                          
         SPACE  1                                                               
CHKCOLS  NMOD1 0,**CHKC**                                                       
         L     RC,APALOCAL                                                      
*                                                                               
         MVI   TEMPMTYP,MNOMTERR         SET   MESSAGE TYPE TO ERROR            
         MVC   TEMPMNUM,=AL2(ACEIVSS)    CLEAR INVALID SORT SEQUENCE            
         GOTO1 =A(CLRMEMNO),RR=APRELO                                           
         MVC   TEMPMNUM,=AL2(ACEVERT)    CLEAR INVALID VERTICAL %               
         BASR  RE,RF                                                            
         MVC   TEMPMNUM,=AL2(ACEIVCN)    CLEAR INVALID COLUMN NUMBER            
         BASR  RE,RF                                                            
         MVC   TEMPMNUM,=AL2(1449)       CLEAR LOOPING ERRORS                   
         BASR  RE,RF                                                            
         MVC   TEMPMNUM,=AL2(1451)       CLEAR LOOPING ERROR                    
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
*                                                                               
         LA    R6,1(,R6)           OTHER END OF BOX                             
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
*        B     CHKCOL90                                                         
*                                                                               
CHKCOL90 CLC   FVMSGNO,=AL2(FVFOK)                                              
         XMOD1 ,                   RETURN                                       
         DROP  NXT                                                              
         DROP  R3,RC                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CLEAR ALL OCCURENCES OF TEMPORARY ERROR MESSAGE NUMBER FROM THE    *         
*  MESSAGE ELEMENTS                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING LWSD,RC                                                          
         SPACE  1                                                               
CLRMEMNO NMOD1 0,**CLME**          CLEAR ERROR MESSAGE SUBROUTINE               
         L     RC,APALOCAL                                                      
         GOTO1 =A(DELWARN),APPARM,(0,TEMPMNUM),(TEMPMTYP,0),           X        
               RR=APRELO                                                        
         XMOD1 ,                   RETURN                                       
         DROP  RC                                                               
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
         USING LWSD,RC                                                          
         SPACE  1                                                               
DELXTRM  NMOD1 0,**DLXM**          DELETE  EXTRA   MESSAGE ELEMENTS             
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
         DROP  R1,RC                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DELETE WARNING MESSAGE ELEMENT(S)                                  *         
*                                                                     *         
*  PARAMETERS:                                                        *         
*    PARM 1 BYTE  0   COLUMN NUMBER OR 0 MEANING ALL COLUMNS          *         
*           BYTES 1-3 ADDRESS OF A TWO BYTE MESSAGE NUMBER            *         
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
         USING LWSD,RC                                                          
         SPACE  1                                                               
DELWARN  NMOD1 0,**DELW**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE    APELEM                               
         MVC   WARNCOL,0(R1)       SAVE    COLUMN  NUMBER                       
         L     RF,0(,R1)           ->      MESSAGE NUMBER                       
         MVC   WARNMNUM,0(RF)      SAVE    MESSAGE NUMBER                       
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
         DROP  R1,WARNP,NEXTMSG,RC                                              
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
         USING LWSD,RC                                                          
         SPACE  1                                                               
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
         DROP  R1,WARNP,RC                                                      
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
         USING LWSD,RC                                                          
         SPACE  1                                                               
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
GETNUM03 CLI   5(R2),GETNMAXD      INPUT DATA SIZE >    11 ?                    
         BH    GETNUMER            YES,  INVALID   INPUT                        
         IC    R3,5(,R2)           GET   DATA LENGTH                            
         LA    R2,8(,R2)           ->    FIELD                                  
***********************************************************************         
*        AT THIS TIME R2 = A(FIELD), R3 = LENGTH OF FILED             *         
***********************************************************************         
         SPACE 1                                                                
GETNUM06 AR    R2,R3               FIND  LAST BYTE                              
         BCTR  R2,0                                                             
         LA    R5,APWORK+GETNMXM1  ->    WORK AREA (11  DIGITS    MAX)          
*                                  INITIALIZE WORK AREA                         
         MVC   APWORK(GETNMAXD),GETNC0S                                         
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
*                                  END   OF   INPUT                             
         TM    APBYTE,GETNDIG      DIGIT FOUND ?                                
         BZ    GETNUMNG            NO,   NOT  NUMERIC                           
*                                  TURN  OFF  BITS 2,3 (X'CN')                  
         NI    APWORK+GETNMXM1,X'CF'                                            
         TM    APBYTE,GETNNEG            MINUS SIGN FOUND ?                     
         BZ    *+8                       NO,   SKIP                             
         OI    APWORK+GETNMXM1,X'10'     TURN  ON   MINUS    (X'DN')            
*                                                                               
         PACK  APDUB,APWORK(GETNMAXD)                                           
         CP    APDUB,=P'2147483647'      MAX   PACKED    NUMBER                 
         BH    GETNUMOV                  HIGH, OVERFLOW                         
         CP    APDUB,=P'-2147483648'     MIN   PACKED    NUMBER                 
         BL    GETNUMOV                  LOW,  OVERFLOW                         
         CVB   R2,APDUB            CONVERT    TO   BINARY                       
         ST    R2,4(,R1)           SAVE  THE  NUMBER                            
         SR    RF,RF               GOOD  RETURN    CODE                         
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMOV LA    RF,4                OVERFLOW                                     
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMNG LA    RF,8                NOT NUMERIC                                  
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMER LA    RF,16               OVERFLOW                                     
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMEX STC   RF,0(,R1)           STORE RETURN    CODE                         
         XMOD1 ,                   EXIT                                         
         DROP  RC                                                               
         EJECT ,                                                                
***********************************************************************         
*        CONSTANTS FOR GETNUM                                         *         
***********************************************************************         
         SPACE 1                                                                
GETNC0S  DC    16CL1'0'            ZEROES                                       
***********************************************************************         
*        EQUATES FOR BITS IN APBYTE                                   *         
***********************************************************************         
         SPACE 1                                                                
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
         USING SFDD,R2                                                          
         USING LWSD,RC                                                          
         USING TWAD,R5                                                          
PARM     USING TWAPARMD,APPARM                                                  
*                                                                               
BLDSCR   NMOD1 0,**BSCR**                                                       
         L     RC,APALOCAL                                                      
         L     R5,ATWA                                                          
         XC    SFIELDS,SFIELDS     ARRAY OF FIELD NUMBERS BUILT                 
         XC    APHALF,APHALF       LENGTH OF COLUMNS TO DISPLAY                 
         MVI   NFIELDS,0           NUMBER OF FIELDS 0                           
         MVI   ROWPOS,ROWSTR       START WITH ROW 6                             
         XC    APPARM(TWAPARML),APPARM                                          
         ST    R5,PARM.TWAPATWA    SAVE TWA ADDRESS                             
         MVI   RROW#,2             SKIP 2 LINES ON SCREEN                       
         LA    R2,COLTAGH                                                       
         SR    RF,RF                                                            
         IC    RF,0(,R2)                                                        
         AR    R2,RF                                                            
         ST    R2,ANEXTFLD         LOCATION TO BUILD FIELD ON SCREEN            
*        -------------------------                                              
         L     R2,AFLDTAB                                                       
         LA    R1,1                INDEX INTO FLDTAB (START WITH 1)             
         SR    R6,R6               COUNT NUMBER OF FIELDS USED                  
         SR    RF,RF               IC LENGTH OF FIELD IN RF                     
BLDSR010 CLI   0(R2),0             END OF TABLE                                 
         BE    BLDSR020            NO MORE TO SHOW                              
         TM    SFDIND1,SFDFIX      FIELD IS ALWAY ON SCREEN                     
         BO    BLDSR015            ADD TO TOTAL OF COLUMNS                      
         MVC   APFULL,APREPIND     GET   REPORT TYPE                            
         NC    APFULL,SFDREPS      'AND' WITH VALID REPORT TYPES                
         BZ    BLDSR018            NOT   VALID, SKIP IT                         
*                                  NOTE: THIS CODE ASSUMES THAT SOME            
*                                        COLUMN IS ALWAYS VALID                 
         TM    SFDIND1,SFDDDS      DDS   ONLY COLUMN ?                          
         BZ    BLDSR011            NO,   CONTINUE                               
         TM    CUSTAT,CUSDDS       DDS   TERMINAL ?                             
         BZ    BLDSR018            NO,   SKIP IT                                
*                                                                               
BLDSR011 CLI   COL#FRST,0                                                       
         BNE   BLDSR012                                                         
         STC   R1,COL#FRST         FIRST TIME PROCESSING                        
*                                                                               
BLDSR012 CLM   R1,1,COL#FRST       INDEX TABLE ENTRY MATCH                      
         BL    BLDSR018            ADD TO TOTAL OF COLUMNS                      
*                                                                               
BLDSR015 IC    RF,SFDFLEN          FULL LENGTH                                  
         AH    RF,APHALF           ADD PREVIOUS LENGTH                          
         LA    RF,2(,RF)           SPACE BETWEEN FIELDS                         
         CHI   RF,80               CAN'T EXCEED THIS                            
         BH    BLDSR020            FINISHED                                     
         STH   RF,APHALF           SAVE NEW LENGTH                              
         LA    RE,SFIELDS(R6)                                                   
         STC   R1,0(,RE)           SAVE INDEX INTO FLDTAB                       
         LA    R6,1(,R6)           COUNT NUMBER OF FIELDS USED                  
         STC   R6,NFIELDS                                                       
*                                                                               
BLDSR018 LA    R1,1(,R1)           INCREASE INDEX                               
         LA    R2,SFDLNQ(,R2)      NEXT FIELD                                   
         B     BLDSR010            GET NEXT FIELD                               
*        _________________________                                              
*                                                                               
BLDSR020 SR    R0,R0                                                            
         IC    R0,NFIELDS          NUMBER OF SCREEN FIELDS TO BUILD             
         SR    R6,R6               PROCESS EACH INDEX                           
         MVI   COLPOS,2            START WITH COLUMN 1                          
         CLI   ROWPOS,CHDCOLMX     FINISHED PROCESSING COLUMNS ?                
         BH    BLDSR200            YES, FINISHED REST OF SCREEN                 
*                                                                               
BLDSR030 LA    RF,SFIELDS(R6)      POINT TO FIELD # LIST                        
         SR    R2,R2                                                            
         IC    R2,0(,RF)           GET SAVED OFF INDEX INTO FLDTAB              
         BCTR  R2,0                                                             
         MHI   R2,SFDLNQ           TABLE ENTRY POINT                            
         A     R2,AFLDTAB          ADD BASE OF TABLE                            
*                                                                               
*        ------------------------  BEGIN BUILDING HEADINGS                      
         USING CHDD,R3                                                          
BLDSR040 CLI   ROWPOS,CHDHD#2      AFTER LAST HEADER ROW ?                      
         BH    BLDSR100            YES, NOW PROCESS COLUMN INPUT FIELDS         
         L     R3,=A(CHDTAB)                                                    
         A     R3,APRELO                                                        
         SR    RF,RF                                                            
BLDSR042 CLI   0(R3),0             END OF TABLE                                 
         BE    BLDSR170            NO    HEADING, ADD TO NEXT                   
         CLC   CHDHD#,ROWPOS       MATCH HEADING #                              
         BNE   BLDSR044                                                         
         CLC   CHDFNUM,SFDFNUM     MATCH FIELD NUMBER                           
         BE    BLDSR045                                                         
BLDSR044 IC    RF,CHDLN            LENGTH OF ENTRY                              
         AR    R3,RF                                                            
         B     BLDSR042                                                         
*        _________________________                                              
*                                                                               
         USING TWAELEMD,RE                                                      
BLDSR045 L     RE,AIOAREA3                                                      
         XC    0(TWAELLNQ+80,RE),0(RE)                                          
         MVI   TWAELCD,1                                                        
         MVC   TWAERLN,RROW#       RELATIVE ROW    NUMBER                       
         MVC   TWAECOL,COLPOS      CURRENT COLUMN  NUMBER                       
         MVC   TWAEFLN,SFDFLEN     FULL LENGTH                                  
         OI    TWAEATB,FVAPROT                                                  
         MVI   TWAEFLD,0           HEADINGS ARE ZERO                            
         CLI   ROWPOS,CHDHD#2      2ND HEADING LINE ?                           
         BNE   *+10                                                             
         MVC   TWAEFLD,SFDFNUM     SAVE FIELD NUMBER FOR EXTEND HEADER          
         SR    RF,RF                                                            
         IC    RF,CHDLN            CALCULATE LENGTH OF HEADING                  
         SHI   RF,CHDLNQ                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),CHDDATA                                               
         LA    RF,TWAELLNQ+1(,RF)                                               
         B     BLDSR150                                                         
*                                                                               
BLDSR100 L     RE,AIOAREA3                                                      
         XC    0(TWAELLNQ+80,RE),0(RE)                                          
         MVI   TWAELCD,1                                                        
         MVC   TWAERLN,RROW#       RELATIVE ROW    NUMBER                       
         SR    RF,RF                                                            
         TM    SFDIND1,SFDCNTR     CENTER THIS FIELD                            
         BZ    BLDSR120            NO                                           
         IC    RF,SFDFLEN          FULL FIELD LENGTH                            
         SR    R1,R1                                                            
         IC    R1,SFDILEN          INPUT FIELD LENGTH                           
         SR    RF,R1                                                            
         SRL   RF,1                DIVIDE BY 2                                  
*                                                                               
BLDSR120 ZIC   R1,COLPOS           GET CURRENT POST INTO COLUMN                 
         AR    RF,R1               INDENT INTO ORIGINAL COL POS                 
         STC   RF,TWAECOL          CURRENT COLUMN  NUMBER                       
         MVC   TWAEFLN,SFDILEN     INPUT FIELD LENGTH                           
         TM    SFDIND1,SFDPROT     PROTECTED FIELD ?                            
         BZ    *+8                                                              
         OI    TWAEATB,FVAPROT     MAKE PROTECTED FIELD                         
         TM    SFDIND1,SFDLOW      LOWER CASE IS VALID ?                        
         BZ    *+8                                                              
         OI    TWAEATB,FVALOW      MAKE MIXED CASE ALLOWED                      
         MVI   TWAEFLD,0           CLEAR FIELD NUMBER                           
         LA    RF,TWAELLNQ                                                      
*                                                                               
BLDSR150 STC   RF,TWAELLN          ELEMENT LENGTH                               
         GOTO1 VTWABLD,APPARM,ATWA,AIOAREA3,ANEXTFLD,0,0,0                      
         CLI   PARM.TWAPERRS,TWAPEOK                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   ANEXTFLD,PARM.TWAPANXT                                           
         MVI   RROW#,0             SAME RELATIVE ROW                            
         DROP  RE                                                               
*                                                                               
BLDSR170 SR    RF,RF                                                            
         IC    RF,SFDFLEN          FULL LENGTH                                  
         ZIC   R1,COLPOS                                                        
         AR    RF,R1               KEEP COLUMN POSITION                         
         LA    RF,2(,RF)           ADD 2 SPACES BETWEEN ENTRIES                 
         STC   RF,COLPOS           NEW POSITION                                 
         LA    R6,1(,R6)           NEXT INDEX TO PROCESS                        
         BCT   R0,BLDSR030                                                      
*                                                                               
BLDSR180 ZIC   RF,ROWPOS           INCREASE ROW WORKING ON                      
         LA    RF,1(,RF)                                                        
         STC   RF,ROWPOS                                                        
         MVI   RROW#,1             SKIP TO NEXT ROW                             
         B     BLDSR020            PROCESS NEXT ROW                             
*                                                                               
         USING TWAELEMD,RE                                                      
         USING SFD2D,R2                                                         
BLDSR200 L     R2,=A(FLDTAB2)                                                   
         A     R2,APRELO                                                        
***********************************************************************         
*        BUILD OTHER FIELDS ON SCREEN                                 *         
***********************************************************************         
BLDSR210 SR    RF,RF                                                            
         ICM   RF,1,0(R2)          END OF TABLE                                 
         BZ    BLDSR300            FINISHED                                     
         L     RE,AIOAREA3                                                      
         XC    TWAELCD(TWAELLNQ+80),TWAELCD                                     
         MVI   TWAELCD,1           ELEMENT CODE                                 
         MVC   TWAERLN,SFD2ROW     ROW                                          
         OI    TWAERLN,TWAERLAB    ABSOLUTE ADDRESS                             
         MVC   TWAECOL,SFD2COL     COLUMN                                       
         OC    TWAEATB,SFD2ATB     ATRIBUTE                                     
         MVC   TWAEFLD,SFD2FLD     FIELD NUMBER                                 
         LR    R1,RF               RF = LENGTH OF TABLE ENTRY                   
         SHI   R1,(SFD2LNQ+1)                                                   
         BM    BLDSR220            NO DATA TO MOVE IN                           
         EX    R1,*+8                                                           
         B     BLDSR220                                                         
         MVC   TWAEDTA(0),SFD2DATA DATA                                         
*                                                                               
BLDSR220 MVC   TWAEFLN,SFD2FLN     INPUT FIELD LENGTH                           
         LA    R1,TWAELLNQ+1(,R1)                                               
         STC   R1,TWAELLN          ELEMENT LENGTH                               
         AR    R2,RF               BUMP TO NEXT ENTRY IN FLDTAB2                
         GOTO1 VTWABLD,APPARM,ATWA,AIOAREA3,ANEXTFLD,0,0,0                      
         CLI   PARM.TWAPERRS,TWAPEOK                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   ANEXTFLD,PARM.TWAPANXT                                           
         B     BLDSR210            LOOP                                         
         DROP  RE                                                               
*                                                                               
BLDSR300 DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         DROP  R2,R3,R5,RC                                                      
         TITLE 'SET SCREEN VALUES TO HELP PROCESSING'                           
***********************************************************************         
*        SET VALUES (1) NFILEDS  - NUMBER  OF FIELDS/ROW              *         
*                   (2) ACOLFRST - ADDRESS OF EACH FIELD HEADER/ROW   *         
*                   (3) NEXTCOL  - LENGTH  OF EACH ROW                *         
*                   (4) AFRSTCOL - A(FIRST COLUMN ON SCREEN)          *         
*                   (5) ALASTCOL - A(LAST  COLUMN ON SCREEN)          *         
*                   (6) AENDCOL  - A(JUST PAST LAST COLUMN ON SCREEN) *         
***********************************************************************         
         USING SFDD,R2                                                          
         USING LWSD,RC                                                          
         USING TWAD,R5                                                          
SETSCR   NMOD1 0,**SETS**                                                       
         L     RC,APALOCAL                                                      
         L     R5,ATWA                                                          
         LA    R4,COLTAGH                                                       
         MVI   NFIELDS,0                                                        
         XC    SFIELDS,SFIELDS                                                  
         XC    NEXTCOL,NEXTCOL                                                  
         XC    ACOLFRST(ACOLLNQ),ACOLFRST                                       
         MVI   APBYTE,NO                                                        
         SR    R6,R6               COUNT OF # OF FIELDS/COLUMN                  
         SR    RF,RF                                                            
         IC    RF,0(,R4)           BUMP TO NEXT FIELD                           
*                                                                               
*        ------------------------- EXAMINE   EACH FIELD                         
SETSCR10 AR    R4,RF               BUMP   TO NEXT FIELD                         
         IC    RF,0(,R4)           FIELD  LENGTH                                
         TM    1(R4),FVAXTND       EXTEND FIELD HEADER ?                        
         BO    SETSCR15            YES,   SEE IF IT IS "COL" FIELD              
         CLI   APBYTE,YES          2ND    PASS ?                                
         BE    SETSCR40            YES,   FINISHED                              
         B     SETSCR10            GET NEXT FIELD, TILL HIT EXTENDED            
*                                                                               
SETSCR15 SHI   RF,8                SUBTRACT XTENDED FIELD HEADER LENGTH         
         LA    R3,0(RF,R4)         POINT TO XTENDED FIELD HEADER                
         LA    R1,1                START WITH ENTRY 1                           
         L     R2,AFLDTAB          A(FLDTAB)                                    
         CLI   APBYTE,YES          2ND PASS                                     
         BE    SETSCR20            ALL READY GOT HERE, FIND # IN TABLE          
         CLI   0(R3),SFDCOLN       FIRST COLUMN ?                               
         BNE   SETSCR30            NOT FIRST COLUMN YET                         
         MVI   APBYTE,YES          GOT FIRST COLUMN                             
*                                                                               
*        ------------------------- SEARCH IN TABLE FOR MATCH                    
SETSCR20 CLI   SFDFNUM,0           END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'00'               HAS TO BE ONE                                
         CLC   SFDFNUM,0(R3)       MATCH ON FIELD NUMBER                        
         BNE   SETSCR25            NOT FOUND, CHECK NEXT                        
         LA    RE,SFIELDS(R6)      LOCATION TO SAVE INDEX                       
         STC   R1,0(,RE)           STORE INDEX INTO FLDTAB                      
         LA    R6,1(,R6)           BUMP UP COUNT OF COLUMNS                     
         B     SETSCR30            NEXT FIELD                                   
*                                                                               
SETSCR25 LA    R1,1(,R1)           BUMP UP TABLE ENTRY INDEX                    
         LA    R2,SFDLNQ(,R2)      NEXT ENTRY                                   
         B     SETSCR20                                                         
*        _________________________ LOOP FOR NEXT TABLE ENTRY                    
*                                                                               
SETSCR30 SR    RF,RF                                                            
         IC    RF,0(,R4)           GET FIELD LENGTH                             
         B     SETSCR10                                                         
*        _________________________ LOOP TO NEXT FIELD                           
*                                                                               
SETSCR40 STC   R6,NFIELDS          SAVE NUMBER OF FIELDS                        
         ST    R4,ACOLFRST         MUST BE FIRST COLUMN ROW                     
         LR    R0,R6               SET TO LOOP, R0 = NUMBER OF COLUMNS          
         SR    R6,R6                                                            
*                                                                               
*        _________________________ IDENTIFY & SAVE EACH COLUMN USED             
SETSCR42 LA    R3,SFIELDS(R6)      GET INDEX INTO TABLE                         
         ZIC   R2,0(,R3)                                                        
         BCTR  R2,0                                                             
         MHI   R2,SFDLNQ                                                        
         A     R2,AFLDTAB          POINT TO ENTRY INTO TABLE                    
         SR    RE,RE                                                            
         ICM   RE,3,SFDFADR        DISPLACMENT TO STORE ADDRESS                 
         LA    RE,LWSD(RE)         BASE OF LWSD DSECT                           
         ST    R4,0(,RE)           SAVE OF ADDRESS OF COLUMN FIELD              
         LA    R6,1(,R6)           NEXT ENTRY                                   
         SR    RF,RF               GET LENGTH TO TOTAL AND BUMP                 
         IC    RF,0(,R4)                                                        
         LR    R1,RF                                                            
         AH    R1,NEXTCOL                                                       
         STH   R1,NEXTCOL          SAVE LENGTH OF COLUMN ROW                    
         AR    R4,RF               BUMP TO NEXT FIELD                           
         BCT   R0,SETSCR42         PROCESS NEXT                                 
*        _________________________ LOOP TO NEXT FIELD                           
*                                                                               
         L     R4,ACOLFRST         FIRST COLUMN                                 
         LA    RF,CLIMIT           NUMBER OF COLUMNS ON SCREEN                  
         MH    RF,NEXTCOL          LENGTH OF EACH ROW                           
         AR    RF,R4                                                            
         ST    RF,AENDCOL          A(END OF COLUMNS)                            
         SH    RF,NEXTCOL          POINT TO LAST COLUMN                         
         ST    RF,ALASTCOL         A(LAST COLUMN)                               
*                                                                               
*        ------------------------- LOCATE REPWIDTH, COLUMN FIELDS               
SETSCR50 SR    RF,RF               RF = LENGTH OF FIELD                         
         ICM   RF,1,0(R4)          GET LENGTH                                   
         BZ    SETSCR80            GET OUT LENGTH ZERO                          
         TM    1(R4),FVAXTND       EXTENDED FIELD ?                             
         BZ    SETSCR70            GET NEXT FIELD                               
         LR    R3,RF                                                            
         SHI   R3,8                LESS FIELD HEADER                            
         AR    R3,R4               POINT TO EXTEND FIELD HEADER                 
*                                                                               
         USING SFD2D,R2                                                         
         SR    R1,R1                                                            
         L     R2,=A(FLDTAB2)                                                   
         A     R2,APRELO                                                        
SETSCR52 ICM   R1,1,SFD2LN         END OF TABLE (LENGTH = ZERO)                 
         BZ    SETSCR70            GET NEXT FIELD                               
         SR    RE,RE                                                            
         ICM   RE,3,SFD2ADR        NEED TO LOCATE ?                             
         BZ    SETSCR55            NO, GET NEXT TABLE ENTRY                     
         CLC   SFD2FLD,0(R3)       MATCH FIELD NUMBER                           
         BE    SETSCR60                                                         
SETSCR55 AR    R2,R1               NEXT TABLE ENTRY                             
         B     SETSCR52                                                         
*        _________________________                                              
SETSCR60 LA    RE,LWSD(RE)         POINT INTO LOCAL WORKING STORAGE             
         ST    R4,0(,RE)           SAVE OFF ADDRESS                             
*                                                                               
SETSCR70 AR    R4,RF               NEXT FIELD ON SCREEN                         
         B     SETSCR50                                                         
*                                                                               
SETSCR80 CLI   APMODE,APMVALK                                                   
         BNE   SETSCR95                                                         
         MVI   FXDCURSR,YES        WE FORCED CURSOR TO 1ST LINE                 
         MVC   ACURCOL,ACOLNUM     DEFAULT TO FIRST COLUMN                      
         CLI   CURRCOLN,0          COMING  VIA  PF4  ? (COL FILTER)             
         BE    SETSCR85            NO,     THEN DON'T RESET ACURSOR             
         CLC   CURRCOLN,STSEQ      CURRENT COLUMN , START COLUMN                
         BL    SETSCR82            CURRENT COLUMN   BEFORE START                
         CLI   STSEQ,0             FIRST TIME IN                                
         BNE   *+8                 NO, SO ALREADY SET                           
         MVI   STSEQ,1             SET START TO 1                               
         ZIC   RF,CURRCOLN                                                      
         ZIC   RE,STSEQ                                                         
         SR    RF,RE               SEE IF WE PASS END OF SCREEN                 
         CHI   RF,CLIMIT           IF MORE THEN 12 THEN PASSED                  
         BL    SETSCR84            NO, IN RANGE OF SCREEN                       
*                                                                               
SETSCR82 MVC   STSEQ,CURRCOLN      YES, USE SELECTED COL AS START               
         CLI   STSEQ,MAXCOLS-CLIMIT+1                                           
         BNH   *+8                                                              
         MVI   STSEQ,MAXCOLS-CLIMIT+1                                           
         SR    RF,RF                                                            
*                                                                               
SETSCR84 L     R4,ACOLNUM          SET COLUMN LOCATION                          
         MH    RF,NEXTCOL                                                       
         AR    RF,R4                                                            
         ST    RF,ACURCOL                                                       
         MVI   CURRCOLN,0          RESET                                        
         B     SETSCR90                                                         
*                                                                               
SETSCR85 CLC   ATWA,ACURSOR        IF ACURSOR = ATWA CURSOR WAS INVALID         
         BE    SETSCR90            CURSOR NOT LEGAL SO SET TO 1ST COL           
         CLI   TWALREC,RECCOL      IF   1ST  TIME IN,  THEN SET CURSOR          
         BNE   SETSCR90            CURSOR NOT SET, SET TO 1ST COL               
*                                                                               
         LA    R0,CLIMIT           NUMBER OF COLUMNS                            
         L     R4,ACOLNUM          FIRST COLUMN ON SCREEN                       
SETSCR86 C     R4,ACURSOR          FIND IF CURSOR IS ON A COLUMN                
         BH    SETSCR90            NO NOT ON A LINE                             
         AH    R4,NEXTCOL          R4 = END OF COLUMN OR START OF NEXT          
         C     R4,ACURSOR                                                       
         BH    SETSCR88            YES IT IS ON THIS LINE                       
         BCT   R0,SETSCR86                                                      
         B     SETSCR90                                                         
*                                                                               
SETSCR88 SH    R4,NEXTCOL          BUMP BACK THE ONE ADDED                      
         ST    R4,ACURCOL          SET CURRENT COLUMN CURSOR ON                 
         MVI   FXDCURSR,NO         WE FORCED CURSOR TO 1ST LINE                 
*                                                                               
SETSCR90 LA    R1,1                COUNT COLUMNS UNTIL FIND CURRENT COL         
         LA    R0,CLIMIT           MAX NUMBER OF COLUMNS ON SCREEN              
         L     R4,ACOLFRST         FIRST COLUMN                                 
*                                                                               
SETSCR92 C     R4,ACURCOL          IS CURSOR ON THIS COLUMN ?                   
         BE    SETSCR94            YES                                          
         LA    R1,1(,R1)           TRY NEXT COLUMN                              
         AH    R4,NEXTCOL                                                       
         BCT   R0,SETSCR92         LOOP                                         
         LA    R1,1                DEFAULT TO ONE                               
*                                                                               
SETSCR94 STC   R1,CRSRCOL#         SAVE COLUMN NUMBER OF CURSOR                 
*                                                                               
SETSCR95 XIT1                                                                   
         DROP  R2,R5                                                            
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'TABLE TO BUILD SCREEN FIELDS'                                   
FLDTAB   DS    0F                                                               
*                                                      COLUMN  NUMBER           
         DC    AL1(SFDCOLN,SFDCOLNQ,3,SFDPROT+SFDFIX)                           
         DC    AL2(ACOLNUM-LWSD)                                                
         DC    AL4(REPALL)                                                      
*                                                      DATA                     
         DC    AL1(SFDDATA,SFDDATAQ,12,SFDFIX)                                  
         DC    AL2(ACOLDATA-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      DATE    RANGE            
         DC    AL1(SFDDTER,SFDDTERQ,10,SFDFIX)                                  
         DC    AL2(ACOLDTER-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      WIDTH                    
         DC    AL1(SFDWDTH,SFDWDTHQ,2,SFDFIX)                                   
         DC    AL2(ACOLWDTH-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      TOTAL                    
         DC    AL1(SFDTOTL,SFDTOTLQ,3,SFDCNTR)                                  
         DC    AL2(ACOLTOTL-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                                               
COLLOWQ  EQU   (*-FLDTAB)/SFDLNQ                LOWEST NUMBER                   
*                                                      PRINT                    
         DC    AL1(SFDPRNT,SFDPRNTQ,3,SFDCNTR)                                  
         DC    AL2(ACOLPRNT-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      HEAD    1                
         DC    AL1(SFDHD#1,SFDHD#1Q,12,SFDLOW)                                  
         DC    AL2(ACOLHD#1-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      HEAD    2                
         DC    AL1(SFDHD#2,SFDHD#2Q,12,SFDLOW)                                  
         DC    AL2(ACOLHD#2-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                                               
         DC    AL1(SFDSORD,SFDSORDQ,5,SFDCNTR)         SORT    ORDER            
         DC    AL2(ACOLSORD-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      STACK   UNDER            
         DC    AL1(SFDCSTK,SFDCSTKQ,5,SFDCNTR)                                  
         DC    AL2(ACOLCSTK-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                                               
*        FIELD NUMBER,INPUT LENGTH. FULL FIELD LENGHT                           
*        INPUT INDICATORS (CENTER INPUT, DDS ONLY)                              
*        SAVE FIELD ADDRESS                                                     
*        VALID REPORT TYPES                                                     
*                                                      DECIMAL/ROUND            
         DC    AL1(SFDDECM,SFDDECMQ,8,SFDCNTR)                                  
         DC    AL2(ACOLDECM-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      REPEAT CONSTANT          
         DC    AL1(SFDRPTC,SFDPRTCQ,8,SFDCNTR)                                  
         DC    AL2(ARPTCNSD-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      PRINT REDUNDANT          
         DC    AL1(SFDPRDN,SFDPRDNQ,9,SFDCNTR)                                  
         DC    AL2(APRTRDND-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      NEGATIVE AMOUNTS         
         DC    AL1(SFDNGAM,SFDNGAMQ,8,SFDCNTR)                                  
         DC    AL2(ACOLNGAM-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      PRINT COMMAS             
         DC    AL1(SFDPRCM,SFDPRCMQ,5,SFDCNTR)                                  
         DC    AL2(ACOLPRCM-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      PRINT ZERO AMTS          
         DC    AL1(SFDPRZR,SFDPRZRQ,7,SFDCNTR)                                  
         DC    AL2(ACOLPRZR-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      UNDERLINE AMNTS          
         DC    AL1(SFDUNLN,SFDUNLNQ,9,SFDCNTR)                                  
         DC    AL2(ACOLUNLN-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                      PRINT ZERO TTLS          
         DC    AL1(SFDPZTT,SFDPZTTQ,6,SFDCNTR)                                  
         DC    AL2(ACOLPZTT-LWSD)                                               
         DC    AL4(REPALL)                                                      
*                                                                               
*&&DO                                                  TYPE                     
         DC    AL1(SFDTTYP,SFDTTYPQ,SFDTTYPQ,0)                                 
         DC    AL2(ACOLTTYP-LWSD)                                               
         DC    AL4(REPALL-REPCOST)                                              
*                                                      BUDGET                   
         DC    AL1(SFDBUDG,SFDBUDGQ,SFDBUDGQ,0)                                 
         DC    AL2(ACOLBUDG-LWSD)                                               
*&&US*&& DC    AL4(REPALL)                                                      
*&&UK*&& DC    AL4(REPALL-REPPROD)                                              
*&&                                                    FOREIGN CURRENCY         
*&&UK*&& DC    AL1(SFDFCUR,SFDFCURQ,SFDFCURQ+1,0)                               
*&&UK*&& DC    AL2(ACOLFCUR-LWSD)                                               
*&&UK*&& DC    AL4(REPALL)                                                      
*                                                                               
COLHIGHQ EQU   (*-FLDTAB)/SFDLNQ         # OF ENTRIES OR HIGHEST NUMBER         
         DC    AL1(0)                    END OF TABLE                           
         EJECT ,                                                                
FLDTAB2  DS    0F                                                               
         DC    AL1(FLD01X-*,20,3,12,0,FVAPROT+FVAHIGH+FVALOW),AL2(0)            
         DCDD  AC#RPTWD,12                                                      
FLD01X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD02X-*,20,21,1,0,FVAPROT+FVAHIGH),AL2(0)                   
         DC    C'='                                                             
FLD02X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD03X-*,20,23,4,250,FVAPROT),AL2(AREPWDTH-LWSD)             
         DC    C'000'                                                           
FLD03X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD04X-*,21,3,17,0,FVAPROT+FVAHIGH+FVALOW),AL2(0)            
         DCDD  AC#NUCOL,17                                                      
FLD04X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD05X-*,21,21,1,0,FVAPROT+FVAHIGH),AL2(0)                   
         DC    C'='                                                             
FLD05X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD06X-*,21,25,2,251,FVAPROT),AL2(ANUMCOLS-LWSD)             
         DC    C'00'                                                            
FLD06X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD07X-*,23,2,78,254,FVAPROT+FVAHIGH+FVALOW),AL2(0)          
         DC    C'PFKEY 1'                                                       
FLD07X   EQU   *                                                                
*                                                                               
         DC    AL1(FLD08X-*,24,2,77,254,FVAPROT+FVAHIGH+FVALOW),AL2(0)          
         DC    C'PFKEY 2'                                                       
FLD08X   EQU   *                                                                
*                                                                               
         DC    AL1(0)                                                           
FVALOW   EQU   X'40'                                                            
         EJECT ,                                                                
***********************************************************************         
*        COLUMN HEADINGS 1 & 2. MUST ALWAYS SUPPLY HEADING 2          *         
***********************************************************************         
CHDTAB   DS    0F                                                               
         DC    AL1(CHD1BX-*,SFDCOLN,CHDHD#2)       COL                          
         DCDD  AC#COLS,3                                                        
CHD1BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD2BX-*,SFDDATA,CHDHD#2)                                    
         DCDD  AC#DATA,SFDDATAQ                                                 
CHD2BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD3AX-*,SFDDTER,CHDHD#1)                                    
         DCDD  AC#DATE,SFDDTERQ                                                 
CHD3AX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD3BX-*,SFDDTER,CHDHD#2)                                    
         DCDD  AC#RANGE,SFDDTERQ                                                
CHD3BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD4AX-*,SFDWDTH,CHDHD#2)                                    
         DC    C'WD'                                                            
CHD4AX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD5AX-*,SFDTOTL,CHDHD#1)                                    
         DCDD  AC#TOTAL,3                                                       
CHD5AX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD5BX-*,SFDTOTL,CHDHD#2)                                    
         DCDD  AC#YORN,3                                                        
CHD5BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD6AX-*,SFDPRNT,CHDHD#1)                                    
         DCDD  AC#PRINT,3                                                       
CHD6AX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD6BX-*,SFDPRNT,CHDHD#2)                                    
         DCDD  AC#YORN,3                                                        
CHD6BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD7AX-*,SFDSORD,CHDHD#1)                                    
         DCDD  AC#SORT,5                                                        
CHD7AX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD7BX-*,SFDSORD,CHDHD#2)                                    
         DCDD  AC#ORDER,5                                                       
CHD7BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD8BX-*,SFDHD#1,CHDHD#2)                                    
         DCDD  AC#HEAD1,12                                                      
CHD8BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD9BX-*,SFDHD#2,CHDHD#2)                                    
         DCDD  AC#HEAD2,12                                                      
CHD9BX   EQU   *                                                                
*                                                                               
         DC    AL1(CHD10AX-*,SFDCSTK,CHDHD#1)                                   
         DCDD  AC#STACK,5                                                       
CHD10AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD10BX-*,SFDCSTK,CHDHD#2)                                   
         DCDD  AC#UNDER,5                                                       
CHD10BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD11AX-*,SFDTTYP,CHDHD#1)                                   
         DCDD  AC#TRN,SFDTTYPQ                                                  
CHD11AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD11BX-*,SFDTTYP,CHDHD#2)                                   
         DCDD  AC#TYPE1,SFDTTYPQ                                                
CHD11BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD12AX-*,SFDBUDG,CHDHD#1)                                   
         DCDD  AC#BGT,SFDBUDGQ                                                  
CHD12AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD12BX-*,SFDBUDG,CHDHD#2)                                   
         DCDD  AC#CODE,SFDBUDGQ                                                 
CHD12BX  EQU   *                                                                
*&&UK                                                                           
         DC    AL1(CHD13AX-*,SFDFCUR,CHDHD#1)                                   
         DC    CL(SFDFCURQ+1)'CURR'                                             
CHD13AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD13BX-*,SFDFCUR,CHDHD#2)                                   
         DCDD  AC#CODE,SFDFCURQ+1                                               
CHD13BX  EQU   *                                                                
*&&                                                                             
*                                                                               
         DC    AL1(CHD14AX-*,SFDDECM,CHDHD#1)                                   
         DCDD  AC#RNDOR,8                                                       
CHD14AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD14BX-*,SFDDECM,CHDHD#2)                                   
         DCDD  AC#DCML,8                                                        
CHD14BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD15AX-*,SFDRPTC,CHDHD#1)                                   
         DCDD  AC#REPET,8                                                       
CHD15AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD15BX-*,SFDRPTC,CHDHD#2)                                   
         DCDD  AC#CONST,8                                                       
CHD15BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD16AX-*,SFDPRDN,CHDHD#1)                                   
         DCDD  AC#PRINT,9                                                       
CHD16AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD16BX-*,SFDPRDN,CHDHD#2)                                   
         DCDD  AC#REDUN,9                                                       
CHD16BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD17AX-*,SFDNGAM,CHDHD#1)                                   
         DCDD  AC#NEG,8                                                         
CHD17AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD17BX-*,SFDNGAM,CHDHD#2)                                   
         DCDD  AC#AMTS,8                                                        
CHD17BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD18AX-*,SFDPRCM,CHDHD#2)                                   
         DCDD  AC#COMMA,5                                                       
CHD18AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD19AX-*,SFDPRZR,CHDHD#1)                                   
         DCDD  AC#ZERO,7                                                        
CHD19AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD19BX-*,SFDPRZR,CHDHD#2)                                   
         DCDD  AC#AMTS,7                                                        
CHD19BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD20AX-*,SFDUNLN,CHDHD#1)                                   
         DCDD  AC#UNDLN,9                                                       
CHD20AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD20BX-*,SFDUNLN,CHDHD#2)                                   
         DCDD  AC#TOTLS,9                                                       
CHD20BX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD21AX-*,SFDPZTT,CHDHD#1)                                   
         DCDD  AC#ZERO,6                                                        
CHD21AX  EQU   *                                                                
*                                                                               
         DC    AL1(CHD21BX-*,SFDPZTT,CHDHD#2)                                   
         DCDD  AC#TOTLS,6                                                       
CHD21BX  EQU   *                                                                
*                                                                               
         DC    AL1(0)                                                           
         DROP  RC                                                               
         EJECT ,                                                                
         TITLE 'DSECTS AND STORAGE VALUES'                                      
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXSCRLL EQU   MAXCOLS-CLIMIT+1                                                 
CLIMIT   EQU   12                                                               
ROWSTR   EQU   6                   START ON ROW 6                               
*                                                                               
AEXITRTN DS    A                   A(EXIT ROUTINE)                              
CURELEM  DS    A                   A(CURRENT ELEMENT W/MATCHING SEQ #)          
ANEXTFLD DS    A                   A(ADDRESS ON SCREEN TO BUILD FIELD)          
DEFENTRY DS    A                   A(CURRENT ENTRY IN DEFTAB)                   
ENDOFEXP DS    A                   A(END OF EXPRESSION)                         
SAVEREGS DS    4A                  SAVE REGISTERS                               
SAVER1   DS    A                   SAVE REGISTER 1                              
LASTNTRY DS    A                   A(LAST ENTRY IN CHKCOL TAB)                  
CHKCOLR3 DS    A                   SAVE R3 FOR CHKCOL IN CASE OF ERROR          
SVCURSOR DS    A                   A(SAVE CURRENT CURSOR)                       
ALASTCOL DS    A                   A(LAST COLUMN)                               
AENDCOL  DS    A                   A(END OF COLUMNS)                            
ACURCOL  DS    A                   A(CURRENT COLUMN CURSOR IS ON)               
AFLDTAB  DS    A                   A(FLDTAB)                                    
ARPFELM  DS    A                   A(FORMAT PROFILE ELEMENT)                    
*                                  COLUMN FIELD ADDRESS (CURRENT LINE)          
ACOLFRST DS    A                   FIRST COLUMN                                 
ACOLNUM  DS    A                   .  COL                                       
ACOLDATA DS    A                   .  DATA                                      
ACOLDTER DS    A                   .  DATE RANGE                                
ACOLWDTH DS    A                   .  WD                                        
ACOLTOTL DS    A                   .  TOTAL                                     
ACOLPRNT DS    A                   .  PRINT                                     
ACOLHD#1 DS    A                   .  HEADING 1                                 
ACOLHD#2 DS    A                   .  HEADING 2                                 
ACOLSORD DS    A                   .  SORT ORDER                                
ACOLCSTK DS    A                   .  STACK UNDER                               
ACOLDECM DS    A                   .  DECIMAL PLACES                            
ARPTCNSD DS    A                   .  REPEAT CONSTANT                           
APRTRDND DS    A                   .  PRINT REDUNDANT INFO                      
ACOLNGAM DS    A                   .  NEGATIVE AMOUNTS                          
ACOLPRCM DS    A                   .  PRINT COMMAS                              
ACOLPRZR DS    A                   .  PRINT ZERO AMOUNTS                        
ACOLUNLN DS    A                   .  UNDERLINE AMOUNTS                         
ACOLPZTT DS    A                   .  PRINT ZERO AMOUNTS                        
ACOLTTYP DS    A                   .  TYPE                                      
ACOLBUDG DS    A                   .  BUDGET                                    
ACOLFCUR DS    A                   .  FOREIGN CURRENCY                          
ACOLLNQ  EQU   *-ACOLFRST          LENGTH OF BLOCK                              
*                                                                               
AREPWDTH DS    A                   .  REPORT WIDTH FIELD                        
ANUMCOLS DS    A                   .  NUMBER OF COLUMNS                         
SVBILCOL DS    A                   .  SAVE ADDR OF BILLT COL. FLD (UK)          
SVDAYCOL DS    A                   .  SAVE ADDR OF TSDAY COL. FLD (UK)          
*                                                                               
FULL     DS    F                                                                
SCROLL   DS    H                                                                
JOBRNUM  DS    H                   COUNT NUMBER OF JOBBER KEYWORDS              
NEXTCOL  DS    H                   LENGTH OF COLUMN ROW                         
COLPOS   DS    AL1                 COL #                                        
ROWPOS   DS    AL1                 ROW #                                        
RROW#    DS    AL1                 RELATIVE ROW NUMBER                          
SVREPJCL DS    CL1                 SEE IF WE SWITCHED REPORT TYPES              
SFIELDS  DS    XL12                INDEX # INTO FLDTAB (SCREEN FIELDS)          
NFIELDS  DS    XL1                 NUMBER OF FIELDS ON SCREEN                   
BLOCK    DS    13CL32              DATA BLOCK FOR SCANNER                       
DUMFLDH  DS    CL8                 DUMMY FIELD HEADER                           
DUMFLD   DS    CL12                DUMMY FIELD                                  
SVHEAD#1 DS    CL(L'RCLNHDL1)      SAVE HEADING # 1                             
SVHEAD#2 DS    CL(L'RCLNHDL2)      SAVE HEADING # 2                             
SVRCLOPT DS    XL1                 SAVE OLD RCLOPT                              
SVRCLOP2 DS    XL1                 SAVE OLD RCLOPT2                             
SVRCLOP4 DS    XL1                 SAVE OLD RCLOPT4                             
SVRCLSPC DS    XL1                 SAVE OLD RCLSPCL                             
TEMP1    DS    XL1                 TEMP BYTE 1                                  
TEMP2    DS    XL1                 TEMP BYTE 2                                  
ERRCOL#  DS    XL1                 COLUMN NUMBER IN ERROR                       
DELCOL#  DS    XL1                 CURRENT COLUMN DELETED                       
INSCOL#  DS    XL1                 CURRENT COLUMN INSERTED                      
COLINSRT DS    XL1                 COL INSERTED TO COMPARE WITH ERRCOL#         
LSTCOL#  DS    XL1                 LAST COLUMN OR NUMBER OF COLUMNS             
TOT#COLS DS    XL1                 TOTAL NUMBER OF COLUMNS ON FORMAT            
NROWS    DS    AL1                 NUMBER OF ROWS ON RECORD                     
NDECIMAL DS    XL1                 0 OR N FOR NUMBER OF DECIMALS                
CURCOL   DS    XL1                 NEXT SEQUENCE NUMBER                         
*ECLVL   DS    XL1                 SECURITY BIT LEVEL                           
NEWELEM  DS    XL1                 SET TO TELL IF ADDING NEW ELEMENT            
CPARMS   DS    XL1                 NUMBER OF PARAMETERS                         
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
HAVE5TH  DS    CL1                 HAVE RESKS5TH RECORD (YES/NO)                
RANKON   DS    CL2                 RANK ON                                      
RANKCOL  DS    AL1                 RANK COLUMN                                  
DOWNOPT  DS    AL1                 DOWNLOAD ONLY REPORT                         
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
RESETSEQ DS    XL1                 RESET START SEQUENCE WHEN DISPLAYING         
TYPEIND  DS    AL1                                                              
TYPECOL  EQU   X'80'                                                            
TYPEROW  EQU   X'40'                                                            
TYPEVRT  EQU   X'20'                                                            
TYPEPARM DS    AL1                                                              
COLFC    DS    CL1                 COLUMN WITH FOREIGN CURRENCY                 
COLUMN#  DS    XL1                 COLUMN NUMBER                                
CURTYP#  DS    XL1                 TEMPORARY BYTE                               
FXDCURSR DS    CL1                 FIXED CURSOR INDICATOR                       
SAVECOL# DS    XL1                 SAVE AREA FOR COLUMN NUMBER                  
FIXSEQ#  DS    CL1                 SAVE AREA FOR FIX UP MNOSEQ                  
PASTESEQ DS    XL1                 PASTE SEQUENCE NUMBER                        
SVSVINSQ DS    XL1                 SAVE THE SAVE INSERT SEQUENCE NUMBER         
CRSRCOL# DS    AL1                 CURSOR COLUMN NUMBER                         
MIXESTK  DS    XL1                 MIXED ESTIMATE KEYWORDS INDICATOR            
*                                                                               
KWDINDS  DS    XL1                 KEYWORD INDICATOR                            
KWDDRBIL EQU   X'80'               DR,BILL                                      
KWDBILLT EQU   X'40'               BILLT                                        
KWDTSDTE EQU   X'20'               TSDTE                                        
KWDTSDAY EQU   X'10'               TSDAY                                        
KWDSHEAD EQU   X'08'               ALREADY SHOW THE HEADING                     
*                                                                               
DWNTYPE  DS    XL1                 DOWNLOAD TRANSMISSION TYPE                   
DWNTACNT EQU   X'80'               ACCENT                                       
DWNTQREP EQU   X'40'               QUICK REPORT                                 
*                                                                               
MSGELSW  DS    XL1                 MESSAGE ELEMENT SWITCH                       
MSGELFND EQU   X'80'                  MESSAGE ELEMENT FOUND                     
MSGELERR EQU   X'40'                  MESSAGE ELEMENT ERROR                     
MSGELINS EQU   X'08'                  MESSAGE ELEMENT INSERT                    
MSGELDEL EQU   X'04'                  MESSAGE ELEMENT DELETE                    
MSGELIDR EQU   X'02'                  MESSAGE ELEMENT IN DISPLAY RTN            
*                                                                               
TEMPMNUM DS    AL2                 TEMPORARY MESSAGE NUMBER                     
TEMPMTYP DS    CL1                 TEMPORARY MESSAGE TYPE                       
TEMPDATA DS    CL2                 TEMPORARY MESSAGE DATA                       
*                                                                               
*                                  FOR DELWARN                                  
WARNCOL  DS    XL1                 .   COLUMN  NUMBER  OR   ZERO                
WARNMNUM DS    AL2                 .   MESSAGE NUMBER                           
WARNMTYP DS    CL1                 .   MESSAGE TYPE                             
WARNADAT DS    XL4                 .   ADDRESS MESSAGE DATA                     
WARNDTYP DS    XL1                 .   MESSAGE DATA    TYPE                     
WARNDLNG DS    XL1                 .   MESSAGE DATA    LENGTH                   
WARNSW   DS    XL1                 .   SWITCH                                   
WARNDREQ EQU   X'80'                   ..      DELEL   REQUIRED                 
*                                                                               
*                                  FOR DELAYED SCREEN  MESSAGES                 
DSRNCOL  DS    XL1                 .   COLUMN  NUMBER                           
DSRNMNUM DS    AL2                 .   MESSAGE NUMBER                           
DSRNMTYP DS    XL1                 .   MESSAGE TYPE                             
DSRNDTYP DS    XL1                 .   MESSAGE DATA    TYPE                     
DSRNDLNG DS    XL1                 .   MESSAGE DATA    LENGTH                   
DSRNDATA DS    CL20                .   MESSAGE DATA                             
*                                                                               
         DS    0F                  FOR DISPLAY RECORD  ROUTINE                  
DRCURSOR DS    XL(L'FVADDR)        .   SAVE    CURSOR  POINTER                  
DRMSGNO  DS    XL(L'FVMSGNO)       .   SAVE    MESSAGE NUMBER                   
*                                                                               
TABOFCOL DS    (MAXCOLS)XL(CSEQLNQ)    ARRAY OF COLUMNS CALULATION COLS         
COLSORT# DS    CL(2*(MAXCOLS+1))   OVER-RIDE SORT, 1 THRU N, CONTIGUOUS         
COLARRY2 DS    CL(MAXCOLS)         ONE BYTE ARRAY TO CHECK CALCULATIONS         
ACCENT   DS    CL1                 YES OR NO - ACCENT ENABLED                   
*                                                                               
ELEMENT  DS    CL(L'APELEM)                                                     
*                                                                               
SVKEY    DS    CL(L'IOKEY)         SAVE IOKEY                                   
SVELEM   DS    CL(L'APELEM)        SAVE APELEM                                  
SVELEM1  DS    CL(L'APELEM)        SAVE APELEM                                  
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
SFDD     DSECT                                                                  
SFDFNUM  DS    XL1                 SCREEN FIELD NUMBER                          
SFDCOLN  EQU   1                      COLUMN  NUMBER                            
SFDDATA  EQU   2                      DATA                                      
SFDDTER  EQU   3                      DATE    RANGE                             
SFDWDTH  EQU   4                      WIDTH                                     
SFDTOTL  EQU   5                      TOTAL   Y/N                               
SFDPRNT  EQU   6                      PRINT   Y/N                               
SFDSORD  EQU   7                      SORT    ORDER                             
SFDHD#1  EQU   8                      HEADING 1                                 
SFDHD#2  EQU   9                      HEADING 2                                 
SFDCSTK  EQU   10                     STACK   UNDER                             
SFDDECM  EQU   11                     DECIMAL PLACES                            
SFDRPTC  EQU   12                     REPEAT CONSTANT                           
SFDPRDN  EQU   13                     PRINT REDUNDANT                           
SFDNGAM  EQU   14                     NEGATIVE AMOUNTS                          
SFDPRCM  EQU   15                     PRINT COMMAS                              
SFDPRZR  EQU   16                     PRINT ZERO AMOUNTS                        
SFDUNLN  EQU   17                     UNDERLINE AMOUNTS                         
SFDPZTT  EQU   18                     PRINT ZERO AMOUNTS                        
SFDBUDG  EQU   19                                                               
SFDFCUR  EQU   20                                                               
SFDTTYP  EQU   21                                                               
                                                                                
SFDILEN  DS    XL1                 SCREEN FIELD INPUT LENGTH                    
SFDCOLNQ EQU   3                   .                                            
SFDCOLM1 EQU   SFDCOLNQ-1          .                                            
SFDDATAQ EQU   12                  .                                            
SFDDTERQ EQU   10                  .                                            
SFDWDTHQ EQU   2                   .                                            
SFDTOTLQ EQU   1                   .                                            
SFDPRNTQ EQU   1                   .                                            
SFDHD#1Q EQU   12                  .                                            
SFDHD#2Q EQU   12                  .                                            
SFDSORDQ EQU   2                   .                                            
SFDCSTKQ EQU   3                   .                                            
SFDDECMQ EQU   4                   .  DECIMAL PLACES                            
SFDPRTCQ EQU   3                   .  REPEAT CONSTANT                           
SFDPRDNQ EQU   3                   .  PRINT REDUNDANT                           
SFDNGAMQ EQU   3                   .  NEGATIVE AMOUNTS                          
SFDPRCMQ EQU   3                   .  PRINT COMMAS                              
SFDPRZRQ EQU   3                   .  PRINT ZERO AMOUNTS                        
SFDUNLNQ EQU   3                   .  UNDERLINE AMOUNTS                         
SFDPZTTQ EQU   3                   .  PRINT ZERO AMOUNTS                        
SFDTTYPQ EQU   LINPTTYP            .  TRANSACTION TYPE FIELD LENGTH             
SFDBUDGQ EQU   LINPBUDG            .  BUDGET           FIELD LENGTH             
SFDFCURQ EQU   3                   .                                            
SFDFLEN  DS    XL1                 SCREEN FIELD   FULL LENGTH                   
SFDIND1  DS    XL1                 SCREEN FIELD   INDICATORS                    
SFDPROT  EQU   X'80'                  PROTECTED        FIELD                    
SFDFIX   EQU   X'40'                  ALWAYS DISPLAYED                          
SFDCNTR  EQU   X'20'                  CENTER INPUT     FIELD                    
SFDLOW   EQU   X'10'                  LOWER  CASE      DATA IS VALID            
SFDDDS   EQU   X'08'                  DDS    ONLY                               
SFDFADR  DS    AL2                 DISPLACENT TO  SAVE FIELD ADDRESS            
SFDREPS  DS    AL4                 VALID  REPORT  TYPES                         
SFDLNQ   EQU   *-SFDD                                                           
         EJECT ,                                                                
SFD2D    DSECT                                                                  
SFD2LN   DS    AL1                                                              
SFD2ROW  DS    AL1                                                              
SFD2COL  DS    AL1                                                              
SFD2FLN  DS    AL1                                                              
SFDRPWDQ EQU   4                                                                
SFD#COLQ EQU   2                                                                
SFD2FLD  DS    AL1                                                              
SFD2ATB  DS    AL1                                                              
SFD2ADR  DS    AL2                                                              
SFD2LNQ  EQU   *-SFD2D                                                          
SFD2DATA DS    0C                                                               
         EJECT ,                                                                
CHDD     DSECT                                                                  
CHDLN    DS    XL1                 LENGTH OF ENTRY                              
CHDFNUM  DS    XL1                 FIELD NUMBER                                 
CHDHD#   DS    XL1                 HEADING NUMBER                               
CHDHD#1  EQU   ROWSTR              HEADING 1                                    
CHDHD#2  EQU   ROWSTR+1            HEADING 2                                    
CHDHDMAX EQU   CHDHD#2             MAX HEADING ROW NUMBER                       
CHDCOLMX EQU   CHDHDMAX+CLIMIT     MAX COLUMN  ROW NUMBER                       
CHDLNQ   EQU   *-CHDD                                                           
CHDDATA  DS    0C                                                               
         EJECT ,                                                                
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
*DDTWABLDD                                                                      
       ++INCLUDE DDTWABLDD                                                      
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRFBD                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACSCR04   04/11/19'                                      
         END                                                                    
