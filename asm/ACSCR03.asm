*          DATA SET ACSCR03    AT LEVEL 099 AS OF 04/11/19                      
*PHASE T60C03C,+0                                                               
*&&ONLIN SET   Y                                                                
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 98 AS OF 12/16/11         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 097 10NOV11 PR000122 Estimate Phase 1                                    
* SMAN 098 16DEC11 PR002242 UK/US MERGE                                         
* JFOS 099 04JAN13 PR003402 ESHRAT/C KWORDS NEED CAC OR CAN IN FORMAT           
*                                                                               
* GHOA 099 11APR19 SPEC-28645 CHANGE BRANCHES (BEST PRACTICE)                   
         TITLE 'ROW ELEMENTS MAINTAINANCE'                                      
T60C03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C03,RA,RR=RE,CLEAR=YES                                       
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         MVI   REPMODE,REPROW                                                   
         MVI   PASTESEQ,X'00'      CLEAR PASTE INSERT SEQ NUMBER                
*                                                                               
         CLI   APMODE,APMVALK      DURING    VALKEY ?                           
         BNE   SCR10               NO,  SKIP                                    
         LA    R2,SCRTXT           INITIALIZE     TEXT                          
*                                                                               
SCR05    CLI   0(R2),X'FF'         END  OF   TABLE ?                            
         BE    SCR10                                                            
         L     R4,0(,R2)           GET  HEADER    ADDRESS                       
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT  FIELD                              
         L     R3,4(,R2)           GET  SCREEN    NUMBER                        
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP TO   NEXT                               
         B     SCR05                                                            
*                                                                               
         USING RESRECD,R2                                                       
SCR10    DS    0H                                                               
         LA    R2,IOKEY                                                         
         EJECT ,                                                                
***********************************************************************         
*        INITIALIZE ROW INFORMATION                                   *         
***********************************************************************         
         USING ROWD,R4                                                          
         MVI   ROW#CSR,0           INITIALIZE ROW STARTING POINT                
         CLI   TWALREC,RECROW                                                   
         BE    ROW10                                                            
         LA    R4,ROWFRSTH                                                      
         LA    RE,ROWDATAH                                                      
         ST    RE,ACURSOR          RESET TO FIRST ROW 1ST TIME IN               
*                                                                               
ROW10    CLI   APPFKEY,PFKDEL      DELETE ROW?                                  
         BE    ROW20                                                            
         CLI   APPFKEY,PFKINS      INSERT ROW?                                  
         BNE   ROW50                                                            
         LA    R4,ROWTABH          SEE IF ANYTHING IN LAST LINE                 
         SHI   R4,ROWLNQ                                                        
         CLI   ROWDATAH+5,0                                                     
         BNE   IVALINS             YES, CAN'T INSERT                            
*                                                                               
ROW20    L     R3,ACURSOR          CURRENT CURSOR POSITION                      
         LA    RF,ROWFRSTH                                                      
         CR    R3,RF               IS IT BEFORE FIRST ROW?                      
         BL    IVALPFK                                                          
         LA    RF,ROWTABH                                                       
         CR    R3,RF               IS IT AFTER LAST ROW?                        
         BNL   IVALPFK                                                          
*                                                                               
         LA    R1,1                                                             
         LA    R4,ROWFRSTH         FIND WHICH LINE CURSOR IS ON                 
ROW25    LA    RF,ROWLNQ(,R4)                                                   
         CR    R3,RF               IS IT LESS THAN THE END OF LINE?             
         BL    ROW30                                                            
         LR    R4,RF                                                            
         LA    R1,1(,R1)           NEXT   LINE                                  
         B     ROW25                                                            
*                                                                               
ROW30    STC   R1,ROW#CSR          ROW WHERE CURSOR CURRENTLY IS                
         LA    R3,ROWDATAH         FORCE ROW TO THE DATA FIELD                  
         ST    R3,ACURSOR          SAVE THE CURSOR                              
         B     ROW200                                                           
*                                                                               
ROW50    DS    0H                  CASE = NOT DELETE OR INSTERT ROW             
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BNE   ROW100              YES, SKIP                                    
         LA    R4,ROWFRSTH         NO, SET CURSOR TO 1ST ROW                    
         LA    RE,ROWDATAH                                                      
         ST    RE,ACURSOR                                                       
         B     ROW200                                                           
         EJECT ,                                                                
*=====================================================================*         
*     SET CURSOR TO START OF CLOSEST ROW BASED ON WHERE IT IS NOW     *         
*=====================================================================*         
ROW100   DS    0H                                                               
         CLI   APMODE,APMVALK      VALKEY ?                                     
         BE    ROW200              YES,   SKIP THIS ROUTINE                     
         CLI   APMODE,APMDISK      DISKEY ?                                     
         BE    ROW200              YES,   SKIP THIS ROUTINE                     
         CLI   APACTN,ACTCPY                                                    
         BE    ROW200                                                           
*                                                                               
         L     RE,ACURSOR          CURRENT     LOCATION  OF   CURSOR            
         LA    R4,ROWFRSTH                                                      
         LA    R1,ROWDATAH                                                      
         ST    R1,ACURSOR          DEFAULT     CURSOR    LOCATION               
         CR    RE,R1               CURSOR      BEFORE    ROWS ?                 
         BL    ROW200              YES,   DONE                                  
*                                                                               
         LA    RF,ROWTABH          POINT  PAST ROW  FIELDS                      
         CR    RE,RF               CURSOR PAST ROW  FIELDS ?                    
         BNL   ROW120              YES,   SPECIAL   PROCESSING                  
*                                                                               
ROW110   DS    0H                  FIND   THE  RIGHT     ROW                    
         AHI   R4,ROWLNQ           ->     NEXT ROW                              
         CR    RE,R4               CURSOR AFTER     THIS ROW ?                  
         BNL   ROW110              YES,   GET  NEXT ROW                         
*                                                                               
         SHI   R4,ROWLNQ           BACK   UP   ONE  ROW                         
         LA    R1,ROWDATAH         ->     DATA FIELD                            
         ST    R1,ACURSOR          SET    CURSOR    LOCATION                    
         B     ROW200                                                           
*                                                                               
ROW120   DS    0H                  CURSOR PAST ROW  FIELDS                      
         LA    RF,ROWTAB+L'ROWTAB  ->     TEXT FOR  ROW  TO   STRT PAGE         
         CR    RE,RF               BEFORE THIS LINE ?                           
         BL    ROW200              YES,   USE  DEFAULT                          
*                                                                               
         LA    RF,ROWRPG+L'ROWRPG  ->     PAST ROW  TO   START     PAGE         
         CR    RE,RF               AFTER  THIS LINE ?                           
         BNL   ROW200              YES,   USE  DEFAULT                          
         LA    R1,ROWRPGH          ->     ROW  TO   START     NEW  PAGE         
         ST    R1,ACURSOR          SAVE   CURSOR                                
*                                                                               
ROW200   DS    0H                                                               
         EJECT ,                                                                
         CLI   APMODE,APMVALR      VALREC ?                                     
         BE    ROW210              YES, CONTINUE                                
         CLI   APMODE,APMDISR      DISREC ?                                     
         BNE   ROW300              NO,  SKIP                                    
*                                                                               
ROW210   DS    0H                                                               
         L     R2,ACURSOR          GET  CURSOR    LOCATION                      
*                                                                               
         CLI   APPFKEY,PFKHLP      HELP PF   KEY  ?                             
         BNE   ROW300              NO,  SKIP                                    
         MVI   SVINSSEQ,0          CLEAR     SAVE INSSEQ                        
         CLI   APACTN,ACTCHA       ARE  WE   UPDATING  THE  RECORD ?            
         BE    ROW220              YES, CONTINUE                                
         CLI   APACTN,ACTADD       ARE  WE   ADDING    A    RECORD ?            
         BNE   ROW300              NO,  SKIP                                    
*                                                                               
ROW220   DS    0H                                                               
         LA    R4,ROWFRSTH         IS   CURSOR    BEFORE    ALL  LINES?         
         CR    R2,R4                                                            
         BL    ROW300              YES, SKIP                                    
         LA    RF,ROWTABH          IS   CURSOR    AFTER     ALL  LINES?         
         CR    R2,RF                                                            
         BNL   ROW300              YES, SKIP                                    
*                                                                               
         GOTO1 AFVAL,(R2)          ANY  DATA ?                                  
         BE    ROW300              YES, SKIP                                    
         LR    RE,R2               NO,  SAVE OFFSET                             
         S     RE,ATWA                  FROM START     OF   TWA                 
         ST    RE,CUR@RHLP              TO   ENABLE    PASTE                    
         LR    R1,R2               GET  CURSOR    LOCATION                      
         LA    RE,ROWDATAH         ->   1ST  DATA FIELD                         
         SR    R1,RE               GET  OFFSET    FROM 1ST  DATA FIELD          
         SR    R0,R0                                                            
         D     R0,=A(ROWLNQ)       GET  ROW  NUM  (1ST =    0)                  
         AHI   R1,1                GET  ROW  NUM  (1ST =    1)                  
         STC   R1,SVINSSEQ         SET  INSERT    SEQUENCE  NUM                 
*                                                                               
ROW300   DS    0H                                                               
         DROP  R4                                                               
         EJECT ,                                                                
*=====================================================================*         
*    Branch table of modes                                            *         
*=====================================================================*         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY              DISKEY                                       
         B     DISREC                                                           
         B     DELRWEL             DELETE ROW ELEMENT'S ONLY                    
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
         B     CPYRWEL             COPY ROW ELEMENT'S                           
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
*=====================================================================*         
*   Exit for modual                                                   *         
*=====================================================================*         
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT95              NO                                           
         XC    ACURDEF,ACURDEF     SET TO BEGINING OF HELP                      
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP                                                    
         CLI   TWASWPAC,ACTADD     ACTION = ADD ?                               
         BNE   EXIT90              NO,      CONTINUE                            
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
                                                                                
EXIT90   MVC   APPARM(1),TWASWPRE  SWAP     TO   NEW  RECORD                    
         MVC   APPARM+1(1),TWASWPAC         TO   NEW  ACTION                    
         MVI   APPFKEY,0           CLEAR    PF   KEY                            
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         CLI   APMODE,APMDISR      MODE IS   DISPLAY RECORD                     
         BNE   XIT                 NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY IS HAPPY                        
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
*=====================================================================*         
*        Validate key and get format record(s)                        *         
*=====================================================================*         
VALKEY   DS    0H                                                               
         MVI   HAVE5TH,NO                                                       
         MVI   NEWKEY,NO           RESET TO NO                                  
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,ROWCODEH                                                   
         MVC   RESKFORM,FVIFLD     FORMAT                                       
         BE    VALKEY15                                                         
         TM    ROWCODEH+4,FVITHIS  ANY INPUT THIS TIME?                         
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   ROWCODE,SAVFORM                                                  
         OI    ROWCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM        SAVE FORMAT FOR PF SWITCHING             
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         OI    SCRTYPH+6,FVOXMT                                                 
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    VALKEY40                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN ON TO TRICK ACTION COPY                 
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         MVI   NEWKEY,YES          WE ADDING A NEW RECORD                       
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
         B     VALKEY98                                                         
*                                                                               
VALKEY40 CLI   APACTN,ACTCHA                                                    
         BNE   VALKEY98                                                         
         LA    R2,IOKEY                                                         
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
         EJECT                                                                  
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
CPYRWEL  DS    0H                  ONLY COPY ROW ELEMENTS                       
         CLI   NEWKEY,YES          ARE WE ADDING A NEW RECORD?                  
         BE    *+8                                                              
         NI    APINDS,X'FF'-APIOKADD    TURN OFF TO TRICK ACTION COPY           
*                                                                               
DELRWEL  DS    0H                  ONLY DELETE ROW ELEMENTS                     
         EJECT ,                                                                
         USING ROWD,R4                                                          
VALREC   DS    0H                                                               
         MVC   SAV@RHLP,CUR@RHLP   SAVE RETURN    FROM HELP ADDR                
         MVC   SAVSTSEQ,ROW#CSR    SAVE ROW  START     SEQUENCE  NUMBER         
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,ROWNMEH                                                    
         BNE   VALREC00            NAME HAS NOT BEEN INPUT                      
         GOTO1 ADDNAME,APPARM,(R2),ROWNMEH    GET FORMAT NAME                   
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC00 CLI   APACTN,ACTCPY       COPYING A RECORD?                            
         BE    *+8                                                              
         CLI   APACTN,ACTADD       ADDING NEW RECORD?                           
         BNE   VALREC01                                                         
         CLI   NEWKEY,YES                                                       
         BNE   VALREC01                                                         
         MVC   APREPNUM,APREPUL+1  CHANGE FROM MULTY LEDG TO ONE LEDG           
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON ERROR, EXIT                               
         XC    APELEM,APELEM                                                    
*                                                                               
         USING RFLELD,R3                                                        
         LA    R3,APELEM                                                        
         MVI   RFLEL,RFLELQ        X'C5' FILTER TYPE                            
         MVI   RFLLN,RFLLNQ+2                                                   
         MVI   RFLTYPE,RFLLDG      SET LEDGER ON RECORD                         
         MVC   RFLDATA,APREPUL     DEFAULT UNIT AND LEDGER                      
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON ERROR, EXIT                               
         DROP  R3                                                               
*                                                                               
         USING RPFELD,R1                                                        
VALREC01 L     R1,AIOAREA1                                                      
         XC    RANKON,RANKON       INIT RANK ON                                 
         XC    ORANKON,ORANKON     INIT RANK ON OLD                             
         MVI   DWNTYPE,0                                                        
         MVI   MIXESTK,0                                                        
         MVI   APELCODE,RPFELQ     X'C4' PROFILE ELEMENT                        
         GOTO1 GETEL                                                            
         BNE   VALREC02                                                         
         MVC   RANKON,RPFRKON      SAVE RANK ON                                 
         MVC   ORANKON,RPFRKON     SAVE RANK ON                                 
*                                                                               
         TM    RPFXMIT,RPFXACNT    ACCENT ENABLED ?                             
         BZ    *+12                                                             
         OI    DWNTYPE,DWNTACNT                                                 
         B     VALREC02                                                         
         TM    RPFXMIT,RPFXQREP    QREPORTS ENABLED ?                           
         BZ    *+8                                                              
         OI    DWNTYPE,DWNTQREP                                                 
         DROP  R1                                                               
*                                                                               
         USING RCLELD,R1                                                        
VALREC02 MVI   NCOLS,0             NUMBER OF COLUMNS                            
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     X'C3' COLUMN ELEMENT                         
         GOTO1 GETEL               GET AN ELEMENT                               
         BNE   VALREC03            NO COLUMNS                                   
VREC02C  GOTO1 NEXTEL              FIND LAST COLUMN                             
         BE    VREC02C                                                          
         MVC   NCOLS,RCLSEQ                                                     
         DROP  R1                                                               
*                                                                               
VALREC03 MVC   RESKEY,APRECKEY                                                  
         CLI   APACTN,ACTDEL       DELETE ONLY                                  
         BNE   VALREC08            NO,  SKIP                                    
         CLI   RANKON,C'R'         RANK ON ROW ?                                
         BE    IVALRRNK            YES, DELETE OF A RANKING FIELD               
         MVI   APELCODE,RRWELQ     DELETE ROW ELEMENTS                          
         L     R1,AIOAREA1                                                      
         GOTO1 DELEL                                                            
         B     VALREC92            Deleted all rows                             
*                                                                               
         USING DEFTABD,R2                                                       
VALREC08 MVI   NROWSEQ,1           INITIALIZE NEW ROW NUMBER                    
         MVI   CUROW#,1            INITIALIZE ROW NUMBER                        
         MVI   MIDLNFG,NO          INITIALIZE TO NO MIDLINE ENCOUNTERED         
         LA    R4,ROWFRSTH         START OF ROWS                                
*                                                                               
         USING RRWELD,R3                                                        
VALREC10 L     R1,AIOAREA1         A(FORMAT RECORD)                             
         MVI   APELCODE,RRWELQ     X'C2', row element                           
         MVC   ELEMSEQ,CUROW#                                                   
         SR    R3,R3                                                            
         GOTO1 GETEL                                                            
         BNE   VALREC11            Not found so new element                     
         LR    R3,R1                                                            
         MVI   0(R1),X'FF'         Mark element deleted                         
*                                                                               
VALREC11 CLI   APPFKEY,PFKDEL      Delete PFkey used ?                          
         BNE   VALREC12            No                                           
         CLC   ROW#CSR,CUROW#      Do we want to delete this row ?              
         BNE   VALREC12            No, skip                                     
         MVC   ROWDATA,SPACES      Clear data to delete row                     
*                                                                               
VALREC12 CLC   CUROW#,SVINROW#     Was this line inserted ?                     
         BNE   VALREC14            No, skip                                     
         MVI   SVINROW#,0          Yes, clear row number                        
         ZIC   R6,CUROW#                                                        
         GOTO1 =A(FIXXTREL),APPARM,(R6),1,RR=APRELO                             
         GOTO1 =A(FIX5TH),APPARM,(R6),1,RR=APRELO    Adjust values              
         GOTO1 =A(FIXRPROF),APPARM,1,RR=APRELO       Fix row profile            
         LR    R1,R3                                                            
         SR    R3,R3                                                            
*                                                                               
VALREC13 SR    RF,RF               Increase all sequences afterwards            
         IC    RF,RRWSEQ-RRWELD(R1)                                             
         AHI   RF,1                                                             
         STC   RF,RRWSEQ-RRWELD(R1)                                             
         GOTO1 NEXTEL                                                           
         BE    VALREC13                                                         
*                                                                               
VALREC14 GOTO1 AFVAL,ROWDATAH      Any keyword input ?                          
         BE    VALREC16            Yes                                          
         ZIC   R6,CUROW#                                                        
         GOTO1 =A(FIXXTREL),APPARM,(R6),-1,RR=APRELO                            
         GOTO1 =A(FIX5TH),APPARM,(R6),-1,RR=APRELO                              
         B     VALREC80            NO INPUT, CHECK PFKHLP                       
*                                                                               
VALREC16 LTR   R3,R3                                                            
         BZ    VALREC18                                                         
         GOTO1 =A(MATCHON),APPARM,(RRWDATLN,RRWNDATA),ROWDATA,RR=APRELO         
         BE    VALREC18                                                         
         ZIC   RF,RRWSEQ                                                        
         GOTO1 =A(FIX5TH),APPARM,(RF),0,RR=APRELO                               
         DROP  R3                                                               
*                                                                               
VALREC18 GOTO1 VALDEF,ROWDATAH                                                  
         BNE   IVALKEYW                                                         
         ST    R1,DEFENTRY                                                      
         LR    R2,R1               R1 RETURNED POINTING TO VALID ENTRY          
*                                                                               
         USING RRWELD,R9                                                        
         LA    R9,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RRWEL,RRWELQ        ELEMENT CODE                                 
         MVI   RRWLN,RRWNLNQ       ELEMENT LENGTH                               
         MVC   RRWSEQ,NROWSEQ      SEQUENCE                                     
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,RRWNDATA,FVIFLD  ROW DATA                                     
         LA    RF,RRWNLNQ+1(,RF)   LENGTH OF ELEMENT                            
         STC   RF,RRWLN                                                         
         MVC   RRWDATLN,FVILEN     LENGTH OF DATA FIELD                         
         MVC   RRWOPT,DEFRWIND     TURN ON ROW INDICATORS                       
         NI    RRWOPT,CDE+NME      TURN OFF INDICATORS EXCEPT NME+CDE           
         OI    RRWOPT,RRWNEWEL     NEW TYPE ELEMENT                             
         NI    RRWOPT2,TURNOFF-RRWAIDX     Reset                                
*        OI    RRWOPT3,DEFRWBDR    TURN ON ROW INDICATOR FOR BDR ATTRI          
*&&UK                                                                           
         TM    DEFTYPE,DEFPRD      PERIOD TYPE KEYWORD                          
         BZ    *+8                                                              
         OI    RRWOPT,RRWPRDTY     YES - MUST ENTER PERIOD RANGE                
*&&                                                                             
*&&US                                                                           
         LA    RF,0                                                             
         TM    MIXESTK,DEFOEST+DEFNEST                                          
         BNM   *+8                                                              
         LA    RF,1                HAVE OLD *OR* NEW EST KEYWORD                
*&&                                                                             
         OC    MIXESTK,DEFTYP2                                                  
*&&US                                                                           
         TM    DEFTYP2,DEFCACR     TEST COL REQUIRES C/A KEYWORD                
         BZ    *+12                                                             
         LA    RE,ROWDATAH                                                      
         ST    RE,FULL             SAVE CURSOR POS. IN CASE OF ERROR            
                                                                                
         CHI   RF,0                TEST HAD OLD OR NEW EST KEYWORDS             
         BE    VALREC19            NO                                           
         TM    MIXESTK,DEFOEST+DEFNEST  TEST NOW HAVE OLD *AND* NEW             
         BNO   VALREC19                                                         
         LA    RE,ROWDATAH                                                      
         ST    RE,FULL             SAVE CURSOR POS. FOR ERROR                   
*&&                                                                             
VALREC19 CLI   DEFDATES,0          ANY  VALID DATA BASES ?                      
         BE    VALREC20            NO,  SKIP                                    
         MVI   RRWDATES,RRWTRDT    DEFAULT TO TRANSACTION DATE BASIS            
         TM    DEFDATES,DEFTRDT    TRANSACTION BASIS VALID ?                    
         BO    VALREC20            YES, CONTINUE                                
         MVI   RRWDATES,RRWMODT    DEFAULT TO MOA DATE BASIS                    
         TM    DEFDATES,DEFMODT    MOA  BASIS VALID ?                           
         BO    VALREC20            YES, CONTINUE                                
         DC    H'00'               NO,  KEYWORD DEFINITION TABLE ERROR          
*                                                                               
VALREC20 GOTO1 VSCANNER,APPARM,ROWDATAH,(6,BLOCK),SCNP3NEQ                      
         SR    R0,R0                                                            
         IC    R0,APPARM+4                                                      
         BCTR  R0,0                                                             
         STC   R0,NPARMS                                                        
         CLC   NPARMS,DEFRWMIN                                                  
         BL    IVALLOW             **ERROR** TOO FEW PARAMETERS                 
         CLC   NPARMS,DEFRWMAX                                                  
         BH    IVALHIGH            **ERROR** TOO MANY PARAMETERS                
         CLI   NPARMS,0            NO VALUES NEEDED                             
         BE    VALREC35                                                         
         MVI   PARM_N,1                                                         
         LA    R3,BLOCK+32         SKIP FIRST BLOCK                             
*                                                                               
*MN                                USER FIELD (SPECIAL) ?                       
*&&US*&& CLC   DEFDDNUM,=AL2(AC#RSUSC)                                          
*&&US*&& BE    VALREC24            YES, VALIDATE USER FIELDS                    
         CLC   DEFDDNUM,=AL2(AC#RSUSF)                                          
         BNE   VALREC25            NO,  PROCESS PARAMETERS                      
VALREC24 BAS   RE,VALRUF           YES, VALIDATE USER FIELDS                    
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS?                                 
         BE    VALREC35            NO,  CONTINUE                                
         B     VALREC99                                                         
*                                                                               
VALREC25 DS    0H                  VALIDATE THE PARAMETERS                      
         GOTO1 VALPARM,APPARM,(R9),(NPARMS,BLOCK),DEFENTRY,0                    
         BNE   VALREC99                                                         
*                                                                               
VALREC35 GOTO1 AFVAL,ROWTYPEH                                                   
         BE    VALREC37                                                         
         MVI   ROWTYPE,RRWLHEAD                                                 
         CLI   MIDLNFG,YES         ARE WE UP TO MIDLINES?                       
         BNE   VALREC37                                                         
         MVI   ROWTYPE,RRWMID      MUST BE A MIDLINE                            
*                                                                               
VALREC37 MVC   RRWTYPE,ROWTYPE                                                  
         CLI   ROWTYPE,RRWMID      IS IT A MIDLINE?                             
         BNE   VALREC39                                                         
         MVI   MIDLNFG,YES         FORCE TO RECOGNIZE NO MORE HEADS             
         TM    RRWOPT,RRWADR                                                    
         BO    VALREC38                                                         
*&&US*&& TM    RRWOPT3,RRWBDR                                                   
         BZ    VALREC40                                                         
VALREC38 GOTO1 AFVAL,ROWDATAH                                                   
         MVCDD FVXTRA(4),AC#RSADD                                               
         B     IVALADR                                                          
*                                                                               
VALREC39 CLI   MIDLNFG,NO          NO HEAD AFTER MIDLINE                        
         BNE   IVALHEAD                                                         
         CLI   RRWTYPE,RRWLHEAD    PRINT AS LEFT HEAD?                          
         BE    *+8                                                              
         CLI   RRWTYPE,RRWCHEAD    PRINT AS CENTER HEAD?                        
         BE    *+8                                                              
         CLI   RRWTYPE,RRWRHEAD    PRINT AS RIGHT HEAD?                         
         BE    *+8                                                              
         CLI   RRWTYPE,NO          SUPPRESS PRINTING ROW?                       
         BNE   IVALTYPE            No such type                                 
         OI    RRWOPT,RRWPAGE      PAGE ON HEADING                              
*                                                                               
VALREC40 GOTO1 AFVAL,ROWTOTLH                                                   
         BE    VALREC42                                                         
         MVC   ROWTOTL,APNO                                                     
         OI    ROWTOTLH+6,FVOXMT                                                
*                                                                               
VALREC42 DS    0H                                                               
*ALREC42 CLI   ROWTOTL,C'I'        Accent indexing                              
*        BNE   VALREC43                                                         
*        OI    RRWOPT2,RRWAIDX     Row indexing on for Accent                   
*        B     VALREC45                                                         
                                                                                
VALREC43 CLC   ROWTOTL,APNO        TOTAL ON ROW?                                
         BE    VALREC45                                                         
         OI    RRWOPT,RRWTOT                                                    
         CLC   ROWTOTL,APYES       TOTAL ON ROW?                                
         BE    VALREC45                                                         
         CLI   ROWTOTL,C'S'        TOTAL ON SEPERATE PAGE?                      
         BNE   *+12                                                             
         OI    RRWOPT,RRWTOTSP                                                  
         B     VALREC45                                                         
         CLI   ROWTOTL,C'B'        TOTAL AT BOTTOM OF PAGE?                     
         BNE   IVALTOT                                                          
         OI    RRWOPT2,RRWBTM                                                   
*                                                                               
VALREC45 SR    R1,R1                                                            
         GOTO1 AFVAL,ROWPRFXH                                                   
         BNE   VALREC50                                                         
         CLI   ROWTYPE,RRWMID      IS IT A MIDLINE?                             
         BE    IVALIPUT            NOT ALLOWED WITH MIDLINE                     
         SR    R1,R1                                                            
         IC    R1,RRWDATLN         LENGTH OF DATA                               
         LA    RE,RRWNDATA(R1)     POINT TO END OF RRWNDATA                     
         MVC   RRWPFXLN,FVILEN     SAVE LENGTH OF PREFIX                        
         IC    R1,FVXLEN                                                        
         EXMVC R1,0(RE),FVIFLD                                                  
         LA    RE,1(R1,RE)         RE = END       OF ELEMENT                    
         LA    RF,RRWEL            RF = BEGINGING OF ELEMENT                    
         SR    RE,RF                                                            
         STC   RE,RRWLN                                                         
*                                                                               
VALREC50 GOTO1 AFVAL,ROWRPGH                                                    
         TM    FVIIND,FVINUM       IS IT NUMERIC ?                              
         L     R1,SCFULL           GET NUMERIC VALUE                            
         CHI   R1,0                                                             
         BL    IVALPARM                                                         
         CHI   R1,MAXROWS                                                       
         BH    IVALPARM                                                         
         SR    R1,R1                                                            
         CLC   NROWSEQ,SCFULL+3    MATCH ROW WITH INPUT NUMBER                  
         BNE   VALREC52                                                         
         OI    RRWOPT2,RRWPGNO     TURN ON FLAG TO RENUMBER PAGE                
         CLI   MIDLNFG,YES         ARE WE UP TO MIDLINES?                       
         BE    IVALPARM            YES, SO NO GOOD                              
*                                                                               
VALREC52 L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99            ON ERROR, EXIT                               
         SR    R0,R0                                                            
         IC    R0,NROWSEQ          ROW SEQUENCE NUBMER                          
         AHI   R0,1                BUMP TO NEXT NUMBER                          
         STC   R0,NROWSEQ          SAVE NUMBER                                  
         B     VALREC90                                                         
*                                                                               
VALREC80 CLI   APPFKEY,PFKHLP      HELP PF   KEY ?                              
         BNE   VALREC85            NO,  SKIP                                    
         LA    RF,ROWDATAH         ->   HEADER    FOR  DATA                     
         S     RF,ATWA             IS   THE  DELETED   LINE PRIOR    TO         
         C     RF,SAV@RHLP              THE  PASTE     LINE ?                   
         BNL   VALREC87            NO,  SKIP                                    
         L     RF,CUR@RHLP         YES, SUBTRACT  ONE  LINE                     
         SHI   RF,ROWLNQ                FROM THE                                
         ST    RF,CUR@RHLP              PASTE     LINE                          
         ZIC   RF,SVINSSEQ         AND  SUBTRACT  ONE                           
         BCTR  RF,0                     FROM THE                                
         STC   RF,SVINSSEQ              LINE NUMBER                             
         B     VALREC87            CONTINUE                                     
*                                                                               
VALREC85 CLI   ROW#CSR,0           INSERT OR DELETE PFKEY ?                     
         BE    VALREC87            NO, SKIP                                     
         ZIC   RF,SAVSTSEQ         GET ORIGINAL ROW SEQUENCE NUMBER             
         BCTR  RF,0                MINUS ONE                                    
         MHI   RF,-ROWLNQ          TIMES ROW LENGTH                             
         LA    RE,ROWFRSTH         ->   1ST ROW                                 
         AR    RE,RF               ->   INSERT/DELETE LINE                      
*                                  IS THE DELETED LINE PRIOR TO                 
         CR    R4,RE                  THE INSERT/DELETE LINE                    
         BNL   VALREC87            NO, SKIP                                     
         ZIC   RF,ROW#CSR          YES, SUBTRACT ONE FROM THE SEQ#              
         BCTR  RF,0                                                             
         STC   RF,ROW#CSR                                                       
*                                  NULL LINE AND  NOT  INSERT    PF KEY         
VALREC87 CLI   RANKON,C'R'         RANK ON   ROWS ?                             
         BNE   VALREC90            NO,  SKIP TO   NEXT ROW                      
         CLC   CUROW#,ORANKON+1    RANK ON   THIS ROW ?                         
         BE    IVALRRNK            YES, DELETE OF A RANKING FLD                 
         GOTO1 =A(FIXRPROF),APPARM,-1,RR=APRELO     FIX ROW PROFILE #S          
*                                                                               
VALREC90 ZIC   RE,CUROW#           UPDATE OLD ROW SEQUENCE NUMBER               
         LA    RE,1(,RE)                                                        
         STC   RE,CUROW#                                                        
         LA    RF,ROWTABH          END OF SCREEN                                
         LA    R4,ROWLNQ(,R4)      LOOP TO NEXT ROW ON SCREEN                   
         CR    R4,RF               ARE WE AT OR BEYOND END OF SCREEN?           
         BL    VALREC10                                                         
         TM    MIXESTK,DEFOEST+DEFNEST                                          
         BO    IVALMIX                                                          
*&&US                                                                           
         TM    MIXESTK,DEFCACR     TEST KEYWORD REQUIRES CONTRA A/C             
         BZ    *+12                                                             
         TM    MIXESTK,DEFCACI     TEST CONTRA A/C KEYWORD PRESENT              
         BZ    IVALNOCA                                                         
*&&                                                                             
VALREC92 DS    0H                                                               
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
         TM    DWNTYPE,DWNTACNT+DWNTQREP   ACCENT OR QREPORT SPEC ?             
         BZ    VALREC94                    NO - OK                              
*&&UK                                                                           
         L     R1,AIOAREA1                 A(FORMAT RECORD)                     
         MVI   APELCODE,RRWELQ             X'C2', row element                   
         GOTO1 GETEL                                                            
         BE    VALREC93                                                         
         MVC   FVMSGNO,=AL2(ACEM1R1C)                                           
         B     VALREC99            MUST DEFINE AT LEASE 1 ROW                   
*&&                                                                             
VALREC93 TM    DWNTYPE,DWNTQREP                                                 
         BZ    VALREC94                                                         
VREC93C  GOTO1 NEXTEL               FIND LAST ROW                               
         BE    VREC93C                                                          
         XR    RE,RE                                                            
         IC    RE,RRWSEQ                                                        
         XR    RF,RF                                                            
         IC    RF,NCOLS                                                         
         AR    RE,RF                                                            
         CHI   RE,20                MORE THAN 20 ROWS+COLUMNS?                  
         BNH   VALREC94                                                         
         MVC   FVMSGNO,=AL2(ACEQR20)                                            
         B     VALREC99                                                         
                                                                                
VALREC94 L     R2,AIOAREA1                                                      
*                                                                               
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD           ADD A RECORD?                          
         BO    VALREC95                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA           CHANGE A RECORD?                       
         BO    VALREC95                                                         
         DC    H'0'                      WHAT THE HELL?                         
*                                                                               
VALREC95 GOTO1 AIO                                                              
         BE    VALREC97                                                         
         TM    IOERR,IOEDUP        DELETED RECORD (DUPLICATE) ON FILE           
         BNZ   *+6                                                              
         DC    H'0'                BAD WRITE OR SOMETHING DUDE                  
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
         CLI   0(RE),EOR           ANY ELEMENTS ON RECORD ?                     
         BNE   *+8                                                              
         OI    RESRSTA,X'80'       MARK DELETED, NO ELEMENT ON RECORD           
         MVC   IOADDR,AIOAREA0                                                  
         GOTO1 AIO,IOWRITE+IOACCFIL                                             
         DROP  R2                                                               
*                                                                               
VALREC98 DS    0H                                                               
         CLI   APACTN,ACTDEL       DELETE ONLY                                  
         BE    DISREC                                                           
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    DISREC                                                           
         CLI   APMODE,APMNEWK      ACTION COPY?                                 
         BNE   DISREC              NO                                           
         CLI   NEWKEY,YES          IS IT REALY A NEW KEY?                       
         BNE   IVALRCPY            NO,UPDATE RECORD WITH ROWS ONLY              
         B     EXIT                YES, ADD NEW RECORD WITH ROWS ONLY           
         DROP  R4,R9                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY ROW DATA                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R4                                                          
DISREC   L     R2,AIOAREA1         ROW ELEMENTS                                 
         TWAXC ROWNMEH,ROWTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),ROWNMEH                                      
         GOTO1 GETPER,APPARM,(R2),ROWOWNH                                       
         MVI   SVINROW#,0          CLEAR OUT SAVE INSERT ROW NUMBER             
*                                                                               
         LA    R4,ROWFRSTH                                                      
         LA    RE,ROWTABH                                                       
DISREC05 MVI   ROWNUM,C' '         CLEAR OUT FIRST CHARACTER                    
         OI    ROWNUMH+6,FVOXMT                                                 
         LA    R4,ROWLNQ(,R4)                                                   
         CR    R4,RE                                                            
         BL    DISREC05                                                         
*                                                                               
         MVC   ROWRPG,=C'00'                                                    
         OI    ROWRPGH+6,FVOXMT                                                 
*                                                                               
         OC    KWDPASTE,KWDPASTE   ANY  KEYWORD   FROM HELP FACILITY ?          
         BZ    DISREC10            NO,  SKIP                                    
         ZIC   RF,SVINSSEQ         LOAD THE  INSERT    SEQUENCE  NUMBER         
         STC   RF,ROW#CSR          SAVE THE  INSERT    SEQUENCE  NUMBER         
         STC   RF,PASTESEQ         SAVE THE  PASTE     SEQUENCE  NUMBER         
         L     RE,CUR@RHLP         ->   ADDR RETURNED  FROM HELP                
         A     RE,ATWA                  PLUS ADDR OF   TWA                      
         ST    RE,ACURSOR          SAVE CURSOR    ADDRESS                       
*                                  INSERT    KEYWORD                            
         MVC   8(L'KWDPASTE,RE),KWDPASTE                                        
         OI    4(RE),FVITHIS       SAY  DATA INPUTED                            
         MVI   5(RE),L'KWDPASTE    LENGTH    OF   KEYWORD                       
         OI    6(RE),FVOXMT        TRANSMIT                                     
*                                                                               
DISREC10 XC    KWDPASTE,KWDPASTE   CLEAR     KEYWORD                            
         CLI   APPFKEY,PFKHLP      HELP PF   KEY  ?                             
         BE    DISREC15            YES, SKIP                                    
*                                  CLEAR     CURSOR    ADDR FOR  RETURN         
         XC    CUR@RHLP,CUR@RHLP             FROM HELP                          
         MVI   SVINSSEQ,0          CLEAR     SAVE INSSEQ                        
*                                                                               
DISREC15 MVI   APELCODE,RRWELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC95                                                         
*                                                                               
         USING RRWELD,R1                                                        
         LA    R3,1                ROW  NUMBER                                  
         LA    R4,ROWFRSTH                                                      
DISREC20 CLI   APPFKEY,PFKINS      INSERT    USED ?                             
         BNE   DISREC22            NO,  DO   NOT  MODIFY                        
         CLI   ROW#CSR,0           DO   WE   INSERT    ON   1ST  ROW ?          
         BE    DISREC25            YES, SO   OK   WE   ARE  THERE               
         CLM   R3,1,ROW#CSR        DO   WE   INSERT    ON   THIS ROW ?          
         BNE   DISREC25            NO,  SKIP                                    
         MVC   SVINROW#,ROW#CSR    SAVE THE  INSERT    ROW  NUMBER              
         AHI   R3,1                YES, INCREMENT ROW  COUNTER                  
         AHI   R4,ROWLNQ           NEXT ROW                                     
         B     DISREC25            CONTINUE                                     
*                                                                               
DISREC22 CLI   PASTESEQ,0          JUST PASTED    A    KEYWORD ?                
         BE    DISREC25            NO,  SKIP                                    
         CLM   R3,1,PASTESEQ       WAS  THIS LINE PASTED ?                      
         BNE   DISREC25            NO,  SKIP                                    
         MVC   SVINROW#,PASTESEQ   SAVE THE  INSERT    ROW  NUMBER              
         MVI   PASTESEQ,0          CLEAR     SWITCH                             
         AHI   R3,1                INCREMENT ROW  COUNTER                       
         AHI   R4,ROWLNQ           NEXT ROW                                     
*                                                                               
DISREC25 CLI   RRWDATLN,L'ROWDATA                                               
         BNH   *+6                                                              
         DC    H'00'               TOO BIG FOR FIELD                            
*                                                                               
         MVC   ROWDATA,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,RRWDATLN         LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ROWDATA(0),RRWNDATA                                              
         MVC   ROWTYPE,RRWTYPE                                                  
         CLI   ROWTYPE,C' '                                                     
         BH    *+8                                                              
         MVI   ROWTYPE,RRWLHEAD                                                 
*                                                                               
         TM    RRWOPT2,RRWHIDE     ONLY SET BY STEREO                           
         BZ    *+8                                                              
         MVI   ROWTYPE,C'N'                                                     
*&&US                                                                           
         MVC   ROWTOTL,APNO                                                     
         TM    RRWOPT2,RRWAIDX     Indexing for Accent?                         
         BZ    *+8                                                              
         MVI   ROWTOTL,C'I'                                                     
*&&                                                                             
*        MVC   ROWTOTL,APNO                                                     
*        TM    RRWOPT2,RRWAIDX     Indexing for Accent?                         
*        BZ    *+8                                                              
*        MVI   ROWTOTL,C'I'                                                     
                                                                                
         TM    RRWOPT,RRWTOT                                                    
         BZ    DISREC30                                                         
         MVC   ROWTOTL,APYES                                                    
         TM    RRWOPT,RRWTOTSP                                                  
         BZ    *+8                                                              
         MVI   ROWTOTL,C'S'                                                     
         TM    RRWOPT2,RRWBTM                                                   
         BZ    DISREC30                                                         
         MVI   ROWTOTL,C'B'                                                     
*                                                                               
DISREC30 SR    RF,RF                                                            
         IC    RF,RRWDATLN         LENGTH OF RRWNDATA                           
         LA    RE,RRWNDATA(RF)     END OF DATA, OR START OF PREFIX              
         ICM   RF,1,RRWPFXLN       LENGTH OF PREFIX                             
         BZ    DISREC35            NO PREFIX ATTACHED                           
         CHI   RF,L'ROWPRFX        WILL IT FIT ?                                
         BNH   *+6                 YES                                          
         DC    H'00'               TOO BIG FOR FIELD                            
         BCTR  RF,0                                                             
         EXMVC RF,ROWPRFX,0(RE)                                                 
*                                                                               
DISREC35 TM    RRWOPT2,RRWPGNO                                                  
         BZ    DISREC40                                                         
         MVI   ROWNUM,C'*'         MARK ROW WITH RE-PAGE                        
         CVD   R3,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  ROWRPG,APDUB        TURN TO CHARACTER NUMBER                     
*                                                                               
DISREC40 GOTO1 NEXTEL              R1 = A(ELEMENT FOUND)                        
         BNE   DISREC95                                                         
         AHI   R4,ROWLNQ           NEXT ROWFLD ENTRY                            
         AHI   R3,1                INCREMENT ROW COUNTER                        
         LA    RF,ROWTABH                                                       
         CR    R4,RF               ARE WE AT END OF ROW SCREEN                  
         BL    DISREC20                                                         
*                                                                               
DISREC95 CLI   APACTN,ACTCPY                                                    
         BE    DISREC99                                                         
         MVC   APCURSOR,ACURSOR    RESET  CURSOR                                
*                                                                               
DISREC99 CLI   APMODE,APMDELR                                                   
         BE    IVALRDEL                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R1,R4                                                            
         EJECT ,                                                                
         USING DEFTABD,R2                                                       
         USING RRWELD,R9                                                        
VALRUF   DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,NPARMS                                                        
*                                                                               
VALRUF10 CLI   0(R3),2             LENGTH OF PARAMETER                          
         BH    IVALUFLD                                                         
         LA    R3,32(,R3)          BUMP TO NEXT BLOCK PARAMETER                 
         BCT   RF,VALRUF10                                                      
         BR    RE                                                               
         DROP  R2,R9                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO EXPAND PARAMETER                                        *         
*        NTRY - R1=A(PARAMETER ENTRY)                                 *         
*        EXIT - DICPARM CONTAINS EXPANDED PARAMETER                   *         
***********************************************************************         
         SPACE 1                                                                
         USING PARMD,R1                                                         
         SPACE 1                                                                
EXPPARM  MVC   DICPARM,PARMCDE                                                  
         CLI   DICPARM,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
EXPPARMN STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,SCPARM,SCDICONE,DICPARM,0                                
*                                                                               
EXPPARMX LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  ERRORS TO DISPLAY ON TOP OF SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
IVALKEYW MVC   FVMSGNO,=AL2(FVFKYWD)         KEYWORD NOT RECOGNIZED             
         MVC   FVXTRA(L'APKEYWRD),APKEYWRD                                      
         LTR   R1,R1                                                            
         BZ    IVALEXIT                                                         
         MVC   FVMSGNO,=AL2(ACEKYWDI)        KEYWORD IS NOT VALID               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALMIX  MVC   FVMSGNO,=AL2(ACECMXE)  CANNOT MIX OLD WITH NEW ESTIMATE          
*&&US                                                                           
         L     R0,FULL                KEYWORDS                                  
         ST    R0,FVADDR                                                        
*&&                                                                             
         B     IVALEXIT                                                         
         SPACE 1                                                                
*&&US                                                                           
IVALNOCA MVC   FVMSGNO,=AL2(ACEFMICA) FORMAT MUST INCLUDE CONTRA                
         L     R0,FULL                A/C WITH THIS KEYWORD                     
         ST    R0,FVADDR                                                        
         B     IVALEXIT                                                         
*&&                                                                             
         SPACE 1                                                                
IVALADR  MVC   FVMSGNO,=AL2(ACEPRM)                                             
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)         INVALID INPUT                      
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEKEY MVC   FVMSGNO,=AL2(FVFEKEY)         ENTER KEY                          
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALLOW  MVC   FVMSGNO,=AL2(ACE2FEW)         TOO FEW PARAMETERS                 
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALHIGH MVC   FVMSGNO,=AL2(ACE2MANY)        TOO MANY PARAMETERS                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALTYPE MVC   FVMSGNO,=AL2(ACEIVTY)         INVALID TYPE                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALTOT  MVC   FVMSGNO,=AL2(ACEIVTO)         INVALID TOTAL OPTION               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALHEAD MVC   FVMSGNO,=AL2(ACEIVHD)         INVALID HEADING DEFINITION         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALUFLD MVC   FVMSGNO,=AL2(ACEIVUF)         INVALID USER FIELD                 
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALPARM MVC   FVMSGNO,=AL2(ACEPRM)          INVALID PARAMETERS                 
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALPFK  MVC   FVMSGNO,=AL2(ACEPFK)          INVALID PF KEY                     
         MVC   FVADDR,AACTHDR                ->   ACTION HEADER                 
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALINS  MVC   FVMSGNO,=AL2(ACEIVINS)        CAN'T INSERT                       
         MVC   FVADDR,ACURSOR                ->   CURSOR LOCATION               
         B     IVALEXIT                                                         
         SPACE 1                                                                
*                                            DELETE OF A RANKING FIELD          
IVALRRNK MVC   FVMSGNO,=AL2(ACEDRNKF)        INVALID - FIX PROFILE DATA         
         LA    R1,ROWFRSTH                   ->   1ST ROW                       
         ZIC   R2,RANKON+1                   ROW NUMBER                         
         BCTR  R2,0                                                             
         MH    R2,=Y(ROWLNQ)                 ROW OFFSET                         
         AR    R1,R2                         ROW IN ERROR                       
         LA    R1,ROWDATAH-ROWD(,R1)         GET TO DATA FIELD                  
         ST    R1,FVADDR                     ->  CURSOR LOCATION                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRDEL MVC   FVMSGNO,=AL2(ACIRWDEL)        ROWS DELETED                       
         B     IVALXINF                                                         
         SPACE 1                                                                
IVALRCPY MVC   FVMSGNO,=AL2(ACIRWCPY)        ROWS COPIED                        
         B     IVALXINF                                                         
         SPACE 2                                                                
IVALXINF MVI   FVOMTYP,GTMINF                CHANGE TO INFO TYPE                
         B     EXIT                                                             
         SPACE 1                                                                
IVALEXIT B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  TABLES                                                             *         
***********************************************************************         
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(ROW008H-TWAD,5665)     (Y/S/N)                               
         DC    AL4(ROW020H-TWAD,5664)     START NEW PAGE NUM. ON ROW            
         DC    X'FF'                                                            
         EJECT ,                                                                
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
         MVC   SVELEM,APELEM       SAVE APELEM                                  
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
         B     FIX5TH30            Are there others ?                           
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
FIX5TH90 MVC   APELEM,SVELEM       RESTORE APELEM                               
         XMOD1 ,                   RETURN                                       
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
*=====================================================================*         
*        ADJUST XTRELD BASED ON ROW DELETED                           *         
*=====================================================================*         
                                                                                
FIXXTREL NMOD1 0,**XTRL**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE   APELEM                                
         L     R3,0(,R1)           ROW/COL # TO START FROM                      
         STC   R3,APBYTE           ROW NUMBER WORKING ON                        
         L     R2,4(,R1)           +1 OR -1 FOR INSERT OR DELETE                
*                                                                               
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,XTRELQ     EXTRA DATA ELEMENTS      X'C6'               
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
*                                                                               
MATCHED  SR    RE,RE               SET TO INDICATE     MATCHED                  
MATCHNO  LTR   RE,RE               SET TO INDICATE NOT MATCHED                  
         XIT1                                                                   
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
*    CUROW#   - OLD     ROW     SEQUENCE NUMBER                       *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    RANKON   - PROFILE RANK    ON     FIELD                          *         
*                                                                     *         
*  USES:                                                              *         
*    APELCODE - MESSAGE ELEMENT                                       *         
*    SVELEM   - SAVE    AREA    FOR    APELEM                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R1           PROFILE DATA ELEMENT                         
         USING LWSD,RC                                                          
         SPACE 1                                                                
FIXRPROF NMOD1 0,**RPRF**                                                       
         L     RC,APALOCAL                                                      
         MVC   SVELEM,APELEM       SAVE    APELEM                               
         L     R2,0(,R1)           +1      OR   -1   FOR  UPDATE                
         SR    R6,R6               CLEAR   REGISTER                             
*                                                                               
         L     R1,AIOAREA1         ->      RECORD                               
         MVI   APELCODE,RPFELQ     GET     PROFILE   ELEMENT                    
         GOTO1 GETEL                                                            
         BNE   FIXRPRFX            NONE,   EXIT                                 
         OC    RPFRKON,RPFRKON     ANY     RANK ON   ROW ?                      
         BZ    FIXR20              NO,     SKIP RANKING   TESTS                 
         CLI   RPFRKON,C'R'        RANK    ON   ROW  NUMBER ?                   
         BNE   FIXR20              NO,     SKIP                                 
         CLC   CUROW#,ORANKON+1    DO      WE   NEED AN   UPDATE?               
         BH    FIXR20              HIGH,   SKIP UPDATING                        
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
FIXRPRFX DS    0H                                                               
         MVC   APELEM,SVELEM       RESTORE APELEM                               
         XMOD1 ,                                                                
         DROP  R1,RC                                                            
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
*&&US                                                                           
FULL     DS    A                                                                
*&&                                                                             
SVREGS   DS    6A                                                               
DEFENTRY DS    A                   POINTS TO DEFINITION ENTRY                   
SAV@RHLP DS    A                   SAVE   CUR@RHLP                              
BLOCK    DS    6CL32               DATA   BLOCK FOR SCANNER                     
HAVE5TH  DS    CL1                 HAVE RESKS5TH RECORD TYPE (Y/N)              
NROWSEQ  DS    XL1                 NEW    ROW NUMBER                            
CUROW#   DS    XL1                 CURRENT ROW WORKING ON                       
DELROW#  DS    XL1                 Row number to delete                         
ROW#CSR  DS    XL1                 ROW WHERE CURSOR WAS FOUND                   
NCOLS    DS    XL1                 NUMBER OF COLUMNS                            
MIDLNFG  DS    XL1                 FLAG   IF MIDLINE ENCONTERED                 
PARM_N   DS    CL1                 CURRENT PARAMETER WORKING ON                 
PASTESEQ DS    XL1                 PASTE  SEQUENCE NUMBER                       
SAVSTSEQ DS    XL1                 SAVE   ROW START SEQUENCE NUMBER             
RANKON   DS    CL2                 RANK   ON                                    
ORANKON  DS    CL2                 RANK   ON  (OLD)                             
MIXESTK  DS    XL1                 MIXED ESTIMATE KEYWORDS INDICATOR            
*                                                                               
DWNTYPE  DS    XL1                 DOWNLOAD SCREEN INDICATOR                    
DWNTACNT EQU   X'80'               ACCENT ENABLED                               
DWNTQREP EQU   X'40'               QUICK REPORT ENABLED                         
*                                                                               
SVELEM   DS    CL(L'APELEM)        SAVE   APELEM                                
LWSX     DS    0C                                                               
         EJECT ,                                                                
ROWD     DSECT                                                                  
ROWNUMH  DS    CL8                 HEADER FOR NUMBER                            
ROWNUM   DS    CL3                 ROW NUMBER                                   
ROWDATAH DS    CL8                 HEADER FOR DATA                              
ROWDATA  DS    CL12                DATA                                         
ROWTYPEH DS    CL8                 HEADER FOR ROW HEADING TYPE                  
ROWTYPE  DS    CL1                 RIGHT/LEFT/CENTER HEADING                    
ROWTOTLH DS    CL8                 HEADER FOR TOTAL                             
ROWTOTL  DS    CL1                 TOTAL Y/S/N                                  
ROWPRFXH DS    CL8                 HEADER FOR PREFIX                            
ROWPRFX  DS    CL14                PREFIX                                       
ROWLNQ   EQU   *-ROWD                                                           
         EJECT ,                                                                
*ACSCRWRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR HEADLINE DEFINITIONS                                     *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRFCD                                                       
         ORG   ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099ACSCR03   04/11/19'                                      
         END                                                                    
