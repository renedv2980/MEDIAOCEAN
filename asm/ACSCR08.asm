*          DATA SET ACSCR08    AT LEVEL 031 AS OF 09/02/15                      
*PHASE T60C08A,+0                                                               
*&&ONLIN SET   Y                                                                
*INCLUDE XSORT                                                                  
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 30 AS OF 09/26/14         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* YNGX 030 26SEP14 PCA01185 NEW KEYWORD TYMAP(C#)                               
*                                                                               
         TITLE 'DISPLAY HELP FOR ROW OR COLUMN OR HEADING'                      
T60C08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C08,RA,RR=RE,CLEAR=YES                                       
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        INSURE     CURSOR   WAS  IN  A FIELD         
         BE    SCR01               NO,  SET   CURSOR                            
         CLI   TWALACT,ACTHLP      IF   FIRST TIME IN, THEN SET CURSOR          
         BNE   SCR01               YES, SET   CURSOR                            
         CLI   TWASCRN,SCRHLP      IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,HLPKYWDH         USER KEYWORD    FIELD                        
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         MVC   SVPFKEY,APPFKEY     SAVE PF    KEY                               
         L     RF,=V(XSORT)                                                     
         A     RF,APRELO                                                        
         ST    RF,AXSORT                                                        
*                                                                               
         CLI   REPMODE,0                                                        
         BZ    *+10                                                             
         MVC   SVRPMODE,REPMODE    SAVE IT   BEFORE    HOOK CLEARS IT           
         MVC   REPMODE,SVRPMODE                                                 
*                                                                               
         MVC   ERRORMSG,SPACES                                                  
         XC    KWDPASTE,KWDPASTE   CLEAR     DATA TO   BE   PASTED              
         EJECT ,                                                                
***********************************************************************         
*  CHECK PFKEYS FOR SCROLLING                                         *         
***********************************************************************         
         SPACE 1                                                                
         CLI   APMODE,APMVALK                                                   
         BNE   SCR25                                                            
         GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   SCR03                                                            
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     EXIT                                                             
*                                                                               
SCR03    MVC   ASORTWRK,0(R1)                                                   
         MVI   ASORTWRK,0                                                       
*                                                                               
         TM    SCRSRLH+4,FVITHIS   INPUTED THIS TIME?                           
         BNZ   SCR04               YES,SO VALIDATE IT                           
         MVC   SCRSRL,SRLHLP       RESET SCROLL # AS BEFORE                     
*                                                                               
SCR04    GOTO1 AFVAL,SCRSRLH                                                    
         CLI   FVILEN,0            ANY  SCROLL DATA ENTERED ?                   
         BNE   SCR04A              YES, SKIP                                    
         MVC   SCRSRL,AC@PAGE      NO,  ASSUME PAGE                             
         MVI   FVILEN,4            RESET       DATA LENGTH                      
         MVI   FVXLEN,3            RESET       EXMVC     LENGTH                 
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
*                                                                               
SCR04A   DS    0H                                                               
         ZIC   RF,FVXLEN           GET  EXECUTE     LENGTH                      
*                                  SET  TO     SCROLL    BY   HALF PAGE         
         MVC   SCROLL,=Y(MAXDISP/2)                                             
         EX    RF,*+8              COMPARE     FOR  INPUT     LENGTH            
         B     *+10                                                             
         CLC   SCRSRL(0),AC@HALF   ENTERED     "HALF"    INTO SCROLL ?          
         BNE   SCR04B              NO,  SKIP                                    
         MVC   SCRSRL,AC@HALF      YES, ALWAYS DISPLAY   "HALF"                 
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         B     SCR05               YES, SAVE   SCROLL                           
*                                                                               
SCR04B   DS    0H                                                               
         MVC   SCROLL,=Y(MAXDISP)  SET  TO     SCROLL    BY   PAGE              
         EX    RF,*+8              COMPARE     FOR  INPUT     LENGTH            
         B     *+10                                                             
         CLC   SCRSRL(0),AC@PAGE   ENTERED     PAGE INTO SCROLL ?               
         BNE   SCR04C              NO,  SKIP                                    
         MVC   SCRSRL,AC@PAGE      YES, ALWAYS DISPLAY   "PAGE"                 
         OI    SCRSRLH+6,FVOXMT    TRANSMIT                                     
         B     SCR05               YES, SAVE   SCROLL                           
*                                                                               
SCR04C   DS    0H                                                               
         SR    R3,R3                                                            
         IC    R3,FVILEN           LENGTH      OF   INPUT     MINUS ONE         
         GOTO1 VCASHVAL,APPARM,(C'N',SCRSRL),(R3)                               
         CLI   APPARM,0            VALID       INPUT ?                          
         BNE   IVALIPUT            NO,  INVALID     INPUT                       
         MVC   SCROLL,APPARM+6     YES, SAVE   BINARY    VALUE                  
*                                                                               
SCR05    MVC   SRLHLP,SCRSRL       SAVE SCROLL INPUT     FROM SCREEN            
*                                                                               
SCR20    DS    0H                                                               
         EJECT ,                                                                
***********************************************************************         
*  MAKE SURE THAT ACURSOR IS PROPERLY ADJUSTED                        *         
***********************************************************************         
         SPACE 1                                                                
SCR25    L     R2,ACURSOR          GET  ACURSOR                                 
         LA    RE,HLPKYWDH         ->   USER KEYWORD   FIELD                    
         LA    RE,L'HLPKYWDH+L'HLPKYWD(,RE)  ->   END  OF   DATA AREA           
         CR    R2,RE               A(CURSOR) <    END  OF   DATA AREA ?         
         BL    SCR50               YES, SKIP                                    
*                                                                               
         LA    RE,HLPTABH          ->   PAST LAST KEYWORD                       
         CR    R2,RE               A(CURSOR) >    END  OF   KEYWORDS ?          
         BNL   SCR40               YES, RESET     ACURSOR                       
*                                                                               
         LA    RE,HLPKYW1H         ->   1ST  KEYWORD                            
         CR    R2,RE               A(CURSOR) <    1ST  KEYWORD ?                
         BL    SCR40               YES, RESET     ACURSOR                       
*                                                                               
         USING HELPD,RE            KEYWORD   LINE DSECT                         
*                                                                               
         LA    R1,MAXSCRLL         NUM  OF   KEYWORD   LINES                    
         LA    RF,HELPLNQ-1(,RE)   LAST BYTE OF   LINE                          
*                                                                               
SCR30    CR    R2,RF               A(CURSOR) >    END  OF   LINE ?              
         BH    SCR35               YES, TRY  NEXT LINE                          
         ST    RE,ACURSOR          ->   HEADER    FOR  KEYWORD                  
         B     SCR50               CURSER    IS   OKAY                          
*                                                                               
SCR35    LA    RE,HELPLNQ(,RE)     ->   NEXT KEYWORD   HEADER                   
         LA    RF,HELPLNQ(,RF)     ->   NEXT KEYWORD   TEXT END                 
         BCT   R1,SCR30            TEST NEXT KEYWORD                            
         DC    H'00'               SHOULD    NEVER     OCCUR                    
*                                                                               
*                                  RESET     CURSOR                             
SCR40    LA    RE,HLPKYWDH         ->   USER KEYWORD   FIELD                    
         ST    RE,ACURSOR          RESET     CURSOR                             
*                                                                               
         DROP  RE                  KEEP IT   CLEAN                              
         EJECT ,                                                                
         SPACE 1                                                                
SCR50    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALIDATE KEY                                 
         B     EXIT                VALIDATE RECORD                              
         B     DISKEY              DISPLAY KEY                                  
         B     DISREC              DISPLAY RECORD                               
         B     EXIT                DELETE RECORD                                
         B     EXIT                RESTORE RECORD                               
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
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
EXIT     CLI   APMODE,APMDISR      DISPLAY MODE ONLY                            
         BNE   EXIT95                                                           
         CLI   APPFKEY,PFKEXIT     RETURN IN NON-SELECT MODE?                   
         BNE   EXIT95                                                           
*                                                                               
         MVI   APPFKEY,0                    RE-SET PFKEY                        
         XC    APCURSOR,APCURSOR                                                
         XC    ACURSOR,ACURSOR                                                  
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE                                               
         MVC   APPARM+1(1),TWASWPAC         RE-INSTATE LAST ACTION              
*                                                                               
EXIT95   OI    TWALSCTL,TWALSRTN+TWALSHLD                                       
         CLI   APMODE,APMDISR      MODE IS   DISPLAY RECORD ?                   
         BNE   EXIT99              NO,  EXIT                                    
         OI    APINDS2,APIOVROK    SET  OVERLAY IS   HAPPY                      
*                                                                               
         CLI   SVPFKEY,0           ANY  PF   KEY  DEPRESSED ?                   
         BNE   EXIT99              YES, SKIP                                    
         MVC   APCURSOR,ACURSOR    NO,  SET  APPLICATION CURSOR                 
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
         USING RESRECD,R2                                                       
VALKEY   MVI   APINDS,APIOKDIS                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   SAVEUL,APREPUL                                                   
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         CLC   SAVFORM,SPACES      ANY FORMAT?                                  
         BE    VALKEY05                                                         
         MVC   RESKFORM,SAVFORM                                                 
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
         GOTO1 AIO,IORD+IOACCFIL+IO1                                            
         BNE   VALKEY05                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         BE    VALKEY40            IF  NOT  FROM FORMAT THEN                    
*                                  GET TYPE SET  FROM SCREEN                    
         USING REPTABD,R3                                                       
VALKEY05 L     R3,ACTYPTAB                                                      
*                                                                               
VALKEY10 CLI   REPCODE,EOT         END OF TABLE?                                
         BE    VALKEY25            YES, FINISHED                                
         LR    R1,R3                                                            
         BAS   RE,EXPREPTY                                                      
         CLC   APREPCDE,REPCDE                                                  
         BNE   VALKEY20                                                         
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALKEY15            NO SO OK                                     
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BZ    VALKEY20            NO, SO SKIP                                  
*                                                                               
VALKEY15 OC    APREPIND,REPIND     ALL ARE VALID FOR TYPE                       
*                                                                               
VALKEY20 LA    R3,REPLNQ(,R3)                                                   
         B     VALKEY10                                                         
         DROP  R3                  KEEP IT   CLEAN                              
*                                                                               
VALKEY25 MVC   FVMSGNO,=AL2(FVFOK) RESET MSG                                    
         MVC   APREPUL,SAVEUL      USE DEFAULT LEDGER                           
*                                                                               
VALKEY40 BAS   RE,SORTKEYS         SORT KEYWORDS                                
         LH    R0,SCROLL           GET SCROLL AMOUNT                            
         L     R8,ASORTWRK         GET BEGINING OF SORT BLOCK                   
         A     R8,ACURDEF          ADD IN DISPLACEMENT                          
*                                                                               
         SR    R2,R2                                                            
         LR    R4,R8               INITIALIZE R4                                
*                                                                               
         CLI   APPFKEY,PFKDOWN     SCROLL DOWN?                                 
         BNE   VALKEY45            NO                                           
         MVI   SVPFKEY,0           RE-SET PFKEY                                 
         MVI   APPFKEY,0           RE-SET PFKEY                                 
         MVI   LISTLOC,C' '        INITIALIZE TOP/BOTTOM FLAG                   
         CLM   R0,1,SCR#KYWD       SCROLL VALUE < KEYWORDS ON SCREEN ?          
         BNH   *+8                 NO,  SKIP                                    
         IC    R0,SCR#KYWD         DOWN ONLY NUM  KEYWORDS ON SCREEN            
         MH    R0,=Y(KYWLNQ)       SCROLL * 10                                  
         AR    R4,R0                                                            
         C     R4,ASORTEND         ENDING SOMEWHERE?                            
         BNL   IVALETB0                                                         
*        LR    RF,R4                                                            
*        AR    RF,R0                                                            
*        C     RF,ASORTEND                                                      
*        BL    *+8                                                              
*        MVI   LISTLOC,C'E'                                                     
         L     RF,ASORTWRK                                                      
         SR    R4,RF                                                            
         ST    R4,ACURDEF                                                       
         B     VALKEY55                                                         
*                                                                               
VALKEY45 CLI   APPFKEY,PFKUP       SCROLL UP?                                   
         BNE   VALKEY60            NEITHER UP NOR DOWN WERE PRESSED             
         MVI   SVPFKEY,0           RE-SET PFKEY                                 
         MVI   APPFKEY,0           RE-SET PFKEY                                 
         MH    R0,=Y(KYWLNQ)                                                    
         SR    R4,R0                                                            
         L     RF,ASORTWRK                                                      
         CR    R4,RF                                                            
         BH    VALKEY50                                                         
         MVI   LISTLOC,C'T'        TOP OF LIST                                  
         LR    R4,RF               POINT TO TOP                                 
*                                                                               
VALKEY50 SR    R4,RF                                                            
         ST    R4,ACURDEF                                                       
*                                                                               
VALKEY55 MVC   HLPKYWD,SPACES      ERASE ANY FILTERING ON KEYWORD               
         OI    HLPKYWDH+6,FVOXMT   TRANSMIT SPACES                              
*                                                                               
VALKEY60 CLI   APPFKEY,PFKPASTE    PASTE AND EXIT ?                             
         BNE   VALKEY70            NO,  SKIP                                    
         NC    CUR@RHLP,CUR@RHLP   ANY  PLACE     TO   PASTE     DATA ?         
         BZ    IVALPAST            NO,  CAN  NOT  PASTE                         
*                                                                               
VALKEY70 B     EXIT                RETURN                                       
         EJECT ,                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY RECORD NAME AND OWNER                           *         
***********************************************************************         
         SPACE 1                                                                
         USING HELPD,R6                                                         
         USING DEFTABD,R3                                                       
DISREC   MVI   COLNUM,COL1         INITIALIZE TO FIRST COLUMN                   
         MVI   SCR#KYWD,0          INITIALIZE NUM OF KEYWORDS ON SCREEN         
         TWAXC HLPKYW1H,HLPTABH,PROT=Y                                          
         L     R8,ASORTWRK         SORT AREA OF KEYWORDS                        
         A     R8,ACURDEF                                                       
*                                                                               
         USING KYWSORTD,R8                                                      
         LA    R6,HLPKYW1H                                                      
         GOTO1 AFVAL,HLPKYWDH      FILTER ON KEYWORD FIELD                      
         BNE   DISREC10                                                         
         L     R8,ASORTWRK                                                      
         LH    R3,SORTNUM          # OF SORT RECORDS                            
         SR    R2,R2                                                            
         IC    R2,FVXLEN           LENGTH - 1                                   
*                                                                               
DISREC05 EXCLC R2,KYWCODE,FVIFLD                                                
         BNL   DISREC06            UP TO NEXT HIGHEST                           
         LA    R8,KYWLNQ(,R8)      BUMP TO NEXT ONE                             
         BCT   R3,DISREC05                                                      
*                                                                               
         MVI   LISTLOC,C'E'        CAN'T FIND IT                                
         SHI   R8,KYWLNQ           JUST SHOW LAST ONE                           
*                                                                               
DISREC06 LR    RF,R8                                                            
         L     RE,ASORTWRK                                                      
         SR    RF,RE                                                            
         ST    RF,ACURDEF                                                       
*                                                                               
DISREC10 ICM   R3,15,KYWDISP                                                    
         A     R3,ACDEFTAB                                                      
         CLI   DEFLEN,0            END OF TABLE?                                
         BE    IVALETB             YES                                          
*                                                                               
         BAS   RE,EXKEYWD                                                       
         CLI   DEFSPCL,DEFEXRT     EXRT(C#) KEYWORD?                            
         BE    *+8                                                              
         CLI   DEFSPCL,DEFSUM      CUME(C#) KEYWORD?                            
*&&UK*&& BE    *+8                                                              
*&&UK*&& CLI   DEFSPCL,DEFMAP      TYMAP(C#) KEYWORD?                           
         BNE   DISREC11                                                         
         LA    R1,CURKEYWD                                                      
         CLI   0(R1),C' '          FIND FIRST SPACE                             
         BE    *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVC   0(1,R1),APOPNPRN                                                 
         MVC   1(1,R1),APCLSPRN                                                 
*                                                                               
         USING RCLELD,APELEM                                                    
DISREC11 LR    R1,R3                                                            
         ICM   R1,8,=CL1'E'        VALIDATE FOR ENTRY IN TABLE                  
         TM    INOPT1,INOTYPE                                                   
         BO    DISRC11A            DON'T VALIDATE TYPE OF KEYWORD               
         TM    INOPT2,INOEXP2                                                   
         BO    DISRC11A                                                         
         GOTO1 VALDEF                                                           
         BE    DISRC11A            LOOP                                         
         TM    INOPT2,INOEXPD                                                   
         BZ    DISREC70            LOOP                                         
*                                                                               
DISRC11A MVC   CHOPWRK,SPACES                                                   
         CLI   REPMODE,REPCOL                                                   
         BNE   DISREC13            COLUMNS ONLY                                 
         TM    INOPT2,INOEXPD                                                   
         BZ    DISREC13                                                         
         XC    APELEM,APELEM                                                    
         GOTO1 MKHEAD,APPARM,APELEM                                             
         SR    RE,RE                                                            
         IC    RE,RCLDATLN         LENGTH OF RCLNDATA                           
         LA    RE,RCLNDATA(RE)     POINT TO END OF RCLNDATA                     
         SR    RF,RF                                                            
         ICM   RF,1,RCLHD1LN       LENGTH OF HEADING 1                          
         BZ    DISRC11C                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   CHOPWRK(0),0(RE)                                                 
         LA    RE,1(RF,RE)         POINT TO END OF HEADING 1                    
*                                                                               
DISRC11C ICM   RF,1,RCLHD2LN                                                    
         BZ    DISREC12                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   CHOPWRK+13(0),0(RE)                                              
*                                                                               
DISREC12 SR    R1,R1               DISPLAY WIDTH                                
         IC    R1,DEFWDTH                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CHOPWRK+26(2),APDUB                                              
         LA    R1,1                ONE LINE USED                                
         STCM  R1,15,APPARM+8      NUMBER OF LINES USED                         
         B     DISREC28                                                         
*                                                                               
DISREC13 TM    INOPT2,INOEXP2      EXPAND TYPE 2                                
         BZ    DISRC13Z                                                         
         LA    RE,CHOPWRK                                                       
         TM    DEFTYPE,DEFPACK                                                  
         BZ    *+8                                                              
         MVI   0(RE),C'$'                                                       
         TM    DEFTYPE,DEFRATE                                                  
         BZ    *+8                                                              
         MVI   0(RE),C'R'                                                       
         TM    DEFTYPE,DEFCHAR                                                  
         BZ    *+8                                                              
         MVI   0(RE),C'C'                                                       
         TM    DEFTYPE,DEFDTE1                                                  
         BZ    *+8                                                              
         MVI   0(RE),C'D'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFTRDT    TRANSACTION DATE TYPE                        
         BZ    *+8                                                              
         MVI   0(RE),C'T'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFBLDT    BILLED      DATE TYPE                        
         BZ    *+8                                                              
         MVI   0(RE),C'B'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFBMOA    BILLED     MOA                               
         BZ    *+8                                                              
         MVI   0(RE),C'C'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFSTDT    STATEMENT   DATE TYPE                        
         BZ    *+8                                                              
         MVI   0(RE),C'S'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFATDT    ACTIVITY    DATE TYPE                        
         BZ    *+8                                                              
         MVI   0(RE),C'A'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFDUDT    DUE         DATE TYPE                        
         BZ    *+8                                                              
         MVI   0(RE),C'D'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFDATES,DEFMODT    MOA         DATE TYPE                        
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFCLIND,DEFCLCDE                                                
         BZ    *+8                                                              
         MVI   0(RE),C'C'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFCLIND,DEFCLNME                                                
         BZ    *+8                                                              
         MVI   0(RE),C'N'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFCLIND,DEFCLLVL                                                
         BZ    *+8                                                              
         MVI   0(RE),C'L'                                                       
         TM    DEFCLIND,DEFCLJBR                                                
         BZ    *+8                                                              
         MVI   0(RE),C'J'                                                       
         TM    DEFTYPE,DEFMTHD                                                  
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
         LA    RE,1(,RE)                                                        
*                                                                               
         L     R1,=A(REPTTAB)                                                   
         A     R1,APRELO                                                        
DISRC13C CLI   0(R1),0                                                          
         BE    DISRC13E                                                         
         MVC   APFULL,DEFREPS                                                   
         NC    APFULL,1(R1)                                                     
         BZ    *+10                                                             
         MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(,RE)                                                        
         LA    R1,5(,R1)                                                        
         B     DISRC13C                                                         
*                                                                               
DISRC13E LA    RE,1(,RE)                                                        
         TM    DEFIND,DEFHEAD                                                   
         BZ    *+8                                                              
         MVI   0(RE),C'H'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFIND,DEFROW                                                    
         BZ    *+8                                                              
         MVI   0(RE),C'R'                                                       
         LA    RE,1(,RE)                                                        
         TM    DEFIND,DEFCOL                                                    
         BZ    *+8                                                              
         MVI   0(RE),C'C'                                                       
         LA    RE,1(,RE)                                                        
         LA    R1,1                ONE LINE USED                                
         STCM  R1,15,APPARM+8      NUMBER OF LINES USED                         
         B     DISREC28                                                         
*                                                                               
DISRC13Z TM    DEFIND,DEFGTXT      GET TEXT HELP?                               
         BZ    DISREC14                                                         
*                                                                               
         USING GETTXTD,R2                                                       
         LA    R2,APPARM                                                        
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,DEFHELP#                                                 
         MVI   GTMTYP,GTMSCR                                                    
         MVC   GTMSYS,ASSYSO                                                    
         MVI   GTMAXL,L'MSGAREA                                                 
         LA    R4,MSGAREA                                                       
         STCM  R4,7,GTAOUT                                                      
         OI    GT1INDS,GT1OWRK+GT1NOREF                                         
         GOTO1 VGETTXT,GETTXTD                                                  
         SR    R1,R1                                                            
         IC    R1,GTMAXL                                                        
         TM    GT1INDS,GT1NOMSG    NO MESSAGE FOUND?                            
         BZ    *+14                                                             
         MVC   MSGAREA(30),=CL30'*** MESSAGE NOT DEFINED ***'                   
         LA    R1,30                                                            
         LR    R2,R1                                                            
         B     DISREC25                                                         
*                                                                               
DISREC14 SR    R2,R2                                                            
         ICM   R2,1,DEFLEN                  LENGTH OF KEYWORD ENTRY             
         BZ    DISREC80                                                         
         CLI   DEFDSC,0                                                         
         BE    DISREC15                                                         
         SHI   R2,(DEFDSC-DEFTABD)        LENGTH OF DISCRIPTION                 
         BP    *+10                                                             
         BZ    DISREC70                                                         
         DC    H'0'                                                             
         LA    R4,DEFDSC                                                        
         B     DISREC25                                                         
*                                                                               
DISREC15 LA    R2,L'APKEYHD                                                     
         LA    R4,APKEYHD                                                       
*                                                                               
DISREC25 GOTO1 VCHOPPER,APPARM,((R2),(R4)),(L'HELPDSC,CHOPWRK),        X        
               (=AL1(L'HELPDSC),5)                                              
*                                                                               
DISREC28 SR    R1,R1                                                            
         ICM   R1,15,APPARM+8      NUMBER OF LINES USED                         
         BZ    *+6                 NONE, JUST OUTPUT THE KEYWORD                
         BCTR  R1,0                                                             
         MH    R1,=Y(2*HELPLNQ)                                                 
         AR    R1,R6                                                            
         LA    R0,HLPTABH                                                       
         CR    R1,R0               AT END OF COLUMN SCREEN                      
         BL    DISREC30            NO                                           
         CLI   COLNUM,COL2         YES, ARE WE IN COLUMN 1 OR 2                 
         BE    DISREC80            COL 2 SO EXIT                                
         MVI   COLNUM,COL2         SWITCH TO COLUMN 2                           
         LA    R6,HLPKYW2H         POINT TO SECOND COLUMN                       
*                                                                               
DISREC30 LA    R1,HELPKY                                                        
         CLI   INREC,RECHEAD                                                    
         BE    *+8                                                              
         CLI   REPMODE,REPHEAD     HEADING MODE                                 
         BNE   DISREC32                                                         
         LA    R1,HELPKY+1                                                      
         MVI   HELPKY,C'&&'                                                     
*                                                                               
DISREC32 MVC   0(KEYWDLN,R1),CURKEYWD                                           
         MVC   HELPKY+L'HELPKY-1(1),SCEQUAL                                     
         OI    HELPKYH+6,FVOXMT    TRANSMIT                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,15,APPARM+8      NUMBER OF LINES USED                         
         BZ    DISREC50            NONE, SKIP DESCRIPTION                       
         LA    R4,CHOPWRK                                                       
*                                                                               
DISREC45 MVC   HELPDSC,0(R4)       OUTPUT ONE DESCRIPTION LINE                  
         OI    HELPDSCH+6,FVOXMT   TRANSMIT                                     
         LA    R4,L'HELPDSC(,R4)   BUMP UP IN TO NEXT DISCRIPTION               
         LA    R6,HELPLNQ*2(,R6)   BUMP UP TO NEXT LINE                         
         BCT   R1,DISREC45                                                      
         B     DISREC60                                                         
*                                                                               
DISREC50 MVC   HELPDSC,SPACES      CLEAR DESCRIPTION                            
         OI    HELPDSCH+6,FVOXMT   TRANSMIT                                     
         LA    R6,HELPLNQ*2(,R6)   BUMP UP TO NEXT LINE                         
*                                                                               
DISREC60 IC    R1,SCR#KYWD         ADD  1  TO NUM OF KEYWORDS ON SCREEN         
         LA    R1,1(,R1)                                                        
         STC   R1,SCR#KYWD                                                      
*                                                                               
         LA    R0,HLPTABH                                                       
         CR    R6,R0                                                            
         BL    DISREC70                                                         
         CLI   COLNUM,COL2         YES, ARE WE IN COLUMN 1 OR 2                 
         BE    DISREC80            2 SO EXIT                                    
         MVI   COLNUM,COL2         SWITCH TO COLUMN 2                           
         LA    R6,HLPKYW2H         POINT TO SECOND COLUMN                       
*                                                                               
DISREC70 SR    R1,R1                                                            
         LA    R8,KYWLNQ(,R8)                                                   
         OC    KYWCODE,KYWCODE                                                  
         BNZ   DISREC10                                                         
         MVI   LISTLOC,C'E'        END  OF   TABLE                              
*                                                                               
         DROP  R3                  KEEP IT   CLEAN                              
         EJECT ,                                                                
DISREC80 CLI   APPFKEY,PFKEXIT     RETURN    IN   NON-SELECT     MODE ?         
         BE    DISREC95            YES, EXIT                                    
*                                                                               
*                                  MAKE SURE THAT WE   ARE  POINTING            
*                                       AT   A    KEYWORD                       
         L     R6,ACURSOR          GET  ACURSOR                                 
         LA    RE,HLPKYW1H         ->   FIRST     KEYWORD                       
         LA    R1,HELPLNQ*2        SIZE OF   TWO  ENTRIES  (ONE  LINE)          
*                                                                               
         CR    R6,RE               CURSOR    BEFORE    1ST  KEYWORD ?           
         BL    DISREC88            YES, LEAVE     CURSOR    ALONE               
*                                                                               
DISREC82 NC    HELPKY,HELPKY       ANY  KEYWORD   TEXT ?                        
         BNZ   DISREC86            YES, SAVE CURSOR                             
         LR    RF,R6               SAVE CURSOR                                  
         SR    R6,R1               ->   PREVIOUS  LINE                          
         CR    R6,RE               BEFORE    1ST  KEYWORD ?                     
         BNL   DISREC82            NO,  TEST PREVIOUS LINE                      
         CR    RF,RE               WERE WE   AT   1ST KEYWORD ?                 
         BE    DISREC84            YES, REINITIALIZE  CURSOR                    
         LA    R6,HLPTABH          NO,  TRY  LAST KEYWORD   IN                  
         SR    R6,R1                    FIRST     COLUMN                        
         B     DISREC82            TEST THE  KEYWORD                            
*                                                                               
DISREC84 LA    R6,HLPKYWDH         ->   USER KEYWORD  FIELD                     
*                                                                               
DISREC86 ST    R6,ACURSOR          SAVE CURSOR                                  
*                                                                               
DISREC88 CLI   APPFKEY,PFKPASTE    PASTE     AND  RETURN                        
         BNE   DISREC90            NO,  SKIP                                    
         L     RE,ACURSOR          ->   CURSOR    LOCATION                      
         LA    RE,0(,RE)           CLEAR     HIGH ORDER     BYTE                
         LA    RF,HLPKYW1H         ->   1ST  KEYWORD                            
         CR    RE,RF               IN   KEYWORD   AREA ?                        
         BL    IVALNPST            NO,  NOTHING   TO   PASTE                    
         CLI   8(RE),X'40'         ANY  DATA ?                                  
         BNH   IVALNPST            NO,  NOTHING   TO   PASTE                    
         MVC   KWDPASTE,8(RE)      MOVE KEYWORD                                 
         CLI   REPMODE,REPHEAD     HEADING   MODE ?                             
         BNE   *+10                NO,  SKIP                                    
         MVC   KWDPASTE,9(RE)      MOVE KEYWORD   W/O  THE  "&"                 
         MVI   APPFKEY,PFKEXIT     MAKE PFK  KEY  EXIT                          
         B     DISREC95            EXIT                                         
*                                                                               
DISREC90 CLI   LISTLOC,C'T'        TOP  OF   TABLE?                             
         BE    IVALTTB                                                          
         CLI   LISTLOC,C'E'        END  OF   TABLE?                             
         BE    IVALETB                                                          
*                                                                               
DISREC95 B     EXIT                EXIT                                         
*                                                                               
         DROP  R6                  KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  R3 SET BEFORE CALLING TO DEFTAB ENTRY                              *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R3                                                       
         SPACE 1                                                                
EXKEYWD  MVC   CURKEYWD,DEFCODE                                                 
         CLI   CURKEYWD,ESCHIGHQ                                                
         BNLR  RE                                                               
         STM   RE,R1,SAVEREG                                                    
         GOTO1 VDICTAT,APPARM,C'SU  ',CURKEYWD,0                                
         CLI   DEFSPCL,DEFLVL1     ACCOUNT LEVEL TYPE KEYWORD ?                 
         BL    EXKEYWD2            NO                                           
         CLI   DEFSPCL,DEFLVL9     ACCOUNT LEVEL TYPE KEYWORD ?                 
         BH    EXKEYWD2            NO                                           
         LA    RE,CURKEYWD                                                      
         LA    RF,L'CURKEYWD                                                    
         CLI   0(RE),C'#'          FIND NUMBER SIGN                             
         BE    EXKEYWD1                                                         
         LA    RE,1(,RE)           NEXT CHARACTER                               
         BCT   RF,*-12                                                          
         DC    H'00'                                                            
*                                                                               
EXKEYWD1 MVC   0(1,RE),DEFSPCL     SUBSTITUTE LEVEL #                           
EXKEYWD2 LM    RE,R1,SAVEREG                                                    
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
         USING REPTABD,R1                                                       
EXPREPTY MVC   REPCDE,REPCODE                                                   
         CLI   REPCDE,ESCHIGHQ                                                  
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         ST    RE,SAVEREG                                                       
         GOTO1 VDICTAT,APPARM,C'SU  ',REPCDE,0                                  
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT ,                                                                
IVALTTB  MVC   FVMSGNO,=AL2(ACITOKL)          TOP OF   KEYWORD   LIST           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALETB0 MVC   APCURSOR,ACURSOR               SET CURSOR                        
*                                                                               
IVALETB  MVC   FVMSGNO,=AL2(ACIEOKL)          END OF   KEYWORD   LIST           
         MVI   SCR#KYWD,MAXDISP               SO  THAT NEXT PASS KEEPS          
*                                                 THIS SCREEN,   SAY            
*                                                 FULL SCREEN                   
*                                                 DISPLAYED                     
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT MVI   FVOMTYP,GTMINF                                                   
         XC    FVADDR,FVADDR                  CLEAR CURSOR                      
         B     EXIT                                                             
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALPAST MVC   FVMSGNO,=AL2(2100)             NO  FLD  TO  INSERT KWD           
         B     IVALXIT2                                                         
         SPACE 1                                                                
IVALNPST MVC   FVMSGNO,=AL2(2101)             NO  KEYWORD  TO     PASTE         
         B     IVALXIT2                                                         
         SPACE 1                                                                
IVALXIT2 LA    RE,HLPKYWDH                    ->  USER KEYWORD FIELD            
         ST    RE,ACURSOR                     RETURN   ADDRESS                  
         ST    RE,FVADDR                      RETURN   ADDRESS                  
         MVI   APPFKEY,0                      RE-SET   PFKEY                    
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO SORT KEYWORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R3                                                       
         USING KYWSORTD,R8                                                      
         SPACE 1                                                                
SORTKEYS NTR1                                                                   
         L     R8,ASORTWRK                                                      
         SR    R6,R6               COUNTER OF SORT RECORDS                      
         SR    R2,R2                                                            
         L     R3,ACDEFTAB                                                      
         MVI   TABLENUM,1          START WITH BASE KEYWORD TABLE                
*                                                                               
SK010    ICM   R2,1,DEFLEN         END OF TABLE?                                
         BZ    SK200               YES, GO SORT THEM                            
         BAS   RE,EXKEYWD          SET UP KEYWORD FOR VALIDATION                
         LR    R1,R3                                                            
         ICM   R1,8,=CL1'E'        VALIDATE FOR ENTRY IN TABLE                  
         GOTO1 VALDEF                                                           
         BE    SK050                                                            
         TM    INOPT1,INOTYPE                                                   
         BO    SK050               DON'T VALIDATE TYPE OF KEYWORD               
         TM    INOPT2,INOEXP2                                                   
         BO    SK050                                                            
         TM    INOPT2,INOEXPD                                                   
         BO    SK050                                                            
*                                                                               
SK020    AR    R3,R2               BUMP TO NEXT DEF ENTRY                       
         B     SK010                                                            
*                                                                               
SK050    TM    DEFCLIND,DEFCLJBR   JOBBER KEYWORD ?                             
         BZ    SK060               NO,  SKIP                                    
         LA    R4,JOBBRWRK         ->   JOBBER WORK AREA                        
         XC    CURKEYWH,CURKEYWH   CLEAR CURRENT KEYWORD HEADER AREA            
*                                  INSERT FIELD LENGTH                          
         MVI   CURKEYWH,L'CURKEYWH+L'CURKEYWD                                   
*                                  MAXIMUM KEYWORD LENGTH                       
         MVI   CURKEYWH+5,L'CURKEYWD                                            
         GOTO1 AJOBCOL,APPARM,(1,CURKEYWH),(R4),ACOM                            
         CLI   4(R1),0             VALID JOBBER ESTIMATE KEYWORD ?              
         BE    SK020               NOT FOUND,   NOT A ESTIMATE TYPE             
                                                                                
*                                                                               
SK060    MVC   KYWCODE,CURKEYWD                                                 
         LR    RF,R3                                                            
         S     RF,ACDEFTAB         FIND DISPLACEMENT TO START OF TABLE          
         STCM  RF,15,KYWDISP       SAVE IT                                      
         LA    R8,KYWLNQ(,R8)      NEXT SPACE IN TABLE                          
         LA    R6,1(,R6)           INCREMENT COUNTER                            
         B     SK020                                                            
*                                                                               
SK200    CLI   TABLENUM,1          BASE KEYWORD TABLE                           
         BNE   SK300               NO,  DONE                                    
         MVI   TABLENUM,2          SAY  SECOND KEYWORD TABLE                    
         L     R3,ACDEFTB2         ->   SECOND KEYWORD TABLE                    
         B     SK010               TRY  AGAIN                                   
*                                                                               
SK300    XC    0(KYWLNQ,R8),0(R8)  CLEAR LAST ONE                               
         STH   R6,SORTNUM          SAVE # OF SORT RECORDS                       
         ST    R8,ASORTEND                                                      
         GOTO1 AXSORT,APPARM,ASORTWRK,(R6),KYWLNQ,L'KYWCODE,0                   
         B     EXIT99                                                           
*                                                                               
         DROP  R3,R8               KEEP IT   CLEAN                              
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
REPTTAB  DS    0F                                                               
         DC    C'R',AL4(REPRCV)                                                 
         DC    C'I',AL4(REPINC)                                                 
         DC    C'P',AL4(REPPAY)                                                 
         DC    C'X',AL4(REPEXP)                                                 
         DC    C'V',AL4(REPPROD)                                                
         DC    C'1',AL4(REPCOST)                                                
         DC    C'2',AL4(REPPNL)                                                 
         DC    C'B',AL4(REPCASH)                                                
         DC    C'G',AL4(REPGEN)                                                 
         DC    C'Z',AL4(REP_SZ)                                                 
         DC    AL1(0)                                                           
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXDISP  EQU   26                                                               
MAXSCRLL EQU   26                                                               
SAVEREG  DS    6A                                                               
AXSORT   DS    A                   XSORT ROUTINE                                
ASORTWRK DS    A                   BEGINING OF SORT BLOCK                       
ASORTEND DS    A                   END OF BLOCK                                 
SORTNUM  DS    H                   NUMBER OF SORT RECORDS                       
SCROLL   DS    H                                                                
ERRORMSG DS    CL60                                                             
XTRA_MSG DS    CL15                                                             
MSGAREA  DS    CL(L'HELPDSC*5)                                                  
CHOPWRK  DS    CL(L'HELPDSC*5)                                                  
LISTLOC  DS    CL1                 LIST LOCATION FLAG T-TOP, E-END              
COLNUM   DS    CL1                 COLUMN CURRENTLY IN                          
COL1     EQU   1                                                                
COL2     EQU   2                                                                
SAVEUL   DS    CL2                                                              
CURKEYWH DS    CL8                 CURRENT KEYWORD HEADER                       
CURKEYWD DS    CL(KEYWDLN)         CURRENT KEYWORD                              
REPCDE   DS    CL(L'APREPCDE)                                                   
TABLENUM DS    XL1                                                              
SVPFKEY  DS    XL(L'APPFKEY)       SAVE PF KEY                                  
JOBBRWRK DS    CL(JBCLENQ+1)       JOBBER WORK AREA                             
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
HELPD    DSECT                                                                  
HELPKYH  DS    CL8                 HEADER FOR NUMBER                            
HELPKY   DS    CL(L'HLPKYW1)       KEYWORD                                      
HELPDSCH DS    CL8                 '= '                                         
HELPDSC  DS    CL(L'HLPDSC1)       DEFINITION                                   
HELPLNQ  EQU   *-HELPD                                                          
         EJECT ,                                                                
         SPACE 1                                                                
KYWSORTD DSECT                                                                  
KYWCODE  DS    CL6                 KEYWORD                                      
KYWDISP  DS    XL4                 DISP OF KEYWORD TO TABLE                     
KYWLNQ   EQU   *-KYWCODE                                                        
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
       ++INCLUDE ACSCRF6D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACSCR08   09/02/15'                                      
         END                                                                    
