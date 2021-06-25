*          DATA SET PEPAN00    AT LEVEL 071 AS OF 02/25/11                      
*PHASE TE0800A                                                                  
*&&      SET   NOP=N                                                            
         PRINT NOGEN                                                            
PEPANX   CSECT                                                                  
         NMODL WORKX-WORKD,**PANX*,RA,RR=R4,CLEAR=YES                           
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   SVPARMS,0(R1)                                                    
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
         L     R9,ATWA                                                          
         USING PEPANFFD,R9         R9=A(TWA)                                    
         USING TWAD,TWAHDR                                                      
         LA    R7,3072(R9)         R7=A(TWA SAVE AREA)                          
         USING SAVED,R7                                                         
*                                                                               
         USING PEPANKD,PERKEY      SET UP KEY USING                             
         USING TSARD,TSARBLK       SET UP TSAR USING                            
         ST    R8,TSACOM                                                        
*                                                                               
         L     R1,=A(TSARBUFF-WORKD)                                            
         AR    R1,RC                                                            
         ST    R1,ATSARBUF                                                      
*                                                                               
         GOTO1 CCALLOV,DMCB,0,X'D9000A5D',0                                     
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         ICM   R0,3,TIOBCURD                                                    
         AR    R0,R9               ADD TWA ADDRESS                              
         ST    R0,CURSOR                                                        
         MVC   PFKEY,TIOBAID       SAVE PFKEY DATA                              
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         CLI   PFKEY,12            TEST FOR PF 13 - 24                          
         BNH   *+8                                                              
         SH    R0,=H'12'           CONVERT TO 7 - 12                            
         STC   R0,PFKEY                                                         
*                                                                               
         CLI   LASTREC,C'Y'        STOP USER FROM PF8 OFF SCREEN                
         BNE   *+16                                                             
         CLI   PFKEY,8                                                          
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
         MVI   LASTREC,C'N'                                                     
*                                                                               
         LA    R1,PANSCNH                                                       
         ST    R1,CURSOR                                                        
*                                                                               
         L     R1,ALOCSYS                                                       
         MVC   VPANIC,APANIC-ACCFACSD(R1)                                       
         OC    VPANIC,VPANIC                                                    
         BZ    ERR6                                                             
*                                                                               
MAIN010  BAS   RE,VALIDATE         VALIDATE INPUT                               
         BNE   INFO1                                                            
*                                                                               
         CLI   CHANGED,C'Y'        IF REQUEST HAS CHANGED                       
         BE    MAIN020                                                          
*                                                                               
         CLI   PFKEY,3             PF3 RETURNS TO VALSEL                        
         BE    MAIN030                                                          
*                                                                               
         CLI   MODE,C'D'           ALREADY IN DISPLAY MODE                      
         BE    MAIN040                                                          
*                                                                               
MAIN015  BAS   RE,VALSEL           SEE IF ANYTHING SELECTED                     
         BNE   MAIN030                                                          
         XC    DISPPAGE,DISPPAGE                                                
         B     MAIN040                                                          
*                                                                               
MAIN020  MVI   MODE,C'L'                                                        
         SR    R1,R1               CLEAR SAVED STORAGE                          
         SR    R0,R0                                                            
         LH    RF,=H'1024'         CLEAR FOR 1K                                 
         LA    RE,SAVECLR                                                       
         MVCL  RE,R0                                                            
         BAS   RE,RELOAD           RELOAD                                       
         BNE   ERR1                                                             
*                                                                               
MAIN030  MVI   MODE,C'L'                                                        
         BAS   RE,LIST             MAIN DISPLAY LOOP                            
         B     EXITX                                                            
*                                                                               
MAIN040  BAS   RE,VALSHIFT         TEST FOR BOOK SHIFT                          
         BAS   RE,VALPRINT         TEST FOR BOOK PRINT                          
*                                                                               
         BAS   RE,DISPLAY          DISPLAY BOOK CONTENTS                        
         B     EXITX                                                            
*                                                                               
EXITX    L     R1,CURSOR           SET CURSOR FLAG                              
         OI    6(R1),X'40'                                                      
         XMOD1                                                                  
         SPACE 1                                                                
*                                                                               
XITNO    LTR   RB,RB                                                            
         B     XIT1                                                             
XITYES   CR    RB,RB                                                            
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELD                                                *         
***********************************************************************         
         SPACE 1                                                                
VALIDATE NTR1                                                                   
         MVC   OLDBOOK,NOWBOOK     PRESET SCAN FIELDS                           
         MVC   OLDLABL,NOWLABL                                                  
         MVC   OLDIGNOR,NOWIGNOR                                                
         XC    NOWIGNOR,NOWIGNOR                                                
         XC    NOWBOOK,NOWBOOK                                                  
         XC    NOWLABL,NOWLABL                                                  
         XC    BOOKLEN,BOOKLEN                                                  
*                                                                               
VALID010 SR    R1,R1               ANY SCAN REQUESTED                           
         ICM   R1,1,PANSCNH+5                                                   
         BZ    XITNO                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE LABLE TO FIELD                          
         B     *+10                                                             
         MVC   NOWLABL(0),PANSCN                                                
         ST    R1,LABLLEN                                                       
         OI    PANSCNH+6,X'80'     XMIT                                         
*                                                                               
VALID020 SR    R1,R1               ANY BOOK REQUESTED                           
         ICM   R1,1,PANBOOH+5                                                   
         BZ    VALID025                                                         
         STC   R1,BOOKLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE BOOKNAME TO FIELD                       
         B     *+10                                                             
         MVC   NOWBOOK(0),PANBOO                                                
         OI    PANBOOH+6,X'80'     XMIT                                         
*                                                                               
VALID025 SR    R1,R1               ANY BOOK REQUESTED                           
         ICM   R1,1,PANIGNH+5                                                   
         BZ    VALID030                                                         
         MVC   NOWIGNOR,PANIGN                                                  
         OI    PANIGNH+6,X'80'     XMIT                                         
*                                                                               
VALID030 CLC   OLDBOOK,NOWBOOK     HAS REQUEST CHANGED                          
         BNE   VALID050                                                         
         CLC   OLDLABL,NOWLABL                                                  
         BNE   VALID050                                                         
         CLC   OLDIGNOR,NOWIGNOR                                                
         BNE   VALID050                                                         
*                                                                               
VALID040 MVI   CHANGED,C'N'        NO CHANGE                                    
         B     XITYES                                                           
*                                                                               
VALID050 MVI   CHANGED,C'Y'        YES CHANGED                                  
         B     XITYES                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT FIELDS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   NTR1                                                                   
*                                                                               
         LA    RE,THISPAGE         STARTING POINT                               
         LA    RF,PANSELH                                                       
*                                                                               
VALSEL1  OC    0(10,RE),0(RE)      ANY BOOK DISPLAYED                           
         BZ    VALSELX                                                          
         CLI   5(RF),0             NO INPUT                                     
         BE    VALSEL2                                                          
*                                                                               
         CLI   8(RF),C'S'          S IS THE ONLY THING I'LL ACCEPT              
         BNE   VALSEL2                                                          
         MVC   BOOK,0(RE)          SAVE BOOK AND EXIT                           
         MVC   DA,10(RE)                                                        
         B     VALSELY                                                          
*                                                                               
VALSEL2  LA    RE,14(RE)           NEXT LINE                                    
         LA    RF,93(RF)                                                        
         LA    R1,PANPFKH          TEST END OF SCREEN                           
         CR    RF,R1                                                            
         BL    VALSEL1                                                          
*                                                                               
VALSELX  B     XITNO                                                            
*                                                                               
VALSELY  B     XITYES                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE BOOK SHIFT WITH PF5 OR PF6                                 *         
***********************************************************************         
         SPACE 1                                                                
VALSHIFT NTR1                                                                   
*                                                                               
         CLI   PFKEY,5             5 SHIFT UP A BOOK                            
         BE    VALS010                                                          
         CLI   PFKEY,6             6 SHIFT DOWN A BOOK                          
         BE    VALS010                                                          
         B     VALSHIFX                                                         
*                                                                               
VALS010  LA    R0,20               FIND BOOK ON THIS PAGE                       
         LA    R1,THISPAGE                                                      
VALS011  CLC   BOOK,0(R1)                                                       
         BE    VALS020                                                          
         LA    R1,14(R1)                                                        
         BCT   R0,VALS011                                                       
         DC    H'0'                                                             
*                                                                               
VALS020  CLI   PFKEY,5             PREVIOUS BOOK                                
         BNE   VALS040                                                          
         CHI   R0,20               WAS IS FIRST ON PAGE                         
         BE    VALS030                                                          
         SHI   R1,14               GO TO PREVIOUS                               
         MVC   BOOK,0(R1)                                                       
         MVC   DA,10(R1)                                                        
         B     VALSHIFY                                                         
*                                                                               
VALS030  MVI   PFKEY,7             GO TO LAST BOOK OF PREVIOUS PAGE             
         CLC   LISTPAGE,=F'0'                                                   
         BE    ERR3                                                             
         BAS   RE,LIST                                                          
         LA    R1,THISPAGE+(14*19)                                              
         MVC   BOOK,0(R1)                                                       
         MVC   DA,10(R1)                                                        
         B     VALSHIFY                                                         
*                                                                               
VALS040  CHI   R0,1                WAS THIS LAST BOOK                           
         BE    VALS050                                                          
         AHI   R1,14               GO TO PREVIOUS                               
         MVC   BOOK,0(R1)                                                       
         MVC   DA,10(R1)                                                        
         OC    DA,DA                                                            
         BZ    ERR2                                                             
         B     VALSHIFY                                                         
*                                                                               
VALS050  MVI   PFKEY,8             GO TO FIRST BOOK OF NEXT PAGE                
         L     R4,LISTPAGE                                                      
         BAS   RE,LIST                                                          
         C     R4,LISTPAGE                                                      
         BE    ERR2                                                             
         LA    R1,THISPAGE                                                      
         MVC   BOOK,0(R1)                                                       
         MVC   DA,10(R1)                                                        
         B     VALSHIFY                                                         
*                                                                               
VALSHIFY MVI   MODE,C'R'           SET TO RELOAD TSAR                           
         XC    DISPPAGE,DISPPAGE                                                
*                                                                               
VALSHIFX B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINT REQUEST                                              *         
***********************************************************************         
         SPACE 1                                                                
VALPRINT NTR1                                                                   
*                                                                               
         CLI   PFKEY,4             WAS PRINT PF KEY HIT                         
         BNE   VALPRINX                                                         
*                                                                               
         MVI   FLUSH,C'N'                                                       
         MVC   TSABUF,ATSARBUF                                                  
         MVI   TSACTN,TSARES       RESTORE TSAR BUFFER                          
         MVI   TSPAGL,1                                                         
*NOP     MVI   TSPAGN,TSNMAX       MAX PAGES                                    
*NOP     MVI   TSPAGN,40           USE 40                                       
         MVI   TSPAGN,20           USE 20                                       
         MVI   TSKEYL,5            KEY LEN                                      
         MVC   TSRECL,=H'85'       RECORD LEN+KEY                               
         MVI   TSINDS,TSIALLOC                                                  
*NOP     MVI   TSIND2,TSI2BIGN                                                  
*NOP     MVI   TSNBUF,2                                                         
         LA    R1,PANAREA          SET UP A(RECORD)                             
         ST    R1,TSAREC                                                        
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         LA    R4,WLINE            INITIALISE OPEN PRINT LINE                   
         USING PQPLD,R4                                                         
         XC    WLINE,WLINE                                                      
         MVI   QLEXTRA,X'FF'       SET NEW STYLE CALL                           
*                                                                               
         MVC   QLSUBID,=C'PAN'     PAN,NNNN                                     
         MVC   QLSRCID,TWAUSRID                                                 
         MVC   QLRETNL,=X'0018'    24 HRS                                       
         MVC   QLRETND,=X'000C'    12 HRS                                       
         MVI   QLCLASS,C'P'        CLASS P                                      
         MVC   QLDESC,PANSCN       DESCRIPTION = SCAN FIELD                     
         MVI   QLLINET,QLLTFL+QLLTCC                                            
         MVI   QLLINEW,80                                                       
         MVI   QLSTAT,QLSTAC                                                    
         MVC   QLPAGES,=X'0001'                                                 
*                                                                               
OPEN     GOTO1 CDATAMGR,DMCB,DMOPEN,PRTQUE,0,WLINE,ATIA                         
         CLI   8(R1),0                                                          
         BNE   ERR4                                                             
*                                                                               
         MVC   HALF,QLREPRNO       SAVE NUMBER IN HALF                          
*                                                                               
         LA    R6,IOAREA           START BUFFERING INTO IOAREA                  
         LR    R1,R6                                                            
         A     R1,=A(81*65)                                                     
         ST    R1,FULL             SAVE END ADDRESS IN FULL                     
*                                                                               
         MVI   TSACTN,TSAGET       GET FIRST LINE                               
         MVC   TSRNUM,=X'0001'                                                  
         B     PRINT010                                                         
*                                                                               
PRINT005 MVI   TSACTN,TSANXT       READ NEXT LINE                               
*                                                                               
PRINT010 GOTO1 VTSAR,TSARBLK       GET LINE FROM TSAR                           
         CLI   TSERRS,0                                                         
         BE    PRINT015                                                         
         MVI   FLUSH,C'Y'                                                       
         ST    R6,APAGEBRK                                                      
         B     PRINT020                                                         
*                                                                               
PRINT015 MVC   0(81,R6),PANCC      PUT LINE TO BUFFER                           
         CLC   6(4,R6),=C'- - '                                                 
         BNE   *+8                                                              
         ST    R6,APAGEBRK         POSSIBLE PAGE BREAK                          
         LA    R6,81(R6)                                                        
         C     R6,FULL             COMPARE WITH END                             
         BL    PRINT005                                                         
*                                                                               
PRINT020 MVC   PANAREA(2),=C'- '   BUILD HEADER                                 
         MVC   PANAREA+2(78),PANAREA                                            
         MVC   PANAREA+20(10),BOOK                                              
         MVC   PANAREA+40(12),PANSCN                                            
         MVI   PANCC,X'09'                                                      
*                                                                               
PRINT030 GOTO1 CDATAMGR,DMCB,DMPRINT,PRTQUE,0,PANCC,ATIA                        
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
         LA    R6,IOAREA                                                        
PRINT040 MVI   0(R6),9                                                          
*                                                                               
PRINT041 MVC   WORK(72),6(R6)      TIDY UP PRINT                                
         MVC   6(3,R6),SPACES                                                   
         MVC   9(72,R6),WORK                                                    
         GOTO1 CDATAMGR,DMCB,DMPRINT,PRTQUE,0,(R6),ATIA                         
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         CLI   0(R6),X'89'                                                      
         BE    PRINT050                                                         
*                                                                               
         LA    R6,81(R6)           NEXT LINE                                    
         C     R6,APAGEBRK                                                      
         BL    PRINT040                                                         
         CLI   FLUSH,C'Y'          ARE WE FLUSHING BUFFER                       
         BE    PQCLOSE                                                          
         MVI   0(R6),X'89'         SET PAGE EJECT                               
         B     PRINT041                                                         
*                                                                               
PRINT050 LA    RE,IOAREA           IOAREA=DEST                                  
         LA    R6,81(R6)                                                        
         LR    R0,R6               R0=SOURCE                                    
         L     RF,FULL                                                          
         SR    RF,R0               LEN=FULL-RE                                  
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         L     RF,FULL                                                          
         SR    RF,R6               LEN=FULL-R6                                  
         LA    R6,IOAREA                                                        
         AR    R6,RF                                                            
         B     PRINT005                                                         
*                                                                               
PQCLOSE  GOTO1 CDATAMGR,DMCB,DMCLOSE,PRTQUE,0,WLINE,ATIA                        
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         B     INFO3                                                            
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
ERROR    DC    H'0'                                                             
*                                                                               
VALPRINX B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* NEW SEARCH - RELOAD SAVE AREAS                                      *         
***********************************************************************         
         SPACE 1                                                                
RELOAD   NTR1                                                                   
         XC    PERKEY,PERKEY                                                    
         MVI   PEKPANID,PEKPANIQ                                                
         MVI   PEKPTYP,PEKPLQ                                                   
         MVC   PEKPLABL,NOWLABL    KEY ON LABLE                                 
         MVC   PEKPBOOK,NOWBOOK    AND BOOK                                     
*                                                                               
         SR    R0,R0               REPLACE * WITH SPACE                         
         ICM   R0,1,BOOKLEN                                                     
         BZ    RELOAD02                                                         
         LA    R1,PEKPBOOK                                                      
RELOAD01 CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,RELOAD01                                                      
*                                                                               
RELOAD02 GOTO1 CDATAMGR,DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     RELOAD04                                                         
*                                                                               
RELOAD03 GOTO1 CDATAMGR,DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RELOAD04 CLC   PEKPLABL,NOWLABL    TEST FOR NONE FOUND                          
         BNE   RELOADX                                                          
*                                                                               
         BAS   RE,CHKBOOK          CHECK BOOK FILTER                            
         BNE   RELOAD03                                                         
*                                                                               
RELOADY  MVC   BOOKLIST,PEKPBOOK   SET FIRST BOOK                               
         B     XITYES                                                           
*                                                                               
RELOADX  B     XITNO                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK BOOK FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
CHKBOOK  SR    R0,R0               CHECK BOOK FILTER                            
         ICM   R0,1,BOOKLEN                                                     
         BZ    CHKBOOK5                                                         
         LA    R1,PEKPBOOK                                                      
         LA    RF,NOWBOOK                                                       
CHKBOOK3 CLI   0(RF),C'*'          ALLOW WILD CARDS                             
         BE    CHKBOOK4                                                         
         CLC   0(1,R1),0(RF)                                                    
         BNE   CHKBOOKN                                                         
CHKBOOK4 LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CHKBOOK3                                                      
*                                                                               
CHKBOOK5 CLI   NOWIGNOR,C' '       ANY BOOKS TO IGNORE                          
         BNH   CHKBOOKY                                                         
*                                                                               
         LA    R0,10                                                            
         LA    R1,PEKPBOOK         FIND FIRST SPACE IN BOOKNAME                 
         CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         LA    RF,PEKPBOOK         COMPARE BOOK WITH PREVIOUS                   
         SR    R1,RF                                                            
         BZ    CHKBOOKY                                                         
         SHI   R1,2                -1 FOR EX -1 FOR LAST CHR                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PREVBOOK(0),PEKPBOOK                                             
         BNE   CHKBOOKY                                                         
*                                                                               
         LA    RF,PREVBOOK(R1)                                                  
         CLI   1(RF),C' '                                                       
         BNE   CHKBOOKY                                                         
*                                                                               
         LA    RF,PREVBOOK         ISOLATE AND CHECK LAST CHR                   
         LA    R1,PEKPBOOK(R1)                                                  
         MVC   BYTE,1(R1)                                                       
*                                                                               
         LA    R0,8                IS LAST CHR AN IGNORE CHR                    
         LA    R1,NOWIGNOR                                                      
CHKBOOK9 CLC   BYTE,0(R1)                                                       
         BE    CHKBOOKN                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CHKBOOK9                                                      
*                                                                               
CHKBOOKY MVC   PREVBOOK,PEKPBOOK                                                
         CR    RB,RB                                                            
         BER   RE                                                               
CHKBOOKN LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD LIST SEL TABLES AND DISPLAY LIST ON SCREEN                    *         
***********************************************************************         
         SPACE 1                                                                
LIST     NTR1                                                                   
*                                                                               
         MVC   PANINF(70),DASHES                                                
         MVC   PANINF+6(10),=C'Bookname  '                                      
         MVC   PANINF+18(8),=C'Matches '                                        
         OI    PANINFH+6,X'80'                                                  
         BAS   RE,BLDLIST          BUILD LIST SCREEN                            
*                                                                               
         MVC   PANPFK,SPACES                                                    
         MVC   PANPFK,LISTPFK                                                   
         OI    PANPFKH+6,X'80'                                                  
*                                                                               
         LA    R4,PANSELH                                                       
LIST010  OI    6(R4),X'80'         ZAP ALL FIELDS TO SPACES                     
         MVC   8(5,R4),SPACES                                                   
         LA    R4,13(R4)                                                        
         OI    6(R4),X'80'                                                      
         MVC   8(72,R4),SPACES                                                  
         LA    R4,80(R4)                                                        
         LA    R1,PANPFKH          NEXT                                         
         CR    R4,R1                                                            
         BL    LIST010                                                          
*                                                                               
         CLI   CHANGED,C'Y'        REQUEST CHANGED                              
         BE    LIST030                                                          
*                                                                               
LIST020  L     RF,LISTPAGE         HANDLE PF KEYS                               
         CLI   PFKEY,7                                                          
         BNE   *+6                                                              
         BCTR  RF,0                PF7 UP                                       
         CLI   PFKEY,8                                                          
         BNE   *+8                                                              
         LA    RF,1(RF)            PF8 DOWN                                     
         LTR   RF,RF                                                            
         BP    *+8                                                              
         LA    RF,0                                                             
LIST021  ST    RF,LISTPAGE         SET TOP OF NEXT PAGE                         
*                                                                               
LIST025  L     RF,LISTPAGE         FIND BOOK TO START LIST FROM                 
         MH    RF,=H'10'                                                        
         LA    RF,BOOKLIST(RF)                                                  
         CLI   0(RF),0             BACK UP AT END OF LIST                       
         BNE   LIST026                                                          
         L     RF,LISTPAGE                                                      
         BCTR  RF,0                                                             
         B     LIST021                                                          
*                                                                               
LIST026  XC    PERKEY,PERKEY       PRESET KEY FIELDS                            
         MVI   PEKPANID,PEKPANIQ                                                
         MVI   PEKPTYP,PEKPLQ                                                   
         MVC   PEKPBOOK,0(RF)                                                   
         MVC   PEKPLABL,NOWLABL    KEY ON LABLE                                 
*                                                                               
         GOTO1 CDATAMGR,DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LIST029                                                          
*                                                                               
LIST028  GOTO1 CDATAMGR,DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LIST029  CLC   PEKPLABL,NOWLABL    TEST KEY WITHIN RANGE                        
         BNE   ERR1                                                             
*                                                                               
         BAS   RE,CHKBOOK          TEST BOOK FILTER STILL OK                    
         BNE   LIST028                                                          
*                                                                               
LIST030  XC    THISPAGE(14*10),THISPAGE                                         
         XC    THISPAGE+140(14*10),THISPAGE+140                                 
         LA    R5,THISPAGE         THIS PAGE BOOK LIST                          
*                                                                               
         LA    R4,PANFRSH                                                       
LIST040  OI    6(R4),X'80'         XMIT                                         
         NI    1(R4),X'FF'-X'0C'   NORMAL INTENSITY                             
         MVC   8(10,R4),PEKPBOOK                                                
         EDIT  (B2,PEDSTAT),(4,20(R4)),ALIGN=LEFT                               
         MVC   0(10,R5),PEKPBOOK   FILL IN THISPAGE                             
         MVC   10(4,R5),PEKDA                                                   
         LA    R5,14(R5)                                                        
         LA    R4,93(R4)                                                        
*                                                                               
LIST045  GOTO1 CDATAMGR,DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   PEKPLABL,NOWLABL    TEST KEY WITHIN RANGE                        
         BNE   LIST055                                                          
*                                                                               
         BAS   RE,CHKBOOK          TEST BOOK FILTER STILL OK                    
         BNE   LIST045                                                          
*                                                                               
LIST050  LA    R1,PANPFKH          TEST FOR END OF SCREEN                       
         CR    R4,R1                                                            
         BL    LIST040             OK CARRY ON                                  
*                                                                               
         L     RF,LISTPAGE         SET TOP OF NEXT PAGE                         
         LA    RF,1(RF)                                                         
         MH    RF,=H'10'                                                        
         LA    RF,BOOKLIST(RF)                                                  
         MVC   0(10,RF),PEKPBOOK                                                
*                                                                               
LIST055  MVC   PANHDR(60),SPACES                                                
         MVC   PANHDR(25),=C'Scan results displayed   '                         
         OI    PANHDRH+6,X'80'                                                  
         LA    R1,PANSELH                                                       
         ST    R1,CURSOR                                                        
         B     LISTX                                                            
*                                                                               
LIST060  MVI   LASTREC,C'Y'                                                     
*                                                                               
LISTX    BAS   RE,DBINFO           ADD DB INFO TO LIST SCREEN                   
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DATABASE INFO SCREEN                                                *         
***********************************************************************         
         SPACE 1                                                                
DBINFO   NTR1                                                                   
*                                                                               
         XC    PERKEY,PERKEY       PRESET KEY FIELDS                            
         MVI   PEKPANID,PEKPANIQ                                                
         MVI   PEKPTYP,PEKHDR      FOR HEADER REECORD                           
*                                                                               
         GOTO1 CDATCON,DMCB,(5,0),(15,JTODAY)                                   
*                                                                               
         GOTO1 CDATAMGR,DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,PERKEY+38                                                     
         CLC   PEKPANID(2),=X'02C8'                                             
         BNE   DBINFOX                                                          
         GOTO1 CDATAMGR,DMCB,GETREC,PERFIL,DA,IOAREA,DMWORK                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOAREA                                                        
         LA    R1,PEKDATA-PEPANKD(R1)                                           
         MVC   LOADDATE,2(R1)      1ST EL IS DATE                               
         MVC   LOADJULE,11(R1)                                                  
*                                                                               
         XC    LOADJULE(2),LOADJULE                                             
         XC    JTODAY(2),JTODAY                                                 
         SP    JTODAY,LOADJULE     CONVERT JTODAY TO DAYS OLD                   
         BNM   *+10                                                             
         AP    JTODAY,=P'365'      NEW YEAR                                     
         MVC   LOADJULE,11(R1)                                                  
*                                                                               
         LA    R1,15(R1)                                                        
         LA    R4,PANSELH                                                       
         LA    R4,13(R4)                                                        
         LA    R4,80(R4)                                                        
         LA    R4,13(R4)                                                        
*                                                                               
         ICM   RF,15,DBICOUNT                                                   
         BM    DBINF005                                                         
         CLI   PFKEY,6             6 IS DOWN 10                                 
         BNE   *+8                                                              
         AHI   RF,10                                                            
DBINF005 N     RF,=X'00FFFFFF'                                                  
         CLI   PFKEY,5             5 IS UP 10                                   
         BNE   *+12                                                             
         SHI   RF,10                                                            
         BM    *+8                                                              
         ST    RF,DBICOUNT                                                      
*                                                                               
         OI    6(R4),X'80'         XMIT                                         
         NI    1(R4),X'FF'-X'0C'   NORMAL INTENSITY                             
         MVC   40(30,R4),=C'*----Database Information----*'                     
         SR    RF,RF                                                            
         ICM   RF,3,DBILINE                                                     
*                                                                               
         LA    R0,17                                                            
         L     RF,DBICOUNT                                                      
DBINF010 LA    R4,93(R4)                                                        
         MVC   40(30,R4),=C'|                            |'                     
*                                                                               
         CHI   RF,1                                                             
         BNE   *+16                                                             
         MVC   41(28,R4),=C'- -  Built on ddmmmyyyy  - -'                       
         MVC   55(10,R4),LOADDATE                                               
*                                                                               
         CHI   RF,2                                                             
         BNE   DBINF015                                                         
         MVC   41(28,R4),=C'- - - -  nn Days ago - - - -'                       
         EDIT  (P4,JTODAY),(2,50(R4)),FILL=0                                    
         CP    JTODAY,=PL1'1'                                                   
         BNE   DBINF015                                                         
         MVC   41(28,R4),=C'- - - - - Yesterday - - - - '                       
*                                                                               
DBINF015 CHI   RF,4                                                             
         BNE   *+10                                                             
         MVC   41(28,R4),=C'The following books were too'                       
         CHI   RF,5                                                             
         BNE   *+10                                                             
         MVC   41(28,R4),=C'big to be fully included:   '                       
*                                                                               
         CHI   RF,7                                                             
         BL    DBINF020                                                         
         LR    RE,RF                                                            
         SHI   RE,7                                                             
         MH    RE,=H'12'                                                        
         AR    RE,R1                                                            
         MVC   41(10,R4),SPACES                                                 
         CLI   0(RE),0                                                          
         BE    *+14                                                             
         MVC   41(10,R4),2(RE)                                                  
         B     DBINF020                                                         
*                                                                               
         OI    DBICOUNT,X'80'                                                   
*                                                                               
DBINF020 LA    RF,1(RF)                                                         
         BCT   R0,DBINF010                                                      
*                                                                               
         MVC   40(30,R4),=C'*--PF5 Up PF6 Down-----------*'                     
*                                                                               
DBINFOX  B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY BOOK CONTENTS                                               *         
***********************************************************************         
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BAS   RE,BLDBOOK          BUILD BOOK SCREEN                            
         MVC   PANPFK,SPACES                                                    
         MVC   PANPFK,DISPPFK                                                   
         OI    PANPFKH+6,X'80'                                                  
         CLI   MODE,C'D'                                                        
         BE    DISP000                                                          
*                                                                               
         MVI   MODE,C'D'           SET DISPLAY MODE                             
         BAS   RE,TEMPLOAD         LOAD BOOK TO TEMPEST                         
         B     DISP005                                                          
*                                                                               
DISP000  MVC   TSABUF,ATSARBUF                                                  
         MVI   TSACTN,TSARES                                                    
         MVI   TSPAGL,1                                                         
*NOP     MVI   TSPAGN,TSNMAX       MAX PAGES                                    
*NOP     MVI   TSPAGN,40           USE 40                                       
         MVI   TSPAGN,20           USE 40                                       
         MVI   TSKEYL,5            KEY LEN                                      
         MVC   TSRECL,=H'85'       RECORD LEN+KEY                               
         MVI   TSINDS,TSIALLOC                                                  
*NOP     MVI   TSIND2,TSI2BIGN                                                  
*NOP     MVI   TSNBUF,2                                                         
         LA    R1,PANAREA          SET UP A(RECORD)                             
         ST    R1,TSAREC                                                        
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
DISP005  LA    R4,PANSELH                                                       
DISP010  OI    6(R4),X'80'         ZAP ALL FIELDS TO SPACES                     
         MVC   8(5,R4),SPACES                                                   
         LA    R4,13(R4)                                                        
         OI    6(R4),X'80'                                                      
         MVC   8(72,R4),SPACES                                                  
         LA    R4,80(R4)                                                        
         LA    R1,PANPFKH          NEXT                                         
         CR    R4,R1                                                            
         BL    DISP010                                                          
*                                                                               
DISP020  L     RF,DISPPAGE         HANDLE PF KEYS                               
         CLI   PFKEY,7                                                          
         BNE   *+6                                                              
         BCTR  RF,0                PF7 UP                                       
         CLI   PFKEY,8                                                          
         BNE   *+8                                                              
         LA    RF,1(RF)            PF8 DOWN                                     
         LTR   RF,RF                                                            
         BP    *+8                                                              
         LA    RF,0                                                             
DISP021  ST    RF,DISPPAGE         SET TOP OF NEXT PAGE                         
*                                                                               
         MVI   TSACTN,TSAGET                                                    
         LA    R1,10                                                            
         SR    R0,R0                                                            
         MR    R0,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TSRNUM                                                        
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         LA    R4,PANSELH                                                       
DISP040  OI    6(R4),X'80'         XMIT                                         
         MVC   8(5,R4),PANAREA                                                  
         LA    R4,13(R4)                                                        
         OI    6(R4),X'80'         XMIT                                         
         MVC   8(72,R4),PANAREA+5                                               
*                                                                               
         NI    1(R4),X'FF'-X'0C'   NORMAL INTENSITY                             
         CLC   8(4,R4),=C'- - '                                                 
         BNE   *+8                                                              
         OI    1(R4),X'08'         SET HIGH INTENSITY                           
*                                                                               
         LR    R1,R4               DO HIGHLIGHTING                              
         LA    R4,80(R4)                                                        
         ST    R4,FULL             SAVE A(NEXT FIELD) IN FULL                   
         BAS   RE,HILITE                                                        
         L     R4,FULL             HILITE MAY HAVE CHANGED THIS                 
*                                                                               
         LA    R1,PANPFKH          TEST FOR END OF SCREEN                       
         CR    R4,R1                                                            
         BNL   DISPLAYX                                                         
*                                                                               
         MVI   TSACTN,TSANXT       GET NEXT RECORD                              
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    DISP040                                                          
         MVI   LASTREC,C'Y'                                                     
         B     DISPLAYX                                                         
*                                                                               
DISPLAYX B     INFO2                                                            
         EJECT                                                                  
***********************************************************************         
* LOAD TEMPEST WITH BOOK CONTENTS                                     *         
***********************************************************************         
         SPACE 1                                                                
TEMPLOAD NTR1                                                                   
         SR    R6,R6                                                            
         MVC   PANINF+6(10),BOOK   SET THE INFO ON LOAD                         
         MVC   PANINF+18(40),DASHES                                             
         OI    PANINFH+6,X'80'                                                  
*                                                                               
         GOTO1 CDATAMGR,DMCB,GETREC,PERFIL,DA,IOAREA,DMWORK                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,IOAREA                                                        
         LA    RF,IOAREA+44        POINT TO FIRST ELEMENT                       
LOAD005  CLI   0(RF),0                                                          
         BE    LOAD007             END OF DATA                                  
         SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),2(RF)       COPY ELEMENT TO IO                           
         AR    RF,R1                                                            
         SH    R1,=H'2'                                                         
         AR    RE,R1                                                            
         B     LOAD005                                                          
*                                                                               
LOAD007  MVC   0(4,RE),=X'FFFFFFFF'                                             
         LA    R4,IOAREA           POINT TO TABLE OF OFFSETS                    
*                                                                               
         CLI   ALLOC,C'Y'          RELEASE TEMPEST BEFORE REALLOCATING          
         BNE   LOAD008                                                          
         GOTO1 CDATAMGR,DMCB,(0,=C'DMRLSE'),=C'TEMPEST',(255,0),0               
         MVI   ALLOC,C'N'                                                       
*                                                                               
LOAD008  MVC   TSABUF,ATSARBUF                                                  
         MVI   TSACTN,TSAINI       INIT                                         
         MVI   TSPAGL,1                                                         
*NOP     MVI   TSPAGN,TSNMAX       MAX PAGES                                    
*NOP     MVI   TSPAGN,40           USE 40                                       
         MVI   TSPAGN,20           USE 40                                       
         MVI   TSKEYL,5            KEY LEN                                      
         MVC   TSRECL,=H'85'       RECORD LEN+KEY                               
         MVI   TSINDS,TSIALLOC                                                  
*NOP     MVI   TSIND2,TSI2BIGN                                                  
*NOP     MVI   TSNBUF,2                                                         
         LA    R1,PANAREA          SET UP A(RECORD)                             
         ST    R1,TSAREC                                                        
*NOP     CLI   ALLOC,C'Y'          DON'T REUSE JUST RELEASE AND REALLOC         
*NOP     BNE   *+8                                                              
*NOP     OI    TSINDS,TSIREUSE                                                  
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ALLOC,C'Y'                                                       
         MVI   TSACTN,TSAADD       ADD                                          
*                                                                               
         MVI   CODE,C'N'                                                        
         MVI   NOT,C'N'                                                         
         SR    R5,R5                                                            
LOAD010  LA    R5,1(R5)                                                         
*                                                                               
         GOTO1 CPROTOFF            TURN PROTECTION OFF FOR PANIC CALL           
*                                                                               
         GOTO1 VPANIC,DMCB,READ,PAN,BOOK,PANAREA+5                              
         CLI   8(R1),0                                                          
         BNE   LOAD052                                                          
*                                                                               
         GOTO1 CPROTON             MAKE SURE IT'S ON - HERE OR LOAD052          
*                                                                               
         CLC   PANAREA+5(20),=C'*          DATA SET '                           
         BNE   LOAD011                                                          
         GOTO1 CDATCON,DMCB,(4,PANAREA+55),(15,WORK)                            
         CP    LOADJULE,WORK(4)                                                 
         BH    LOAD011                                                          
         MVC   PANINF+20(37),=C'*** WARNING *** Index is out of date '          
*                                                                               
LOAD011  CH    R5,0(R4)            CHECK WITH OFFSETS                           
         BNL   LOAD015                                                          
*                                                                               
LOAD012  CLI   NOT,C'Y'            ARE WE IN NOT MODE                           
         BE    LOAD010                                                          
         MVC   NONLINE(72),DOTLINE                                              
         BCTR  R5,0                                                             
         ST    R5,FULL                                                          
         LA    R5,1(R5)                                                         
         MVI   NOT,C'Y'            WE ARE NOW                                   
         B     LOAD010                                                          
*                                                                               
LOAD015  CLI   NOT,C'N'            WERE WE IN NOT MODE                          
         BE    LOAD016                                                          
*                                                                               
         SR    R6,R6               ZERO R6 COUNTER                              
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EDIT  (RF),(5,NONHDR),FILL=0                                           
         S     RF,FULL                                                          
         EDIT  (RF),(5,NONLINE+50)                                              
         LA    R1,NONHDR           SET UP A(RECORD)                             
         ST    R1,TSAREC                                                        
         GOTO1 VTSAR,TSARBLK       WRITE TO TSAR                                
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,PANAREA          SET UP A(RECORD)                             
         ST    R1,TSAREC                                                        
         MVI   NOT,C'N'            NOT NOT ANY MORE                             
*                                                                               
LOAD016  EDIT  (R5),(5,PANAREA),FILL=0                                          
*                                                                               
         CLC   PANAREA+5(8),=C'*INCLUDE'                                        
         BE    LOAD017                                                          
         CLC   PANAREA+5(7),=C'*CATALP'                                         
         BE    LOAD017                                                          
         CLC   PANAREA+5(6),=C'*PHASE'                                          
         BE    LOAD017                                                          
         CLC   PANAREA+5(3),=C'*&&&&'                                           
         BE    LOAD017                                                          
         CLI   PANAREA+5,C'*'      CHECK FOR NON CODE                           
         BE    LOAD020                                                          
LOAD017  CLC   PANAREA+5+9(5),=C'EJECT'                                         
         BE    LOAD020                                                          
         CLC   PANAREA+5+9(5),=C'SPACE'                                         
         BE    LOAD020                                                          
         CLC   PANAREA+5(20),SPACES                                             
         BE    LOAD020                                                          
*                                                                               
         MVI   CODE,C'Y'           OK GOT CODE                                  
         B     LOAD030                                                          
*                                                                               
LOAD020  CLI   CODE,C'Y'           ARE WE WRITING CODE YET                      
         BNE   LOAD030                                                          
         LA    R4,2(R4)            NEXT BLOCK                                   
         CLC   0(4,R4),=X'FFFFFFFF'                                             
         BE    LOAD050                                                          
         CH    R5,0(R4)            CHECK THE BOUNDARY TOO                       
         BL    LOAD025                                                          
         SR    R6,R6                                                            
         B     LOAD040                                                          
*                                                                               
LOAD025  MVI   CODE,C'N'           NOT ANY MORE                                 
         B     LOAD012                                                          
*                                                                               
LOAD030  LA    R6,1(R6)            COUNT LINES OUT                              
         CHI   R6,11               10 IS ENOUGH - BUT WE NEED 11                
         BNL   LOAD020                                                          
*                                                                               
LOAD040  GOTO1 VTSAR,TSARBLK       WRITE TO TSAR                                
         CLI   TSERRS,0                                                         
         BE    LOAD010                                                          
         B     ERR5                                                             
*                                                                               
LOAD050  GOTO1 CPROTOFF            PROTOFF FOR PANIC                            
*                                                                               
LOAD052  GOTO1 VPANIC,DMCB,CLOSE,PAN,BOOK,PANAREA+5                             
*                                                                               
         GOTO1 CPROTON                                                          
*                                                                               
         MVC   PANAREA(85),ENDLINE                                              
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
TEMPLOAX MVI   TSACTN,TSASAV                                                    
         MVI   TSINDS,TSIALLOC                                                  
         MVI   TSPAGN,TSNMAX       MAX PAGES                                    
         MVI   TSPAGN,20           USE 20                                       
         GOTO1 VTSAR,TSARBLK                                                    
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DYNAMIC SCREEN RECONSTRUCTS                                         *         
***********************************************************************         
         SPACE 1                                                                
BLDBOOK  MVI   BYTE,C'B'           BOOK MODE                                    
         B     *+8                                                              
BLDLIST  MVI   BYTE,C'L'           LIST MODE                                    
         ST    RE,SAVERE                                                        
*                                                                               
         LA    R1,PANSELH          INIT POSITION                                
         LA    R0,20               INIT COUNTER                                 
         MVC   HALF,=X'00F1'       INIT ADDR VALUE                              
*                                                                               
BLD010   MVC   0(13,R1),SELHDRS    MOVE IN DUMMY SEL FIELD                      
         MVC   2(2,R1),HALF                                                     
         CLI   BYTE,C'L'                                                        
         BE    *+8                                                              
         MVI   1(R1),X'60'         PROT THIS IN BOOK MODE                       
         LH    RF,HALF                                                          
         LA    RF,6(RF)            BUMP SCREEN ADDR                             
         STH   RF,HALF                                                          
         LA    R1,13(R1)                                                        
*                                                                               
         MVC   0(78,R1),LINHDRS    MOVE IN DUMMY LINE FIELD                     
         MVC   2(2,R1),HALF                                                     
         LH    RF,HALF                                                          
         LA    RF,74(RF)           BUMP SCREEN ADDR                             
         STH   RF,HALF                                                          
         LA    R1,80(R1)                                                        
*                                                                               
         BCT   R0,BLD010                                                        
*                                                                               
         MVC   0(89,R1),PFKHDRS                                                 
         BR    RE                                                               
         SPACE 1                                                                
SELHDRS  DC    XL8'0D0000F100008005'                                            
SELFIELD DC    CL5'     '                                                       
LINHDRS  DC    XL8'506000F700008001'                                            
LINFIELD DC    CL72' '                                                          
PFKHDRS  DC    XL8'5668073100008044'                                            
PFKFIELD DC    CL78' '                                                          
FOOT     DC    XL3'000000'                                                      
         EJECT                                                                  
***********************************************************************         
*        DYNAMIC HILITE  R1=A(FIELD) SCAN FOR NOWLABL AND HILITE      *         
***********************************************************************         
         SPACE 1                                                                
HILITE   NTR1                                                                   
*                                                                               
         LR    R2,R1               SAVE ORIGINAL FIELD ADDR                     
*                                                                               
         LA    R1,8(R1)                                                         
         L     RE,LABLLEN          RE=EXEC LEN                                  
         LA    R0,72               72 = FIELD LENGTH                            
HIL010   EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),NOWLABL     SEARCH FOR A MATCH                           
         BE    HIL020                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,HIL010           SEARCH ALL 72 CHRS                           
         B     HILITEX             NOT FOUND SO DON'T BOTHER                    
*                                                                               
* LABLE EXISTS IN THIS LINE SO BREAK IT UP INTO FIELDS                          
*                                                                               
HIL020   LA    R1,8(R2)            START OF DATA                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         SHI   R0,8                R0=DATA LEN                                  
*                                                                               
HIL025   CLI   0(R1),C' '          FIND FIRST SPACE                             
         BE    HIL030                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,HIL025                                                        
         B     HIL050              END OF FIELD                                 
*                                                                               
HIL030   CLI   0(R1),C' '          FIND FIRST NON SPACE                         
         BNE   HIL040                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,HIL030                                                        
         B     HIL050              END OF FIELD                                 
*                                                                               
HIL040   LA    RE,9(R2)            DON'T ATTEMPT A 0 LEN FIELD                  
         CR    R1,RE                                                            
         BE    HIL025                                                           
*                                                                               
         ST    R1,DUB              SOURCE                                       
         LA    R1,7(R1)                                                         
         ST    R1,DUB+4            DEST                                         
         SHI   R1,7                RESTORE R1                                   
         LA    RF,TWAEND                                                        
         SR    RF,R1                                                            
         ST    RF,FULL             SET UP P3 FOR LENGTH                         
         SHI   R1,1                                                             
         LR    RF,R1                                                            
         LA    R1,DUB                                                           
         BAS   RE,MOVE             MOVE TWA 7 BYTES RIGHT                       
         LR    R1,RF                                                            
         MVC   0(8,R1),0(R2)       COPY INITIAL HEADER                          
*                                                                               
         SR    R1,R2               CALC AND SET SCREEN ADDR                     
         STC   R1,0(R2)            FIX OLD LEN WHILE HERE                       
         SR    RE,RE                                                            
         ICM   RE,3,2(R2)          INITIAL ADDR                                 
         AR    R1,RE               PLUS DIFFERENCE                              
         SHI   R1,7                                                             
         STCM  R1,3,2(RF)          SET NEW SCREEN ADDR                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(RF)            FIX UP NEW LEN                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            START WITH ORIGINAL LEN                      
         SR    R1,RE                                                            
         LA    R1,7(R1)                                                         
         STC   R1,0(RF)            SET LEN IN HEADER                            
         LR    R0,RF                                                            
*                                                                               
HIL050   LR    RF,R0                                                            
         SR    R0,R0               LOOK FOR SEARCH FIELD                        
         IC    R0,0(R2)                                                         
         SHI   R0,8                                                             
         LA    R1,8(R2)                                                         
         L     RE,LABLLEN          RE=EXEC LEN                                  
HIL060   EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),NOWLABL     SEARCH FOR A MATCH                           
         BE    HIL070                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,HIL060           SEARCH ALL FIELD                             
         MVI   1(R2),X'60'         NOT FOUND SO LO INTENSITY                    
         B     *+8                                                              
HIL070   MVI   1(R2),X'68'         FOUND SO HIGH INTENSITY                      
*                                                                               
         LTR   RF,RF               ARE THERE ANY MORE FIELDS                    
         BZ    HIL090                                                           
         LR    R2,RF               YES SO GO DO THEM                            
         B     HIL020                                                           
*                                                                               
HIL090   IC    R0,0(R2)            FIND ADDR OF NEXT LINE                       
         AR    R2,R0                                                            
         ST    R2,FULL             SAVE IT IN FULL                              
*                                                                               
HILITEX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        NON DESTRUCTIVE MOVE ROUTINE                                 *         
***********************************************************************         
MOVE     NTR1                                                                   
         L     R2,0(R1)            P1 SOURCE                                    
         L     R4,4(R1)            P2 DEST                                      
         L     R3,8(R1)            P3 LENGTH                                    
         CR    R4,R2               IS IT DOWNWARDS ?                            
         BH    RMOVE                                                            
NMOVE    LR    R5,R3                                                            
         MVCL  R4,R2               NO SO MVCL                                   
         B     XIT                                                              
EXMOVE   MVC   0(0,R4),0(R2)                                                    
RMOVE    LR    R5,R4                                                            
         SR    R5,R2               R5=OFFSET                                    
         CR    R5,R3               WILL IT OVERLAP ?                            
         BNL   NMOVE                                                            
         CH    R5,=H'256'                                                       
         BNH   M1                                                               
         LH    R5,=H'256'                                                       
M1       AR    R2,R3               GO TO END                                    
         AR    R4,R3                                                            
M0       CR    R3,R5               IS LENGTH < BLOCK                            
         BNL   M2                                                               
         LR    R5,R3               IF YES BLOCK = LENGTH                        
M2       SR    R2,R5                                                            
         SR    R4,R5               BACK 1 BLOCK                                 
         BCTR  R5,0                                                             
         EX    R5,EXMOVE           MOVE 1 BLOCK                                 
         LA    R5,1(R5)                                                         
         SR    R3,R5               SUB BLOCK FROM LENGTH                        
         BNE   M0                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ERR1     MVC   PANHDR(25),=C'No matches found         '                         
         B     ERRX                                                             
ERR2     MVC   PANHDR(25),=C'No more books in list    '                         
         B     ERRX                                                             
ERR3     MVC   PANHDR(25),=C'This is the first book   '                         
         B     ERRX                                                             
ERR4     MVC   PANHDR(25),=C'Error - No report created'                         
         B     ERRX                                                             
ERR5     MVC   PANHDR(25),=C'Error - Loading tempest  '                         
         B     ERRX                                                             
ERR6     MVC   PANHDR(25),=C'Test systems only please '                         
         B     ERRX                                                             
INFO1    MVC   PANHDR(25),=C'Enter field to search for'                         
         B     ERRX                                                             
INFO2    MVC   PANHDR(25),=C'Book details displayed   '                         
         B     ERRX                                                             
INFO3    MVC   PANHDR(25),=C'Report PAN,              '                         
         EDIT  (B2,HALF),(5,PANHDR+11),ALIGN=LEFT                               
         LA    R1,PANHDR+12                                                     
         AR    R1,R0                                                            
         MVC   0(10,R1),=C'Spooled '                                            
         B     ERRX                                                             
*                                                                               
ERRX     OI    PANHDRH+6,X'80'                                                  
         L     RD,SAVERD                                                        
         B     EXITX                                                            
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
DMOPEN   DC    CL8'OPEN   '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
DMCLOSE  DC    CL8'CLOSE  '                                                     
CLOSE    DC    CL8'CLOSE  '                                                     
PRTQUE   DC    CL8'PRTQUE'                                                      
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
PERDIR   DC    C'PERDIR '                                                       
PERFIL   DC    C'PERFIL '                                                       
READ     DC    C'READ   '                                                       
PAN      DC    C'PAN    '                                                       
*                                                                               
DASHES   DC    80C'-'                                                           
SPACES   DC    CL80' '                                                          
DOTLINE  DC    CL40'- - - - - - - - - - - - - - - - - - - - '                   
         DC    CL32'- - - - - ..... lines not shown '                           
ENDLINE  DC    CL40'99999- - - - - - - - - - - - - - END OF '                   
         DC    CL40'DATA - - - - - - - - - - - - - - - - - -'                   
LISTPFK  DC    CL40'PF7=Up PF8=Down                         '                   
         DC    CL38'                                      '                     
DISPPFK  DC    CL40'PF7=Up PF8=Down PF5=Previous Book PF6=Ne'                   
         DC    CL38'xt Book PF3=Return PF4=Print          '                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SAVED STORAGE IN TWA                                 *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
*                                                                               
NOWIGNOR DS    CL8                 IGNORE BOOKS                                 
NOWBOOK  DS    CL10                BOOK FILTER                                  
BOOKLEN  DS    CL1                 LENGTH OF BOOK FILTER                        
NOWLABL  DS    CL12                LABLE FILTER                                 
LABLLEN  DS    A                   LABLE LENGTH - 1                             
*                                                                               
BOOK     DS    CL10                                                             
DA       DS    CL4                                                              
*                                                                               
PREVBOOK DS    CL10                                                             
*                                                                               
ALLOC    DS    C                                                                
MODE     DS    C                                                                
LASTREC  DS    C                                                                
DBILINE  DS    XL2                                                              
*                                                                               
JTODAY   DS    PL4                                                              
LOADJULE DS    PL4                                                              
LOADDATE DS    CL10                                                             
*                                                                               
SAVECLR  DS    0CL1024             THIS AREA CLEARED ON NEW REQUEST             
*                                                                               
BOOKLIST DS    50CL10              TOP BOOK IN EACH PAGE (50 PAGES)             
LISTPAGE DS    F                   CURRENT PAGE FOR LIST                        
DISPPAGE DS    F                   CURRENT PAGE FOR DISPLAY                     
DBICOUNT DS    F                   COUNTER FOR DBI SCREEN                       
*                                                                               
THISPAGE DS    20CL14              BOOKS ON THIS PAGE                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
*DSECT TO COVER WORKING STORAGE                                       *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PFKEY    DS    X                                                                
RELO     DS    A                                                                
CURSOR   DS    A                                                                
ATSARBUF DS    A                                                                
*                                                                               
CHANGED  DS    C                                                                
CODE     DS    C                                                                
NOT      DS    C                                                                
FLUSH    DS    C                                                                
*                                                                               
VPANIC   DS    A                                                                
VTSAR    DS    A                                                                
*                                                                               
         DS    0A                                                               
SVPARMS  DS    0XL32                                                            
ATIOB    DS    A                                                                
ATWA     DS    A                                                                
ALOCSYS  DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                                                                
AXTRA    DS    A                                                                
         DS    A                                                                
         DS    A                                                                
*                                                                               
APAGEBRK DS    A                                                                
*                                                                               
PERKEY   DS    CL44                                                             
*                                                                               
OLDIGNOR DS    CL8                 PREVIOUS FILTERS                             
OLDBOOK  DS    CL10                                                             
OLDLABL  DS    CL12                                                             
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    XL1024                                                           
DMWORK   DS    12D                                                              
*                                                                               
TSARBLK  DS    32F                                                              
*                                                                               
NONHDR   DS    CL5                                                              
NONLINE  DS    CL80                                                             
*                                                                               
         DS    0D                                                               
WLINEL   DS    CL8                                                              
WLINE    DS    0CL255                                                           
WCTL     DS    C                                                                
WDAT     DS    CL254                                                            
*                                                                               
PANCC    DS    X                                                                
PANAREA  DS    1024C                                                            
IOAREA   DS    6144C                                                            
TSARBUFF DS    18432C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TWA                                                  *         
***********************************************************************         
         SPACE 1                                                                
PEPANFFD DSECT                                                                  
TWAHDR   DS    CL64                                                             
       ++INCLUDE PEPANFFD                                                       
         SPACE 1                                                                
         DS    CL256' '            SPACE FOR HILITE HEADERS                     
TWAEND   EQU   *                                                                
*DDCOMFACS                                                                      
*DDACCFACS                                                                      
*DDTSARD                                                                        
*PEGENPAN                                                                       
*FATIOB                                                                         
*DMPRTQL                                                                        
*FATWA                                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE PEGENPAN                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071PEPAN00   02/25/11'                                      
         END                                                                    
