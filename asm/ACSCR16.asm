*          DATA SET ACSCR16    AT LEVEL 022 AS OF 10/10/19                      
*PHASE T60C16A                                                                  
*&&ONLIN SET   Y                                                                
*INCLUDE LOADER                                                                 
***********************************************************************         
* ID   LVL Date    Ticket     Description                             *         
* ---- --- ------- ---------- ----------------------------------------*         
* JSAY 022 09APR19 SPEC-33734 Relink to new options added in alternate*         
*                             for timesheet approval date.            *         
* JSAY 022 09APR19 SPEC-33735 Relink to new options added in alternate*         
*                             for timesheet submitted date.           *         
***********************************************************************         
         TITLE 'DISPLAY A REQUEST'                                              
T60C16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C16,RA,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         L     RF,=V(LOADER)                                                    
         AR    RF,RE                                                            
         ST    RF,ALOADER                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    TWAMODE,TWAMLSM     CAN ONLY BE IN OVERLAY IN LSM                
         BNZ   SCR10                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         MVC   FVADDR,AACTHDR                                                   
         B     EXIT                                                             
         EJECT ,                                                                
SCR10    CLI   APMODE,APMVALR                                                   
         BNE   SCR50                                                            
         CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APPFKEY,PFKEXIT                                                  
         BNE   SCR50                                                            
         TM    TWASWPST,TWASWAP                                                 
         BZ    EXIT                                                             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE                                               
         MVC   APPARM+1(1),TWASWPAC                                             
         B     EXIT                                                             
*                                                                               
         USING RESRECD,R2                                                       
SCR50    CLI   APMODE,APMVALK                                                   
         BNE   SCR52                                                            
         MVC   SAVPFKEY,APPFKEY                                                 
*                                                                               
SCR52    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALKEY                                       
         B     EXIT                VALREC                                       
         B     DISKEY                                                           
         B     DISREQ                                                           
         B     EXIT                DELETE COLUMN ELEMENT'S ONLY                 
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
         B     EXIT                COPY COLUMN ELEMENT'S                        
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         OI    TWALSCTL,TWALSHLD                                                
EXIT97   OI    TWALSCTL,TWALSRTN                                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   NEWKEY,NO                                                        
         MVI   APGFLAG,NO                                                       
         MVC   IOKEY,APRECKEY                                                   
         LA    R2,IOKEY                                                         
         CLI   RESKSUB,RESKSUBQ    X'02' OR X'07'                               
         BE    *+8                                                              
         MVI   APGFLAG,YES                                                      
         MVC   REQ#,RESKFORM+L'RESKFORM+1       REQUEST NUMBER                  
         MVC   RESKFORM+L'RESKFORM(2),=C'  '    NEED FORMAT'S KEY               
         MVI   APINDS,APIOKDIS                                                  
*                                                                               
         USING PTRELD,R1                                                        
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IORD+IOACCFIL+IO1          READ FORMAT                       
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
*                                                                               
         MVI   APELCODE,PTRELQ                                                  
         GOTO1 GETEL,(R2)          SAVED REQUEST ADDRESS?                       
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    RE,PTRCODE          ADDRESS OF REQUEST                           
         SR    R6,R6                                                            
         IC    R6,REQ#                                                          
*                                                                               
VK100    CHI   R6,63                                                            
         BNH   VK150                                                            
         SHI   R6,63                                                            
         GOTO1 NEXTEL                                                           
         BE    VK100                                                            
         DC    H'0'                                                             
*                                                                               
VK150    LA    RE,PTRCODE          ADDRESS OF REQUEST                           
         SHI   R6,1                                                             
         BZ    *+12                                                             
         LA    RE,4(,RE)                                                        
         BCT   R6,*-4                                                           
         MVC   ADDR,0(RE)          GET REQUEST ADDRESS                          
*                                                                               
         USING ACQD,R3                                                          
         L     R3,AIOAREA3                                                      
         GOTO1 VDMGR,APPARM,(X'20',DMRDIR),REQUEST,ADDR,(R3),DWORK              
         LA    R3,L'ACQCARD1(,R3)                                               
         LA    RE,L'ACQCARD1*3(,R3)  POINT TO 4TH CARD                          
         CLI   ACQCONT3,C'C'                                                    
         BE    VK300                                                            
         MVC   0(L'ACQCARD1,RE),SPACES                                          
         SHI   RE,L'ACQCARD1         POINT TO 3RD CARD                          
         CLI   ACQCONT2,C'C'                                                    
         BE    VK300                                                            
         MVC   0(L'ACQCARD1,RE),SPACES                                          
         SHI   RE,L'ACQCARD1         POINT TO 2RD CARD                          
         CLI   ACQCONT1,C'C'                                                    
         BE    VK300                                                            
         MVC   0(L'ACQCARD1,RE),SPACES                                          
*                                                                               
VK300    CLI   APGFLAG,YES                                                      
         BNE   VK350                                                            
         GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   VK310                                                            
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     VK999                                                            
*                                                                               
VK310    MVC   AAPGIO,0(R1)                                                     
         BAS   RE,GETAPG                                                        
*                                                                               
VK350    BAS   RE,GETRCAP          CHECK FOR RECAP                              
*                                                                               
         L     RF,=A(SCRTABLE)                                                  
         A     RF,APRELO                                                        
*                                                                               
VK460    CLI   0(RF),0                                                          
         BNE   VK463                                                            
         MVC   FVMSGNO,=AL2(2090)  CANNOT LOAD SCREEN                           
         B     VK999               EXIT                                         
*                                                                               
VK463    CLC   APREPJCL,0(RF)                                                   
         BNE   VK465                                                            
         MVI   APBYTE,NO                                                        
         TM    1(RF),REPIAPG                                                    
         BZ    *+8                                                              
         MVI   APBYTE,YES                                                       
         CLC   APGFLAG,APBYTE                                                   
         BE    VK470                                                            
*                                                                               
VK465    LA    RF,4(RF)                                                         
         B     VK460                                                            
*                                                                               
VK470    SR    R2,R2                                                            
         ICM   R2,3,2(RF)                                                       
         A     R2,=A(REPFLDS)                                                   
         A     R2,APRELO                                                        
         ST    R2,ASCRTAB                                                       
*                                                                               
         CLC   APRECKEY(L'RESKEY),SAVRECK    SAME FORMAT ?                      
         BNE   VK800                         NO,  REBUILD   SCREEN              
         TM    INFLAG1,INFDISR               FORCE     DISPLAY   RCD ?          
         BZ    VK999                         NO,  DO   NOT  REBUILD             
*                                                                               
VK800    L     RF,=A(SETSCRN)                REBUILD   SCREEN                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VK999    B     EXIT                                                             
         DROP  R1,R2,R3                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISKEY   MVC   SVSELACT,SCACTN     SAVE SELECT ACTION                           
         MVC   REQFMT,SPACES                                                    
         LA    R2,APRECKEY                                                      
         CLI   RESKSUB,RESKSUBQ    X'02' RECORD?                                
         BNE   *+10                                                             
         MVC   REQFMT,RESKFORM                                                  
*                                                                               
         USING APGRECD,R2                                                       
         CLI   APGKSUB,APGKSUBQ    X'07' RECORD?                                
         BNE   *+10                                                             
         MVC   REQFMT(L'APGKFMT),APGKFMT                                        
         TM    TWASWPST,TWASWAP        DID I COME FROM SWAPPING                 
         BNZ   *+8                                                              
         MVI   SVRECORD,0          NO SO CLEAR SAVE RECORD                      
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISREQ                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISREQ   L     R2,AIOAREA1         DISPLAY RECORD                               
         CLI   APREPJCL,REPJCL1    MANPOWER (PERSON) ?                          
         BNE   DRQ03                                                            
         SR    R4,R4                                                            
         ICM   R4,3,TWAOFFLD       Displacement to office field                 
         AR    R4,R5               Add base of screen                           
         SR    RF,RF                                                            
         IC    RF,0(,R4)                                                        
         AHI   RF,-9                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R4),SPACES                                                   
         EX    RF,*+4                                                           
         MVC   8(0,R4),=CL17'Person office'                                     
         STC   RF,BYTE                                                          
*                                                                               
         USING RPFELD,R1                                                        
         MVI   APELCODE,RPFELQ     PROFILE ELEMENT X'C4'                        
         GOTO1 GETEL,(R2)                                                       
         BNE   DRQ03                                                            
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         CLI   RPFCLIOF,C'C'              (CONTRA SECURITY)                     
         BNE   DRQ02                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R4),=CL17'Contra office'                                     
DRQ02    CLI   RPFCLIOF,C'B'              (BOTH PERSON OR CONTRA)               
         BNE   DRQ03                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R4),=CL17'1R or 1C office'                                   
*                                                                               
DRQ03    L     R3,AIOAREA3         REQUEST DETAILS   HEADER                     
*&&UK                                                                           
         TM    GENIND,GENEURO      DISPLAY 2ND CURRENCY                         
         BO    DRQ04                                                            
         MVI   REQRCU,C' '                                                      
         OI    REQRCUH+6,FVOXMT+X'20'                                           
         MVC   REQRCF,SPACES       WIPE OUT FIELD                               
         OI    REQRCFH+6,FVOXMT                                                 
*&&                                                                             
         USING DMREQHDR,R3         MAP     REQUEST   DETAILS   HDR AREA         
DRQ04    MVC   REQOTYP,SPACES      CLEAR   OUTPUT    TYPE                       
         OI    REQOTYPH+6,FVOXMT   TRANSMIT                                     
         TM    RQHFLAG1,RQHFOUT    ANY     OUTPUT    TYPE REQUESTED ?           
         BZ    *+10                NO,     SKIP                                 
         MVC   REQOTYP,RQHOUT      INSERT  OUTPUT    TYPE                       
*                                                                               
         MVC   REQDES,SPACES       CLEAR   DESTINATION    AREA                  
         OI    REQDESH+6,FVOXMT    TRANSMIT                                     
         TM    RQHFLAG1,RQHFDEST   ANY     DESTINATION ID SPECIFIED ?           
         BZ    DRQ30               NO,     SKIP                                 
         CLC   RQHDEST,RQHORIG     DESTINATION  ID   =    ORIGINAL ID?          
         BE    DRQ30               YES,    SKIP                                 
*                                                                               
         USING CTIREC,R4           CONTROL FILE                                 
         LA    R4,IOKEY            IOKEY   AREA                                 
         XC    IOKEY,IOKEY         CLEAR WHOLE     KEY  AREA                    
         MVI   CTIKEY,CTIKTYPQ     ID      RECORD                               
         MVC   CTIKNUM,RQHDEST     DESTINATION                                  
         GOTO1 AIO,IO2+IOCTFILE+IORD                                            
         BNE   DRQ30               ERROR,  SKIP                                 
*                                                                               
         L     R4,AIOAREA2         ->      CONTROL   RECORD                     
         LA    R4,CTIDATA          FIND    ID   ELEMENT                         
         SR    R0,R0               CLEAR   REGISTER                             
         DROP  R4                                                               
*                                                                               
DRQ10    DS    0H                                                               
         CLI   0(R4),0             NO      ELEMENT   ?                          
         BE    DRQ30               YES,    SKIP                                 
         CLI   0(R4),CTDSCELQ      X'02'   ELEMENT   ?                          
         BE    DRQ20               YES,    INSERT    ELEMENT                    
         IC    R0,1(,R4)           FIND    NEXT      ELEMENT                    
         AR    R4,R0                                                            
         B     DRQ10               TRY     NEXT      ELEMENT                    
*                                                                               
DRQ20    DS    0H                                                               
         MVC   REQDES,2(R4)        INSERT  ID   FROM ELEMENT                    
*                                                                               
DRQ30    DS    0H                                                               
*&&US                                                                           
         MVI   REQOPT2,C'L'        DEFAULT TO   LANDSCAPE ORIENTATION           
         CLI   APGFLAG,YES         APG     ?                                    
         BE    DRQ50               NO,     CONTINUE                             
*                                                                               
         USING REPTABD,R4          REPORT  TYPE DEFINITION                      
         L     R4,ACTYPTAB         ->      TYPE TABLE                           
DRQ40    DS    0H                                                               
         CLI   REPCODE,EOT         END     OF   TABLE     ?                     
         BE    DRQ45               YES,    CLEAR     ORIENTATION                
         MVI   REQOPT2,C'L'        TRY     LANDSCAPE                            
         CLC   REPLDNO,RQHNUMB     DOES    REQUEST   NUM  MATCH ?               
         BE    DRQ50               YES,    CONTINUE                             
         MVI   REQOPT2,C'P'        TRY     PORTRAIT                             
         CLC   REPPTNO,RQHNUMB     DOES    REQUEST   NUM  MATCH ?               
         BE    DRQ50               YES,    CONTINUE                             
         LA    R4,REPLNQ(,R4)      NEXT    TABLE     ENTRY                      
         B     DRQ40                                                            
         DROP  R4                                                               
*                                                                               
DRQ45    DS    0H                                                               
         MVC   REQOPT2,SPACES      CLEAR   ORIENTATION                          
*                                                                               
DRQ50    DS    0H                                                               
         OI    REQOPT2H+6,FVOXMT   TRANSMIT                                     
*&&                                                                             
*                                                                               
*                                  SPACE   LEFT FOR  MORE SPECIAL CASES         
*                                          USING     THE  HDR  AREA             
DRQ70    DS    0H                                                               
         LA    R3,RQHCARD          POINT   TO   REQUEST   DETAILS               
*                                                                               
         USING ACQD,R3                                                          
         MVC   REQPER(L'ACQESTOR),ACQESTOR    REQUESTOR                         
*&&UK                                                                           
         TM    GENIND,GENEURO      DISPLAY 2ND CURRENCY                         
         BZ    DRQ72                                                            
         MVC   REQRCU,ACQCURR                                                   
*&&                                                                             
DRQ72    BAS   RE,SETLIST          SET POSSIBLE +/- LIST                        
         GOTO1 GETNAME,APPARM,AIOAREA1,REQFMTNH                                 
         L     R2,ASCRTAB          R2=REPORT TYPE FIELD TABLE                   
         LA    R6,REQFMTNH         LAST FIELD ON PANGEN SCREEN                  
         BAS   RE,NEXTUNPT         R6=NEXT UNPROTECTED FIELD                    
*                                                                               
         USING REPFD,R2                                                         
         USING RTND,R4             ROUTINES TABLE DSECT                         
DRQ80    CLI   0(R2),X'FF'         EOT?                                         
         BE    DRQ98               NO MORE SCREEN FIELDS, LEAVE                 
         TM    REPFIND1,REPFSTO    STEREO FIELD ONLY?                           
         BZ    *+12                                                             
         TM    GENIND,GENSTRO      STEREO MODE ON?                              
         BZ    DRQ90                                                            
         L     R4,=A(ROUTINES)                                                  
         A     R4,APRELO                                                        
*                                                                               
DRQ85    CLI   0(R4),X'FF'         EOT?                                         
         BE    DRQ90                                                            
         CLC   REPFFLDN,RTNFLDN    MATCH ON FIELD NUMBER?                       
         BE    DRQ88                                                            
         LA    R4,RTNLNQ(R4)       BUMP TO CHECK NEXT ONE                       
         B     DRQ85                                                            
*                                                                               
DRQ88    BAS   RE,FLDCNTL                                                       
         BNE   DRQ90                                                            
         LA    RF,DISROUT                                                       
         ICM   RF,8,RTNDDISP       GET ROUTINE DISPLAY #                        
         BASR  RE,RF               BRANCH TO DISPLAY ROUTINE                    
         BAS   RE,NEXTUNPT         R6=NEXT UNPROTECTED FIELD                    
*                                                                               
DRQ90    LA    R2,REPFLNQ(R2)      BUMP TO NEXT FIELD ON SCREEN                 
         B     DRQ80                                                            
*                                                                               
DRQ98    SR    RE,RE               SET CONCODE TO YES                           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY ROUTINES                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISROUT  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,0(,R6)           FIELD FOR DATA                               
         SHI   R1,9                                                             
         TM    1(R6),FVAXTND                                                    
         BZ    *+8                                                              
         SHI   R1,8                                                             
         STC   R1,FLDXLEN                                                       
         OI    6(R6),FVOXMT        RE-TRANSMIT FIELD                            
         SRL   RF,24               MOVE ROUTINE# TO LOB                         
         SLL   RF,2                TIME 4                                       
         B     *(RF)                                                            
*                                                                               
         B     DISULAC             01 DISPLAY UNIT/LEDGER/ACCOUNT               
         B     DISGENC             02 GENERAL/GENERIC                           
         B     DISSDTE             03 SINGLE DATE                               
         B     DISRDTE             04 DATE RANGE                                
         B     DISSPCL             05 DATE, CONTRA (BILLING SOURCE)             
         B     DISESTS             06 ESTIMATE STATUS                           
         B     DISACCT             07 DISPLAY ACCOUNTS/CONTRA                   
         B     DISFLTR             08 DISPLAY FILTERS 1-5                       
         B     DISTTYP             09 DISPLAY TRANSACTION TYPE                  
         B     DISBUDG             10 DISPLAY APG BUDGETS                       
         B     DISOPTS             11 DISPLAY OPTION STUFF                      
         B     DISMTHD             12 DISPLAY METHOD                            
         B     DIS2DEC             13 DISPLAY CURRENCY OR OVERHEAD              
         B     DISLFJT             14 DISPLAY LEFT JUSTIFY                      
         B     DISRNGE             15 DISPLAY DATE RANGE                        
         B     DISLOCS             16 DISPLAY LOCATIONS STATUS                  
         B     DISPRSC             17 DISPLAY PERSON CODE                       
         B     DISWKCD             18 DISPLAY WORKCODE                          
         B     DISRECAP            19 DISPLAY RECAP                             
         B     DISYNO              20 DISPLAY YES, NO, OR ONLY                  
         B     DISAUTH             21 DISPLAY AUTHORIZATION CODE                
*                                                                               
DISROUTX B     XIT                                                              
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY UNIT/LEDGER/ACCOUNT DATA                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
ELM      USING RFLELD,APELEM                                                    
         SPACE 1                                                                
DISULAC  CLI   RTNFLDN,FLDNUNLG    UNIT/LEDGER                                  
         BNE   DISULA10                                                         
         LR    RF,R6                                                            
         S     RF,ATWA                                                          
         ST    RF,AFLDUNLG         SAVE UNIT/LEDGER FIELD DISP.                 
         CLC   ACQUNT(LUNLG),SPACES                                             
         BH    DISULA10            U/L IN REQUEST CARD                          
         CLI   APGFLAG,YES                                                      
         BNE   DISULA10                                                         
         CLI   LDGR#,1                                                          
         BL    DISULA10                                                         
         XC    APELEM,APELEM                                                    
         MVI   ELM.RFLEL,RFLELQ    X'C5' SIMULATE SCRIBE ELEMENT                
         MVI   ELM.RFLTYPE,RFLLDG  X'01' LEDGER                                 
         SR    RF,RF                                                            
         IC    RF,LDGR#                                                         
         LA    RE,ELM.RFLDATA                                                   
         LA    R1,LDGLIST                                                       
DISULA08 MVC   0(2,RE),0(R1)                                                    
         MVC   2(1,RE),SCCOMMA                                                  
         LA    R1,2(,R1)                                                        
         LA    RE,3(,RE)                                                        
         BCT   RF,DISULA08                                                      
         BCTR  RE,0                                                             
         LA    RF,ELM.RFLEL                                                     
         SR    RE,RF                                                            
         STC   RE,ELM.RFLLN                                                     
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         DROP  ELM                                                              
*                                                                               
DISULA10 SR    R5,R5               ACCOUNT                                      
         TM    REPFIND1,REPFNME                                                 
         BZ    *+10                                                             
         IC    R5,0(,R6)                                                        
         AR    R5,R6                                                            
         BAS   RE,CHKJCL                                                        
         BE    DISULA20            NOTHING IN JCL                               
         TM    0(R1),X'40'         IS   1ST BYTE IN UPPER CASE ?                
         BO    DISULA15            YES, SKIP                                    
         MVI   8(R6),C'*'          NO,  INSERT "*"                              
         EXMVC RF,9(R6),0(R1)      AND  ACCOUNT NUMBER                          
         OI    9(R6),X'40'         MAKE UPPER CASE                              
         B     DISULA40            CONTINUE                                     
*                                                                               
DISULA15 DS    0H                                                               
         EXMVC RF,8(R6),0(R1)      MOVE VALUE TO SCREEN                         
         CLI   8(R6),C'+'                                                       
         BE    DISULA30                                                         
         CLI   8(R6),C'-'                                                       
         BE    DISULA30                                                         
         CLI   8(R6),C'('                                                       
         BE    DISULA25                                                         
         B     DISULA40            GET NAME                                     
*                                                                               
DISULA20 DS    0H                  ALL    SPACES                                
         BAS   RE,CHKRFILL         SHOULD WE REFILL ?                           
         BNE   DISULA90            NO,    SKIP                                  
*                                                                               
DISULA25 DS    0H                  SPACES OR "("                                
         SR    RF,RF                                                            
         ICM   RF,3,RTNGTXNO                                                    
         ST    RF,APPARM+12                                                     
         GOTO1 FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R5)                     
         CLI   APPARM,1            SINGLE ACCOUNT?                              
         BE    DISULA40            YES                                          
         CLI   APPARM,2            +/- LIST                                     
         BNE   DISULA90                                                         
*                                                                               
         USING LSTRECD,R2                                                       
DISULA30 GOTO1 AFVAL,(R6)                                                       
         CLI   RTNFLDN,FLDNUNLG    UNIT/LEDGER?                                 
         BE    DISULA90                                                         
         SR    RF,RF                                                            
         IC    RF,FVILEN           GET  FIELD LENGTH                            
         GOTO1 DISPLIST,APPARM,((RF),FVIFLD),(R5)                               
         B     DISULA90                                                         
*                                                                               
DISULA40 MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(1),CUABIN     COMPANY CODE                                 
         GOTO1 AFVAL,(R6)                                                       
         CLI   RTNFLDN,FLDNUNLG    UNIT/LEDGER?                                 
         BNE   DISULA50                                                         
         MVC   SAVEUNL,FVIFLD                                                   
         MVC   IOKEY+1(LUNLG),FVIFLD                                            
         B     DISULA55                                                         
*                                                                               
DISULA50 L     R2,AFLDUNLG                                                      
         A     R2,ATWA                                                          
         MVC   IOKEY+1(LUNLG),SAVEUNL   MOVE IN U/L                             
         CLC   SAVEUNL,8(R2)                                                    
         BE    DISULA54                                                         
         MVC   IOKEY+1(LUNLG),8(R2)                                             
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   DISULA54                                                         
         SR    RF,RF                                                            
         IC    RF,0(,R2)                                                        
         AR    RF,R2                                                            
         OI    6(RF),FVOXMT             RETRANSMIT                              
         GOTO1 GETNAME,APPARM,AIOAREA2,(RF)                                     
*                                                                               
DISULA54 DS    0H                                                               
         LA    RF,FVIFLD           GET  ACCOUNT NAME                            
         CLI   FVIFLD,C'*'         EXCLUDE ?                                    
         BNE   *+8                 NO,  SKIP                                    
         LA    RF,1(,RF)           YES, POINT PAST THE "*"                      
         MVC   IOKEY+3(LULACNT),0(RF)   MOVE ACCOUNT NAME                       
*                                                                               
DISULA55 GOTO1 AIO,IORD+IOACCFIL+IO2     REFRESH U/L NAME                       
         BNE   DISULA90                                                         
         GOTO1 GETNAME,APPARM,AIOAREA2,(R5)                                     
         OI    6(R5),FVOXMT              RETRANSMIT                             
*                                                                               
DISULA90 B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  GENERIC ROUTINE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT    FIELD    DSECT                     
DISGENC  DS    0H                                                               
         BAS   RE,CHKJCL           SEE  IF   JCL  FLD  HAS  SPACES              
         BE    DISGEN30            YES, TRY  TO   FILL IN   THE  FIELD          
         CLI   0(R1),C'('          PROFILE   LIST ?                             
         BE    DISGEN40            YES, FILL IN   PROFILE                       
*                                                                               
DISGEN10 DS    0H                                                               
         LA    R5,8(,R6)                                                        
         TM    REPFIND1,REPFXLD    R1=  JCL  DATA                               
         BZ    DISGEN20                                                         
         TM    0(R1),X'40'         IS   IT   LOWER     CASE LETTER ?            
         BNZ   DISGEN20                                                         
         MVI   0(R5),C'*'                                                       
         LA    R5,1(,R5)                                                        
         OI    0(R1),X'40'         MAKE UPPER     CASE                          
*                                                                               
DISGEN20 DS    0H                                                               
         EXMVC RF,0(R5),0(R1)                                                   
         B     DISROUTX                                                         
*                                                                               
DISGEN30 DS    0H                                                               
         BAS   RE,CHKRFILL         SHOULD    WE   REFILL ?                      
         BNE   DISROUTX            NO,  SKIP                                    
*                                                                               
DISGEN40 DS    0H                                                               
         SR    R5,R5                                                            
         TM    REPFIND1,REPFNME    DO   WE   HAVE A    NAME FIELD?              
         BZ    DISGEN50                                                         
         IC    R5,0(,R6)           BUMP TO   NEXT FIELD,    NAME FLD            
         AR    R5,R6                                                            
*                                                                               
DISGEN50 DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,RTNGTXNO                                                    
         ST    RF,APPARM+12                                                     
         GOTO1 FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R5)                     
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  SHOW AUTHORIZATION CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DISAUTH  BAS   RE,CHKJCL                                                        
         BE    DISROUTX                                                         
         EXMVC RF,8(R6),1(R1)                                                   
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  SHOW SINGLE DATE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SOFDATD,R1                                                       
DISSDTE  BAS   RE,CHKJCL           SHOW SINGLE DATE                             
         BE    DISROUTX            NO DATA                                      
         LR    R0,R1                                                            
         LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R0,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITSD2                                                
         MVI   SOFIINDS,SOFIIONE                                                
         MVI   SOFOTYPE,SOFOTPRT                                                
         ST    R6,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
         B     DISROUTX            NEXT FIELD                                   
         EJECT ,                                                                
***********************************************************************         
*  SHOW DATE RANGES                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SOFDATD,R1                                                       
DISRDTE  BAS   RE,CHKJCL           SEE     IF    JCL  FIELD HAS  SPACES         
         BE    DISRDTEX            YES,    NEXT  FIELD                          
         LR    R0,R1                                                            
         LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R0,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITSD1                                                
         MVI   SOFOTYPE,SOFOTPRT                                                
         ST    R6,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
*                                                                               
DISRDTEX DS    0H                                                               
         B     DISROUTX            NEXT    FIELD                                
         EJECT ,                                                                
***********************************************************************         
*  SPECIALS - ALTERNATE DATE INFO                                     *         
***********************************************************************         
         SPACE 1                                                                
DISSPCL  MVI   BYTE,ACQDATE        SPECIALS                                     
         CLI   RTNFLDN,FLDNALTS    ALTERNATE START                              
         BE    DISSPC40                                                         
         CLI   RTNFLDN,FLDNALTE    ALTERNATE END                                
         BE    DISSPC50                                                         
         CLI   RTNFLDN,FLDNOCUR    OVER-RIDE CURRENT DATE                       
         BE    DISSPC50                                                         
*&&US                                                                           
*       - - - - - - - - - - - - - -                                             
         CLI   RTNFLDN,FLDNMMOS    Media MOS range                              
         BE    DISSPC50                                                         
*       - - - - - - - - - - - - - -                                             
*&&                                                                             
         CLI   RTNFLDN,FLDNTYPE    ALTERNATE TYPE                               
         BNE   DISROUTX                                                         
*                                                                               
         MVC   TYPETEXT,FTBTYPET   GET  DEFAULT TEXT FOR TYPE FIELD             
         CLI   TYPETEXT,ESCHIGHQ   DEFAULT TEXT A CONSTANT ?                    
         BNL   DISSPC10            YES, SKIP                                    
*                                  GET  DEFAULT TEXT FOR TYPE VALUE             
         GOTO1 VDICTAT,APPARM,C'SL  ',('ADODESCL',TYPETEXT),0                   
*                                                                               
DISSPC10 DS    0H                                                               
         L     RE,ATEXTFLD         GET  OFFSET  OF TYPE FIELD HDR               
         A     RE,ATWA             GET  ADDRESS OF TYPE FIELD HDR               
*                                  MOVE DEFAULT TEXT TO SCREEN                  
         MVC   8(ADODESCL,RE),TYPETEXT                                          
         OI    6(RE),FVOXMT        RE-TRANSMIT THIS FIELD                       
         B     DISSPC50                                                         
*                                                                               
DISSPC40 DS    0H                                                               
*&&UK                                                                           
         OC    ACQEPDST(L'ACQEPDST*2),ACQEPDST                                  
         BZ    DISSPC50                                                         
         GOTO1 GETFFLD,APPARM,(R3)    FIND OPEN SPOT IN REQUEST CARD            
         L     RF,APPARM              DUMMY UP REQUEST CARD                     
         MVI   0(RF),ACQDATE                                                    
         MVI   1(RF),X'01'         BOGUS TYPE FOR EARLY PAYMENT DATE            
         MVC   2(12,RF),ACQEPDST   FILL IN DATES                                
*&&                                                                             
DISSPC50 BAS   RE,FINDSPCL                                                      
         B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  ESTIMATE STATUS DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
DISESTS  BAS   RE,CHKJCL                                                        
         BE    DISROUTX                                                         
         EX    RF,*+8              RF=SET BY CHKJCL (FIELD LENGTH)              
         B     *+10                                                             
         MVC   8(0,R6),0(R1)       R1=SET BY CHKJCL (QCARD AREA)                
         B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY ANALYSIS, BILLING SOURCE, AND CONTRA ACCOUNT DATA          *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DISACCT  MVI   BYTE,ACQANAL        ANALYSIS                                     
         CLI   RTNFLDN,FLDNCNTR                                                 
         BE    *+8                                                              
         CLI   RTNFLDN,FLDNBSRC                                                 
         BNE   *+8                                                              
         MVI   BYTE,ACQCNTR        CONTRA / BILLING SOURCE                      
         BAS   RE,FINDSPCL                                                      
         BNE   DISACT10            CAN'T   FIND IT, CHK  THE  PROFILE           
         CLI   8(R6),C'+'          WAS     IT   A   +    LIST ?                 
         BE    DISROUTX            YES,    DONE                                 
         CLI   8(R6),C'-'          WAS     IT   A   -    LIST ?                 
         BE    DISROUTX            YES,    DONE                                 
         CLI   8(R6),C'('          WAS     IT   MULTIPLE LISTS/ITEMS ?          
         BE    DISROUTX            YES,    DONE                                 
         B     DISACT20            DISPLAY NAME                                 
*                                                                               
DISACT10 DS    0H                  CHECK  THE  PROFILE                          
         BAS   RE,CHKRFILL         SHOULD WE   REFILL ?                         
         BNE   DISROUTX            NO,    SKIP                                  
         SR    R5,R5                                                            
         TM    REPFIND1,REPFNME                                                 
         BZ    *+10                                                             
         IC    R5,0(,R6)           GET FIELD LENGTH                             
         AR    R5,R6               POINT TO POSSIBLE NAME FIELD                 
         SR    RF,RF                                                            
         ICM   RF,3,RTNGTXNO                                                    
         ST    RF,APPARM+12                                                     
         GOTO1 FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R5)                     
         CLI   APPARM,0                                                         
         BE    DISROUTX            NOTHING FOUND                                
         CLI   APPARM,2                                                         
         BNL   DISROUTX            FOUND A LIST OR MULTIPLE ACCOUNTS            
         GOTO1 AFVAL,(R6)          GET  THE VALUES TO FVAREA                    
*                                                                               
DISACT20 TM    REPFIND1,REPFNME    DISPLAY NAME                                 
         BZ    DISROUTX                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(1),CUABIN     COMPANY CODE                                 
*                                                                               
         CLI   RTNFLDN,FLDNPART                                                 
         BE    DISACT30            WHOLE ACCT MUST BE IN SCREEN FIELD           
         CLI   RTNFLDN,FLDNVNDR                                                 
         BE    DISACT30            WHOLE ACCT MUST BE IN SCREEN FIELD           
         CLI   RTNFLDN,FLDNCNTR                                                 
         BE    DISACT29            WHOLE ACCT MUST BE IN SCREEN FIELD           
         L     RF,=A(LEDGLIST)     FIND LEDGER FOR FIELD                        
         A     RF,APRELO                                                        
*                                                                               
DISACT25 CLI   0(RF),X'FF'         EOT?                                         
         BE    DISACT30            ASSUME U/L & ACCT                            
         CLC   RTNFLDN,0(RF)                                                    
         BE    DISACT28                                                         
         LA    RF,3(,RF)                                                        
         B     DISACT25                                                         
*                                                                               
*                                  INSERT UNIT/LEDGER                           
DISACT28 MVC   IOKEY+1(LUNLG),1(RF)                                             
         LA    RF,FVIFLD           POINT TO THE ACCOUNT DATA                    
         CLI   FVIFLD,C'*'         IS    EXCLUDE REQUESTED                      
         BNE   *+8                 NO,   SKIP                                   
         LA    RF,1(,RF)           YES,  POINT TO ACCOUNT DATA                  
         MVC   IOKEY+3(LACCOUNT),0(RF)                                          
         B     DISACT40                                                         
*                                                                               
DISACT29 DS    0H                                                               
         CLI   FVIFLD,C'*'         IS    EXCLUDE REQUESTED ?                    
         BNE   DISACT30            NO,   SKIP                                   
*                                  YES,  INSERT ACCOUNT DATA W/O C'*'           
         MVC   IOKEY+1(LULACNT),FVIFLD+1                                        
         B     DISACT40                                                         
*                                                                               
*                                  INSERT U/L AND ACCOUNT DATA                  
DISACT30 MVC   IOKEY+1(LULACNT),FVIFLD                                          
*                                                                               
DISACT40 DS    0H                                                               
         OC    IOKEY+1(LULACNT),SPACES                                          
         CLI   RTNFLDN,FLDNBSRC    BILLING SOURCE ?                             
         BE    DISROUTX            YES, DO NOT VALIDATE                         
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   DISROUTX                                                         
         ZIC   RF,0(,R6)                                                        
         AR    RF,R6                                                            
         GOTO1 GETNAME,APPARM,AIOAREA2,(RF)                                     
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY FILTERS 1-5 AND CONTRA FILTERS 1-5                         *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DISFLTR  LA    R5,8(,R6)                                                        
         BAS   RE,CHKJCL           SEE IF JCL FIELD HAS SPACES                  
         BE    DISROUTX            IF NOT IN JCL DON'T GET PROF                 
         TM    REPFIND1,REPFXLD                                                 
         BZ    DISFLT35                                                         
         CLI   0(R1),C' '                                                       
         BNH   DISROUTX            NO VALUE AT ALL                              
         TM    0(R1),X'40'         IS IT LOWER CASE LETTER?                     
         BNZ   DISFLT35                                                         
         MVI   0(R5),C'*'                                                       
         LA    R5,1(,R5)                                                        
         OI    0(R1),X'40'         MAKE UPPER CASE                              
*                                                                               
DISFLT35 MVC   0(1,R5),0(R1)                                                    
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY TRANSACTION TYPE                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT    FIELD     DSECT                    
DISTTYP  BAS   RE,CHKJCL           SEE  IF   JCL  FLD  HAS  SPACES              
         BE    DISTTY50            YES, TRY  TO   FILL IN   THE  FIELD          
*                                  R1   IS   SET  BY   CHKJCL TO A(FLD)         
         CLC   0(3,R1),SPACES                                                   
         BE    DISTTYEX            EXIT                                         
         CLI   0(R1),C'('          USE  PROFILE   REQUESTED ?                   
         BE    DISTTY60            FILL IN   PROFILE                            
         LR    R8,R1               SAVE R1                                      
*                                                                               
         USING RFLELD,R5                                                        
         LA    R5,APELEM           DUMMY   ELEMENT   IS   IN   APELEM           
*                                  CLEAR   ELEMENT   AREA                       
         XC    RFLEL(RFLLNQ+1),RFLEL                                            
         MVI   RFLEL,RFLELQ        X'C5'   ELEMENT                              
         MVI   RFLLN,RFLLNQ+1      ELEMENT LENGTH                               
         MVI   RFLTYPE,RFLTTYPE    TRANSACTION  TYPE                            
*                                                                               
         MVI   EXCLUDE,NO          ASSUME  NOT  EXCLUDE                         
         TM    0(R8),X'40'         LOWER   CASE LETTER    ?                     
         BO    DISTTY10            NO,     SKIP                                 
         MVI   EXCLUDE,YES         SET     TO   EXCLUDE                         
         OI    0(R8),X'40'         TEMPORARILY, MAKE UPPER     CASE             
         OI    RFLIND,RFLXCLD      INDICATE     EXCLUDE                         
*                                                                               
DISTTY10 DS    0H                                                               
*&&US                                                                           
         CLC   0(2,R8),=C'M '      MANUAL    BILLING   ?                        
         BNE   DISTTY20            NO,  SKIP                                    
         LA    RE,TY06MN           USE  TRANSACTION    CODE                     
         B     DISTTY30            CONVERT   TO   LANGUAGE  SOFT                
*&&                                                                             
*                                                                               
DISTTY20 DS    0H                                                               
         MVC   WORK(3),0(R8)       INSERT    ALL  3    DIGITS                   
         PACK  DWORK(8),WORK(3)    PACK THE  NUMBER                             
         CVB   RE,DWORK            CONVERT   TO   BINARY                        
*                                                                               
DISTTY30 DS    0H                  CONVERT   TRANSACTION    CODE                
         STC   RE,RFLDATA          SAVE IN   ELEMENT                            
         GOTO1 CNVTTYPE,APPARM,(C'S',APELEM),8(R6)                              
         CLI   EXCLUDE,NO          IS   EXCLUDE   REQUESTED ?                   
         BE    DISTTYEX            NO,  EXIT                                    
         NI    0(R8),TURNOFF-X'40' RESET     TO   LOWER     CASE LETTER         
         B     DISTTYEX            EXIT                                         
*                                                                               
DISTTY50 DS    0H                                                               
         BAS   RE,CHKRFILL         SHOULD    WE   REFILL ?                      
         BNE   DISTTYEX            NO,  SKIP                                    
*                                                                               
DISTTY60 DS    0H                  REFILL    THE  FIELD                         
         SR    R8,R8                                                            
         TM    REPFIND1,REPFNME    DO   WE   HAVE A    NAME FIELD?              
         BZ    DISTTY70                                                         
         ZIC   R8,0(,R6)           BUMP TO   NEXT FIELD,    NAME FLD            
         AR    R8,R6                                                            
*                                                                               
DISTTY70 DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,RTNGTXNO                                                    
         ST    RF,APPARM+12                                                     
         GOTO1 FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R8)                     
*                                                                               
DISTTYEX B     DISROUTX                                                         
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY BUDGET FOR AGP                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DISBUDG  BAS   RE,CHKJCL           SEE IF BUDGETS REQUESTED                     
         SR    RF,RF                                                            
         CLI   0(R1),0                                                          
         BNE   DISBUD10            GET DATA FROM REQUEST CARD                   
         SR    R1,R1                                                            
         ICM   R1,3,REPFDDSP       GET DATA FROM SPECS                          
         SH    R1,=Y(ACQAPPL-ACQD)                                              
         LA    R1,BUDLIST(R1)                                                   
         ICM   RF,1,0(R1)                                                       
         BNZ   DISBUD15                                                         
         B     DISBUDX                                                          
*                                                                               
         USING BUDRECD,R2                                                       
DISBUD10 SR    RF,RF                                                            
         ICM   RF,1,0(R1)                                                       
         BZ    DISBUDX                                                          
         SH    RF,=Y(ESCHIGHQ)     READJUST FOR REAL BUD#                       
*                                                                               
DISBUD15 STH   RF,APHALF                                                        
         LA    R2,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
         MVC   BUDKNO1,APHALF                                                   
         GOTO1 AIO,IO2+IOACCDIR+IOHI                                            
         BL    DISBUDX             IF   HARDWARE ERROR, RETURN                  
         CLC   BUDKNO1,APHALF                                                   
         BNE   DISBUD20                                                         
         SR    RF,RF                                                            
         IC    RF,FLDXLEN          LENGTH OF FIELD                              
         EXMVC RF,8(R6),BUDKCOD                                                 
         B     DISBUDX                                                          
*                                                                               
*                                  THIS    BUDGET    NUMBER   MIGHT             
*                                          NOT  EXIST    BECAUSE IT             
*                                          WAS  OBSOLETED     -                 
*                                  DISPLAY THE  BUDGET   AS   A  NUMBER         
*                                          INSTEAD   OF  AS   A                 
*                                          BUDGET    ID  NAME                   
DISBUD20 DS    0H                                                               
         SR    R0,R0               CLEAR   REGISTER                             
         ICM   R0,3,APHALF         GET     THE  BUDGET   NUMBER                 
         CVD   R0,APDUB            CONVERT IT   TO       PACKED DECIMAL         
         OI    APDUB+7,X'0F'       SETUP   FOR  DISPLAY                         
         UNPK  8(5,R6),APDUB       CONVERT TO   DISPLAY                         
         MVC   8+5(2,R6),=C' ?'    SAY     SOMETHING IS  WRONG                  
*                                                                               
DISBUDX  B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY OPTIONS FOR APG                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DISOPTS  BAS   RE,CHKJCL                                                        
         BE    DISOPTX             JCL IS SPACES                                
         CLI   REPFFLDN,FLDNSELT   SELECT FIELD                                 
         BNE   DISOPT05                                                         
         L     RE,ATEXTFLD                                                      
         A     RE,ATWA                                                          
         GOTO1 DISPFLD,APPARM,(10,(RE))                                         
         L     RE,AREQLOC                                                       
         SR    RF,RF                                                            
         IC    RF,FLDXLEN                                                       
         EXMVC RF,8(R6),0(RE)                                                   
         B     DISOPTX                                                          
*                                                                               
DISOPT05 MVI   APBYTE,1            OPTION 1                                     
         LA    RF,OPT1#                                                         
         LA    RE,OPT1LST                                                       
         CLI   REPFFLDN,FLDNOPT1                                                
         BE    DISOPT10                                                         
         MVI   APBYTE,3            OPTION 3                                     
         LA    RF,OPT3#                                                         
         LA    RE,OPT3LST                                                       
         CLI   REPFFLDN,FLDNOPT3                                                
         BE    DISOPT10                                                         
         MVI   APBYTE,4            OPTION 4                                     
         LA    RF,OPT4#                                                         
         LA    RE,OPT4LST                                                       
         CLI   REPFFLDN,FLDNOPT6                                                
         BNE   DISOPT10                                                         
         CLI   APGFLAG,YES                                                      
         BNE   DISOPTX                                                          
*                                                                               
DISOPT10 MVC   NOPTS,0(RF)          SAVE NUMBER OF OPTIONS                      
         ST    RE,APFULL            SAVE ADDRES OF OPTION LIST                  
         L     RE,ATEXTFLD                                                      
         A     RE,ATWA                                                          
         GOTO1 DISPFLD,APPARM,(APBYTE,(RE))                                     
         SR    R1,R1                                                            
         IC    R1,NOPTS                                                         
         BZ    DISOPTX                                                          
         TM    REPFIND1,REPFNME    IS THERE ROOM ON SCREEN                      
         BZ    DISOPTX             NO SO DON'T BOTHER                           
*                                                                               
         L     RF,APFULL                                                        
         LA    RE,APWORK                                                        
*                                                                               
DISOPT12 CLI   0(RE),C' '                                                       
         BNE   DISOPT15                                                         
         MVC   0(6,RF),=C'SPACE,'                                               
         LA    RF,6(RF)                                                         
         B     DISOPT18                                                         
*                                                                               
DISOPT15 MVC   0(1,RF),0(RE)                                                    
         MVC   1(1,RE),SCCOMMA                                                  
         LA    RF,2(,RF)                                                        
*                                                                               
DISOPT18 LA    RE,1(,RE)                                                        
         BCT   R1,DISOPT12                                                      
*                                                                               
         BCTR  RF,0                                                             
         LA    R1,APWORK                                                        
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         SR    R5,R5               POINT TO NAME FIELD                          
         IC    R5,0(,R6)                                                        
         AR    R5,R6                                                            
         AR    R1,R1                                                            
         IC    R1,0(,R5)                                                        
         SH    R1,=H'09'           GET LENGHT OF FIELD                          
         TM    1(R5),FVAXTND                                                    
         BZ    *+8                                                              
         SH    R1,=H'08'                                                        
         CR    R1,RF                                                            
         BNL   *+6                                                              
         LR    RF,R1                                                            
         EXMVC RF,8(R5),APWORK                                                  
*                                                                               
DISOPTX  B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY METHOD NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CAHRECD,R5                                                       
DISMTHD  DS    0H                                                               
         LA    R5,IOKEY                                                         
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ     X'3E'                                       
         MVI   CAHKSUB,CAHKSUBQ     X'01'                                       
         MVC   CAHKCPY,CUABIN       COMPANY                                     
         MVI   CAHKMTHD,C'1'        DEFAULT METHOD                              
         XC    CAHKOFC,CAHKOFC                                                  
*                                                                               
         BAS   RE,CHKJCL                                                        
         BE    DISMTHD5            JCL = SPACES                                 
         MVC   CAHKMTHD,ACQMTHD                                                 
*                                                                               
DISMTHD5 DS    0H                                                               
         GOTO1 AIO,IO2+IOACCFIL+IORD                                            
         BNE   DISMTHDX                                                         
*                                                                               
         USING METELD,R1                                                        
         L     R1,AIOAREA2                                                      
         MVI   APELCODE,METELQ     X'82'                                        
         GOTO1 GETEL                                                            
         BNE   DISMTHDX                                                         
         MVC   8(3,R6),METCODE     MOVE IN CODE                                 
*                                                                               
DISMTHDX B     DISROUTX                                                         
         DROP  R1,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY SQUISHED 2 DECIMAL NUMBER                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DIS2DEC  DS    0H                                                               
         CLI   REPFFLDN,FLDNOVHD   OVERHEAD                                     
         BNE   DIS2DC10            NO,  SKIP                                    
         MVI   BYTE,ACQOVHD        LOOK FOR  OVERHEAD  ENTRY                    
         BAS   RE,FINDSPCL         FIND IT                                      
         BNE   DIS2DECX            NOT  FOUND,    EXIT                          
         B     DIS2DC20            CONTINUE                                     
*                                                                               
DIS2DC10 DS    0H                                                               
         BAS   RE,CHKJCL                                                        
         BE    DIS2DECX            NO DATA                                      
*                                                                               
DIS2DC20 DS    0H                                                               
         L     RE,AREQLOC          REQUEST CARD LOCATION                        
         MVC   APWORK,SPACES                                                    
         CLI   REPFFLDN,FLDNCURC   CURRENCY                                     
         BNE   *+8                                                              
         LA    RF,1                # OF DIGITS (NON-DEC)                        
         CLI   REPFFLDN,FLDNOVHD   OVERHEAD                                     
         BNE   *+8                                                              
         LA    RF,3                # OF DIGITS (NON-DEC)                        
         LR    R1,RF               MOVE DIGIT INTO APWORK                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   APWORK(0),0(RE)                                                  
         LA    R1,APWORK(RF)       POINT TO WHERE DEC GOES                      
         MVI   0(R1),C'.'                                                       
         LA    R1,1(,R1)           POINT TO WHERE DEC NUMBERS GO                
         AR    RE,RF                                                            
         MVC   0(2,R1),0(RE)       MOVE IN 2 DECIMAL PART                       
         LA    RE,APWORK           DON'T PRINT LEADING ZERO'S                   
         CLI   0(RE),C'0'          LEFT JUSTIFY                                 
         BNE   *+12                                                             
         LA    RE,1(,RE)                                                        
         BCT   RF,*-12                                                          
         SR    R1,R1               MOVE TO SCREEN                               
         IC    R1,FLDXLEN                                                       
         EXMVC R1,8(R6),0(RE)                                                   
*                                                                               
DIS2DECX B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY DATA LEFT JUSTIFY                                          *         
***********************************************************************         
         SPACE 1                                                                
DISLFJT  BAS   RE,CHKJCL           SEE IF JCL FIELD HAS SPACES                  
         BE    DISLFJTX            YES, NEXT FIELD                              
         L     RE,AREQLOC                                                       
         MVC   APWORK,SPACES                                                    
         EX    RF,*+4                                                           
         MVC   APWORK(0),0(RE)                                                  
         LA    RE,APWORK                                                        
         CLI   0(RE),C' '                                                       
         BNE   *+12                                                             
         LA    RE,1(,RE)                                                        
         BCT   RF,*-12                                                          
         EX    RF,*+4                                                           
         MVC   8(0,R6),0(RE)                                                    
*                                                                               
DISLFJTX B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY LOCATION STATUS                                            *         
***********************************************************************         
         SPACE 1                                                                
DISLOCS  BAS   RE,CHKJCL           SEE IF JCL FIELD HAS SPACES                  
         BE    DISLOCSX            YES, NEXT FIELD                              
         L     R8,AREQLOC                                                       
         MVC   APWORK,SPACES                                                    
         MVC   APBYTE,0(R8)                                                     
         OI    APBYTE,X'40'        FORCE UPPER CASE                             
         L     R2,ACLOSTAB         LOCATION STATUS TABLE                        
*                                                                               
DISLOC10 DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   0(1,R2),APBYTE      FOUND MATCH ?                                
         BE    DISLOC20                                                         
         LA    R2,LLOSTATU+1(,R2)  NEXT TABLE ENTRY                             
         B     DISLOC10                                                         
*                                                                               
DISLOC20 DS    0H                                                               
         MVC   LOSTATUS,1(R2)      MOVE LOCATION STATUS TO WORK AREA            
         CLI   LOSTATUS,ESCHIGHQ   LOCATION STATUS A CONSTANT ?                 
         BNL   DISLOC30            YES, SKIP                                    
*                                  GET  LOCATION STATUS VALUE                   
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
*                                                                               
DISLOC30 DS    0H                                                               
         LA    RF,8(,R6)                                                        
*                                  CLEAR SCREEN FIELD                           
         MVC   0(LENFLOCS,RF),SPACES                                            
         TM    0(R8),X'40'         SHOULD WE EXCLUDE?                           
         BO    DISLOC40            NO                                           
         MVI   0(RF),C'*'          PREFIX WITH X'*' TO INDICATE EXCLUDE         
         LA    RF,9(,R6)                                                        
*                                                                               
DISLOC40 DS    0H                                                               
*                                  INSERT THE LOCATION STATUS                   
         MVC   0(LLOSTATU,RF),LOSTATUS                                          
*                                                                               
DISLOCSX DS    0H                  EXIT                                         
         B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY DATE RANGE WHERE THE DATES ARE IN ACQSTART                 *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
DISRNGE  BAS   RE,CHKJCL           SEE     IF   JCL  FIELD HAS  SPACES          
         BE    DISRNGEX            YES,    NEXT FIELD                           
*                                                                               
         LR    R8,R1                                                            
         CLC   0(L'ACQSTART,R8),SPACES                                          
         BE    DISRNGE2                                                         
         CLC   4(2,R8),SPACES                                                   
         BE    DISRNGE4                                                         
         B     DISRNGE6                                                         
*                                                                               
DISRNGE2 CLC   L'ACQSTART+4(2,R8),SPACES                                        
         BNE   DISRNGE6                                                         
*                                                                               
DISRNGE4 CLI   REPFFLDN,FLDNPRDR                                                
         BE    DISRNGEX                                                         
         B     DISRNGE8                                                         
*                                                                               
DISRNGE6 CLI   REPFFLDN,FLDNPRDR                                                
         BNE   DISRNGEX                                                         
*                                                                               
         USING SOFDATD,R1                                                       
DISRNGE8 LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R8,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITSD2                                                
         MVI   SOFOTYPE,SOFOTPRT                                                
         ST    R6,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
*                                                                               
DISRNGEX DS    0H                                                               
         B     DISROUTX            EXIT                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY PERSON CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING PERRECD,R1                                                       
         USING REPFD,R2                                                         
DISPRSC  MVI   BYTE,ACQPRSN                                                     
         BAS   RE,FINDSPCL                                                      
         BNE   DISPRSCX                                                         
         TM    REPFIND1,REPFNME                                                 
         BZ    DISPRSCX                                                         
         LA    R1,IOKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,CUABIN                                                   
         MVC   PERKCODE,FVIFLD                                                  
         GOTO1 AIO,IO2+IOACCFIL+IORD                                            
         BNE   DISPRSCX                                                         
*                                                                               
         USING GPNELD,R1                                                        
         L     R1,AIOAREA2                                                      
         AH    R1,DATADISP                                                      
         LA    RE,APWORK                                                        
         MVC   APWORK,SPACES                                                    
*                                                                               
DISPRS10 CLI   0(R1),0                                                          
         BE    DISPRS40                                                         
         CLI   0(R1),GPNELQ        X'5A'                                        
         BNE   DISPRS20                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         AHI   RF,-(GPNLNQ+1)                                                   
         BM    DISPRS20                                                         
         EXMVC RF,0(RE),GPNNME                                                  
         LA    RE,1(RE,RF)                                                      
         MVC   0(1,RE),SCCOMMA                                                  
         LA    RE,1(,RE)                                                        
*                                                                               
DISPRS20 SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     DISPRS10                                                         
*                                                                               
DISPRS40 BCTR  RE,0                SUBTRACT ONE                                 
         MVI   0(RE),C' '          BLANK OUT LAST COMMA                         
         LA    RF,APWORK                                                        
         SR    RE,RF                                                            
         BM    DISPRSCX                                                         
         ST    RE,APPARM+4         LENGTH OF FIRST/LAST NAME                    
         GOTO1 VSQUASH,APPARM,APWORK                                            
         L     RF,APPARM+4         LENGTH AFTER SQUISHING                       
         AHI   RF,-1                                                            
         BM    DISPRSCX            NO NAME                                      
         SR    R1,R1                                                            
         IC    R1,0(,R6)           BUMP TO NEXT FIELD                           
         AR    R6,R1                                                            
         IC    R1,0(,R6)           NAME FIELD                                   
         AHI   R1,-9                                                            
         TM    1(R6),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         CR    R1,RF                                                            
         BNL   *+6                                                              
         LR    RF,R1                                                            
         EX    RF,*+4                                                           
         MVC   8(0,R6),APWORK                                                   
         OI    6(R6),FVOXMT                                                     
*                                                                               
DISPRSCX B     DISROUTX                                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY WORKCODE OR LIST                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PERRECD,R1                                                       
         USING REPFD,R2                                                         
DISWKCD  DS    0H                                                               
         BAS   RE,CHKJCL           SEE IF JCL FIELD HAS SPACES                  
         BE    DISWKC30            YES, TRY TO FILL IN THE FIELD                
         CLI   0(R1),C'('          STRING LIST ?                                
         BE    DISWKC40            YES, TRY TO FILL IN THE FIELD                
         L     RE,AREQLOC                                                       
         LA    RF,LENFWKCD-1       MAX  LEN OF WORK CODE FIELD - 1              
         LA    R1,8(,R6)                                                        
         MVI   EXCLUDE,NO          ASSUME NOT EXCLUDE                           
         TM    0(RE),X'40'         TEST FOR LOWER CASE - EXCLUDE BIT            
         BO    DISWKC10            NOT  EXCLUDE, SKIP                           
         MVI   EXCLUDE,YES         SET  EXCLUDE  TO YES                         
         LA    RF,L'WCOKWRK-1      MAX  LENGTH OF A WORK CODE - 1               
         LA    R1,9(,R6)                                                        
         MVI   8(R6),C'*'                                                       
         OI    0(RE),X'40'                                                      
*                                                                               
DISWKC10 DS    0H                                                               
         EXMVC RF,0(R1),0(RE)      INSERT THE DATA                              
         GOTO1 AFVAL,(R6)                                                       
         BNE   DISWKCDX                                                         
*                                                                               
         L     RE,AREQLOC          RELOAD A(DATA IN REQ CARD)                   
         TM    REPFIND1,REPFNME                                                 
         BZ    DISWKCDX                                                         
         SR    R5,R5                                                            
         IC    R5,0(,R6)                                                        
         AR    R5,R6                                                            
         CLI   0(RE),C'+'          MUST BE A LIST                               
         BE    *+8                                                              
         CLI   0(RE),C'-'                                                       
         BNE   DISWKC20                                                         
         GOTO1 DISPLIST,TMPPARM,(FVILEN,(RE)),(R5)                              
         B     DISWKCDX                                                         
*                                                                               
         USING WCORECD,R1                                                       
DISWKC20 DS    0H                                                               
         LA    R1,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A'                                        
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(2),=C'SJ'                                                
         LA    RE,FVIFLD           ->   WORK CODE                               
         CLI   EXCLUDE,C'*'        IS   EXCLUDE ON ?                            
         BNE   *+8                 NO,  SKIP                                    
         LA    RE,FVIFLD+1         ->   WORK CODE                               
         MVC   WCOKWRK(2),0(RE)                                                 
         CLC   WCOKWRK(2),=C'99'   SPECIAL WORKCODE ?                           
         BE    DISWKCDX            YES, DON'T BOTHER TO GET NAME                
         CLC   WCOKWRK(2),=C'**'   SPECIAL WORKCODE ?                           
         BE    DISWKCDX            YES, DON'T BOTHER TO GET NAME                
         GOTO1 AIO,IO2+IOACCFIL+IORD                                            
         BNE   DISWKCDX                                                         
         GOTO1 GETNAME,TMPPARM,AIOAREA2,(R5)                                    
         B     DISWKCDX            EXIT                                         
*                                                                               
DISWKC30 DS    0H                  REFILL THE  FLD  FROM THE  PROFILE           
         BAS   RE,CHKRFILL         SHOULD WE   REFILL ?                         
         BNE   DISWKCDX            NO,    SKIP                                  
*                                                                               
DISWKC40 DS    0H                  REFILL THE  FIELD                            
         SR    R8,R8                                                            
         TM    REPFIND1,REPFNME    DO     WE   HAVE A    NAME FIELD ?           
         BZ    DISWKC50                                                         
         ZIC   R8,0(,R6)           BUMP   TO   NEXT FIELD,    NAME FLD          
         AR    R8,R6                                                            
*                                                                               
DISWKC50 DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,RTNGTXNO                                                    
         ST    RF,APPARM+12                                                     
         GOTO1 FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R8)                     
*                                                                               
DISWKCDX DS    0H                                                               
         B     DISROUTX                                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECAP FIELD                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT FIELD    DSECT                        
DISRECAP DS    0H                                                               
         LH    R5,REPFDDSP         GET    REPORT   NUMBER                       
         LA    R5,RCAPACQT(R5)     ->     RECAP    ACQTYP1  FIELD               
         BCTR  R5,0                                                             
         MVC   8(1,R6),APNO        ASSUME NOT      RECAP                        
         CLI   0(R5),C'N'          DON'T  RECAP    REQUESTED ?                  
         BE    DISRCPEX            YES,   EXIT                                  
         MVC   8(1,R6),APYES       ELSE   ASSUME   RECAP    REQUESTED           
*                                                                               
DISRCPEX DS    0H                  EXIT                                         
         B     DISROUTX            RETURN                                       
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY YES, NO, OR ONLY FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2            REPORT    FIELD     DSECT                    
DISYNO   BAS   RE,CHKJCL           SEE  IF   JCL  FIELD     HAS  SPACES         
         BE    DISYNOEX            YES, EXIT                                    
         ZIC   RE,RTNFLDLN         GET  LENGTH    OF   FIELD                    
         CLI   RTNFLDLN,1          IS   LENGTH    OF   FIELD     = 1 ?          
         BNE   DISYNO50            NO,  JUST MOVE THE  FIELD                    
         CLI   0(R1),C'Y'          JCL  HAS  'Y' ?                              
         BNE   DISYNO10                                                         
         MVC   8(1,R6),APYES       "Yes"                                        
         B     DISYNOEX                                                         
*                                                                               
DISYNO10 CLI   0(R1),C'N'          JCL  HAS  'N' ?                              
         BNE   DISYNO20                                                         
         MVC   8(1,R6),APNO        "No"                                         
         B     DISYNOEX                                                         
*                                                                               
DISYNO20 CLI   0(R1),C'O'          JCL  HAS  'O' ?                              
         BNE   DISYNO50                                                         
         MVC   8(1,R6),APONLY      "Only"                                       
         B     DISYNOEX                                                         
*                                                                               
DISYNO50 BCTR  RE,0                SUBTRACT  ONE  FOR  EXECUTE                  
         EXMVC RE,8(R6),0(R1)      MOVE THE  DATA                               
*                                                                               
DISYNOEX DS    0H                  EXIT                                         
         B     DISROUTX            RETURN                                       
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY DESCRIPTION OF FIELD SORTED IN X'B0' ELEMENT               *         
***********************************************************************         
         SPACE 1                                                                
         USING TFDELD,R1                                                        
DISPFLD  NTR1                                                                   
         MVC   APBYTE,0(R1)                                                     
         L     R5,0(,R1)                                                        
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,TFDELQ     X'B0' TEXT FIELD ELEMENT                     
         GOTO1 GETEL                                                            
*                                                                               
DISPFLD2 BNE   DISPFLDX                                                         
         CLC   TFDSEQ,APBYTE       MATCH SEQUENCE NUMBER                        
         BE    DISPFLD5            GOT THE ONE FOR THIS FIELD                   
         GOTO1 NEXTEL                                                           
         B     DISPFLD2                                                         
*                                                                               
DISPFLD5 SR    RF,RF                                                            
         IC    RF,TFDLN            LEN OF DESCRIPTION                           
         SHI   RF,TFDTEXT-TFDELD                                                
         SR    RE,RE                                                            
         IC    RE,0(,R5)                                                        
         SHI   RE,8                                                             
         TM    1(R5),FVAXTND       EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SHI   RE,8                                                             
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                SUBTRACT ONE                                 
         EXMVC RF,8(R5),TFDTEXT                                                 
*                                                                               
DISPFLDX B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  CHKJCL - SEE IF JCL HAS SPACES FOR CURRENT FIELD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
CHKJCL   SR    R1,R1                                                            
         ICM   R1,3,REPFDDSP       GET DATA DISPL FROM CARD                     
         AR    R1,R3                                                            
         ST    R1,AREQLOC                                                       
         SR    RF,RF                                                            
         IC    RF,RTNFLDLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         CLC   0(0,R1),SPACES      SEE IF THERE WAS SPACES                      
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  FINDSPCL - FIND DATA IN CARDS 3 & 4                                *         
*                                                                     *         
*  USED TO FIND:                                                      *         
*          ANALYSIS DATA                                              *         
*          CONTRA ACCOUNT                                             *         
*          BILLING SOURCE                                             *         
*          ALTERNATE DATES                                            *         
*       - - - - - - - - - - - - - -                                             
*          MEDIA MOS DATE RANGE                                       *         
*       - - - - - - - - - - - - - -                                             
*          PERSON CODES                                               *         
*          OVERHEAD DATA                                              *         
*          UK SPECIAL ALTERNATIVE ALTERNATE DATES                     *         
*          RECAP DATA                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
FINDSPCL NTR1                                                                   
         LA    RF,2                LOOP TROUGH FOR 3RD AND 4TH CARD             
         LA    R0,4                UP TO FOUR FIELDS TO CHECK                   
         LA    R1,ACQTYP1          POINT TO 3RD CARD                            
*                                                                               
FS010    CLC   BYTE,0(R1)                                                       
         BNE   FS018                                                            
         CLI   BYTE,ACQDATE        A DATE?                                      
         BE    FS028                                                            
         CLI   BYTE,ACQPRSN        PERSON CODE?                                 
         BE    FS042                                                            
         CLI   BYTE,ACQCNTR        CONTRA ACCOUNT or BILLING SOURCE?            
         BE    FS045                                                            
         CLI   BYTE,ACQOVHD        OVERHEAD DATA?                               
         BE    FS025                                                            
         CLI   BYTE,ACQRCAP        RECAP DATA?                                  
         BE    FS050                                                            
*                                                                               
         CLI   1(R1),C'+'          IS IT A + LIST?                              
         BE    FS020                                                            
         CLI   1(R1),C'-'          IS IT A - LIST?                              
         BE    FS020                                                            
         CLI   1(R1),C'('          IS IT A PROFILE LIST ?                       
         BE    FS047                                                            
*                                  ************************************         
*                                  * ANALYSIS                         *         
*                                  ************************************         
         L     R8,=A(LEDGLIST)                                                  
         A     R8,APRELO                                                        
*                                                                               
FS012    CLI   0(R8),X'FF'         END OF LIST                                  
         BE    FS018               NOT ONE OF THESE                             
         CLC   RTNFLDN,0(R8)                                                    
         BNE   FS015                                                            
         CLC   1(2,R1),1(R8)       MATCH LEDGERS                                
         BE    FS060                                                            
         CLI   RTNFLDN,FLDNPART    PARTICIPANT?                                 
         BNE   FS015               TRY AGAIN                                    
         CLC   1(1,R1),1(R8)       JUST CHECK UNIT                              
         BE    FS060                                                            
*                                                                               
FS015    LA    R8,3(,R8)                                                        
         B     FS012                                                            
*                                                                               
FS018    LA    R1,L'ACQTYP1+L'ACQFLT1(,R1)                                      
         BCT   R0,FS010                                                         
         LA    R0,4                RESET TO 4 FIELDS                            
         LA    R1,ACQTYP5          POINT TO 4TH CARD                            
         BCT   RF,FS010                                                         
         B     FS090               CAN'T FIND A MATCH                           
*                                                                               
*                                  ************************************         
FS020    DS    0H                  * +/- LIST                         *         
*                                  ************************************         
         USING LISTD,R8                                                         
         SR    R8,R8                                                            
         IC    R8,14(,R1)          GET STORED ARRAY NUMBER                      
         BCTR  R8,0                MINUS ONE                                    
         MHI   R8,LISTLNQ                                                       
         LA    R8,LISTBLK(R8)      POINT INTO  BLOCK                            
         CLC   RTNFLDN,LISTTYP     MATCH FIELD NUMBER                           
         BNE   FS018               TRY AGAIN                                    
*                                                                               
*                                  FOUND VALID +/- LIST                         
         B     FS055               GOTO COMMON +/- LIST PROCESSING              
         DROP  R8                                                               
*                                                                               
*                                  ************************************         
FS025    DS    0H                  * OVERHEAD DATA                    *         
*                                  ************************************         
*                                  ->   OVERHEAD  DATA                          
         LA    R1,ACQFLT1-ACQTYP1(,R1)                                          
         ST    R1,AREQLOC          SAVE DATA ADDRESS                            
         SR    RE,RE               SAY  GOT  THE  DATA                          
         B     FS090               EXIT                                         
*                                                                               
*                                  ************************************         
FS028    DS    0H                  * ALTERNATE DATE, MEDIA MOS        *         
*                                  ************************************         
*&&US                                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -               
         CLI   1(R1),ACQDTMOS      Is it a MEDIA MOS Date field?                
         BNE   FS030               No                                           
         LA    RF,2(R1)                                                         
         CLI   RTNFLDN,FLDNMMOS    MEDIA MOS field?                             
         BE    FS039               Yes, have field and data                     
         B     FS090               Wasn't it                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -               
*&&                                                                             
FS030    SR    RF,RF                                                            
         ICM   RF,3,REPFDDSP       Get data disp form card                      
         AR    RF,R1                                                            
         CLI   1(R1),ACQDTREL      Is it an OVER-RIDE Date field?               
         BNE   FS031               No                                           
         CLI   RTNFLDN,FLDNOCUR    Yes, not a part of option table              
         BE    FS038               Yes, have field and data                     
         B     FS080               Wasn't it                                    
*                                                                               
FS031    CLI   RTNFLDN,FLDNOCUR    ARE WE PROCESSING OVER-RIDE FIELD ?          
         BE    FS080                YES, EXIT, NOT VALID AT THIS POINT          
         CLI   RTNFLDN,FLDNTYPE    Date Type Field?                             
         BNE   FS038                No                                          
*                                                                               
         L     R5,=A(ADOPTTAB)     ALTERNATE DATE OPTIONS TABLE                 
         A     R5,APRELO           RELOCATE                                     
*                                                                               
         USING ADOPTD,R5           ALTERNATE DATE OPTIONS DSECT                 
FS032    DS    0H                                                               
         CLI   0(R5),EOT           END OF TABLE ?                               
         BNE   *+6                 NO, CONTINUE                                 
         DC    H'0'                YES, ABEND                                   
         CLC   ADODTYPE,0(RF)      MATCHING DATE TYPE ?                         
         BE    FS034               YES, CONTINUE                                
         LA    R5,ADOPTLNQ(,R5)    GET NEXT ENTRY                               
         B     FS032               TEST NEXT ENTRY                              
*                                                                               
FS034    DS    0H                                                               
         ZIC   R1,CULANG           GET    CURRENT   LANGUAGE                    
         AHI   R1,-1               MINUS  ONE                                   
         BNM   *+6                 LANGUAGE    =    ENG  AND  ZERO ?            
         DC    H'0'                YES,   ABEND                                 
*                                  LANGUAGE    NUM  >    14   ?                 
         CHI   R1,(L'ADOLCODE-1)                                                
         BNH   *+6                 NO,    CONTINUE                              
         DC    H'0'                YES,   ABEND                                 
         LA    R1,ADOLCODE(R1)     ->     CODE IN   USER'S    LANGUAGE          
         MVC   8(1,R6),0(R1)       INSERT CODE FOR  OPTION                      
*                                                                               
         MVC   TYPETEXT,ADODESCR   GET    DESCRIPTION    TEXT                   
         CLI   TYPETEXT,ESCHIGHQ   DESCRIPTION TEXT A    CONSTANT ?             
         BNL   FS036               YES,   SKIP                                  
*                                  GET    DESCRIPTION    TEXT VALUE             
         GOTO1 VDICTAT,APPARM,C'SL  ',('ADODESCL',TYPETEXT),0                   
*                                                                               
FS036    DS    0H                                                               
         L     RE,ATEXTFLD         GET    OFFSET    OF   TYPE FIELD HDR         
         A     RE,ATWA             GET    ADDRESS   OF   TYPE FIELD HDR         
*                                  MOVE   DESCRIPTION    TEXT TO SCREEN         
         MVC   8(ADODESCL,RE),TYPETEXT                                          
         OI    6(RE),FVOXMT        RE-TRANSMIT THIS FIELD                       
         B     FS080               EXIT                                         
         DROP  R5                                                               
*                                                                               
FS038    DS    0H                                                               
         CLI   RTNFLDN,FLDNALTS    MUST  BE AN ALTERNATE TYPE                   
         BE    FS039                                                            
         CLI   RTNFLDN,FLDNALTE                                                 
         BE    FS039                                                            
         CLI   RTNFLDN,FLDNOCUR    MUST  BE AN ALTERNATE TYPE                   
         BNE   FS090               WHAT WAS THAT?                               
*                                                                               
         USING SOFDATD,R1                                                       
FS039    LR    R0,RF                                                            
         LA    R1,SOFBLOCK                                                      
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R0,SOFAINP                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVC   SOFLANG,CULANG                                                   
         MVC   SOFACFST,FISCALMO                                                
         MVI   SOFITYPE,SOFITSD2                                                
         MVI   SOFIINDS,SOFIIONE                                                
         MVI   SOFOTYPE,SOFOTPRT                                                
*&&US                                                                           
* - - - - - - - - - - - - - - - - - -                                           
         CLI   RTNFLDN,FLDNMMOS                                                 
         BNE   FS040                                                            
         OI    SOFOTYPE,SOFOTPYM                                                
         MVI   SOFIINDS,SOFIIANY                                                
* - - - - - - - - - - - - - - - - - -                                           
*&&                                                                             
FS040    ST    R6,SOFAOUT                                                       
         GOTO1 ASOFDAT                                                          
         B     FS080                                                            
*                                                                               
*                                  ************************************         
FS042    DS    0H                  * PERSON CODE                      *         
*                                  ************************************         
         LA    RE,7                LENGTH OF PERSON CODE                        
         LA    RF,1(,R1)                                                        
         B     FS070                                                            
*                                  ************************************         
FS045    DS    0H                  * BILLING SOURCE AND CONTRA ACCOUNT*         
*                                  ************************************         
         CLI   1(R1),C'+'          IS      THIS A   +LIST ?                     
         BE    FS055               YES,    PROCESS   LIST                       
         CLI   1(R1),C'-'          IS      THIS A   -LIST ?                     
         BE    FS055               YES,    PROCESS   LIST                       
*                                                                               
         CLI   1(R1),C'('          SHOULD  WE   TAKE FROM RECORD ?              
         BNE   FS060               NO,     PROCESS   ITEM                       
*                                                                               
*                                  ************************************         
FS047    DS    0H                  * PROFILE LIST                     *         
*                                  ************************************         
*                                  GET     DATA FROM RECORD                     
         SR    R5,R5               CLEAR   REGISTER                             
         TM    REPFIND1,REPFNME    DETAIL  FLD  AVAILABLE ?                     
         BZ    *+10                NO,     SKIP                                 
         IC    R5,0(,R6)           GET     BASE FLD  LENGTH                     
         AR    R5,R6               POINT   TO   DETAIL    FIELD                 
         SR    RF,RF               CLEAR   REGISTER                             
         ICM   RF,3,RTNGTXNO       GET     GETTEXT   NUMBER                     
         ST    RF,APPARM+12        SAVE    IN   PARM LIST                       
*                                                                               
*                                  GET     DATA FROM RECORD                     
         GOTO1 FILLFLD,APPARM,AIOAREA1,(RTNPROFN,(R6)),(R5)                     
*                                                                               
         CLI   APPARM,0            ANYTHING     FOUND ?                         
         BE    FS090               NO,   EXIT   WITHOUT   DATA                  
*                                  YES,  FILLFLD    INSERTED FIELD,             
         B     FS080               SO    PROCESSING COMPLETE                    
*                                                                               
*                                  ************************************         
FS050    DS    0H                  * RECAP CODE                       *         
*                                  ************************************         
*                                  ->      RECAP DATA                           
         LA    R1,ACQFLT1-ACQTYP1+1(,R1)                                        
         ST    R1,AREQLOC          SAVE    DATA  ADDRESS                        
         SR    RE,RE               SAY     GOT   THE  DATA                      
         B     FS090               EXIT                                         
*                                                                               
*                                  ************************************         
FS055    DS    0H                  * COMMON +/- LIST PROCESSING       *         
*                                  ************************************         
         LR    R8,R1               SAVE    R1                                   
         SR    R5,R5               CLEAR   R5                                   
         TM    REPFIND1,REPFNME    DETAIL  FLD  AVAILABLE ?                     
         BZ    *+10                NO,     SKIP                                 
         IC    R5,0(,R6)           GET     BASE FLD  LENGTH                     
         AR    R5,R6               POINT   TO   DETAIL    FIELD                 
*                                  INSERT  LIST NAME                            
         GOTO1 DISPLIST,APPARM,(6,1(R8)),(R5)                                   
         LR    R1,R8               RESTORE R1                                   
         LA    RE,LLIST            MAXIMUM SIZE OF   +/-  LIST                  
         LA    RF,1(,R1)           ADDR    OF   +/-  LIST                       
         B     FS070               INSERT  NAME INTO FIELD                      
*                                                                               
*                                  ************************************         
FS060    DS    0H                  * COMMON ITEM PROCESSING           *         
*                                  ************************************         
         LA    RF,ACQFLT1-ACQTYP1(,R1)  POINT TO UNIT/LEDGER/ACCT               
         LA    RE,L'ACQFLT1-1      MAX  EXMVC LENGTH                            
         CLI   RTNFLDN,FLDNBSRC    BILLING SOURCE?                              
         BE    FS065                                                            
         CLI   BYTE,ACQCNTR        CONTRA?                                      
         BE    FS070                                                            
         CLI   RTNFLDN,FLDNPART    PARTICIPANT                                  
         BE    FS070                                                            
         CLI   RTNFLDN,FLDNVNDR    AND VENDOR                                   
         BE    FS070                                                            
*                                                                               
FS065    DS    0H                                                               
         LA    RF,2(,RF)           STRIP OFF UNIT LEDGER                        
         AHI   RE,-2                                                            
*                                                                               
*                                  ************************************         
*                                  * COMMON CODE                      *         
*                                  ************************************         
FS070    DS    0H                                                               
         TM    1(R1),X'40'         LOWER CASE ?                                 
         BZ    FS075               YES, SKIP                                    
         EXMVC RE,8(R6),0(RF)      NO,  INSERT FIELD                            
         B     FS080               GET  READY TO EXIT                           
*                                                                               
FS075    DS    0H                                                               
         OI    1(R1),X'40'         TEMPORARILY UPPER   CASE THIS BYTE           
         MVI   8(R6),C'*'          INDICATE    EXCLUDE                          
         EXMVC RE,9(R6),0(RF)      INSERT      FIELD                            
         NI    1(R1),TURNOFF-X'40' RESTORE     LOWER   CASE                     
*                                                                               
FS080    DS    0H                                                               
         GOTO1 AFVAL,(R6)          VALIDATE    FIELD                            
         BNE   FS090               ERROR,      EXIT    WITH ERROR               
         SR    RE,RE                                                            
*                                                                               
FS090    LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  SEE IF FIELD WAS BUILT (SPECIAL RULE FOR FIELD)                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
FLDCNTL  NTR1                                                                   
         CLI   REPFFLDN,FLDNBUDG   BUDGET ?                                     
         BNE   FLDCNT10                                                         
         SR    R1,R1                                                            
         ICM   R1,3,REPFDDSP                                                    
         SHI   R1,ACQAPPL-ACQD-1                                                
         CLM   R1,1,BUD#                                                        
         BH    FLDCNT98            SKIP  FIELD                                  
*                                                                               
FLDCNT10 CLI   APGFLAG,YES                                                      
         BNE   FLDCNT30            NO,   TEST   SOME MORE                       
         CLI   REPFFLDN,FLDNOPT6                                                
*&&UK*&& BE    FLDCNT90            YES,  FIELD  ON   SCREEN                     
*&&US                                                                           
         BNE   FLDCNT30            NO,   TEST   SOME MORE                       
         TM    SVCMPST5,CPYAPGS                                                 
         BZ    FLDCNT98            FIELD NOT    ON   SCREEN                     
         TM    CUAUTH,X'20'        ARE   THEY   AUTHORIZED ?                    
         BZ    FLDCNT98            NO,   FIELD  NOT  ON   SCREEN                
         B     FLDCNT90            YES,  FIELD  ON   SCREEN                     
*&&                                                                             
*                                                                               
FLDCNT30 CLI   REPFFLDN,FLDNRCAP   Recap field ?                                
         BNE   FLDCNT90            No                                           
         CLC   RECAP#,REPFDDSP+1   Recap format exists ?                        
         BL    FLDCNT98            No, field not on screen                      
*                                                                               
FLDCNT90 SR    RE,RE               FIELD ON     SCREEN                          
*                                                                               
FLDCNT98 LTR   RE,RE               IS    FIELD  ON   SCREEN ?                   
         B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET NEXT AVAILABLE FIELD IN REQUEST CARD 3 OR 4                    *         
***********************************************************************         
         SPACE 1                                                                
GETFFLD  NTR1                                                                   
         LA    R0,8                                                             
         L     R3,0(,R1)                                                        
         LA    R4,ACQTYP1          POINT TO 1ST FIELD IN 3RD CARD               
*                                                                               
GETFFLD4 DS    0H                                                               
         CHI   R0,4                GO TO 2ND CARD ?                             
         BNE   *+8                                                              
         LA    R4,ACQTYP5          POINT TO 1ST FIELD 4TH CARD                  
         CLI   0(R4),C' '          IS   THIS FIELD AVAILABLE ?                  
         BNH   GETFFLD9            YES, USE IT                                  
         LA    R4,ACQTYP2-ACQTYP1(,R4)  BUMP TO NEXT FIELD                      
         BCT   R0,GETFFLD4                                                      
         SR    R4,R4               SET TO ZERO TO SAY NO                        
*                                                                               
GETFFLD9 ST    R4,0(,R1)           SAVE ADDRESS                                 
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  NEXTUNPT - FIND NEXT UNPROTECTED FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R5                                                          
NEXTUNPT NTR1                                                                   
*                                                                               
NEXTUN05 ZIC   R1,0(,R6)           LENGTH OF FIELD                              
         LR    RF,R6                                                            
         SR    RF,R5                                                            
         ST    RF,ATEXTFLD                                                      
         AR    R6,R1               BUMP TO NEXT ONE                             
         TM    1(R6),X'20'         PROTECTED?                                   
         BO    NEXTUN05                                                         
         XIT1  REGS=(R6)                                                        
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* CHKRFILL -   THIS ROUTINE MAY BE USED TO DETERMINE IF WE MAY REFILL *         
*              A FIELD FROM THE USER'S PROFILE; E.G. WE SHOULD NOT BE *         
*              REFILLING THE FIELD IN SOME CASES WHEN RECAPPING IS IN *         
*              EFFECT AND THE REPFRNRF IS ON.                         *         
*                                                                     *         
* NOTE - ON INPUT R2 POINTS TO THE THE report field dsect                       
*                                                                     *         
* ON EXIT:                                                            *         
*    CONDITION CODE:                                                  *         
*        EQ -  REFILL THE FIELD                                       *         
*        NE -  DO NOT REFILL THE FIELD                                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
CHKRFILL NTR1                                                                   
         SR    RE,RE               CC = refill this field                       
         TM    REPFIND2,REPFRNRF   Dd not refill when recap on ?                
         BZ    CHKRFL90            No                                           
         CLI   RECAP#,0            Any recap requested ?                        
         BE    CHKRFL90            No                                           
         TM    RCAPSW,RCAPFURP     Any usage of recap's profile flag ?          
         BZ    CHKRFL90            No, then refill anyway                       
*                                                                               
         LA    RE,1                CC = Do not refill this field                
CHKRFL90 DS    0H                                                               
         LTR   RE,RE               Set Condition code                           
         B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  FILLFLD - INSERT ELEMENT DATA                                      *         
*                                                                     *         
*  ON INPUT:                                                          *         
*     PARM LIST:                                                      *         
*        P1  = A(IO AREA OF RECORD)                                   *         
*        P2  = AL1(FIELD TYPE # OF X'C5'ELEMENT)                      *         
*              A(HEADER FIELD TO FILL)                                *         
*        P3  = A(HEADER FIELD FOR NEXT (DETAIL) FIELD NAME) OR A(0)   *         
*        P4  = A(PROFILE NUMBER FOR GETTEXT)                          *         
*                                                                     *         
* ON EXIT:                                                            *         
*    THE FIRST BYTE OF P1:                                            *         
*        00 = NOT FOUND                                               *         
*        01 = FOUND AN ITEM                                           *         
*        02 = FOUND A LIST                                            *         
*        03 = FOUND MULTIPLE ITEMS OR LISTS                           *         
***********************************************************************         
         SPACE 1                                                                
FILLFLD  NTR1                                                                   
         L     R2,0(,R1)           A(IO AREA)                                   
         ICM   R4,15,8(R1)         CLEAR NAME FIELD IN ANY                      
         BZ    FILL10              NOTHING TO PUT                               
         SR    RF,RF                                                            
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         SHI   RF,9                MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         SHI   RF,8                MINUS EXTENDED FIELD HEADER                  
         EXMVC RF,8(R4),SPACES     CLEAR FIELD                                  
*                                                                               
FILL10   L     R4,4(,R1)           A(FIELD HEADER)                              
         LA    R4,0(,R4)           CLEAR HOB                                    
         MVC   TYPEC5,4(R1)        SAVE TYPE # FOR X'C5' ELEMENT                
         XC    0(4,R1),0(R1)                                                    
         MVI   EXTFLD,NO                                                        
*                                                                               
         USING RFLELD,R2                                                        
         SR    R6,R6                                                            
         AH    R2,DATADISP                                                      
FILL20   IC    R6,RFLLN                                                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    FILL90                                                           
         CLI   0(R2),RFLELQ        X'C5' FILTER ELEMENT                         
         BNE   FILL25                                                           
         CLI   RFLSEQ,0            HIGH LEVEL FILTER?                           
         BNE   FILL25              NO , COLUMN FILTER (SKIP)                    
         CLC   RFLTYPE,TYPEC5      MATCH TYPE                                   
         BE    FILL40                                                           
*                                                                               
FILL25   AR    R2,R6                                                            
         B     FILL20                                                           
*                                  ************************************         
FILL40   DS    0H                  * GOT X'C5' FILTER ELEMENT         *         
*                                  ************************************         
         SHI   R6,RFLLNQ+1         EXMVC LENGTH                                 
         TM    RFLIND,RFLXCLD      SHOW EXCLUDE?                                
         BZ    *+8                                                              
         LA    R6,1(,R6)           ADD ONE FOR C'*'                             
         MVI   0(R1),3             MARK AS MULTIPLE ACCOUNTS                    
         CLI   RFLTYPE,RFLTTYPE    TRANS TYPE?                                  
         BNE   FILL42                                                           
*                                                                               
         BAS   RE,FILLTTY          R3=NEW EXMVC LENGTH, R2=NEW ELEMENT          
         LR    R6,R3               NEW LENGTH                                   
         BH    FILL50              MARK AS MULTIPLE TYPES                       
         BE    FILL90              NO OUTPUT                                    
***********************************************************************         
*        DETERMINE DATA LENGTH + IS IT SINGLE ITEM OR LIST OR         *         
*             MULITPLE ELEMENTS OR LISTS.                             *         
*        R6 = EXMVC LENGHT OF DATA                                    *         
*        RF = FIELD SIZE                                              *         
***********************************************************************         
FILL42   SR    RF,RF                                                            
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         SHI   RF,8                MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         SHI   RF,8                MINUS EXTENDED FIELD HEADER                  
         LR    R3,RF                                                            
         BCTR  R3,0                SUBTRACT ONE FOR EXECUTE                     
         EXMVC R3,8(R4),SPACES     CLEAR FIELD                                  
         CR    R6,RF                                                            
         BNL   FILL50              TOO BIG A FIELD                              
*                                                                               
         LR    RF,R6               NOW RF BECOMES EXMVC DATA LENGTH             
         TM    RFLIND,RFLXCLD      EXCLUDE ON IN RECORD ?                       
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    FILL60              ONE CHARACTER OF DATA                        
         BM    FILL90              NO DATA                                      
*                                                                               
FILL45   LA    RE,RFLDATA(RF)      POINT TO END OF DATA                         
         CLC   SCCOMMA,0(RE)       LOOK FOR DELIMITER (COMMA)                   
         BE    FILL50              ONLY ONE FIELD ALLOWED                       
         BCT   RF,FILL45           LOOP                                         
         B     FILL60                                                           
*                                  ************************************         
FILL50   DS    0H                  * TOO BIG OR MULTIPLE ITEMS/LISTS  *         
*                                  ************************************         
         ICM   RF,15,12(R1)        GET PROFILE TEXT #                           
         BZ    FILL51                                                           
         LR    R3,R1               SAVE A(PARM LIST)                            
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
*                                  INSERT TEXT FIELD ONTO SCREEN                
         GOTO1 TEXTGET,TMPPARM,(RF),(R4),0                                      
         LR    R1,R3               RESTORE A(PARM LIST)                         
*                                                                               
FILL51   ICM   R4,15,8(R1)         HEADER FIELD FOR DETAIL                      
         BZ    FILL90              NOTHING TO PUT                               
         SR    RF,RF                                                            
         IC    RF,0(,R4)           GET LENGTH OF FIELD                          
         SHI   RF,8                MINUS FIELD HEADER                           
         TM    1(R4),FVAXTND       EXTENDED HEADER?                             
         BZ    *+8                                                              
         SHI   RF,8                MINUS EXTENDED FIELD HEADER                  
         BCTR  RF,0                MINUS ONE FOR POSSIBLE EXECUTE               
         CR    R6,RF               RF=LENGTH OF FIELD VS FIELD SIZE             
         BNH   FILL80              TOO BIG A FIELD?                             
         MVI   EXTFLD,YES          YES                                          
         LR    R6,RF               USE RF INSTEAD                               
         B     FILL80                                                           
*                                  ************************************         
FILL60   DS    0H                  * ONE ITEM OR ONE LIST             *         
*                                  ************************************         
         MVI   0(R1),2             MARK AS A LIST                               
         CLI   RFLDATA,C'+'        IS IT A INCLUDE LIST TYPE                    
         BE    FILL62                                                           
         CLI   RFLDATA,C'-'        IS IT A EXCLUDE LIST TYPE                    
         BNE   FILL70                                                           
FILL62   DS    0H                  <**** SINGLE LIST ****>                      
         ICM   RF,15,8(R1)         GET  NAME (DETAIL) FIELD                     
         BZ    FILL80              NONE, SKIP                                   
         LA    R6,1(,R6)           GET  FIELD LENGTH (NOT EXMVC LENGTH)         
*                                                                               
*                                  DISPLAY THE NAME OF THE LIST                 
         GOTO1 DISPLIST,TMPPARM,((R6),RFLDATA),(RF)                             
         BCTR  R6,0                RESTORE THE EXMVC LENGTH                     
         B     FILL80              MOVE DATA TO THE SCREEN                      
*                                                                               
         USING ACQD,R3                                                          
FILL70   DS    0H                  <*** SINGLE ITEM ****>                       
         MVI   0(R1),1             MARK AS AN ACCOUNT                           
         CLI   RFLTYPE,RFLACC                                                   
         BNE   FILL80                                                           
         L     R3,AIOAREA3                                                      
         LA    R3,L'ACQCARD1(,R3)                                               
         CLI   APMLDGR,YES         MULTILEDGER REPORT TYPE?                     
         BNE   FILL80              NO, SO DON'T WORRY ABOUT UL                  
         L     RF,AFLDUNLG         NO,  UNIT LEDGER SUPPLIED SO                 
         A     RF,ATWA             REPLACE WITH SINGLE ACCOUNT                  
         CLC   ACQUNT(2),SPACES    IS   UNIT/LEDGER SPECIFIED ?                 
         BNE   FILL73              YES, JUST INSERT THE ACCOUNT DATA            
         MVC   8(2,RF),RFLDATA                                                  
         OI    6(RF),FVOXMT        TRANSMIT FIELD                               
*                                                                               
FILL73   DS    0H                                                               
         CLC   RFLDATA(2),8(RF)    IF U/L DOESN'T MATCH THEN SKIP               
         BNE   FILL90                                                           
         TM    RFLIND,RFLXCLD      EXCLUDE DATA TYPE?                           
         BZ    FILL75              NO,  SKIP                                    
         MVI   8(R4),C'*'          YES, SHOW AS EXCLUDE                         
         SHI   R6,3                SUBTRACT 1 FOR "*" AND 2 FOR U/L             
         EXMVC R6,9(R4),RFLDATA+2  MOVE DATA TO SCREEN                          
         LA    RF,2(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         B     FILL90                                                           
*                                                                               
FILL75   DS    0H                                                               
         SHI   R6,2                SUBTRACT 2 FOR UNIT/LEDGER                   
         EXMVC R6,8(R4),RFLDATA+2  MOVE DATA TO SCREEN                          
         LA    RF,1(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         B     FILL90                                                           
         DROP  R3                                                               
*                                  ************************************         
FILL80   DS    0H                  * MOVE DATA TO SCREEN              *         
*                                  ************************************         
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         LA    R8,8(,R4)           POINT PAST FIELD HEADER                      
         LR    RF,R6               SAVE  EXMVC LENGTH                           
         TM    RFLIND,RFLXCLD      EXCLUDE DATA TYPE?                           
         BZ    FILL88                                                           
         MVI   0(R8),C'*'          SHOW AS EXCLUDE                              
         SHI   RF,1                SUBTRACT ONE                                 
         BM    FILL90                                                           
         LA    R8,1(,R8)           BUMP UP BECAUSE OF C'*'                      
*                                                                               
FILL88   EXMVC RF,0(R8),RFLDATA    MOVE IN DATA                                 
         LA    RF,1(,R6)           GET  REAL LENGTH                             
         STC   RF,7(,R4)           SAVE IN FIELD HEADER                         
         CLI   EXTFLD,YES                                                       
         BNE   FILL90                                                           
         LA    RE,8(R6,R4)                                                      
         MVI   0(RE),C'>'                                                       
*                                                                               
FILL90   B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DISPLAY THE NAME OF THE LIST                                        *         
*                                                                     *         
* INPUT PARAMETERS:                                                   *         
*   P1 BYTE  0    = FIELD LENGTH (NOT EXMVC LENGTH)                   *         
*      BYTES 1-3  = ADDRESS OF THE CLIST NAME INCLUDING +/-           *         
*  P2  BYTE  0    = X"00'                                             *         
*      BYTES 1-3  = ADDRESS OF HEADER FOR DETAIL (NAME) FIELD OR      *         
*                   ZERO    IF NO         DETAIL (NAME) FIELD         *         
***********************************************************************         
         SPACE 1                                                                
         USING LSTRECD,R3                                                       
DISPLIST NTR1                                                                   
         L     R2,0(,R1)           ADDR OF LIST NAME                            
         ICM   R4,15,4(R1)         ADDR OF DETAIL (NAME) FIELD HEADER           
         BZ    XIT                 NONE,   EXIT                                 
         SR    RF,RF                                                            
         IC    RF,0(,R1)           GET  THE DATA LENGTH                         
         SHI   RF,2                LESS THE C"+" OR C"-" AND EX INSTR.          
         LA    R3,IOKEY                                                         
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,CUABIN      COMPANY CODE                                 
         EX    RF,*+4                                                           
         MVC   LSTKLST(0),1(R2)                                                 
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   XIT                                                              
         GOTO1 GETNAME,TMPPARM,AIOAREA2,(R4)                                    
         OI    6(R4),FVOXMT        TRANSMIT DETAIL FIELD                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* FILL IN THE TRANSACTION TYPE                                        *         
*                                                                     *         
* CONCODE   MEANS              EXIT CONDITION CODE                    *         
*    0      SINGLE   ELEMENT        LOW                               *         
*    1      NO DATA                 EQUAL                             *         
*    2      MULTIPLE ELEMENTS       HIGH                              *         
*                                                                     *         
* ON EXIT:                                                            *         
*    R2 = ADDRESS OF DUMMY ELEMENT                                    *         
*    R3 = EXMVC LENGTH OF ELEMENT (WITHIN THE MODULE R1 HAS EXMVC LNG)*         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
FILLTTY  NTR1                                                                   
         MVI   CONCODE,1           NO DATA                                      
         MVC   APWORK,SPACES                                                    
         GOTO1 CNVTTYPE,APPARM,(C'S',(R2)),APWORK                               
         MVC   APELEM(RFLLNQ),RFLEL                                             
         LA    R1,40               MAX FIELD SIZE                               
         LA    R8,APWORK-1(R1)     POINT TO THE LAST BYTE OF THE FIELD          
*                                                                               
FILLTY05 DS    0H                                                               
         CLI   0(R8),C' '          IS   THIS BYTE BLANK ?                       
         BH    FILLTY10            NO,  WE GOT THE FIELD LENGTH !               
         BCTR  R8,0                YES, SUBTRACT ONE AND                        
         BCT   R1,FILLTY05         TRY  THE PREVIOUS CHARACTER                  
         B     FILLTY90            SET  CC = EQUAL - NO DATA FOUND              
*                                                                               
NEW      USING RFLELD,R3                                                        
*                                                                               
FILLTY10 LA    R3,APELEM           CREATE NEW ELEMENT                           
         LA    RF,APWORK                                                        
         CLI   APWORK,C'*'         WAS IT EXCLUDE                               
         BNE   FILLTY15                                                         
         LA    RF,APWORK+1                                                      
         BCTR  R1,0                                                             
*                                  R1   IS THE REAL LENGTH                      
FILLTY15 DS    0H                  RF   POINTS TO DATA WITH NO "*"              
         BCTR  R1,0                GET  MOVE LENGTH                             
         EXMVC R1,NEW.RFLDATA,0(RF)     INSERT INTO DUMMY RECORD AREA           
         LA    RF,RFLLNQ+1(,R1)    GET  LENGTH OF   DUMMY RECORD                
         STC   R1,NEW.RFLLN        SAVE LENGTH IN   DUMMY RECORD                
         DROP  NEW                                                              
*                                  BACK TO THE ORIGINAL INPUT                   
         MVI   CONCODE,2                                                        
         CLI   RFLLN,RFLLNQ+1      ALLOW ONLY ONE                               
         BH    FILLTY90            MULTIPLE, SKIP                               
*        CLI   RFLDATA,TY30DI      IS SPECIAL TYPE ?                            
*        BNL   FILLTY90            YES, TREAT AS MULTIPLE                       
         MVI   CONCODE,0           SET TO SINGLE                                
*                                                                               
FILLTY90 LA    R2,APELEM           USE NEW ELEMENT                              
         LR    R3,R1                                                            
         TM    RFLIND,RFLXCLD                                                   
         BZ    *+8                                                              
         LA    R3,1(,R3)           ADD  ONE                                     
         CLI   CONCODE,1           SET CON CODE                                 
         XIT1  REGS=(R2,R3)        BE = NO DATA, BL = SINGLE, BH = MULT         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET RECAP DETAILS (DURING VALKEY)                                  *         
***********************************************************************         
         SPACE 1                                                                
GETRCAP  NTR1                                                                   
         L     R1,AIOAREA1         ->     RECORD                                
         MVI   RECAP#,0            Assume no recapping                          
         MVI   RCAPSW,0            Clear recap info                             
         XC    RCAPFMTS(MAXRCP#*L'RCAPFMTS),RCAPFMTS                            
         MVC   RCAPACQT,SPACES                                                  
*                                                                               
         LA    R2,RCAPFMTS         ->     RECAP  FORMATS                        
         MVI   APELCODE,RCPELQ     X'C8'  FILTER ELEMENT                        
         GOTO1 GETEL               GET    ELEMENT                               
*                                                                               
GETRCP10 DS    0H                                                               
         BNE   GETRCP50            NO     MORE   ELEMENTS,     RETURN           
*                                                                               
         USING RCPELD,R1           RECAP  ELEMENT                               
         CLI   RCPSEQ,0            MAIN   FORMAT ?                              
         BE    GETRCP30            YES,   GET    1ST   RECAP   FORMAT           
         CLI   RCPSEQ,MAXRCP#      TOO    MANY   RECAP FORMATS                  
         BNH   *+6                 NO,    CONTINUE                              
         DC    H'0'                YES,   ABEND                                 
*                                                                               
         MVC   RECAP#,RCPSEQ       Save hightest recap number                   
*                                                                               
         MVC   0(L'RCAPFMTS,R2),RCPCODE   Save off format code                  
         LA    R2,L'RCAPFMTS(,R2)  ->     Next recap entry for format           
         TM    RCPOPT1,RCPPROF     USE    RECAP'S      PROFILE ?                
         BO    GETRCP20            YES,   TURN   ON    RECAP   PROFILE          
         OI    RCAPSW,RCAPFUMP     NO,    FOUND  USE   MAIN'S  PROFILE          
         B     GETRCP30            CONTINUE                                     
*                                                                               
GETRCP20 DS    0H                  TURN   ON     RECAP PROFILE                  
         OI    RCAPSW,RCAPFURP     NO,    FOUND  USE   RECAP'S PROFILE          
*                                                                               
GETRCP30 DS    0H                  GET    NEXT   RECAP ELEMENT                  
         GOTO1 NEXTEL                                                           
         B     GETRCP10                                                         
*                                                                               
GETRCP50 DS    0H                                                               
         CLI   RECAP#,0            Any recap elements ?                         
         BE    GETRCPEX            No                                           
         MVI   RCAPACQT,C'Y'       Yes, assume they user wants                  
         MVC   RCAPACQT+1(L'RCAPACQT-1),RCAPACQT                                
*                                                                               
         L     R3,AIOAREA3         Request area                                 
         MVI   BYTE,ACQRCAP        Look for recap element                       
         BAS   RE,FINDSPCL         Find it ?                                    
         BNE   GETRCPEX            No, not found                                
         L     R2,AREQLOC          A(Recap data field in request card)          
         MVC   RCAPACQT,0(R2)      Save recap data                              
*                                                                               
GETRCPEX B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET APG DETAILS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
GETAPG   NTR1                                                                   
         LA    R2,APRECKEY                                                      
         L     R3,AAPGIO                                                        
         MVC   APDUB,=CL8'AC'                                                   
         MVC   APDUB+2(2),APGKRTY                                               
         MVC   APDUB+4(2),CUAALF                                                
         MVC   APDUB+6(1),APGKFMT                                               
         GOTO1 ALOADER,APPARM,APDUB,AAPGIO,(C'M',AAPGIO)                        
         OC    APPARM+4(4),APPARM+4                                             
         BNZ   *+6                                                              
         DC    H'00'                                                            
         DROP  R2                                                               
*                                                                               
         MVI   BUD#,0                                                           
         MVI   OPT1#,0                                                          
         MVI   OPT3#,0                                                          
         MVI   OPT4#,0                                                          
         MVI   LDGR#,0                                                          
         MVC   LDGLIST,SPACES                                                   
         XC    BUDLIST,BUDLIST                                                  
         XC    OPT1LST,OPT1LST                                                  
         XC    OPT3LST,OPT3LST                                                  
         XC    OPT4LST,OPT4LST                                                  
*                                                                               
GETAGP10 CLI   0(R3),X'FF'         END OF REPORT                                
         BE    GETAGP90                                                         
         CLI   0(R3),CMREAD        SINGLE LEDGER                                
         BE    GETAGP20                                                         
         CLI   0(R3),CMRLST        MULTIPLE LEDGER                              
         BE    GETAGP20                                                         
         CLI   0(R3),CMBGDT        BUDGETS                                      
         BE    GETAGP40                                                         
         CLI   0(R3),CMIF          CONDITIONAL IF                               
         BE    GETAGP60                                                         
         CLI   0(R3),CMOR          CONDITIONAL OR                               
         BE    GETAGP60                                                         
         CLI   0(R3),CMAND         CONDITIONAL AND                              
         BE    GETAGP60                                                         
*                                                                               
GETAGP15 SR    R1,R1                                                            
         IC    R1,1(,R3)           BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     GETAGP10                                                         
         EJECT ,                                                                
***********************************************************************         
* GET LEDGER INFO                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETAGP20 SR    R1,R1                                                            
         IC    R1,1(,R3)                                                        
         SHI   R1,2                                                             
         LA    RE,2(,R3)                                                        
         CLI   0(R3),CMREAD                                                     
         BE    GETAGP25                                                         
         BCTR  R1,0                SUBTRACT ONE                                 
         LA    RE,3(,R3)                                                        
*                                                                               
GETAGP25 LA    R6,LDGLIST                                                       
         SR    RF,RF                                                            
         ICM   RF,1,LDGR#                                                       
         BZ    GETAGP30                                                         
         CLC   0(2,RE),0(R6)       CHECK FOR DUPLICATES                         
         BE    GETAGP35                                                         
         LA    R6,2(,R6)           NEXT LEDGER                                  
         BCT   RF,*-14                                                          
*                                                                               
GETAGP30 IC    RF,LDGR#                                                         
         LA    RF,1(,RF)                                                        
         STC   RF,LDGR#                                                         
         MVC   0(2,R6),0(RE)                                                    
*                                                                               
GETAGP35 LA    RE,3(,RE)                                                        
         SHI   R1,3                                                             
         BZ    GETAGP15            FINISHED                                     
         B     GETAGP25            GET ANOTHER                                  
         EJECT ,                                                                
***********************************************************************         
*  GET BUDGET INFO                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETAGP40 SR    R6,R6                                                            
         IC    R6,2(,R3)           BUDGET TO PROCESS                            
         LA    R6,BUDLIST-1(R6)                                                 
         CLC   BUD#,2(R3)          SAVE HIGHEST BUD NUMBER                      
         BH    *+10                                                             
         MVC   BUD#,2(R3)                                                       
         OC    3(2,R3),3(R3)       ANY BUDGET?                                  
         BZ    GETAGP45                                                         
         MVC   0(1,R6),4(R3)       SUPPORT BUD# 1-230                           
         B     GETAGP15            FINISHED                                     
*                                                                               
         USING BUDRECD,R2                                                       
GETAGP45 LA    R2,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
         MVC   BUDKCOD,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         SHI   RF,6                                                             
         BM    GETAGP15                                                         
         EX    RF,*+4                                                           
         MVC   BUDKCOD(0),5(R3)                                                 
         MVC   SAVBUD,BUDKCOD                                                   
         GOTO1 AIO,IO3+IOACCDIR+IOHI                                            
         CLC   BUDKCOD,SAVBUD                                                   
         BNE   GETAGP15            BUDGET MISSING                               
         MVC   0(1,R6),BUDKNO2+1                                                
         B     GETAGP15            FINISHED                                     
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET OPTION INFO                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETAGP60 LA    R1,OPT1#                                                         
         LA    R2,OPT1LST                                                       
         CLI   3(R3),FRQ1          OPTION 1                                     
         BE    GETAGP65                                                         
         LA    R1,OPT3#                                                         
         LA    R2,OPT3LST                                                       
         CLI   3(R3),FRQ3          OPTION 3                                     
         BE    GETAGP65                                                         
         LA    R1,OPT4#                                                         
         LA    R2,OPT4LST                                                       
         CLI   3(R3),FRQ4          OPTION 4                                     
         BNE   GETAGP15            LOOP                                         
*                                                                               
GETAGP65 MVC   APBYTE,7(R3)                                                     
         CLI   1(R3),8                                                          
         BNH   *+8                                                              
         MVI   APBYTE,C' '                                                      
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)          NUMBER OF OPTIONS SO FAR                     
         BZ    GETAGP70                                                         
         LR    RE,R2                                                            
         AR    R2,RF                                                            
         CLC   APBYTE,0(RE)                                                     
         BE    GETAGP15            DUPLICATE OPTION                             
         LA    RE,1(,RE)                                                        
         BCT   RF,*-14                                                          
*                                                                               
GETAGP70 MVC   0(1,R2),APBYTE                                                   
         IC    RF,0(,R1)                                                        
         LA    RF,1(,RF)           ADD  ONE                                     
         STC   RF,0(,R1)                                                        
         B     GETAGP15            FINISHED                                     
*                                                                               
GETAGP90 CLI   BUD#,12                                                          
         BNH   *+8                                                              
         MVI   BUD#,12             IS THE HIGHEST SUPPORTED                     
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  SET LIST OF +/- LIST INFO FROM REQUEST CARD                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R3                                                          
SETLIST  NTR1                                                                   
         MVI   LIST#,0             SET TO NONE                                  
         LA    R8,2                                                             
         LA    R0,4                CHECK THE FIRST FOUR FIELDS                  
         LA    R2,ACQTYP1          POINT TO 3RD REQUEST CARD                    
*                                                                               
SETLST10 CLI   0(R2),ACQANAL       USE ONLY ANALYSIS ONES                       
         BNE   SETLST15                                                         
         CLI   1(R2),C'+'          IS IT A INCLUDE LIST TYPE                    
         BE    SETLST20                                                         
         CLI   1(R2),C'-'          IS IT A EXCLUDE LIST TYPE                    
         BE    SETLST20                                                         
*                                                                               
SETLST15 LA    R2,L'ACQTYP1+L'ACQFLT1(R2)                                       
         BCT   R0,SETLST10                                                      
         LA    R0,4                RESET TO PROCESS FOUR FIELDS                 
         LA    R2,ACQTYP5                                                       
         BCT   R8,SETLST10                                                      
         B     SETLST90                                                         
*                                                                               
         USING LSTRECD,R4                                                       
         USING LISTD,R6                                                         
SETLST20 LA    R4,IOKEY                                                         
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,CUABIN      COMPANY CODE                                 
         MVC   LSTKLST(5),2(R2)    MOVE IN LIST                                 
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   SETLST15            BAD LIST?                                    
         ZIC   R1,LIST#                                                         
         LR    R6,R1                                                            
         MH    R6,=Y(LISTLNQ)                                                   
         LA    R6,LISTBLK(R6)                                                   
         LA    R1,1(,R1)                                                        
         STC   R1,LIST#                                                         
         STC   R1,14(R2)           SAVE OF ARRAY # IN REQ CARD                  
         MVC   LISTNME,SPACES                                                   
*                                                                               
         L     R4,AIOAREA2         FIGURE OUT WHICH LIST                        
         AH    R4,DATADISP                                                      
SETLST40 CLI   0(R4),0             END OF RECORD                                
         BE    SETLST15            NEXT FIELD                                   
         CLI   0(R4),LIDELQ        X'1F' LIST INFO ELEMENT                      
         BE    SETLST50                                                         
         CLI   0(R4),NAMELQ        X'20' NAME ELEMENT?                          
         BE    SETLST60                                                         
*                                                                               
SETLST45 SR    R1,R1                                                            
         IC    R1,1(,R4)           NEXT ELEMENT                                 
         AR    R4,R1                                                            
         B     SETLST40                                                         
*                                                                               
         USING LIDELD,R4                                                        
SETLST50 L     RF,=A(LEDGLIST)                                                  
         A     RF,APRELO                                                        
         MVI   LISTTYP,FLDNPART    SET TO PARTICIPANT                           
         CLI   LIDDLEDG,C'3'                                                    
         BE    SETLST45            NEXT                                         
         MVI   LISTTYP,0           RESET                                        
*                                                                               
SETLST52 CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    SETLST45            NOT FOUND                                    
         CLC   LIDDLEDG,1(RF)                                                   
         BE    SETLST55                                                         
         LA    RF,3(,RF)           NEXT IN TABLE                                
         B     SETLST52                                                         
*                                                                               
SETLST55 MVC   LISTTYP,0(RF)       SET TYPE (FIELD NUMBER)                      
         B     SETLST45            NEXT ELEMENT                                 
*                                                                               
         USING NAMELD,R4                                                        
SETLST60 SR    R1,R1                                                            
         IC    R1,NAMLN            ELEMENT LENGTH                               
         SH    R1,=Y(NAMELQ+1)                                                  
         BM    SETLST45                                                         
         EXMVC R1,LISTNME,NAMEREC                                               
         B     SETLST45            NEXT ELEMENT                                 
*                                                                               
SETLST90 B     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO SET IN A ROUTINE (NTR1)                                   *         
***********************************************************************         
         SPACE 1                                                                
ERRIVPUT MVC   FVMSGNO,=AL2(FVFNOTV)       INVALID INPUT                        
         B     ERRXIT                                                           
*                                                                               
ERRMISS  MVC   FVMSGNO,=AL2(FVFMISS)       INPUT MISSING                        
         B     ERRXIT                                                           
*                                                                               
ERRLIST  MVC   FVMSGNO,=AL2(ACEIVLT)       INVALID LIST                         
         B     ERRXIT                                                           
*                                                                               
ERRDATE  MVC   FVMSGNO,=AL2(ACEIVDT)       INVALID DATE                         
         B     ERRXIT                                                           
*                                                                               
ERRXIT   CLC   FVMSGNO,=AL2(FVFOK) SET CON CODE                                 
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* ERROR TO DISPLAY ON TOP OF SCREEN                                   *         
***********************************************************************         
         SPACE 1                                                                
IVALFMT  MVC   FVMSGNO,=AL2(ACEIVFT)                                            
         MVC   SAVFORM,SPACES                                                   
         B     EXIT                                                             
*                                                                               
IVALREQ  MVC   FVMSGNO,=AL2(ACEIVRQ)       INVALID REQUEST                      
         B     EXIT                                                             
*                                                                               
IVALINPT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
IVALMISS MVC   FVMSGNO,=AL2(FVFMISS)                                            
         B     EXIT                                                             
*                                                                               
IVALLIST MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         B     EXIT                                                             
*                                                                               
IVALHARD MVC   FVMSGNO,=AL2(51)            HARDWARE ERROR                       
         B     IVALEXIT                                                         
*                                                                               
IVALCNTR MVC   FVMSGNO,=AL2(74)           BAD CONTRA                            
         B     EXIT                                                             
*                                                                               
IVALEXIT MVI   FVOMTYP,GTMINF              INFORMATION TYPE                     
         MVC   FVADDR,AACTHDR              PUT CURSOR AT ACTION FIELD           
         B     EXIT                                                             
         EJECT ,                                                                
         DS    0A                                                               
ADDR     DC    XL4'AAAAAAAA'                                                    
DMRDIR   DC    CL8'DMRDIR'                                                      
DMADD    DC    CL8'DMADD'                                                       
REQUEST  DC    CL8'ACCREQ'                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
LEDGLIST DS    0C                                                               
*                                  *** CLIENT ***                               
         DC    AL1(FLDNCLNT),CL2'SJ'                                            
*                                  *** VENDOR ***                               
         DC    AL1(FLDNVNDR),CL2'SV'                                            
         DC    AL1(FLDNVNDR),CL2'SX'                                            
         DC    AL1(FLDNVNDR),CL2'SY'                                            
*                                  *** COST ACCOUNT ***                         
         DC    AL1(FLDNCSTA),CL2'1C'                                            
*                                  *** DIRECT EXPENSE ***                       
         DC    AL1(FLDNDEXP),CL2'13'                                            
*                                  *** DEPARTMENT ***                           
         DC    AL1(FLDNDEPT),CL2'2D'                                            
*                                  *** PERSON ***                               
         DC    AL1(FLDNPRSN),CL2'2P'                                            
*                                  *** BILLING SOURCE ***                       
         DC    AL1(FLDNBSRC),CL2'  '                                            
*                                  *** PARTICIPANT ***                          
         DC    AL1(FLDNPART),CL2'3 '                                            
*                                                                               
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TABLE FOR DISPLAYING OF THE DETAIL FIELDS                  *         
***********************************************************************         
         SPACE 1                                                                
ROUTINES DS    0F                                                               
         DC    AL1(FLDNUNLG,LUNLG,RFLLDG,RTDULAC),AL2(1688)                     
         DC    AL1(FLDNACCT,L'ACQACT,RFLACC,RTDULAC),AL2(1680)                  
*                                                                               
         DC    AL1(FLDNBGRP,L'ACQBILGP,RFLBLGP,RTDGENC),AL2(0)                  
         DC    AL1(FLDNOGRP,L'ACQOFGRP,RFLOFGP,RTDGENC),AL2(1687)               
         DC    AL1(FLDNOFFC,L'ACQOFFFL,RFLOFF,RTDGENC),AL2(1687)                
*&&DO*&& DC    AL1(FLDNMGRP,L'ACQMEDGP,RFLMDGP,RTDGENC),AL2(1691)               
         DC    AL1(FLDNMGRP,L'ACQMEDGP,RFLMDGP,RTDGENC),AL2(1693)               
         DC    AL1(FLDNMEDA,L'ACQMEDFL,RFLMED,RTDGENC),AL2(1693)                
         DC    AL1(FLDNWCGP,L'ACQWCGRP,RFLWCGP,RTDGENC),AL2(1692)               
*                                                                               
         DC    AL1(FLDNWKCD,L'ACQWRKLS,RFLWC,RTDWKCD),AL2(1696)                 
*                                                                               
         DC    AL1(FLDNSTDT,L'ACQSTART,0,RTDSDTE),AL2(MSG#STRT)                 
         DC    AL1(FLDNENDT,L'ACQEND,0,RTDSDTE),AL2(MSG#END)                    
         DC    AL1(FLDNMOAR,L'ACQMOSPD,0,RTDRDTE),AL2(0)                        
*       - - - - - - - - - - - - - - - - - - - - - - - - -                       
*&&US*&& DC    AL1(FLDNMMOS,L'ACQDTSTR+L'ACQDTEND,0,RTDSPCL),AL2(0)             
*       - - - - - - - - - - - - - - - - - - - - - - - - -                       
         DC    AL1(FLDNACTS,L'ACQACTST,0,RTDSDTE),AL2(MSG#ASTR)                 
         DC    AL1(FLDNACTE,L'ACQACTND,0,RTDSDTE),AL2(MSG#AEND)                 
*                                                                               
         DC    AL1(FLDNALTS,L'ACQDTSTR,0,RTDSPCL),AL2(MSG#ALTS)                 
         DC    AL1(FLDNALTE,L'ACQDTEND,0,RTDSPCL),AL2(MSG#ALTE)                 
         DC    AL1(FLDNTYPE,L'ACQDTTYP,0,RTDSPCL),AL2(0)                        
         DC    AL1(FLDNOCUR,L'ACQDTEND,0,RTDSPCL),AL2(MSG#END)                  
*                                                                               
         DC    AL1(FLDNTRNT,L'ACQTTYPE,RFLTTYPE,RTDTRNT),AL2(1689)              
*                                                                               
         DC    AL1(FLDNBILT,L'ACQBILTY,RFLBTYP,RTDGENC),AL2(1697)               
*                                                                               
         DC    AL1(FLDNESTS,L'ACQOPT7,0,RTDESTS),AL2(0)                         
*                                                                               
         DC    AL1(FLDNSTTY,L'ACQSTUTY,RFLSTTY,RTDGENC),AL2(0)                  
         DC    AL1(FLDNUSRF,L'ACQUSFLD,RFLUFLD,RTDGENC),AL2(0)                  
*                                                                               
         DC    AL1(FLDNCNTR,LULACNT,RFLCNTR,RTDACCT),AL2(1695)                  
         DC    AL1(FLDNCLNT,LACCOUNT,RFLCLI,RTDACCT),AL2(1681)                  
         DC    AL1(FLDNCSTA,LACCOUNT,RFLCOST,RTDACCT),AL2(1685)                 
         DC    AL1(FLDNBSRC,LACCOUNT,RFLBSR,RTDACCT),AL2(1694)                  
         DC    AL1(FLDNPART,LULACNT,RFLPRTP,RTDACCT),AL2(0)                     
         DC    AL1(FLDNDEPT,LACCOUNT,RFLDEPT,RTDACCT),AL2(1682)                 
         DC    AL1(FLDNPRSN,LACCOUNT,RFLPRSN,RTDACCT),AL2(1683)                 
         DC    AL1(FLDNDEXP,LACCOUNT,RFLXCAT,RTDACCT),AL2(1686)                 
         DC    AL1(FLDNVNDR,LULACNT,RFLVNDR,RTDACCT),AL2(1684)                  
*                                                                               
         DC    AL1(FLDNAOFF,L'ACQANOF,RFLAOFF,RTDGENC),AL2(0)                   
         DC    AL1(FLDNOFCC,L'ACQANOF,RFLCOFF,RTDGENC),AL2(0)                   
*                                                                               
         DC    AL1(FLDNFLT1,L'ACQACTF1,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLT2,L'ACQACTF2,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLT3,L'ACQACTF3,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLT4,L'ACQACTF4,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLT5,L'ACQACTF5,0,RTDFLTR),AL2(0)                        
*                                                                               
         DC    AL1(FLDNFLC1,L'ACQCFLT1,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLC2,L'ACQCFLT2,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLC3,L'ACQCFLT3,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLC4,L'ACQCFLT4,0,RTDFLTR),AL2(0)                        
         DC    AL1(FLDNFLC5,L'ACQCFLT5,0,RTDFLTR),AL2(0)                        
*                                                                               
         DC    AL1(FLDNSELT,L'ACQSEL,0,RTDOPTS),AL2(0)                          
*                                                                               
         DC    AL1(FLDNOPT1,L'ACQOPT1,0,RTDOPTS),AL2(0)                         
         DC    AL1(FLDNOPT2,L'ACQOPT2,0,RTDOPTS),AL2(0)                         
         DC    AL1(FLDNOPT3,L'ACQOPT3,0,RTDOPTS),AL2(0)                         
         DC    AL1(FLDNOPT4,L'ACQOPT4,0,RTDOPTS),AL2(0)                         
         DC    AL1(FLDNOPT5,L'ACQOPT5,0,RTDOPTS),AL2(0)                         
         DC    AL1(FLDNOPT6,L'ACQOPT6,0,RTDOPTS),AL2(0)                         
         DC    AL1(FLDNOPT7,L'ACQOPT7,0,RTDOPTS),AL2(0)                         
*                                                                               
         DC    AL1(FLDNNARA,L'ACQCOMNT,0,RTDLFJT),AL2(0)                        
*                                                                               
*                                  HARDCODE BUD LENGTH TO 1                     
         DC    AL1(FLDNBUDG,1,0,RTDBUDG),AL2(0)                                 
*                                                                               
         DC    AL1(FLDNFRPT,LENFFRPT,0,RTDYNO),AL2(0)                           
*                                                                               
         DC    AL1(FLDNRCAP,LENFRCAP,0,RTDRECAP),AL2(0)                         
*                                                                               
         DC    AL1(FLDNMTHD,L'ACQMTHD,0,RTDMTHD),AL2(0)                         
*                                                                               
         DC    AL1(FLDNCURC,L'ACQBILGP,0,RTD2DEC),AL2(0)                        
         DC    AL1(FLDNOVHD,LENFOVHD,0,RTD2DEC),AL2(0)                          
*                                                                               
         DC    AL1(FLDNPRDR,L'ACQSTART+L'ACQEND,0,RTDRANG),AL2(0)               
         DC    AL1(FLDNCNDR,L'ACQSTART+L'ACQEND,0,RTDRANG),AL2(0)               
*                                                                               
         DC    AL1(FLDNLOCS,L'ACQLOCS,0,RTDLOCS),AL2(0)                         
*                                                                               
         DC    AL1(FLDNRCON,L'ACQRECON,0,RTDGENC),AL2(0)                        
*                                                                               
         DC    AL1(FLDNPRSC,LENFPRSC,0,RTDPRSC),AL2(0)                          
         DC    AL1(FLDNAUTH,LENFAUTH,0,RTDAUTH),AL2(0)                          
*                                                                               
         DC    X'FF'                                                            
         EJECT ,                                                                
       ++INCLUDE ACSCRREQ                                                       
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR LOCAL WORKING STORAGE                                     *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
DWORK    DS    12D                                                              
DUB      DS    D                                                                
TMPPARM  DS    8F                                                               
SVRF     DS    A                                                                
AAPGIO   DS    A                                                                
ASCRTAB  DS    A                                                                
AFLDUNLG DS    A                   DISP TO UNIT/LEDGER FIELD HEADER             
ALOADER  DS    A                                                                
ATEXTFLD DS    A                   OFFSET FROM TWA OF LAST UNPROTECTED          
*WAOFFLD DS    A                   A(OFFICE FIELD DISCRIPTION)                  
AREQLOC  DS    A                                                                
FLDXLEN  DS    XL1                                                              
SAVPFKEY DS    AL1                                                              
BYTE     DS    XL1                                                              
REQ#     DS    XL1                 REQUEST NUMBER REQUESTED TO DISPLAY          
NOPTS    DS    XL1                                                              
EXCLUDE  DS    XL1                 EXCLUDE YES OR NO                            
TYPEC5   DS    XL1                                                              
EXTFLD   DS    XL1                                                              
CONCODE  DS    XL1                                                              
SAVBUD   DS    CL10                                                             
SAVEUNL  DS    CL2                                                              
RFPFLAG  DS    XL1                 RFP INDICATOR                                
RFPFON   EQU   X'80'               RFP IS ON                                    
CUROFF   DS    CL2                                                              
MOASTR   DS    CL4                 YYMM    MOA START                            
MOAEND   DS    CL4                 YYMM    MOA END                              
TMPDTE   DS    CL6                 YYMMDD  TEMP DATE                            
ALTSDTE  DS    CL6                 YYMMDD  ALT START DATE                       
ALTEDTE  DS    CL6                 YYMMDD  ALT END   DATE                       
BUD#     DS    AL1                                                              
BUDCOUNT DS    AL1                                                              
LDGR#    DS    AL1                                                              
OPT1#    DS    AL1                                                              
OPT3#    DS    AL1                                                              
OPT4#    DS    AL1                                                              
BUDLIST  DS    CL16                                                             
*                                                                               
RECAP#   DS    AL1                 HIGHEST RECAP REPORT  NUMBER                 
RCAPFMTS DS    (MAXRCP#)CL(L'RCPCODE)      RECAP FORMAT  NAMES                  
RCAPACQT DS    CL(MAXRCP#)         ACQTYP1 INFO  FOR     RECAP                  
*                                                                               
RCAPSW   DS    XL1                 RECAP   SWITCH                               
RCAPFUMP EQU   X'80'               . FOUND USE   MAIN'S  PROFILE                
RCAPFURP EQU   X'40'               . FOUND USE   RECAP'S PROFILE                
*                                                                               
LDGLIST  DS    CL20                                                             
OPT1LST  DS    CL10                                                             
OPT3LST  DS    CL10                                                             
OPT4LST  DS    CL10                                                             
OPTNME   DS    CL15                OPTION NAME TO DISPLAY                       
OPTIDNUM DS    AL1                                                              
OPTEQU   DS    AL1                 TYPE EQUATE                                  
OPTVALUE DS    AL1                 VALUE IN OPTION                              
*                                                                               
LOSTATUS DS    CL(LENFLOCS-1)      LOCATION STATUS WORK AREA                    
LLOSTATU EQU   L'LOSTATUS          LENGTH OF LOCATION STATUS WORK AREA          
*                                                                               
TYPETEXT DS    CL(ADODESCL)        TEXT FOR TYPE FIELD                          
*                                                                               
WORK     DS    CL20                                                             
SOFBLOCK DS    XL(SOFDATL)                                                      
*                                                                               
LIST#    DS    XL1                 NUMBER OF +/- LIST SAVED OFF                 
LISTBLK  DS    6CL37                                                            
LWSX     DS    0C                                                               
         EJECT ,                                                                
LISTD    DSECT                                                                  
LISTTYP  DS    AL1                 FIELD NUMBER TYPE MATCHED ON                 
LISTNME  DS    CL36                SAVED +/- LIST NAME                          
LISTLNQ  EQU   *-LISTD                                                          
         SPACE 1                                                                
RTND     DSECT                     ** ROUTINE TABLE DSECT **                    
RTNFLDN  DS    XL1                 ROUTINE FIELD NUMBER                         
RTNFLDLN DS    XL1                 ROUTINE FIELD LENGTH                         
RTNPROFN DS    XL1                 FIELD PROFILE NUMBER                         
*                                                                               
RTNDDISP DS    AL1                 DISPALY  ROUTINE #                           
RTDULAC  EQU   1                   UNIT/LEDGER                                  
RTDGENC  EQU   2                                                                
RTDSDTE  EQU   3                   SINGLE DATE                                  
RTDRDTE  EQU   4                                                                
RTDSPCL  EQU   5                                                                
RTDESTS  EQU   6                                                                
RTDACCT  EQU   7                   ACCOUNT                                      
RTDFLTR  EQU   8                                                                
RTDTRNT  EQU   9                   TRANACTION TYPE                              
RTDBUDG  EQU   10                                                               
RTDOPTS  EQU   11                  OPTIONS                                      
RTDMTHD  EQU   12                  COST METHOD                                  
RTD2DEC  EQU   13                                                               
RTDLFJT  EQU   14                                                               
RTDRANG  EQU   15                                                               
RTDLOCS  EQU   16                  LOCATION STATUS                              
RTDPRSC  EQU   17                                                               
RTDWKCD  EQU   18                  WORK CODE                                    
RTDRECAP EQU   19                  RECAP                                        
RTDYNO   EQU   20                  YES, NO, OR ONLY                             
RTDAUTH  EQU   21                  AUTHORIZATION                                
*TDRCON  EQU   22                  RECONCILED ITEMS                             
*                                                                               
RTNGTXNO DS    XL2                 GETTEXT NUMBER                               
RTNLNQ   EQU   *-RTND                                                           
         EJECT ,                                                                
*ACAPGEQU                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACAPGEQU                                                       
         PRINT ON                                                               
*DDSOFDATD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSOFDATD                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
         EJECT ,                                                                
DMREQHDR DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE8D                                                       
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACSCR16   10/10/19'                                      
         END                                                                    
