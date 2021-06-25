*          DATA SET ACPRO02    AT LEVEL 061 AS OF 07/23/14                      
*PHASE T60B02A                                                                  
*INCLUDE ACSRCHP                                                                
*INCLUDE SRCHPASS                                                               
         TITLE 'T60B02 - PRODUCT RECORD MAINTENANCE'                            
T60B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B02**,R7,RR=R2                                   
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         ST    R2,MYRELO                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     RE,=V(ACSRCHP)                                                   
         A     RE,MYRELO                                                        
         ST    RE,ACSRCHP                                                       
*                                                                               
         L     RE,=A(PROOFBLK)                                                  
         A     RE,MYRELO                                                        
         ST    RE,APROOFBK                                                      
*                                                                               
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY             VALIDATE COMPANY, CLIENT AND PRODUCT         
         CLI   ACTNUM,ACTDEL       TRYING TO DELETE?                            
         BE    DELETIT             YES, SEE IF OK                               
         CLI   ACTNUM,ACTADD       CAN DO EXTRA ON ADD                          
         BNE   *+8                                                              
         BAS   RE,PROCPF                                                        
         B     RESETKEY                                                         
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VKEY             VALIDATE COMPANY, CLIENT AND PRODUCT         
         CLI   ACTNUM,ACTCHA                                                    
         BNE   *+8                                                              
         BAS   RE,PROCPF                                                        
         CLI   RECNUM,RECNXPRO                                                  
         BNE   MODE2A                                                           
         BAS   RE,VREC2            VALIDATE EXTRA DETAILS                       
         BAS   RE,DREC2            DISPLAY EXTRA DETAILS                        
         B     RESETKEY                                                         
*                                                                               
MODE2A   BAS   RE,VREC             VALIDATE RECORD                              
         BAS   RE,DREC             DISPLAY NEW RECORD                           
         B     RESETKEY                                                         
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY             DISPLAY KEY                                  
         BAS   RE,VKEY             VALIDATE COMPANY, CLIENT AND PRODUCT         
         B     RESETKEY                                                         
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,PROCPF                                                        
         CLI   RECNUM,RECNXPRO                                                  
         BNE   MODE6A                                                           
         BAS   RE,DREC2            DISPLAY EXTRA DETAILS                        
         B     RESETKEY                                                         
*                                                                               
MODE6A   BAS   RE,DREC             DISPLAY RECORD                               
         B     RESETKEY                                                         
*                                                                               
MODE8    CLI   MODE,XRECPUT        DID I PUT A RECORD                           
         BE    *+12                                                             
         CLI   MODE,XRECADD                                                     
         BNE   OKEXIT                                                           
*                                                                               
         GOTO1 CHKNAME,DMCB,AIO,OLDPRONM SEE IF I CHANGED NAME                  
         BAS   RE,NAMESRCH                                                      
*                                                                               
RESETKEY MVC   KEY,SAVEKEY                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                          DISPLAY KEY                                *         
***********************************************************************         
*                                                                               
DKEY     NTR1                                                                   
         MVC   AIO,AIO2            SWAP IO AREAS                                
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVC   PRRCLACH+5(1),LCLI                                               
         OI    PRRCLACH+6,X'80'                                                 
         MVC   PRRSJACH+5(1),LPRO                                               
         OI    PRRSJACH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         LA    R6,3(R6)            BREAK KEY DOWN INTO CLIENT                   
         SR    R1,R1                AND PRODUCT                                 
         IC    R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRRCLAC(0),0(R6)                                                 
         LA    R6,1(R1,R6)                                                      
         IC    R1,LPRO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   PRRSJAC(0),0(R6)                                                 
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         MVC   AIO,AIO2            SWAP IO AREA                                 
         MVC   KEY,SPACES         VERIFY COMPANY FIRST                          
         MVC   KEY(1),CUL          AND GET LEDGERS AND                          
         GOTO1 READ                 STATUS                                      
         MVI   ELCODE,ACMPELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACCOMPD,R6                                                       
         MVC   PRODLEDG,ACMPJOB                                                 
         MVC   RECVLEDG,ACMPRECV                                                
         MVC   COMPSTAT,ACMPSTAT                                                
*                                                                               
         LA    R2,PRRCLACH         ADDRESS CLIENT                               
         OI    6(R2),X'80'                                                      
         MVI   OPTION,C'Y'         TURN OPTION ON TO GET NAME                   
         GOTO1 VALCLI              VALIDATE CLIENT,                             
*                                                                               
         LA    R2,PRRCLF1H                                                      
         LA    R0,NFLTS                                                         
         LA    R1,CLFLTS                                                        
*                                                                               
VKEY02   MVC   8(1,R2),0(R1)                                                    
         OI    6(R2),X'80'                                                      
         LA    R1,1(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,VKEY02                                                        
*                                                                               
         MVC   PRRCLOF,CLIOFFC                                                  
         OI    PRRCLOFH+6,X'80'                                                 
*                                                                               
         LA    R2,PRRSJACH         ADDRESS PRODUCT                              
         MVI   ERROR,INVALID                                                    
         SR    R1,R1                                                            
         ICM   R1,1,PRRSJACH+5                                                  
         CH    R1,=H'1'                                                         
         BNH   ERREXIT                                                          
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ACTNUM,ACTREST      ARE WE RESTORING?                            
         BE    VKEY06              YES, SKIP VALPROD                            
         CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BNE   VKEY04              NO                                           
         CLC   8(3,R2),=C'ALL'     PRODUCT = ALL NOT VALID FOR ADD              
         BE    ERREXIT                                                          
         B     VKEY06              SKIP VALPROD                                 
*                                                                               
VKEY04   MVI   OPTION,C'N'         TURN OFF OPTION TO GET NAME                  
         GOTO1 VALPROD                                                          
*                                                                               
VKEY06   GOTO1 ANY                                                              
         CLC   5(1,R2),LPRO        VERIFY PRODUCT TO HIERARCHY                  
         MVI   ERROR,TOOLONG                                                    
         BH    ERREXIT                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   PRODCODE,WORK                                                    
*                                                                               
         MVC   AIO,AIO1            SWAP IO BACK                                 
         MVC   KEY,SPACES          PREPARE KEY FOR GENCOM                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE     MOVE IN CLIENT                              
         SR    R1,R1                                                            
         IC    R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),WORK         MOVE IN PRODUCT                             
         MVC   SAVEKEY,KEY         SAVE FOR EXIT                                
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         CLI   RECNUM,RECNXPRO                                                  
         BE    PROCPFX                                                          
         LA    R2,PRRSJACH                                                      
         CLI   PFKEY,PF1                                                        
         BNE   PROCPFX                                                          
*                                                                               
PROCPF1  CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BNE   *+8                 NO                                           
         MVI   ACTNUM,ACTCHA       YES, MAKE IT A CHANGE                        
         SR    RF,RF                                                            
         IC    RF,ACTNUM                                                        
         GOTO1 VCALL,WORK,RECNXPRO,(RF),(6,CLICODE),(6,PRODCODE),0              
*                                                                               
PROCPFX  B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY RECORD                               *         
***********************************************************************         
*                                                                               
DREC     NTR1                                                                   
         TWAXC PRRSJNMH                                                         
DISSJ    MVC   THISLEDG,PRODLEDG                                                
         LA    R2,PRRSJNMH           SUB-DEPT                                   
         BAS   RE,DISFILT           FILTERS, OFFICE AND                         
         GOTO1 NAMEOUT                                                          
         OI    4(R2),X'20'                                                      
         MVI   PRRSRACH+5,0        CLEAR RECEIVABLE AND COSTING                 
         MVI   PRR1CACH+5,0          ACCOUNT LENGTHS                            
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISADDR                                                          
*                                                                               
         USING ACPROFD,R6                                                       
DISSJOF  MVC   PRRSJOF,ACPROFFC     AND GET IT FROM PROFILE                     
*                                                                               
DGETSRAC CLI   ACPRRECV,C' '       IF NO RECEIVABLE ACCOUNT -                   
         BE    DGET1CAC             LOOK FOR COSTING                            
         CLI   ACPRRECV,0                                                       
         BE    DGET1CAC                                                         
         MVC   SAVESRLG,ACPRRECV+1 SAVE THE LEDGER                              
         MVC   PRRSRAC,ACPRRECV+3  MOVE KEY TO SCREEN                           
         MVI   PRRSRACH+5,12        AND FORCE THE LENGTH FOR VALACCT            
*                                                                               
DGET1CAC OI    PRRSRACH+4,X'20'    SET CHANGE INDICATOR                         
         CLI   ACPRCOST,C' '       IF NO COSTING ACCOUNT -                      
         BE    DISPBIL              DISPLAY DATA TO PRINT ON BILL               
         CLI   ACPRCOST,0                                                       
         BE    DISPBIL                                                          
         MVC   SAVE1CLG,ACPRCOST+1                                              
         MVC   PRR1CAC,ACPRCOST+3                                               
         MVI   PRR1CACH+5,12                                                    
*                                                                               
DISPBIL  OI    PRR1CACH+4,X'20'    SET CHANGE INDICATOR                         
         MVC   PRRPBIL,ACPRBLPR    GET DATA TO PRINT ON BILL                    
*                                                                               
DISINFO  CLI   ACPRLEN,105         GET ADDITIONAL INFORMATION                   
         BE    DISADDR                                                          
         MVC   PRRHED1,ACPRNARR                                                 
         CLI   ACPRLEN,155                                                      
         BE    DISADDR                                                          
         MVC   PRRHED2,ACPRNARR+50                                              
         CLI   ACPRLEN,205                                                      
         BE    DISADDR                                                          
         MVC   PRRHED3,ACPRNARR+100                                             
*                                                                               
DISADDR  LA    R2,PRRADR1H         GET BILLING ADDRESS                          
         GOTO1 ADDROUT                                                          
*                                                                               
         USING ACSAND,R6                                                        
DGETSAAC MVI   PRRSAACH+5,0                                                     
         MVI   ELCODE,X'3D'        GET SALES ANALYSIS ELEMENT                   
         BAS   RE,GETELIO                                                       
         BNE   DISLAST             IF NO ELEMENT, OR FIELD BLANK,               
         CLI   ACSACODE,C' '        GO DISPLAY LAST ACTIVITY DATE               
         BE    DISLAST                                                          
         MVC   SAVESALG,ACSACODE+1                                              
         MVC   PRRSAAC,ACSACODE+3                                               
         MVC   PRRSAACH+5(1),LLEVAB                                             
         DROP  R6                                                               
*                                                                               
DISLAST  OI    PRRSAACH+4,X'20'                                                 
         GOTO1 PERSOUT             DATE OF LAST ACTIVITY                        
         LA    R2,PRRLACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FROM NOW UNTIL XIT                  
*                                                                               
DISSRAC  CLI   PRRSRACH+5,0                                                     
         BE    DIS1CAC                                                          
         MVC   THISLEDG,SAVESRLG                                                
         LA    R2,PRRSRACH         VALIDATE ACCOUNT, DISPLAY NAME,              
         BAS   RE,VALACCT           OFFICE, FILTERS AND SUB-DEPT                
         LA    R2,PRRSRNMH                                                      
         BAS   RE,DISFILT                                                       
         GOTO1 NAMEOUT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DIS1CAC  CLI   PRR1CACH+5,0                                                     
         BE    DISSAAC                                                          
         MVC   THISLEDG,SAVE1CLG                                                
         LA    R2,PRR1CACH         VALIDATE ACCOUNT, DISPLAY NAME,              
         BAS   RE,VALACCT           OFFICE, FILTERS AND SUB-DEPT                
         LA    R2,PRR1CNMH                                                      
         BAS   RE,DISFILT                                                       
         GOTO1 NAMEOUT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DISSAAC  CLI   PRRSAACH+5,0                                                     
         BE    DSWAP                                                            
         MVC   THISLEDG,SAVESALG                                                
         LA    R2,PRRSAACH         VALIDATE ACCOUNT, DISPLAY NAME,              
         BAS   RE,VALACCT           OFFICE, FILTERS AND SUB-DEPT                
         LA    R2,PRRSANMH                                                      
         BAS   RE,DISFILT                                                       
         GOTO1 NAMEOUT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
         USING ACPROFD,R6                                                       
DISSAOF  MVI   ELCODE,ACPRELQ      GET SALES ANALYSIS PROFILE                   
         BAS   RE,GETELIO                                                       
         BNE   *+10                                                             
         MVC   PRRSAOF,ACPROFFC                                                 
*                                                                               
DSWAP    MVC   AIO,AIO1            USE AIO1 NOW                                 
         MVC   CUL+1(2),=C'SJ'     RESET IN CASE LOST                           
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY EXTRA DETAILS                        *         
***********************************************************************         
*                                                                               
DREC2    NTR1                                                                   
         TWAXC PRXSJNMH                                                         
         MVC   THISLEDG,PRODLEDG                                                
         LA    R2,PRXSJNMH                                                      
         BAS   RE,DISFIL2          GET FILTERS                                  
         GOTO1 NAMEOUT             DISPLAY CLIENT NAME                          
*                                                                               
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISNAME                                                          
         USING ACPROFD,R6                                                       
         MVC   PRXSJOF,ACPROFFC    DISPLAY OFFICE                               
         OI    PRXSJOFH+6,X'80'                                                 
*                                                                               
DISNAME  GOTO1 PERSOU2             DATE OF LAST ACTIVITY                        
         LA    R2,PRXLACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         USING ACOMMD,R6                                                        
DISFOOT  MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETELIO                                                       
         BNE   DISEQU                                                           
         LA    R2,PRXFT1H                                                       
         LA    R0,3                                                             
         MVI   DUB,C'N'            N=NORMAL COMMENTS ON THIS LINE               
         MVI   DUB+1,1             NOTHING YET IN LINE                          
         LA    R3,8(R2)            ADDRESS FIRST BYTE IN LINE                   
*                                                                               
DISF020  CLI   ACOMTYPE,C'M'                                                    
         BE    *+12                                                             
         CLI   ACOMTYPE,0                                                       
         BNE   DISF060                                                          
         CLI   DUB,C'N'                                                         
         BE    DISF040                                                          
         BAS   RE,BUMPFT           GET NEXT LINE                                
         MVI   DUB,C'N'                                                         
*                                                                               
DISF040  SR    R1,R1                                                            
         IC    R1,ACOMLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACOMMENT                                                 
         BAS   RE,BUMPFT                                                        
         B     DISF120                                                          
*                                                                               
DISF060  LA    RE,ACOMMENT                                                      
         LA    R1,6                                                             
*                                                                               
DISF080  CLI   0(RE),C' '                                                       
         BNE   DISF100                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,DISF080                                                       
         B     DISF120                                                          
*                                                                               
DISF100  MVI   DUB,C'Y'            MARK AS NEW STYLE COMMENTS                   
         STC   R1,DUB+2            R1 = L'COMMENT                               
         ST    RE,DUB+4            RE = A(1ST SIGNIFICANT BYTE)                 
         AH    R1,=H'5'            R1 = TOTAL LENGTH DISPLAYED ITEM             
         AR    R1,R3               R1 = A(LAST BYTE + 1 OF DSPLYD ITEM)         
         LR    RE,R2               RE = A(HEADER)                               
         SR    R1,RE                                                            
         CH    R1,=H'58'                                                        
         BNH   *+8                 IF NOT SUFFICIENT ROOM ON THIS LINE          
         BAS   RE,BUMPFT            BUMP TO NEXT                                
         CLI   DUB+1,1                                                          
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVI   DUB+1,2                                                          
         MVC   0(4,R3),=C'EST='    SET TYPE OF COMMENT                          
         TM    ACOMTYPE,X'40'                                                   
         BO    *+10                                                             
         MVC   0(3,R3),=C'BIL'                                                  
         TM    ACOMTYPE,X'C0'                                                   
         BM    *+10                                                             
         MVC   0(3,R3),=C'B+E'                                                  
         L     RE,DUB+4                                                         
         SR    R1,R1                                                            
         IC    R1,DUB+2                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),0(RE)      DISPLAY NUMBER                                
         LA    R3,5(R1,R3)                                                      
*                                                                               
DISF120  SR    R1,R1              SEE IF MORE ELEMENTS                          
         IC    R1,ACOMLEN                                                       
         AR    R6,R1                                                            
         CLI   ACOMEL,0                                                         
         BE    DISEQU                                                           
         CLI   ACOMEL,X'3E'                                                     
         BE    DISF020                                                          
         B     DISF120                                                          
*                                                                               
BUMPFT   IC    R1,0(R2)           BUMP TO NEXT LINE                             
         AR    R2,R1                                                            
         LA    R3,8(R2)                                                         
*                                                                               
         TM    1(R2),X'20'         PROTECTED                                    
         BNO   BUMPFT20            YES, SKIP TWO                                
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R3,8(R2)                                                         
         B     BUMPFT                                                           
*                                                                               
BUMPFT20 MVI   DUB+1,1                                                          
         BCT   R0,*+8                                                           
         B     *+6                                                              
         BR    RE                                                               
*                                                                               
DISEQU   TWAXC PRXLAB1H,PRXEQU5H   CLEAR ALL ACCOUNT EQUIVALENCIES              
*                                                                               
         USING EQUIVD,R2                                                        
         LA    R2,PRXLAB1H                                                      
         LA    R1,EQUMAX                                                        
*                                                                               
DISE02   OI    EQUACCH+1,X'20'     PROTECT THE SCREEN FIRST                     
         LA    R2,EQULNQ(R2)                                                    
         BCT   R1,DISE02                                                        
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES          CLEAR KEY                                    
         MVC   KEY(3),CUL          COMPANY, UNIT, LEDGER                        
         GOTO1 READ                                                             
*                                                                               
         MVI   ELCODE,APRELQ       ANY RULES RECORDS?                           
         BAS   RE,GETELIO                                                       
         BNE   DISACCM             NO, DO ACCOUNT MEMO                          
*                                                                               
         USING APRELD,R6                                                        
DISE04   LA    R2,PRXEQU1H         YES, FIND CORRECT ENTRY                      
         SR    R1,R1               SEQUENCE NUMBER                              
         IC    R1,APRSEQ           TIMES LENGTH OF AN ENTRY                     
         MHI   R1,PRXEQU2H-PRXEQU1H                                             
         AR    R2,R1               ADDED TO FIRST ENTRY                         
         MVC   8(L'APRDESC,R2),APRDESC                                          
         OI    6(R2),X'80'         TRANSMIT IT                                  
         BAS   RE,BUMP             GET TO DATA FIELD                            
         NI    1(R2),X'FF'-X'20'   UNPROTECT IT                                 
*                                                                               
DISE06   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    DISE08                                                           
         CLI   0(R6),APRELQ                                                     
         BE    DISE04                                                           
         B     DISE06                                                           
         DROP  R6                                                               
*                                                                               
DISE08   MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE    PRINT CLIENT DATA FIRST                      
         GOTO1 READ                                                             
         MVI   ELCODE,FFTELQ       READ THE DATA AND FILL IN THE SCREEN         
         BAS   RE,GETELIO                                                       
         BNE   DISE16                                                           
*                                                                               
         USING FFTELD,R6                                                        
DISE10   CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BE    DISE14                                                           
*                                                                               
DISE12   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    DISE16                                                           
         CLI   0(R6),FFTELQ        X'DB' FREE FORM TEXT                         
         BE    DISE10                                                           
         B     DISE12                                                           
*                                                                               
DISE14   LA    R2,PRXACC1H         POINT WHERE TO PUT THE ACCOUNT               
         SR    R1,R1                                                            
*                                                                               
         IC    R1,FFTSEQ                                                        
         MHI   R1,PRXACC2H-PRXACC1H                                             
         AR    R2,R1               ADD DISPLACEMENT TO IT                       
         LR    RE,R2                                                            
*                                                                               
         MVI   8(RE),C'('          PUT BRACKETS FOR THIS LEVEL                  
         LA    RE,1(RE)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FFTDLEN          GET LENGTH OF ACCOUNT TO OUTPUT              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),FFTDATA                                                  
         LA    R1,1(R1)                                                         
         AR    RE,R1                                                            
         MVI   8(RE),C')'                                                       
         OI    6(R2),X'80'         TRANSMIT IT                                  
         B     DISE12              CHECK IF MORE EQUIVALENT ACCOUNTS            
*                                                                               
DISE16   MVC   AIO,AIO1            RESTORE IO AREA                              
         MVI   ELCODE,FFTELQ       READ THE DATA AND FILL IN THE SCREEN         
         BAS   RE,GETELIO                                                       
         BNE   DISACCM                                                          
*                                                                               
         USING FFTELD,R6                                                        
DISE18   CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BE    DISE22                                                           
*                                                                               
DISE20   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    DISACCM                                                          
         CLI   0(R6),FFTELQ        X'DB' FREE FORM TEXT                         
         BE    DISE18                                                           
         B     DISE20                                                           
*                                                                               
DISE22   LA    R2,PRXACC1H         POINT WHERE TO PUT THE ACCOUNT               
         SR    R1,R1                                                            
*                                                                               
         IC    R1,FFTSEQ                                                        
         MHI   R1,PRXACC2H-PRXACC1H                                             
         AR    R2,R1               ADD DISPLACEMENT TO IT                       
         MVC   8(L'PRXACC1,R2),SPACES                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FFTDLEN          GET LENGTH OF ACCOUNT TO OUTPUT              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FFTDATA                                                  
         OI    6(R2),X'80'         TRANSMIT IT                                  
         NI    1(R2),X'FF'-X'20'   UNPROTECT IT                                 
         B     DISE20              CHECK IF MORE EQUIVALENT ACCOUNTS            
*                                                                               
DISACCM  MVC   AIO,AIO1            RESTORE AIO                                  
         MVI   ELCODE,OMEELQ       ACCOUNT MEMO                                 
         BAS   RE,GETELIO                                                       
         BNE   DISPGRP                                                          
         USING OMEELD,R6                                                        
         ZIC   RF,OMELN            ON-LINE MEMO DATA?                           
         AHI   RF,-3                                                            
         CHI   RF,L'PRXACM-1       CHECK IF WE EXCEEDED FIELD LENGTH            
         BL    *+8                                                              
         LHI   RF,L'PRXACM-1       SET TO MAX FIELD LENGTH                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PRXACM(0),OMEMO                                                  
         OC    PRXACM,SPACES                                                    
         OI    PRXACMH+6,X'80'                                                  
*                                                                               
DISPGRP  MVI   ELCODE,OTHELQ       DISPLAY PRODUCT GROUP                        
         BAS   RE,GETELIO                                                       
         BNE   DREC2XIT                                                         
         USING OTHELD,R6                                                        
         CLC   OTHNUM,SPACES                                                    
         BE    DREC2XIT                                                         
         MVC   PRXPGR,OTHNUM                                                    
         OC    PRXPGR,SPACES                                                    
         OI    PRXPGRH+6,X'80'                                                  
*                                                                               
DREC2XIT B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VREC     NTR1                                                                   
         LA    R2,PRRSJACH                                                      
         MVC   THISLEDG,PRODLEDG                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VALSJ                                                            
         USING ACKEYD,R6                                                        
         L     R6,AIO              AIO HAS RECORD KEY ALREADY, JUST             
         MVC   ACLENGTH,=H'50'      INSERT THE LENGTH                           
VALSJ    LA    R2,PRRSJNMH                                                      
         GOTO1 ANY                                                              
*                                                                               
         CLI   ACTNUM,ACTCHA       DO I WANT TO CHANGE                          
         BNE   VALSJ20                                                          
         GOTO1 SVNAME,DMCB,OLDPRONM SAVE NAME BEFORE YOU DO                     
*                                                                               
VALSJ20  BAS   RE,VALNAME                                                       
         BAS   RE,VALFILT                                                       
*                                                                               
VALSJOF  LA    R2,PRRSJOFH         VALIDATE OFFICE                              
         CLI   5(R2),0                                                          
         BE    VSJO020                                                          
         TM    COMPSTAT,X'20'                                                   
         BZ    VSJO020             NO VALIDATION FOR NON-OFFICE COMP.           
         LA    RE,MYBUFF           USE MY BUFFER FOR OFFICE                     
         ST    RE,AIO                                                           
         LA    R6,KEY                                                           
         USING ACOGKEY,R6                                                       
         XC    ACOGKEY,ACOGKEY     BUILD OFFICE KEY                             
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         GOTO1 ANY                                                              
         MVC   ACOGOFC,WORK                                                     
         GOTO1 HIGH                READ FOR OFFICE.                             
         CLC   ACOGKEY,KEYSAVE                                                  
         BNE   VSJO010             NO OFFICE RECORD                             
*                                                                               
         TM    COMPSTA4,X'01'      ON NEW OFFICES?                              
         BZ    VSJO005             NO, SKIP NEXT VALIDATION                     
         LA    R6,KEY              VERIFY GENERAL ACCOUNTING OFFICE             
         USING OFFRECD,R6                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUL                                                      
         MVC   OFFKOFF,WORK                                                     
         MVI   ERROR,GENOFERR                                                   
         GOTO1 HIGH                                                             
         CLC   OFFKEY,KEYSAVE      OFFICE NOT DEFINED                           
         BNE   ERREXIT                                                          
*                                                                               
         L     R6,AIO              NOW CHECK FOR LIST                           
         MVI   ERROR,LISTOFF                                                    
         TM    OFFKEY+(ACSTATUS-ACKEYD),OFFSLIST                                
         BO    ERREXIT                                                          
*                                                                               
VSJO005  MVC   AIO,AIO1            CHANGE BUFFERS BACK                          
         B     VSJO020                                                          
*                                                                               
VSJO010  MVI   ERROR,INVALID       OFFICE NOT FOUND.                            
         B     ERREXIT                                                          
*                                                                               
VSJO020  MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    VSJO040                                                          
         BAS   RE,ADDPROF                                                       
         B     VSJO020                                                          
*                                                                               
         USING ACPROFD,R6                                                       
VSJO040  MVC   OLDOFF,ACPROFFC     SAVE OLD OFFICE                              
         CLI   5(R2),0                                                          
         BNE   VSJO060                                                          
         MVI   ACPRUNIT,0                                                       
         MVC   ACPROFFC,SPACES                                                  
         B     VGETSRAC                                                         
*                                                                               
VSJO060  GOTO1 ANY                                                              
         MVC   ACPRUNIT,WORK                                                    
         MVC   ACPROFFC,WORK                                                    
*                                                                               
VGETSRAC MVC   NEWOFF,ACPROFFC     SAVE NEW OFFICE CODE                         
         SR    R1,R1               SAVE DATA TO VALIDATE RECEIVABLE             
         ICM   R1,1,PRRSRACH+5                                                  
         BNZ   VGSR020                                                          
         XC    ACPRRECV,ACPRRECV   IF NO RECEIVABLE, CLEAR SCREEN               
         XC    PRRSRNM,PRRSRNM                                                  
         XC    PRRSROF,PRRSROF                                                  
         LA    R2,PRRSRF1H                                                      
         BAS   RE,CLRFLTS          CLEAR THE 5 1 BYTE FIELDS                    
         B     VGET1CAC                                                         
*                                                                               
VGSR020  BCTR  R1,0                                                             
         MVC   ACPRRECV,SPACES                                                  
         MVC   ACPRRECV(1),CUL                                                  
         MVC   ACPRRECV+1(2),RECVLEDG                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRRECV+3(0),PRRSRAC MOVE IN RECV+3BL ACCOUNT CODE.             
*                                                                               
VGET1CAC SR    R1,R1               SAVE DATA TO VALIDATE COSTING                
         ICM   R1,1,PRR1CACH+5                                                  
         BNZ   VG1C020                                                          
         XC    ACPRCOST,ACPRCOST                                                
         XC    PRR1CNM,PRR1CNM                                                  
         XC    PRR1COF,PRR1COF                                                  
         LA    R2,PRR1CF1H                                                      
         BAS   RE,CLRFLTS          CLEAR THE 5 1 BYTE FIELDS                    
         B     VG1C040                                                          
*                                                                               
VG1C020  MVC   ACPRCOST,SPACES                                                  
         MVC   ACPRCOST(1),CUL                                                  
         MVC   ACPRCOST+1(2),=C'1C'                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRCOST+3(0),PRR1CAC MOVE IN COST+3ING ACCOUNT CODE.            
*                                                                               
VG1C040  BAS   RE,VALPOFC          UPDATE PRODUCTS OFFICE CODE ELEMENT          
*                                                                               
VALPBIL  MVC   ACPRBLPR,SPACES     VALIDATE DATA TO PRINT ON BILL               
         SR    R1,R1                                                            
         ICM   R1,1,PRRPBILH+5                                                  
         BZ    VALINFO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRBLPR(0),PRRPBIL                                              
*                                                                               
VALINFO  MVI   ELEMENT+105,C' '      VALIDATE ADDITIONAL INFORMATION            
         MVC   ELEMENT+106(149),ELEMENT+105                                     
         MVC   ELEMENT(105),ACPREL PRESERVE START OF OLD ELEMENT.               
         LA    R6,ELEMENT                                                       
         GOTO1 REMELEM                                                          
         MVI   ACPRNARR,C' '                                                    
         MVC   ACPRNARR+1(L'ACPRNARR-1),ACPRNARR                                
         LA    RF,ACPRNARR         RF = A(START OF NARRATIVE).                  
         LA    R2,PRRHED1H         R2 = A(1ST INFO HEADER).                     
         LA    R0,3                R0 = N'INFO LINES POSSIBLE.                  
VALI040  SR    R1,R1                                                            
         ICM   R1,1,5(R2)            R1 = L'THIS LINE.                          
         BZ    VALI060                                                          
         BCTR  R1,0                                                             
         EX    R1,VALIMVC          MVC   0(0,R6),8(R2)                          
         LA    RF,50(RF)           RF = A(NEXT LYN POST'N IN EL).               
         IC    R1,0(R2)            R1 = L'HDR + L'FLD.                          
         AR    R2,R1               R2 = A(NEXT HDR)                             
         BCT   R0,VALI040                                                       
VALI060  LA    R2,3                DERIVE N'LINES I/P.                          
         SR    R2,R0               TOTAL L'INPUT IS N'LINES * 50.               
         MH    R2,=H'50'                                                        
         AH    R2,=H'105'          TOTAL L'ELEMENT = L'INPUT + 105.             
         STC   R2,ACPRLEN                                                       
         GOTO1 ADDELEM              ADD THE ELEMENT.                            
         B     *+10                                                             
VALIMVC  MVC   0(0,RF),8(R2)                                                    
         DROP  R6                                                               
*                                                                               
VALADDR  LA    R2,PRRADR1H         VALIDATE BILLING ADDRESS                     
         GOTO1 ADDRIN                                                           
*                                                                               
VGETSAAC MVI   ELCODE,X'3D'        SAVE DATA TO VALIDATE SALES ANALYSIS         
         GOTO1 REMELEM                                                          
         LA    R2,PRRSAACH                                                      
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BNZ   VGSA020                                                          
         XC    PRRSANM,PRRSANM                                                  
         XC    PRRSAOF,PRRSAOF                                                  
         BAS   RE,CLRFLTS                                                       
         B     VALLAST                                                          
*                                                                               
VGSA020  MVI   ERROR,DUPINPUT                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAVEKEY+3(0),PRRSAAC   EQUAL TO PRODUCT CODE, ERROR              
         BE    ERREXIT                                                          
*                                                                               
VALLAST  GOTO1 PERSIN                                                           
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FROM NOW UNTIL XIT                  
*                                                                               
VALSRAC  LA    R2,PRRSRACH         VALIDATE RECEIVABLE                          
         MVC   THISLEDG,RECVLEDG                                                
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VAL1CAC                                                          
         BAS   RE,GETFUNC                                                       
         LA    R2,PRRSRNMH                                                      
         BAS   RE,VALUPDT                                                       
*                                                                               
VAL1CAC  LA    R2,PRR1CACH         VALIDATE COSTING ACCOUNTS                    
         MVC   THISLEDG,=C'1C'                                                  
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VAL29AC                                                          
         BAS   RE,GETFUNC                                                       
         LA    R2,PRR1CNMH                                                      
         BAS   RE,VALUPDT                                                       
*                                                                               
VAL29AC  LA    R2,PRR1CACH                                                      
         MVC   THISLEDG,=C'29'                                                  
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VALSAAC                                                          
         BAS   RE,GETFUNC                                                       
         LA    R2,PRR1CNMH                                                      
         BAS   RE,VALUPDT                                                       
*                                                                               
VALSAAC  LA    R2,PRRSAACH         VALIDATE SALES ANALYSIS                      
         MVC   THISLEDG,PRODLEDG                                                
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VSWAP                                                            
         BAS   RE,GETFUNC                                                       
VALSAOF  LA    R2,PRRSAOFH                                                      
         CLI   5(R2),0                                                          
         BE    VSAO020                                                          
         TM    COMPSTAT,X'20'                                                   
         BZ    VSAO020                                                          
         LA    RE,MYBUFF           USE MY BUFFER FOR OFFICE                     
         ST    RE,AIO                                                           
         LA    R6,KEY                                                           
         USING ACOGKEY,R6                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         GOTO1 ANY                                                              
         MVC   ACOGOFC,WORK                                                     
         GOTO1 HIGH                READ FOR OFFICE.                             
         MVC   AIO,AIO2            PUT BUFFER BACK                              
         CLC   ACOGKEY,KEY                                                      
         BE    VSAO020                                                          
         MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
VSAO020  MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    VSAO040                                                          
         BAS   RE,ADDPROF                                                       
         B     VSAO020                                                          
*                                                                               
         USING ACPROFD,R6                                                       
VSAO040  CLI   5(R2),0                                                          
         BE    VSAO060                                                          
         GOTO1 ANY                                                              
         MVC   ACPRUNIT,WORK                                                    
         MVC   ACPROFFC,WORK                                                    
VSAO060  LA    R2,PRRSANMH                                                      
         BAS   RE,VALUPDT                                                       
         DROP  R6                                                               
*                                                                               
VSWAP    MVC   AIO,AIO1            USE AIO1 NOW                                 
         MVC   CUL+1(2),=C'SJ'     RESET IN CASE LOST                           
         LA    R2,PRRSAACH         CREATE SALES ANALYSIS ELEMENT                
         CLI   5(R2),0                                                          
         BE    OKEXIT                                                           
         USING ACSAND,R6                                                        
         LA    R6,ELEMENT                                                       
         MVI   ACSAEL,X'3D'                                                     
         MVI   ACSALN,X'35'                                                     
         MVC   ACSACODE(1),CUL                                                  
         MVC   ACSACODE+1(2),PRODLEDG                                           
         MVC   ACSACODE+3(12),PRRSAAC                                           
         MVC   ACSANAME,PRRSANM                                                 
         OC    ACSACODE+1(50),SPACES                                            
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE EXTRA DETAILS                        *         
***********************************************************************         
*                                                                               
VREC2    NTR1                                                                   
*                                                                               
         BAS   RE,VALSTAT                                                       
*                                                                               
VALFT1   LA    R2,PRXFT1H          FOOTER                                       
         MVI   ELCODE,X'3E'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'3E'                                                    
         BAS   RE,VALFOOT                                                       
*                                                                               
VALFT2   LA    R2,PRXFT2H                                                       
         BAS   RE,VALFOOT                                                       
*                                                                               
VALFT3   LA    R2,PRXFT3H                                                       
         BAS   RE,VALFOOT                                                       
*                                                                               
         GOTO1 PERSIN2             UPDATE PERSON/ACTIVITY                       
*                                                                               
VALEQU   L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
*                                                                               
         USING FFTELD,R6                                                        
VEQU02   CLI   0(R6),0             DELETE TYPE 71 ELEMENTS                      
         BE    VEQU08                                                           
         CLI   0(R6),FFTELQ                                                     
         BNE   VEQU04                                                           
         CLI   FFTTYPE,FFTTEPTR                                                 
         BE    VEQU06                                                           
*                                                                               
VEQU04   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     VEQU02                                                           
*                                                                               
VEQU06   MVI   0(R6),X'FF'         MARK THE ELEMENT                             
         MVI   ELCODE,X'FF'        PASS THE ELEMENT CODE                        
         GOTO1 REMELEM                                                          
         B     VEQU02                                                           
*                                                                               
VEQU08   MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES          READ FOR LEDGER RECORD                       
         MVC   KEY(3),CUL                                                       
         GOTO1 READ                                                             
*                                                                               
         USING APRELD,R5                                                        
         L     R5,AIO                                                           
         AH    R5,DATADISP                                                      
*                                                                               
VEQU10   CLI   0(R5),APRELQ        ANY RULES RECORDS?                           
         BE    VEQU14              YES, PROCESS IT                              
         CLI   0(R5),0             NO, ARE WE AT END                            
         BE    VALACCM             YES, DO ACCOUNT MEMO                         
*                                                                               
VEQU12   SR    R1,R1               GET NEXT                                     
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VEQU10                                                           
*                                                                               
VEQU14   MVC   AIO,AIO1            RESTORE IO AREA                              
         LA    R2,PRXEQU1H         INDEX INTO SCREEN BASED ON CA SEQ            
         SR    R1,R1                                                            
         IC    R1,APRSEQ           GET SEQUENCE OF CA ELEMENT                   
         MHI   R1,PRXEQU2H-PRXEQU1H                                             
         AR    R2,R1               POINT TO CORRECT ENTRY                       
*                                                                               
         IC    R1,7(R2)            GET LENGTH OF CODE                           
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                DESCRIPTION SHOULD BE THERE                  
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),APRDESC     DOES IT MATCH THE SCREEN?                    
         BE    *+6                 YES                                          
         DC    H'0'                NOPE, SOMETHING'S WRONG                      
*                                                                               
         BAS   RE,BUMP             BUMP TO DATA FIELD                           
         CLI   5(R2),0             ANY EQUIVALENCY?                             
         BE    VEQU12              NO, GET NEXT APREL                           
         CLI   8(R2),C'('          IS IT FROM PREVIOUS LEVEL?                   
         BE    VEQU12              YES, GET NEXT                                
         GOTO1 CKCARUL,DMCB,(R5),(R2)                                           
         BNZ   ERREXIT             DOESN'T MATCH RULE                           
*                                                                               
         USING FFTELD,R6                                                        
         LA    R6,ELEMENT          OK, ADD ELEMENT FOR IT                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTEPTR                                                 
         MVC   FFTSEQ,APRSEQ                                                    
         ZIC   RF,5(R2)                                                         
         STC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),8(R2)                                                 
         AHI   RF,FFTDATA-FFTELD+1                                              
         STC   RF,FFTLN                                                         
         GOTO1 ADDELEM                                                          
         B     VEQU12              GET NEXT RULES ELEMENT                       
*                                                                               
VALACCM  MVC   AIO,AIO1            RESTORE IN CASE CHANGED                      
         LA    R2,PRXACMH          ACCCOUNT MEMO                                
         MVI   ELCODE,OMEELQ                                                    
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         CLI   5(R2),0                                                          
         BE    VALPGRP                                                          
         USING OMEELD,R6                                                        
         LA    R6,ELEMENT                                                       
         MVI   ELEMENT,OMEELQ                                                   
         ZIC   RF,PRXACMH+5                                                     
         AHI   RF,2                                                             
         STC   RF,OMELN                                                         
         AHI   RF,-3                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   OMEMO(0),PRXACM                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
VALPGRP  LA    R2,PRXPGRH          PRODUCT GROUP                                
         MVI   ELCODE,OTHELQ                                                    
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         CLI   5(R2),0                                                          
         BE    VREC2XIT                                                         
         USING OTHELD,R6                                                        
         LA    R6,ELEMENT                                                       
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM,PRXPGR                                                    
         OC    OTHNUM,SPACES                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VREC2XIT B     OKEXIT                                                           
         EJECT                                                                  
*                                                                               
*  CLEAR THE 5 1 BYTE FIELDS STARTING AT THE FIELD HEADER POINTED TO            
*  BY R2                                                                        
*  USES R0, R2 AND RF                                                           
*                                                                               
CLRFLTS  EQU   *                                                                
         LA    R0,5                                                             
         XR    RF,RF                                                            
CF10     MVI   8(R2),0                                                          
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,CF10                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE ACCOUNT CODE, LEVEL ETC.                            
***********************************************************************         
*                                                                               
VALACCT  NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C' '          DOES ACCOUNT START WITH A BLANK ?            
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         MVC   CUL+1(2),THISLEDG                                                
         MVI   OPTION2,C'N'        DON'T CHECK SECURITY HERE                    
         GOTO1 SETHEIR                                                          
         MVI   OPTION2,C'Y'        RESET FOR NEXT TIME                          
         LA    R0,1                                                             
         LA    R6,LLEVA                                                         
VAC020   CLC   5(1,R2),0(R6)       FIND HIERARCHY LEVEL FOR ACCT CODE.          
         BNH   VAC040                                                           
         LA    R6,1(R6)                                                         
         AH    R0,=H'1'                                                         
         CH    R0,=H'4'                                                         
         BNH   VAC020                                                           
         MVI   ERROR,TOOLONG                                                    
         B     ERREXIT                                                          
*                                                                               
VAC040   CH    R0,=H'4'            MUST BE A LOW LEVEL ACCOUNT.                 
         BE    VAC080                                                           
         CLC   THISLEDG,PRODLEDG                                                
         BNE   VAC060              SALES ANALYSIS ACCOUNT MUST BE               
         CH    R0,=H'2'            AT PRODUCT LEVEL.                            
         BE    VAC080                                                           
         B     *+12                                                             
VAC060   CLI   1(R6),0                                                          
         BE    VAC080                                                           
         MVI   ERROR,WRNGLVAC      IT ISN'T.                                    
         B     ERREXIT                                                          
*                                                                               
VAC080   LA    R6,LLEVA            MAKE SURE HIGHER LEVEL ACCOUNTS XIST         
VAC100   BCT   R0,*+8                                                           
         B     VAC120              NO HIGHER LEVELS.                            
         MVC   KEY+1(L'KEY-1),SPACES                                            
         MVC   KEY+1(2),THISLEDG                                                
         LA    R6,LLEVA            FIND LENGTH OF THIS LEVEL.                   
         AR    R6,R0                                                            
         BCTR  R6,0                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)      R2 = A(ACCT CODE FIELD HEADER).              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BE    VAC100              FOUND THIS LEVEL OK.                         
         MVC   HIGHMSG+44(2),THISLEDG                                           
         MVC   CONHEAD(L'HIGHMSG),HIGHMSG    NO, HIGHER LEVEL MISSING           
         ST    R2,ACURFORC                                                      
         MVI   ERROR,X'FE'                                                      
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
VAC120   MVC   KEY+3(L'KEY-3),SPACES                                            
         SR    R1,R1               NOW READ THE ACCOUNT                         
         ICM   R1,1,5(R2)                                                       
         BZ    VAC140              NO INPUT, EXIT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)      MOVE IN KEY VALUE LESS CO/UNIT/LEDG.         
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(42),KEY                                                  
         BNE   EXIT                                                             
VAC140   L     R6,AIO                                                           
         NI    (ACSTATUS-ACKEYD)(R6),X'7F'                                      
         CLC   THISLEDG,PRODLEDG      SALES ANAL ACCOUNT?                       
         BE    VAC160                                                           
         MVI   ELCODE,ACBLELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    VAC160                                                           
         MVI   ERROR,INVPOST                                                    
         B     ERREXIT                                                          
*                                                                               
VAC160   GOTO1 SVNAME,DMCB,OLDACCNM                                             
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                      VALIDATE FOOT COMMENTS                         *         
***********************************************************************         
*                                                                               
         USING PRXFT1H,R2                                                       
         USING ACOMMD,R6                                                        
VALFOOT  NTR1                                                                   
         LA    R6,ELEMENT                                                       
         CLI   PRXFT1H+5,0                                                      
         BE    OKEXIT                                                           
         MVI   ACOMTYPE,0                                                       
         GOTO1 SCANNER,DMCB,PRXFT1H,BLOCK,0                                     
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VFT020                                                           
         CLI   BLOCK+1,0                                                        
         BNE   VFT040                                                           
*                                                                               
VFT020   SR    R1,R1                                                            
         IC    R1,PRXFT1H+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOMMENT(0),PRXFT1                                               
         LA    R1,5(R1)                                                         
         STC   R1,ACOMLEN                                                       
         GOTO1 ADDELEM                                                          
         SR    R1,R1                                                            
         IC    R1,ACOMSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ACOMSEQ                                                       
         B     OKEXIT                                                           
*                                                                               
VFT040   LA    R5,BLOCK                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),CUL                                                     
*                                                                               
VFT060   MVI   ACOMLEN,10                                                       
         MVI   ACOMTYPE,X'44'                                                   
         CLC   12(3,R5),=C'EST='                                                
         BE    VFT080                                                           
         MVI   ACOMTYPE,X'84'                                                   
         CLC   12(3,R5),=C'BIL'                                                 
         BE    VFT080                                                           
         CLC   12(3,R5),=C'B+E'                                                 
         BNE   VFT100                                                           
         OI    ACOMTYPE,X'40'                                                   
*                                                                               
VFT080   MVC   ACOMMENT,SPACES                                                  
         SR    R1,R1                                                            
         ICM   R1,1,1(R5)                                                       
         BZ    VFT100                                                           
         CLI   1(R5),6             MUST BE 1-6 BYTES LONG                       
         BH    VFT100                                                           
         LA    RF,6                                                             
         SR    RF,R1                                                            
         LA    RF,ACOMMENT(RF)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),22(R5)                                                   
         MVC   KEY+2(6),ACOMMENT                                                
         MVC   AIO,AIO2              USE OTHER BUFFER TO READ                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BNE   VFT100                                                           
         MVC   AIO,AIO1              GET JOB RECORD BACK                        
         GOTO1 ADDELEM                                                          
         SR    R1,R1                                                            
         IC    R1,ACOMSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ACOMSEQ                                                       
         LA    R5,32(R5)                                                        
         BCT   R0,VFT060                                                        
         B     OKEXIT                                                           
*                                                                               
VFT100   MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                           DISPLAY FILTERS                           *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING SCREEND,R2                                                       
DISFILT  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISFILTX                                                         
         LA    R2,ACF1H                                                         
         GOTO1 SETFLTS                                                          
*                                                                               
DISFILTX B     OKEXIT                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                           DISPLAY FILTERS, STATUS AND @TRACKER      *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING SCREEND,R2                                                       
DISFIL2  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISFIL2X                                                         
         LA    R2,ACF1H                                                         
         GOTO1 SETFLTS                                                          
*                                                                               
         XC    PRXSTAT,PRXSTAT     CLEAR STATUS                                 
         TM    RSTSTAT,X'20'       IS CLIENT LOCKED?                            
         BZ    *+12                NO                                           
         MVI   PRXSTAT,C'L'        YES, INDICATE IT                             
         OI    PRXSTATH+6,X'80'                                                 
*&&DO                                                                           
         MVI   PRXTRAK,C'N'        DEFAULT IS NO                                
         CLI   RSTLN,RSTLN3Q                                                    
         BL    DISFIL2X            ELEMENT SHOULD NEVER BE TOO SHORT            
         TM    RSTSTAT6,RSTSXF@T                                                
         BZ    *+8                                                              
         MVI   PRXTRAK,C'Y'                                                     
         OI    PRXTRAKH+6,X'80'                                                 
*&&                                                                             
DISFIL2X B     OKEXIT                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              BUILD A BLANK PROFILE ELEMENT                                    
***********************************************************************         
*                                                                               
         USING ACPROFD,R6                                                       
ADDPROF  NTR1                                                                   
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACPREL,ACPRELQ                                                   
         MVI   ACPRLEN,ACPRNARR-ACPROFD                                         
         MVC   ACPROFFC,SPACES                                                  
         MVC   ACPRUNBL,SPACES                                                  
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE FILTERS                              *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING SCREEND,R2                                                       
VALFILT  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    VALFILT2                                                         
*                                                                               
         LA    R6,ELEMENT                                                       
         XC    ELEMENT(RSTLN3Q),ELEMENT                                         
         MVI   RSTLN,RSTLN3Q       VALFLTS CHECKS LENGTH                        
         B     VALFILT4                                                         
*                                                                               
VALFILT2 CLI   RSTLN,RSTLN3Q       IS THIS BIG ENOUGH FOR FILTER VALS           
         BNL   VALFILT4            YES                                          
*                                                                               
         MVI   ELCODE,RSTELQ       EXTRACT RSTEL TO "ELEMENT"                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT          UPDATE LENGTH                                
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 ADDELEM                                                          
         BAS   RE,GETELIO                                                       
         BE    VALFILT4                                                         
         DC    H'0'                                                             
*                                                                               
VALFILT4 LA    R2,ACF1H                                                         
         XR    R1,R1               ZERO HOB OF R1 FOR VALIDATE ALL              
         GOTO1 VALFLTS                                                          
*                                                                               
         CLI   RSTEL,0                                                          
         BNE   VALFILTX            STATUS EL ALREADY EXISTS                     
         MVI   RSTEL,RSTELQ        NEW ELEMENT                                  
         MVI   RSTCOSTG,C' '                                                    
         MVC   RSTTDATE,TODAYP                                                  
         MVC   RSTBDATE,TODAYP                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
VALFILTX B     OKEXIT                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE STATUS AND @TRACKER                  *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING SCREEND,R2                                                       
VALSTAT  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,PRXSTATH                                                      
         CLI   5(R2),0                                                          
         BE    VALSTAT2                                                         
         CLI   5(R2),1                                                          
         BH    ERREXIT                                                          
         OI    RSTSTAT,X'20'      SET IT TO LOCK                                
         CLI   8(R2),C'L'                                                       
         BE    VALSTAT2                                                         
*                                                                               
         NI    RSTSTAT,X'FF'-X'20' UNLOCK IT                                    
         CLI   8(R2),C'U'                                                       
         BE    VALSTAT2                                                         
         CLI   8(R2),C' '                                                       
         BNE   ERREXIT                                                          
*                                   VALIDATE @TRACKER FIELD                     
VALSTAT2 CLI   RSTLN,RSTLN3Q                                                    
         BL    VALSTATX            ELEMENT TOO SHORT                            
*&&DO                                                                           
         LA    R2,PRXTRAKH                                                      
         NI    RSTSTAT6,X'FF'-RSTSXF@T                                          
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    RSTSTAT6,RSTSXF@T     INCLUDE                                    
*&&                                                                             
VALSTATX B     OKEXIT                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE LEDGER SECURITY                    *         
***********************************************************************         
*                                                                               
VALSEC   NTR1                                                                   
         CLI   MODE,VALREC         ARE WE VALIDATING A FIELD ?                  
         BNE   VALSECX             NO, RETURN                                   
         CLI   5(R2),0             IS THERE ANYTHING THERE ?                    
         BNE   VALSEC2             YES                                          
         CLI   ACTNUM,ACTADD       NO, IS THIS AN ADD ?                         
         BE    VALSECX             YES, NOTHING TO VALIDATE THEN                
*                                                                               
VALSEC2  TM    4(R2),X'20'         YES, WAS THIS FIELD CHANGED ?                
         BO    VALSECX             NO, RETURN                                   
         MVC   CUL+1(2),THISLEDG   YES, CHECK THE SECURITY                      
         GOTO1 SETHEIR                                                          
*                                                                               
VALSECX  B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE ACCOUNT NAME                       *         
***********************************************************************         
*                                                                               
         USING ACNAMED,R6                                                       
         USING SCREEND,R2                                                       
VALNAME  NTR1                                                                   
         MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   NAME020                                                          
         CLI   ACNAMH+5,0                                                       
         BE    NAME040                                                          
         XC    ELEMENT,ELEMENT                                                  
         GOTO1 REMELEM                                                          
NAME020  SR    R1,R1                                                            
         ICM   R1,1,ACNAMH+5                                                    
         BZ    NAME060                                                          
         BCTR  R1,0                                                             
         LA    R6,ELEMENT          SET UP TO BUILD NEW ELEMENT.                 
         MVI   ACNMEL,ACNMELQ                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNMNAME(0),ACNAM                                                
         AH    R1,=H'3'            L'ELEMENT = L'NAME + 3.                      
         STC   R1,ACNMLEN                                                       
         GOTO1 ADDELEM             ADD THE NEW ELEMENT.                         
         B     OKEXIT                                                           
*                                                                               
NAME040  SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACNAM(0),ACNMNAME                                                
*                                                                               
         USING PRRSRNMH,R2                                                      
NAME060  LA    RF,PRRSJNMH         IF IT'S CLI/PRO/JOB, EXIT NOW                
         CR    R2,RF                                                            
         BE    OKEXIT                                                           
         SR    R1,R1                                                            
         IC    R1,PRRSJNMH+5       IF THERE'S BEEN NO NAME INPUT TO             
         STC   R1,PRRSRNMH+5       THIS ACCOUNT, AND IT HAD NO NAME             
         OI    PRRSRNMH+6,X'80'    ELEMENT, USE THE NAME INPUT TO               
         BCTR  R1,0                THE CLIENT/PRODUCT/JOB.                      
         EX    R1,*+8                                                           
         B     NAME020                                                          
         MVC   PRRSRNM(0),PRRSJNM                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              BUILD A BLANK ACCOUNT RECORD IN THIS RECORD AREA.                
***********************************************************************         
*                                                                               
         USING ACKEYD,R6                                                        
RECBUILD NTR1                                                                   
         LR    R6,R2               SAVE CURSOR POSITION                         
         L     R2,AIO              CLEAR AIO                                    
         LA    R3,1000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LR    R2,R6                                                            
         L     R6,AIO                                                           
         SR    R1,R1                                                            
         IC    R1,5(R2)            R1 = L'ACCOUNT CODE INPUT.                   
         BCTR  R1,0                                                             
         MVC   ACKEYACC(42),SPACES INIT KEY.                                    
         MVC   ACLENGTH,=H'50'                                                  
         MVC   ACKEYACC(1),CUL                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),8(R2)                                              
         MVC   ACKEYACC+1(2),THISLEDG                                           
         CLC   THISLEDG,PRODLEDG   EXIT IF SALES ANALYSIS RECORD                
         BE    OKEXIT                                                           
         XC    ELEMENT(ACBLLNQ),ELEMENT                                         
         MVI   ELEMENT,ACBLELQ                                                  
         MVI   ELEMENT+1,ACBLLNQ                                                
         USING ACBALD,R6                                                        
         LA    R6,ELEMENT          RECEIVABLE AND COSTING ACCOUNTS              
         ZAP   ACBLFRWD,=P'0'       GET BALANCE AND PEEL ELEMENTS               
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         GOTO1 ADDELEM                                                          
         USING ACPEELD,R6                                                       
         XC    ELEMENT(20),ELEMENT                                              
         MVC   ELEMENT(2),=X'3314'                                              
         ZAP   ACPEDR,=P'0'                                                     
         ZAP   ACPECR,=P'0'                                                     
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*           VALIDATE NAME AND STATUS AND ADD/UPDATE RECORD            *         
***********************************************************************         
*                                                                               
VALUPDT  NTR1                                                                   
         USING SCREEND,R2                                                       
         BAS   RE,VALNAME                                                       
         BAS   RE,VALFILT                                                       
                                                                                
         CLC   FUNCTION,DMWRT        DID I UPDATE AN EXISTING RECORD            
         BNE   VALUP02               NO, IT MUST BE AN ADD                      
                                                                                
         TM    4(R2),X'20'         WAS THE NAME CHANGED?                        
         BZ    VALUP02             YES, ADD THE PERSON ELEMENT                  
         LA    R0,5                CHECK FILTERS NOW                            
         LA    R2,ACF1H                                                         
         TM    4(R2),X'20'                                                      
         BZ    VALUP02                                                          
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R0,*-16                                                          
         B     VALUP06                                                          
         DROP  R2                                                               
                                                                                
         USING SCREEND,RF                                                       
VALUP02  LA    RF,PRR1CNMH                                                      
         CR    R2,RF               ONLY DO FOR 1C ACCOUNT                       
         BE    VALUP04                                                          
         LA    RF,ACF1H                                                         
         CR    R2,RF                                                            
         BNE   VALUP06                                                          
                                                                                
VALUP04  GOTO1 PERSIN                                                           
         BAS   RE,MANAGER                                                       
         GOTO1 VSAVPTRS,DMCB,(X'80',0),POINTERS                                 
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),POINTERS                               
         B     VALUP08                                                          
                                                                                
VALUP06  BAS   RE,MANAGER                                                       
                                                                                
VALUP08  CLC   FUNCTION,DMWRT        DID I UPDATE AN EXISTING RECORD            
         BNE   VALUPX                NO                                         
         GOTO1 CHKNAME,DMCB,AIO,OLDACCNM                                        
                                                                                
VALUPX   B     OKEXIT                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*             ADD OR UPDATE RECORD, DEPENDING ON FUNCTION             *         
***********************************************************************         
*                                                                               
MANAGER  NTR1                                                                   
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,FUNCTION,ACCOUNT,(R6),(R6),DMWORK                   
         CLI   DMCB+8,0                                                         
         BE    OKEXIT                                                           
DUMP     DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CREATE/UPDATE LIDTPOFC HERE (PRODUCT OFFICE CODES)                  *         
***********************************************************************         
         SPACE 1                                                                
VALPOFC  NTR1                                                                   
         TM    COMPSTAT,CPYSOROE   CHECK IF COMPANY IS OFFICE BASED             
         BNO   OKEXIT                                                           
         CLC   OLDOFF,NEWOFF       NOTHING TO CHANGE?                           
         BE    OKEXIT                                                           
*                                                                               
         MVC   ADDOFBLK,APROOFBK                                                
         L     R2,AIO1             R2=A(CURRENT PRODUCT RECORD)                 
         L     R3,AIO3                                                          
CUR      USING ACTRECD,R2                                                       
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         XR    RE,RE                                                            
         IC    RE,LLEVA                                                         
         AHI   RE,L'ACTKCPY+L'ACTKUNT+L'ACTKLDG-1                               
         STC   RE,BYTE                                                          
         EXMVC RE,ACTKEY,CUR.ACTKEY                                             
         XR    R6,R6               R6=OFFICE CODE COUNTER                       
         CLC   NEWOFF,SPACES                                                    
         BNH   VPOFC02                                                          
         CLC   NEWOFF,CLIOFFC                                                   
         BE    VPOFC02                                                          
         L     RF,APROOFBK         ADD NEW OFFICE CODE TO PROOFBLK              
         MVC   0(L'NEWOFF,RF),NEWOFF                                            
         AHI   R6,1                                                             
         LA    RF,L'NEWOFF(RF)                                                  
         ST    RF,ADDOFBLK                                                      
*                                                                               
VPOFC02  GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,ACTKEY,ACTKEY                        
         CLI   DMCB+8,0                                                         
         BE    VPOFC06                                                          
         DC    H'0'                                                             
*                                                                               
VPOFC04  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCOUNT,ACTKEY,ACTKEY                        
*                                                                               
VPOFC06  XR    RE,RE                                                            
         IC    RE,BYTE                                                          
         EXCLC RE,ACTKEY,CUR.ACTKEY SAME COMPANY+LEDGER+CLIENT?                 
         BNE   VPOFC18              NO                                          
         LA    RF,ACTKEY+1(RE)                                                  
         CLI   0(RF),C' '          CLIENT RECORD?                               
         BH    VPOFC08                                                          
         MVC   KEYSAVE,ACTKEY                                                   
         B     VPOFC04                                                          
*                                                                               
VPOFC08  IC    RE,LLEVAB                                                        
         LA    RE,ACTKACT(RE)                                                   
         CLI   0(RE),C' '          A PRODUCT?                                   
         BH    VPOFC10             NO                                           
         CLC   ACTKACT,CUR.ACTKACT SAME AS CURRENT PRODUCT?                     
         BE    VPOFC04             YES, OFFICE CODE ALREADY DEALT WITH          
         B     VPOFC12             NO, ADD TO PROOFBLK                          
*                                                                               
VPOFC10  MVI   0(RE),X'FF'         MUST BE A JOB                                
         B     VPOFC02             READ HIGH FOR NEXT PRODUCT                   
         DROP  R3,CUR                                                           
*                                                                               
VPOFC12  GOTO1 HELLO,DMCB,(C'G',ACCOUNT),('PPRELQ',AIO3)                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING PPRELD,R5                                                        
         CLC   PPRGAOFF,SPACES    PRODUCT INHERITED CLIENT OFFICE CODE?         
         BE    VPOFC04            YES                                           
         CLC   PPRGAOFF,CLIOFFC                                                 
         BE    VPOFC04                                                          
         LTR   R6,R6                                                            
         BZ    VPOFC16                                                          
         LR    R0,R6                                                            
         L     RE,APROOFBK                                                      
*                                                                               
VPOFC14  CLC   0(L'PPRGAOFF,RE),PPRGAOFF                                        
         BE    VPOFC04            OFFICE CODE ALREADY IN PROOFBLK               
         LA    RE,L'PPRGAOFF(RE)                                                
         BCT   R0,VPOFC14                                                       
         CHI   R6,PROOFBLQ   NEED TO INCREASE PROOFBLK AND PROOFBLQ?            
         BNE   *+6           NO                                                 
         DC    H'0'                                                             
*                                                                               
VPOFC16  L     R1,ADDOFBLK                                                      
         MVC   0(L'PPRGAOFF,R1),PPRGAOFF                                        
         LA    R1,L'PPRGAOFF(R1)                                                
         ST    R1,ADDOFBLK                                                      
         AHI   R6,1                INCREMENT OFFICE CODE COUNTER                
         B     VPOFC04             READ FOR NEXT PRODUCT                        
         DROP  R5                                                               
*                                                                               
         USING LIDELD,R3                                                        
VPOFC18  MVC   AIO,AIO2            READ CLIENT RECORD INTO AIO(2)               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,KEYSAVE,AIO                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DELETE EXISTING LIDTPOFC(S)                  
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),('LIDELQ',AIO),=X'0214'                
*                                                                               
VPOFC20  LTR   R6,R6               ANY OFFICE CODES IN PROOFBLK?                
         BZ    VPOFCX              NO                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'NEWOFF                                                 
         MVI   LIDTYPE,LIDTPOFC                                                 
*                                                                               
         L     R1,APROOFBK                                                      
         LA    RF,L'NEWOFF                                                      
         XR    R2,R2                                                            
         CHI   R6,MAXOFFCQ                                                      
         BNH   VPOFC22                                                          
         MVI   0(R2),MAXOFFCQ                                                   
         B     *+6                                                              
*                                                                               
VPOFC22  LR    R2,R6                                                            
         MR    RE,R2               RE/RF=LENGTH OF LIDDATA TO SAVE              
         SHI   RF,1                                                             
         EXMVC RF,LIDDATA,0(R1)                                                 
*                                                                               
         LA    RF,LIDDATA-LIDELD+1(RF)                                          
         STC   RF,LIDLN                                                         
         GOTO1 ADDELEM                                                          
         CHI   R6,MAXOFFCQ                                                      
         BNH   VPOFCX                                                           
         SHI   R6,MAXOFFCQ                                                      
         LA    RE,L'NEWOFF         MOVE TO NEXT LOT OF OFFICE CODES             
         MHI   RE,MAXOFFCQ                                                      
         L     R1,APROOFBK                                                      
         LA    R1,RE(R1)           R1=A(NEXT LOT OF OFFICE CODES)               
         ST    R1,APROOFBK                                                      
         B     VPOFC20                                                          
         DROP  R3                                                               
*                                                                               
VPOFCX   MVC   FUNCTION,DMWRT         WRITE BACK THE CLIENT                     
         BAS   RE,MANAGER                                                       
         MVC   AIO,AIO1                                                         
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*          DETERMINE WHETHER THIS IS AN ADD OR A CHANGE               *         
***********************************************************************         
*                                                                               
GETFUNC  NTR1                                                                   
         MVC   FUNCTION,DMWRT                                                   
         BAS   RE,VALACCT                                                       
         BE    OKEXIT                                                           
         BAS   RE,RECBUILD                                                      
         MVC   FUNCTION,DMADD                                                   
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*             SAVE THE NAME ELEMENT OF THE ACCOUNT IN AIO IN 0(P1)              
***********************************************************************         
*                                                                               
SVNAME   NTR1                                                                   
         L     R3,0(R1)                                                         
*                                                                               
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)                                                    
         B     EXIT                                                             
         SPACE 3                                                                
***********************************************************************         
*        IF I JUST UPDATED AN ACCOUNT, SEE IF I NEED CREATE A "NEW NAME         
*        POINTER" (ANC) RECORD.                                                 
*        P1 IS ASSUMED TO POINT TO THE RECORD YOU JUST WROTE                    
***********************************************************************         
CHKNAME  NTR1                                                                   
         CLI   EMULATE,C'Y'        AM I ONLY EMULATING IS ACC                   
         BNE   CHKNX               NO                                           
*                                                                               
         L     R6,0(R1)            P1 IS A( ACCOUNT JUST ADDED)                 
         ST    R6,FULL             SAVE IN CASE I NEED DA                       
         L     R2,4(R1)            P2 IS WHERE I SAVED THE OLD NAME             
*                                                                               
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   1(1,R2),1(R6)       WAS THE LENGTH CHANGED                       
         BNE   CHKN20              YES                                          
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R6)                                                    
         BE    CHKNX                                                            
         OI    NAMEFLAG,NFCHANGE                                                
*                                                                               
CHKN20   L     R6,FULL             RESET R6                                     
         BAS   RE,PUTANC                                                        
*                                                                               
CHKNX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        POINTER MAINTENANCE - THE NAME ELEMENT OF THE RECORD IN R6             
*        HAS BEEN CHANGED, CREATE A POINTER SO THE DUMP AND LOAD                
*        WILL FIND THE NEW NAME                                                 
***********************************************************************         
PUTANC   NTR1                                                                   
         USING ACTRECD,R6                                                       
         L     R5,AIO3                                                          
         USING ANCRECD,R5                                                       
         MVC   ANCKEY,SPACES                                                    
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,(R5),(R5),0                   
         BE    PUTANCX             POINTER ALREADY EXISTS                       
*                                                                               
         MVC   0(L'ACTKEY,R5),ACTKEY   READ ACCOUNT KEY                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,(R5),(R5),0                           
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND !!                         
         MVC   FULL,ACTKDA-ACTRECD(R5) SAVE DISK ADDRESS                        
*                                                                               
         MVC   ANCKEY,SPACES       WRITE OUT POINTER TO NEW DA                  
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         XC    ANCKSTA,ANCKSTA                                                  
         OI    ANCKSTA,ACTSDELT    WRITE AS DELETED FOR DUMP/LOAD               
         MVC   ANCKDA,FULL                                                      
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,(R5),(R5),0                            
*                                                                               
PUTANCX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* UPDATE JOB NAME SEARCH PASSIVE RECORDS                                        
**********************************************************************          
NAMESRCH NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+1(3),=CL3'TF '                                              
         MVI   DMCB,C'A'           ADD                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    NSRCH04                                                          
         MVI   DMCB,C'D'           DEL                                          
         CLI   ACTNUM,ACTDEL                                                    
         BE    NSRCH04                                                          
         MVI   DMCB,C'C'           CHANGE                                       
         TM    NAMEFLAG,NFCHANGE                                                
         BZ    NSRCHX                                                           
         NI    NAMEFLAG,X'FF'-NFCHANGE                                          
*                                                                               
NSRCH04  GOTO1 ACSRCHP,DMCB,,AIO,,OLDPRONM,ACOMFACS,AACCFACS                    
*                                                                               
NSRCHX   XIT1                                                                   
         EJECT                                                                  
         USING ACTRECD,R5                                                       
DELETIT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PRRCLACH         ADDRESS CLIENT                               
         L     R5,AIO3                                                          
         MVC   AIO,AIO3                                                         
         MVC   FUNCTION,=CL8'DMREAD'                                            
         GOTO1 READ                                                             
                                                                                
         GOTO1 SEQ                 GET THE NEXT RECORD                          
         MVI   ERROR,CANTDEL       PREPARE FOR ERROR                            
         MVC   KEYSAVE2,KEYSAVE                                                 
         CLC   ACTKEY(9),KEYSAVE                                                
         BE    ERREXIT             IF KEY MATCHES, THERE ARE JOB                
                                                                                
         MVC   KEY,KEYSAVE2                                                     
         GOTO1 READ                                                             
                                                                                
         USING TSJPASD,R5          CHECK TO SEE IF TIME EXISTS                  
         L     R5,AIO3                                                          
         XC    TSJPAS,TSJPAS                                                    
         MVI   TSJPTYP,TSJPTYPQ                                                 
         MVI   TSJPSUB,TSJPSUBQ                                                 
         MVI   TSJPVIEW,TSJPSJAQ                                                
         MVC   TSJPCPY,CUL                                                      
         MVC   TSJPACT,SAVEKEY+L'ACTKCPY+L'ACTKLDG+L'ACTKUNT                    
         MVC   TSJPCOFF,PRRSJOF                                                 
         CLC   TSJPCOFF,SPACES                                                  
         BH    *+10                                                             
         MVC   TSJPCOFF,PRRCLOF                                                 
         OC    TSJPCOFF,SPACES                                                  
         MVC   KEYSAVE,TSJPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TSJPAS,TSJPAS                         
         L     R5,AIO3                                                          
         CLC   TSJPAS(TSJPMED-TSJPASD),KEYSAVE                                  
         BNE   *+12                                                             
         MVI   ERROR,TIMDATA                                                    
         B     ERREXIT                                                          
                                                                                
         LA    RF,KEY              DELETE RELATED TIMESHEET LIST RECS           
         USING TSLRECD,RF                                                       
         XC    TSLKEY,TSLKEY                                                    
         MVI   TSLKTYP,TSLKTYPQ                                                 
         MVI   TSLKSUB,TSLKSUBQ                                                 
         MVC   TSLKCULA,SAVEKEY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'TSLKEY),KEYSAVE                                            
         BNE   DELE02                                                           
         L     RF,AIO                                                           
         OI    ACCOSTAT(RF),X'80'  MARK DELETED                                 
         GOTO1 WRITE                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
DELE02   MVC   KEY,KEYSAVE2        RESTORE KEY                                  
         GOTO1 READ                RE-READ THE PRODUCT                          
         L     RF,AIO                                                           
         OI    ACCOSTAT(RF),X'80'  MARK DELETED                                 
         GOTO1 PERSIN                                                           
         GOTO1 WRITE               AND UPDATE                                   
         GOTO1 VSAVPTRS,DMCB,(X'80',0),POINTERS                                 
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),POINTERS                               
         BAS   RE,NAMESRCH                                                      
                                                                                
         MVC   AIO,AIO2            RESTORE AIO                                  
         MVI   MYMSGNO1,IPDELD     SET UP MESSAGE                               
         ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
OKEXIT   CR    R8,R8                                                            
EXIT     XIT1                                                                   
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
         EJECT                                                                  
HIGHMSG  DC    CL46'HIGHER LEVEL ACCOUNT MISSING ON UNIT/LEDGER   '             
*                                                                               
STATTAB  DC    AL2(RSTFILT1-RSTELD)                                             
         DC    AL2(RSTFILT2-RSTELD)                                             
         DC    AL2(RSTFILT3-RSTELD)                                             
         DC    AL2(RSTFILT4-RSTELD)                                             
         DC    X'FF'                                                            
*                                                                               
*        DATAMGR COMMANDS                                                       
DMADD    DC    CL8'DMADD'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCDIR   DC    CL8'ACCDIR'                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PROOFBLQ EQU   2000                  MAX NUMBER OF ENTRIES IN PROOFBLK          
PROOFBLK DC    2000CL(L'TRNOFFC)'  ' PRODUCT OFFICE CODES                       
MAXOFFCQ EQU   125                                                              
*                                                                               
MYD      DSECT                                                                  
MYRELO   DS    A                                                                
ACSRCHP  DS    A                                                                
MYBUFF   DS    2000C                                                            
SAVESRLG DS    CL2                 SAVE AREA FOR RECEIVABLE LEDGER              
SAVE1CLG DS    CL2                 SAVE AREA FOR COSTING LEDGER                 
SAVESALG DS    CL2                 SAVE AREA FOR SALES ANALYSIS LEDGER          
THISLEDG DS    CL2                 LEDGER BEING PROCESSED                       
FUNCTION DS    CL8                 SAVE DATAMGR COMMAND                         
SAVERE   DS    A                   SAVE AREA FOR RE                             
NAMEFLAG DS    XL1                                                              
NFCHANGE EQU   X'80'                                                            
OLDOFF   DS    XL2                 ORIGINAL OFFICE CODE                         
NEWOFF   DS    XL2                 NEW OFFICE CODE                              
APROOFBK DS    A                   A(PROOFBLK)                                  
ADDOFBLK DS    A                   A(NEXT AVAILABLE ENTRY) OF PROOFBLK          
MYDEND   EQU   *                                                                
         EJECT                                                                  
SCREEND  DSECT                                                                  
ACNAMH   DS    CL8                                                              
ACNAM    DS    CL(L'PRRSJNM)                                                    
         ORG   ACNAMH+(PRRSJF1H-PRRSJNMH)                                       
ACF1H    DS    CL8                                                              
ACF1     DS    CL(L'PRRSJF1)                                                    
         ORG   ACNAMH+(PRRSJOFH-PRRSJNMH)                                       
ACOFH    DS    CL8                                                              
ACOF     DS    CL(L'PRRSJOF)                                                    
         EJECT                                                                  
EQUIVD   DSECT                                                                  
EQULABH  DS    CL8                                                              
EQULAB   DS    CL(L'PRXLAB1)                                                    
         ORG   EQULABH+(PRXEQU1H-PRXLAB1H)                                      
EQUEQUH  DS    CL8                                                              
EQUEQU   DS    CL(L'PRXEQU1)                                                    
         ORG   EQULABH+(PRXACC1H-PRXLAB1H)                                      
EQUACCH  DS    CL8                                                              
EQUACC   DS    CL(L'PRXACC1)                                                    
EQULNQ   EQU   *-EQUIVD                                                         
         EJECT                                                                  
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACPROWORKD                                                                   
         PRINT  OFF                                                             
       ++INCLUDE ACPROWORKD                                                     
         PRINT  ON                                                              
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPRODBD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROF2D                                                       
         DS    0F                                                               
PRODLEDG DS    CL2                 PRODUCTION UNIT/LEDGER                       
RECVLEDG DS    CL2                 RECEIVABLE UNIT/LEDGER                       
COMPSTAT DS    XL1                 COMPANY ELEMENT STATUS BYTE                  
SAVEKEY  DS    CL42                HOLD KEY FOR RETURN                          
OLDPRONM DS    CL(NAMLN1Q+L'NAMEREC)                                            
OLDACCNM DS    CL(NAMLN1Q+L'NAMEREC)                                            
POINTERS DS    XL(8*54+1)          PASSIVE POINTER BLOCK                        
KEYSAVE2 DS    CL48                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061ACPRO02   07/23/14'                                      
         END                                                                    
