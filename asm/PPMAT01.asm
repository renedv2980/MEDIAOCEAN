*          DATA SET PPMAT01    AT LEVEL 085 AS OF 07/22/02                      
*PHASE T40201A                                                                  
*                                                                               
*    SMYE 07/02   FIX LOCK TESTING FOR SUB-CLIENTS                              
*                                                                               
*    SMYE 10/01   CHANGE MESSAGE FOR "NO PROFILE FOUND"                         
*                                                                               
*    SMYE 06/01   ADD LOCK TESTING FOR UPDATIVE SOON CONDITIONS                 
*                                                                               
*    SMYE 06/01   ADD OPTION (M=N) FOR A REPORT "EXCLUSION"                     
*                                                                               
*    SMYE 12/01   FIX BUG IN DATE HANDLING AT VL34..                            
*                                                                               
*     BPLA  10/00  TEMP CHECK FOR YNR PROBLEM                                   
*                 MED=N,CLT=AWR,PRD=LRA,PUB=10506375                            
*                                                                               
*    SMYE  7/00   DISALLOW (IN SETPFTBL) USE OF ANY FUNCTION KEY                
*                   EXCEPT ENTER IF ANY DATA FIELDS HAVE BEEN CHANGED           
*                                                                               
*    SMYE  1/00   MANIPULATE "YEARS" (IN VL10A..AND VL34L..) TO                 
*                   ELIMINATE CALENDAR YEARS MORE THAN 1 YEAR IN THE            
*                   FUTURE OR MORE THAN 8 YEARS IN THE PAST                     
*                   IF YEAR NOT ENTERED BY USER - ALSO NO-OPPPED                
*                   (*NOP*) 9/99 CHANGES FOR 1990 (NO LONGER NEEDED)            
*                                                                               
*    BPLA  10/99  WAS PPMAT01B - RENAMED 10/3/99 OLD LIVE                       
*                 RENAMED TO PPMAT01X                                           
*                                                                               
*    SMYE  9/99   MANIPULATE "YEARS" (IN VL10A..AND VL34L..) TO                 
*                   ELIMINATE 1990 FROM INVOICE AND PERIOD DATES                
*                   IF YEAR NOT ENTERED BY USER                                 
*                                                                               
*    BPLA   6/99  AT LR150 CLOBBER CALL IF GLOBBER ERROR                        
*                 IS DETECTED DON'T DIE - JUST SKIP                             
*                 TO NEW TAG (LR155)  WHICH WILL PF TO CHECK SCREEN             
*                                                                               
*    BPLA  9/98   ACCEPT OUTDOOR FOR ALL AGENCIES                               
*                                                                               
*    BPLA  7/98   ALLOW MORE INVOICES PER PUB/MONTH                             
*                 WAS LIMITED TO 9 - CHANGED TO 12                              
*                                                                               
*    BPLA  2/98   ACCEPT OUTDOOR FOR SJR FOR TESTING                            
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE: T40201 - MAINTENANCE/LIST OF PRINT INVOICES                           
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     PPMATFE (T402FE) -- LIST                                        
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
* ***NOTE:*** IF YOU ARE MAKING CHANGES TO THIS PROGRAM BE AWARE THAT           
* FOR PRODUCT VARIOUS QPRD IS ***.  IF YOU ARE CALLING ANY ALREADY              
* EXISTING PRINT MATCH SUBROUTINES BE SURE TO CHECK THE SUBROUTINE              
* CAREFULLY AND TO CHECK OTHER CALLS TO THE SUBROUTINE TO SEE HOW QPRD          
* IS HANDLED.  THANKS, ABBEY                                                    
*                                                                               
***********************************************************************         
T40201   TITLE 'PPMAT01 - PRINT INVOICE MATCHING OVERLAY'                       
T40201   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40201*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         TM    LSTMEDH+4,X'20'     IF MEDIA HAS BEEN MODIFIED                   
         BNZ   MAIN                                                             
*                                                                               
         XC    WORK,WORK           THEN SEE IF AGENCY USES THIS PROGRAM         
         XC    SVPROF,SVPROF                                                    
         MVC   WORK(4),=C'P0IM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         CLI   SVPROF,0            AGENCY DOESN'T HAVE A PROFILE                
         BNE   *+12                DON'T GIVE ACCESS                            
         LA    R2,CONSERVH                                                      
         B     INVLPROF                                                         
*                                                                               
MAIN     NI    MNIOFLAG,X'FF'-X'40'  CLR MESS BIT BECAUSE LIST SCREEN           
*                                  WAS SAVED WITH ANY OLD MNIOFLAG BITS         
*                                                                               
         BAS   RE,SETPFKYS                                                      
*                                                                               
CKMODES  CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD?                      
         BE    VL                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
SETPFKYS LR    R0,RE                                                            
         GOTO1 =A(SETPFTBL),DMCB,(RC),RR=RELO  SET THE PFKEY TABLE              
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVC   LLIST,=Y(LINNEXTL-LINIDTH)  L(LINE W/O SELECT FIELD)             
         MVI   ADDEDFLG,C'N'                                                    
         NI    BITFLAG,X'FF'-X'08'-X'01'                                        
         MVI   KEYCHNGF,C'N'       ASSUME NO KEY FIELDS CHANGED                 
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   GLOBBER,CGLOBBER                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,80,GLVPRRTN                          
         CLI   DMCB+8,0                                                         
         BNE   *+8                                                              
         BAS   RE,GLOBBED          WE GOT GLOBBED                               
*                                                                               
         LA    R2,LSTPAYRH         WE NEED A PAYER                              
         CLI   5(R2),0                                                          
         BE    NEEDPAYR            PLEASE ENTER FIELDS AS REQUIRED              
*                                                                               
* VALIDATE THE MEDIA                                                            
VK10     LA    R2,LSTMEDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BZ    VK15                                                             
         MVC   QMED,MYQMED         COPY WHAT WE HAD BEFORE                      
         MVC   SCRTYPE,QMED                                                     
*                                                                               
         CLI   SCRTYPE,C'N'        SCREEN FOR NEWSPAPER TYPES?                  
         BE    *+8                                                              
         MVI   SCRTYPE,C'M'        NO, FOR MAGAZINE TYPES                       
         B     VK20                                                             
*                                                                               
VK15     GOTO1 VALIMED                                                          
         MVC   LSTMDXP,MEDNM       SHOW MEDIA NAME                              
         OI    LSTMDXPH+6,X'80'                                                 
         MVI   KEYCHNGF,C'Y'       SHOW HEADERS FROM BEGINNING                  
         MVC   MYQMED,QMED                                                      
         MVC   SCRTYPE,QMED                                                     
         B     VK17                OUTDOOR NOW O.K. FOR ALL                     
*                                                                               
*******  CLC   AGENCY,=C'SJ'       ACCEPT OUTDOOR FOR SJR                       
*******  BE    VK17                                                             
*******  CLI   QMED,C'O'           OUTDOOR MEDIA?                               
*******  BE    INVLOPTN                                                         
*                                                                               
VK17     OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         NI    LSTCLTH+4,X'FF'-X'20'  MEDIA CHANGED, REVALIDATE THESE           
         NI    LSTPRDH+4,X'FF'-X'20'      FIELDS                                
         NI    LSTPUBH+4,X'FF'-X'20'                                            
*                                                                               
         CLI   SCRTYPE,C'N'        SCREEN FOR NEWSPAPER TYPES?                  
         BE    *+8                                                              
         MVI   SCRTYPE,C'M'        NO, FOR MAGAZINE TYPES                       
*                                                                               
* VALIDATE THE CLIENT                                                           
VK20     LA    R2,LSTCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VK25                                                             
         MVC   QCLT,MYQCLT         COPY WHAT WE HAD BEFORE                      
         MVC   SVPROF,MYPROF                                                    
         B     VK30                                                             
*                                                                               
VK25     GOTO1 VALICLT                                                          
         MVC   LSTCLXP,CLTNM       SHOW CLIENT NAME                             
         OI    LSTCLXPH+6,X'80'                                                 
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVI   KEYCHNGF,C'Y'       SHOW HEADERS FROM BEGINNING                  
         MVC   MYQCLT,QCLT                                                      
*                                                                               
         NI    LSTPRDH+4,X'FF'-X'20'   CLIENT CHANGED, REVALIDATE PRD           
*                                                                               
         XC    WORK,WORK           GET AGENCY/MEDIA/CLIENT PROFILE              
         XC    SVPROF,SVPROF                                                    
         MVC   WORK(4),=C'P0IM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         CLI   CLTOFICE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFICE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         CLI   SVPROF,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MYPROF,SVPROF                                                    
*                                                                               
* VALIDATE THE PRODUCT                                                          
VK30     LA    R2,LSTPRDH                                                       
*                                                                               
         CLC   =C'***',8(R2)                                                    
         BNE   *+20                                                             
         CLI   PFKEY,8             MBC WITH PRD VAR IS TREATED LIKE ALL         
         BNE   VK31                                                             
         OI    BITFLAG,X'08'                                                    
         B     VK40                                                             
*                                                                               
         CLI   5(R2),0             ANY PRODUCT?                                 
         BNE   VK31                                                             
*                                                                               
         CLI   PFKEY,8             NONE, GOING TO MBC?                          
         BNE   MISSFLD                                                          
         OI    BITFLAG,X'08'       PRD NOT ENTERED AND GOING TO MBC             
         B     VK40                YES, WILL BE TRANSLATED AS 'ALL'             
*                                                                               
VK31     TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VK32                                                             
         MVC   QPRD,MYQPRD         COPY WHAT WE HAD BEFORE                      
         B     VK40                                                             
*                                                                               
VK32     CLC   =C'***',8(R2)       PRODUCT VARIOUS                              
         BNE   VK32A                                                            
         MVC   QPRD,=C'***'                                                     
         MVC   PRDNM,=C'VARIOUS              '                                  
         B     VK33                                                             
VK32A    GOTO1 VALIPRD                                                          
VK33     MVC   LSTPDXP,PRDNM       SHOW PRODUCT NAME                            
         OI    LSTPDXPH+6,X'80'                                                 
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVI   KEYCHNGF,C'Y'       SHOW HEADERS FROM BEGINNING                  
         MVC   MYQPRD,QPRD                                                      
*                                                                               
* VALIDATE THE PUB                                                              
VK40     LA    R2,LSTPUBH                                                       
*                                                                               
         CLC   AGENCY,=C'YN'                                                    
         BNE   VK40C                                                            
         CLI   LSTMED,C'N'           NEWSPAPERS                                 
         BNE   VK40C                                                            
         CLC   LSTCLT(3),=C'AWR'                                                
         BNE   VK40C                                                            
         CLC   LSTPRD(3),=C'LRA'                                                
         BNE   VK40C                                                            
         CLC   LSTPUB(8),=C'10506375'                                           
         BNE   VK40C                                                            
         B     INVLFLD                                                          
*                                                                               
VK40C    DS    0H                                                               
         NI    GLOBFLG1,X'FF'-X'80'  NOT ALL ZONES AND EDITIONS YET             
*                                                                               
         CLI   5(R2),0             ANY PUB?                                     
         BNE   VK41                                                             
         CLI   PFKEY,8             NONE, GOING TO MBC?                          
         BNE   MISSFLD                                                          
         OI    BITFLAG,X'08'       PUB NOT ENTERED AND GOING TO MBC             
         B     VK50                YES, WILL BE TRANSLATED AS 'ALL'             
*                                                                               
VK41     TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VK45                                                             
         MVC   BPUB,MYBPUB         COPY WHAT WE HAD BEFORE                      
*                                                                               
         CLC   =X'FFFF',BPUB+4     ALL ZONE AND EDITIONS?                       
         BNE   *+8                                                              
         OI    GLOBFLG1,X'80'      SET ALL ZONES AND EDITIONS BIT               
*                                                                               
         MVC   PUBPYREP,MYPAYREP                                                
         B     VK50                                                             
*                                                                               
VK45     CLI   8(R2),C'='          NAME SEARCH FOR PUB?                         
         BNE   VK45A               NO, VALIDATE PUB AS IS                       
*******  BNE   VK45B               NO, VALIDATE PUB AS IS                       
*                                                                               
         SR    R2,RA               YES, CALL  $SEARCH  FOR PUBS                 
         LA    R3,WORK                 WITH THE USER FILTER                     
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         NI    LSTMEDH+4,X'FF'-X'20'                                            
         NI    LSTCLTH+4,X'FF'-X'20'                                            
         NI    LSTPRDH+4,X'FF'-X'20'                                            
         NI    LSTPUBH+4,X'FF'-X'20'                                            
         GOTO1 VSRCHCAL,DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,           X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0                                 
         DROP  R3                                                               
*                                                                               
VK45A    LA    R1,8(R2)            CONVERT ANY PERIODS TO COMMAS                
         ZIC   R0,5(R2)                                                         
VK45A00  CLI   0(R1),C'.'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,VK45A00          SHITTY LABELS                                
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         CLI   DMCB+4,1                                                         
         BL    MISSFLD                                                          
*                                  USE '.' INSTEAD OF ',' OTHERWISE             
         LA    R3,BLOCK            GENCON WILL SEPARATE ZONE & EDTN             
         ZIC   RE,DMCB+4                                                        
         LA    RF,PERVALST         USED AS TEMPORARY STORAGE                    
         XC    PERVALST,PERVALST                                                
*                                                                               
VK45A10  ZIC   R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    VK45A20                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),12(R3)                                                   
         LA    R1,1(R1)                                                         
         LA    RF,0(R1,RF)                                                      
         MVI   0(RF),C'.'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
VK45A20  LA    R3,32(R3)                                                        
         BCT   RE,VK45A10                                                       
         BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
*                                                                               
         CLI   DMCB+4,2            2 LINES?                                     
         BL    VK45B               NO                                           
*                                                                               
         LA    R3,BLOCK+32         LOOK AT THE SECOND LINE                      
         CLC   =C'ALL',12(R3)      ALL ZONES AND EDITIONS?                      
         BNE   VK45B                                                            
*                                                                               
         LA    R3,BLOCK                                                         
         TM    2(R3),X'80'         VALID NUMERIC?                               
         BZ    INVLFLD                                                          
         OI    4(R2),X'08'       YES, JUST VALIDATE THE NUMERIC PART            
         MVC   5(1,R2),0(R3)                                                    
         OI    6(R2),X'80'                                                      
         OI    GLOBFLG1,X'80'      SET ALL ZONES & EDITIONS BIT                 
*                                                                               
VK45B    GOTO1 VALIPUB                                                          
*                                  SVPROF GETS CLOBBERED BY VALIPUB             
         MVC   SVPROF,MYPROF           SOMETIMES                                
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES & EDITIONS?                        
         BZ    *+10                                                             
         MVC   BPUB+4(2),=X'FFFF'  YES, FILL IT WITH X'FF'S                     
*                                                                               
         MVC   LSTP1XP,PUBNM       SHOW PUB NAME                                
         OI    LSTP1XPH+6,X'80'                                                 
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
*                                                                               
         MVC   LSTPRPX(20),REPNAME    GET PART OF REP'S NAME                    
         MVC   LSTPRPX+21(25),REPADR1     AND PART OF THE REP'S ADDRESS         
         OI    LSTPRPXH+6,X'80'                                                 
         XC    LSTREPT,LSTREPT                                                  
         OI    LSTREPTH+6,X'80'                                                 
*                                                                               
         MVI   KEYCHNGF,C'Y'       YES, SHOW HEADERS FROM BEGINNING             
         MVC   MYBPUB,BPUB                                                      
         MVC   MYPAYREP,PUBPYREP                                                
*                                                                               
         MVC   LSTPUB,PERVALST                                                  
*                                                                               
         XC    LSTPREP,LSTPREP                                                  
         MVC   LSTPREP,PUBPYREP                                                 
         LA    R2,LSTPREPH                                                      
         OI    4(R2),X'08'                                                      
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    PUBPYREP,PUBPYREP   IF NO PAYING REP                             
         BZ    VK45C               THEN NOTHING TO VALIDATE                     
*                                                                               
         GOTO1 VALIREP                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PREPRECD,R6                                                      
         MVC   LSTREPT,=C'PAYING REP'                                           
         MVC   LSTPRPX(20),PREPNAME    GET PART OF REP'S NAME                   
         MVC   LSTPRPX+21(25),PREPLIN1    AND PART OF THE REP'S ADDRESS         
         B     VK50                DID A VALIREP AFTER VALIPUB                  
         DROP  R6                                                               
*                                                                               
VK45C    LA    R2,LSTCLTH          RESETS DISK ADDRESS TO 1 FROM PRTDIR         
         GOTO1 VALICLT                INSTEAD OF PUBDIR AFTER A VALIPUB         
*                                                                               
* VALIDATE THE YEAR                                                             
VK50     LA    R2,LSTYEARH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BZ    *+14                                                             
         MVC   QYEAR,MYQYEAR       YES                                          
         B     VK60                                                             
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VK55                                                             
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)                                        
         MVC   LSTYEAR,DUB+1       COPY THE LAST DIGIT OF YEAR                  
         MVC   QYEAR,DUB+1                                                      
         OI    6(R2),X'80'                                                      
         B     VK58                                                             
*                                                                               
VK55     TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    INVLFLD                                                          
         MVC   QYEAR,LSTYEAR       YES                                          
*                                                                               
VK58     MVC   MYQYEAR,QYEAR                                                    
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVI   KEYCHNGF,C'Y'       YES, SHOW HEADERS FROM BEGINNING             
*                                                                               
* VALIDATE THE PERIOD                                                           
VK60     LA    R2,LSTPERH                                                       
         CLI   5(R2),0             ANY PERIOD?                                  
         BNE   VK65                                                             
*                                                                               
         TM    BITFLAG,X'08'       NO, MISSING IF NO PRD/PUB ENTERED            
         BNZ   MISSFLD                                                          
*                                                                               
         CLI   PFKEY,8             GOING TO MBC?                                
         BNE   VK61                                                             
*                                                                               
         LH    R1,CURDISP                                                       
         AR    R1,RA                                                            
         LA    R0,LSTSEL2H                                                      
         CR    R1,R0                                                            
         BL    MISSFLD                                                          
         LA    R0,LSTPFLNH                                                      
         CR    R1,R0                                                            
         BNL   MISSFLD                                                          
*                                                                               
         LH    R1,2(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL2H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(L'LSTHDRSQ,R1),0(R1)   NO INVOICE HEADER?                      
         BZ    MISSFLD             DON'T GO ANYWHERE                            
*                                                                               
VK61     NI    BITFLAG,X'FF'-X'80'   NOT FILTERING ON PERIOD                    
         OI    4(R2),X'20'                                                      
         B     VK70                                                             
*                                                                               
VK65     TM    4(R2),X'20'         VALIDATE PREVIOUSLY?                         
         BNZ   VK70                YES                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),PERDFILT                                   
         OC    DMCB,DMCB                                                        
         BZ    INVLFLD                                                          
         OI    BITFLAG,X'80'       SET FILTER FLAG BIT                          
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVI   KEYCHNGF,C'Y'       YES, SHOW HEADERS FROM BEGINNING             
*                                                                               
* VALIDATE THE OPTIONS                                                          
VK70     LA    R2,LSTOPTH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VK90                                                             
*                                                                               
         NI    BITFLAG,X'89'       NOT FILTERING ON ANYTHING ELSE               
         XC    ESTFILT,ESTFILT                                                  
         XC    SREPFILT,SREPFILT                                                
         XC    NEXTSCRN,NEXTSCRN                                                
         MVI   MYRPTOPT,C' '                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         OI    LSTOPTH+4,X'20'                                                  
         MVI   KEYCHNGF,C'Y'                                                    
         B     VK90                                                             
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK)                                      
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
VK70INVL LA    R2,LSTOPTH                                                       
         B     INVLOPTN                                                         
*                                                                               
         LA    R1,BLOCK                                                         
         ZIC   R3,DMCB+4           NUMBER OF LINES                              
*                                                                               
VK70LOOP CLI   0(R1),1             JUST AN E=, S=, M= OR N=?                    
         BNE   VK70INVL            NO, INVALID OPTIONS INPUT                    
*                                                                               
VK70E    CLI   12(R1),C'E'         FILTER FOR ESTIMATE?                         
         BNE   VK70N                                                            
*                                                                               
         TM    BITFLAG,X'40'       ALREADY GOT AN ESTIMATE FILTER?              
         BNZ   VK70INVL            YES                                          
*                                                                               
         CLI   1(R1),1             ESTIMATE CAN'T BE MORE THAN 3 DIGITS         
         BL    VK70INVL                                                         
         CLI   1(R1),3             ESTIMATE CAN'T BE MORE THAN 3 DIGITS         
         BH    VK70INVL                                                         
*                                                                               
         TM    3(R1),X'40'         VALID ALPHA?                                 
         BZ    VK70E10                                                          
         ZIC   R2,1(R1)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R1),=C'NONE'   E=NONE?                                      
         BNE   VK70INVL                                                         
         B     VK70ESET            YES, FILTER ON EMPTY SREP                    
*                                                                               
VK70E10  TM    3(R1),X'80'         VALID NUMERIC?                               
         BZ    VK70INVL            NO                                           
*                                                                               
         ZIC   R2,1(R1)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R1)                                                     
         UNPK  ESTFILT,DUB                                                      
*                                                                               
VK70ESET OI    BITFLAG,X'40'       SET FILTER FLAG BIT                          
         B     VK70NEXT                                                         
*                                                                               
VK70N    CLI   12(R1),C'N'         NEXT SCREEN OPTION?                          
         BNE   VK70S                                                            
*                                                                               
         TM    BITFLAG,X'10'       ALREADY GOT OVERRIDE NEXT SCREEN?            
         BNZ   VK70INVL            YES                                          
*                                                                               
         CLI   1(R1),1             SCREEN PROFILE MUST BE ONE BYTE              
         BNE   VK70INVL                                                         
*                                                                               
         CLI   22(R1),C'C'         ONLY VALID SCREEN PROFILES                   
         BE    VK70N10                                                          
         CLI   22(R1),C'U'                                                      
         BE    VK70N10                                                          
         CLI   22(R1),C'S'                                                      
         BE    VK70N10                                                          
         CLI   22(R1),C'L'                                                      
         BNE   VK70INVL                                                         
*                                                                               
VK70N10  MVC   NEXTSCRN,22(R1)                                                  
         OI    BITFLAG,X'10'       SET FLAG BIT                                 
         B     VK70NEXT                                                         
*                                                                               
VK70S    CLI   12(R1),C'S'         FILTER FOR SREP?                             
         BNE   VK70V                                                            
*                                                                               
         TM    BITFLAG,X'20'       ALREADY GOT SREP FILTER?                     
         BNZ   VK70INVL            YES                                          
*                                                                               
         CLI   1(R1),1             SREP CAN'T BE MORE THAN 4 DIGITS             
         BL    VK70INVL                                                         
         CLI   1(R1),4                                                          
         BH    VK70INVL                                                         
*                                                                               
         TM    3(R1),X'40'         VALID ALPHA?                                 
         BZ    VK70S10                                                          
         ZIC   R2,1(R1)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R1),=C'NONE'   S=NONE?                                      
         BNE   VK70INVL                                                         
         B     VK70SSET            YES, FILTER ON EMPTY SREP                    
*                                                                               
VK70S10  TM    3(R1),X'80'         VALID NUMERIC?                               
         BZ    VK70INVL            NO                                           
*                                                                               
         ZIC   R2,1(R1)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R1)                                                     
         UNPK  SREPFILT,DUB                                                     
*                                                                               
VK70SSET OI    BITFLAG,X'20'       SET FILTER FLAG BIT                          
*                                                                               
VK70V    CLI   12(R1),C'M'         REPORT EXCLUSION OPTION ?                    
         BNE   VK70INVL                                                         
*                                                                               
         CLI   1(R1),1             SCREEN PROFILE MUST BE ONE BYTE              
         BNE   VK70INVL                                                         
*                                                                               
         CLI   22(R1),C'N'         ONLY VALID SCREEN PROFILE(S)                 
         BNE   VK70INVL                                                         
*                                                                               
VK70V10  MVC   MYRPTOPT,22(R1)                                                  
*****    B     VK70NEXT                                                         
*                                                                               
VK70NEXT LA    R1,32(R1)           POINT TO NEXT LINE                           
         BCT   R3,VK70LOOP                                                      
*                                                                               
         OI    LSTOPTH+4,X'20'     HAS BEEN VALIDATED                           
         MVI   KEYCHNGF,C'Y'       YES, SHOW HEADERS FROM BEGINNING             
*                                                                               
VK90     TM    BITFLAG,X'08'       PRD/PUB NOT ENTERED & GOING TO MBC?          
         BNZ   VK92                                                             
*                                                                               
*******  BAS   RE,ANYOZNED         TEST FOR PUB,ALL AND PUB,???                 
*                                                                               
         GOTO1 MNIOINIT            INITIALIZE MINIO                             
*                                                                               
         CLI   PFKEY,8             MBC?                                         
         BNE   VK93                                                             
VK92     GOTO1 =A(SWTCHMBC),DMCB,(RC),RR=RELO   YES                             
         B     VKX                                                              
*                                                                               
VK93     CLI   KEYCHNGF,C'Y'       DID THE KEYS OR FILTERS CHANGE?              
         BNE   *+12                                                             
         MVI   LSTHDRSQ,0          YES, START LIST FROM THE BEGINNING           
         B     VKX                 CAN'T USE THE OTHER PFKEYS                   
*                                                                               
VK95     CLI   PFKEY,4             MATCH AN INVOICE HEADER?                     
         BNE   VK100                                                            
         BAS   RE,MIHEADER         YES                                          
         MVI   PFKEY,2             GO TO CHECK SCREEN AFTER MATCH               
         BAS   RE,SETPFKYS                                                      
         B     VK95                                                             
*                                                                               
VK100    CLI   PFKEY,13            DELETE AN INVOICE HEADER?                    
         BNE   VKX                                                              
         GOTO1 =A(DIHEADER),DMCB,(RC),RR=RELO   YES                             
         MVI   LISTSW,C'T'                                                      
         BAS   RE,SETPFKYS                                                      
         B     VK95                LOOP BACK TO SEE IF MORE SELECTS             
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LIST RECORD                                                      
***********************************************************************         
VL       DS    0H                                                               
         MVI   IOOPT,C'Y'          TELL GENCON I WILL DO IO (MINIO)             
         OI    MNIOFLAG,X'80'      MINIO BUFFER NEEDS TO BE CLOSED              
*                                                                               
         L     R2,ATHISLST         R2 = A(LIST LINE BEING VALIDATED)            
         SH    R2,=Y(LINIDTH-LINSELH)                                           
         USING LINSELH,R2                                                       
*                                                                               
         NI    MNIOFLAG,X'FF'-X'01'                                             
         LA    R0,LSTSEL1H                                                      
         CR    R2,R0               ADDED OR CHANGED?                            
         BNH   *+8                 ADDED                                        
         OI    MNIOFLAG,X'01'      CHANGED                                      
*                                                                               
         L     R6,MINELEM          POINT TO THE ELEMENT                         
         USING PIMHDREL,R6                                                      
         XC    0(L'MELEM,R6),0(R6)  CLEAR THE ELEMENT                           
         MVI   PIMHDREL,PIMHDREQ   SET UP ELEMENT CODE                          
         MVI   PIMHDRLN,PIMHDRLQ       AND LENGTH                               
*                                                                               
VL10     CLI   LINIDTH+5,0         NEED INVOICE DATE                            
         BNE   *+12                                                             
         LA    R2,LINIDTH                                                       
         B     MISSFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,LINIDT),ADATEFLD                                  
         OC    DMCB,DMCB                                                        
         BZ    VL10A               NOT  M/D/Y   OR   D/M/Y                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,ADATEFLD),(3,PIMINVDT)                            
         B     VL20                                                             
*                                                                               
VL10A    GOTO1 DATVAL,DMCB,(1,LINIDT),ADATEFLD                                  
         OC    DMCB,DMCB                                                        
         BZ    VL10B                                                            
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   ADATEFLD(2),WORK    COPY TODAY'S YEAR TEN DIGIT                  
         MVC   ADATEFLD+1(L'LSTYEAR),LSTYEAR     USE OUR UNIT DIGIT             
*                                                                               
         GOTO1 DATCON,DMCB,(0,ADATEFLD),(3,FULL)                                
*                                                                               
         CLC   ADATEFLD(2),WORK    IF YEAR > TODAY'S YEAR                       
         BNH   VL10A70                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,WORK+10)                                    
*                                                                               
         ZIC   RE,FULL             SUBTRACT 10 FROM YEAR                        
         LR    R1,RE                ONLY IF "IMPLIED" YEAR MINUS                
         ZIC   RF,WORK+10            "TODAY'S" YEAR IS MORE THAN 1              
         SR    RE,RF                                                            
         CHI   RE,1                                                             
         BNH   VL10A70                                                          
         SH    R1,=H'10'           "IMPLIED" YEAR IN R1                         
         STC   R1,FULL                                                          
VL10A70  MVC   PIMINVDT,FULL                                                    
         B     VL20                                                             
*                                                                               
VL10B    GOTO1 DATVAL,DMCB,(2,LINIDT),ADATEFLD                                  
         OC    DMCB,DMCB           INVALID IF NOT IN FORMAT OF                  
         BNZ   *+12                    M/D/Y (US)    D/M/Y (UK)                 
         LA    R2,LINIDTH              M/D   (US)    D/M   (UK)                 
         B     INVLDATE                M/Y                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,ADATEFLD),(3,FULL)                                
         MVC   PIMINVDT,FULL                                                    
         MVI   PIMINVDT+2,1        FIRST DAY OF MONTH/YEAR                      
*                                                                               
VL20     CLI   LININVH+5,0         NEED AN INVOICE NUMBER                       
         BNE   *+12                                                             
         LA    R2,LININVH                                                       
         B     MISSFLD                                                          
         MVC   PIMINVNO,LININV     COPY INVOICE NUMBER                          
*                                                                               
VL30     CLI   LINPERH+5,0         VALIDATE PERIOD                              
         BNE   *+12                                                             
         LA    R2,LINPERH                                                       
         B     MISSFLD                                                          
*                                                                               
         TM    LINPERH+4,X'08'     VALID NUMERIC?                               
         BZ    VL33                NO                                           
*                                                                               
         CLI   LINPERH+5,6                                                      
         BH    VL30INVL                                                         
         ZIC   R1,LINPERH+5        YES, CHECK IF JUST A MONTH                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LINPER(0)                                                    
         CVB   R1,DUB                                                           
*                                                                               
         CH    R1,=H'12'           CHECK NUMBER WITHIN 1-12                     
         BNH   *+12                                                             
         NI    LINPERH+4,X'FF'-X'08'                                            
         B     VL33                                                             
         CH    R1,=H'1'                                                         
         BL    VL30INVL                                                         
*                                                                               
VL33     LA    R3,LINPER                                                        
         ICM   R3,8,LINPERH+5                                                   
         GOTO1 PERVAL,DMCB,(R3),(X'20',PERVALST)                                
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT?                         
         BE    VL36                YES                                          
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
VL34     DS    0H                  SEE IF ENTERED ONLY THE MONTH                
         CLC   LSTYEAR,PVALCPER+7  IF YEARS DON'T MATCH                         
         BE    VL35                                                             
         TM    LINPERH+4,X'04'     VALID ALPHABETIC?                            
         BZ    VL35                NO                                           
*                                                                               
**********************************************************************          
*                                                                    *          
*   YEAR HAS NOT BEEN ENTERED IN PERIOD - LSTYEAR (SINGLE DIGIT)     *          
*           IS USED TO LOGICALLY CREATE YEAR AS FOLLOWS:             *          
*     LSTYEAR = LAST DIGIT OF CURRENT YEAR - O/P CURRENT YEAR        *          
*     LSTYEAR = LAST DIGIT OF "NEXT"  YEAR - O/P NEXT    YEAR        *          
*       ALL 8 OTHER DIGITS - OUTPUT PREVIOUS YEARS (BACK 10)         *          
*                                                                    *          
*   IN OTHER WORDS, IF YEAR NOT ENTERED, WE ACCESS ONLY INVOICES     *          
*  FOR CURRENT YEAR OR 1 YEAR IN THE FUTURE OR 8 YEARS IN THE PAST   *          
**********************************************************************          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   ADATEFLD(2),WORK    COPY TODAY'S YEAR TEN DIGIT                  
         MVC   ADATEFLD+1(L'LSTYEAR),LSTYEAR     USE OUR UNIT DIGIT             
*                                                                               
         GOTO1 DATCON,DMCB,(0,ADATEFLD),(3,FULL)                                
*                                                                               
         LA    R1,PERVALST         REPOINT R1 TO PERVAL OUTPUT                  
*                                                                               
         ZIC   RE,FULL             SUBTRACT 10 FROM YEAR                        
         LR    R3,RE                ONLY IF "IMPLIED" YEAR MINUS                
         ZIC   RF,PVALBSTA           PERVAL YEAR IS MORE THAN 1                 
         SR    RE,RF                                                            
*NOP*    BP    *+6                                                              
*NOP*    DC    H'0'                SOMETHING WRONG                              
         BNP   VL34L                                                            
         CHI   RE,1                                                             
         BE    VL34L                                                            
*****    VL34G BEL0W                                                            
*                                                                               
VL34G    DS    0H            CURRENT YEAR GREATER THAN LINPER YEAR              
         MVC   LINPER(3),PVALCPER          MONTH                                
*NOP*    ZIC   R3,PVALBSTA         YEAR                                         
         SH    R3,=H'10'           SUBTRACT TEN                                 
         EDIT  (R3),(2,FULL)                                                    
         MVI   LINPER+3,C'/'               /YEAR                                
         MVC   LINPER+4(1),FULL            /YEAR                                
         MVC   LINPER+5(1),LSTYEAR         /YEAR                                
         MVI   LINPERH+5,X'06'    LENGTH                                        
         B     VL34PER                                                          
VL34L    DS    0H                  ENTERED YEAR LESS THAN CURRENT               
         MVC   LINPER(3),PVALCPER          MONTH                                
         MVC   LINPER+3(2),PVALCPER+5      /YEAR                                
         MVC   LINPER+5(1),LSTYEAR         /YEAR                                
         MVI   LINPERH+5,X'06'    LENGTH                                        
*                                                                               
VL34PER  LA    R3,LINPER                                                        
         ICM   R3,8,LINPERH+5                                                   
         GOTO1 PERVAL,DMCB,(R3),(X'20',PERVALST)                                
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT?                         
         BE    VL36                YES                                          
*                                                                               
VL35     CLI   DMCB+4,0            BOTH FIELDS VALID                            
         BE    *+12                                                             
VL30INVL LA    R2,LINPERH          INVALID PERIOD                               
         B     INVPEROD                                                         
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   PIMSTDT,PVALBSTA    SAVE START DATE                              
         MVC   PIMENDDT,PVALBEND        AND END DATE OF PERIOD                  
*                                                                               
         CLC   PIMSTDT(2),PIMENDDT  MUST BE IN SAME YEAR/MONTH                  
         BNE   VL30INVL                                                         
*                                                                               
         CLC   PIMSTDT,PIMENDDT                                                 
         BH    VL30INVL            START DATE GREATER THAN END DATE             
         CLC   LSTYEAR,PVALCPER+7                                               
         BNE   VL30INVL                                                         
         CLC   LSTYEAR,PVALCPER+16                                              
         BNE   VL30INVL                                                         
         B     VL38                                                             
         DROP  R1                                                               
*                                                                               
VL36     CLI   DMCB,X'01'          FIELD 1 INVALID?                             
         BE    VL30INVL                                                         
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   PIMSTDT,PVALBSTA    SAVE START DATE                              
         MVC   PIMENDDT,PVALBSTA       END DATE IS SAME IF ONLY ONE             
         CLC   LSTYEAR,PVALCPER+7                                               
         BNE   VL30INVL                                                         
         MVC   0(L'PVALCPER,R3),PVALCPER                                        
         DROP  R1                                                               
*                                                                               
VL38     MVC   INVSTDT,PIMSTDT                                                  
         MVC   INVENDDT,PIMENDDT                                                
         XC    BEST,BEST                                                        
         CLI   SVPROF+3,C'Y'       MULTI-ESTIMATE?                              
*        BNE   VL40                                                             
*        GOTO1 =A(ANYBUYS),DMCB,(RC),(R2),RR=RELO                               
         BE    VL50                                                             
*                                                                               
VL40     CLI   LINESTH+5,0         YES, ANY ESTIMATE INPUTTED?                  
         BNE   VL40A                                                            
*                                                                               
         CLI   SVPROF+4,C'Y'       NO, MUST ENTER ESTIMATE IN LIST?             
         BNE   *+12                                                             
         LA    R2,LINESTH          YES                                          
         B     MISSFLD                                                          
*                                                                               
         XC    BEST,BEST           NO, MUST ENTER IT IN THE DETAILS             
         B     VL50                                                             
*                                                                               
VL40A    TM    LINESTH+4,X'08'     VALID NUMERIC?                               
         BNZ   *+12                                                             
         LA    R2,LINESTH          NO                                           
         B     INVLFLD                                                          
*                                                                               
         LR    R0,R2                                                            
         LA    R2,LINESTH                                                       
*                                                                               
         CLC   =C'***',LSTPRD      PRODUCT VARIOUS                              
         BNE   VL41                                                             
         MVC   BEST(2),LINEST                                                   
         MVC   QEST(2),BEST                                                     
         B     VL45                                                             
*                                                                               
VL41     GOTO1 VALIEST                                                          
*                                                                               
VL43     GOTO1 DATCON,DMCB,(3,PIMSTDT),(0,DUB)                                  
         CLC   ESTSTDT,DUB                                                      
         BH    INVLEPER            PERIOD NOT IN ESTIMATE PERIOD                
         CLC   ESTNDDT,DUB                                                      
         BL    INVLEPER                                                         
         OC    PIMENDDT,PIMENDDT                                                
         BZ    VL45                                                             
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(0,DUB)                                 
         CLC   ESTSTDT,DUB                                                      
         BH    INVLEPER            PERIOD NOT IN ESTIMATE PERIOD                
         CLC   ESTNDDT,DUB                                                      
         BL    INVLEPER                                                         
*                                                                               
VL45     LR    R2,R0                                                            
         ZIC   R1,LINESTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  FULL(2),LINEST(0)                                                
         UNPK  LINEST(3),FULL(2)                                                
         MVC   PIMEST,LINEST                                                    
         OI    LINESTH+6,X'80'                                                  
*                                                                               
VL50     CLI   LINGNH+5,0          NEED (G)ROSS OR (N)ET                        
         BNE   *+12                                                             
         LA    R2,LINGNH                                                        
         B     MISSFLD                                                          
         CLI   LINGN,C'N'          NET                                          
         BE    VL60                                                             
         CLI   LINGN,C'G'          INVALID IF NOT (G)ROSS OR (N)ET              
         BE    *+12                                                             
         LA    R2,LINGNH                                                        
         B     INVLFLD                                                          
         OI    PIMSTAT,X'40'       SET GROSS AMOUNT BIT ON                      
*                                                                               
VL60     CLI   LINAMTH+5,0         ANY INVOICE AMOUNT?                          
         BNE   VL65                                                             
         ZAP   PIMAMT,=P'0'                                                     
         OI    PIMSTAT,X'10'       NO, INVOICE AMOUNT IS SUM OF DETAILS         
         B     VL70                                                             
VL65     ZIC   R0,LINAMTH+5        YES, EXTRACT IT FROM THE SCREEN              
         GOTO1 CASHVAL,DMCB,(X'82',LINAMT),(R0)                                 
         CLI   DMCB,X'FF'          INVALID INPUT?                               
         BNE   *+12                                                             
         LA    R2,LINAMTH          YES                                          
         B     INVLFLD                                                          
         ZAP   PIMAMT,DMCB+4(8)                                                 
*                                                                               
VL70     CLI   LINCDH+5,0          NEED (Y)ES OR (N)O                           
         BNE   *+12                                                             
         LA    R2,LINCDH                                                        
         B     MISSFLD                                                          
         CLI   LINCD,C'Y'          NET                                          
         BE    VL75                                                             
         CLI   LINCD,C'N'          INVALID IF NOT (Y)ES OR (N)O                 
         BE    *+12                                                             
         LA    R2,LINCDH                                                        
         B     INVLFLD                                                          
         OI    PIMSTAT,X'20'       SET NO CASH DISCOUNT BIT ON                  
*                                                                               
VL75     DS    0H                                                               
         XC    SPCLREP,SPCLREP                                                  
         CLI   LINSREPH+5,0                                                     
         BE    VL80                                                             
*                                                                               
         LR    R0,R2                                                            
         LA    R2,LINSREPH                                                      
         GOTO1 VALIREP                                                          
         LR    R2,R0                                                            
         MVC   PIMSREP,LINSREP                                                  
         MVC   SPCLREP,LINSREP                                                  
*                                                                               
******VL80     GOTO1 =A(ANYBUYS),DMCB,(RC),(R2),RR=RELO                         
*                                                                               
VL80     TM    MNIOFLAG,X'01'      CHANGED HEADER ELEMENT?                      
         BNZ   VL90                YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,PIMCREDT)                                   
*                                                                               
VL85     BAS   RE,NXTHDRSQ                                                      
         BE    VL85A                                                            
         GOTO1 =A(RSEQHDRS),DMCB,(RC),RR=RELO                                   
         B     VL85                                                             
*                                                                               
VL85A    MVC   PIMHDRSQ,BYTE                                                    
         BAS   RE,MINIOADD         ADD THE HEADER                               
         MVI   ADDEDFLG,C'Y'                                                    
         MVC   IHDREST,PIMEST      THESE IHDR VARIABLES NEEDED TO PASS          
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(11,IHDRPER)   TO NEXT SCREEN            
         MVI   IHDRPER+8,C'-'                        BECAUSE WE SWITCH          
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(11,IHDRPER+9)  AFTER LR SO             
         MVC   IHDRNUMB,PIMINVNO       THE LINE MIGHT NOT BE THE ONE WE         
         MVC   IHDRSREP,PIMSREP        ADDED                                    
         B     VLX                                                              
*                                                                               
VL90     MVC   MELEM2,MELEM        SAVE CHANGED INVOICE HEADER                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ    ELEMENT CODE FOR INVOICE HDR ELEMENT         
         ZIC   R1,SELLISTN         RETRIEVE THE HEADER SEQUENCE NUMBER          
         BCTR  R1,0                    FOR THIS CHANGED LIST RECORD             
         LA    R1,SEQLIST(R1)                                                   
         MVC   MINEKEY+1(L'PIMHDRSQ),0(R1)                                      
         MVC   LSTHDRSQ,0(R1)      SAVE HEADER SEQUENCE NUMBER                  
         BAS   RE,MINIORD          GET THE HEADER                               
*                                                                               
         TM    LINPERH+4,X'20'     PERIOD WAS CHANGED?                          
         BNZ   VL90A                                                            
         CLC   PIMSTDT(L'PIMSTDT+L'PIMENDDT),MELEM2+PIMSTDT-PIMHDREL            
         BE    VL90A               NO REAL CHANGE TO PERIOD                     
         MVC   MELEM,MELEM2                                                     
         BAS   RE,CHNGPERD         ALL DETAILS MUST BE WITHIN PERIOD            
         BNZ   VL90B                                                            
*                                                                               
VL90A    TM    LINESTH+4,X'20'     ESTIMATE WAS CHANGED?                        
         BNZ   VL90C                                                            
         CLC   PIMEST,MELEM2+PIMEST-PIMHDREL                                    
         BE    VL90C               NO REAL CHANGE TO ESTIMATE                   
         MVC   MELEM,MELEM2                                                     
         BAS   RE,CHNGESTM         ALL THE DETAILS MUST BE UNMATCHED            
VL90B    MVC   MELEM2,MELEM        SAVE CHANGED INVOICE HEADER                  
*                                                                               
VL90C    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ    ELEMENT CODE FOR INVOICE HDR ELEMENT         
         ZIC   R1,SELLISTN         RETRIEVE THE HEADER SEQUENCE NUMBER          
         BCTR  R1,0                    FOR THIS CHANGED LIST RECORD             
         LA    R1,SEQLIST(R1)                                                   
         MVC   MINEKEY+1(L'PIMHDRSQ),0(R1)                                      
         MVC   LSTHDRSQ,0(R1)      SAVE HEADER SEQUENCE NUMBER                  
         BAS   RE,MINIORD          GET THE HEADER                               
*                                                                               
         MVC   MELEM2+PIMCREDT-PIMHDREL(L'PIMCREDT),PIMCREDT                    
         CLI   LINAMTH+5,0         NO AMOUNT?                                   
         BE    VL91                                                             
         TM    LINAMTH+4,X'80'     AMOUNT INPUT THIS TIME?                      
         BNZ   VL92                                                             
         TM    PIMSTAT,X'10'       AMOUNT IS TOTAL OF DETAILS?                  
         BZ    VL92                                                             
         B     VL91A                                                            
*                                                                               
VL91     BAS   RE,CALCTOTS         CALCULATE TOTAL FROM DETAILS                 
         ZAP   MELEM2+PIMAMT-PIMHDREL(L'PIMAMT),TOTDTLAM                        
         OI    MELEM2+PIMSTAT-PIMHDREL,X'10'   AMNT IS STILL OF DETAILS         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         BAS   RE,MINIORD                                                       
         B     VL92                                                             
*                                                                               
VL91A    OI    MELEM2+PIMSTAT-PIMHDREL,X'10'   AMNT IS STILL OF DETAILS         
         ZAP   MELEM2+PIMAMT-PIMHDREL(L'PIMAMT),PIMAMT                          
*                                                                               
VL92     TM    LINIDTH+4,X'20'     DATE CHANGED?                                
         BNZ   VL100                                                            
         MVC   OLDHDRSQ,PIMHDRSQ   SAVE WHAT SEQUENCE IS                        
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MELEM,MELEM2                                                     
         GOTO1 DATCON,DMCB,(5,0),(3,PIMACTDT)                                   
*                                                                               
VL94     BAS   RE,NXTHDRSQ                                                      
         BE    VL95                                                             
         GOTO1 =A(RSEQHDRS),DMCB,(RC),RR=RELO                                   
         B     VL94                                                             
*                                                                               
VL95     MVC   PIMHDRSQ,BYTE                                                    
         MVC   NEWHDRSQ,BYTE       WHAT SEQUENCE WILL BE                        
         BAS   RE,MINIOADD         ADD THE HEADER                               
*                                                                               
         CLC   OLDHDRSQ,NEWHDRSQ                                                
         BE    VL95C                                                            
*                                                                               
VL95A    XC    MINEKEY,MINEKEY     MAKE SURE SEQUENCE NUMBERS FOR THE           
         MVI   MINEKEY,PIMDTLEQ        DETAILS MATCH THAT OF THE HEADER         
         MVC   MINEKEY+1(L'OLDHDRSQ),OLDHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   VL95B                                                            
*                                                                               
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   VL95B                                                            
         CLC   PIMDTLS1,OLDHDRSQ                                                
         BNE   VL95B                                                            
*                                                                               
         MVC   PIMDTLS1,NEWHDRSQ                                                
         BAS   RE,MINIOWRT                                                      
         B     VL95A                                                            
*                                                                               
VL95B    XC    MINEKEY,MINEKEY     MAKE SURE SEQUENCE NUMBERS FOR THE           
         MVI   MINEKEY,PIMCOMEQ        COMMENTS MATCH THE HEADER'S              
         MVC   MINEKEY+1(L'OLDHDRSQ),OLDHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   VL95C                                                            
*                                                                               
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ                                                
         BNE   VL95C                                                            
         CLC   PIMCOMS1,OLDHDRSQ                                                
         BNE   VL95C                                                            
*                                                                               
         MVC   PIMCOMS1,NEWHDRSQ                                                
         BAS   RE,MINIOWRT                                                      
         B     VL95B                                                            
*                                                                               
VL95C    B     VLX                                                              
*                                                                               
         USING PIMHDREL,R6                                                      
VL100    MVC   MELEM,MELEM2                                                     
         MVC   PIMHDRSQ,MINEKEY+1                                               
         GOTO1 DATCON,DMCB,(5,0),(3,PIMACTDT)                                   
*                                                                               
*******  BAS   RE,CKPERIOD                                                      
*                                                                               
         MVC   MELEM2,MELEM                                                     
*                                                                               
         XC    MINEKEY,MINEKEY     REREAD THE INVOICE HEADER                    
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         BAS   RE,MINIORD                                                       
*                                                                               
         MVC   MELEM,MELEM2                                                     
*                                                                               
         BAS   RE,MINIOWRT         SO WE CAN WRITE HEADER BACK OUT              
*                                                                               
VLX      B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATES THE TOTAL FROM THE DETAILS FOR THE HEADER             
*                                                                               
* ON ENTRY:    (R2)                A(SCREEN LINE)                               
***********************************************************************         
CALCTOTS NTR1                                                                   
         USING LINDSECT,R2                                                      
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         ZAP   TOTDTLAM,=P'0'                                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   CTOTSX                                                           
*                                                                               
CTOTSLP  L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   CTOTSX                                                           
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   CTOTSX                                                           
*                                                                               
         GOTO1 CALCDTLG,DMCB,GETINSWK    CALCULATE GROSS FROM DETAIL            
         LA    R1,GETINSWK                                                      
         USING PVALUES,R1                                                       
         L     RE,GROSS                                                         
         CLI   LINGN,C'G'                                                       
         BE    *+10                                                             
         L     RF,AGYCOM                                                        
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         AP    TOTDTLAM,DUB                                                     
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    CTOTSLP                                                          
*                                                                               
CTOTSX   B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MATCHES AN INVOICE HEADER'S DETAILS                              
***********************************************************************         
MIHEADER NTR1                                                                   
         MVI   PFKEY,0             CLEAR THE PFKEY                              
         L     RE,ATIOB                                                         
         MVI   TIOBAID-TIOBD(RE),0                                              
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,LSTSEL2H                                                      
         CR    R2,R0                                                            
         BL    MIHNOT              IF MATCH AN ADD, THEN EXIT (NO DTLS)         
         LA    R0,LSTPFLNH                                                      
         CR    R2,R0                                                            
         BNL   MIHNOT              IF MATCH AN ADD, THEN EXIT (NO DTLS)         
*                                                                               
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL2H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(L'LSTHDRSQ,R1),0(R1)   NO INVOICE HEADER?                      
         BZ    MIHNOT              NONE, CAN'T MATCH                            
*                                                                               
         MVC   LSTHDRSQ,0(R1)                                                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIORD                                                       
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         SR    R0,R0                                                            
         OC    PIMEST,PIMEST                                                    
         BZ    *+14                                                             
         PACK  DUB,PIMEST          NEED ESTIMATE                                
         CVB   R0,DUB                                                           
         STH   R0,BEST                                                          
         MVC   SPCLREP,PIMSREP          SPECIAL REP                             
         MVC   INVSTDT,PIMSTDT          AND PERIOD TO MATCH                     
         MVC   INVENDDT,PIMENDDT                                                
*                                                                               
         GOTO1 MATCHUNS            MATCH UNMATCHED INVOICE DETAILS              
*                                                                               
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    MNIOFLAG,X'FF'-X'80'    DON'T NEED TO CLOSE AGAIN                
*                                                                               
MIHX     B     XIT                                                              
*                                                                               
MIHNOT   B     NEEDINVH            NO INVOICE HEADER                            
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORDS                                                              
***********************************************************************         
LR       DS    0H                                                               
         MVI   NLISTS,10                                                        
         XC    SEQLIST,SEQLIST     NO RECORDS SHOWN                             
*                                                                               
*  TWAXC LIST PORTION OF SCREEN AND ALSO VALIDATE FIELDS                        
         LA    R1,LSTSEL1H         CLEAR LIST PORTION OF SCREEN                 
         LA    R2,LSTLSTFH                                                      
         USING LINDSECT,R2                                                      
         LA    RF,LINSREPH                                                      
         DROP  R2                                                               
LR10LP   ZIC   RE,0(R1)            GET LENGTH                                   
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                 NO                                           
         SH    RE,=H'8'            YES                                          
         LTR   RE,RE                                                            
         BM    LR20                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)       NULL OUT THE DATA                            
         OI    6(R1),X'80'         TRANSMIT THE FIELD                           
         OI    4(R1),X'20'         VALIDATE THE FIELD                           
         ZIC   RE,0(R1)                                                         
         BXLE  R1,RE,LR10LP                                                     
*                                                                               
LR20     CLI   SVPROF+3,C'Y'       IF ACROSS ESTIMATES                          
         BE    LR22                                                             
         CLI   SVPROF+4,C'N'                                                    
         BNE   LR25                                                             
*                                                                               
LR22     L     R2,ATHISLST                                                      
         USING LINIDTH,R2                                                       
         OI    LINESTH+6,X'20'     PROTECT THE ESTIMATE FIELD                   
         DROP  R2                                                               
*                                                                               
LR25     GOTO1 LISTMON             1ST LINE USED TO ADD, USE NEXT LINE          
*                                                                               
         CLI   LISTSW,C'T'         SAME PAGE?  (BECAUSE OF LVALREC)             
         BNE   *+14                                                             
         MVC   LSTHDRSQ,FRSHDRSQ   YES, START FROM BEGINNING OF PAGE            
         B     LR30                                                             
*                                                                               
         CLI   PFKEY,5             TOP PAGE?                                    
         BNE   *+12                                                             
         MVI   LSTHDRSQ,0          YES, START FROM BEGINNING                    
         B     LR30                                                             
*                                                                               
         CLI   PFKEY,6             BOTTOM?                                      
         BNE   *+16                                                             
         BAS   RE,PF06             YES                                          
         BNE   LR90                                                             
         B     LR30                                                             
*                                                                               
         CLI   PFKEY,7             PREVIOUS PAGE?                               
         BNE   *+16                                                             
         BAS   RE,PF07             YES                                          
         BNE   LR90                                                             
         B     LR30                                                             
*                                                                               
LR30     MVC   FRSHDRSQ,LSTHDRSQ   SAVE THE FIRST HEADER SHOWN                  
*                                                                               
         MVI   MINEKEY,PIMHDREQ    SHOW INVOICE HEADER RECORDS                  
         MVC   MINEKEY+1(L'FRSHDRSQ),FRSHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   LR90                                                             
*                                                                               
LR40     L     R6,MINELEM          NOT INVOICE HEADERS?                         
         USING PIMHDREL,R6                                                      
         CLI   PIMHDREL,PIMHDREQ                                                
         BE    *+12                                                             
         MVI   LSTHDRSQ,0                                                       
         B     LR90                NO, WE'RE DONE                               
*                                                                               
         TM    BITFLAG,X'80'       FILTER ON YEAR & MONTH?                      
         BZ    LR45                                                             
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(0,DUB)   YES                            
         CLC   PERDFILT(4),DUB     MONTH/YEAR SHOULD BE WITHIN PERIOD           
         BL    LR80                                                             
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(0,DUB)                                 
         CLC   PERDFILT(4),DUB                                                  
         BH    LR80                GET NEXT HEADER IF NO MATCH                  
*                                                                               
LR45     TM    BITFLAG,X'40'       FILTER ON ESTIMATE?                          
         BZ    LR45S                                                            
         CLC   ESTFILT,PIMEST      YES                                          
         BNE   LR80                GET NEXT HEADER IF NO MATCH                  
*                                                                               
LR45S    TM    BITFLAG,X'20'       FILTER ON SREP?                              
         BZ    LR50                                                             
         CLC   SREPFILT,PIMSREP    YES                                          
         BNE   LR80                GET NEXT HEADER IF NO MATCH                  
*                                                                               
LR50     CLC   LISTNUM,NLISTS      IS THE LIST FULL?                            
         BE    LR70                YES, WE'RE DONE                              
*                                                                               
         L     R2,ATHISLST                                                      
         USING LINIDTH,R2                                                       
         GOTO1 DATCON,DMCB,(3,PIMINVDT),(11,LINIDT)                             
         MVC   LININV,PIMINVNO                                                  
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(11,LINPER)                              
         OC    PIMENDDT,PIMENDDT                                                
         BZ    LR60                                                             
         MVI   LINPER+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(11,LINPER+9)                           
*                                                                               
LR60     OC    PIMEST,PIMEST                                                    
         BZ    *+10                                                             
         MVC   LINEST,PIMEST                                                    
*                                                                               
         CLI   SVPROF+3,C'Y'       IF ACROSS ESTIMATES                          
         BNE   *+8                                                              
         OI    LINESTH+6,X'20'     PROTECT THE ESTIMATE FIELD                   
*                                                                               
         CLI   SVPROF+4,C'N'       IF WE DON'T ENTER ESTIMATE IN LIST           
         BNE   *+8                                                              
         OI    LINESTH+6,X'20'     PROTECT THE ESTIMATE FIELD                   
*                                                                               
LR65     MVI   LINGN,C'G'                                                       
         TM    PIMSTAT,X'40'                                                    
         BO    *+8                                                              
         MVI   LINGN,C'N'                                                       
*                                                                               
*        EDIT  (P5,PIMAMT),(11,LINAMT),2,ALIGN=LEFT,MINUS=YES                   
         EDIT  (P5,PIMAMT),(11,LINAMT),2,MINUS=YES                              
*                                                                               
         MVI   LINCD,C'N'                                                       
         TM    PIMSTAT,X'20'                                                    
         BO    *+8                                                              
         MVI   LINCD,C'Y'                                                       
*                                                                               
         OC    PIMSREP,PIMSREP                                                  
         BZ    *+10                                                             
         MVC   LINSREP,PIMSREP                                                  
*                                                                               
         ZIC   R1,LISTNUM          SAVE THE HEADER SEQUENCE NUMBER              
         BCTR  R1,0                    THAT WAS PUT ON THE LIST                 
         LA    R1,SEQLIST(R1)                                                   
         MVC   0(L'PIMHDRSQ,R1),PIMHDRSQ                                        
*                                                                               
LR70     MVC   LSTHDRSQ,PIMHDRSQ   SAVE LAST HEADER SEQUENCE SHOWN              
*                                                                               
         TM    BITFLAG,X'04'       REACHED BEYOND OUR LIST?                     
         BNZ   LR90                YES, NO MORE LINES                           
*                                                                               
LRLSTMON GOTO1 LISTMON                                                          
         BE    LR80                                                             
         OI    BITFLAG,X'04'       FLAG THAT THERE ARE NO MORE LINES            
*                                                                               
LR80     BAS   RE,MINIOSEQ                                                      
         BE    LR40                                                             
         MVI   LSTHDRSQ,0                                                       
*                                                                               
LR90     CLI   ADDEDFLG,C'Y'       ADDED A NEW INVOICE?                         
         BNE   LR150                                                            
*                                                                               
         NI    BITFLAG,X'FF'-X'04'  DON'T CARE HERE IF NO MORE LINES            
*                                                                               
         GOTO1 MINIO,DMCB,('MINCLS',(R5))   CLOSE MINIO BUFFER BEFORE           
         CLI   MINERR,0                WE GO TO THE CHECK SCREEN                
         BE    *+6                                                              
         DC    H'0'                DIE IF THERE ARE ANY                         
         NI    MNIOFLAG,X'FF'-X'80'                                             
*                                                                               
         MVC   BYTE,SVPROF         GET NEXT SCREEN FROM PROFILE                 
         TM    BITFLAG,X'10'       IS THERE AN OVERRIDE TO THE SCREEN?          
         BZ    *+10                                                             
         MVC   BYTE,NEXTSCRN       YES, USE THIS INSTEAD OF PROFILE'S           
*                                                                               
         CLI   BYTE,C'L'           STAY ON LIST SCREEN?                         
         BE    LRX                 YES                                          
*                                                                               
         CLI   BYTE,C'C'           GO TO CHECK SCREEN?                          
         BNE   *+12                                                             
         MVI   PFKEY,14                                                         
         B     LR100                                                            
*                                                                               
         CLI   BYTE,C'U'           GO TO UPDATE SCREEN?                         
         BNE   *+12                                                             
         MVI   PFKEY,15                                                         
         B     LR100                                                            
*                                                                               
         CLI   BYTE,C'S'           GO TO SUPERCHECK SCREEN?                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PFKEY,16                                                         
*                                                                               
LR100    LA    R2,PFTABLE                                                       
         GOTO1 INITIAL,DMCB,(R2)                                                
         B     LRX                                                              
*                                                                               
LR150    TM    BITFLAG,X'01'       GO TO CHECK SCREEN AFTER PAY?                
         BZ    LRX                                                              
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',KEY,80,GLVPRMAT                            
         CLI   DMCB+8,0                                                         
         BNE   LR155                                                            
*                                                                               
         LA    R1,KEY                                                           
         USING PINVKEY,R1                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'PINVMIS1),PINVMIS1                                   
         DROP  R1                                                               
*                                                                               
         BAS   RE,MINIORD                                                       
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         MVC   IHDREST,PIMEST                                                   
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(11,IHDRPER)                             
         MVI   IHDRPER+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(11,IHDRPER+9)                          
         MVC   IHDRNUMB,PIMINVNO                                                
         MVC   IHDRSREP,PIMSREP                                                 
*                                                                               
LR155    DS    0H                  MIGHT GET HERE ON GLOBBER ERROR              
*                                                                               
         MVI   PFKEY,14            ACTIVATE THE CHECK SCREEN                    
         B     LR100                                                            
*                                                                               
LRX      TM    BITFLAG,X'04'                                                    
         BZ    XIT                                                              
         NI    BITFLAG,X'FF'-X'04'                                              
         B     LRLSTMON            IT'LL PUT OUT "HIT ENTER FOR NEXT"           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* INVOICE RECORD, LIST MODE, PF6 INVOKED FOR BOTTOM.                            
***********************************************************************         
PF06     NTR1                                                                   
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
*                                                                               
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         BAS   RE,MINIOHI                                                       
         BNE   PF06NO                                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         CLI   PIMHDREL,PIMHDREQ                                                
         BNE   PF06NO                                                           
*                                                                               
PF0610   BAS   RE,MINIOSEQ                                                      
         BE    *+14                                                             
         CLI   MINERR,MINEEOF                                                   
         BE    PF0620                                                           
         DC    H'0'                                                             
*                                                                               
         CLI   PIMHDREL,PIMHDREQ                                                
         BNE   PF0620                                                           
         MVC   LSTHDRSQ,PIMHDRSQ                                                
         B     PF0610                                                           
*                                                                               
PF0620   XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         BAS   RE,MINIOHI                                                       
         BNE   PF06NO                                                           
*                                                                               
         LA    R2,1                GOT 1                                        
*                                                                               
PF0630   GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,0                                                         
         BE    PF0640                                                           
         CLI   MINERR,MINEEOF      BEGINNING OF RECORD?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   LSTHDRSQ,0          YES, START FROM BEGINNING                    
         B     PF06YES                                                          
*                                                                               
PF0640   TM    BITFLAG,X'80'       FILTER ON YEAR & MONTH?                      
         BZ    PF0650                                                           
         GOTO1 DATCON,DMCB,(3,PIMINVDT),(0,DUB)   YES                           
         CLC   PERDFILT(4),DUB                                                  
         BNE   PF0630              GET PREV HEADER IF NO MATCH                  
*                                                                               
PF0650   TM    BITFLAG,X'40'       FILTER ON ESTIMATE?                          
         BZ    PF0660                                                           
         CLC   ESTFILT,PIMEST      YES                                          
         BNE   PF0630              GET PREV HEADER IF NO MATCH                  
*                                                                               
PF0660   LA    R2,1(R2)            GOT 1 MORE                                   
         CH    R2,=H'9'            CAN WE FILL A SCREEN?                        
         BNE   PF0630              NO, GO BACK FOR MORE                         
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         MVC   LSTHDRSQ,PIMHDRSQ   YES, SAVE WHERE TO START                     
*                                                                               
PF06YES  B     YES                                                              
*                                                                               
PF06NO   B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* INVOICE RECORD, LIST MODE, PF7 INVOKED FOR PREVIOUS.                          
***********************************************************************         
PF07     NTR1                                                                   
         OC    FRSHDRSQ,FRSHDRSQ                                                
         BNZ   *+14                                                             
         MVC   LSTHDRSQ,FRSHDRSQ                                                
         B     PF07YES                                                          
*                                                                               
         XC    MINEKEY,MINEKEY     GET FIRST INVOICE HEADER OF PAGE             
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'FRSHDRSQ),FRSHDRSQ                                   
         BAS   RE,MINIOHI                                                       
         BNE   PF07NO                                                           
         LA    R2,1                GOT 1                                        
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
*                                                                               
PF07LP   GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,0                                                         
         BE    PF0710                                                           
         CLI   MINERR,MINEEOF      BEGINNING OF RECORD?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   LSTHDRSQ,0          YES, START FROM BEGINNING                    
         B     PF07YES                                                          
*                                                                               
PF0710   TM    BITFLAG,X'80'       FILTER ON YEAR & MONTH?                      
         BZ    PF0720                                                           
         GOTO1 DATCON,DMCB,(3,PIMINVDT),(0,DUB)   YES                           
         CLC   PERDFILT(4),DUB                                                  
         BNE   PF07LP              GET PREV HEADER IF NO MATCH                  
*                                                                               
PF0720   TM    BITFLAG,X'40'       FILTER ON ESTIMATE?                          
         BZ    PF0730                                                           
         CLC   ESTFILT,PIMEST      YES                                          
         BNE   PF07LP              GET PREV HEADER IF NO MATCH                  
*                                                                               
PF0730   LA    R2,1(R2)            GOT 1 MORE                                   
         CH    R2,=H'9'            CAN WE FILL A SCREEN?                        
         BNE   PF07LP              NO, GO BACK FOR MORE                         
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         MVC   LSTHDRSQ,PIMHDRSQ   YES, SAVE WHERE TO START                     
*                                                                               
PF07YES  B     YES                                                              
*                                                                               
PF07NO   B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FILLS IN THE FIELDS WE GOT FROM GLOBBER.                         
***********************************************************************         
GLOBBED  NTR1                                                                   
         OI    BITFLAG,X'01'       CAME BACK FROM PAY                           
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETF',LSTPAYRH,,GLVPRREQ                         
         CLI   DMCB+8,0                                                         
         BNE   GLOBNO                                                           
         OI    LSTPAYRH+6,X'80'                                                 
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETF',LSTMEDH,,GLVPRMD                           
         CLI   DMCB+8,0                                                         
         BNE   GLOBNO                                                           
         OI    LSTMEDH+6,X'80'                                                  
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETF',LSTCLTH,,GLVPRCLT                          
         CLI   DMCB+8,0                                                         
         BNE   GLOBNO                                                           
         OI    LSTCLTH+6,X'80'                                                  
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,80,GLVPRPRD                          
         CLI   DMCB+8,0                                                         
         BNE   GLOBNO                                                           
         MVC   LSTPRD(L'QPRD),BLOCK                                             
         MVI   LSTPRDH+5,L'QPRD                                                 
         OI    LSTPRDH+6,X'80'                                                  
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETF',LSTPUBH,,GLVPRPUB                          
         CLI   DMCB+8,0                                                         
         BNE   GLOBNO                                                           
         OI    LSTPUBH+6,X'80'                                                  
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',KEY,80,GLVPRMAT                            
         CLI   DMCB+8,0                                                         
         BNE   GLOBNO                                                           
*                                                                               
         LA    R3,KEY                                                           
         USING PINVKEY,R3                                                       
         MVC   LSTYEAR,PINVYS                                                   
         MVI   LSTYEARH+5,1                                                     
         OI    LSTYEARH+4,X'08'                                                 
         DROP  R3                                                               
*                                                                               
GLOBYES  B     YES                                                              
*                                                                               
GLOBNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATES THE NEXT HEADER SEQUENCE NUMBER.                      
***********************************************************************         
NXTHDRSQ NTR1                                                                   
         LA    R0,X'FD'                                                         
         SR    R2,R2                                                            
         SR    R3,R3               R3 = COUNTER OF HDRS FOR PUB/MONTH           
         MVI   BYTE,X'FF'          BYTE STORES LOWEST SEQ # FOR MONTH           
         MVC   MELEM2,MELEM                                                     
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
*                                                                               
         BAS   RE,MINIOHI          ANY INVOICE HEADERS?                         
         BE    NXH10                                                            
         CLI   MINERR,MINEEOF                                                   
         BE    NXH25                                                            
         CLI   MINERR,MINERNF                                                   
         BE    NXH25                                                            
         CLI   MINERR,MINESNF                                                   
         BE    NXH25                                                            
         DC    H'0'                                                             
*                                                                               
NXH10    CLI   PIMHDREL,PIMHDREQ   NO INVOICE HEADERS                           
         BNE   NXH25                                                            
*                                                                               
NXHLOOP  CLI   MINERR,0            YES                                          
         BE    NXH30                                                            
         CLI   MINERR,MINEEOF                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R0,BYTE                                                          
         B     NXH21                                                            
*                                                                               
NXH20    ZIC   R0,PIMHDRSQ         CALCULATE NEW SEQUENCE NUMBER                
         LTR   R3,R3                                                            
         BZ    NXH21                                                            
         CLC   PIMHDRSQ,BYTE                                                    
         BL    NXH21                                                            
         IC    R0,BYTE                                                          
NXH21    AR    R0,R2                                                            
         SRL   R0,1                                                             
*                                                                               
         CR    R0,R2               SAME SEQUENCE AS A PREVIOUS ONE?             
         BNE   *+14                                                             
NXH23    MVC   MELEM,MELEM2        YES, WILL NEED TO RESEQUENCE                 
         B     NXHNO                                                            
*                                                                               
NXH25    STC   R0,BYTE             NO, GOT OUR SEQUENCE NUMBER                  
NXH25A   MVC   MELEM,MELEM2                                                     
         B     NXHYES                                                           
*                                                                               
NXH30    CLI   PIMHDREL,PIMHDREQ   NO MORE INVOICE HEADERS                      
         BE    NXH40                                                            
         ZIC   R0,BYTE                                                          
         B     NXH21                                                            
*                                                                               
NXH40    CLC   PIMSTDT(2),MELEM2+PIMSTDT-PIMHDREL    SAME MONTH?                
*        CLC   PIMINVDT(2),MELEM2+PIMINVDT-PIMHDREL    SAME MONTH?              
         BNE   NXH50                                                            
*                                                                               
         LA    R3,1(R3)                                YES                      
         CH    R3,=H'12'         HAVE 12 HDRS THIS PUB MONTH ALREADY?           
         BL    NXH45             NO, WE HAVE ROOM FOR MORE                      
         LA    R2,LSTSEL1H                                                      
         B     NOHDRMER          YES, NO HEADROOM, CAN'T ADD HEADER             
*                                                                               
NXH45    CLC   PIMSTDT,MELEM2+PIMSTDT-PIMHDREL   FIND # OF HDRS IN              
*        CLC   PIMINVDT,MELEM2+PIMINVDT-PIMHDREL   FIND # OF HDRS IN            
         BNL   NXH60                                   THIS PUB/MONTH           
*                                                                               
         CLC   PIMHDRSQ,BYTE       BYTE STORES LOWEST SEQ # FOR MONTH           
         BH    *+10                                                             
         MVC   BYTE,PIMHDRSQ                                                    
         B     NXH70                                                            
*                                                                               
NXH50    CLC   PIMSTDT,MELEM2+PIMSTDT-PIMHDREL                                  
*        CLC   PIMINVDT,MELEM2+PIMINVDT-PIMHDREL                                
         BL    NXH20                                                            
*                                                                               
NXH60    ZIC   R2,PIMHDRSQ                                                      
*                                                                               
NXH70    BAS   RE,MINIOSEQ                                                      
         B     NXHLOOP                                                          
*                                                                               
NXHYES   B     YES                                                              
NXHNO    B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
*******  BAS   RE,CKPERIOD         CHECK FOR INTERSECTING PERIODS               
*                                                                               
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE CHECKS FOR INTERSECTING PERIODS.                                 
*                                                                               
* RULE:  IF ESTIMATE AND SPECIAL REP ARE THE SAME IN TWO INVOICE                
*        DETAILS, THEN WE HAVE AN ERROR IF THE PERIODS INTERSECT.               
*                                                                               
* AS LONG AS THE ABOVE RULE IS ENFORCED, WE CAN HAVE THE SAME INVOICE           
* NUMBER FOR 2 INVOICE HEADERS BECAUSE THE PERIODS ARE GUARANTEED NOT           
* TO INTERSECT.                                                                 
***********************************************************************         
CKPERIOD NTR1                                                                   
         USING LINDSECT,R2                                                      
         LA    R2,LINPERH                                                       
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ    LOOK FOR FIRST INVOICE HEADER                
*                                                                               
         BAS   RE,MINIOHI                                                       
CKPLOOP  BNE   CKPYES              IF NONE, RESTORE HEADER TO ADD               
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
*                                                                               
         CLI   PIMHDREL,PIMHDREQ   IF NO MORE HEADERS, THEN 'YES'               
         BNE   CKPYES                                                           
*                                                                               
         CLC   PIMHDRSQ,MELEM2+PIMHDRSQ-PIMHDREL   SAME SEQUENCE                
         BE    CKPNEXT             YES, MUST BE ON A CHANGE                     
*                                                                               
         CLC   PIMEST,MELEM2+PIMEST-PIMHDREL   SAME ESTIMATE?                   
         BNE   CKPNEXT             NO, CHECK NEXT HEADER                        
*                                                                               
         CLC   PIMSREP,MELEM2+PIMSREP-PIMHDREL   SAME SPECIAL REP?              
         BNE   CKPNEXT             NO, CHECK NEXT HEADER                        
*                                                                               
         CLC   PIMSTDT,MELEM2+PIMSTDT-PIMHDREL   IF PERIODS CROSS               
         BE    INVLPERD                          THEN ERROR                     
         BH    CKP10                                                            
         CLC   PIMENDDT,MELEM2+PIMSTDT-PIMHDREL                                 
         BL    CKPNEXT                                                          
         B     INVLPERD                                                         
*                                                                               
CKP10    CLC   PIMSTDT,MELEM2+PIMENDDT-PIMHDREL                                 
         BNH   INVLPERD                                                         
*                                                                               
CKPNEXT  BAS   RE,MINIOSEQ         CHECK NEXT ELEMENT                           
         B     CKPLOOP                                                          
*                                                                               
CKPYES   MVC   MELEM,MELEM2                                                     
         B     YES                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* AS OF MARCH 1995 THIS ROUTINE IS NO LONGER CALLED.                            
* TO CALL IT WILL CREATE PROBLEMS WITH THE CHANGES MADE TO ALLOW FOR            
* EPIC.                                                                         
* THIS ROUTINE CHECKS TO SEE IF THERE ARE ANY CONFLICTS FOR PUB,ALL AND         
* THE SAME PUB WITH ANY OTHER ZONE OR EDITION.                                  
***********************************************************************         
ANYOZNED NTR1                                                                   
         XC    KEY,KEY             SEE IF MASTER WITH                           
         LA    R3,KEY                                                           
         USING PINVKEY,R3                                                       
         MVC   PINVAGY,AGENCY                                                   
         MVC   PINVMED,QMED                                                     
         MVI   PINVTYPE,PINVTYPQ                                                
         MVC   PINVCLT,QCLT                                                     
         MVC   PINVPRD,QPRD                                                     
         MVC   PINVPUB(L'BPUB),BPUB                                             
         XC    PINVPUB+4(2),PINVPUB+4                                           
*                                                                               
         LA    R2,LSTYEARH                                                      
         CLI   5(R2),0                                                          
         BNE   ANYZ00                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)                                        
         MVC   LSTYEAR,DUB+1       COPY THE LAST DIGIT OF YEAR                  
         MVC   QYEAR,DUB+1                                                      
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'08'                                                      
*                                                                               
ANYZ00   TM    4(R2),X'08'                                                      
         BZ    INVLFLD                                                          
         MVC   PINVYS,LSTYEAR                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PINVPUB-PINVKEY),KEYSAVE                                     
         BNE   ANYZX                                                            
*                                                                               
         CLC   PINVPUB(4),BPUB     NEW PUB                                      
         BNE   ANYZX                                                            
*                                                                               
         LA    R2,LSTPUBH                                                       
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    ANYZ20              NO                                           
         CLC   =X'FFFF',PINVPUB+4                                               
         BE    ANYZX                                                            
ANYZ10   CLC   PINVYS,LSTYEAR                                                   
         BNE   ANYZX                                                            
         B     PUBALPUB            CAN'T HAVE ALL AND NON-ALL                   
*                                                                               
ANYZ20   CLC   =X'FFFF',PINVPUB+4                                               
         BE    ANYZ10                                                           
*                                                                               
ANYZX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* THIS ROUTINE MAKES SURE THAT ALL THE DETAILS ARE STILL IN THE                 
* CHANGED PERIOD FOR THE INVOICE HEADER.  ERROR IF NOT.                         
***********************************************************************         
CHNGPERD NTR1                                                                   
         USING LINSELH,R2                                                       
         MVC   MELEM2,MELEM                                                     
*                                                                               
         MVI   MINEKEY,PIMDTLEQ    SET UP THE DETAIL KEY USING THE              
         ZIC   R1,SELLISTN             HEADER'S SEQUENCE NUMBER                 
         BCTR  R1,0                                                             
         LA    R1,SEQLIST(R1)                                                   
         MVC   LSTHDRSQ,0(R1)                                                   
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI          GET THE FIRST DETAIL FOR THE HEADER          
         BNE   CPERX                                                            
*                                                                               
CPERLOOP LA    R6,MELEM                                                         
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ   STILL AN INVOICE DETAIL?                     
         BNE   CPERX                                                            
         CLC   PIMDTLS1,LSTHDRSQ   STILL FOR THIS INVOICE HEADER                
         BNE   CPERX               IF NOT THEN WE'RE DONE                       
*                                                                               
         CLC   PIMIDATE,MELEM2+PIMSTDT-PIMHDREL  IS DATE IN PERIOD?             
         BNL   *+16                                                             
CPERERR  LA    R2,LINPERH                                                       
         MVI   GERROR1,CANTCPER                                                 
         B     ERREXIT             CANCELS ANY RECORD CHANGES WE MADE           
*                                                                               
         CLC   PIMIDATE,MELEM2+PIMENDDT-PIMHDREL                                
         BH    CPERERR             NO                                           
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    CPERLOOP                                                         
*                                                                               
CPERX    MVC   MELEM,MELEM2                                                     
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THERE ARE ANY CORRECTED OR MATCHED              
* INVOICE DETAILS.  IF THERE IS THEN WE WILL REPORT AN ERROR BECAUSE            
* WE ARE TRYING TO CHANGE THE ESTIMATE OF THE INVOICE HEADER.                   
***********************************************************************         
CHNGESTM NTR1                                                                   
         USING LINSELH,R2                                                       
         MVC   MELEM2,MELEM        SAVE WHAT WE HAVE SO FAR                     
*                                                                               
         MVI   MINEKEY,PIMDTLEQ    SET UP THE DETAIL KEY USING THE              
         ZIC   R1,SELLISTN             HEADER'S SEQUENCE NUMBER                 
         BCTR  R1,0                                                             
         LA    R1,SEQLIST(R1)                                                   
         MVC   LSTHDRSQ,0(R1)                                                   
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI          GET THE FIRST DETAIL FOR THE HEADER          
         BNE   CESTX                                                            
*                                                                               
CESTLOOP LA    R6,MELEM                                                         
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ   STILL AN INVOICE DETAIL?                     
         BNE   CESTX                                                            
         CLC   PIMDTLS1,LSTHDRSQ   STILL FOR THIS INVOICE HEADER                
         BNE   CESTX               IF NOT THEN WE'RE DONE                       
*                                                                               
         CLI   PIMBLINE,0          MATHCED OR CORRECTED?                        
         BE    *+16                NO                                           
         LA    R2,LINESTH                                                       
         MVI   GERROR1,CANTCEST    YES, CAN'T CHANGE ESTIMATE                   
         B     ERREXIT             CANCELS ANY RECORD CHANGES WE MADE           
*                                                                               
         MVC   PIMIEST,BEST        COPY CHANGED HDR'S EST                       
         BAS   RE,MINIOWRT         WRITE THE DETAIL OUT                         
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    CESTLOOP                                                         
*                                                                               
CESTX    MVC   MELEM,MELEM2                                                     
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE     INVALID DATE EXPRESSION                      
         B     ERREXIT                                                          
*                                                                               
INVLOPTN MVI   GERROR1,BADOPTN     INVALID OPTION SPECIFIED                     
         B     ERREXIT                                                          
*                                                                               
INVLEPER MVI   GERROR1,ERREPERD    ERROR: ESTIMATE PERIOD                       
         B     ERREXIT                                                          
*                                                                               
INVLPERD MVI   GERROR1,CROSPERD    CROSSING PERIODS W/ SAME EST & SREP          
         B     ERREXIT                                                          
*                                                                               
INVPEROD MVI   GERROR1,INVLPER     INVALID PERIOD                               
         B     ERREXIT                                                          
*                                                                               
*NOBUYSIN MVI   GERROR1,NOBYINPD    NO INSERTIONS IN THE PERIOD                 
*         B     ERREXIT                                                         
*                                                                               
PUBALPUB MVI   GERROR1,INVPUBAL    CAN'T HAVE PUB,ALL AND PUB,????              
         B     ERREXIT                                                          
*                                                                               
NEEDINVH MVI   GERROR1,NOINVHDR    NO INVOICE HEADER                            
         B     ERREXIT                                                          
*                                                                               
CANTDELE MVI   GERROR1,DONTDELE    DELETE ALL THE INVOICE DETAILS FIRST         
         B     ERREXIT                                                          
*                                                                               
NOHDRMER MVI   GERROR1,OVER9HDR    HAVE 9 HEADERS THIS PUB/MONTH ALRDY          
         B     ERREXIT                                                          
*                                                                               
INVLPFK  MVI   GERROR1,MUSTENTR    FIELD(S) CHANGED - ENTER TO COMMIT           
         B     ERREXIT                                                          
*                                                                               
INVLPROF MVI   GERROR1,NOPROF      PROGRAM PROFILE NEEDED FOR MATCH             
         B     ERREXIT                                                          
*                                                                               
RECLOCKD MVI   GERROR1,DATALOK     BUYS LOCKED FOR OFFLINE PROCESSING           
         LA    R2,LSTMEDH                                                       
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         NI    MNIOFLAG,X'FF'-X'80'  DON'T SAVE CHANGES ON ERRORS               
         B     MYERRXIT                                                         
*                                                                               
NEEDPAYR MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE LIST                                      
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* INVOICE CHECK                                                                 
         DC    AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                   
         DC    CL3'C',CL8'INVOICE ',CL8'CHECK   '                               
PF02     DC    AL1(KEYTYTWA,L'CHKMED-1),AL2(LSTMED-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKCLT-1),AL2(LSTCLT-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKPRD-1),AL2(LSTPRD-T402FFD)                     
         DC    AL1(KEYTYCUR,L'CHKESTM-1),AL2(LINEST-LINIDT)                     
         DC    AL1(KEYTYCUR,L'CHKPER-1),AL2(LINPER-LINIDT)                      
         DC    AL1(KEYTYTWA,L'CHKPUB-1),AL2(LSTPUB-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKYEAR-1),AL2(LSTYEAR-T402FFD)                   
         DC    AL1(KEYTYCUR,L'CHKINVN-1),AL2(LININV-LINIDT)                     
         DC    AL1(KEYTYCUR,L'CHKREPN-1),AL2(LINSREP-LINIDT)                    
         DC    AL1(KEYTYTWA,L'CHKINIT-1),AL2(LSTPAYR-T402FFD)                   
PF02X    EQU   *                                                                
*                                                                               
* INVOICE UPDATE                                                                
         DC    AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                   
         DC    CL3'U',CL8'INVOICE ',CL8'UPDATE  '                               
PF03     DC    AL1(KEYTYTWA,L'CHKMED-1),AL2(LSTMED-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKCLT-1),AL2(LSTCLT-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKPRD-1),AL2(LSTPRD-T402FFD)                     
         DC    AL1(KEYTYCUR,L'CHKESTM-1),AL2(LINEST-LINIDT)                     
         DC    AL1(KEYTYCUR,L'CHKPER-1),AL2(LINPER-LINIDT)                      
         DC    AL1(KEYTYTWA,L'CHKPUB-1),AL2(LSTPUB-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKYEAR-1),AL2(LSTYEAR-T402FFD)                   
         DC    AL1(KEYTYCUR,L'CHKINVN-1),AL2(LININV-LINIDT)                     
         DC    AL1(KEYTYCUR,L'CHKREPN-1),AL2(LINSREP-LINIDT)                    
         DC    AL1(KEYTYTWA,L'CHKINIT-1),AL2(LSTPAYR-T402FFD)                   
PF03X    EQU   *                                                                
*                                                                               
* INVOICE SUPERCHECK                                                            
         DC    AL1(PF09X-*,09,PFTCPROG,(PF09X-PF09)/KEYLNQ,0)                   
         DC    CL3'S',CL8'INVOICE ',CL8'SUPERCK '                               
PF09     DC    AL1(KEYTYTWA,L'CHKMED-1),AL2(LSTMED-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKCLT-1),AL2(LSTCLT-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKPRD-1),AL2(LSTPRD-T402FFD)                     
         DC    AL1(KEYTYCUR,L'CHKESTM-1),AL2(LINEST-LINIDT)                     
         DC    AL1(KEYTYCUR,L'CHKPER-1),AL2(LINPER-LINIDT)                      
         DC    AL1(KEYTYTWA,L'CHKPUB-1),AL2(LSTPUB-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKYEAR-1),AL2(LSTYEAR-T402FFD)                   
         DC    AL1(KEYTYCUR,L'CHKINVN-1),AL2(LININV-LINIDT)                     
         DC    AL1(KEYTYCUR,L'CHKREPN-1),AL2(LINSREP-LINIDT)                    
         DC    AL1(KEYTYTWA,L'CHKINIT-1),AL2(LSTPAYR-T402FFD)                   
PF09X    EQU   *                                                                
*                                                                               
* INVOICE DELETE                   CURSDISP SET TO DISP(LINE)                   
         DC    AL1(PF13X-*,13,0,0,PFTRETRN)                                     
         DC    CL3'D',CL8'        ',CL8'        '                               
PF13X    EQU   *                                                                
*                                                                               
* INVOICE CHECK AFTER ADD                                                       
         DC    AL1(PF14X-*,14,PFTCPROG,(PF14X-PF14)/KEYLNQ,0)                   
         DC    CL3' ',CL8'INVOICE ',CL8'CHECK   '                               
PF14     DC    AL1(KEYTYTWA,L'CHKMED-1),AL2(LSTMED-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKCLT-1),AL2(LSTCLT-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKPRD-1),AL2(LSTPRD-T402FFD)                     
         DC    AL1(KEYTYWS,L'CHKESTM-1),AL2(IHDREST-MYAREAD)                    
         DC    AL1(KEYTYWS,L'CHKPER-1),AL2(IHDRPER-MYAREAD)                     
         DC    AL1(KEYTYTWA,L'CHKPUB-1),AL2(LSTPUB-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKYEAR-1),AL2(LSTYEAR-T402FFD)                   
         DC    AL1(KEYTYWS,L'CHKINVN-1),AL2(IHDRNUMB-MYAREAD)                   
         DC    AL1(KEYTYWS,L'CHKREPN-1),AL2(IHDRSREP-MYAREAD)                   
         DC    AL1(KEYTYTWA,L'CHKINIT-1),AL2(LSTPAYR-T402FFD)                   
PF14X    EQU   *                                                                
*                                                                               
* INVOICE UPDATE AFTER ADD                                                      
         DC    AL1(PF15X-*,15,PFTCPROG,(PF15X-PF15)/KEYLNQ,0)                   
         DC    CL3' ',CL8'INVOICE ',CL8'UPDATE  '                               
PF15     DC    AL1(KEYTYTWA,L'CHKMED-1),AL2(LSTMED-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKCLT-1),AL2(LSTCLT-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKPRD-1),AL2(LSTPRD-T402FFD)                     
         DC    AL1(KEYTYWS,L'CHKESTM-1),AL2(IHDREST-MYAREAD)                    
         DC    AL1(KEYTYWS,L'CHKPER-1),AL2(IHDRPER-MYAREAD)                     
         DC    AL1(KEYTYTWA,L'CHKPUB-1),AL2(LSTPUB-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKYEAR-1),AL2(LSTYEAR-T402FFD)                   
         DC    AL1(KEYTYWS,L'CHKINVN-1),AL2(IHDRNUMB-MYAREAD)                   
         DC    AL1(KEYTYWS,L'CHKREPN-1),AL2(IHDRSREP-MYAREAD)                   
         DC    AL1(KEYTYTWA,L'CHKINIT-1),AL2(LSTPAYR-T402FFD)                   
PF15X    EQU   *                                                                
*                                                                               
* INVOICE SUPERCHECK AFTER ADD                                                  
         DC    AL1(PF16X-*,16,PFTCPROG,(PF16X-PF16)/KEYLNQ,0)                   
         DC    CL3' ',CL8'INVOICE ',CL8'SUPERCK '                               
PF16     DC    AL1(KEYTYTWA,L'CHKMED-1),AL2(LSTMED-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKCLT-1),AL2(LSTCLT-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKPRD-1),AL2(LSTPRD-T402FFD)                     
         DC    AL1(KEYTYWS,L'CHKESTM-1),AL2(IHDREST-MYAREAD)                    
         DC    AL1(KEYTYWS,L'CHKPER-1),AL2(IHDRPER-MYAREAD)                     
         DC    AL1(KEYTYTWA,L'CHKPUB-1),AL2(LSTPUB-T402FFD)                     
         DC    AL1(KEYTYTWA,L'CHKYEAR-1),AL2(LSTYEAR-T402FFD)                   
         DC    AL1(KEYTYWS,L'CHKINVN-1),AL2(IHDRNUMB-MYAREAD)                   
         DC    AL1(KEYTYWS,L'CHKREPN-1),AL2(IHDRSREP-MYAREAD)                   
         DC    AL1(KEYTYTWA,L'CHKINIT-1),AL2(LSTPAYR-T402FFD)                   
PF16X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SET THE APPROPRIATE PFKEY TABLE                                               
***********************************************************************         
SETPFTBL DS    0H                                                               
         NMOD1 0,**STPF**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
         SR    R2,R2               NO TABLE NEEDED YET                          
*                                                                               
         TM    LSTMEDH+4,X'20'     IF ANY OF THE KEY FIELDS CHANGE              
         BZ    PFTBL20                                                          
         TM    LSTCLTH+4,X'20'                                                  
         BZ    PFTBL20                                                          
         TM    LSTPRDH+4,X'20'                                                  
         BZ    PFTBL20                                                          
         TM    LSTPUBH+4,X'20'                                                  
         BZ    PFTBL20                                                          
         TM    LSTYEARH+4,X'20'                                                 
         BZ    PFTBL20                                                          
         TM    LSTPERH+4,X'20'                                                  
         BZ    PFTBL20                                                          
         TM    LSTOPTH+4,X'20'                                                  
         BNZ   PFTBL40                                                          
*                                                                               
PFTBL20  XC    SEQLIST,SEQLIST     THEN SEQUENCE LIST GETS CLOBBERED            
         LA    R3,LSTLSTF                                                       
         USING LINDSECT,R3                                                      
         LA    R3,LINSREPH                                                      
         DROP  R3                                                               
         TWAXC LSTSEL1H,(R3),PROT=Y                                             
         B     PFTBLX                  AND DON'T ALLOW PFKEY PROCESSING         
*                                                                               
PFTBL40  DS    0H      IF ANY FIELDS CHANGED, ENTER IS ONLY VALID PFKEY         
         CLI   PFKEY,0             "ENTER" ?                                    
         BE    PFTBL80             YES                                          
PFTBL45  DS    0H      IF ANYTHING ENTERED THIS TIME, GIVE ERR MESSAGE          
         LA    R2,LSTSEL1H                                                      
PFTBL46  CLI   0(R2),0             TWA END ?                                    
         BE    PFTBL50             OK - CONTINUE                                
         TM    1(R2),X'20'         PROTECTED ?                                  
         BO    PFTBL49             YES - NEXT FIELD                             
         TM    4(R2),X'80'         INPUT THIS TIME ?                            
         BZ    PFTBL49             NO - OK                                      
         OI    6(R2),X'81'         SET MODIFIED FOR NEXT I/P & TRANSMIT         
         B     INVLPFK             ERROR MESSAGE                                
PFTBL49  ZIC   RE,0(R2)                                                         
         AR    R2,RE               NEXT FIELD                                   
         B     PFTBL46                                                          
*                                                                               
PFTBL50  DS    0H                                                               
         CLI   PFKEY,2                                                          
         BE    PFTBL60                                                          
         CLI   PFKEY,3                                                          
         BE    PFTBL60                                                          
         CLI   PFKEY,4                                                          
         BE    PFTBL60                                                          
         CLI   PFKEY,9                                                          
         BNE   PFTBL80                                                          
*                                                                               
PFTBL60  LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,LSTSEL2H                                                      
         CR    R2,R0                                                            
         BL    NEEDINVH                                                         
         LA    R0,LSTPFLNH                                                      
         CR    R2,R0                                                            
         BNL   NEEDINVH                                                         
*                                                                               
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL2H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(L'LSTHDRSQ,R1),0(R1)   NO INVOICE HEADER?                      
         BZ    NEEDINVH            DON'T GO ANYWHERE                            
*                                                                               
PFTBL80  SR    R2,R2                                                            
         CLI   PFKEY,4                                                          
         BL    *+12                                                             
         CLI   PFKEY,8                                                          
         BNH   PFTBLX                                                           
*                                                                               
         LA    R2,PFTABLE                                                       
*                                                                               
PFTBLX   GOTO1 INITIAL,DMCB,(R2)                                                
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DELETES AN INVOICE HEADER AND IT'S DETAILS                       
***********************************************************************         
DIHEADER DS    0H                                                               
         NMOD1 0,**DIHD**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         MVI   PFKEY,0             CLEAR THE PFKEY                              
         L     RE,ATIOB                                                         
         MVI   TIOBAID-TIOBD(RE),0                                              
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,LSTSEL1H                                                      
         CR    R2,R0                                                            
         BNE   DIH10               IF DELETE AN ADD                             
         USING LINSELH,R2                                                       
         TWAXC LINIDTH,LINESTH                                                  
         DROP  R2                                                               
         B     DIHX                                                             
*                                                                               
DIH10    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL2H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         LA    R1,SEQLIST(R1)                                                   
         MVC   LSTHDRSQ,0(R1)                                                   
*                                                                               
         XC    MINEKEY,MINEKEY     ANY INVOICE DETAILS?                         
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   DIH15                                                            
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ   INVOICE DETAIL?                              
         BNE   DIH15               NO                                           
         CLC   LSTHDRSQ,PIMDTLS1   YES, BELONGS TO THIS INVOICE HEADER?         
         BNE   DIH15                                                            
         USING LINSELH,R2                                                       
         MVI   LINSEL,C'D'                                                      
         B     CANTDELE            YES, DELETE ALL DETAILS FIRST                
         DROP  R2                                                               
*                                                                               
DIH15    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIORD                                                       
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    MNIOFLAG,X'80'                                                   
*                                                                               
DIHLOOP  XC    MINEKEY,MINEKEY     DELETE INVOICE DETAILS ASSOCIATED            
         MVI   MINEKEY,PIMDTLEQ        WITH THE INVOICE HEADER                  
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         L     R6,MINELEM                                                       
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE INVOICE DETAILS                      
         BE    DIH30                                                            
         DC    H'0'                                                             
*                                                                               
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ   STILL AN INVOICE DETAIL                      
         BNE   DIH30                                                            
         CLC   LSTHDRSQ,PIMDTLS1   HEADER SEQUENCE NUMBER THE SAME?             
         BNE   DIH30                                                            
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED TO A BUY?                            
         BZ    DIH20               NO                                           
*                                                                               
         XC    KEY,KEY             YES, GET BUY RECORD IT'S MATCHED TO          
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         CLC   QPRD,=C'***'                                                     
         BNE   *+14                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         B     *+10                                                             
         MVC   PBUYKPRD,QPRD                                                    
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
*                                                                               
         TM    GLOBFLG1,X'80'           ALL ZONES AND EDITIONS?                 
         BZ    *+10                                                             
         MVC   PBUYKPUB+4(2),PIMBZONE   YES, ZONE & EDITION IN DETAIL           
*                                                                               
         MVC   PBUYKDAT,PIMBDATE                                                
         MVC   PBUYKEST,PIMBEST                                                 
         MVC   PBUYKLIN,PIMBLINE                                                
         DROP  R3                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         BRAS  RE,TSTLOCK          RECORDS (BUY) LOCKED ?                       
         BNE   RECLOCKD            YES - CAN'T UPDATE                           
*                                                                               
         CLC   KEY(L'PBUYKEY),KEYSAVE   BUY SHOULD EXIST                        
         BNE   DIH20               IT DOESN'T, JUST DELETE INVOICE ITEM         
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING PBUYREC,R3                                                       
*                                                                               
         TM    PBDSTAT,X'40'       IS BUY ACTUALLY MATCHED?                     
         BZ    DIH20               NO, JUST DELETE INVOICE ITEM                 
         NI    PBDSTAT,X'FF'-X'40'  TAKE OFF MATCHED BIT                        
*                                                                               
         GOTO1 PUTREC              WRITE BUY RECORD BACK                        
         DROP  R3                                                               
*                                                                               
DIH20    GOTO1 MINIO,DMCB,('MINDEL',(R5))    DELETE THE INVOICE DETAIL          
         CLI   MINERR,0                                                         
         BE    DIHLOOP                                                          
         DC    H'0'                                                             
*                                                                               
DIH30    XC    MINEKEY,MINEKEY     DELETE COMMENTS ASSOCIATED WITH THE          
         MVI   MINEKEY,PIMCOMEQ        INVOICE HEADER                           
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         L     R6,MINELEM                                                       
*                                                                               
         BAS   RE,MINIOHI                                                       
DIH30LP  BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE INVOICE DETAILS                      
         BE    DIHX                                                             
         DC    H'0'                                                             
*                                                                               
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMCOMEQ   STILL A COMMENT                              
         BNE   DIHX                                                             
         CLC   LSTHDRSQ,PIMDTLS1   HEADER SEQUENCE NUMBER THE SAME?             
         BNE   DIHX                                                             
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))    DELETE THE COMMENT                 
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         B     DIH30LP                                                          
*                                                                               
DIHX     B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* AS OF MARCH 1995 THIS ROUTINE IS NO LONGER CALLED. TO CALL IT WILL            
* CREATE PROBLEMS WITH THE CHANGES MADE TO ALLOW FOR EPIC.                      
* THIS ROUTINE CHECKS TO SEE IF THERE ARE INSERTIONS IN THE PERIOD              
* SPECIFIED BY INVSTDT AND INVENDDT.                                            
***********************************************************************         
ANYBUYS  DS    0H                                                               
         NMOD1 0,**ABUY**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
*                                                                               
         USING LINSELH,R2                                                       
         LA    R2,LINPERH                                                       
         DROP  R2                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
*                                                                               
ANYB10   MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         XC    PBUYKEST(PBUYLEN-PBUYKEST),PBUYKEST                              
*                                                                               
ANYBHIGH GOTO1 HIGH                                                             
*                                                                               
ANYBCHK  DS    0H                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    ANYB20              NO                                           
*                                                                               
         CLC   KEY(PBUYKZON-PBUYKEY),KEYSAVE   CHECK IF SAME UPTO PUB           
         BNE   NOBUYSIN            NO BUYS IN PERIOD                            
*                                                                               
         CLC   PBUYKZON(2),KEYSAVE+PBUYKZON-PBUYKEY  ZON/EDT NOT SAME           
         BNE   ANYB10              THEN CHECK FOR THIS ZON/EDT                  
         B     ANYB30              ELSE CHECK THE DATES                         
*                                                                               
ANYB20   CLC   KEY(PBUYKDAT-PBUYKEY),KEYSAVE  PUBS ARE NOT THE SAME             
         BNE   NOBUYSIN            NO BUYS IN PERIOD                            
*                                                                               
ANYB30   CLC   PBUYKDAT,INVSTDT    DATE ON OR ABOVE START DATE?                 
         BNL   ANYB40              YES                                          
         TM    GLOBFLG1,X'80'      NO, IT'S BELOW                               
         BZ    NOBUYSIN            NO BUYS IN PERIOD IF SINGLE PUB              
         B     ANYB10              SET DATE TO START DATE                       
*                                                                               
ANYB40   CLC   PBUYKDAT,INVENDDT   DATE WITHIN PERIOD?                          
         BNH   ANYB50              YES, GOT AN INSERTION                        
         TM    GLOBFLG1,X'80'      NO, DATE'S ABOVE PERIOD                      
         BZ    NOBUYSIN            NO BUYS IN PERIOD IF SINGLE PUB              
         MVI   PBUYKDAT,X'FF'      FORCE TO NEXT ZONE/EDITION OR PUB            
         B     ANYBHIGH                                                         
*                                                                               
ANYB50   CLI   SVPROF+3,C'Y'       ACROSS ESTIMATES?                            
         BE    ANYB60                                                           
*                                                                               
         OC    BEST,BEST           NO ESTIMATE GIVEN YET                        
         BZ    ANYB60              SO SKIP ESTIMATE CHECK                       
*                                                                               
         CLC   PBUYKEST,BEST       NO, SEE IF IT'S ESTIMATE MATCHES             
         BNE   ANYBSEQ             IT IS                                        
         DROP  R3                                                               
*                                                                               
ANYB60   DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         OC    SPCLREP,SPCLREP                                                  
         BZ    ANYB64              NO SREP ELEMENT                              
*                                                                               
         L     R6,AIO              SREP GIVEN                                   
         MVI   ELCODE,X'80'                                                     
         BAS   RE,GETEL                                                         
         BNE   ANYBSEQ             BUT NO SREP ELEMENT                          
         USING PBSREPEL,R6                                                      
         CLC   PBSREP,SPCLREP      DO THEY MATCH?                               
         BNE   ANYBSEQ                                                          
         B     ANYB70                                                           
*                                                                               
ANYB64   DS    0H                  NO SREP GIVEN                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'80'        SREP ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   ANYB70              NO SREP ELEMENT IS OK                        
         USING PBSREPEL,R6                                                      
         OC    PBSREP,PBSREP       ANYTHING IN ELEMENT                          
         BNZ   ANYBSEQ             IT IS NOT GIVEN BUT IN BUY                   
*                                                                               
ANYB70   L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         OC    PBUYKACT,PBUYKACT   SEE IF IT'S A USABLE INSERTION               
         BNZ   ANYBSEQ             IT IS NOT                                    
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    ANYBSEQ             IT IS                                        
*                                                                               
         B     ANYBX                                                            
*                                                                               
ANYBSEQ  GOTO1 SEQ                                                              
         B     ANYBCHK                                                          
*                                                                               
ANYBX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* THIS ROUTINE SWITCHES TO $MBC                                                 
***********************************************************************         
SWTCHMBC DS    0H                                                               
         NMOD1 0,**SMBC**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         MVI   PFKEY,0             CLEAR THE PFKEY                              
         L     RE,ATIOB                                                         
         MVI   TIOBAID-TIOBD(RE),0                                              
*                                                                               
         LH    R2,CURDISP          FIND CURSOR POSITION                         
         AR    R2,RA                                                            
         LA    R0,LSTSEL2H                                                      
*                                                                               
         CR    R2,R0               CURSOR ABOVE SECOND LIST LINE                
         BNL   MBC10                                                            
         OI    BITFLAG,X'08'       NO HEADER BUT GOING TO MBC                   
         B     MBC30                                                            
*                                                                               
MBC10    LA    R0,LSTPFLNH                                                      
         CR    R2,R0               CURSOR BELOW LAST LIST LINE                  
         BL    MBC20                                                            
         OI    BITFLAG,X'08'       NO HEADER BUT GOING TO MBC                   
         B     MBC30                                                            
*                                                                               
MBC20    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL2H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(L'LSTHDRSQ,R1),0(R1)  NO INVOICE HEADER?                       
         BNZ   *+12                                                             
         OI    BITFLAG,X'08'       NONE, BUT GOING TO MBC                       
         B     MBC30                                                            
*                                                                               
         MVC   LSTHDRSQ,0(R1)                                                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIORD                                                       
*                                                                               
MBC30    L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',LSTMEDH,,GLVPRMD                           
         CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',LSTCLTH,,GLVPRCLT                          
         CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
         CLC   =C'***',LSTPRD                                                   
         BE    *+12                                                             
         CLI   LSTPRDH+5,0                                                      
         BNE   MBC40                                                            
         GOTO1 GLOBBER,DMCB,=C'PUTD',=C'ALL',3,GLVPRPRD   PUT OUT "ALL"         
         B     MBC45                                                            
*                                                                               
MBC40    GOTO1 GLOBBER,DMCB,=C'PUTF',LSTPRDH,,GLVPRPRD                          
MBC45    CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
         CLI   LSTPUBH+5,0                                                      
         BNE   MBC50                                                            
         GOTO1 GLOBBER,DMCB,=C'PUTD',=C'ALL',3,GLVPRPUB   PUT OUT "ALL"         
         B     MBC65                                                            
*                                                                               
MBC50    TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    MBC60                                                            
         MVC   BLOCK(L'LSTPUB),LSTPUB  YES                                      
         ZIC   R1,LSTPUBH+5                                                     
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB+8           STORE LENGTH OF PUB INPUT                    
*                                                                               
         LA    R1,BLOCK                                                         
*                                                                               
         CLI   0(R1),C'.'          CHANGE THE DECIMAL TO A COMMA                
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVI   0(R1),C','                                                       
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,,GLVPRPUB                            
         B     MBC65                                                            
*                                                                               
MBC60    MVC   FAKEFLDH,LSTPUBH    SET UP FAKE FIELD TO SIMULATE                
         MVC   FAKEFLD(L'LSTPUB),LSTPUB   THE PUB FIELD SO THAT WE              
         ZIC   R0,FAKEFLDH+5              CAN CHANGE THE '.' TO ','             
         LA    R1,FAKEFLD                                                       
*                                                                               
MBC60LP  CLI   0(R1),C'.'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,MBC60LP                                                       
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',FAKEFLDH,,GLVPRPUB                         
MBC65    CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
         TM    BITFLAG,X'08'       PRD/PUB NOT ENTERED & GOING TO MBC?          
         BNZ   *+14                YES, USE ALL ESTIMATES                       
*                                                                               
         OC    PIMEST,PIMEST       IF MULTI-ESTIMATE                            
         BNZ   MBC70                                                            
         GOTO1 GLOBBER,DMCB,=C'PUTD',=C'ALL',3,GLVPREST   PUT OUT "ALL"         
         CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
         B     MBC80                                                            
*                                                                               
MBC70    GOTO1 GLOBBER,DMCB,=C'PUTD',PIMEST,3,GLVPREST                          
         CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
MBC80    TM    BITFLAG,X'08'       PRD/PUB NOT ENTERED & GOING TO MBC?          
         BZ    MBC90                                                            
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R3,LSTPER                                                        
         ICM   R3,8,LSTPERH+5                                                   
         GOTO1 PERVAL,DMCB,(R3),(X'20',PERVALST)                                
*                                                                               
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
*                                                                               
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT?                         
         BNE   *+10                                                             
         MVC   PVALEEND,PVALESTA   YES, END DATE IS START DATE                  
*                                                                               
         MVC   BLOCK(L'PVALESTA),PVALESTA                                       
         MVC   BLOCK+L'PVALESTA(L'PVALEEND),PVALEEND                            
         B     MBC95                                                            
         DROP  R3                                                               
*                                                                               
MBC90    GOTO1 DATCON,DMCB,(3,PIMSTDT),(0,BLOCK)                                
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(0,BLOCK+6)                             
*                                                                               
MBC95    GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVPRPER                          
         CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
         MVC   BLOCK(11),=CL11'CD,STATUS=L'   PUT OUT MBC DATA                  
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,11,GLVPRDTA                          
         CLI   DMCB+8,0                                                         
         BNE   MBCNOT                                                           
*                                                                               
         L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(17,R2),=CL17'+MBC,SV'   GO TO  T41800, 01                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
MBCX     MVI   GERROR1,FROMMBC     CAME BACK FROM MBC PROGRAM MESSAGE           
         LA    R2,LSTPRDH              AND POSITION CURSOR AT PRODUCT           
         B     INFEXIT             SO WE WON'T GO TO VALIDATE RECORD            
*                                                                               
MBCNOT   MVI   GERROR1,MBCSWTCH    CAN'T SWITCH TO THE MBC PROGRAM              
         LA    R2,LSTSEL1H                                                      
         B     ERREXIT                                                          
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RESEQUENCES THE INVOICE HEADERS.                                 
***********************************************************************         
RSEQHDRS DS    0H                                                               
         NMOD1 0,**RSEQ**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
*                                                                               
* PHASE 1 - RENUMBER THE SEQUENCE NUMBERS IN SEQUENTIAL ORDER FROM 1            
*                                                                               
         SR    R0,R0               R0 = SEQUENCE NUMBER BEFORE CHANGE           
         SR    R2,R2               R2 = SEQUENCE NUMBER AFTER CHANGE            
         LA    R3,SEQLIST          R3 = A(FIRST ENTRY IN SEQUENCE LIST)         
         L     R6,MINELEM                                                       
*                                                                               
         USING PIMHDREL,R6                                                      
RSQ10    BAS   RE,MINIOHI          ANY INVOICE HEADERS?                         
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE HEADERS?                             
         BE    RSQ50               YES, GO TO SECOND PHASE                      
         DC    H'0'                                                             
*                                                                               
         CLI   PIMHDREL,PIMHDREQ   NO MORE HEADERS?                             
         BNE   RSQ50               YES, GO TO SECOND PHASE                      
*                                                                               
         IC    R0,PIMHDRSQ         R0 = SEQUENCE NUMBER BEFORE CHANGE           
         LA    R2,1(R2)            R2 = NEXT SEQUENCE NUMBER TO BE USED         
*                                                                               
RSQ13    CLI   0(R3),0             END OF SEQ LIST?                             
         BE    RSQ16               YES                                          
*                                                                               
         CLC   PIMHDRSQ,0(R3)      CHANGE SEQUENCE # IN LIST?                   
         BL    RSQ16                                                            
         BH    *+16                                                             
         STC   R2,0(R3)            YES, SEQUENCE # IN LIST GETS NEW #           
         LA    R3,1(R3)            R3 = A(NEXT ENTRY IN LIST)                   
         B     RSQ16                                                            
         LA    R3,1(R3)                                                         
         B     RSQ13                                                            
*                                                                               
RSQ16    CLM   R2,1,PIMHDRSQ       NO CHANGE NEEDED FOR THIS HEADER?            
         BE    RSQ40               NO, SAME NUMBER                              
*                                                                               
         STC   R2,PIMHDRSQ         INVOICE HEADER GETS NEW SEQUENCE #           
         BAS   RE,MINIOWRT         WRITE OUT INVOICE HEADER                     
*                                                                               
* PHASE 1A - MAKE SURE THE SEQUENCE NUMBERS FOR THE DETAILS MATCH THOSE         
*            FOR THE HEADERS                                                    
*                                                                               
RSQ20    XC    MINEKEY,MINEKEY     GET ALL INVOICE DETAILS ASSOCIATED           
         MVI   MINEKEY,PIMDTLEQ        WITH THE INVOICE HEADER                  
         STC   R0,MINEKEY+1                                                     
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE INVOICE DETAILS                      
         BE    RSQ30                                                            
         DC    H'0'                                                             
*                                                                               
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ   STILL AN INVOICE DETAIL                      
         BNE   RSQ30                                                            
         CLM   R0,1,PIMDTLS1       HEADER SEQUENCE NUMBER THE SAME?             
         BNE   RSQ30                                                            
*                                                                               
         STC   R2,PIMDTLS1         CHANGE THE HEADER SEQUENCE                   
*                                                                               
         BAS   RE,MINIOWRT         WRITE OUT INVOICE DETAIL                     
         B     RSQ20                                                            
*                                                                               
*                                                                               
* PHASE 1B - MAKE SURE THE SEQUENCE NUMBERS FOR THE COMMENTS MATCH              
*            THOSE FOR THE HEADERS                                              
*                                                                               
RSQ30    XC    MINEKEY,MINEKEY     GET ALL INVOICE DETAILS ASSOCIATED           
         MVI   MINEKEY,PIMCOMEQ        WITH THE INVOICE HEADER                  
         STC   R0,MINEKEY+1                                                     
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE INVOICE DETAILS                      
         BE    RSQ40                                                            
         DC    H'0'                                                             
*                                                                               
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ   STILL AN INVOICE DETAIL                      
         BNE   RSQ40                                                            
         CLM   R0,1,PIMCOMS1       HEADER SEQUENCE NUMBER THE SAME?             
         BNE   RSQ40                                                            
*                                                                               
         STC   R2,PIMCOMS1         CHANGE THE HEADER SEQUENCE                   
*                                                                               
         BAS   RE,MINIOWRT         WRITE OUT INVOICE DETAIL                     
         B     RSQ30                                                            
*                                                                               
RSQ40    XC    MINEKEY,MINEKEY     GET NEXT HEADER SEQUENCE                     
         MVI   MINEKEY,PIMHDREQ                                                 
         AH    R0,=H'1'                                                         
         STC   R0,MINEKEY+1                                                     
         CH    R0,=H'256'          PASSED HDR SEQ OF X'FF'?                     
         BL    RSQ10               NO, CONTINUE LOOKING FOR HEADERS             
         EJECT                                                                  
*                                                                               
* PHASE 2 - RENUMBER THE SEQUENCE NUMBERS IN THIS ORDER:                        
*              LAST INVOICE DETAIL GETS A SEQUENCE OF X'FC'                     
*              INVOICE DETAILS AFTER WILL GET A SEQUENCE OF X'FC'-3N            
*                                                                               
RSQ50    LTR   R2,R2               IF NO DETAILS                                
         BZ    RSQX                THEN WE CAN LEAVE                            
*                                                                               
         LA    R0,X'FD'            R0 = SEQUENCE NUMBER AFTER CHANGE            
         BCTR  R3,0                                                             
*                                                                               
RSQ60    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         STC   R2,MINEKEY+1        R2 = LAST SEQUENCE NUMBER USED               
*                                                                               
         BAS   RE,MINIOHI          ANY INVOICE HEADERS?                         
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE HEADERS                              
         BE    RSQX                                                             
         DC    H'0'                                                             
*                                                                               
         USING PIMHDREL,R6                                                      
RSQ62    CLI   0(R3),0             END OF SEQ LIST?                             
         BE    RSQ66               YES                                          
*                                                                               
         CLM   R2,1,0(R3)          CHANGE SEQUENCE # IN LIST?                   
         BH    RSQ66                                                            
         BL    RSQ64                                                            
         STC   R0,0(R3)            YES, SEQUENCE # IN LIST GETS NEW #           
         LA    RF,SEQLIST                                                       
         CR    R3,RF                                                            
         BE    *+6                                                              
         BCTR  R3,0                R3 = A(PREV ENTRY IN LIST)                   
         B     RSQ66                                                            
RSQ64    LA    RF,SEQLIST                                                       
         CR    R3,RF                                                            
         BE    RSQ66                                                            
         BCTR  R3,0                                                             
         B     RSQ62                                                            
*                                                                               
RSQ66    STC   R0,PIMHDRSQ         NEW SEQUENCE NUMBER FOR HEADER               
*                                                                               
         BAS   RE,MINIOWRT                                                      
*                                                                               
* PHASE 2A - MAKE SURE THE SEQUENCE NUMBERS FOR THE DETAILS MATCH THOSE         
*            FOR THE HEADERS                                                    
*                                                                               
RSQ70    XC    MINEKEY,MINEKEY     GET ALL INVOICE DETAILS ASSOCIATED           
         MVI   MINEKEY,PIMDTLEQ        WITH THE INVOICE HEADER                  
         STC   R2,MINEKEY+1                                                     
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE INVOICE DETAILS                      
         BE    RSQ80                                                            
         DC    H'0'                                                             
*                                                                               
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ   STILL AN INVOICE DETAIL                      
         BNE   RSQ80                                                            
         CLM   R2,1,PIMDTLS1       HEADER SEQUENCE NUMBER THE SAME?             
         BNE   RSQ80                                                            
*                                                                               
         STC   R0,PIMDTLS1         CHANGE THE HEADER SEQUENCE                   
*                                                                               
         BAS   RE,MINIOWRT         WRITE OUT INVOICE DETAIL                     
         B     RSQ70                                                            
*                                                                               
* PHASE 2B - MAKE SURE THE SEQUENCE NUMBERS FOR THE COMMENTS MATCH              
*            THOSE FOR THE HEADERS                                              
*                                                                               
RSQ80    XC    MINEKEY,MINEKEY     GET ALL INVOICE DETAILS ASSOCIATED           
         MVI   MINEKEY,PIMCOMEQ        WITH THE INVOICE HEADER                  
         STC   R2,MINEKEY+1                                                     
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+14                                                             
         CLI   MINERR,MINEEOF      NO MORE INVOICE DETAILS                      
         BE    RSQ90                                                            
         DC    H'0'                                                             
*                                                                               
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ   STILL AN INVOICE DETAIL                      
         BNE   RSQ90                                                            
         CLM   R2,1,PIMCOMS1       HEADER SEQUENCE NUMBER THE SAME?             
         BNE   RSQ90                                                            
*                                                                               
         STC   R0,PIMCOMS1         CHANGE THE HEADER SEQUENCE                   
*                                                                               
         BAS   RE,MINIOWRT         WRITE OUT INVOICE DETAIL                     
         B     RSQ80                                                            
*                                                                               
RSQ90    SH    R0,=H'2'            KEEP INVOICE HEADERS ONE APART               
         BNM   *+6                                                              
         DC    H'0'                DIE IF NO MORE ROOM                          
         BCT   R2,RSQ60                                                         
*                                                                               
RSQX     MVC   MELEM,MELEM2                                                     
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         DROP  RE                                                               
*                                                                               
TSTLOK20 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK20                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         BNE   TSTLOK28            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK24 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK24                                                         
*                                  CHECK CLIENT/PUB LOCK                        
TSTLOK28 LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT/PUB LOCK                              
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         MVC   L.LOCKPUB,PBUYKPUB                                               
         XC    L.LOCKPUB,=4X'FF'   TO ELIMINATE X'00' FIELDS                    
         DROP  RE                                                               
*                                                                               
TSTLOK30 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK30                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         BNE   TSTLOK40            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK34 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK34                                                         
*                                                                               
TSTLOK40 DS    0H                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               REC LOCKED - SEND MESSAGE                    
         XIT1                                                                   
*                                                                               
LKUPKEY  DS    XL16                DATA LOCKING KEY                             
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                 BASE PUB ONLY                                
         DS    XL2                                                              
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFED          (OUR LIST SCREEN)                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
DSPARMD  DSECT                                                                  
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
GLOBBER  DS    A                   A(GLOBBER)                                   
MYQMED   DS    CL1                 SAVED MEDIA                                  
MYQCLT   DS    CL3                 SAVED CLIENT                                 
MYQPRD   DS    CL3                 SAVED PRODUCT                                
MYBPUB   DS    XL6                 SAVED PUB                                    
MYPAYREP DS    CL4                 SAVED PAYING REP                             
MYQYEAR  DS    CL1                 SAVED YEAR                                   
MYPROF   DS    CL16                SAVED PROFILE                                
*                                                                               
ADATEFLD DS    CL6                 A DATE FIELD                                 
PERDFILT DS    CL6                 PERIOD FILTER                                
ESTFILT  DS    CL3                 ESTIMATE FILTER                              
SREPFILT DS    CL4                 SREP FILTER                                  
NEXTSCRN DS    CL1                 NEXT SCREEN AFTER ADD (C,U,S,L)              
IHDREST  DS    CL3                 INVOICE HEADER ESTIMATE                      
IHDRPER  DS    CL17                INVOICE HEADER PERIOD                        
IHDRNUMB DS    CL11                INVOICE HEADER NUMBER                        
IHDRSREP DS    CL4                 INVOICE HEADER SPECIAL REP                   
ADDEDFLG DS    XL1                 ADDED LINE FLAG (Y/N)                        
BITFLAG  DS    X                   X'80' - PERIOD BEING FILTERED                
*                                  X'40' - ESTIMATE BEING FILTERED              
*                                  X'20' - SREP BEING FILTERED                  
*                                  X'10' - OVERRIDE SCREEN AFTER ADD            
*                                  X'08' - PRD/PUB NOT ENTERED AND MBC          
*                                  X'04' - NO MORE ROOM IN LIST SCREEN          
*                                  X'01' - CAME BACK FROM PAY                   
KEYCHNGF DS    CL1                 KEY FIELDS WERE CHANGED (Y/N)                
OLDHDRSQ DS    XL1                 OLD HEADER SEQUENCE                          
NEWHDRSQ DS    XL1                 NEW HEADER SEQUENCE                          
TOTDTLAM DS    PL7                 TOTAL DETAIL AMOUNT                          
FAKEFLDH DS    XL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    CL80                FAKE FIELD                                   
PERVALST DS    XL56                PERVAL STORAGE                               
SEQLIST  DS    XL(9*L'PIMHDRSQ)    SEQ #'S SHOWN IN LIST                        
         DS    XL(L'PIMHDRSQ)                                                   
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
*                                                                               
FRSHDRSQ DS    XL(L'PIMHDRSQ)      FIRST MINELEM KEY ON LIST                    
         EJECT                                                                  
*                                                                               
* LIST LINE DSECT                                                               
*                                                                               
LINDSECT DSECT                                                                  
LINSELH  DS    CL8                                                              
LINSEL   DS    CL3                                                              
LINIDTH  DS    CL8                                                              
LINIDT   DS    CL8                                                              
LININVH  DS    CL8                                                              
LININV   DS    CL11                                                             
LINPERH  DS    CL8                                                              
LINPER   DS    CL17                                                             
LINESTH  DS    CL8                                                              
LINEST   DS    CL3                                                              
LINGNH   DS    CL8                                                              
LINGN    DS    CL1                                                              
LINAMTH  DS    CL8                                                              
LINAMT   DS    CL11                                                             
LINCDH   DS    CL8                                                              
LINCD    DS    CL1                                                              
LINSREPH DS    CL8                                                              
LINSREP  DS    CL4                                                              
LINNEXTL DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085PPMAT01   07/22/02'                                      
         END                                                                    
