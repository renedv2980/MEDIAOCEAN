*          DATA SET ANXXXXX    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T11A00A                                                                  
*INCLUDE GETIDS                                                                 
***********************************************************************         
*                                                                     *         
* REGISTER USAGE:                                                     *         
*      R1:  WORK                                                      *         
*      R2:  WORK AND USED FOR ADDRESS WHEN BRANCHING TO SUBROUTINE    *         
*      R3:  POINTER TO SCREEN DSECT                                   *         
*      R4:  WORK                                                      *         
*      R5:  WORK                                                      *         
*      R6:  POINTER TO A LINE ON THE SCREEN                           *         
*      R7:  POINTER TO WHICH RECORD IN EDICTBLK                       *         
*      R8:  POINTER TO TIA                                            *         
*      R9:  POINTER TO SRPARMD/THEN TO SYSFACD                        *         
*      RA:  SECOND BASE REGISTER                                      *         
*      RB:  BASE                                                      *         
*      RC:  POINTER TO WORKING STORAGE                                *         
***********************************************************************         
*      NOTE:  TO ADD ANOTHER METHOD OF TRANSMISSION (SEE FAXGATE),    *         
*             ADD THE APPROPRIATE ENTRIES TO METHTAB, TOTTAB, PRNTOT, *         
*             AND ADD THE 6 TOTALS BUCKETS                            *         
***********************************************************************         
         PRINT NOGEN                                                            
T11A00   CSECT                                                                  
         NMOD1 WRKX-WRKD,*$ETI**,RA,CLEAR=YES,RR=R4                             
         USING WRKD,RC                                                          
         ST    R4,RELO                                                          
         ST    RD,BASERD                                                        
         TITLE 'EDICT RECORD DISPLAY - TRANSACTION REPORT'                      
         EJECT                                                                  
***********************************************************************         
*        SETUP AND INITIALIZATION                                               
***********************************************************************         
*                                                                               
         USING SRPARMD,R9          R9=A(S/R PARAM LIST)                         
         LR    R9,R1                                                            
         USING TIOBD,R5                                                         
         L     R5,SRPARM8          TRANSLATOR I/O BLOCK                         
         MVC   PFKEY,TIOBAID       PFKEY NUMBER OR ZERO (ENTER)                 
         DROP  R5                                                               
*                                                                               
         USING SRSD,R8             R8=A(TIA - USED FOR SR SAVE PAGE)            
         L     R8,SRPARM2                                                       
         USING SRETIFFD,R3         R3=A(TWA)                                    
         L     R3,SRPARM6                                                       
*                                                                               
         L     R4,SRPARM4          COMFACS                                      
         ST    R4,VCOMFACS                                                      
         XC    EDSMSG,EDSMSG       CLEAR MESSAGE LINE                           
*                                                                               
         L     R4,SRPARM3                                                       
         USING UTLD,R4                                                          
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         MVC   TRNNUM,TTRCNT       TRANSACTION NUMBER                           
*                                                                               
         MVI   DDSVERS,C'N'                                                     
         MVI   DDSTERM,C'N'                                                     
         TM    TSTAT1,TSTATDDS     DDS TERMINAL?                                
         BZ    SET10                                                            
         MVI   DDSTERM,C'Y'                                                     
         MVI   DDSVERS,C'Y'        GETS DDS VERSION (ETD)                       
         DROP  R4                                                               
*                                                                               
SET10    CLC   =C'ETI,C',EDSID+1   WANT USER VERSION                            
         BNE   SET30                                                            
         MVI   DDSVERS,C'N'                                                     
*                                                                               
SET30    L     R5,SRPARM1                                                       
         L     R5,VSSB-SYSFACD(R5)                                              
         MVC   RECLEN,SSBTWAL-SSBD(R5)                                          
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
*                                                                               
         MVC   HELPKEY,HELPID      INITIALIZE HELP KEY                          
*                                                                               
         OI    EDSMENUH+6,X'80'                                                 
         XC    EDSMENU,EDSMENU                                                  
         MVC   EDSMENU(34),=C'PF5=UP PF6=DOWN PF7=TOP PF8=BOTTOM'               
         CLI   DDSVERS,C'Y'        ETD?                                         
         BNE   *+10                                                             
         MVC   EDSMENU+35(26),=C'PF11=SUMMARY ENTER=REFRESH'                    
*                                                                               
         CLI   PFKEY,12            HIGHEST PF KEY FOR NOW                       
         BNH   SET40                                                            
         ZIC   R1,PFKEY                                                         
         SH    R1,=H'12'           EQUATE HIGH PF KEYS TO LOWER VALUES          
         STC   R1,PFKEY                                                         
*                                                                               
SET40    CLI   PFKEY,8                                                          
         BNE   *+8                                                              
         OI    FLAG,BOTTOM         USER HIT PF8 FOR BOTTOM                      
*                                                                               
         BAS   RE,READSTR          READ SAVED STORAGE                           
         CLC   ISITME,=C'$EDI'     MAKE SURE STILL ETI                          
         BNE   SET70                                                            
*                                                                               
         SR    R1,R1                                                            
         LH    R1,LASTTRN          CHECK LAST TRANSACTION NUMBER                
         LA    R1,1(R1)                                                         
         CH    R1,TRNNUM                                                        
         BNE   SET70               START OVER IF NOT SEQUENTIAL                 
*                                                                               
         CLI   PFKEY,8             BOTTOM                                       
         BE    SET50                                                            
         CLI   PFKEY,7             TOP                                          
         BE    SET70                                                            
         CLI   PFKEY,6             DOWN                                         
         BE    SET50                                                            
         CLI   PFKEY,5             UP                                           
         BE    SET50                                                            
         CLI   PFKEY,0             REFRESH FOR DDS VERSION                      
         BNE   SET70                                                            
*                                                                               
         CLI   DDSVERS,C'Y'        IS IT ETD?                                   
         BNE   SET50               NO                                           
         MVC   DSKAD,FRSTTBR       REDISPLAY SCREEN ON ENTER                    
         B     SET60                                                            
*                                                                               
SET50    MVC   DSKAD,LASTTBR       PASS ALONG OLD LAST TTBR                     
SET60    MVC   FDSKAD,FRSTTBR      TTBR FOR PAGE UP                             
         XC    DAFILT,DAFILT                                                    
         B     SET80                                                            
*                                                                               
SET70    BAS   RE,PARMCLR          CLEAR AND START FROM TOP                     
         XC    OLDFILT,OLDFILT                                                  
         XC    SYSFILT,SYSFILT                                                  
         XC    RECSKIP,RECSKIP                                                  
*                                                                               
SET80    GOTO1 =A(PARAMS),DMCB,(RC),RR=RELO    VALIDATE PARAMS                  
         BAS   RE,MAIN                    DISPLAY RECS                          
         B     MSG0                                                             
         EJECT                                                                  
***********************************************************************         
*        READ IN SAVED STORAGE                                                  
***********************************************************************         
READSTR  NTR1                                                                   
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TERMNUM                                                     
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),SRSD               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEDSTR(SAVEDL),SR$EDICT                                        
READX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        MAIN PROCESSING                                                        
***********************************************************************         
*                                                                               
MAIN     NTR1                                                                   
         L     RF,VCOMFACS         GET SYSTEM ID AND TODAY'S DATE               
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R4,0(R1)                                                         
         USING FACTSD,R4                                                        
         MVC   SYSID,FASYSID       SYSTEM ID                                    
         MVC   DAY,FADATEB         BINARY (YMD)                                 
         TM    FLAGS,FLSENT        SENT= FILTER?                                
         BZ    MAIN10              NO                                           
         MVI   SYSID,2             TO FORCE DEFAULT TO READ ADV FILE            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        COLUMN HEADINGS                                                        
***********************************************************************         
*                                                                               
MAIN10   LA    R6,EDSLN1H          R6-POINTER FOR SCREEN LINE HEADER            
         OI    6(R6),X'80'         TRANSMIT COLUMN HEADINGS                     
         TM    FLAGS,FLSUMM        SUMMARY OPTION                               
         BO    MAIN30              THEN SKIP ALL THIS                           
*                                                                               
         LA    R1,COLSDDS          ETD COLS                                     
         CLI   DDSVERS,C'Y'        ETD?                                         
         BE    MAIN20                                                           
         LA    R1,COLSADV          ADV COLS (ALL BUT SYS=REP OR DARE)           
*                                                                               
         ZIC   RF,SYSID            FACPAK NUMBER                                
         MH    RF,=Y(L'FACIDTAB)                                                
         LA    RF,FACIDTAB(RF)     RF = A(ENTRY IN FACIDTAB)                    
         TM    5(RF),X'20'         IS THIS A REP SYSTEM?                        
         BZ    MAIN15              NO                                           
*                                                                               
         CLI   SYSFILT,EDFSREPQ    REP ON REP                                   
         BNE   *+8                                                              
         LA    R1,COLSREP          REP COLS (NON DARE)                          
         CLI   SYSFILT,EDFDARRQ    DARE                                         
         BNE   *+8                                                              
         LA    R1,COLSDARR         DARE FOR REP                                 
         B     MAIN20                                                           
*                                                                               
MAIN15   CLI   SYSFILT,EDFDARRQ    DARE                                         
         BNE   *+8                                                              
         LA    R1,COLSDARA         DARE FOR ADV                                 
*                                                                               
MAIN20   MVC   EDSLN1,0(R1)        MOVE IN COLUMN HEADINGS                      
         LA    R6,NXTHEAD+8(R6)    BUMP TO NEXT SCREEN LINE HEADER              
         EJECT                                                                  
***********************************************************************         
*        GET ADDRESS OF THE EDICT FILE                                          
***********************************************************************         
*                                                                               
MAIN30   ZIC   RF,SYSID            FACPAK NUMBER                                
         MH    RF,=Y(L'FACIDTAB)                                                
         LA    RF,FACIDTAB(RF)     RF = A(ENTRY IN FACIDTAB)                    
         TM    5(RF),X'20'         IS THIS A REP SYSTEM?                        
         BO    MAIN50              YES                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTA'                               
         MVC   EDICTFL,12(R1)      A(EDICT DCB)                                 
         MVI   EDICTFL,X'00'       CLEAR HIGH ORDER BYTE                        
         B     MAIN70                                                           
*                                                                               
MAIN50   GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTR'                               
         MVC   EDICTFL,12(R1)      A(EDICT DCB)                                 
         MVI   EDICTFL,X'00'       CLEAR HIGH ORDER BYTE                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        READ CONTROL RECORD                                                    
***********************************************************************         
*                                                                               
MAIN70   GOTO1 VDATAMGR,DMCB,=C'DADDS',RDID,EDICTBLK,0,                +        
               EDICTFL,=X'00010100',0                                           
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         USING EDFILD,R7                                                        
         LA    R7,EDICTBLK                                                      
         CLI   EDFMON,EDFMONPQ                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EDCTFTPD,EDFTKPDY         TRACKS/DAY                             
         XC    EDCTFRPT,EDCTFRPT                                                
         MVC   EDCTFRPT+1(1),EDFBKPTK    PHYSICAL RECS (BLKS)/TRACK             
         XC    EDCTRPB,EDCTRPB                                                  
         MVC   EDCTRPB+1(1),EDFRCPBK     LOGICAL RECORDS/BLOCK                  
         MVC   EDCTFRCL,EDFLRECL         LOGICAL RECORD LENGTH                  
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        CALCULATE DISK ADDRESS AND GO TO DADDS                                 
***********************************************************************         
*                                                                               
         OC    DAYFILT,DAYFILT     IS THERE A DAY FILTER?                       
         BZ    *+10                NO                                           
         MVC   DAY,DAYFILT         DAY FILTER                                   
*                                                                               
         MVC   BMONTH,DAY+1        SAVE MONTH                                   
         ZIC   R2,DAY+2            DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR TODAY         
         STH   R2,FIRSTRAC                                                      
         LH    R1,EDCTFTPD                                                      
         BCTR  R1,0                                                             
         AR    R1,R2               LAST TRACK NUMBER FOR TODAY                  
         STH   R1,EDCTFLST                                                      
*                                                                               
         TM    FLAGS,FLSUMM        SUMMARY OPTION                               
         BNO   MAIN90                                                           
         GOTO1 =A(SUMMARY),DMCB,(RC),RR=RELO                                    
         B     EXIT                                                             
*                                                                               
MAIN90   STH   R2,EDCTFDSK         TRACK NUMBER                                 
         STH   R2,LASTRAC                                                       
*                                                                               
         LA    R2,1                                                             
MAIN100  STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         STC   R2,LASTBLK                                                       
         MVI   EDCTFDSK+3,X'00'                                                 
         CLI   SKIPTB,C'Y'         SKIP TRKBLK - ALREADY GOT TRACK              
         BE    MAIN110                                                          
*                                                                               
         BAS   RE,TRKBLK           GETS RIGHT TRACK AND BLOCK                   
MAIN110  BAS   RE,READBLK          DADDS CALL                                   
*                                                                               
         USING EDFILD,R7                                                        
         LA    R7,EDICTBLK         R7-POINTER IN BLOCK                          
         ST    R7,AEDCTBLK                                                      
*                                                                               
         OC    RECSKIP,RECSKIP     TAB INTO BLOCK?                              
         BZ    MAIN120                                                          
         BAS   RE,RECBUMP          BUMPS TO RIGHT RECORD                        
         B     MAIN130                                                          
*                                                                               
MAIN120  LA    R1,1                                                             
         STC   R1,RECNUM                                                        
         MVI   FIRST,C'Y'                                                       
         MVC   FRSTTBR,EDCTFDSK                                                 
MAIN130  CLI   EDFMON,EDFMONPQ     CONTROL REC                                  
         BE    MAIN250                                                          
         CLC   EDFMON,BMONTH       IS THIS RECORD FROM THIS MONTH?              
         BE    MAIN150             YES -- CONTINUE...                           
         CLI   PFKEY,8             DID USER WANT BOTTOM                         
         BE    MAIN170             YES THEN PAGE UP FROM END                    
         BNE   MSG3                NO -- WE'VE FOUND THE EOF                    
         EJECT                                                                  
***********************************************************************         
*        PAGE UP                                                                
***********************************************************************         
*                                                                               
MAIN150  CLI   PFKEY,5             SCROLLING UP?                                
         BNE   MAIN190                                                          
MAIN160  GOTO1 =A(FILTCK),DMCB,(RC),RR=RELO     FILTER CHECK                    
         BNE   MAIN170             NO , BACK UP ANOTHER REC                     
         CLI   BACKCNT,19          START DISPLAYING?                            
         BL    MAIN170             NO,BACK UP ANOTHER                           
         MVI   PFKEY,6             MAKE SURE DISPLAY DOWN NOW                   
         B     MAIN190             START DISPLAYING                             
*                                                                               
MAIN170  SH    R7,EDCTFRCL         BACKS UP 1 RECORD                            
         C     R7,AEDCTBLK                                                      
         BL    MAIN180             MUST BACK UP BLOCK                           
         ZIC   R1,RECNUM                                                        
         SH    R1,=H'1'                                                         
         STC   R1,RECNUM                                                        
         B     MAIN160                                                          
*                                                                               
MAIN180  BAS   RE,BACKBLK          BACK UP 1 BLOCK                              
         BAS   RE,READBLK          DADDS CALL                                   
         LA    R7,EDICTBLK         R7-POINTER IN BLOCK                          
         ST    R7,AEDCTBLK                                                      
         BAS   RE,RECBUMP          BUMP TO RIGHT RECORD                         
         B     MAIN160                                                          
         EJECT                                                                  
***********************************************************************         
*        PAGE DOWN                                                              
***********************************************************************         
*                                                                               
MAIN190  CLI   PFKEY,8             IS IT BOTTOM?                                
         BE    MAIN250             YES -- SKIP TO BOTTOM OF FILE                
         GOTO1 =A(FILTCK),DMCB,(RC),RR=RELO     FILTER CHECK                    
         BNE   MAIN250             NO, GET NEXT REC                             
*                                                                               
MAIN210  OI    6(R6),X'80'         TRANSMIT                                     
         LA    R6,8(R6)            BUMP PAST HEADER                             
         CLI   DDSVERS,C'Y'        ETD?                                         
         BE    MAIN220                                                          
         BAS   RE,USERDISP         USER DISPLAY = ETI                           
         B     MAIN230                                                          
MAIN220  BAS   RE,DDSDISP          DDS DISPLAY = ETD                            
*                                                                               
MAIN230  TM    FLAG,FLFIRST        IF FIRST TIME THROUGH                        
         BNZ   MAIN240             SAVE TOP OF PAGE DISK ADDRESS                
         MVC   FRSTTBR,EDCTFDSK                                                 
         OI    FLAG,FLFIRST                                                     
*                                                                               
MAIN240  LA    R6,NXTHEAD(R6)      BUMP TO NEXT SCREEN LINE HEADER              
         LA    R2,EDSLLST                                                       
         CR    R6,R2               END OF SREEN?                                
         BNL   MAINX                                                            
         EJECT                                                                  
***********************************************************************         
*        GET NEXT RECORD                                                        
***********************************************************************         
*                                                                               
MAIN250  AH    R7,EDCTFRCL         BUMP TO NEXT RECORD                          
         ZIC   R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
         CH    R1,EDCTRPB          ANY MORE RECORDS IN THIS BLOCK?              
         BNH   MAIN130                                                          
*                                                                               
         ZIC   R2,EDCTFDSK+2                                                    
         LA    R2,1(R2)            NO                                           
         XC    RECSKIP,RECSKIP                                                  
         MVI   SKIPTB,C'Y'                                                      
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   MAIN100             YES                                          
*                                                                               
         LH    R2,EDCTFDSK                                                      
         LA    R2,1(R2)                                                         
         CH    R2,EDCTFLST                                                      
         BNH   MAIN90                                                           
*                                                                               
         CLI   PFKEY,8             IS IT BOTTOM?                                
         BE    MAIN170             YES, GO BACK AND PAGE UP FROM BOT            
         B     MSG2A                                                            
*                                                                               
MAINX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        BUMP RECSKIP NUMBER OF RECORDS INTO BLOCK                              
***********************************************************************         
*                                                                               
RECBUMP  NTR1                                                                   
         ZIC   R1,RECSKIP                                                       
         SH    R1,=H'1'                                                         
         STC   R1,RECSKIP                                                       
*                                                                               
         LA    R1,1(R1)            ADD BACK THAT 1                              
         CH    R1,EDCTRPB          IF LAST REC WAS LAST IN BLOCK                
         BL    RB1                 NO                                           
         XC    RECSKIP,RECSKIP     YES, THEN JUST READ NEXT BLOCK               
         SR    R1,R1                                                            
         ZIC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         STC   R2,LASTBLK                                                       
         MVC   LASTRAC,EDCTFDSK    TRACK NUMBER                                 
         OI    FLAG,SKIPBUMP       AND DON'T SKIP INTO IT                       
         B     RB50                                                             
*                                                                               
RB1      SR    R1,R1                                                            
         B     RB40                                                             
RB10     STH   R2,EDCTFDSK         TRACK NUMBER                                 
         STH   R2,LASTRAC          SAVE TRACK NUMBER                            
*                                                                               
         LA    R2,1                                                             
RB20     STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         STC   R2,LASTBLK                                                       
         BAS   RE,READBLK                                                       
         LA    R7,EDICTBLK         R7-POINTER IN BLOCK                          
         USING EDFILD,R7                                                        
*                                                                               
RB40     DS    0H                                                               
         TM    FLAG,SKIPBUMP       SKIP ANY MORE BUMPING?                       
         BO    RBYES                                                            
*                                                                               
RB45     ZIC   R2,RECSKIP                                                       
         CR    R1,R2               HAVE WE SKIPPED ENOUGH YET?                  
         BH    RBYES               YES, WANT TO SKIP TO NEXT REC                
*                                                                               
         CLI   PFKEY,0             FOR REFRESH FOR DDS VERSION                  
         BNE   RB48                                                             
         CLI   DDSVERS,C'Y'                                                     
         BNE   RB48                                                             
         CR    R1,R2               WANT TO REDISPLAY SAME REC                   
         BE    RBYES               YES                                          
*                                                                               
RB48     CH    R1,EDCTRPB          ANY MORE RECORDS IN THIS BLOCK?              
         BH    RB50                                                             
*                                                                               
         AH    R7,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R1,1(R1)                                                         
         B     RB45                                                             
*                                                                               
RB50     ZIC   R2,LASTBLK                                                       
         LA    R2,1(R2)            NO                                           
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   RB20                YES                                          
*                                                                               
         LH    R2,LASTRAC          CURRENT TRACK NUMBER                         
         LA    R2,1(R2)            BUMP TO NEXT TRACK NUMBER                    
         CH    R2,EDCTFLST         IS IT LAST TRACK?                            
         BNH   RB10                NO                                           
         DC    H'0'                ALL TRACKS FOR TODAY ARE USED                
*                                                                               
RBYES    DS    0H                                                               
         LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
         XIT1  REGS=(R7)                                                        
         EJECT                                                                  
***********************************************************************         
*        GET RIGHT TRACK AND BLOCK                                              
***********************************************************************         
*                                                                               
TRKBLK   NTR1                                                                   
         CLI   PFKEY,5             SCROLLING UP                                 
         BNE   TB100                                                            
*                                                                               
         MVC   DUMMY(3),EDCTFDSK                                                
         MVI   DUMMY+3,X'01'                                                    
         CLC   DUMMY,FDSKAD        IS NEW AND OLD DA THE SAME                   
         BE    MSG2                THEN YOU CAN'T SCROLL UP                     
*                                                                               
         OC    FDSKAD,FDSKAD       USE FDSKAD TO SCROLL UP                      
         BZ    MSG2                NO MORE PREVIOUS RECORD                      
*                                                                               
         XC    EDCTFDSK,EDCTFDSK                                                
         MVC   EDCTFDSK(3),FDSKAD   MOVE IN TRACK AND BLOCK NUMS                
         MVC   RECSKIP(1),FDSKAD+3   LAST REC NUM                               
         ZIC   R1,RECSKIP                                                       
         SH    R1,=H'1'            BACK UP ONE BY ONE TO CHECK                  
         STC   R1,RECSKIP                                                       
         CH    R1,=H'0'                                                         
         BNL   TB50                                                             
*                                                                               
         BAS   RE,BACKBLK                                                       
         B     EXIT                                                             
*                                                                               
TB50     STC   R1,RECSKIP                                                       
         B     EXIT                                                             
*                                                                               
TB100    OC    DAFILT,DAFILT                                                    
         BZ    TB150                                                            
         MVC   DSKAD,DAFILT                                                     
         XC    DAFILT,DAFILT                                                    
*                                                                               
TB150    OC    DSKAD,DSKAD                                                      
         BZ    EXIT                                                             
         XC    EDCTFDSK,EDCTFDSK                                                
         MVC   EDCTFDSK(3),DSKAD   MOVE IN TRACK AND BLOCK NUMS                 
         MVC   RECSKIP(1),DSKAD+3   LAST REC NUM                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        BACK UP BY ONE BLOCK                                                   
***********************************************************************         
*                                                                               
BACKBLK  NTR1                                                                   
         ZIC   R1,EDCTFDSK+2        NEED TO DECREMENT BLOCK NUM                 
         CH    R1,=H'1'                                                         
         BE    TB30                                                             
         SH    R1,=H'1'                                                         
         B     TB60                NO                                           
*                                                                               
TB30     CLC   EDCTFDSK(2),FIRSTRAC    IS IT ALREADY THE FIRST TRAC?            
         BNH   TB40                    YES                                      
         LH    R1,EDCTFDSK         NEED TO DECR. TRACK                          
         SH    R1,=H'1'                                                         
         STH   R1,EDCTFDSK                                                      
         MVC   EDCTFDSK+2(1),EDCTFRPT+1          MAX BLOCK NUM                  
         MVI   EDCTFDSK+3,X'00'                                                 
         LH    R1,EDCTRPB          LOGICAL RECORDS PER BLOCK (112)              
         STC   R1,RECNUM                                                        
         BCTR  R1,0                                                             
         STC   R1,RECSKIP                                                       
*        MVI   RECSKIP,111                                                      
*        MVI   RECNUM,112                                                       
         B     TBX                                                              
TB40     LA    R1,1                                                             
         STC   R1,EDCTFDSK+2       USE BLOCK NUM 1                              
         MVI   EDCTFDSK+3,X'00'                                                 
         XC    RECSKIP,RECSKIP                                                  
         B     MSG2                NO PREVIOUS RECORD                           
*                                                                               
TB60     STC   R1,EDCTFDSK+2       NEW BLK NUMBER                               
         MVI   EDCTFDSK+3,X'00'                                                 
         LH    R1,EDCTRPB          LOGICAL RECORDS PER BLOCK                    
         STC   R1,RECNUM                                                        
         BCTR  R1,0                                                             
         STC   R1,RECSKIP                                                       
*        MVI   RECSKIP,111                                                      
*        MVI   RECNUM,112                                                       
*                                                                               
*BX      MVI   PFKEY,6                                                          
TBX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READ TTBR                                                              
***********************************************************************         
*                                                                               
READBLK  NTR1                                                                   
         LH    R1,EDCTFDSK         IS TRACK FOR THIS DAY                        
         CH    R1,EDCTFLST                                                      
         BH    MSG1                                                             
         GOTO1 VDADDS,DMCB,RDID,EDICTBLK,0,EDICTFL,EDCTFDSK,0                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ETD DISPLAY                                                            
***********************************************************************         
*                                                                               
         USING EDDDSLD,R6                                                       
DDSDISP  NTR1                                                                   
         LA    R2,DDSLST                                                        
         BAS   RE,SYSTYPE                                                       
*                                                                               
         LA    R2,DDSLSTAT                                                      
         BAS   RE,STATUS                                                        
*                                                                               
         CLI   EDFSYS,EDFDAREQ     DARE REC DISPLAY??                           
         BNE   DD10                                                             
         MVC   DDSLDARE,EDFDARE    DARE DATA                                    
         LA    R2,DDSLDA           DISK ADDRESS                                 
         BAS   RE,DISK                                                          
*                                                                               
         USING RDLNNOTD,R1                                                      
         LA    R1,EDFDARE          DARE DATA                                    
         CLC   RDNTTID,=C'ERRNOT'  DARE ERROR                                   
         BNE   EXIT                                                             
         MVC   DDSLERR,RDNTEFLG    DISPLAY ERROR FLAG                           
         B     EXIT                                                             
         DROP  R1                                                               
*                                                                               
DD10     LA    R2,DDSLTMS                                                       
         BAS   RE,TIMESENT                                                      
*                                                                               
         LA    R2,DDSLDAY                                                       
         BAS   RE,DAYDLV                                                        
*                                                                               
         LA    R2,DDSLTMD                                                       
         BAS   RE,TIMEDLV                                                       
*                                                                               
         LA    R2,DDSLSEND                                                      
         BAS   RE,SENDER                                                        
*                                                                               
         LA    R2,DDSLPQ                                                        
         BAS   RE,PQINFO                                                        
*                                                                               
         LA    R2,DDSLDEST                                                      
         BAS   RE,DESTINE                                                       
*                                                                               
         LA    R2,DDSLLEDG                                                      
         BAS   RE,DISK                                                          
*                                                                               
         CLI   EDFMETH,C'F'                                                     
         BNE   EXIT                                                             
         LA    R2,DDSLFTP                                                       
         BAS   RE,DISPFTP                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ETI DISPLAY                                                            
***********************************************************************         
*                                                                               
         USING EDUSERLD,R6                                                      
USERDISP NTR1                                                                   
         LA    R2,EDUST                                                         
         BAS   RE,USERSYS                                                       
*                                                                               
         LA    R2,EDUSTAT                                                       
         BAS   RE,USERSTAT                                                      
*                                                                               
         LA    R2,EDUQDAY                                                       
         BAS   RE,DAYDLV                                                        
*                                                                               
         LA    R2,EDUTIME                                                       
         BAS   RE,USERTIME                                                      
*                                                                               
         CLI   EDFSYS,EDFDARRQ     SKIP DESTINATION AND EZ# FOR DARE            
         BE    UD30                                                             
         LA    R2,EDUDS                                                         
         CLI   EZLED,C'Y'          DISPLAY EASYLINK LEDGER #                    
         BNE   *+12                                                             
         BAS   RE,USERLEDG                                                      
         B     UD30                                                             
         CLI   ELNDIS,C'Y'         DISPLAY EASYLINK MAILBOX#                    
         BNE   *+12                                                             
         BAS   RE,USERELN                                                       
         B     UD30                                                             
         BAS   RE,USERDS           OR DESTINATION                               
*                                                                               
UD30     LA    R2,EDUMCPP                                                       
         BAS   RE,USERAPPL                                                      
*                                                                               
UD100    MVC   EDCTFDSK+3(1),RECNUM                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ETI DISPLAY - TYPE DEPENDANT INFO                                      
***********************************************************************         
*                                                                               
USERAPPL NTR1                                                                   
         USING EDUSERLD,R6                                                      
         CLI   EDFTYPE,EDFTGENQ    GENERAL FAX ACROSS SYSTEMS                   
         BE    UA900                                                            
*                                                                               
         CLI   EDFSYS,EDFSSPTQ     SPOT SYSTEM                                  
         BNE   UA200                                                            
         CLI   EDFTYPE,EDFTADDQ    SPOT ADDS DRAFT ORDERS                       
         BE    UA10                                                             
         CLI   EDFTYPE,EDFTBUCQ    SPOT BUYING GUIDE                            
         BNE   UA20                                                             
UA10     MVC   SAMED,SPEDMED                                                    
         MVC   SACLT,SPEDCLT                                                    
         MVC   SAPRD,SPEDPRD                                                    
         MVC   SAEST,SPEDEST                                                    
         CLC   SPEDFLST,SPACES                                                  
         BNH   UA11                                                             
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,SPEDFLST),(12,SADATES)                              
         MVI   SADATES+5,C'-'                                                   
         LA    R2,SADATES                                                       
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,SPEDFLND),(12,6(R2))                                
UA11     MVC   SASTN,SPEDSTA                                                    
         B     EXIT                                                             
*                                                                               
UA20     DS    0H                                                               
         CLI   EDFTYPE,EDFTREQQ    SPOT ADDS AVAIL REQUESTS                     
         BNE   UA20A                                                            
         MVC   SRMED,SPAVMED                                                    
         MVC   SRCLT,SPAVCLT                                                    
         MVC   SRPRD,SPAVPRD                                                    
         MVC   SREST,SPAVEST                                                    
         CLC   SPAVFLST,SPACES                                                  
         BNH   UA21                                                             
         MVC   SRDATES(5),SPAVFLST                                              
         MVI   SRDATES+5,C'-'                                                   
         MVC   SRDATES+6(5),SPAVFLND                                            
UA21     MVC   SRREF,SPAVREFN                                                   
         B     EXIT                                                             
*                                                                               
UA20A    DS    0H                                                               
         CLI   EDFTYPE,EDFTDMBQ    SPOT DMBDE SPECIAL TRANSMISSION              
         BNE   UA20B                                                            
         MVC   SDMED,SPDAMED                                                    
         MVC   SDCLT,SPDACLT                                                    
         MVC   SDPRD,SPDAPRD                                                    
         MVC   SDEST,SPDAEST                                                    
         MVC   SDMKT,SPDAMKT                                                    
         MVC   SDBYR,SPDABYR                                                    
         MVC   SDCMP,SPDACAM                                                    
         MVC   SDSTA,SPDASTA                                                    
         B     EXIT                                                             
*                                                                               
UA20B    DS    0H                                                               
         CLI   EDFTYPE,EDFTGOAQ    SPOT DMBDE GOAL TRANSFER                     
         BNE   UA20C                                                            
         MVC   SDMED,SPGTMED                                                    
         MVC   SDCLT,SPGTCLT                                                    
         MVC   SDPRD,SPGTPRD                                                    
         MVC   SDEST,SPGTEST                                                    
         B     EXIT                                                             
*                                                                               
UA20C    DS    0H                                                               
         CLI   EDFTYPE,EDFTINVQ    SPOT INVOINCE CONTROL REPORT                 
         BNE   UA30                                                             
         MVC   SVMED,SPNVMED                                                    
         MVC   SVCLT,SPNVCLT                                                    
         MVC   SVSTA,SPNVSTA                                                    
         B     EXIT                                                             
*                                                                               
UA30     DS    0H                                                               
         CLI   EDFTYPE,EDFTTWXQ    SPOT TRAFFIC TWX                             
         BE    UA31                                                             
         CLI   EDFTYPE,EDFTSPGQ    SPOT TRAFFIC SWX                             
         BE    UA31                                                             
         CLI   EDFTYPE,EDFTSPUQ    SPOT TRAFFIC TYPE U                          
         BE    UA31                                                             
         CLI   EDFTYPE,EDFTFAXL    SPOT TRAFFIC FAX LETTERS                     
         BNE   UA40                                                             
UA31     MVC   STMED,EDISTTMD                                                   
         MVC   STCLT,EDISTTCL                                                   
         MVC   STPRD,EDISTTPR                                                   
         CLC   EDISTTP2,SPACES                                                  
         BNH   UA30A                                                            
         MVI   STHYPH,C'-'                                                      
         MVC   STPTNR,EDISTTP2                                                  
UA30A    MVC   STEST,EDISTTES                                                   
         OC    EDISTTDT,EDISTTDT                                                
         BZ    UA33                                                             
         CLC   EDISTTDT,SPACES                                                  
         BNH   UA33                                                             
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,EDISTTDT),(12,STDATES)                              
         MVI   STDATES+5,C'-'                                                   
         LA    R4,EDISTTDT                                                      
         LA    R2,STDATES                                                       
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,6(R4)),(12,6(R2))                                   
UA33     MVC   STCONTC,EDISTTCT                                                 
         B     EXIT                                                             
*                                                                               
UA40     DS    0H                                                               
         CLI   EDFTYPE,EDFTCOVQ    SPOT TRAFFIC COVER LETTER                    
         BE    *+12                                                             
         CLI   EDFTYPE,EDFTSPMQ    SPOT TRAFFIC TYPE M                          
         BNE   UA50                                                             
         MVC   SCMED,EDISTCMD                                                   
         MVC   SCCLT,EDISTCCL                                                   
         MVC   SCPRD,EDISTCPR                                                   
         CLC   EDISTCP2,SPACES                                                  
         BNH   UA40A                                                            
         MVI   SCHYPH,C'-'                                                      
         MVC   SCPTNR,EDISTCP2                                                  
UA40A    MVC   SCEST,EDISTCES                                                   
         CLC   EDISTCDT,SPACES                                                  
         BNH   UA41                                                             
         CLI   EDISTCDT,C'0'                                                    
         BL    UA41                                                             
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,EDISTCDT),(12,SCDATES)                              
         CLC   EDISTCDT+6(6),SPACES                                             
         BNH   UA41                                                             
         MVI   SCDATES+5,C'-'                                                   
         LA    R4,EDISTCDT                                                      
         LA    R2,SCDATES                                                       
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,6(R4)),(12,6(R2))                                   
UA41     MVC   SCCONTC,EDISTCCT                                                 
         B     EXIT                                                             
*                                                                               
UA50     DS    0H                                                               
         CLI   EDFTYPE,EDFTSHIQ    SPOT TRAFFIC SHIPPING ORDERS                 
         BNE   UA60                                                             
         MVC   SSMED,EDISTSMD                                                   
         MVC   SSCLT,EDISTSCL                                                   
         MVC   SSPRD,EDISTSPR                                                   
         CLC   EDISTSP2,SPACES                                                  
         BNH   UA50A                                                            
         MVI   SSHYPH,C'-'                                                      
         MVC   SSPTNR,EDISTSP2                                                  
UA50A    MVC   SSHOUSE,EDISTSHS                                                 
         MVC   SSCONTC,EDISTSCT                                                 
         B     EXIT                                                             
*                                                                               
UA60     DS    0H                                                               
         CLI   EDFTYPE,EDFTXCPQ    SX- ADDS CONFIRMATION OF PURCHASE            
         BE    UA62                                                             
         CLI   EDFTYPE,EDFTSRYQ    SY- SPOT RY REPORT                           
         BE    UA62                                                             
         CLI   EDFTYPE,EDFTSRXQ    SR- SPOT RX REPORT                           
         BNE   EXIT                                                             
UA62     MVC   SAMED,SPCPMED                                                    
         MVC   SACLT,SPCPCLT                                                    
         MVC   SAPRD,SPCPPRD                                                    
         MVC   SAEST,SPCPEST                                                    
         MVC   SASTN,SPCPSTA                                                    
         MVC   SAREQ,SPCPRQST                                                   
         B     EXIT                                                             
*                                                                               
UA200    DS    0H                                                               
         CLI   EDFSYS,EDFSNETQ     NET                                          
         BNE   UA300                                                            
         CLI   EDFTYPE,EDFTNINQ    NET TRAFFIC NINS GEN                         
         BNE   UA210                                                            
         MVC   NNMED,EDINTNMD                                                   
         MVC   NNCLT,EDINTNCL                                                   
         MVC   NNPRD,EDINTNPR                                                   
         MVC   NNNET,EDINTNET                                                   
         MVC   NNPGM,EDINTNPG                                                   
         CLC   EDINTNDT,SPACES                                                  
         BNH   UA201                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,EDINTNDT),(12,NNDATES)                              
         MVI   NNDATES+5,C'-'                                                   
         LA    R4,EDINTNDT                                                      
         LA    R2,NNDATES                                                       
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,6(R4)),(12,6(R2))                                   
UA201    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
UA210    DS    0H                                                               
         CLI   EDFTYPE,EDFTCABQ    NET TRAFFIC CABLE GEN                        
         BNE   UA220                                                            
         MVC   NCMED,EDINTCMD                                                   
         MVC   NCCLT,EDINTCCL                                                   
         MVC   NCPRD,EDINTCPR                                                   
         MVC   NCNET,EDINTCNT                                                   
         CLC   EDINTNDT,SPACES                                                  
         BNH   UA211                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,EDINTCDT),(12,NCDATES)                              
         MVI   NCDATES+5,C'-'                                                   
         LA    R4,EDINTCDT                                                      
         LA    R2,NCDATES                                                       
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,6(R4)),(12,6(R2))                                   
UA211    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
UA220    DS    0H                                                               
         CLI   EDFTYPE,EDFTPAKQ    NETPAK APPLICATION                           
         BNE   UA230                                                            
         MVC   NPMED,NEEDMED                                                    
         MVC   NPCLT,NEEDCLT                                                    
         MVC   NPNET,NEEDNET                                                    
         MVC   NPEST,NEEDEST                                                    
         MVC   NPPAK,NEEDPAK                                                    
         MVC   NPREQ,NEEDREQ                                                    
         MVC   NPPRG,NEEDPRG                                                    
         B     EXIT                                                             
*                                                                               
UA230    CLI   EDFTYPE,EDFTNRXQ    NX -NETPAK RX REPORT                         
         BNE   UA300                                                            
         MVC   SAMED,SPCPMED                                                    
         MVC   SACLT,SPCPCLT                                                    
         MVC   SAPRD,SPCPPRD                                                    
         MVC   SAEST,SPCPEST                                                    
         MVC   SASTN,SPCPSTA                                                    
         MVC   SAREQ,SPCPRQST                                                   
         B     EXIT                                                             
*                                                                               
UA300    DS    0H                                                               
         CLI   EDFSYS,EDFSPRTQ     PRINT                                        
         BNE   UA400                                                            
         CLI   EDFTYPE,EDFTINSQ    PRINT INSERTIONS                             
         BE    UA310                                                            
         CLI   EDFTYPE,EDFTPPPQ    PRINT SOMETHING                              
         BE    UA310                                                            
         CLI   EDFTYPE,EDFTPINQ    PRINT INVOICE CONTROL REPORTS                
         BNE   UA400                                                            
UA310    MVC   PIMED,PPEDMED                                                    
         MVC   PICLT,PPEDCLT                                                    
         MVC   PIPRD,PPEDPRD                                                    
         MVC   PIPUB,PPEDPUB                                                    
         CLI   EDFTYPE,EDFTPPPQ    PRINT SOMETHING                              
         BE    *+14                                                             
         MVC   PIJOB,PPEDJOB                                                    
         B     EXIT                                                             
         MVC   PIJOB,PPEDRPT                                                    
         B     EXIT                                                             
*                                                                               
UA400    DS    0H                                                               
         CLI   EDFSYS,EDFSREPQ     REP                                          
         BNE   UA500                                                            
         CLI   EDFTYPE,EDFTKWXQ    REP KWX                                      
         BNE   UA410                                                            
         MVC   RKOFF,EDIRKXOF                                                   
         MVC   RKBOOK,EDIRKXBC+2                                                
         MVC   RKKWXN,EDIRKXNM                                                  
         MVC   RKSTN,EDIRKXST                                                   
         B     EXIT                                                             
*                                                                               
UA410    CLI   EDFTYPE,EDFTSTNQ    REP STATION REP                              
         BNE   UA420                                                            
         MVC   RKSTN,EDIRDTST                                                   
         B     EXIT                                                             
*                                                                               
UA420    CLI   EDFTYPE,EDFTCONQ    REP CONTRACT                                 
         BE    UA421                                                            
         CLI   EDFTYPE,EDFTCNEQ    REP CONTRACT                                 
         BE    UA421                                                            
         CLI   EDFTYPE,EDFTCCCQ    REP CONTRACT                                 
         BNE   UA500                                                            
UA421    MVC   ROOFF,EDIRCNOF                                                   
         MVC   ROSLP,EDIRCNSP                                                   
         MVC   ROHNM,EDIRCNHN                                                   
         MVC   ROAGY,EDIRCNAG                                                   
         MVC   ROCTY,EDIRCNAO                                                   
         MVC   ROADV,EDIRCNAD                                                   
         MVC   ROSTN,EDIRCNST                                                   
         B     EXIT                                                             
*                                                                               
UA500    DS    0H                                                               
         CLI   EDFSYS,EDFSACCQ    ACC REPORT                                    
         BNE   UA600                                                            
         CLI   EDFTYPE,EDFTORDQ   ORDER                                         
         BNE   UA600                                                            
         CLI   ACFXTYPE,C'P'                                                    
         BNE   UA510                                                            
         MVC   AOCLT,ACFXCLI                                                    
         MVC   AOPRD,ACFXPRO                                                    
         MVC   AOJOB(6),ACFXJOB                                                 
         MVC   AOPONUM,ACFXONUM                                                 
         MVC   AOVEND,ACFXVEND                                                  
         B     EXIT                                                             
UA510    DS    0H                                                               
         MVC   AOJOB(14),ACFXEXP                                                
         MVC   AOPONUM,ACFXONUM                                                 
         MVC   AOVEND,ACFXVEND                                                  
         B     EXIT                                                             
*                                                                               
UA600    DS    0H                                                               
         CLI   EDFSYS,EDFDARRQ     DARE SYSTEM                                  
         BNE   UA700                                                            
         ZIC   RF,SYSID            FACPAK NUMBER                                
         MH    RF,=Y(L'FACIDTAB)                                                
         LA    RF,FACIDTAB(RF)     RF = A(ENTRY IN FACIDTAB)                    
         TM    5(RF),X'20'         IS THIS A REP SYSTEM?                        
         BZ    UA710               NO                                           
*                                                                               
         MVC   DRSTA,EDIRDRST      REP DARE LINE                                
         MVC   DRAGY,EDIRDRAG                                                   
         MVC   DRMED,EDIRDRMD                                                   
         MVC   DRCLT,EDIRDRCL                                                   
         MVC   DRPRD,EDIRDRP1                                                   
         CLC   EDIRDRP2,SPACES                                                  
         BNH   *+14                                                             
         MVI   DRHYPH,C'-'                                                      
         MVC   DRPTN,EDIRDRP2                                                   
         MVC   DREST,EDIRDRES                                                   
         MVC   DRBYR,EDIRDRBY                                                   
         MVC   DRORD#,EDIRDRAN                                                  
         MVC   DRSAL,EDIRDRSP                                                   
         MVC   DRCON#,EDIRDRCN                                                  
         B     EXIT                                                             
*                                                                               
UA710    MVC   DASTA,EDIRDRST      ADV DARE LINE                                
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN                                        
         BZ    *+10                                                             
         MVC   DADEST,EDFDEST                                                   
         MVC   DAMED,EDIRDRMD                                                   
         MVC   DACLT,EDIRDRCL                                                   
         MVC   DAPRD,EDIRDRP1                                                   
         CLC   EDIRDRP2,SPACES                                                  
         BNH   *+14                                                             
         MVI   DAHYPH,C'-'                                                      
         MVC   DAPTN,EDIRDRP2                                                   
         MVC   DAEST,EDIRDRES                                                   
         MVC   DABYR,EDIRDRBY                                                   
         MVC   DAORD#,EDIRDRAN                                                  
         MVC   DASAL,EDIRDRSP                                                   
         MVC   DACON#,EDIRDRCN                                                  
         B     EXIT                                                             
*                                                                               
UA700    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
UA900    DS    0H                                                               
         MVC   GXDISP,EDFAPPL    JUST DISP AS IS                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY SYSTEM-TYPE -- USER TERMINAL                                   
***********************************************************************         
*                                                                               
USERSYS  NTR1                                                                   
         MVC   0(1,R2),EDFSYS                                                   
         MVI   1(R2),C'-'                                                       
         MVC   2(1,R2),EDFTYPE                                                  
         B     YES                                                              
***********************************************************************         
*        DISPLAY STATUS -- USER TERMINAL                                        
***********************************************************************         
*                                                                               
USERSTAT NTR1                                                                   
         SH    R6,=H'8'            BACK UP TO HEADER                            
         OI    6(R6),X'00'         TURN OFF                                     
*                                                                               
         LA    R4,STATTAB                                                       
US10     MVC   WORK(1),0(R4)                                                    
         NC    WORK(1),EDFSTAT                                                  
         CLC   WORK(1),0(R4)       IS BIT ON                                    
         BE    US20                                                             
         LA    R4,5(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   US10                                                             
         B     USX                                                              
*                                                                               
US20     MVC   PRTSTAT,1(R4)                                                    
         TM    0(R4),EDFSTJNK+EDFSTCAN                                          
         BZ    *+8                                                              
         OI    6(R6),X'08'         HIGHLIGHT GARBAGE                            
USX      MVC   0(4,R2),PRTSTAT                                                  
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY TIME -- USER TERMINAL                                          
***********************************************************************         
*                                                                               
USERTIME NTR1                                                                   
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN  WAS REPORT RECEIVED OR CANX           
         BZ    UT20                                                             
         OC    EDFRCVTM,EDFRCVTM   USE DLVD TIME                                
         BNZ   UT10                                                             
         MVC   0(4,R2),=C'2400'    ZERO TIME MEANS MIDNIGHT                     
         B     YES                                                              
UT10     L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFRCVTM,0(R2),2                                       
         B     YES                                                              
*                                                                               
UT20     TM    EDFSTAT,EDFSTSNT    WAS REPORT SENT                              
         BNO   YES                                                              
         OC    EDFSNTIM,EDFSNTIM                                                
         BNZ   UT22                                                             
         MVC   0(4,R2),=C'2400'    ZERO TIME MEANS MIDNIGHT                     
         B     YES                                                              
UT22     L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFSNTIM,0(R2),2                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EITHER DESTINATION OR SENDER -- USER TERMINAL                  
***********************************************************************         
*                                                                               
USERDS   NTR1                                                                   
         OC    PRTSEND,PRTSEND                                                  
         BZ    USERDEST                                                         
         MVC   0(8,R2),PRTSEND     SENDERS ALPHA USERID                         
         B     YES                                                              
USERDEST DS    0H                                                               
         MVC   0(16,R2),EDFDEST    DESTINATION                                  
         B     YES                                                              
***********************************************************************         
*        DISPLAY EASYLINK LEDGER NUMBER -- USER TERMINAL                        
***********************************************************************         
*                                                                               
USERLEDG NTR1                                                                   
         CLI   EDFMETH,C'F'        FTP METHOD                                   
         BNE   UL100                                                            
         EDIT  (B4,EDFEZLED),(5,0(R2))                                          
         B     YES                                                              
UL100    MVC   0(8,R2),EDFEZLED                                                 
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EASYLINK AMILBOX NUMBER -- USER TERMINAL                       
***********************************************************************         
*                                                                               
USERELN  NTR1                                                                   
         MVC   WORK(2),=C'62'                ALWAYS STARTS WITH 62              
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFEZBOX,WORK+2,6                                      
         MVC   0(8,R2),WORK                                                     
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT SYSTEM AND TYPE ---- DDS TERMINAL                   
***********************************************************************         
*                                                                               
SYSTYPE  NTR1                                                                   
         MVI   0(R2),C'*'                                                       
         OC    EDFSYS,EDFSYS                                                    
         BZ    *+10                                                             
         MVC   0(1,R2),EDFSYS                                                   
*                                                                               
         MVI   1(R2),C'*'                                                       
         OC    EDFTYPE,EDFTYPE                                                  
         BZ    *+10                                                             
         MVC   1(1,R2),EDFTYPE                                                  
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT METHOD-STATUS  -- DDS TERMINAL                      
***********************************************************************         
*                                                                               
STATUS   NTR1                                                                   
         MVI   0(R2),C'*'                                                       
         OC    EDFMETH,EDFMETH                                                  
         BZ    *+10                                                             
         MVC   0(1,R2),EDFMETH                                                  
         MVC   1(1,R2),=C'-'                                                    
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFSTAT,2(R2),1                                        
         OC    EDFSTAT,EDFSTAT                                                  
         BNZ   *+10                                                             
         MVC   2(2,R2),=C'**'                                                   
*                                                                               
         SH    R6,=H'8'                                                         
         MVI   6(R6),0                                                          
         TM    EDFSTAT,EDFSTSNT+EDFSTRCV+EDFSTWTG                               
         BNZ   DSX                                                              
         OI    6(R6),X'08'         HIGHLIGHT                                    
DSX      LA    R6,8(R6)                                                         
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT TIME SENT -- DDS TERMINAL                           
***********************************************************************         
*                                                                               
TIMESENT NTR1                                                                   
         TM    EDFSTAT,EDFSTSNT    WAS REPORT SENT                              
         BNO   YES                                                              
         OC    EDFSNTIM,EDFSNTIM                                                
         BNZ   TS10                                                             
         MVC   0(4,R2),=C'2400'    ZERO TIME MEANS MIDNIGHT                     
         B     YES                                                              
TS10     L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFSNTIM,0(R2),2                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT SYMBOL IF NOT SAME DAY  --  DDS                     
***********************************************************************         
*                                                                               
DAYDLV   NTR1                                                                   
         TM    EDFSTAT,EDFSTRCV    WAS REPORT RECEIVED                          
         BNO   YES                                                              
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DAY),(1,DAYPWOS)                                    
         CLC   EDFRCVDY,DAYPWOS+2  SAME DAY DELIVERY                            
         BE    YES                                                              
         MVI   0(R2),C'*'                                                       
         B     YES                                                              
***********************************************************************         
*        ROUTINE TO PUT OUT TIME DELIVERED -- DDS TERMINAL                      
***********************************************************************         
*                                                                               
TIMEDLV  NTR1                                                                   
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN    WAS REPORT RECEIVED OR CANX         
         BZ    TD20                                                             
         OC    EDFRCVTM,EDFRCVTM   USE DLVD TIME                                
         BNZ   TD10                                                             
         MVC   0(4,R2),=C'2400'    ZERO TIME MEANS MIDNIGHT                     
         B     YES                                                              
TD10     L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFRCVTM,(R2),2                                        
         B     YES                                                              
*                                                                               
TD20     TM    EDFSTAT,EDFSTJNK    IF JUNK PUT OUT ERROR NUMBER                 
         BNO   YES                                                              
         MVC   0(2,R2),=C'ER'                                                   
         LA    R2,2(R2)                                                         
         EDIT  (B1,EDFERROR),(2,(R2)),FILL=0                                    
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT SENDER -- DDS TERMINAL                              
***********************************************************************         
*                                                                               
SENDER   NTR1                                                                   
         MVC   IDNUM,EDFPQUID                                                   
         BAS   RE,ALPHANME         GET ALPHA SENDER'S ID                        
         MVC   PRTSEND,NAME                                                     
SEND10   MVC   0(8,R2),PRTSEND     SENDERS ALPHA USERID                         
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT PRINT QUEUE INFORMATION: REPORT SUB-ID,             
*        REFERENCE NUMBER, CREATION TIME, LOG. SEQ NUMBER - DDS TERM            
***********************************************************************         
*                                                                               
PQINFO   NTR1                                                                   
         MVC   0(3,R2),EDFPQSUB                                                 
         OC    EDFPQSUB,EDFPQSUB                                                
         BNZ   *+10                                                             
         MVC   0(3,R2),=C'***'                                                  
         MVI   3(R2),C','                                                       
         EDIT  (B2,EDFPQREF),(5,4(R2)),FILL=0                                   
         OC    EDFPQREF,EDFPQREF                                                
         BNZ   *+10                                                             
         MVC   4(5,R2),=C'*****'                                                
         MVI   9(R2),C'/'                                                       
*                                                                               
         SR    R4,R4               CREATION TIME                                
         ZICM  R5,EDFPQTIM,2         TIME IN 3/4 SECS                           
         D     R4,=F'3'            DIVIDE BY 3                                  
         SR    R4,R4                                                            
         M     R4,=F'4'            MULT BY 4                                    
         SR    R4,R4                                                            
         D     R4,=F'60'           TO GET MIN                                   
         SR    R4,R4                                                            
         D     R4,=F'60'           TO GET HOURS W/MIN REMAINDING                
         EDIT  (R5),(2,10(R2)),FILL=0                                           
         EDIT  (R4),(2,12(R2)),FILL=0                                           
         OC    EDFPQTIM,EDFPQTIM                                                
         BNZ   *+10                                                             
         MVC   10(4,R2),=C'****'                                                
*                                                                               
         MVI   14(R2),C'/'                                                      
         EDIT  (B2,EDFPQSEQ),(5,15(R2)),FILL=0                                  
         OC    EDFPQSEQ,EDFPQSEQ                                                
         BNZ   *+10                                                             
         MVC   16(5,R2),=C'*****'                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT THE DESTINATION -- DDS TERMINAL                     
***********************************************************************         
*                                                                               
DESTINE  NTR1                                                                   
         MVC   0(16,R2),=C'****************'                                    
         OC    EDFDEST,EDFDEST                                                  
         BZ    *+10                                                             
         MVC   0(16,R2),EDFDEST                                                 
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        FOR NOW WILL BE THE DISK ADDRESS - FOR DAVID -- DDS TERMINAL           
***********************************************************************         
*                                                                               
DISK     NTR1                                                                   
         OC    EDCTFDSK,EDCTFDSK                                                
         BNZ   *+14                                                             
         MVC   0(8,R2),=C'********'                                             
         B     YES                                                              
         MVC   EDCTFDSK+3(1),RECNUM                                             
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDCTFDSK,0(R2),4                                       
         B     YES                                                              
***********************************************************************         
*        ROUTINE TO DISPLAY NETVIEW FTP QUEUE NUMBER                            
***********************************************************************         
*                                                                               
DISPFTP  NTR1                                                                   
         MVC   0(5,R2),=C'*****'                                                
         OC    EDFEZLED(4),EDFEZLED                                             
         BZ    YES                                                              
         EDIT  (B4,EDFEZLED),(5,0(R2))                                          
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET ALPHA SENDER'S ID                                       
*        USES NUMBER IN IDNUM AND PUTS NAME IN NAME                             
***********************************************************************         
*                                                                               
ALPHANME NTR1                                                                   
         XC    NAME,NAME                                                        
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),IDNUM                                                  
         CLC   KEY,IO              SAME READ                                    
         BE    SKIPREAD                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO,0                     
         CLC   KEY,IO                                                           
         BNE   USENUM                                                           
SKIPREAD LA    R4,IO                                                            
         MVI   ELCODE,2            PASSIVE POINTER ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   USENUM                                                           
         MVC   NAME,2(R4)          SENDERS ALPHA ID                             
         B     EXIT                                                             
USENUM   DS    0H                                                               
         EDIT  (B2,IDNUM),(5,NAME)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CLEAR FILTERS IF CHANGED                                               
***********************************************************************         
*                                                                               
PARMCLR  NTR1                                                                   
         XC    PARMBLK(PBLKLNQ),PARMBLK                                         
         XC    DSKAD,DSKAD                                                      
         XC    FDSKAD,FDSKAD                                                    
         B     EXIT                                                             
         SPACE 5                                                                
***********************************************************************         
*        CALL GETHELP AND EXIT                                                  
***********************************************************************         
*                                                                               
HELPOUT  L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         L     RF,VCOMFACS                                                      
         L     RF,CGETHELP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'50',HELPKEY),QHDR,0,0,0                             
         DC    H'0'                GETHELP EXITS TO MONITOR                     
         EJECT                                                                  
***********************************************************************         
*        MESSAGES AND COMMON EXIT                                               
***********************************************************************         
*                                                                               
MSG0     MVC   EDSMSG(37),=C'THERE ARE MORE ITEMS - ENTER FOR NEXT'             
         CLI   DDSVERS,C'Y'        IS IT ETD?                                   
         BE    MSG00               NO                                           
         TM    FLAG,BOTTOM         PF TO BOTTOM                                 
         BO    MSG0A                                                            
         B     MSGX                                                             
*                                                                               
MSG00    TM    FLAG,BOTTOM         PF TO BOTTOM                                 
         BO    MSG0B                                                            
         MVC   EDSMSG(37),=C'THERE ARE MORE ITEMS - PF6 FOR NEXT  '             
         B     MSGX                                                             
*                                                                               
MSG0A    MVC   EDSMSG(37),=C'BOTTOM OF LIST - ENTER FOR FIRST     '             
         B     MSGXC                                                            
MSG0B    MVC   EDSMSG(37),=C'BOTTOM OF LIST                       '             
         B     MSGX                                                             
*                                                                               
MSG1     MVC   EDSMSG(28),=C'NO MORE RECORDS FOR THIS DAY'                      
         B     MSGX                                                             
*                                                                               
MSG2     MVC   EDSMSG(32),=C'NO PREVIOUS RECORDS FOR THIS DAY'                  
         B     MSGXC                                                            
*                                                                               
MSG2A    MVC   EDSMSG(32),=C'DISK ADDRESS OUTSIDE OF THIS DAY'                  
         B     MSGXC                                                            
*                                                                               
MSG3     CLI   DDSVERS,C'Y'        IS IT ETD?                                   
         BE    MSG4                NO                                           
         MVC   EDSMSG(41),=C'THERE ARE NO MORE ITEMS - ENTER FOR FIRST'         
         BNE   MSGXC                                                            
*                                                                               
MSG4     MVC   EDSMSG(41),=C'THERE ARE NO MORE ITEMS                  '         
         B     MSGXC                                                            
*                                                                               
MSG23    MVC   EDSMSG(23),=C'PLEASE RE-ENTER REQUEST'                           
         B     MSGXC                                                            
*                                                                               
MSGXC    XC    EDCTFDSK,EDCTFDSK   WANT TO START OVER                           
MSGX     MVC   LASTTBR,EDCTFDSK                                                 
         MVC   ISITME,=C'$EDI'                                                  
         MVC   LASTTRN,TRNNUM      UPDATE TRANSACTION COUNT                     
         BAS   RE,WRITESTR                                                      
         OI    EDSMSGH+6,X'80'                                                  
         LA    R2,EDSCURSH                                                      
         OI    6(R2),X'40'         POSN CURSOR                                  
         L     RD,BASERD                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        WRITE OUT SAVED STORAGE                                                
***********************************************************************         
*                                                                               
WRITESTR NTR1                                                                   
         MVC   SR$EDICT(SAVEDL),SAVEDSTR                                        
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TERMNUM                                                     
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DMWRT',=C'TEMPSTR',(R2),SRSD                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        STUFF                                                                  
***********************************************************************         
*                                                                               
DIE      DC    H'0'                                                             
YES      SR    RC,RC               SET CC EQUAL                                 
NO       LTR   RC,RC               SET CC NOT EQUAL                             
EXIT     XIT1                                                                   
*                                                                               
HELPID   DC    XL10'011AFF00000000000000'  SYS/PRG/SCRN                         
SPACES   DC    CL20' '                                                          
         GETEL R4,28,ELCODE                                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        TABLES ETC                                                             
***********************************************************************         
*                                                                               
COLSADV  DC    CL40'SYS STAT TIME DEST/SENDER      M CLT PRD'                   
         DC    CL39'-PTN REPORT INFORMATION'                                    
COLSREP  DC    CL40'SYS STAT TIME DEST/SENDER      OF INI KW'                   
         DC    CL39'X/CON# REPORT INFORMATION'                                  
COLSDARA DC    CL40'SYS STAT TIME DEST/REP   STATN  M CLT PR'                   
         DC    CL39'D-PTN EST  BYR  ORDER#   SAL CONTRCT#'                      
COLSDARR DC    CL40'SYS STAT TIME  STATN  AGY   M CLT PRD-PT'                   
         DC    CL39'N EST  BYR  ORDER#   SAL CONTRCT#'                          
COLSDDS  DC    CL40'STAT ST SENT DLVD SENDER   PQ INFO      '                   
         DC    CL39'        DESTINATION      DSKADDR  FTPQ#'                    
*                                                                               
STATTAB  DC    AL1(EDFSTJNK),CL4'DDS '                                          
         DC    AL1(EDFSTCAN),CL4'CANX'                                          
         DC    AL1(EDFSTRCV),CL4'DLVD'                                          
         DC    AL1(EDFSTSNT),CL4'SENT'                                          
         DC    AL1(EDFSTWTG),CL4'WTNG'                                          
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PARAMETERS                                                    
***********************************************************************         
*                                                                               
         DS    0H                                                               
PARAMS   NMOD1 0,**PARM**                                                       
         L     RC,0(R1)                                                         
         BAS   RE,VALSUMM          CHECK FOR SUMMARY OPTION                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SYSTEM                                                        
***********************************************************************         
*                                                                               
         LA    R2,EDSP1H                                                        
         NI    FLAGS,X'FF'-FLNEWSYS                                             
         CLI   8(R2),C'?'          HELP?                                        
         BNE   VSYS10                                                           
         MVI   HELPNUM,1                                                        
         ST    R2,QHDR                                                          
         B     HELPOUT                                                          
*                                                                               
VSYS10   CLI   5(R2),0             ANY INPUT                                    
         BNE   VSYS20              YES                                          
         TM    FLAGS,FLSUMM        SUMMARY OPTION                               
         BO    *+12                THEN SYSTEM NOT REQUIRED                     
         CLI   DDSVERS,C'Y'        DDS TERMINAL CAN HAVE NO SYS FILTER          
         BNE   MSG6                                                             
         XC    SYSFILT,SYSFILT     NO SYSTEM FILTER                             
         B     VOPT10                                                           
*                                                                               
VSYS20   MVC   SYSFILT,OLDSYST                                                  
         CLC   OLDSYST,8(R2)       SAME AS BEFORE                               
         BE    VOPT10              THEN CHECK OPTIONS                           
         BAS   RE,PARMCLR          CLEAR OLD PARAMETERS                         
         OI    FLAGS,FLNEWSYS                                                   
         XC    SYSFILT,SYSFILT                                                  
*                                                                               
         LA    R1,STTABLE          SYSTEM AND TYPES TABLE                       
VSYS30   CLC   8(1,R2),0(R1)                                                    
         BE    VSYS40                                                           
         ZIC   R4,1(R1)            #OF TYPES UNDER SYSTEM                       
         LA    R1,2(R4,R1)         BUMP TO NEXT SYSTEM                          
         CLI   0(R1),0                                                          
         BNE   VSYS30                                                           
         B     MSG7                INVALID SYSTEM                               
VSYS40   MVC   SYSFILT,0(R1)                                                    
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                                       
***********************************************************************         
*                                                                               
VOPT10   MVC   OLDSYST,SYSFILT     IN CASE IT CHANGED (SAVE)                    
         LA    R2,EDSP2H                                                        
         CLI   8(R2),C'?'          HELP?                                        
         BNE   VOPT15                                                           
         MVI   HELPNUM,2                                                        
         ST    R2,QHDR                                                          
         B     HELPOUT                                                          
*                                                                               
VOPT15   TM    FLAGS,FLNEWSYS      NEW SYSTEM                                   
         BO    VOPT20                                                           
         CLC   OLDFILT,8(R2)       SAME AS BEFORE                               
         BE    EXIT                                                             
*                                                                               
VOPT20   MVC   OLDFILT,8(R2)       SAVE THE NEW FILTERS                         
         BAS   RE,PARMCLR          CLEAR PARAMS                                 
         CLI   5(R2),0             ANY INPUT IN OPTIONS FIELD                   
         BE    EXIT                                                             
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(16,(R2)),SCANBOX                                      
         CLI   DMCB+4,X'00'                                                     
         BE    MSG11                                                            
*                                                                               
         USING SCANBLKD,R2                                                      
         LA    R2,SCANBOX                                                       
*                                                                               
         USING OPTTABD,R4                                                       
VOPT100  LA    R4,OPTTABLE         TABLE OF VALID OPTIONS                       
VOPT110  ZIC   R1,SC1STLEN                                                      
         SH    R1,=H'1'                                                         
         BM    EXIT                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),OPTNAME                                              
         BE    VOPT150                                                          
         LA    R4,OPTTLNQ(R4)                                                   
         CLI   0(R4),0                                                          
         BNE   VOPT110                                                          
         MVC   EDSMSG(5),SC1STFLD                                               
         B     MSG17                                                            
*                                                                               
VOPT150  ICM   RF,15,OPTROUT       OPTION ROUTINE                               
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R2,SCBLKLQ+6(R2)      NEXT SCANNER LINE                          
         B     VOPT100                                                          
         EJECT                                                                  
***********************************************************************         
*        METHOD OF TRANSMISSION FILTER                                          
***********************************************************************         
*                                                                               
METHF    NTR1                                                                   
         LA    R4,METHTAB                                                       
METHF10  CLC   SC2NDFLD(1),0(R4)     VALID METHOD                               
         BE    METHF20                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   METHF10                                                          
         MVC   EDSMSG(1),SC2NDFLD                                               
         B     MSG19                                                            
METHF20  MVC   METHFILT,SC2NDFLD                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        TYPE FILTER                                                            
***********************************************************************         
*                                                                               
TYPEF    NTR1                                                                   
         OC    SYSFILT,SYSFILT                                                  
         BNZ   TYPEF50                                                          
         CLI   DDSVERS,C'Y'                                                     
         BNE   DIE                 ONLY DDSVERS CAN NOT HAVE A SYSTEM           
         MVC   TYPEFILT,SC2NDFLD                                                
         B     TYPEFX                                                           
*                                                                               
TYPEF50  MVC   TYPEFILT,SC2NDFLD                                                
         BAS   RE,VALTYPE                                                       
TYPEFX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TYPE WITHIN ITS SYSTEM                                        
***********************************************************************         
*                                                                               
VALTYPE  NTR1                                                                   
         LA    R1,STTABLE          SYSTEM AND TYPES TABLE                       
VT10     CLC   SYSFILT,0(R1)       MATCH ON SYSTEM                              
         BE    VT20                                                             
         ZIC   R4,1(R1)            #OF TYPES UNDER SYSTEM                       
         LA    R1,2(R4,R1)         BUMP TO NEXT SYSTEM                          
         CLI   0(R1),0                                                          
         BNE   VT10                                                             
         B     VTERR               INVALID TYPE                                 
*                                                                               
VT20     ZIC   R4,1(R1)            #OF TYPES UNDER SYSTEM                       
         LA    R1,2(R1)            BEGINNING OF TYPE LIST                       
VT30     CLC   TYPEFILT,0(R1)                                                   
         BE    VTX                                                              
         LA    R1,1(R1)                                                         
         BCT   R4,VT30                                                          
*                                                                               
VTERR    MVC   EDSMSG(1),TYPEFILT                                               
         B     MSG18               INVALID TYPE                                 
VTX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        STATUS FILTER                                                          
***********************************************************************         
*                                                                               
STATF    NTR1                                                                   
         LA    R4,STATTAB          VALID STATII                                 
*                                                                               
         ZIC   R1,SC2NDLEN                                                      
         SH    R1,=H'1'                                                         
         BM    MSG12                                                            
*                                                                               
STATF60  EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'NOTPRTD'                                          
         BNE   STATF10                                                          
         MVC   STATFILT,=C'NOTP'                                                
         B     STATFX                                                           
*                                                                               
STATF10  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC2NDFLD(0),1(R4)                                                
         BE    STATF20                                                          
         LA    R4,5(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   STATF10                                                          
         MVC   EDSMSG(4),SC2NDFLD                                               
         B     MSG12                                                            
*                                                                               
STATF20  MVC   STATFILT,1(R4)                                                   
STATFX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DESTINATION FILTER                                                     
***********************************************************************         
*                                                                               
DESTF    NTR1                                                                   
         TM    FLAGS,FLSENT        IS THERE ALREADY A SENT FILTER               
         BO    MSG15                                                            
         ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG13                                                            
         STC   R1,DESTLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DESTFILT(0),SC2NDFLD                                             
         OI    FLAGS,FLDEST        YES THERE'S A DEST FILTER                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REQUESTOR FILTER                                                       
***********************************************************************         
*                                                                               
REQF     NTR1                                                                   
         MVC   REQFILT,SC2NDFLD                                                 
         OC    REQFILT,SPACES                                                   
         B     EXIT                                                             
*                                                                               
         SPACE 5                                                                
***********************************************************************         
*        DARE ORDER# FILTER                                                     
***********************************************************************         
*                                                                               
ORDERF   NTR1                                                                   
         CLI   SC2NDLEN,0                                                       
         BNE   ORDF10                                                           
         CLI   SC1STLEN,1                                                       
         BE    ORDF20                                                           
ORDF10   MVC   ORDERFLT,SC2NDFLD                                                
         B     EXIT                                                             
*                                                                               
ORDF20   CLI   DDSTERM,C'Y'        JUST O IS DARE OPTION                        
         BNE   MSG20                                                            
         OI    FLAGS,FLODARE                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DARE FILTER - INCOMING DARE ONLY                                       
***********************************************************************         
*                                                                               
DAREIF   NTR1                                                                   
         CLI   DDSTERM,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   SC1STFLD,C'I'                                                    
         BNE   *+12                                                             
         CLI   SYSFILT,EDFDARRQ    SYSTEM DARE                                  
         BNE   MSG20                                                            
         OI    FLAGS,FLIDARE                                                    
         MVC   DIDFILT,SC2NDFLD    DARE= DARE ID                                
         OC    DIDFILT,SPACES                                                   
         B     EXIT                                                             
         SPACE 5                                                                
***********************************************************************         
*        DARE FILTER - OUTGOING DARE ONLY                                       
***********************************************************************         
*                                                                               
DAREOF   NTR1                                                                   
         CLI   DDSTERM,C'Y'                                                     
         BNE   MSG20                                                            
         OI    FLAGS,FLODARE                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        STATION FILTER                                                         
***********************************************************************         
*                                                                               
STATNF   NTR1                                                                   
         ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG13                                                            
         STC   R1,STATNLEN         STATION                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STATNFLT(0),SC2NDFLD                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        TIME FILTER                                                            
***********************************************************************         
*                                                                               
TIMEF    NTR1                                                                   
         ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG13                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIMEFILT(0),SC2NDFLD                                             
         OC    TIMEFILT,SPACES                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SUMMARY OPTION                                                         
***********************************************************************         
*                                                                               
SUMMF    NTR1                                                                   
         OI    FLAGS,FLSUMM                                                     
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        EZ OR FTP # DISPLAY OPTION                                             
***********************************************************************         
*                                                                               
EZF      NTR1                                                                   
         MVI   EZLED,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ELN -EASYLINK MAILBOX NUMBER DISPLAY OPTION                            
***********************************************************************         
*                                                                               
ELNF     NTR1                                                                   
         MVI   ELNDIS,C'Y'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DATE FILTER                                                            
***********************************************************************         
*                                                                               
DATEF    NTR1                                                                   
         CLI   SC2NDLEN,2          CAN USE DAY NUMBER                           
         BH    DF20                                                             
         TM    SC2NDVAL,SCNUMQ     VALID NUMERIC                                
         BNO   DF20                                                             
*                                                                               
         L     RF,VCOMFACS         GET TODAY'S DATE                             
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(5,0),(11,WORK)                                        
         CLC   WORK+3(2),SC2NDFLD  COMPARE TO ENTERED DAY                       
         BL    DF10                                                             
         MVC   WORK+3(2),SC2NDFLD  REPLACE DAY AND USE THIS MONTH               
         MVC   SC2NDFLD(8),WORK       IF TODAY >= ENTERED DAY                   
         MVI   SC2NDLEN,8                                                       
         B     DF20                                                             
*                                                                               
DF10     MVC   WORK+3(2),SC2NDFLD     IF TODAY < ENTERED DAY                    
         MVC   WORK+8(5),=C'(-1M)'    THEN BACK UP 1 MONTH                      
         MVC   SC2NDFLD(13),WORK      IF TODAY >= ENTERED DAY                   
         MVI   SC2NDLEN,13                                                      
*                                                                               
DF20     LA    R1,SC2NDFLD                                                      
         ST    R1,DMCB                                                          
         ZIC   R1,SC2NDLEN                                                      
         STC   R1,DMCB                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,,(X'60',PVOUT)                                         
         CLI   DMCB+4,X'01'                                                     
         BE    MSG10                                                            
         CLI   DMCB+4,X'03'                                                     
         BE    MSG10                                                            
         LA    R4,PVOUT                                                         
         MVC   DAYFILT,28(R4)                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER FILTER --- RECEIVER'S REPORT                                    
***********************************************************************         
*                                                                               
SENTF    NTR1                                                                   
         TM    FLAGS,FLDEST        DEST= FILTER TOO?                            
         BO    MSG15                                                            
         CLI   SC2NDLEN,3          LENGTH OF 3                                  
         BNE   SENTF20                                                          
         CLC   =C'ALL',SC2NDFLD    ALL SENDERS?                                 
         BE    SENTF50                                                          
*                                                                               
SENTF20  ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG14               NO                                           
         STC   R1,SENDLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SENDFILT(0),SC2NDFLD                                             
*                                                                               
SENTF50  MVC   IDNUM,USERNUM       IF SENT IS GIVEN DEST=USERID                 
         BAS   RE,ALPHANME                                                      
         MVC   DESTFILT(8),NAME                                                 
         MVI   DESTLEN,7                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DISK ADDRESS FILTER FOR EASY LOCATION                                  
***********************************************************************         
*                                                                               
DAF      NTR1                                                                   
         CLI   DDSTERM,C'Y'                                                     
         BNE   DAFX                                                             
         CLI   SC2NDLEN,0                                                       
         BE    MSG21                                                            
         CLI   SC2NDLEN,8          LEN=8 FOR DISK ADDR                          
         BNE   MSG21                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,SC2NDFLD,DAFILT,8                                      
         CLI   DMCB+15,0                                                        
         BE    MSG21                                                            
         CLI   DAFILT+2,X'03'                                                   
         BH    MSG21                                                            
         CLI   DAFILT+2,0                                                       
         BH    DAF20                                                            
         MVI   DAFILT+2,X'01'                                                   
DAF20    CLI   DAFILT+3,0                                                       
         BE    DAFX                                                             
         ZIC   R1,DAFILT+3                                                      
         BCTR  R1,0                                                             
         STC   R1,DAFILT+3                                                      
DAFX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        AGENCY FILTER FOR DDS TERMINALS ONLY                                   
***********************************************************************         
*                                                                               
AGYF     NTR1                                                                   
*                                                                               
         CLI   SC2NDLEN,3          =ALL                                         
         BNE   AGYF10                                                           
         CLC   =C'ALL',SC2NDFLD                                                 
         BNE   AGYF10                                                           
         CLI   DDSTERM,C'Y'                                                     
         BNE   MSG20                                                            
         MVI   DISPALL,C'Y'                                                     
         B     AGYFX                                                            
*                                                                               
AGYF10   ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG20                                                            
         STC   R1,AGYLEN                                                        
         MVC   AGYFILT,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AGYFILT(0),SC2NDFLD                                              
*                                                                               
         CLI   DDSTERM,C'Y'        SKIP VALIDATING COMPATIBLE ID                
         BE    AGYF50                                                           
         OC    USERNUM,USERNUM                                                  
         BZ    MSG27               MUST BE CONNECTED                            
*                                                                               
         XC    KEY,KEY             GET USERS ID REC                             
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(L'USERNUM),USERNUM                                        
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO,0                     
         CLC   KEY,IO                                                           
         BNE   MSG22                                                            
         LA    R2,IO+28                                                         
AGYF20   CLI   0(R2),0                                                          
         BE    MSG25                                                            
         CLI   0(R2),X'07'         ID DESCRIPTION ELEM                          
         BE    AGYF30                                                           
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     AGYF20                                                           
AGYF30   TM    2(R2),X'01'         MUST BE PRIVILEDGED USER                     
         BNO   MSG25                                                            
*                                                                               
         L     R2,VCOMFACS         SET A(DATAMGR) FOR GETIDS                    
         L     R2,CDATAMGR-COMFACSD(R2)                                         
         L     RF,=V(GETIDS)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(C'C',IO),0,(C'A',(R2)),AGYFILT                        
         CLI   DMCB+12,X'00'                                                    
         BE    MSG26               NO MATCH                                     
*                                                                               
AGYF50   XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),AGYFILT                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO,0                     
         CLC   KEY,IO                                                           
         BNE   MSG22                                                            
         LA    R4,IO                                                            
         MVI   ELCODE,2            PASSIVE POINTER ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   MSG22                                                            
         MVC   AGYFNUM,2(R4)       SENDERS ALPHA ID                             
AGYFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DDS FILTER FOR DDS TERMINALS ONLY                                      
***********************************************************************         
*                                                                               
DDSF     NTR1                                                                   
         CLI   DDSTERM,C'Y'                                                     
         BNE   DDSFX                                                            
         CLI   SC2NDLEN,0                                                       
         BE    MSG20                                                            
         CLC   =C'ALL',SC2NDFLD                                                 
         BNE   DDSFX                                                            
         MVI   DISPALL,C'Y'                                                     
DDSFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT QUEUE SUB ID FILTER                                              
***********************************************************************         
*                                                                               
PQSUBF NTR1                                                                     
         MVC   PQSUBFLT,SC2NDFLD                                                
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        PRINT QUEUE REFERENCE NUMBER FILER                                     
***********************************************************************         
*                                                                               
PQREFF   NTR1                                                                   
         TM    SC2NDVAL,SCNUMQ     VALID NUMERIC                                
         BZ    MSG16                                                            
         ZIC   R1,SC2NDLEN         LENGTH                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SC2NDFLD(0)                                               
         CVB   R4,DUB                                                           
         STH   R4,PQREFILT                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CLIENT FILTER                                                          
***********************************************************************         
*                                                                               
CLTF     NTR1                                                                   
         CLI   SC2NDLEN,2                                                       
         BL    MSG24                                                            
         CLI   SC2NDLEN,3                                                       
         BH    MSG24                                                            
         MVC   CLTFILT,SC2NDFLD                                                 
         OC    CLTFILT,SPACES                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR SUMMARY OPTIONS                                              
***********************************************************************         
*                                                                               
VALSUMM  NTR1                                                                   
         LA    R2,EDSP2H                                                        
         NI    FLAGS,X'FF'-FLSUMM                                               
         XC    DAYFILT,DAYFILT                                                  
*                                                                               
         CLI   PFKEY,11            SUMMARY OPTION                               
         BNE   *+8                                                              
         OI    FLAGS,FLSUMM                                                     
         CLI   5(R2),0             ANY INPUT IN OPTIONS FIELD                   
         BE    VSUMX                                                            
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(16,(R2)),SCANBOX                                      
         CLI   DMCB+4,X'00'                                                     
         BE    MSG11                                                            
         LA    R2,SCANBOX                                                       
*                                                                               
VSUM10   ZIC   R1,SC1STLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    VSUMX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'DATE'                                             
         BNE   VSUM20                                                           
         BAS   RE,DATEF                                                         
         LA    R2,SCBLKLQ+6(R2)                                                 
*                                                                               
VSUM20   ZIC   R1,SC1STLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    VSUMX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'SUMMARY'                                          
         BNE   *+8                                                              
         OI    FLAGS,FLSUMM                                                     
         LA    R2,SCBLKLQ+6(R2)                                                 
         B     VSUM10                                                           
*                                                                               
VSUMX    TM    FLAGS,FLSUMM        ACTION SUMMARY?                              
         BNO   EXIT                                                             
         XC    EDSP1,EDSP1         THEN NO SYSTEM                               
         MVI   EDSP1H+5,0                                                       
         XC    EDSP2,EDSP2         ONLY SUMMARY AND DATE OPTIONS USED           
         LA    R2,EDSP2                                                         
         MVC   0(7,R2),=C'SUMMARY'                                              
         LA    R2,7(R2)                                                         
         MVI   EDSP2H+5,7                                                       
         OC    DAYFILT,DAYFILT                                                  
         BZ    EXIT                                                             
         MVC   0(6,R2),=C',DATE='                                               
         LA    R2,6(R2)                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DAYFILT),(11,0(R2))                                 
         MVI   EDSP2H+5,21                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        MESSAGE EXITS                                                          
***********************************************************************         
*                                                                               
MSG6     MVC   EDSMSG(19),=C'PLEASE ENTER SYSTEM'                               
         B     MSGP1X                                                           
MSG7     MVC   EDSMSG(14),=C'INVALID SYSTEM'                                    
         B     MSGP1X                                                           
MSG10    MVC   EDSMSG(19),=C'INVALID DATE FILTER'                               
         B     MSGP2X                                                           
MSG11    MVC   EDSMSG(22),=C'INVALID OPTIONS FORMAT'                            
         B     MSGP2X                                                           
MSG12    MVC   EDSMSG+5(21),=C'IS NOT A VALID STATUS'                           
         B     MSGP2X                                                           
MSG13    MVC   EDSMSG(22),=C'MUST SPECIFY DEST=NAME'                            
         B     MSGP2X                                                           
MSG14    MVC   EDSMSG(26),=C'MUST SPECIFY SENT=NAME/ALL'                        
         B     MSGP2X                                                           
MSG15    MVC   EDSMSG(34),=C'CAN''T SPECIFY BOTH DEST= AND SENT='               
         B     MSGP2X                                                           
MSG16    MVC   EDSMSG(30),=C'PQREF REQUIRES A VALID NUMERIC'                    
         B     MSGP2X                                                           
MSG17    MVC   EDSMSG+6(21),=C'IS NOT A VALID FILTER'                           
         B     MSGP2X                                                           
MSG18    MVC   EDSMSG+2(19),=C'IS NOT A VALID TYPE'                             
         B     MSGP2X                                                           
MSG19    MVC   EDSMSG+2(21),=C'IS NOT A VALID METHOD'                           
         B     MSGP2X                                                           
MSG20    MVC   EDSMSG(14),=C'INVALID OPTION'                                    
         B     MSGP2X                                                           
MSG21    MVC   EDSMSG(32),=C'THAT''S A BAD DISK ADDRESS, DAVID'                 
         B     MSGP2X                                                           
MSG22    MVC   EDSMSG(19),=C'INVALID AGENCY NAME'                               
         B     MSGP2X                                                           
MSG24    MVC   EDSMSG(37),=C'CLIENT NAME MUST BE 2 OR 3 CHARACTERS'             
         B     MSGP2X                                                           
MSG25    MVC   EDSMSG(38),=C'NOT CONNECTED WITH PRIVILEDGED USER ID'            
         B     MSGP2X                                                           
MSG26    MVC   EDSMSG(25),=C'USER ID IS NOT COMPATIBLE'                         
         B     MSGP2X                                                           
MSG27    MVC   EDSMSG(17),=C'MUST BE CONNECTED'                                 
         B     MSGP2X                                                           
*                                                                               
MSGP1X   OI    EDSP1H+6,X'40'      CURSOR                                       
         B     *+8                                                              
MSGP2X   OI    EDSP2H+6,X'40'      CURSOR                                       
         OI    EDSMSGH+6,X'80'     TRANSMIT                                     
         XC    OLDFILT,OLDFILT                                                  
         BAS   RE,WRITESTR                                                      
         L     RD,BASERD                                                        
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                                 
***********************************************************************         
*                                                                               
METHTAB  DC    C'E'                EASYLINK                                     
         DC    C'Q'                Q TO Q                                       
         DC    C'J'                NJE                                          
         DC    C'F'                FTP                                          
         DC    C'D'                DARE                                         
         DC    C'S'                ESS                                          
         DC    C'C'                COLUMBINE                                    
         DC    C'X'                FAXGATE                                      
         DC    X'00'                                                            
*                                                                               
OPTTABLE DC    CL8'DATE    ',AL4(DATEF)                                         
         DC    CL8'DEST    ',AL4(DESTF)                                         
         DC    CL8'SENT    ',AL4(SENTF)                                         
         DC    CL8'STAT    ',AL4(STATF)                                         
         DC    CL8'TYPE    ',AL4(TYPEF)                                         
         DC    CL8'METH    ',AL4(METHF)                                         
         DC    CL8'PQSUB   ',AL4(PQSUBF)                                        
         DC    CL8'PQREF   ',AL4(PQREFF)                                        
         DC    CL8'EZ      ',AL4(EZF)                                           
         DC    CL8'FTP     ',AL4(EZF)                                           
         DC    CL8'ELN     ',AL4(ELNF)                                          
         DC    CL8'DDS     ',AL4(DDSF)                                          
         DC    CL8'TIME    ',AL4(TIMEF)                                         
         DC    CL8'AGY     ',AL4(AGYF)                                          
         DC    CL8'USERID  ',AL4(AGYF)                                          
         DC    CL8'DSKADR  ',AL4(DAF)                                           
         DC    CL8'REQ     ',AL4(REQF)                                          
         DC    CL8'ORDER   ',AL4(ORDERF)                                        
         DC    CL8'STATION ',AL4(STATNF)                                        
         DC    CL8'STN     ',AL4(STATNF)                                        
         DC    CL8'DARE    ',AL4(DAREIF)                                        
         DC    CL8'SUMMARY ',AL4(SUMMF)                                         
         DC    CL8'I       ',AL4(DAREIF)                                        
         DC    CL8'O       ',AL4(DAREOF)                                        
         DC    CL8'CLT     ',AL4(CLTF)                                          
         DC    X'00'                                                            
*                                                                               
STTABLE  DC    AL1(EDFSSPTQ)       SPOT SYSTEM                                  
         DC    AL1(18)             # OF TYPES FOR SPOT                          
         DC    AL1(EDFTSPUQ)       O                                            
         DC    AL1(EDFTADDQ)       O                                            
         DC    AL1(EDFTBUCQ)       B                                            
         DC    AL1(EDFTREQQ)       A                                            
         DC    AL1(EDFTDMBQ)       D                                            
         DC    AL1(EDFTGOAQ)       G                                            
         DC    AL1(EDFTXCPQ)       X                                            
         DC    AL1(EDFTWRTQ)       W                                            
         DC    AL1(EDFTINVQ)       V                                            
         DC    AL1(EDFTTWXQ)       T                                            
         DC    AL1(EDFTSPGQ)       I                                            
         DC    AL1(EDFTCOVQ)       C                                            
         DC    AL1(EDFTSHIQ)       S                                            
         DC    AL1(EDFTFAXL)       F                                            
         DC    AL1(EDFTSPMQ)       M                                            
         DC    AL1(EDFTSRYQ)       Y                                            
         DC    AL1(EDFTSRXQ)       R                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSPRTQ)       PRINT SYSTEM                                 
         DC    AL1(5)              # OF TYPES FOR PRINT                         
         DC    AL1(EDFTINSQ)       I                                            
         DC    AL1(EDFTPINQ)       V                                            
         DC    AL1(EDFTPPPQ)       P                                            
         DC    AL1(EDFTPWRQ)       W                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSREPQ)       REP SYSTEM                                   
         DC    AL1(6)              # OF TYPES FOR REP                           
         DC    AL1(EDFTKWXQ)       K                                            
         DC    AL1(EDFTCONQ)       O                                            
         DC    AL1(EDFTCNEQ)       E                                            
         DC    AL1(EDFTCCCQ)       C                                            
         DC    AL1(EDFTSTNQ)       S                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSNETQ)       NET SYSTEM                                   
         DC    AL1(5)              # OF TYPES FOR NET                           
         DC    AL1(EDFTNINQ)       N                                            
         DC    AL1(EDFTCABQ)       C                                            
         DC    AL1(EDFTPAKQ)       R                                            
         DC    AL1(EDFTNRXQ)       X                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSACCQ)       ACC SYSTEM                                   
         DC    AL1(3)              # OF TYPES FOR ACC                           
         DC    AL1(EDFTORDQ)       O                                            
         DC    AL1(EDFTCHKQ)       C                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFDARRQ)       DARE SYSTEM                                  
         DC    AL1(7)              # OF TYPES FOR DARE                          
         DC    AL1(EDFTDORQ)       O                                            
         DC    AL1(EDFTDRJQ)       R                                            
         DC    AL1(EDFTDCFQ)       C                                            
         DC    AL1(EDFTDAPQ)       A                                            
         DC    AL1(EDFTDERQ)       E                                            
         DC    AL1(EDFTDMGQ)       M                                            
         DC    AL1(EDFTDNTQ)       N                                            
*                                                                               
         DC    AL1(EDFSEDIQ)       EDICT SYSTEM                                 
         DC    AL1(1)              # OF TYPES FOR EDICT                         
         DC    AL1(EDFTEDIQ)       I                                            
*                                                                               
         DC    AL1(EDFSCONQ)       CONTROL SYSTEM                               
         DC    AL1(1)              # OF TYPES                                   
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSTALQ)       TALENT SYSTEM                                
         DC    AL1(1)              # OF TYPES                                   
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FILTER CHECK                                                           
***********************************************************************         
*                                                                               
FILTCK   NMOD1 0,**FILT**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING EDFILD,R7                                                        
         CLI   EDFMON,EDFMONPQ     CONTROL REC                                  
         BE    NO                  SKIP                                         
         OC    STATFILT,STATFILT   IF STATUS FILTER - SKIP NOOP ETC             
         BNZ   FILT00                                                           
         CLI   DDSVERS,C'Y'        ETD VERSION                                  
         BE    FILT02                                                           
FILT00   CLI   EDFSTAT,EDFNOOP     SKIP THESE IN ETI                            
         BE    NO                                                               
         CLI   EDFSYS,EDFDAREQ     SKIP INCOMING DARE RECS IN ETI               
         BE    NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        AGENCY FILTER                                                          
*----------------------------------------------------------------------         
*                                                                               
FILT02   CLI   DISPALL,C'Y'        DISPLAY ALL                                  
         BE    FILT05                                                           
         TM    FLAGS,FLSENT        IS THERE A SENT FILTER                       
         BO    FILT05                                                           
*                                                                               
         OC    AGYFILT,AGYFILT                                                  
         BZ    FILT04                                                           
         CLC   AGYFNUM,EDFPQUID    DO THEY MATCH?                               
         BNE   NO                                                               
         B     FILT05                                                           
*                                                                               
FILT04   CLI   DDSVERS,C'Y'        DDS VERSION                                  
         BE    FILT05                                                           
         CLC   EDFPQUID,USERNUM    DEFAULT FILTER ON SENT=USERID                
         BNE   NO                                                               
*                                                                               
FILT05   TM    FLAGS,FLSENT        SENT=ALL FILTER?                             
         BNZ   FILT10              THEN SKIP JUNK                               
         OC    STATFILT,STATFILT   NO JUNK IF STAT FILTER                       
         BNZ   FILT10                                                           
         TM    EDFSTAT,EDFSTJNK    ALWAYS SHOW JUNK                             
         BO    FILT200                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SYSTEM FILTER                                                          
*----------------------------------------------------------------------         
*                                                                               
FILT10   TM    FLAGS,FLIDARE       INCOMING DARE ONLY                           
         BZ    FILT12                                                           
         CLI   EDFSYS,EDFDAREQ     INCOMING DARE HAS SPECIAL SYSTEM             
         BNE   NO                                                               
         CLC   DIDFILT,SPACES      DARE = DARE ID FILT                          
         BNH   FILT20                                                           
         CLC   DIDFILT,EDFDARE     MATCH ON FIRST 6 CHARS                       
         BNE   NO                                                               
         B     FILT20                                                           
*                                                                               
FILT12   OC    SYSFILT,SYSFILT     SYSTEM CHECK                                 
         BZ    FILT20                                                           
         CLC   EDFSYS,SYSFILT      DESIRED SYSTEM?                              
         BNE   NO                                                               
         SPACE 5                                                                
*----------------------------------------------------------------------         
*        DESTINATION FILTER                                                     
*----------------------------------------------------------------------         
*                                                                               
FILT20   OC    DESTFILT,DESTFILT   DESTINATION?                                 
         BZ    FILT30                                                           
         CLC   EDFDEST(6),=C'EDICT='                                            
         BE    FILT22                                                           
         ZIC   R1,DESTLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EDFDEST(0),DESTFILT                                              
         BNE   NO                                                               
         B     FILT30                                                           
FILT22   ZIC   R1,DESTLEN          FOR DDS SENT REPORTS                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EDFDEST+6(0),DESTFILT                                            
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SENDER FILTER                                                          
*----------------------------------------------------------------------         
*                                                                               
FILT30   OC    SENDFILT,SENDFILT   IS THERE A SENDER FILTER                     
         BZ    FILT35              NO                                           
         MVC   IDNUM,EDFPQUID                                                   
         BAS   RE,ALPHANME         GETS ALPHA SENDER'S ID                       
         MVC   PRTSEND,NAME                                                     
         ZIC   R1,SENDLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SENDFILT(0),PRTSEND   DO THEY MATCH?                             
         BE    FILT40              YES                                          
         XC    PRTSEND,PRTSEND     DO NOT PRINT                                 
         B     NO                                                               
*                                                                               
FILT35   TM    FLAGS,FLSENT        SENT=ALL FILTER?                             
         BZ    FILT40                                                           
         MVC   IDNUM,EDFPQUID                                                   
         BAS   RE,ALPHANME         GETS ALPHA SENDER'S ID                       
         MVC   PRTSEND,NAME                                                     
         EJECT                                                                  
*----------------------------------------------------------------------         
*        STATUS FILTER                                                          
*----------------------------------------------------------------------         
*                                                                               
FILT40   OC    STATFILT,STATFILT   FITER ON STATUS?                             
         BZ    FILT50                                                           
         CLC   =C'NOTP',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT42                                                           
         TM    EDFSTAT,EDFSTPRT+EDFSTWTG                                        
         BNZ   NO                  MUST BE OFF                                  
         B     FILT48                                                           
*                                                                               
FILT42   LA    R4,STATTAB                                                       
FILT44   CLC   STATFILT,1(R4)                                                   
         BE    FILT46                                                           
FILT45   LA    R4,5(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILT44                                                           
         B     NO                                                               
*                                                                               
FILT46   MVC   WORK(1),0(R4)       CORRESPONDNG BIT                             
         NC    WORK(1),EDFSTAT                                                  
         CLC   WORK(1),0(R4)       IS BIT ON                                    
         BNE   FILT45                                                           
         CLC   STATFILT,=C'SENT'                                                
         BNE   FILT48                                                           
         TM    EDFSTAT,EDFSTRCV    NOT SENT IF DLVD TOO                         
         BO    NO                                                               
         TM    EDFSTAT,EDFSTCAN    NOT SENT IF CANCELLED                        
         BO    NO                                                               
*                                                                               
FILT48   MVC   PRTSTAT,STATFILT                                                 
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TYPE FILTER                                                            
*----------------------------------------------------------------------         
*                                                                               
FILT50   OC    TYPEFILT,TYPEFILT                                                
         BZ    FILT60                                                           
         CLC   EDFTYPE,TYPEFILT                                                 
         BNE   NO                                                               
*                                                                               
*----------------------------------------------------------------------         
*        METHOD FILTER                                                          
*----------------------------------------------------------------------         
*                                                                               
FILT60   OC    METHFILT,METHFILT                                                
         BZ    FILT70                                                           
         CLC   EDFMETH,METHFILT                                                 
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PRINT QUEUE FILTERS                                                    
*----------------------------------------------------------------------         
*                                                                               
FILT70   OC    PQSUBFLT,PQSUBFLT                                                
         BZ    FILT80                                                           
         CLC   EDFPQSUB,PQSUBFLT                                                
         BNE   NO                                                               
*                                                                               
FILT80   OC    PQREFILT,PQREFILT                                                
         BZ    FILT100                                                          
         CLC   EDFPQREF,PQREFILT                                                
         BNE   NO                                                               
*----------------------------------------------------------------------         
*        REQUESTOR FILTER                                                       
*----------------------------------------------------------------------         
*                                                                               
FILT100  OC    REQFILT,REQFILT                                                  
         BZ    FILT110                                                          
         LA    R4,REQTABLE         TABLE OF WHERE TO FIND REQS                  
FILT102  CLC   EDFSYS,0(R4)        MATCH SYSTEM                                 
         BNE   FILT104                                                          
         CLC   EDFTYPE,1(R4)       MATCH TYPE                                   
         BE    FILT106                                                          
FILT104  LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILT102                                                          
         B     NO                                                               
FILT106  ZICM  R1,2(R4),2          DISP INTO EDICT REC                          
         AR    R1,R7               R7 = A(EDICT REC)                            
         MVC   WORK(3),0(R1)                                                    
         OC    WORK(3),SPACES                                                   
         CLC   REQFILT,WORK                                                     
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ORDER NUMBER FILTER                                                    
*----------------------------------------------------------------------         
*                                                                               
FILT110  OC    ORDERFLT,ORDERFLT                                                
         BZ    FILT120                                                          
         LA    R4,ORDTABLE         TABLE OF WHERE TO FIND ORDER NUMBERS         
FILT112  CLC   EDFSYS,0(R4)        MATCH SYSTEM                                 
         BNE   FILT114                                                          
         CLC   EDFTYPE,1(R4)       MATCH TYPE                                   
         BE    FILT116                                                          
FILT114  LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILT112                                                          
         B     NO                                                               
FILT116  ZICM  R1,2(R4),2          DISP INTO EDICT REC                          
         AR    R1,R7               R7 = A(EDICT REC)                            
         CLC   ORDERFLT,0(R1)                                                   
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        STATION FILTER                                                         
*----------------------------------------------------------------------         
*                                                                               
FILT120  OC    STATNFLT,STATNFLT                                                
         BZ    FILT130                                                          
         LA    R4,STATNTAB         TABLE OF WHERE TO FIND STATIONS              
FILT122  CLC   EDFSYS,0(R4)        MATCH SYSTEM                                 
         BNE   FILT124                                                          
         CLC   EDFTYPE,1(R4)       MATCH TYPE                                   
         BE    FILT126                                                          
FILT124  LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILT122                                                          
         B     NO                                                               
FILT126  ZICM  R1,2(R4),2          DISP INTO EDICT REC                          
         AR    R1,R7               R7 = A(EDICT REC)                            
         ZIC   R5,STATNLEN                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   STATNFLT(0),0(R1)                                                
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TIME FILTER                                                            
*----------------------------------------------------------------------         
*                                                                               
FILT130  OC    TIMEFILT,TIMEFILT                                                
         BZ    FILT140                                                          
         CLI   TIMEFILT,C'*'       SHOW NOT SAME DAY DELIVERIES                 
         BNE   FILT133                                                          
*                                                                               
         TM    EDFSTAT,EDFSTRCV    WAS REPORT RECEIVED                          
         BNO   NO                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DAY),(1,DAYPWOS)                                    
         CLC   EDFRCVDY,DAYPWOS+2  SAME DAY DELIVERY                            
         BE    NO                                                               
         B     FILT140                                                          
*                                                                               
FILT133  L     RF,VCOMFACS         FILTER ON SENT TIME                          
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFSNTIM,WORK,2                                        
         OC    WORK(5),SPACES                                                   
         CLC   TIMEFILT,WORK                                                    
         BH    NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CLIENT FILTER                                                          
*----------------------------------------------------------------------         
*                                                                               
FILT140  OC    CLTFILT,CLTFILT                                                  
         BZ    FILT200                                                          
         LA    R4,CLTTABLE         TABLE OF WHERE TO FIND THE CLIENT            
FILT142  CLC   EDFSYS,0(R4)        MATCH SYSTEM                                 
         BNE   FILT144                                                          
         CLC   EDFTYPE,1(R4)       MATCH TYPE                                   
         BE    FILT146                                                          
FILT144  LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILT142                                                          
         B     NO                                                               
FILT146  ZICM  R1,2(R4),2          DISP INTO EDICT REC                          
         AR    R1,R7               R7 = A(EDICT REC)                            
         CLC   CLTFILT,0(R1)                                                    
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        COUNT                                                                  
*----------------------------------------------------------------------         
*                                                                               
FILT200  ZIC   R1,BACKCNT          COUNT OF HOW MANY PASSED FILTER              
         LA    R1,1(R1)                                                         
         STC   R1,BACKCNT                                                       
         B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                                 
***********************************************************************         
*                                                                               
REQTABLE DC    AL1(EDFSSPTQ,EDFTXCPQ),AL2(SPCPRQST-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTREQQ),AL2(SPAVBUYR-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTADDQ),AL2(SPEDBYR-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTDMBQ),AL2(SPDABYR-EDFILD)                       
         DC    AL1(EDFSREPQ,EDFTKWXQ),AL2(EDIRKXBC+2-EDFILD)                    
         DC    AL1(EDFSREPQ,EDFTCONQ),AL2(EDIRCNSP-EDFILD)                      
         DC    AL1(EDFSREPQ,EDFTCNEQ),AL2(EDIRCNSP-EDFILD)                      
         DC    AL1(EDFSREPQ,EDFTCCCQ),AL2(EDIRCNSP-EDFILD)                      
         DC    AL1(EDFSPRTQ,EDFTINSQ),AL2(PPEDREQ-EDFILD)                       
         DC    AL1(EDFSPRTQ,EDFTPPPQ),AL2(PPEDREQ-EDFILD)                       
         DC    AL1(EDFSPRTQ,EDFTPINQ),AL2(PPEDREQ-EDFILD)                       
         DC    AL1(EDFSNETQ,EDFTPAKQ),AL2(NEEDREQ-EDFILD)                       
         DC    AL1(EDFDARRQ,EDFTDORQ),AL2(EDIRDRBY-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDRJQ),AL2(EDIRDRSP-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDCFQ),AL2(EDIRDRSP-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDAPQ),AL2(EDIRDRSP-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDERQ),AL2(EDIRDRBY-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDNTQ),AL2(EDIRDRBY-EDFILD)                      
         DC    X'00'                                                            
*                                                                               
ORDTABLE DC    AL1(EDFDAREQ,0),AL2(EDFDARE+6-EDFILD)                            
         DC    AL1(EDFDARRQ,EDFTDORQ),AL2(EDIRDRAN-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDRJQ),AL2(EDIRDRAN-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDCFQ),AL2(EDIRDRAN-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDAPQ),AL2(EDIRDRAN-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDERQ),AL2(EDIRDRAN-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDNTQ),AL2(EDIRDRAN-EDFILD)                      
         DC    X'00'                                                            
*                                                                               
STATNTAB DC    AL1(EDFDARRQ,EDFTDORQ),AL2(EDIRDRST-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDRJQ),AL2(EDIRDRST-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDCFQ),AL2(EDIRDRST-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDAPQ),AL2(EDIRDRST-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDERQ),AL2(EDIRDRST-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDNTQ),AL2(EDIRDRST-EDFILD)                      
         DC    AL1(EDFSREPQ,EDFTCONQ),AL2(EDIRCNST-EDFILD)                      
         DC    AL1(EDFSREPQ,EDFTCNEQ),AL2(EDIRCNST-EDFILD)                      
         DC    AL1(EDFSREPQ,EDFTCCCQ),AL2(EDIRCNST-EDFILD)                      
         DC    AL1(EDFSREPQ,EDFTKWXQ),AL2(EDIRKXST-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTADDQ),AL2(SPEDSTA-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTDMBQ),AL2(SPDASTA-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTXCPQ),AL2(SPCPSTA-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTINVQ),AL2(SPNVSTA-EDFILD)                       
         DC    X'00'                                                            
*                                                                               
CLTTABLE DC    AL1(EDFSSPTQ,EDFTADDQ),AL2(SPEDCLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTREQQ),AL2(SPAVCLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTDMBQ),AL2(SPDACLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTGOAQ),AL2(SPGTCLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTINVQ),AL2(SPNVCLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTTWXQ),AL2(EDISTTCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTSPUQ),AL2(EDISTTCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTSPGQ),AL2(EDISTTCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTFAXL),AL2(EDISTTCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTCOVQ),AL2(EDISTCCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTSPMQ),AL2(EDISTCCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTSHIQ),AL2(EDISTSCL-EDFILD)                      
         DC    AL1(EDFSSPTQ,EDFTXCPQ),AL2(SPCPCLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTSRYQ),AL2(SPCPCLT-EDFILD)                       
         DC    AL1(EDFSSPTQ,EDFTSRXQ),AL2(SPCPCLT-EDFILD)                       
         DC    AL1(EDFSNETQ,EDFTNINQ),AL2(EDINTNCL-EDFILD)                      
         DC    AL1(EDFSNETQ,EDFTCABQ),AL2(EDINTCCL-EDFILD)                      
         DC    AL1(EDFSNETQ,EDFTPAKQ),AL2(NEEDCLT-EDFILD)                       
         DC    AL1(EDFSNETQ,EDFTNRXQ),AL2(SPCPCLT-EDFILD)                       
         DC    AL1(EDFSPRTQ,EDFTINSQ),AL2(PPEDCLT-EDFILD)                       
         DC    AL1(EDFSPRTQ,EDFTPPPQ),AL2(PPEDCLT-EDFILD)                       
         DC    AL1(EDFSPRTQ,EDFTPINQ),AL2(PPEDCLT-EDFILD)                       
         DC    AL1(EDFSACCQ,EDFTORDQ),AL2(ACFXCLI-EDFILD)                       
         DC    AL1(EDFDARRQ,EDFTDAPQ),AL2(EDIRDRCL-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDCFQ),AL2(EDIRDRCL-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDERQ),AL2(EDIRDRCL-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDNTQ),AL2(EDIRDRCL-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDORQ),AL2(EDIRDRCL-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDRJQ),AL2(EDIRDRCL-EDFILD)                      
         DC    AL1(EDFDARRQ,EDFTDMGQ),AL2(EDIRDRCL-EDFILD)                      
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SUMMARY OPTION                                                         
***********************************************************************         
*                                                                               
         DS    0H                                                               
SUMMARY  NMOD1 0,**SUMM**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    RECSUSED,RECSUSED   CLEAR OUT NUM OF RECS USED FOR DAY           
         LA    R0,TOTALSQ          CLEAR TOTAL ACCUMULATORS                     
         LA    R1,TOTALS                                                        
         ZAP   0(L'TOTALS,R1),=P'0'                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LH    R2,FIRSTRAC         READ EDICT FILE                              
SUMM10   STH   R2,EDCTFDSK         TRACK NUMBER                                 
         STH   R2,LASTRAC                                                       
*                                                                               
         LA    R2,1                                                             
SUMM20   STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         STC   R2,LASTBLK                                                       
         EJECT                                                                  
***********************************************************************         
*        READ BLOCK                                                             
***********************************************************************         
*                                                                               
         L     R2,EDICTFL                                                       
         LA    R5,EDCTFDSK                                                      
         LA    R4,EDICTBLK                                                      
         GOTO1 VDADDS,DMCB,RDID,(R4),0,(R2),(R5),0                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDFILD,R4                                                        
         LA    R4,EDICTBLK         R4-POINTER IN BLOCK                          
         ST    R4,AEDCTBLK                                                      
         LA    R1,1                                                             
         STH   R1,RECNUM                                                        
*                                                                               
SUMM40   CLI   EDFMON,EDFMONPQ     CONTROL REC                                  
         BE    SUMMNX              SKIP                                         
         CLC   EDFMON,BMONTH       IS THIS RECORD FROM THIS MONTH?              
         BNE   SUMM100             NO -- WE'VE FOUND THE EOF                    
         L     R1,RECSUSED         INCREMENT NUMBER OF RECS FOR DAY             
         LA    R1,1(R1)                                                         
         ST    R1,RECSUSED                                                      
         CLI   EDFSYS,EDFDAREQ     SKIP THESE                                   
         BE    SUMMNX                                                           
         CLI   DDSVERS,C'Y'        DDS VERSION                                  
         BE    SUMM50                                                           
         CLC   EDFPQUID,USERNUM    DEFAULT FILTER ON SENT=USERID                
         BNE   SUMMNX                                                           
         EJECT                                                                  
***********************************************************************         
*        ADD TO TOTALS                                                          
***********************************************************************         
*                                                                               
SUMM50   DS    0H                                                               
         USING TOTTABD,R2                                                       
         LA    R2,TOTTAB           TOTAL TABLE                                  
*                                                                               
SUMM55   CLC   EDFMETH,TOTMETH     MATCH ON METHOD                              
         BNE   SUMM70                                                           
         MVC   BYTE,EDFSTAT        MATCH ON STATUS                              
         NC    BYTE,TOTSTAT                                                     
         CLC   BYTE,TOTSTAT        IS BIT ON                                    
         BNE   SUMM70                                                           
*                                                                               
         LA    R0,TOTALS                                                        
         ZICM  R1,TOTMSTOT,2       ADD 1 TO METH/STAT TOT                       
         AR    R1,R0                                                            
         AP    0(4,R1),=P'1'                                                    
         ZICM  R1,TOTMTOT,2        ADD 1 TO METH TOT                            
         AR    R1,R0                                                            
         AP    0(4,R1),=P'1'                                                    
         ZICM  R1,TOTSTOT,2        ADD 1 TO STAT TOT                            
         AR    R1,R0                                                            
         AP    0(4,R1),=P'1'                                                    
         ZICM  R1,TOTOTAL,2        ADD 1 TO TOTALS                              
         AR    R1,R0                                                            
         AP    0(4,R1),=P'1'                                                    
         B     SUMM80                                                           
*                                                                               
SUMM70   LA    R2,TOTLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   SUMM55                                                           
*                                                                               
SUMM80   LA    R2,DARETOTS         SPECIAL TOTALS                               
SUMM81   CLC   EDFMETH,TOTMETH     MATCH ON METHOD                              
         BNE   SUMM85                                                           
         MVC   BYTE,EDFSTAT        MATCH ON STATUS                              
         NC    BYTE,TOTSTAT                                                     
         CLC   BYTE,TOTSTAT        IS BIT ON                                    
         BNE   SUMM85                                                           
*                                                                               
         CLI   EDFTYPE,EDFTDORQ    SPECIAL DARE ORDERS TOTAL                    
         BNE   SUMMNX                                                           
         LA    R0,TOTALS                                                        
         ZICM  R1,TOTMSTOT,2       ADD 1 TO METH/STAT TOT                       
         AR    R1,R0                                                            
         AP    0(4,R1),=P'1'                                                    
         ZICM  R1,TOTMTOT,2        ADD 1 TO METH TOT                            
         AR    R1,R0                                                            
         AP    0(4,R1),=P'1'                                                    
         B     SUMMNX                                                           
*                                                                               
SUMM85   LA    R2,TOTLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   SUMM81                                                           
         EJECT                                                                  
***********************************************************************         
*        GET NEXT RECORD                                                        
***********************************************************************         
*                                                                               
SUMMNX   AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LH    R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,RECNUM                                                        
         CH    R1,EDCTRPB          ANY MORE RECORDS IN THIS BLOCK?              
         BNH   SUMM40                                                           
*                                                                               
         ZIC   R2,LASTBLK                                                       
         LA    R2,1(R2)            NO                                           
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   SUMM20              YES                                          
*                                                                               
         LH    R2,LASTRAC                                                       
         LA    R2,1(R2)                                                         
         CH    R2,EDCTFLST                                                      
         BNH   SUMM10                                                           
         DC    H'0'                ALL TRACKS FOR TODAY ARE USED                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY TOTALS                                                         
***********************************************************************         
*                                                                               
         USING EDSUMMD,R6                                                       
SUMM100  LA    R6,EDSLN1H          R6-POINTER FOR SCREEN LINE                   
         BAS   RE,SUMCOLS                                                       
*                                                                               
         USING PRNTOTD,R2                                                       
         LA    R2,PRNTOT           PRINT TOTAL TABLE                            
         B     *+8                                                              
*                                                                               
SUMM120  LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
SUMM122  CLC   PRNAME(6),=C'TOTALS'   TOTALS LINE                               
         BNE   SUMM123                                                          
         MVC   SUMWTNG(L'SUMWTNG+1),=C'======'                                  
         MVC   SUMDLVD(L'SUMDLVD+1),=C'======'                                  
         MVC   SUMSENT(L'SUMSENT+1),=C'======'                                  
         MVC   SUMCANX(L'SUMCANX+1),=C'======'                                  
         MVC   SUMERR(L'SUMERR+1),=C'======'                                    
         MVC   SUMTOT(L'SUMTOT),=C'======'                                      
         LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
         B     SUMM127                                                          
*                                                                               
SUMM123  CLC   PRNAME(10),=C'DARE ORDER'                                        
         BNE   SUMM125                                                          
         CLI   DDSVERS,C'Y'        DDS VERSION                                  
         BNE   SUMM130                                                          
         LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
         B     SUMM127                                                          
*                                                                               
SUMM125  CLI   DDSVERS,C'Y'        IF NOT DDS VERSION                           
         BE    SUMM127                                                          
         ZICM  R5,PRTOT,2          SKIP LINE IF ALL ZEROS                       
         LA    R4,TOTALS                                                        
         AR    R5,R4                                                            
         CP    0(4,R5),=P'0'                                                    
         BNE   SUMM127                                                          
         LA    R2,PRNTLNQ(R2)                                                   
         B     SUMM122                                                          
*                                                                               
SUMM127  MVC   SUMROW,PRNAME       METHOD NAME                                  
         LA    R4,TOTALS                                                        
*                                                                               
         ZICM  R5,PRWTNG,2                                                      
         AR    R5,R4                                                            
         LA    R7,SUMWTNG                                                       
         BAS   RE,EDITOT           EDIT OUT WTNG TOTAL                          
*                                                                               
         ZICM  R5,PRDLVD,2                                                      
         AR    R5,R4                                                            
         LA    R7,SUMDLVD                                                       
         BAS   RE,EDITOT           EDIT OUT DLVD TOTAL                          
*                                                                               
         ZICM  R5,PRSENT,2                                                      
         AR    R5,R4                                                            
         LA    R7,SUMSENT                                                       
         BAS   RE,EDITOT           EDIT OUT SENT TOTAL                          
*                                                                               
         ZICM  R5,PRCANX,2                                                      
         AR    R5,R4                                                            
         LA    R7,SUMCANX                                                       
         BAS   RE,EDITOT           EDIT OUT CANX TOTAL                          
*                                                                               
         ZICM  R5,PRERR,2                                                       
         AR    R5,R4                                                            
         LA    R7,SUMERR                                                        
         BAS   RE,EDITOT           EDIT OUT ERR TOTAL                           
*                                                                               
         ZICM  R5,PRTOT,2                                                       
         AR    R5,R4                                                            
         LA    R7,SUMTOT                                                        
         BAS   RE,EDITOT           EDIT OUT METHOD TOTAL                        
*                                                                               
SUMM130  LA    R2,PRNTLNQ(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         BNE   SUMM120                                                          
*                                                                               
         LH    R1,EDCTFTPD         TRACKS PER DAY....                           
         MH    R1,EDCTFRPT         ....TIMES BLOCKS PER TRACK....               
         MH    R1,EDCTRPB          ....TIMES RECORDS PER BLOCK....              
         ST    R1,RECSMAX          MAX NUMBER OF REPORTS PER DAY                
         SR    R0,R0                                                            
         L     R1,RECSUSED         NUMBER OF REPORTS TODAY                      
         MH    R1,=H'100'          TO GET PERCENTAGE                            
         D     R0,RECSMAX                                                       
         LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
         LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
         MVC   0(26,R6),=C'RECORDS USED SO FAR TODAY:'                          
         EDIT  (R1),(4,28(R6)),ZERO=NOBLANK,ALIGN=LEFT,TRAIL=C'%'               
*                                                                               
*        DISPLAY EDICT FILE STATS                                               
*                                                                               
         CLI   DDSTERM,C'Y'                                                     
         BNE   SUMMX                                                            
         LA    R6,NXTHEAD+8(R6)    NEXT LINE                                    
         MVC   0(13,R6),=C'TRACKS/DAY  ='                                       
         EDIT  (B2,EDCTFTPD),(4,15(R6)),ZERO=NOBLANK,ALIGN=LEFT                 
         MVC   22(16,R6),=C'RECORDS/BLOCK  ='                                   
         EDIT  (B2,EDCTRPB),(4,40(R6)),ZERO=NOBLANK,ALIGN=LEFT                  
*                                                                               
         LA    R6,NXTHEAD+8(R6)    NEXT LINE                                    
         MVC   0(13,R6),=C'BLOCKS/TRACK='                                       
         EDIT  (B2,EDCTFRPT),(4,15(R6)),ZERO=NOBLANK,ALIGN=LEFT                 
         MVC   22(16,R6),=C'LOGICAL REC LEN='                                   
         EDIT  (B2,EDCTFRCL),(4,40(R6)),ZERO=NOBLANK,ALIGN=LEFT                 
*                                                                               
SUMMX    MVC   EDSMSG(17),=C'SUMMARY DISPLAYED'                                 
         CH    R1,=H'65'                                                        
         BL    SUMMXX              IF LESS THAN 65%, NO PROBLEM                 
*                                                                               
         MVC   36(28,R6),=C'PLEASE EMAIL DAVID EISENBERG'                       
         SH    R6,=H'8'                   GET TO FIELD HEADER                   
         OI    1(R6),X'08'                HIGH INTENSITY                        
         LA    R6,8(R6)                   RESTORE TO NORMAL PLACE               
         MVC   EDSMSG(28),=C'PLEASE EMAIL DAVID EISENBERG'                      
         OI    EDSMSGH+1,X'08'            HIGH INTENSITY                        
SUMMXX   DS    0H                                                               
*                                                                               
         B     MSGXC                                                            
         DROP  R2,R6                                                            
*                                                                               
EDITOT   NTR1                                                                   
         EDIT  (P4,0(R5)),(5,0(R7)),ALIGN=RIGHT,ZERO=NOBLANK                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        COLUMN HEADINGS FOR SUMMARY SCREEN                                     
***********************************************************************         
*                                                                               
SUMCOLS  NTR1                                                                   
         USING EDSUMMD,R6                                                       
         LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
         LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
         LA    R6,8(R6)                                                         
         MVC   SUMWTNG,=C' WTNG'                                                
         MVC   SUMDLVD,=C' DLVD'                                                
         MVC   SUMSENT,=C' SENT'                                                
         MVC   SUMCANX,=C' CANX'                                                
         MVC   SUMERR,=C' ERR '                                                 
         MVC   SUMTOT,=C'TOTAL '                                                
         LA    R6,NXTHEAD(R6)      BUMP TO NEXT SCREEN LINE                     
         LA    R6,8(R6)                                                         
         MVC   SUMWTNG(L'SUMWTNG+1),=C'------'                                  
         MVC   SUMDLVD(L'SUMDLVD+1),=C'------'                                  
         MVC   SUMSENT(L'SUMSENT+1),=C'------'                                  
         MVC   SUMCANX(L'SUMCANX+1),=C'------'                                  
         MVC   SUMERR(L'SUMERR+1),=C'------'                                    
         MVC   SUMTOT(L'SUMTOT),=C'------'                                      
         LA    R6,NXTHEAD+8(R6)      NEXT LINE                                  
         XIT1  REGS=(R6)                                                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        TOTAL TABLE                                                            
***********************************************************************         
*                                                                               
TOTTAB   DS    0H                                                               
         DC    C'E',AL1(EDFSTJNK)                      EASYLINK                 
         DC    AL2(EERR-TOTALS,ETOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'E',AL1(EDFSTCAN)                                               
         DC    AL2(ECANX-TOTALS,ETOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'E',AL1(EDFSTRCV)                                               
         DC    AL2(EDLVD-TOTALS,ETOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'E',AL1(EDFSTSNT)                                               
         DC    AL2(ESENT-TOTALS,ETOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'E',AL1(EDFSTWTG)                                               
         DC    AL2(EWTNG-TOTALS,ETOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'F',AL1(EDFSTJNK)                      FTP                      
         DC    AL2(FERR-TOTALS,FTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'F',AL1(EDFSTCAN)                                               
         DC    AL2(FCANX-TOTALS,FTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'F',AL1(EDFSTRCV)                                               
         DC    AL2(FDLVD-TOTALS,FTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'F',AL1(EDFSTSNT)                                               
         DC    AL2(FSENT-TOTALS,FTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'F',AL1(EDFSTWTG)                                               
         DC    AL2(FWTNG-TOTALS,FTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         EJECT                                                                  
*                                                                               
         DC    C'Q',AL1(EDFSTJNK)                      Q TO Q                   
         DC    AL2(QERR-TOTALS,QTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'Q',AL1(EDFSTCAN)                                               
         DC    AL2(QCANX-TOTALS,QTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'Q',AL1(EDFSTRCV)                                               
         DC    AL2(QDLVD-TOTALS,QTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'Q',AL1(EDFSTSNT)                                               
         DC    AL2(QSENT-TOTALS,QTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'Q',AL1(EDFSTWTG)                                               
         DC    AL2(QWTNG-TOTALS,QTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'J',AL1(EDFSTJNK)                      NJE                      
         DC    AL2(JERR-TOTALS,JTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'J',AL1(EDFSTCAN)                                               
         DC    AL2(JCANX-TOTALS,JTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'J',AL1(EDFSTRCV)                                               
         DC    AL2(JDLVD-TOTALS,JTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'J',AL1(EDFSTSNT)                                               
         DC    AL2(JSENT-TOTALS,JTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'J',AL1(EDFSTWTG)                                               
         DC    AL2(JWTNG-TOTALS,JTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         EJECT                                                                  
*                                                                               
         DC    C'X',AL1(EDFSTJNK)                      FAXGATE                  
         DC    AL2(XERR-TOTALS,XTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'X',AL1(EDFSTCAN)                                               
         DC    AL2(XCANX-TOTALS,XTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'X',AL1(EDFSTRCV)                                               
         DC    AL2(XDLVD-TOTALS,XTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'X',AL1(EDFSTSNT)                                               
         DC    AL2(XSENT-TOTALS,XTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'X',AL1(EDFSTWTG)                                               
         DC    AL2(XWTNG-TOTALS,XTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'C',AL1(EDFSTJNK)                      COLUMBINE                
         DC    AL2(CERR-TOTALS,CTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'C',AL1(EDFSTCAN)                                               
         DC    AL2(CCANX-TOTALS,CTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'C',AL1(EDFSTRCV)                                               
         DC    AL2(CDLVD-TOTALS,CTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'C',AL1(EDFSTSNT)                                               
         DC    AL2(CSENT-TOTALS,CTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'C',AL1(EDFSTWTG)                                               
         DC    AL2(CWTNG-TOTALS,CTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         EJECT                                                                  
*                                                                               
         DC    C'D',AL1(EDFSTJNK)                      DARE                     
         DC    AL2(DERR-TOTALS,DTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'D',AL1(EDFSTCAN)                                               
         DC    AL2(DCANX-TOTALS,DTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'D',AL1(EDFSTRCV)                                               
         DC    AL2(DDLVD-TOTALS,DTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'D',AL1(EDFSTSNT)                                               
         DC    AL2(DSENT-TOTALS,DTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'D',AL1(EDFSTWTG)                                               
         DC    AL2(DWTNG-TOTALS,DTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    X'00',AL1(EDFSTJNK)                   UNSENDABLE METHOD          
         DC    AL2(UERR-TOTALS,UTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    X'00',AL1(EDFSTCAN)                                              
         DC    AL2(UCANX-TOTALS,UTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    X'00',AL1(EDFSTRCV)                                              
         DC    AL2(UDLVD-TOTALS,UTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    X'00',AL1(EDFSTSNT)                                              
         DC    AL2(USENT-TOTALS,UTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    X'00',AL1(EDFSTWTG)                                              
         DC    AL2(UWTNG-TOTALS,UTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    X'00',AL1(0)                         UNSENDABLE STATUS           
         DC    AL2(UERR-TOTALS,UTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
DARETOTS DC    C'D',AL1(EDFSTJNK)                      DARE ORDERS              
         DC    AL2(DOERR-TOTALS,DOTOT-TOTALS)                                   
         DC    AL2(0,0)                                                         
         DC    C'D',AL1(EDFSTCAN)                                               
         DC    AL2(DOCANX-TOTALS,DOTOT-TOTALS)                                  
         DC    AL2(0,0)                                                         
         DC    C'D',AL1(EDFSTRCV)                                               
         DC    AL2(DODLVD-TOTALS,DOTOT-TOTALS)                                  
         DC    AL2(0,0)                                                         
         DC    C'D',AL1(EDFSTSNT)                                               
         DC    AL2(DOSENT-TOTALS,DOTOT-TOTALS)                                  
         DC    AL2(0,0)                                                         
         DC    C'D',AL1(EDFSTWTG)                                               
         DC    AL2(DOWTNG-TOTALS,DOTOT-TOTALS)                                  
         DC    AL2(0,0)                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        PRINT TOTALS TABLE                                                     
***********************************************************************         
*                                                                               
PRNTOT   DS    0H                                                               
         DC    CL15'EASYLINK'                                                   
         DC    AL2(EWTNG-TOTALS,EDLVD-TOTALS,ESENT-TOTALS)                      
         DC    AL2(ECANX-TOTALS,EERR-TOTALS,ETOT-TOTALS)                        
*                                                                               
         DC    CL15'FTP'                                                        
         DC    AL2(FWTNG-TOTALS,FDLVD-TOTALS,FSENT-TOTALS)                      
         DC    AL2(FCANX-TOTALS,FERR-TOTALS,FTOT-TOTALS)                        
*                                                                               
         DC    CL15'Q TO Q'                                                     
         DC    AL2(QWTNG-TOTALS,QDLVD-TOTALS,QSENT-TOTALS)                      
         DC    AL2(QCANX-TOTALS,QERR-TOTALS,QTOT-TOTALS)                        
*                                                                               
         DC    CL15'NJE'                                                        
         DC    AL2(JWTNG-TOTALS,JDLVD-TOTALS,JSENT-TOTALS)                      
         DC    AL2(JCANX-TOTALS,JERR-TOTALS,JTOT-TOTALS)                        
*                                                                               
         DC    CL15'FAXGATE'                                                    
         DC    AL2(XWTNG-TOTALS,XDLVD-TOTALS,XSENT-TOTALS)                      
         DC    AL2(XCANX-TOTALS,XERR-TOTALS,XTOT-TOTALS)                        
*                                                                               
         DC    CL15'COLUMBINE'                                                  
         DC    AL2(CWTNG-TOTALS,CDLVD-TOTALS,CSENT-TOTALS)                      
         DC    AL2(CCANX-TOTALS,CERR-TOTALS,CTOT-TOTALS)                        
*                                                                               
*        DC    CL15'ESS'                                                        
*        DC    AL2(SWTNG-TOTALS,SDLVD-TOTALS,SSENT-TOTALS)                      
*        DC    AL2(SCANX-TOTALS,SERR-TOTALS,STOT-TOTALS)                        
*                                                                               
         DC    CL15'DARE'                                                       
         DC    AL2(DWTNG-TOTALS,DDLVD-TOTALS,DSENT-TOTALS)                      
         DC    AL2(DCANX-TOTALS,DERR-TOTALS,DTOT-TOTALS)                        
*                                                                               
         DC    CL15'UNSENDABLE'                                                 
         DC    AL2(UWTNG-TOTALS,UDLVD-TOTALS,USENT-TOTALS)                      
         DC    AL2(UCANX-TOTALS,UERR-TOTALS,UTOT-TOTALS)                        
*                                                                               
         DC    CL15'TOTALS'                                                     
         DC    AL2(WTNGTOT-TOTALS,DLVDTOT-TOTALS,SENTTOT-TOTALS)                
         DC    AL2(CANXTOT-TOTALS,ERRTOT-TOTALS,TOTTOT-TOTALS)                  
*                                                                               
         DC    CL15'DARE ORDERS'                                                
         DC    AL2(DOWTNG-TOTALS,DODLVD-TOTALS,DOSENT-TOTALS)                   
         DC    AL2(DOCANX-TOTALS,DOERR-TOTALS,DOTOT-TOTALS)                     
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OPTTABLE DSECT                                                         
***********************************************************************         
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                 OPTION KEYWORD                               
OPTROUT  DS    XL4                 ROUTINE TO VALIDATE OPTION                   
OPTTLNQ  EQU   *-OPTTABD                                                        
*                                                                               
***********************************************************************         
*        TOTTAB DSECT                                                           
***********************************************************************         
*                                                                               
TOTTABD  DSECT                                                                  
TOTMETH  DS    CL1                 METHOD                                       
TOTSTAT  DS    XL1                 STATUS                                       
TOTMSTOT DS    XL2                 DISP TO METHOD/STATUS TOTAL                  
TOTMTOT  DS    XL2                 DISP TO METHOD TOTAL                         
TOTSTOT  DS    XL2                 DISP TO STATUS TOTAL                         
TOTOTAL  DS    XL2                 DISP TO TOTAL TOTAL                          
TOTLNQ   EQU   *-TOTTABD                                                        
*                                                                               
***********************************************************************         
*        PRNTOT DSECT                                                           
***********************************************************************         
*                                                                               
PRNTOTD  DSECT                                                                  
PRNAME   DS    CL15                NAME OF METHOD                               
PRWTNG   DS    XL2                 DISP TO WTNG TOTAL                           
PRDLVD   DS    XL2                 DISP TO DLVD TOTAL                           
PRSENT   DS    XL2                 DISP TO SENT TOTAL                           
PRCANX   DS    XL2                 DISP TO CANX TOTAL                           
PRERR    DS    XL2                 DISP TO ERR  TOTAL                           
PRTOT    DS    XL2                 DISP TO TOT FOR MTHOD                        
PRNTLNQ  EQU   *-PRNTOTD                                                        
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BASERD   DS    A                                                                
DMCB     DS    8F                                                               
*                                                                               
EDICTFL  DS    A                   A(EDICT FILE)                                
DSKAD    DS    F                   LAST PAGE'S LAST DSK ADDRESS                 
FDSKAD   DS    F                   LAST PAGE'S FIRST DISK ADDRESS               
DUMMY    DS    F                                                                
RECSUSED DS    F                   NUMBER OF RECORDS USED SO FAR TODAY          
RECSMAX  DS    F                   TOTAL NUMBER OF RECORDS FOR DAY              
AEDCTBLK DS    F                   A(EDICT BLOCK)                               
EDCTFDSK DC    F'0'                EDICTFIL DISK ADDRESS                        
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTFRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
EDCTRPB  DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
FIRSTRAC DS    H                   FIRST TRAC FOR GIVEN DAY                     
LASTRAC  DS    H                                                                
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
DDSTERM  DS    CL1                 DDS TERMINAL (Y/N)                           
DDSVERS  DS    CL1                 DDS VERSION (Y/N)                            
BMONTH   DS    XL1                 MONTH IN BINARY                              
SYSID    DS    XL1                 SYSTEM ID (REP/ADV/TEST/MEL)                 
RECNUM   DS    XL1                 # OF RECORDS INTO BLOCK                      
RECSKIP  DS    XL1                 # OF RECORDS TO BUMB INTO BLOCK              
PFKEY    DS    XL1                 PRETTY OBVIOUS                               
BACKCNT  DS    XL1                 BACKWARDS COUNT FOR PAGING UP                
LASTBLK  DS    XL1                 LAST BLOCK NUMBER                            
*                                                                               
FLAG     DS    XL1                                                              
FLFIRST  EQU   X'80'               X'80' FIRST TIME THROUGH                     
SKIPBUMP EQU   X'40'               X'40' SKIP ANY MORE BUMPS                    
BOTTOM   EQU   X'20'               USER HIT PF TO BOTTOM                        
*                                                                               
TOTALS   DS    0PL4                SUMMARY TOTALS                               
ETOT     DS    PL4                 EASYLINK                                     
EWTNG    DS    PL4                                                              
EDLVD    DS    PL4                                                              
ESENT    DS    PL4                                                              
ECANX    DS    PL4                                                              
EERR     DS    PL4                                                              
*                                                                               
FTOT     DS    PL4                 FTP                                          
FWTNG    DS    PL4                                                              
FDLVD    DS    PL4                                                              
FSENT    DS    PL4                                                              
FCANX    DS    PL4                                                              
FERR     DS    PL4                                                              
*                                                                               
QTOT     DS    PL4                 Q TO Q                                       
QWTNG    DS    PL4                                                              
QDLVD    DS    PL4                                                              
QSENT    DS    PL4                                                              
QCANX    DS    PL4                                                              
QERR     DS    PL4                                                              
*                                                                               
JTOT     DS    PL4                 NJE                                          
JWTNG    DS    PL4                                                              
JDLVD    DS    PL4                                                              
JSENT    DS    PL4                                                              
JCANX    DS    PL4                                                              
JERR     DS    PL4                                                              
*                                                                               
XTOT     DS    PL4                 FAXGATE                                      
XWTNG    DS    PL4                                                              
XDLVD    DS    PL4                                                              
XSENT    DS    PL4                                                              
XCANX    DS    PL4                                                              
XERR     DS    PL4                                                              
*                                                                               
CTOT     DS    PL4                 COLUMBINE                                    
CWTNG    DS    PL4                                                              
CDLVD    DS    PL4                                                              
CSENT    DS    PL4                                                              
CCANX    DS    PL4                                                              
CERR     DS    PL4                                                              
*                                                                               
STOT     DS    PL4                 ESS                                          
SWTNG    DS    PL4                                                              
SDLVD    DS    PL4                                                              
SSENT    DS    PL4                                                              
SCANX    DS    PL4                                                              
SERR     DS    PL4                                                              
*                                                                               
DTOT     DS    PL4                 DARE                                         
DWTNG    DS    PL4                                                              
DDLVD    DS    PL4                                                              
DSENT    DS    PL4                                                              
DCANX    DS    PL4                                                              
DERR     DS    PL4                                                              
*                                                                               
DOTOT    DS    PL4                 DARE ORDERS                                  
DOWTNG   DS    PL4                                                              
DODLVD   DS    PL4                                                              
DOSENT   DS    PL4                                                              
DOCANX   DS    PL4                                                              
DOERR    DS    PL4                                                              
*                                                                               
UTOT     DS    PL4                 UNSENDABLE METHOD                            
UWTNG    DS    PL4                                                              
UDLVD    DS    PL4                                                              
USENT    DS    PL4                                                              
UCANX    DS    PL4                                                              
UERR     DS    PL4                                                              
*                                                                               
WTNGTOT  DS    PL4                                                              
DLVDTOT  DS    PL4                                                              
SENTTOT  DS    PL4                                                              
CANXTOT  DS    PL4                                                              
ERRTOT   DS    PL4                                                              
TOTTOT   DS    PL4                                                              
TOTALSQ  EQU   (*-TOTALS)/L'TOTALS                                              
BYTE     DS    XL1                                                              
STAT     DS    XL1                                                              
STNOZERO EQU   X'80'               THERE IS A NON 0 SO PRINT LINE               
*                                                                               
PRTSEND  DS    XL8                 DO I WANT TO PRINT SENDER                    
PRTSTAT  DS    XL4                 DO I WANT TO PRINT STAT                      
SYSFILT  DS    CL1                 FILTER ON SYSTEM?                            
LEN      DS    CL1                                                              
DAY      DS    XL3                 WHAT DAY?                                    
NAME     DS    CL8                 USER ID NAME                                 
IDNUM    DS    H                   USER ID NUM                                  
SKIPTB   DS    CL1                                                              
NXTHEAD  EQU   79                  TO BUMP TO NEXT LINE HEADER                  
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE (IN TWAB)                                                
***********************************************************************         
*                                                                               
SAVEDSTR DS    0F                  SAVED STORAGE VARIABLES                      
ISITME   DS    CL4                 $EDI INDENTIFIER                             
LASTTBR  DS    F                   LAST TTBR OF THE LAST SCREEN                 
FRSTTBR  DS    F                   FIRST TTBR OF THE LAST SCREEN                
LASTTRN  DS    H                   LAST TRANSACTION NUMBER                      
*                                                                               
PARMBLK  DS    0C                                                               
PQREFILT DS    H                   PRINT QUEUE REFERENCE NUMBER FILTER          
DAFILT   DS    F                   DISK ADR FILTER                              
DISPALL  DS    CL1                 DDS DISPLAY ALL ON $EDI                      
PQSUBFLT DS    CL3                 PRINT QUEUE SUB ID FILTER                    
METHFILT DS    CL1                 METHOD OF TRANSMISSION FILTER                
TYPEFILT DS    CL1                 TYPE FILTER                                  
TIMEFILT DS    CL5                 TIME FILTER                                  
STATFILT DS    XL4                 STATUS FILTER                                
REQFILT  DS    CL3                 REQUESTOR FILTER                             
DAYFILT  DS    XL3                 DAY FILTER                                   
ORDERFLT DS    CL8                 DARE ORDER NUMBER FILTER                     
DIDFILT  DS    CL6                 DARE ID FILTER                               
STATNFLT DS    CL8                 STATION FILTER                               
STATNLEN DS    XL1                 STATION FLLTER LENGTH                        
SENDFILT DS    XL8                 SENDER FILTER                                
SENDLEN  DS    XL1                 SENDER FILTER LENGTH                         
AGYFILT  DS    XL10                AGENCY FILTER                                
AGYFNUM  DS    XL2                 AGENCY FILTER AS NUMBER                      
AGYLEN   DS    XL1                 AGENCY FILTER LENGTH                         
EZLED    DS    CL1                 DISPLAY EASYLINK LEDGER #                    
ELNDIS   DS    CL1                 DISPLAY EASYLINK MAILBOX#                    
DESTFILT DS    CL16                DESTINATION FILTER                           
DESTLEN  DS    CL1                 DESTINATION FILTER LENGTH                    
CLTFILT  DS    CL3                 CLIENT FILTER                                
*                                                                               
FLAGS    DS    XL1                                                              
FLSENT   EQU   X'10'               SENT FILTER GIVEN                            
FLDEST   EQU   X'20'               DEST FILTER GIVEN                            
FLIDARE  EQU   X'40'               INCOMING DARE FILTER GIVEN                   
FLODARE  EQU   X'80'               OUTGOING DARE FILTER GIVEN                   
FLSUMM   EQU   X'02'               SUMMARY OPTION GIVEN                         
FLNEWSYS EQU   X'01'               CHANGE IN SYSTEM                             
*                                                                               
PBLKLNQ  EQU   *-PARMBLK                                                        
*                                                                               
OLDSYST  DS    CL1                 PREVIOUS SYSTEM                              
FIRST    DS    CL1                 FIRST TIME THROUGH (Y/N)                     
DAYPWOS  DS    CL3                 DATE IN PWOS FORM                            
OLDFILT  DS    CL50                PREVIOUS STRING OF OPTIONAL FILTERS          
SAVEDL   EQU   *-SAVEDSTR                                                       
         EJECT                                                                  
***********************************************************************         
*        MORE WORKING STORAGE                                                   
**********************************************************************          
*                                                                               
QHDR     DS    A                                                                
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
         DS    XL6                                                              
*                                                                               
RELO     DS    A                                                                
VCOMFACS DS    A                                                                
ASYSFACS DS    F                                                                
SCANBOX  DS    5CL38               SCANNER OUTPUT                               
PVOUT    DS    CL60                PERVAL OUTPUT                                
ELCODE   DS    X                                                                
KEY      DS    XL25                FOR CALL TO CTFILE                           
IO       DS    1000X               FOR OUTPUT FROM CTFILE                       
WORK     DS    CL128                                                            
*                                                                               
         DS    0D                                                               
EDICTBLK DC    18432X'00'          EDICT FILE OUTPUT BLOCK                      
*DICTBLK DC    14336X'00'          EDICT FILE OUTPUT BLOCK                      
WRKX     ORG   EDICTBLK+100                                                     
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR DISPLAY LINE DATA FOR USERS                                  
***********************************************************************         
*                                                                               
EDUSERLD DSECT                                                                  
EDUST    DS    CL3                 SYSTEM-TYPE                                  
         DS    CL1                                                              
EDUSTAT  DS    CL4                 STATUS                                       
EDUQDAY  DS    CL1                                                              
EDUTIME  DS    CL4                 TIME                                         
         DS    CL1                                                              
EDUDS    DS    CL16                DESTINATION/SENDER                           
         DS    CL1                                                              
EDUMCPP  DS    CL13                APPLICATION SPECIFIC                         
         DS    CL1                                                              
EDUSPEC  DS    CL33                EASYLINK LEDGER NUMBER                       
         ORG   EDUMCPP                                                          
SAMED    DS    CL1                 LEN=39   MAX=48                              
         DS    CL1                                                              
SACLT    DS    CL3                                                              
         DS    CL1                                                              
SAPRD    DS    CL3                                                              
SAHYPH   DS    CL1                                                              
SAPTNR   DS    CL3                                                              
         DS    CL1                                                              
SAEST    DS    CL3                                                              
         DS    CL1                                                              
SADATES  DS    CL11                                                             
         DS    CL1                                                              
SASTN    DS    CL5                                                              
         DS    CL1                                                              
SAREQ    DS    CL3                                                              
         ORG   EDUMCPP                                                          
SRMED    DS    CL1                 LEN=37                                       
         DS    CL1                                                              
SRCLT    DS    CL3                                                              
         DS    CL1                                                              
SRPRD    DS    CL3                                                              
SRHYPH   DS    CL1                                                              
SRPTNR   DS    CL3                                                              
         DS    CL1                                                              
SREST    DS    CL3                                                              
         DS    CL1                                                              
SRDATES  DS    CL11                                                             
         DS    CL1                                                              
SRREF    DS    CL7                                                              
         ORG   EDUMCPP                                                          
SDMED    DS    CL1                 LEN=38                                       
         DS    CL1                                                              
SDCLT    DS    CL3                                                              
         DS    CL1                                                              
SDPRD    DS    CL3                                                              
SDHYPH   DS    CL1                                                              
SDPTNR   DS    CL3                                                              
         DS    CL1                                                              
SDEST    DS    CL3                                                              
         DS    CL1                                                              
SDMKT    DS    CL4                                                              
         DS    CL1                                                              
SDBYR    DS    CL3                                                              
         DS    CL1                                                              
SDCMP    DS    CL5                                                              
         DS    CL1                                                              
SDSTA    DS    CL5                                                              
         ORG   EDUMCPP                                                          
SVMED    DS    CL1                 LEN=22                                       
         DS    CL1                                                              
SVCLT    DS    CL3                                                              
         DS    CL1                                                              
SVPRD    DS    CL3                                                              
SVHYPH   DS    CL1                                                              
SVPTNR   DS    CL3                                                              
         DS    CL1                                                              
SVSTA    DS    CL8                                                              
         ORG   EDUMCPP                                                          
STMED    DS    CL1                 LEN=40                                       
         DS    CL1                                                              
STCLT    DS    CL3                                                              
         DS    CL1                                                              
STPRD    DS    CL3                                                              
STHYPH   DS    CL1                                                              
STPTNR   DS    CL3                                                              
         DS    CL1                                                              
STEST    DS    CL3                                                              
         DS    CL1                                                              
STDATES  DS    CL11                                                             
         DS    CL1                                                              
STCONTC  DS    CL10                                                             
         ORG   EDUMCPP                                                          
SSMED    DS    CL1                 LEN=36                                       
         DS    CL1                                                              
SSCLT    DS    CL3                                                              
         DS    CL1                                                              
SSPRD    DS    CL3                                                              
SSHYPH   DS    CL1                                                              
SSPTNR   DS    CL3                                                              
         DS    CL1                                                              
SSHOUSE  DS    CL6                                                              
         DS    CL1                                                              
SSCONTC  DS    CL15                                                             
         ORG   EDUMCPP                                                          
SCMED    DS    CL1                 LEN=36                                       
         DS    CL1                                                              
SCCLT    DS    CL3                                                              
         DS    CL1                                                              
SCPRD    DS    CL3                                                              
SCHYPH   DS    CL1                                                              
SCPTNR   DS    CL3                                                              
         DS    CL1                                                              
SCEST    DS    CL3                                                              
         DS    CL1                                                              
SCDATES  DS    CL11                                                             
         DS    CL1                                                              
SCCONTC  DS    CL6                                                              
         ORG   EDUMCPP                                                          
NNMED    DS    CL1                 LEN=38                                       
         DS    CL1                                                              
NNCLT    DS    CL3                                                              
         DS    CL1                                                              
NNPRD    DS    CL3                                                              
         DS    CL1                                                              
         DS    CL3                                                              
         DS    CL1                                                              
NNNET    DS    CL4                                                              
         DS    CL1                                                              
NNDATES  DS    CL11                                                             
         DS    CL1                                                              
NNPGM    DS    CL7                                                              
         ORG   EDUMCPP                                                          
NCMED    DS    CL1                 LEN=30                                       
         DS    CL1                                                              
NCCLT    DS    CL3                                                              
         DS    CL1                                                              
NCPRD    DS    CL3                                                              
         DS    CL1                                                              
         DS    CL3                                                              
         DS    CL1                                                              
NCNET    DS    CL4                                                              
         DS    CL1                                                              
NCDATES  DS    CL11                                                             
         ORG   EDUMCPP                                                          
NPMED    DS    CL1                 LEN=33                                       
         DS    CL1                                                              
NPCLT    DS    CL3                                                              
         DS    CL1                                                              
NPPRD    DS    CL3                                                              
         DS    CL1                                                              
         DS    CL3                                                              
         DS    CL1                                                              
NPNET    DS    CL4                                                              
         DS    CL1                                                              
NPEST    DS    CL3                                                              
         DS    CL1                                                              
NPPAK    DS    CL3                                                              
         DS    CL1                                                              
NPREQ    DS    CL3                                                              
         DS    CL1                                                              
NPPRG    DS    CL2                                                              
         ORG   EDUMCPP                                                          
PIMED    DS    CL1                 LEN=38                                       
         DS    CL1                                                              
PICLT    DS    CL3                                                              
         DS    CL1                                                              
PIPRD    DS    CL3                                                              
         DS    CL1                                                              
         DS    CL3                                                              
         DS    CL1                                                              
PIJOB    DS    CL6                                                              
         DS    CL1                                                              
PIPUB    DS    CL17                                                             
         ORG   EDUMCPP                                                          
         DS    CL1                 LEN=48                                       
         DS    CL1                                                              
AOCLT    DS    CL3                                                              
         DS    CL1                                                              
AOPRD    DS    CL3                                                              
         DS    CL1                                                              
         DS    CL3                                                              
         DS    CL1                                                              
AOJOB    DS    CL14                                                             
         DS    CL1                                                              
AOPONUM  DS    CL6                                                              
         DS    CL1                                                              
AOVEND   DS    CL12                                                             
         ORG   EDUMCPP             LEN=34                                       
ROOFF    DS    CL2                                                              
         DS    CL1                                                              
ROSLP    DS    CL3                                                              
         DS    CL1                                                              
ROHNM    DS    CL8                                                              
         DS    CL1                                                              
ROAGY    DS    CL4                                                              
         DS    CL1                                                              
ROCTY    DS    CL2                                                              
         DS    CL1                                                              
ROADV    DS    CL4                                                              
         DS    CL1                                                              
ROSTN    DS    CL5                                                              
         ORG   EDUMCPP                                                          
RKOFF    DS    CL2                                                              
         DS    CL1                                                              
RKBOOK   DS    CL3                                                              
         DS    CL1                                                              
RKKWXN   DS    CL8                                                              
         DS    CL1                                                              
RKSTN    DS    CL5                                                              
*                                                                               
         ORG   EDUMCPP                                                          
GXDISP   DS    CL47                                                             
*                                                                               
         ORG   EDUDS                                                            
DADEST   DS    CL10                                                             
         DS    CL1                                                              
DASTA    DS    CL5                                                              
         DS    CL2                                                              
DAMED    DS    CL1                                                              
         DS    CL1                                                              
DACLT    DS    CL3                                                              
         DS    CL1                                                              
DAPRD    DS    CL3                                                              
DAHYPH   DS    CL1                                                              
DAPTN    DS    CL3                                                              
         DS    CL1                                                              
DAEST    DS    CL3                                                              
         DS    CL2                                                              
DABYR    DS    CL3                                                              
         DS    CL1                                                              
DAORD#   DS    CL8                                                              
         DS    CL2                                                              
DASAL    DS    CL3                                                              
         DS    CL1                                                              
DACON#   DS    CL8                                                              
*                                                                               
         ORG   EDUDS                                                            
         DS    CL1                                                              
DRSTA    DS    CL5                                                              
         DS    CL1                                                              
DRAGY    DS    CL5                                                              
         DS    CL2                                                              
DRMED    DS    CL1                                                              
         DS    CL1                                                              
DRCLT    DS    CL3                                                              
         DS    CL1                                                              
DRPRD    DS    CL3                                                              
DRHYPH   DS    CL1                                                              
DRPTN    DS    CL3                                                              
         DS    CL1                                                              
DREST    DS    CL3                                                              
         DS    CL2                                                              
DRBYR    DS    CL3                                                              
         DS    CL1                                                              
DRORD#   DS    CL8                                                              
         DS    CL2                                                              
DRSAL    DS    CL3                                                              
         DS    CL1                                                              
DRCON#   DS    CL8                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR DISPLAY LINE DATA FOR DDS (ETD)                              
***********************************************************************         
*                                                                               
EDDDSLD  DSECT                                                                  
DDSLSTAT DS    CL4                                                              
         DS    CL1                                                              
DDSLST   DS    CL2                                                              
         DS    CL1                                                              
DDSLTMS  DS    CL4                                                              
DDSLDAY  DS    CL1                                                              
DDSLTMD  DS    CL4                                                              
         DS    CL1                                                              
DDSLSEND DS    CL8                                                              
         DS    CL1                                                              
DDSLPQ   DS    CL20                                                             
         DS    CL1                                                              
DDSLDEST DS    CL16                                                             
         DS    CL1                                                              
DDSLLEDG DS    CL8                                                              
         DS    CL1                                                              
DDSLFTP  DS    CL5                                                              
         ORG   DDSLTMS                                                          
DDSLDARE DS    CL56                                                             
         DS    CL1                                                              
DDSLDA   DS    CL8                                                              
         DS    CL1                                                              
DDSLERR  DS    CL3                                                              
         DS    CL2                                                              
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR SUMMARY LINE DATA                                            
***********************************************************************         
*                                                                               
EDSUMMD  DSECT                                                                  
SUMROW   DS    CL15                NAME OF ROW                                  
         DS    CL3                                                              
SUMWTNG  DS    CL5                                                              
         DS    CL4                                                              
SUMSENT  DS    CL5                                                              
         DS    CL4                                                              
SUMCANX  DS    CL5                                                              
         DS    CL4                                                              
SUMDLVD  DS    CL5                                                              
         DS    CL4                                                              
SUMERR   DS    CL5                                                              
         DS    CL6                                                              
SUMTOT   DS    CL6                                                              
         EJECT                                                                  
***********************************************************************         
*        SCREEN                                                                 
***********************************************************************         
*                                                                               
SRETIFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRETIFFD                                                       
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
*DDFLDHDR                                                                       
*DDSCANBLKD                                                                     
*DMDTFPH                                                                        
*DMGREQUS                                                                       
*DDCOMFACS                                                                      
*FADSECTS                                                                       
*FAFACTS                                                                        
*DDEDICTFIL                                                                     
*SPDARDARED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
       ++INCLUDE DDEDICTFIL                                                     
         PRINT OFF                                                              
       ++INCLUDE SPDARDARED                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ANXXXXX   05/01/02'                                      
         END                                                                    
