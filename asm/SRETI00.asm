*          DATA SET SRETI00    AT LEVEL 012 AS OF 08/29/19                      
*PHASE T11A00A                                                                  
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
         MVC   TRNSEN,TSESSION     TRANSACTION SESSION NUMBER                   
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
         MVC   AFACIDT,SSBAFID-SSBD(R5)                                         
         MVC   RECLEN,SSBTWAL-SSBD(R5)                                          
         MVC   TRNDATEB,SSBDATEB-SSBD(R5)     SAVE TRANSACTION DATE             
*                                                                               
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
         CLC   LASTSEN,TRNSEN      SAME SESSION                                 
         BNE   SET70                                                            
         CLC   LASTDATB,TRNDATEB   SAME DATE                                    
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
SET70    BRAS  RE,PARMCLR          CLEAR AND START FROM TOP                     
         XC    OLDFILT,OLDFILT                                                  
         XC    SYSFILT,SYSFILT                                                  
         XC    RECSKIP,RECSKIP                                                  
*                                                                               
SET80    BRAS  RE,PARAMS           VALIDATE PARAMS                              
         BAS   RE,MAIN             DISPLAY RECS                                 
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
         SR    R2,R2               SECOND COL LINE                              
         LA    R1,COLSDDS          ETD COLS                                     
         LA    R2,COLSDDS2         ETD COLS                                     
         CLI   DDSVERS,C'Y'        ETD?                                         
         BE    MAIN20                                                           
         LA    R1,COLSADV          ADV COLS (ALL BUT SYS=REP OR DARE)           
         LA    R2,COLSALL2         2ND LINE COLS - FOR ALL                      
*                                                                               
         ZIC   RF,SYSID            FACPAK NUMBER                                
         MHI   RF,L'FACITAB                                                     
         L     RE,AFACIDT                                                       
         AR    RF,RE               RF = A(ENTRY IN FACIDTAB)                    
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
         LTR   R2,R2                                                            
         BZ    MAIN30                                                           
         MVC   8(79,R6),0(R2)      2ND LINE OF COLUMN HEADINGS                  
         LA    R6,NXTHEAD+8(R6)    BUMP TO NEXT SCREEN LINE HEADER              
         EJECT                                                                  
***********************************************************************         
*        GET ADDRESS OF THE EDICT FILE                                          
***********************************************************************         
*                                                                               
MAIN30   ZIC   RF,SYSID            FACPAK NUMBER                                
         MHI   RF,L'FACITAB                                                     
         L     RE,AFACIDT                                                       
         AR    RF,RE               RF = A(ENTRY IN FACIDTAB)                    
         TM    5(RF),X'20'         IS THIS A REP SYSTEM?                        
         BO    MAIN60              YES                                          
*                                                                               
         TM    5(RF),X'80'         IS THIS A TEST SYSTEM?                       
         BNO   MAIN40              NO - SHOW ONLY EDICTA FILE                   
*                                                                               
         CLI   REPFILT,C'Y'        SHOW EDICTR FILE?                            
         BE    MAIN60              YES                                          
*                                                                               
MAIN40   GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTA'                               
         MVC   EDICTFL,12(R1)      A(EDICT DCB)                                 
         MVI   EDICTFL,X'00'       CLEAR HIGH ORDER BYTE                        
         B     MAIN70                                                           
*                                                                               
MAIN60   EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTR'                               
         MVC   EDICTFL,12(R1)      A(EDICT DCB)                                 
         MVI   EDICTFL,X'00'       CLEAR HIGH ORDER BYTE                        
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
         BAS   RE,REFCHK           CONVERT REF# FILTER TO DSKADR?               
*                                                                               
         MVC   BMONTH,DAY+1        SAVE MONTH                                   
         ZIC   R2,DAY+2            DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR TODAY         
         STCM  R2,3,FIRSTRAC                                                    
         LH    R1,EDCTFTPD                                                      
         BCTR  R1,0                                                             
         AR    R1,R2               LAST TRACK NUMBER FOR TODAY                  
         STCM  R1,3,EDCTFLST                                                    
*                                                                               
         TM    FLAGS,FLSUMM        SUMMARY OPTION                               
         BNO   MAIN90                                                           
         BRAS  RE,SUMMARY                                                       
         B     EXIT                                                             
*                                                                               
MAIN90   STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
         STCM  R2,3,LASTRAC                                                     
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
MAIN130  CLI   EDFMON,EDFMONPQ     CONTROL REC                                  
         BE    MAIN250                                                          
         CLC   EDFMON,BMONTH       IS THIS RECORD FROM THIS MONTH?              
         BE    MAIN150             YES -- CONTINUE...                           
         CLI   PFKEY,8             DID USER WANT BOTTOM                         
         BNE   MSG3                NO -- WE'VE FOUND THE EOF                    
         MVI   BACKCNT,1                                                        
         B     MAIN170             YES THEN PAGE UP FROM END                    
         EJECT                                                                  
***********************************************************************         
*        PAGE UP                                                                
***********************************************************************         
*                                                                               
MAIN150  CLI   PFKEY,5             SCROLLING UP?                                
         BNE   MAIN190                                                          
MAIN160  BRAS  RE,FILTCK           FILTER CHECK                                 
         BNE   MAIN170             NO , BACK UP ANOTHER REC                     
         CLI   BACKCNT,10          START DISPLAYING?                            
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
         BRAS  RE,FILTCK           FILTER CHECK                                 
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
         SR    R2,R2                                                            
         ICM   R2,3,EDCTFDSK                                                    
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
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
RB10     STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
         STCM  R2,3,LASTRAC        SAVE TRACK NUMBER                            
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
         SR    R2,R2                                                            
         ICM   R2,3,LASTRAC        CURRENT TRACK NUMBER                         
         LA    R2,1(R2)            BUMP TO NEXT TRACK NUMBER                    
         CLM   R2,3,EDCTFLST       IS IT LAST TRACK?                            
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
         SR    R1,R1                                                            
         ICM   R1,3,EDCTFDSK       NEED TO DECR. TRACK                          
         BCTR  R1,0                                                             
         STCM  R1,3,EDCTFDSK                                                    
         MVC   EDCTFDSK+2(1),EDCTFRPT+1          MAX BLOCK NUM                  
         MVI   EDCTFDSK+3,X'00'                                                 
         LH    R1,EDCTRPB          LOGICAL RECORDS PER BLOCK (112)              
         STC   R1,RECNUM                                                        
         BCTR  R1,0                                                             
         STC   R1,RECSKIP                                                       
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
TBX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READ TTBR                                                              
***********************************************************************         
*                                                                               
READBLK  NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,EDCTFDSK       IS TRACK FOR THIS DAY                        
         CLM   R1,3,EDCTFLST                                                    
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
*        CALCULATE DISK ADR OF REF# FILTER - IF SPECIFIED                       
***********************************************************************         
*                                                                               
REFCHK   NTR1                                                                   
         OC    REFFILT,REFFILT                                                  
         BZ    EXIT                                                             
         PACK  DUB,REFFILT(2)                                                   
         CVB   RF,DUB              RF = DAY NUMBER                              
         BCTR  RF,0                                                             
         MH    RF,EDCTFTPD         RF = STARTING TRACK NUMBER THIS DAY          
         PACK  DUB,REFFILT+2(6)                                                 
         CVB   R1,DUB              R1 = RECORD OFFSET INTO DAY                  
         BCTR  R1,0                                                             
*                                                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         LH    RE,EDCTRPB          NUMBER OF LOGICAL RECORDS PER BLOCK          
         DR    R0,RE                                                            
         AH    R0,=H'1'                                                         
         STC   R0,DAFILTER+3         R0 = LOGICAL RECORD NUMBER                 
         SR    R0,R0               PREPARE FOR DIVIDE                           
         LH    RE,EDCTFRPT         NUMBER OF BLOCKS PER TRACK                   
         DR    R0,RE                                                            
         LA    RF,1(R1,RF)         RF = TRACK NUMBER                            
         STCM  RF,3,DAFILTER                                                    
         AH    R0,=H'1'                                                         
         STC   R0,DAFILTER+2         R0 = BLOCK NUMBER                          
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
         BNE   DD20                                                             
         MVC   DDSLERR,RDNTEFLG    DISPLAY ERROR FLAG                           
         B     DD20                                                             
         DROP  R1                                                               
*                                                                               
DD10     CLI   EDFSYS,EDFBIASQ     BIAS REC DISPLAY??                           
         BNE   DD12                                                             
         MVC   DDSLDARE,EDFBIAS    BIAS DATA                                    
         LA    R2,DDSLDA           DISK ADDRESS                                 
         BAS   RE,DISK                                                          
         B     DD20                                                             
*                                                                               
DD12     LA    R2,DDSLTMS                                                       
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
*                                                                               
         CLI   BCNFILT,C'Y'        Y=SHOW BDE COMMON NAME                       
         BNE   DD15                  INSTEAD OF DESTINATION AND DISKADR         
         LA    R2,DDSLBCN                                                       
         BAS   RE,DISPBCN                                                       
         MVC   EDCTFDSK+3(1),RECNUM                                             
         BE    DD20                                                             
*                                                                               
DD15     LA    R2,DDSLDEST                                                      
         BAS   RE,DESTINE                                                       
         LA    R2,DDSLLEDG                                                      
         BAS   RE,DISK                                                          
*                                                                               
DD20     LA    R6,NXTHEAD+8(R6)    BUMP TO NEXT SCREEN LINE                     
*                                                                               
         CLI   EDFSYS,EDFDAREQ     DARE REC DISPLAY??                           
         BNE   DD26                                                             
         MVC   DDSLDR2,EDFDARE+L'DDSLDARE    2ND LINE DARE DATA                 
         B     DDX                                                              
*                                                                               
DD26     CLI   EDFSYS,EDFBIASQ     BIAS REC DISPLAY??                           
         BNE   DD28                                                             
*        MVC   DDSLDR2,EDFBIAS+L'DDSLDARE    2ND LINE BIAS DATA                 
         B     DDX                                                              
*                                                                               
DD28     EQU   *                                                                
         LA    R2,DDSLQTYP         PQ REPORT TYPE                               
         BAS   RE,DISPQTYP                                                      
*                                                                               
         LA    R2,DDSLBDES         BDE SENDER LETTER                            
         BAS   RE,DISPBDES                                                      
*                                                                               
         CLI   BCRFILT,C'Y'        Y=SHOW BDE CUSTOMER REF#                     
         BNE   DD29                  INSTEAD OF ERROR INFO                      
         LA    R2,DDSLBCR                                                       
         BAS   RE,DISPBCR                                                       
         B     DD30                                                             
*                                                                               
DD29     LA    R2,DDSLREAS         REASON FOR ERROR                             
         BAS   RE,DISPERR                                                       
*                                                                               
DD30     LA    R2,DDSLRLFN         DELIVERED FAX NUMBER                         
         BAS   RE,DISPRLFN                                                      
*                                                                               
         LA    R2,DDSLDSN          DESTINATION SEQUENCE NUMBER                  
         BAS   RE,DSTSEQN                                                       
*                                                                               
         LA    R2,DDSLREF          CUST REF NUMBER                              
         BRAS  RE,CUSTREF                                                       
*                                                                               
         LA    R2,DDSLEZJ          EASYLINK REF NUMBER OR FAXGATE JOB           
         CLI   EDFMETH,EDIEASYQ    EASYLINK                                     
         BNE   *+12                                                             
         BAS   RE,DISPEZ           THEN DISPLAY EASYLINK REF#                   
         B     DD40                                                             
*                                                                               
         CLI   EDFMETH,EDIEXRRQ    EXTREME REACH                                
         BNE   DD30ECN                                                          
         MVC   0(L'EDFXRRTN,R2),EDFXRRTN                                        
         B     DD40                                                             
*                                                                               
DD30ECN  CLI   EDFMETH,EDIECNQ     ECN                                          
         BNE   DD30PDF                                                          
         MVC   0(L'EDFECNTN,R2),EDFECNTN                                        
         B     DD40                                                             
*                                                                               
DD30PDF  CLI   EDFMETH,EDIPDFQ     PDF                                          
         BNE   DD30FXG                                                          
*        BRAS  RE,DISPPDF          THEN DISPLAY PDF INFORMATION                 
         B     DD40                                                             
*                                                                               
DD30FXG  CLI   EDFMETH,EDIFAXGQ    FAXGATE                                      
         BNE   DD30BDE                                                          
         MVC   0(L'EDFFXGTJ,R2),EDFFXGTJ DISPLAY FAXGATE JOB ID                 
         B     DD40                                                             
*                                                                               
DD30BDE  CLI   EDFMETH,EDIBDEQ     BDE-EMAIL                                    
         BE    *+12                                                             
         CLI   EDFMETH,EDIBDFQ     BDE-FTP                                      
         BNE   DD40                                                             
         BAS   RE,DISPBDOC         THEN DISPLAY BDE DOC ID #                    
*                                                                               
DD40     LA    R2,DDSLACK2         EASYLINK MAILBOX NUMBER OF FTP#              
         CLI   EDFMETH,EDIENCOQ    ENCODA                                       
         BNE   *+8                                                              
         BAS   RE,DISPACK2         THEN DISPLAY 'F' IF ACK2(FWRD) REVD          
*                                                                               
         LA    R2,DDSLEFTP         EASYLINK MAILBOX NUMBER OF FTP#              
         CLI   EDFMETH,EDIEASYQ    EASYLINK                                     
         BNE   DD50                                                             
         BAS   RE,DISPELN          THEN DISP MAILBOX NUMBER                     
         B     DDX                                                              
*                                                                               
DD50     CLI   EDFMETH,EDIFTPQ     FTP                                          
         BNE   DD60                                                             
         BRAS  RE,DISPFTP          THEN DISP FTP#                               
         B     DDX                                                              
*                                                                               
DD60     CLI   EDFMETH,EDIBDFQ     BDE-FTP                                      
         BNE   DD70                                                             
         BAS   RE,DISPFILN         THEN DISPLAY BDF FILE NAME                   
         B     DDX                                                              
*                                                                               
DD70     CLI   EDFMETH,EDIECNQ     ECN ID #                                     
         BNE   *+10                                                             
         MVC   0(L'EDFECNID,R2),EDFECNID DISPLAY ECN ID #                       
*                                                                               
         CLI   EDFMETH,EDIEXRRQ    EXTREME REACH #                              
         BNE   *+10                                                             
         MVC   0(L'EDFXRRID,R2),EDFXRRID DISPLAY ECTREME REACH                  
*                                                                               
DDX      XIT1  REGS=(R6)                                                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ETI DISPLAY                                                            
***********************************************************************         
         USING EDUSERLD,R6                                                      
USERDISP NTR1                                                                   
         MVC   EDCTFDSK+3(1),RECNUM     UPDATE RECORD NUMBER                    
*                                                                               
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
         BE    UD10                                                             
         LA    R2,EDUDS                                                         
         CLI   EZLED,C'Y'          DISPLAY EASYLINK LEDGER #                    
         BNE   *+12                                                             
         BAS   RE,DISPEZ                                                        
         B     UD10                                                             
         CLI   ELNDIS,C'Y'         DISPLAY EASYLINK MAILBOX#                    
         BNE   *+12                                                             
         BAS   RE,DISPELN                                                       
         B     UD10                                                             
         BAS   RE,USERDS           OR DESTINATION                               
*                                                                               
UD10     LA    R2,EDUMCPP                                                       
         BRAS  RE,USERAPPL                                                      
*                                                                               
         LA    R6,NXTHEAD+8(R6)    BUMP TO NEXT SCREEN LINE                     
*                                                                               
         LA    R2,EDULREAS         REASON FOR ERROR                             
         BAS   RE,DISPERR                                                       
*                                                                               
         LA    R2,EDULRLFN         DELIVERED FAX NUMBER                         
         BAS   RE,DISPRLFN                                                      
*                                                                               
         LA    R2,EDULREF          CUST REF NUMBER                              
         BRAS  RE,CUSTREF                                                       
*                                                                               
         LA    R2,EDULEZJ          EASYLINK REF NUMBER OR FAXGATE JOB           
         CLI   EDFMETH,EDIEASYQ    EASYLINK                                     
         BNE   *+8                                                              
         BAS   RE,DISPEZ           THEN DISPLAY EASYLINK REF#                   
*                                                                               
         CLI   EDFMETH,EDIECNQ     ECN                                          
         BNE   *+10                                                             
         MVC   0(L'EDFECNTN,R2),EDFECNTN DISPLAY ECN TRACKING NUMBER            
*                                                                               
         CLI   EDFMETH,EDIEXRRQ    EXTREME REACH                                
         BNE   UD20                                                             
         MVC   0(L'EDFXRRTN,R2),EDFXRRTN DISPLAY ER TRACKING NUMBER             
*                                                                               
UD20     CLI   EDFMETH,EDIFAXGQ    FAXGATE                                      
         BNE   UD22                                                             
         MVC   0(L'EDFFXGTJ,R2),EDFFXGTJ DISPLAY FAXGATE JOB ID                 
*                                                                               
UD22     CLI   EDFMETH,EDIBDEQ     BDE-EMAIL?                                   
         BE    *+12                                                             
         CLI   EDFMETH,EDIBDFQ     BDE-FTP?                                     
         BNE   UD30                                                             
         BAS   RE,DISPBDOC         THEN DISPLAY BDE DOC ID #                    
*                                                                               
UD30     LA    R2,EDULEFTP         EASYLINK MAILBOX NUMBER OF FTP#              
         CLI   EDFMETH,EDIEASYQ    EASYLINK                                     
         BNE   UD40                                                             
         BAS   RE,DISPELN          THEN DISP MAILBOX NUMBER                     
         B     UDX                                                              
*                                                                               
UD40     CLI   EDFMETH,EDIFTPQ     FTP                                          
         BNE   UD50                                                             
         BRAS  RE,DISPFTP          THEN DISP FTP#                               
         B     UDX                                                              
*                                                                               
UD50     CLI   EDFMETH,EDIECNQ     ECN                                          
         BNE   *+10                                                             
         MVC   0(L'EDFECNID,R2),EDFECNID  DISPLAY ECN ID #                      
*                                                                               
         CLI   EDFMETH,EDIEXRRQ    ER                                           
         BNE   *+10                                                             
         MVC   0(L'EDFXRRID,R2),EDFXRRID  DISPLAY EXTREME REACH                 
*                                                                               
UDX      XIT1  REGS=(R6)                                                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY SYSTEM-TYPE -- USER TERMINAL                                   
***********************************************************************         
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
         SHI   R6,8                BACK UP TO HEADER                            
         OI    6(R6),X'00'         TURN OFF                                     
*                                                                               
         LA    R4,STATTAB                                                       
US10     MVC   WORK(1),0(R4)                                                    
         NC    WORK(1),EDFSTAT                                                  
         CLC   WORK(1),0(R4)       IS BIT ON                                    
         BE    US15                                                             
         LA    R4,5(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   US10                                                             
         B     USX                                                              
*                                                                               
US15     CLI   EDFMETH,EDIEASYQ    MUST BE EASYLINK TRANSACTION                 
         BNE   US20                                                             
         OC    EDFEZLED,EDFEZLED   MUST HAVE ALREADY GOTTEN LEDGER#             
         BZ    US90                                                             
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   US16                MUST NOT HAVE A RECEIVED TIME                
         MVC   PRTSTAT,=C'ACC '                                                 
         B     USX                                                              
*                                                                               
US16     TM    0(R4),EDFSTCAN      MUST BE CANCELLED ALREADY                    
         BNO   US90                                                             
         MVC   PRTSTAT,=C'REJ '                                                 
         B     USX                                                              
*                                                                               
US20     CLI   EDFMETH,EDIEXRRQ    MUST BE ER  TRANSACTION                      
         BE    US22                                                             
         CLI   EDFMETH,EDIECNQ     MUST BE ECN TRANSACTION                      
         BNE   US40                                                             
US22     MVC   PRTSTAT,=C'REJ '                                                 
         TM    EDFECNFL,EDFECN2Q   REJECTED?                                    
         BO    USX                                                              
         OC    EDFXRRTN,EDFXRRTN   MUST HAVE ALREADY GOTTEN TRACKING#           
         BZ    US90                                                             
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   US90                MUST NOT HAVE A RECEIVED TIME                
         MVC   PRTSTAT,=C'ACC '                                                 
         TM    EDFECNFL,EDFECN1Q                                                
         BNO   USX                                                              
         MVC   PRTSTAT,=C'RETY'                                                 
         B     USX                                                              
*                                                                               
US40     CLI   EDFMETH,EDIBDEQ     MUST BE BDE TRANSACTION                      
         BE    *+12                                                             
         CLI   EDFMETH,EDIBDFQ     MUST BE BDE TRANSACTION                      
         BNE   US50                                                             
         OC    EDFBDDOC,EDFBDDOC   MUST HAVE ALREADY GOTTEN DOC ID#             
         BZ    US90                                                             
         OC    EDFBDRTM,EDFBDRTM   HAVE A SAVED RECEIVED TIME?                  
         BZ    *+14                                                             
         MVC   PRTSTAT,=C'DEL '                                                 
         B     USX                                                              
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   US90                                                             
         MVC   PRTSTAT,=C'SENT'                                                 
*        MVC   PRTSTAT,=C'NTF '                                                 
         B     USX                                                              
*                                                                               
US50     CLI   EDFMETH,EDIENCOQ    MUST BE ENCODA  TRANSACTION                  
         BNE   US90                                                             
         TM    0(R4),EDFSTRCV                                                   
         BNO   *+14                                                             
         MVC   PRTSTAT,=C'SDEL'                                                 
         B     USX                                                              
         TM    EDFENFLG,EDDENF02Q                                               
         BZ    US90                                                             
         MVC   PRTSTAT,=C'FWRD'                                                 
         B     USX                                                              
*                                                                               
US90     EQU   *                                                                
         MVC   PRTSTAT,1(R4)                                                    
         TM    0(R4),EDFSTJNK                                                   
         BZ    *+8                                                              
         OI    6(R6),X'08'         HIGHLIGHT GARBAGE                            
         TM    0(R4),EDFSTRCV      IF RECIEVED DON'T CHECK FOR CANX             
         BO    USX                                                              
         TM    0(R4),EDFSTCAN                                                   
         BZ    *+8                                                              
         OI    6(R6),X'08'         HIGHLIGHT GARBAGE                            
USX      MVC   0(4,R2),PRTSTAT                                                  
         TM    6(R6),X'08'         HIGHLIGHT GARBAGE IS ON                      
         BNO   YES                                                              
         LA    R6,NXTHEAD+8(R6)    BUMP TO NEXT SCREEN LINE                     
         OI    6(R6),X'08'         HIGHLIGHT 2ND LINE TOO                       
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
         CLC   EDFSNTIM,=X'FFFF'   DON'T SHOW SENT TIME YET                     
         BE    YES                                                              
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
         BZ    UDEST10                                                          
         MVC   0(8,R2),PRTSEND     SENDERS ALPHA USERID                         
         B     YES                                                              
UDEST10  DS    0H                                                               
         CLI   DDSVERS,C'Y'        ETD?                                         
         BE    UDEST20                                                          
         CLC   =C'EDICT=',EDFDEST  HEADER?                                      
         BNE   UDEST20                                                          
         MVC   0(10,R2),EDFDEST+6  DESTINATION MINUS EDICT=                     
         B     YES                                                              
UDEST20  MVC   0(16,R2),EDFDEST    DESTINATION                                  
         B     YES                                                              
***********************************************************************         
*        DISPLAY BDE CUSTOMER REF#                                              
***********************************************************************         
DISPBCR  NTR1                                                                   
         MVC   0(L'DDSLBCR,R2),EDFBDECR                                         
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BDE CUSTOMER REF#                                              
***********************************************************************         
DISPBCN  NTR1                                                                   
         CLC   =C'EDICT=',EDFDEST  HEADER?                                      
         BNE   NO                                                               
*                                                                               
         LA    R5,KEY                                                           
         USING EDIKEYD,R5                                                       
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,EDFDEST+6   EDICT=?????                                  
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO,0                     
*                                                                               
         LAY   R5,IO                                                            
         CLC   EDIKEY,KEY                                                       
         BE    DBCN20                                                           
         MVC   0(5,R2),=CL5'?????'                                              
         B     YES                                                              
         DROP  R5                                                               
*                                                                               
DBCN20   LAY   R4,IO                                                            
         AHI   R4,28                                                            
         XR    RF,RF                                                            
         USING EDILNKD,R4                                                       
*                                                                               
DBCN30   CLI   EDILNKEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    *+12                                                             
         IC    RF,EDILNKLN                                                      
         BXH   R4,RF,DBCN30                                                     
*                                                                               
         MVC   0(L'DDSLBCN,R2),EDIBDECN      BDE COMMAN NAME                    
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY REASON FOR ERROR                                               
***********************************************************************         
DISPERR  NTR1                                                                   
         TM    EDFSTAT,EDFSTJNK    IF JUNK PUT OUT ERROR NUMBER                 
         BNO   DERRFXG                                                          
*                                                                               
         MVC   0(24,R2),=CL24'PLEASE CONTACT MO'                                
         CLI   DDSTERM,C'Y'        DDS TERMINAL?                                
         BNE   YES                 NO - DON'T DISPLAY JUNK ERRORS               
*                                                                               
         LAY   R4,DDSERRTB         DDS ERROR TABLE                              
DERR10   CLI   0(R4),X'FF'                                                      
         BE    YES                                                              
         CLC   EDFERROR,0(R4)      MATCH ON CODE                                
         BE    DERR20                                                           
         LA    R4,25(R4)                                                        
         B     DERR10                                                           
*                                                                               
DERR20   MVC   0(24,R2),1(R4)      DISPLAY TEXT                                 
         B     YES                                                              
                                                                                
*****************                                                               
*   FAX GATE    *                                                               
*****************                                                               
DERRFXG  CLI   EDFMETH,EDIFAXGQ    FAXGATE                                      
         BNE   DERREFX                                                          
         CLC   EDFFXGTS,=C'0007'   IF FAX SENT OK                               
         BE    YES                 THEN SKIP DISPLAY                            
         CLC   EDFFXGTS,SPACES     ANY  ERROR                                   
         BNH   YES                 THEN SKIP DISPLAY                            
*                                                                               
         LAY   R4,FXGMSGTB         FAXGATE ERROR TABLE                          
DEFXG10  CLI   0(R4),X'FF'                                                      
         BE    DEFXG30                                                          
         CLC   EDFFXGTS,0(R4)      MATCH ON CODE                                
         BE    DEFXG20                                                          
         LA    R4,28(R4)                                                        
         B     DEFXG10                                                          
*                                                                               
DEFXG20  MVC   0(24,R2),4(R4)      DISPLAY TEXT                                 
         B     YES                                                              
*                                                                               
DEFXG30  MVC   0(11,R2),=C'FAX ERROR #'        NOT IN TABLE                     
         MVC   11(L'EDFFXGTS,R2),EDFFXGTS      SO DISPLAY NUMBER                
         B     YES                                                              
                                                                                
************************                                                        
*   EASYLINK AND ECN   *                                                        
************************                                                        
DERREFX  CLI   EDFMETH,EDIEXRRQ    EXTREME REACH                                
         BE    DEEFX10                                                          
         CLI   EDFMETH,EDIEASYQ    EASYLINK                                     
         BE    DEEFX10                                                          
         CLI   EDFMETH,EDIECNQ     ECN                                          
         BNE   DERRBDE                                                          
*                                                                               
DEEFX10  CLC   EDFEZERR,SPACES     ANY  ERROR                                   
         BNH   YES                 THEN SKIP DISPLAY                            
*                                                                               
         LAY   R4,EZMSGTAB         EASYLINK ERROR TABLE                         
DEEFX20  CLI   0(R4),X'FF'                                                      
         BE    DEEFX40                                                          
         CLC   EDFEZERR,0(R4)      MATCH ON CODE                                
         BE    DEEFX30                                                          
         LA    R4,28(R4)                                                        
         B     DEEFX20                                                          
*                                                                               
DEEFX30  MVC   0(24,R2),4(R4)      DISPLAY TEXT                                 
         B     YES                                                              
*                                                                               
DEEFX40  CLI   EDFMETH,EDIEASYQ    EASYLINK                                     
         BNE   DEEFX42                                                          
         MVC   0(14,R2),=C'EZ FAX ERROR #'     NOT IN TABLE                     
         MVC   14(L'EDFEZERR,R2),EDFEZERR      SO DISPLAY NUMBER                
         B     YES                                                              
*                                                                               
DEEFX42  CLI   EDFMETH,EDIECNQ     ECN                                          
         BNE   DEEFX50                                                          
         MVC   0(15,R2),=C'ECN FAX ERROR #'     NOT IN TABLE                    
         MVC   15(L'EDFEZERR,R2),EDFEZERR      SO DISPLAY NUMBER                
         B     YES                                                              
*                                                                               
DEEFX50  MVC   0(15,R2),=C'ER FAX ERROR # '     NOT IN TABLE                    
         MVC   15(L'EDFEZERR,R2),EDFEZERR      SO DISPLAY NUMBER                
         B     YES                                                              
                                                                                
***********                                                                     
*   BDE   *                                                                     
***********                                                                     
DERRBDE  CLI   EDFMETH,EDIBDFQ     BDE                                          
         BNE   DERRPDF                                                          
         CLC   =C'ERR',EDFBDDOC    ANY ERROR                                    
         BNE   YES                 NO, SKIP DISPLAY                             
*                                                                               
         LAY   R4,BDEMSTAB         BDE ERROR TABLE                              
DEBDE10  CLI   0(R4),X'FF'                                                      
         BE    DEBDE30                                                          
         CLC   EDFBDDOC+3(6),0(R4) MATCH ON CODE                                
         BE    DEBDE20                                                          
         LA    R4,30(R4)                                                        
         B     DEBDE10                                                          
*                                                                               
DEBDE20  MVC   0(24,R2),6(R4)      DISPLAY TEXT                                 
         B     YES                                                              
*                                                                               
DEBDE30  MVC   0(24,R2),=CL24'PLEASE CONTACT MO'                                
         B     YES                                                              
                                                                                
***********                                                                     
*   PDF   *                                                                     
***********                                                                     
DERRPDF  CLI   EDFMETH,EDIPDFQ     PDF                                          
         BNE   YES                                                              
         CLC   EDFEZERR,SPACES     ANY  ERROR                                   
         BNH   YES                 THEN SKIP DISPLAY                            
*                                                                               
         LAY   R4,PDFMSTAB         PDF ERROR TABLE                              
DEPDF10  CLI   0(R4),X'FF'                                                      
         BE    DEPDF30                                                          
         CLC   EDFEZERR,0(R4)      MATCH ON CODE                                
         BE    DEPDF20                                                          
         LA    R4,30(R4)                                                        
         B     DEPDF10                                                          
*                                                                               
DEPDF20  MVC   0(24,R2),6(R4)      DISPLAY TEXT                                 
         B     YES                                                              
*                                                                               
DEPDF30  MVC   0(24,R2),=CL24'PLEASE CONTACT MO'                                
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EASYLINK/ECN FAX NUMBER                                        
***********************************************************************         
DISPRLFN NTR1                                                                   
         CLI   EDFMETH,EDIEXRRQ    EXTREME REACH                                
         BE    DRLFN10                                                          
         CLI   EDFMETH,EDIECNQ     ECN?                                         
         BE    DRLFN10                                                          
         CLI   EDFMETH,EDIEASYQ    EASYLINK?                                    
         BNE   YES                                                              
*                                                                               
DRLFN10  TM    EDFSTAT,EDFSTRCV    DELIVERED?                                   
         BNO   YES                                                              
         CLI   DDSTERM,C'Y'        DDS TERMINAL?                                
         BE    DRLFN30             YES - ALWAYS DISP FAX NUMBER                 
*                                                                               
         LA    R1,EDFDEST                                                       
         LA    RE,10                                                            
DRLFN20  CLI   0(R1),C'A'                                                       
         BL    *+12                                                             
         CLI   0(R1),C'Z'                                                       
         BNH   DRLFN30                                                          
*                                                                               
         LA    R1,1(R1)                                                         
         BCT   RE,DRLFN20                                                       
         B     YES                 THIS IS ALREADY A FAX#                       
*                                                                               
DRLFN30  CLC   EDFEZRLN,SPACES                                                  
         BNH   YES                                                              
         MVC   0(10,R2),=CL10'FAXED TO: '                                       
         MVI   10(R2),C'('                                                      
         MVC   11(3,R2),EDFEZRLN                                                
         MVI   14(R2),C')'                                                      
         MVC   16(3,R2),EDFEZRLN+3                                              
         MVI   19(R2),C'-'                                                      
         MVC   20(4,R2),EDFEZRLN+6                                              
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EASYLINK LEDGER NUMBER                                         
***********************************************************************         
DISPEZ   NTR1                                                                   
         CLI   EDFMETH,EDIFTPQ     FTP METHOD                                   
         BNE   DL100                                                            
         EDIT  (B4,EDFEZLED),(5,0(R2))                                          
         B     YES                                                              
DL100    MVC   0(8,R2),EDFEZLED                                                 
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BDE DOC ID #                                                   
***********************************************************************         
DISPBDOC NTR1                                                                   
         CLI   EDFMETH,EDIBDEQ     BDE-EMAIL                                    
         BE    *+12                                                             
         CLI   EDFMETH,EDIBDFQ     BDE-FTP                                      
         BNE   YES                                                              
         MVC   0(L'EDFBDDOC,R2),EDFBDDOC                                        
         CLI   DDSVERS,C'Y'        ETD VERSION?                                 
         BNE   YES                                                              
         OC    EDFBDRTM,EDFBDRTM                                                
         BZ    YES                                                              
         MVI   L'EDFBDDOC(R2),C'*'                                              
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY 'F' IF EC IS FORWARDED TO THE STATION (METHOD=ENCODA)          
***********************************************************************         
DISPACK2 NTR1                                                                   
         TM    EDFENFLG,EDDENF02Q                                               
         BZ    YES                                                              
         MVI   0(R2),C'F'                                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BDE-FTP FILE NAME                                              
***********************************************************************         
*                                                                               
DISPFILN NTR1                                                                   
         MVC   0(L'EDFBDFIL,R2),EDFBDFIL                                        
*                                                                               
         CLI   ECNDISPF,C'Y'                                                    
         BNE   YES                                                              
         MVI   0(R2),C'#'                                                       
         MVC   1(L'ECNDISPV,R2),ECNDISPV                                        
*                                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EASYLINK MAILBOX NUMBER -- USER TERMINAL                       
***********************************************************************         
*                                                                               
DISPELN  NTR1                                                                   
         MVC   WORK(2),=C'62'                ALWAYS STARTS WITH 62              
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFEZBOX,WORK+2,6                                      
         MVC   0(8,R2),WORK                                                     
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY PQ REPORT TYPE ---- DDS TERMINAL                               
***********************************************************************         
*                                                                               
DISPQTYP NTR1                                                                   
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,EDFPQTYP,0(R2),1                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BDE SENDER LETTER IDENTIFIER ---- DDS TERMINAL                 
***********************************************************************         
*                                                                               
DISPBDES NTR1                                                                   
         MVI   0(R2),C' '                                                       
         OC    EDFBDESC,EDFBDESC                                                
         BZ    *+10                                                             
         MVC   0(1,R2),EDFBDESC                                                 
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
         SHI   R6,8                                                             
         MVI   6(R6),0                                                          
         TM    EDFSTAT,EDFSTSNT+EDFSTRCV+EDFSTWTG                               
         BNZ   DSX                                                              
         OI    6(R6),X'08'         HIGHLIGHT                                    
         LA    R6,NXTHEAD+8(R6)    BUMP TO NEXT SCREEN LINE                     
         OI    6(R6),X'08'         HIGHLIGHT                                    
DSX      LA    R6,8(R6)                                                         
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT OUT TIME SENT -- DDS TERMINAL                           
***********************************************************************         
TIMESENT NTR1                                                                   
         TM    EDFSTAT,EDFSTSNT    WAS REPORT SENT                              
         BNO   YES                                                              
         CLC   EDFSNTIM,=X'FFFF'   DON'T SHOW SENT TIME YET                     
         BE    YES                                                              
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
         BRAS  RE,ALPHANME         GET ALPHA SENDER'S ID                        
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
*        ROUTINE TO PUT OUT THE DESTINATION SEQUENCE #-- DDS TERMINAL           
***********************************************************************         
*                                                                               
DSTSEQN  NTR1                                                                   
         CLC   EDFPQDSS,=XL2'0001'                                              
         BH    *+12                                                             
         TM    EDFSTAT,EDFSTLST                                                 
         BO    YES                                                              
*                                                                               
         MVI   0(R2),C'/'                                                       
         EDIT  (B2,EDFPQDSS),(5,1(R2)),FILL=0                                   
         OC    EDFPQDSS,EDFPQDSS                                                
         BNZ   *+10                                                             
         MVC   0(6,R2),=C'      '                                               
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
MSG0     LAY   RE,=C'THERE ARE MORE ITEMS - ENTER FOR NEXT'                     
         MVC   EDSMSG(37),0(RE)                                                 
         CLI   DDSVERS,C'Y'        IS IT ETD?                                   
         BE    MSG00               NO                                           
         TM    FLAG,BOTTOM         PF TO BOTTOM                                 
         BO    MSG0A                                                            
         B     MSGX                                                             
*                                                                               
MSG00    TM    FLAG,BOTTOM         PF TO BOTTOM                                 
         BO    MSG0B                                                            
         LAY   RE,=C'THERE ARE MORE ITEMS - PF6 FOR NEXT  '                     
         MVC   EDSMSG(37),0(RE)                                                 
         B     MSGX                                                             
*                                                                               
MSG0A    LAY   RE,=C'BOTTOM OF LIST - ENTER FOR FIRST     '                     
         MVC   EDSMSG(37),0(RE)                                                 
         B     MSGXC                                                            
MSG0B    LAY   RE,=C'BOTTOM OF LIST                       '                     
         MVC   EDSMSG(37),0(RE)                                                 
         B     MSGX                                                             
*                                                                               
MSG1     LAY   RE,=C'NO MORE RECORDS FOR THIS DAY'                              
         MVC   EDSMSG(28),0(RE)                                                 
         B     MSGX                                                             
*                                                                               
MSG2     LAY   RE,=C'NO PREVIOUS RECORDS FOR THIS DAY'                          
         MVC   EDSMSG(32),0(RE)                                                 
         B     MSGXC                                                            
*                                                                               
MSG2A    LAY   RE,=C'DISK ADDRESS OUTSIDE OF THIS DAY'                          
         MVC   EDSMSG(32),0(RE)                                                 
         B     MSGXC                                                            
*                                                                               
MSG3     CLI   DDSVERS,C'Y'        IS IT ETD?                                   
         BE    MSG4                NO                                           
         LAY   RE,=C'THERE ARE NO MORE ITEMS - ENTER FOR FIRST'                 
         MVC   EDSMSG(41),0(RE)                                                 
         B     MSGXC                                                            
*                                                                               
MSG4     LAY   RE,=C'THERE ARE NO MORE ITEMS                  '                 
         MVC   EDSMSG(41),0(RE)                                                 
         B     MSGXC                                                            
*                                                                               
MSG23    LAY   RE,=C'PLEASE RE-ENTER REQUEST'                                   
         MVC   EDSMSG(23),0(RE)                                                 
         B     MSGXC                                                            
*                                                                               
MSGXC    XC    EDCTFDSK,EDCTFDSK   WANT TO START OVER                           
MSGX     MVC   LASTTBR,EDCTFDSK                                                 
         MVC   ISITME,=C'$EDI'                                                  
         MVC   LASTTRN,TRNNUM      UPDATE TRANSACTION COUNT                     
         MVC   LASTSEN,TRNSEN      UPDATE TRANSACTION SESSION NUMBER            
         MVC   LASTDATB,TRNDATEB   UPDATE TRANSACTION DATE (YMB BINARY)         
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
WRITESTR NTR1                                                                   
         MVC   SR$EDICT(SAVEDL),SAVEDSTR                                        
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TERMNUM                                                     
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LAY   RE,=C'DMWRT'                                                     
         ST    RE,DMCB                                                          
         LAY   RE,=C'TEMPSTR'                                                   
         ST    RE,DMCB+4                                                        
         ST    R2,DMCB+8                                                        
         LAY   RE,SRSD                                                          
         ST    RE,DMCB+12                                                       
         GOTO1 (RF),DMCB                                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        STUFF                                                                  
***********************************************************************         
DIE      DC    H'0'                                                             
YES      SR    RC,RC               SET CC EQUAL                                 
NO       LTR   RC,RC               SET CC NOT EQUAL                             
EXIT     XIT1                                                                   
*                                                                               
HELPID   DC    XL10'011AFF00000000000000'  SYS/PRG/SCRN                         
SPACES   DC    CL20' '                                                          
         GETEL R4,28,ELCODE                                                     
                                                                                
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
COLSALL2 DC    CL40'               REF#      EZ#/JOB#  ELN/F'                   
         DC    CL39'TP#  REASON FOR ERROR                  '                    
*                                                                               
COLSDDS  DC    CL40'STAT ST SENT DLVD SENDER   PQ INFO      '                   
         DC    CL39'        DESTINATION      DSKADDR       '                    
COLSDDS2 DC    CL40'         REASON FOR ERROR               '                   
         DC    CL39' /DST#  REF#      EZ#/JOB#  ELN/FTP#   '                    
*                                                                               
STATTAB  DC    AL1(EDFSTJNK),CL4'DDS '                                          
         DC    AL1(EDFSTRCV),CL4'DLVD'                                          
         DC    AL1(EDFSTCAN),CL4'CANX'                                          
         DC    AL1(EDFSTSNT),CL4'SENT'                                          
         DC    AL1(EDFSTWTG),CL4'WTNG'                                          
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* NOTE: THIS TABLE MUST BE IN SYNC WITH THE ERROR EQUATES                       
*       IN DDEDICTFIL                                                           
*                                                                               
DDSERRTB DC    AL1(1),CL24'NO EDICT REC FOR SENDER '                            
         DC    AL1(2),CL24'NO EDICT REC FOR RECEIVR'                            
         DC    AL1(3),CL24'NO ++TRN CARD ON FTP RPT'                            
         DC    AL1(4),CL24'NO EASYLINK MAILBOX #   '                            
         DC    AL1(5),CL24'NO FAX NUMBER           '                            
         DC    AL1(7),CL24'RPT NOT FOUND OR DAMAGED'                            
         DC    AL1(8),CL24'EDICT RECORD WAS DELETED'                            
         DC    AL1(9),CL24'FTP RECORD TOO LONG     '                            
         DC    AL1(10),CL24'NO METHOD OF XMISSION   '                           
         DC    AL1(11),CL24'BAD REPORT FORMAT       '                           
         DC    AL1(12),CL24'EMPTY REPORT            '                           
         DC    AL1(13),CL24'UNKNOWN USER            '                           
         DC    AL1(14),CL24'CAN''T PROCESS THE REPORT'                          
         DC    AL1(15),CL24'DSN ERROR                '                          
         DC    AL1(16),CL24'BAD ID OR IDI RECORD     '                          
         DC    AL1(17),CL24'INVALID MQ QUEUE INFO    '                          
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE DDEDIEZERR        EASYLINK ERROR TABLE                         
         EJECT                                                                  
       ++INCLUDE DDEDIFGERR        FAXGATE ERROR TABLE                          
         EJECT                                                                  
       ++INCLUDE DDEDIBDERR        BDE ERROR TABLE                              
         EJECT                                                                  
       ++INCLUDE DDEDIPDFRR        PDF ERROR TABLE                              
         EJECT                                                                  
***********************************************************************         
*        DDS CUSTOMER REFERENCE NUMBER                                          
***********************************************************************         
CUSTREF  NTR1  BASE=*,LABEL=*                                                   
         OC    EDCTFDSK,EDCTFDSK                                                
         JNZ   *+14                                                             
         MVC   0(8,R2),=C'********'                                             
         J     YES                                                              
*                                                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         SR    R1,R1                                                            
         ICM   R1,3,EDCTFDSK       TRACK NUMBER                                 
         BCTR  R1,0                                                             
         LH    RF,EDCTFTPD         TRACKS PER DAY                               
         DR    R0,RF               R1 = QUOTIENT                                
         LA    R1,1(R1)            R1 = DAY NUMBER                              
         CVD   R1,DUB                                                           
         UNPK  0(2,R2),DUB         FIRST TWO DIGITS OF NUMBER ARE DAY           
         OI    1(R2),X'F0'                                                      
*                                                                               
         MH    R0,EDCTFRPT         R0 = BLOCKS UP TO THIS TRACK                 
         ZIC   R1,EDCTFDSK+2       R1 = BLOCK NUMBER FROM DISK ADDRESS          
         BCTR  R1,0                                                             
         AR    R0,R1               R0 = TOTAL BLOCKS PRIOR TO THIS ONE          
         MH    R0,EDCTRPB          R0 = TOTAL RECORDS PRIOR TO THIS ONE         
         ZIC   R1,EDCTFDSK+3       R1 = LOGICAL RECORD NUM. FROM DSKADR         
         AR    R0,R1               R0 = LOGICAL RECNUM FROM DAY START           
         CVD   R0,DUB                                                           
         UNPK  2(6,R2),DUB         LAST SIX DIGITS OF NUMBER IS OFFSET          
         OI    7(R2),X'F0'                                                      
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY NETVIEW FTP QUEUE NUMBER                            
***********************************************************************         
DISPFTP  NTR1  BASE=*,LABEL=*                                                   
         MVC   0(5,R2),=C'*****'                                                
         OC    EDFEZLED(4),EDFEZLED                                             
         JZ    YES                                                              
         EDIT  (B4,EDFEZLED),(5,0(R2))                                          
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET ALPHA SENDER'S ID                                       
*        USES NUMBER IN IDNUM AND PUTS NAME IN NAME                             
***********************************************************************         
ALPHANME NTR1  BASE=*,LABEL=*                                                   
         XC    NAME,NAME                                                        
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),IDNUM                                                  
         CLC   KEY,IO              SAME READ                                    
         JE    SKIPREAD                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO,0                     
         CLC   KEY,IO                                                           
         JNE   USENUM                                                           
*                                                                               
SKIPREAD LA    R4,IO                                                            
         MVI   ELCODE,2            PASSIVE POINTER ELEMENT                      
         BRAS  RE,GETEL                                                         
         JNE   USENUM                                                           
         MVC   NAME,2(R4)          SENDERS ALPHA ID                             
         B     EXIT                                                             
*                                                                               
USENUM   DS    0H                                                               
         EDIT  (B2,IDNUM),(5,NAME)                                              
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CLEAR FILTERS IF CHANGED                                               
***********************************************************************         
PARMCLR  NTR1  BASE=*,LABEL=*                                                   
         XC    PARMBLK(PBLKLNQ),PARMBLK                                         
         XC    DSKAD,DSKAD                                                      
         XC    FDSKAD,FDSKAD                                                    
         J     EXIT                                                             
***********************************************************************         
*        ETI DISPLAY - TYPE DEPENDENT INFO                                      
***********************************************************************         
*                                                                               
USERAPPL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
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
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         CLI   SPEDFLST,C'|'                                                    
         BNE   *+8                                                              
         MVI   SPEDFLST,C' '                                                    
         CLI   SPEDFLND,C'|'                                                    
         BNE   *+8                                                              
         MVI   SPEDFLND,C' '                                                    
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
*                                                                               
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
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         CLI   EDISTTDT,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDISTTDT,C' '                                                    
         CLI   EDISTTDT+6,C'|'                                                  
         BNE   *+8                                                              
         MVI   EDISTTDT+6,C' '                                                  
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
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
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         CLI   EDISTCDT,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDISTCDT,C' '                                                    
         CLI   EDISTCDT+6,C'|'                                                  
         BNE   *+8                                                              
         MVI   EDISTCDT+6,C' '                                                  
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
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
         MVC   SADATES,SPCDRUSR+15                                              
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
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         CLI   EDINTNDT,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDINTNDT,C' '                                                    
         CLI   EDINTNDT+6,C'|'                                                  
         BNE   *+8                                                              
         MVI   EDINTNDT+6,C' '                                                  
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
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
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         CLI   EDINTCDT,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDINTCDT,C' '                                                    
         CLI   EDINTCDT+6,C'|'                                                  
         BNE   *+8                                                              
         MVI   EDINTCDT+6,C' '                                                  
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
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
         BNE   UA240                                                            
         MVC   SAMED,SPCPMED                                                    
         MVC   SACLT,SPCPCLT                                                    
         MVC   SAPRD,SPCPPRD                                                    
         MVC   SAEST,SPCPEST                                                    
         MVC   SASTN,SPCPSTA                                                    
         MVC   SAREQ,SPCPRQST                                                   
         B     EXIT                                                             
*                                                                               
UA240    DS    0H                                                               
         CLI   EDFTYPE,EDFTNDLQ    NET DUNNING LETTERS                          
         BNE   UA300                                                            
         MVC   NVMED,EDINTVMD                                                   
         MVC   NVCLT,EDINTVCL                                                   
         MVC   NVPRD,EDINTVPR                                                   
         MVC   NVSTA,EDIRTVST                                                   
         MVC   NVDATES,EDINTVDT                                                 
         MVC   NVEST,EDISTVES                                                   
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
         BNE   UA430                                                            
UA421    MVC   ROOFF,EDIRCNOF                                                   
         MVC   ROSLP,EDIRCNSP                                                   
         MVC   ROHNM,EDIRCNHN                                                   
         MVC   ROAGY,EDIRCNAG                                                   
         MVC   ROCTY,EDIRCNAO                                                   
         MVC   ROADV,EDIRCNAD                                                   
         MVC   ROSTN,EDIRCNST                                                   
         B     EXIT                                                             
*                                                                               
UA430    CLI   EDFTYPE,EDFTDTRQ    REP DTR                                      
         BNE   UA500                                                            
         MVC   RDSTN,EDIRDTST                                                   
         MVC   RDETRN,EDIRDDTS                                                  
         CLC   EDIRDTSD,SPACES                                                  
         BNH   UA432                                                            
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         CLI   EDIRDTSD,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDIRDTSD,C' '                                                    
         CLI   EDIRDTED,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDIRDTED,C' '                                                    
*****************TEMP PATCH FOR DATA RANGE, 6/18, YYUN********                  
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,EDIRDTSD),(11,RDSTART)                              
UA432    CLC   EDIRDTED,SPACES                                                  
         BNH   UA434                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,EDIRDTED),(11,RDEND)                                
UA434    B     EXIT                                                             
*                                                                               
UA500    DS    0H                                                               
         CLI   EDFSYS,EDFSACCQ    ACC REPORT                                    
         BNE   UA600                                                            
         CLI   EDFTYPE,EDFTORDQ   ORDER                                         
         BNE   UA510                                                            
         CLI   ACFXTYPE,C'P'                                                    
         BNE   UA502                                                            
         MVC   AOCLT,ACFXCLI                                                    
         MVC   AOPRD,ACFXPRO                                                    
         MVC   AOJOB(6),ACFXJOB                                                 
         MVC   AOPONUM,ACFXONUM                                                 
         MVC   AOVEND,ACFXVEND                                                  
         B     EXIT                                                             
UA502    DS    0H                                                               
         MVC   AOJOB(14),ACFXEXP                                                
         MVC   AOPONUM,ACFXONUM                                                 
         MVC   AOVEND,ACFXVEND                                                  
         B     EXIT                                                             
*                                                                               
UA510    CLI   EDFTYPE,EDFTACWQ   ACC TYPE W                                    
         BNE   UA600                                                            
         MVC   AWREPORT,EDFAPPL                                                 
         B     EXIT                                                             
*                                                                               
UA600    DS    0H                                                               
         CLI   EDFSYS,EDFDARRQ     DARE SYSTEM                                  
         BNE   UA700                                                            
         ZIC   RF,SYSID            FACPAK NUMBER                                
         MHI   RF,L'FACITAB                                                     
         L     RE,AFACIDT                                                       
         AR    RF,RE               RF = A(ENTRY IN FACIDTAB)                    
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
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PARAMETERS                                                    
***********************************************************************         
         DS    0H                                                               
PARAMS   NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VALSUMM          CHECK FOR SUMMARY OPTION                     
                                                                                
***********************************************************************         
*        VALIDATE SYSTEM                                                        
***********************************************************************         
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
         BRAS  RE,PARMCLR          CLEAR OLD PARAMETERS                         
         OI    FLAGS,FLNEWSYS                                                   
         XC    SYSFILT,SYSFILT                                                  
*                                                                               
         LAY   R1,STTABLE          SYSTEM AND TYPES TABLE                       
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
         CLI   9(R2),C'2'          2ND PAGE OF HELP                             
         BNE   HELPOUT                                                          
         MVI   HELPPAGE,2                                                       
         B     HELPOUT                                                          
*                                                                               
VOPT15   TM    FLAGS,FLNEWSYS      NEW SYSTEM                                   
         BO    VOPT20                                                           
         CLC   OLDFILT,8(R2)       SAME AS BEFORE                               
         BE    EXIT                                                             
*                                                                               
VOPT20   MVC   OLDFILT,8(R2)       SAVE THE NEW FILTERS                         
         BRAS  RE,PARMCLR          CLEAR PARAMS                                 
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
VOPT100  LAY   R4,OPTTABLE         TABLE OF VALID OPTIONS                       
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
METHF    NTR1                                                                   
         LAY   R4,METHTAB                                                       
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
         LAY   R1,STTABLE          SYSTEM AND TYPES TABLE                       
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
         EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'NOTPRTD'                                          
         BNE   STATF05A                                                         
         MVC   STATFILT,=C'NOTP'                                                
         B     STATFX                                                           
*                                                                               
STATF05A EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'ACC'                                              
         BNE   STATF05B                                                         
         MVC   STATFILT,=C'ACC '                                                
         B     STATFX                                                           
*                                                                               
STATF05B EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'REJ'                                              
         BNE   STATF06B                                                         
         MVC   STATFILT,=C'REJ '                                                
         B     STATFX                                                           
*                                                                               
STATF06B EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'DEL'                                              
         BNE   STATF07A                                                         
         MVC   STATFILT,=C'DEL '                                                
         B     STATFX                                                           
*                                                                               
STATF07A EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'FWRD'                                             
         BNE   STATF07B                                                         
         MVC   STATFILT,=C'FWRD'                                                
         B     STATFX                                                           
*                                                                               
STATF07B EX    R1,*+8              DOESN'T FOLLOW TABLE RULES                   
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'SDEL'                                             
         BNE   STATF10                                                          
         MVC   STATFILT,=C'SDEL'                                                
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
                                                                                
***********************************************************************         
*        DESTINATION FILTER                                                     
***********************************************************************         
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
                                                                                
***********************************************************************         
*        REQUESTOR FILTER                                                       
***********************************************************************         
*                                                                               
REQF     NTR1                                                                   
         MVC   REQFILT,SC2NDFLD                                                 
         OC    REQFILT,SPACES                                                   
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        DARE ORDER# FILTER                                                     
***********************************************************************         
ORDERF   NTR1                                                                   
         CLI   SC2NDLEN,0                                                       
         BNE   ORDF10                                                           
         CLI   SC1STLEN,1                                                       
         BE    ORDF20                                                           
ORDF10   ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG13                                                            
         STC   R1,ORDFLTLN                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ORDERFLT(0),SC2NDFLD                                             
         B     EXIT                                                             
*                                                                               
ORDF20   CLI   DDSTERM,C'Y'        JUST O IS DARE OPTION                        
         BNE   MSG20                                                            
         OI    FLAGS,FLODARE                                                    
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        DARE FILTER - INCOMING DARE ONLY                                       
***********************************************************************         
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
                                                                                
***********************************************************************         
*        DARE FILTER - OUTGOING DARE ONLY                                       
***********************************************************************         
DAREOF   NTR1                                                                   
         CLI   DDSTERM,C'Y'                                                     
         BNE   MSG20                                                            
         OI    FLAGS,FLODARE                                                    
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        BIAS FILTER - INCOMING BIAS ONLY                                       
***********************************************************************         
BIASIN   NTR1                                                                   
         CLI   DDSTERM,C'Y'                                                     
         BNE   EXIT                                                             
         OI    FLAGS,FLBIAS                                                     
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        STATION FILTER                                                         
***********************************************************************         
STATNF   NTR1                                                                   
         ZIC   R1,SC2NDLEN         LENGTH                                       
         SHI   R1,1                                                             
         BM    MSG13                                                            
         STC   R1,STATNLEN         STATION                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STATNFLT(0),SC2NDFLD                                             
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        TIME FILTER                                                            
***********************************************************************         
TIMEF    NTR1                                                                   
         ZIC   R1,SC2NDLEN         LENGTH                                       
         SHI   R1,1                                                             
         BM    MSG13                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIMEFILT(0),SC2NDFLD                                             
         OC    TIMEFILT,SPACES                                                  
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        SUMMARY OPTION                                                         
***********************************************************************         
SUMMF    NTR1                                                                   
         OI    FLAGS,FLSUMM                                                     
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        DATE FILTER                                                            
***********************************************************************         
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
         OI    DMCB,X'40'          VALIDATE AS MM/DD                            
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
         BRAS  RE,ALPHANME                                                      
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
*        REFERENCE NUMBER FILTER - TRANSLATES TO DSKADR FILTER                  
***********************************************************************         
*                                                                               
REFF     NTR1                                                                   
         CLI   SC2NDLEN,0                                                       
         BE    MSG28                                                            
         CLI   SC2NDLEN,8          LEN=8 FOR CUST REF#                          
         BNE   MSG28                                                            
         TM    SC2NDVAL,SCNUMQ     VALID NUMERIC                                
         BNO   MSG28                                                            
         MVC   REFFILT,SC2NDFLD                                                 
         B     EXIT                                                             
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
         L     RF,VCOMFACS                                                      
         L     R2,CDATAMGR-COMFACSD(RF)   GET A(DATAMGR) FOR GETIDS             
         L     RF,CCALLOV-COMFACSD(RF)    GET A(CALLOV)                         
         XC    DMCB(16),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFA' GET GETIDS ADDRESS                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             A(GETIDS)                                    
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
PQSUBF   NTR1                                                                   
         MVC   PQSUBFLT,SC2NDFLD                                                
         B     EXIT                                                             
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
*        ETRAN FILTER FOR REP TYPE D RECORDS                                    
***********************************************************************         
*                                                                               
ETRNF    NTR1                                                                   
         ZIC   R1,SC2NDLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    MSG13A                                                           
         STC   R1,ETRNLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ETRNFILT(0),SC2NDFLD                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REP FILTER                                                             
***********************************************************************         
*                                                                               
REPF     NTR1                                                                   
         CLI   SC2NDLEN,0                                                       
         BNE   MSG20                                                            
         MVI   REPFILT,C'Y'        SEE EDICTR FILE                              
REPFX    B     EXIT                                                             
***********************************************************************         
*        CHECK FOR SUMMARY OPTIONS                                              
***********************************************************************         
*                                                                               
VALSUMM  NTR1                                                                   
         LA    R2,EDSP2H                                                        
         NI    FLAGS,X'FF'-FLSUMM                                               
         XC    DAYFILT,DAYFILT                                                  
         XC    REPFILT,REPFILT                                                  
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
         BNE   VSUM30                                                           
         OI    FLAGS,FLSUMM                                                     
         LA    R2,SCBLKLQ+6(R2)                                                 
         B     VSUM10                                                           
*                                                                               
VSUM30   ZIC   R1,SC1STLEN         LENGTH                                       
         SH    R1,=H'1'                                                         
         BM    VSUMX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'REP'                                              
         BNE   *+8                                                              
         BAS   RE,REPF                                                          
         LA    R2,SCBLKLQ+6(R2)                                                 
         B     VSUM10                                                           
*                                                                               
VSUMX    TM    FLAGS,FLSUMM        ACTION SUMMARY?                              
         BNO   EXIT                                                             
         XC    EDSP1,EDSP1         THEN NO SYSTEM                               
         MVI   EDSP1H+5,0                                                       
         XC    EDSP2,EDSP2         ONLY SUMMARY&DATE&REP OPTIONS USED           
         LA    R2,EDSP2                                                         
         MVC   0(7,R2),=C'SUMMARY'                                              
         LA    R2,7(R2)                                                         
         MVI   EDSP2H+5,7                                                       
*                                                                               
         OC    REPFILT,REPFILT                                                  
         BZ    VSUMXA                                                           
         MVC   0(4,R2),=C',REP'                                                 
         LA    R2,4(R2)                                                         
         SR    RE,RE                                                            
         IC    RE,EDSP2H+5                                                      
         AHI   RE,4                                                             
         STC   RE,EDSP2H+5                                                      
*                                                                               
VSUMXA   OC    DAYFILT,DAYFILT                                                  
         BZ    EXIT                                                             
         MVC   0(6,R2),=C',DATE='                                               
         LA    R2,6(R2)                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DAYFILT),(11,0(R2))                                 
         SR    RE,RE                                                            
         IC    RE,EDSP2H+5                                                      
         AHI   RE,14                                                            
         STC   RE,EDSP2H+5                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        MESSAGE EXITS                                                          
***********************************************************************         
*                                                                               
MSG6     LAY   RE,=C'PLEASE ENTER SYSTEM'                                       
         MVC   EDSMSG(19),0(RE)                                                 
         B     MSGP1X                                                           
MSG7     LAY   RE,=C'INVALID SYSTEM'                                            
         MVC   EDSMSG(14),0(RE)                                                 
         B     MSGP1X                                                           
MSG10    LAY   RE,=C'INVALID DATE FILTER'                                       
         MVC   EDSMSG(19),0(RE)                                                 
         B     MSGP2X                                                           
MSG11    LAY   RE,=C'INVALID OPTIONS FORMAT'                                    
         MVC   EDSMSG(22),0(RE)                                                 
         B     MSGP2X                                                           
MSG12    LAY   RE,=C'IS NOT A VALID STATUS'                                     
         MVC   EDSMSG+5(21),0(RE)                                               
         B     MSGP2X                                                           
MSG13    LAY   RE,=C'MUST SPECIFY DEST=NAME'                                    
         MVC   EDSMSG(22),0(RE)                                                 
         B     MSGP2X                                                           
MSG13A   LAY   RE,=C'MUST SPECIFY ETRAN=????'                                   
         MVC   EDSMSG(22),0(RE)                                                 
         B     MSGP2X                                                           
MSG14    LAY   RE,=C'MUST SPECIFY SENT=NAME/ALL'                                
         MVC   EDSMSG(26),0(RE)                                                 
         B     MSGP2X                                                           
MSG15    LAY   RE,=C'CAN''T SPECIFY BOTH DEST= AND SENT='                       
         MVC   EDSMSG(34),0(RE)                                                 
         B     MSGP2X                                                           
MSG16    LAY   RE,=C'PQREF REQUIRES A VALID NUMERIC'                            
         MVC   EDSMSG(30),0(RE)                                                 
         B     MSGP2X                                                           
MSG17    LAY   RE,=C'IS NOT A VALID FILTER'                                     
         MVC   EDSMSG+6(21),0(RE)                                               
         B     MSGP2X                                                           
MSG18    LAY   RE,=C'IS NOT A VALID TYPE'                                       
         MVC   EDSMSG+2(19),0(RE)                                               
         B     MSGP2X                                                           
MSG19    LAY   RE,=C'IS NOT A VALID METHOD'                                     
         MVC   EDSMSG+2(21),0(RE)                                               
         B     MSGP2X                                                           
MSG20    LAY   RE,=C'INVALID OPTION'                                            
         MVC   EDSMSG(14),0(RE)                                                 
         B     MSGP2X                                                           
MSG21    LAY   RE,=C'THAT''S A BAD DISK ADDRESS, DAVID'                         
         MVC   EDSMSG(32),0(RE)                                                 
         B     MSGP2X                                                           
MSG22    LAY   RE,=C'INVALID AGENCY NAME'                                       
         MVC   EDSMSG(19),0(RE)                                                 
         B     MSGP2X                                                           
MSG24    LAY   RE,=C'CLIENT NAME MUST BE 2 OR 3 CHARACTERS'                     
         MVC   EDSMSG(37),0(RE)                                                 
         B     MSGP2X                                                           
MSG25    LAY   RE,=C'NOT CONNECTED WITH PRIVILEDGED USER ID'                    
         MVC   EDSMSG(38),0(RE)                                                 
         B     MSGP2X                                                           
MSG26    LAY   RE,=C'USER ID IS NOT COMPATIBLE'                                 
         MVC   EDSMSG(25),0(RE)                                                 
         B     MSGP2X                                                           
MSG27    LAY   RE,=C'MUST BE CONNECTED'                                         
         MVC   EDSMSG(17),0(RE)                                                 
         B     MSGP2X                                                           
MSG28    LAY   RE,=C'INVALID REFERENCE NUMBER'                                  
         MVC   EDSMSG(24),0(RE)                                                 
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
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BCR FILTER   (DISPLAY BDE CUSTOMER REF# ON DDS SCREEN ONLY)            
***********************************************************************         
BCRF     NTR1  LABEL=NO                                                         
         CLI   SC2NDLEN,0                                                       
         JNE   MSG20                                                            
         MVI   BCRFILT,C'Y'        BCR                                          
BCRFX    J     EXIT                                                             
***********************************************************************         
*        BCN FILTER   (DISPLAY BDE COMMON NAME ON DDS SCREEN ONLY)              
***********************************************************************         
BCNF     NTR1  LABEL=NO                                                         
         CLI   SC2NDLEN,0                                                       
         JNE   MSG20                                                            
         MVI   BCNFILT,C'Y'        BCN                                          
BCNFX    J     EXIT                                                             
***********************************************************************         
*        NODOC FILTER   (DISPLAY BDE ENTRIES WITHOUT ANY DOC#)                  
***********************************************************************         
DOCF     NTR1  LABEL=NO                                                         
         CLI   SC2NDLEN,0                                                       
         JNE   MSG20                                                            
         MVI   METHFILT,EDIBDFQ    METH=P                                       
         MVI   DOC#FILT,C'N'       NO DOC#                                      
DOCFX    J     EXIT                                                             
***********************************************************************         
*        EC FILTER                                                              
***********************************************************************         
ECF      NTR1  LABEL=NO                                                         
         CLI   SC2NDLEN,0                                                       
         JE    ECF90                                                            
*                                                                               
         ZIC   R1,SC2NDLEN         LENGTH                                       
         CHI   R1,L'ECNFLT                                                      
         JNH   ECF20                                                            
         LHI   R1,L'ECNFLT                                                      
*                                                                               
ECF20    AHI   R1,-1                                                            
         STC   R1,ECNFLEN                                                       
         EXRL  R1,ECFEXMVC                                                      
         J     ECF90                                                            
ECFEXMVC MVC   ECNFLT(0),SC2NDFLD                                               
*                                                                               
ECF90    MVI   METHFILT,EDIBDFQ    METH=P                                       
         MVI   ECNDISPF,C'Y'                                                    
ECFX     J     EXIT                                                             
***********************************************************************         
*        BDE SENDER ID FILTER                                                   
***********************************************************************         
BDESIDF  NTR1  LABEL=NO                                                         
         CLI   SC2NDLEN,0                                                       
         JE    MSG20                                                            
         MVC   BSIDFILT,SC2NDFLD   BDESID=N                                     
BSIDX    J     EXIT                                                             
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                                 
***********************************************************************         
METHTAB  DC    C'E'                EASYLINK                                     
         DC    C'M'                E-MAIL                                       
         DC    C'F'                FTP                                          
         DC    C'D'                DARE                                         
         DC    C'S'                ESS                                          
         DC    C'C'                COLUMBINE                                    
         DC    C'X'                FAXGATE                                      
*        DC    C'B'                BIAS       AHYD REMOVED NOV/15               
         DC    C'B'                BXF                                          
         DC    C'O'                PDF                                          
         DC    C'P'                BDE FTP                                      
         DC    C'R'                EXTREME REACH                                
         DC    C'T'                BDE EMAIL                                    
         DC    C'Z'                ENCODA EC                                    
         DC    C'K'                EDI                                          
         DC    C'I'                ECN                                          
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
         DC    CL8'DDS     ',AL4(DDSF)                                          
         DC    CL8'TIME    ',AL4(TIMEF)                                         
         DC    CL8'AGY     ',AL4(AGYF)                                          
         DC    CL8'USERID  ',AL4(AGYF)                                          
         DC    CL8'DSKADR  ',AL4(DAF)                                           
         DC    CL8'REQ     ',AL4(REQF)                                          
         DC    CL8'REF     ',AL4(REFF)                                          
         DC    CL8'ORDER   ',AL4(ORDERF)                                        
         DC    CL8'STATION ',AL4(STATNF)                                        
         DC    CL8'STN     ',AL4(STATNF)                                        
         DC    CL8'DARE    ',AL4(DAREIF)                                        
*        DC    CL8'BIAS    ',AL4(BIASIN)                                        
         DC    CL8'SUMMARY ',AL4(SUMMF)                                         
         DC    CL8'I       ',AL4(DAREIF)                                        
         DC    CL8'O       ',AL4(DAREOF)                                        
         DC    CL8'CLT     ',AL4(CLTF)                                          
         DC    CL8'ETRAN   ',AL4(ETRNF)                                         
         DC    CL8'EC      ',AL4(ECF)                                           
         DC    CL8'BCR     ',AL4(BCRF)                                          
         DC    CL8'BCN     ',AL4(BCNF)                                          
         DC    CL8'REP     ',AL4(REPF)                                          
         DC    CL8'BDESID  ',AL4(BDESIDF)                                       
         DC    CL8'NODOC   ',AL4(DOCF)                                          
         DC    X'00'                                                            
*                                                                               
STTABLE  DC    AL1(EDFSSPTQ)       SPOT SYSTEM                                  
         DC    AL1(19)             # OF TYPES FOR SPOT                          
         DC    AL1(EDFTSPRQ)       P                                            
         DC    AL1(EDFTSPUQ)       U                                            
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
         DC    AL1(8)              # OF TYPES FOR REP                           
         DC    AL1(EDFTRPRQ)       P                                            
         DC    AL1(EDFTKWXQ)       K                                            
         DC    AL1(EDFTCONQ)       O                                            
         DC    AL1(EDFTCNEQ)       E                                            
         DC    AL1(EDFTCCCQ)       C                                            
         DC    AL1(EDFTSTNQ)       S                                            
         DC    AL1(EDFTDTRQ)       D                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSNETQ)       NET SYSTEM                                   
         DC    AL1(6)              # OF TYPES FOR NET                           
         DC    AL1(EDFTNPRQ)       P                                            
         DC    AL1(EDFTNINQ)       N                                            
         DC    AL1(EDFTCABQ)       C                                            
         DC    AL1(EDFTPAKQ)       R                                            
         DC    AL1(EDFTNRXQ)       X                                            
         DC    AL1(EDFTGENQ)       Z                                            
*                                                                               
         DC    AL1(EDFSACCQ)       ACC SYSTEM                                   
         DC    AL1(4)              # OF TYPES FOR ACC                           
         DC    AL1(EDFTORDQ)       O                                            
         DC    AL1(EDFTCHKQ)       C                                            
         DC    AL1(EDFTACWQ)       W                                            
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
*        FILTER CHECK                                                           
***********************************************************************         
*                                                                               
FILTCK   NTR1  BASE=*,LABEL=*                                                   
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
         CLI   EDFSYS,EDFBIASQ     SKIP INCOMING BIAS RECS IN ETI               
         BE    NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        AGENCY FILTER                                                          
*----------------------------------------------------------------------         
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
*******  TM    EDFSTAT,EDFSTJNK    ALWAYS SHOW JUNK                             
*******  BO    FILT200                                                          
                                                                                
*----------------------------------------------------------------------         
*        SYSTEM FILTER                                                          
*----------------------------------------------------------------------         
FILT10   TM    FLAGS,FLIDARE       INCOMING DARE ONLY                           
         BZ    FILT11                                                           
         CLI   EDFSYS,EDFDAREQ     INCOMING DARE HAS SPECIAL SYSTEM             
         BNE   NO                                                               
         CLC   DIDFILT,SPACES      DARE = DARE ID FILT                          
         BNH   FILT20                                                           
         CLC   DIDFILT,EDFDARE     MATCH ON FIRST 6 CHARS                       
         BNE   NO                                                               
         B     FILT20                                                           
*                                                                               
FILT11   TM    FLAGS,FLBIAS        INCOMING BIAS ONLY                           
         BZ    FILT12                                                           
         CLI   EDFSYS,EDFBIASQ     INCOMING BIAS HAS SPECIAL SYSTEM             
         BNE   NO                                                               
         B     FILT20                                                           
*                                                                               
FILT12   OC    SYSFILT,SYSFILT     SYSTEM CHECK                                 
         BZ    FILT20                                                           
         CLC   EDFSYS,SYSFILT      DESIRED SYSTEM?                              
         BNE   NO                                                               
                                                                                
*----------------------------------------------------------------------         
*        DESTINATION FILTER                                                     
*----------------------------------------------------------------------         
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
                                                                                
*----------------------------------------------------------------------         
*        SENDER FILTER                                                          
*----------------------------------------------------------------------         
FILT30   OC    SENDFILT,SENDFILT   IS THERE A SENDER FILTER                     
         BZ    FILT35              NO                                           
         MVC   IDNUM,EDFPQUID                                                   
         BRAS  RE,ALPHANME         GETS ALPHA SENDER'S ID                       
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
         BRAS  RE,ALPHANME         GETS ALPHA SENDER'S ID                       
         MVC   PRTSEND,NAME                                                     
                                                                                
*----------------------------------------------------------------------         
*        STATUS FILTER                                                          
*----------------------------------------------------------------------         
FILT40   OC    STATFILT,STATFILT   FITER ON STATUS?                             
         BZ    FILT50                                                           
         CLC   =C'NOTP',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT41A                                                          
         TM    EDFSTAT,EDFSTPRT+EDFSTWTG                                        
         BNZ   NO                  MUST BE OFF                                  
         B     FILT48                                                           
*                                                                               
FILT41A  CLC   =C'ACC ',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT41B                                                          
         TM    EDFSTAT,EDFSTSNT                                                 
         BNO   NO                  MUST BE SENT                                 
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN+EDFSTJNK                               
         BNZ   NO                                                               
         B     FILT41C                                                          
*                                                                               
FILT41B  CLC   =C'REJ ',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT41E                                                          
         TM    EDFSTAT,EDFSTCAN                                                 
         BNO   NO                  MUST BE CANCELLED                            
*                                                                               
FILT41C  CLI   EDFMETH,EDIECNQ                                                  
         BNE   FILT41D             MUST BE EASYLINK/ECN TRANSACTION             
         OC    EDFECNTN,EDFECNTN                                                
         BZ    NO                  MUST HAVE A TRACKING NUMBER                  
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   NO                  MUST NOT HAVE A RECEIVED TIME                
         B     FILT49                                                           
*                                                                               
FILT41D  CLI   EDFMETH,EDIEASYQ                                                 
         BNE   FILT41D1                                                         
         OC    EDFEZLED,EDFEZLED                                                
         BZ    NO                  MUST HAVE A LEDGER NUMBER                    
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   NO                  MUST NOT HAVE A RECEIVED TIME                
         B     FILT49                                                           
*                                                                               
FILT41D1 CLI   EDFMETH,EDIEXRRQ                                                 
         BNE   NO                  MUST BE EXTREME REACH                        
         OC    EDFXRRTN,EDFXRRTN                                                
         BZ    NO                  MUST HAVE A TRACKING NUMBER                  
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   NO                  MUST NOT HAVE A RECEIVED TIME                
         B     FILT49                                                           
*                                                                               
FILT41E  CLC   =C'DEL ',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT41G                                                          
         OC    EDFBDRTM,EDFBDRTM                                                
         BZ    NO                  MUST HAVE A SAVED RECEIVED TIME              
*                                                                               
FILT41F  CLI   EDFMETH,EDIBDEQ                                                  
         BE    *+12                MUST BE BDE-EMAIL                            
         CLI   EDFMETH,EDIBDFQ                                                  
         BNE   NO                  MUST BE BDE-FTP                              
         OC    EDFBDDOC,EDFBDDOC                                                
         BZ    NO                  MUST HAVE A DOC ID #                         
         B     FILT49                                                           
*                                                                               
FILT41G  CLC   =C'FWRD',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT41H                                                          
         CLI   EDFMETH,EDIENCOQ                                                 
         BNE   NO                  MUST BE ENCODA                               
         TM    EDFSTAT,EDFSTSNT                                                 
         BNO   NO                  MUST BE SENT                                 
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN+EDFSTJNK                               
         BNZ   NO                                                               
         TM    EDFENFLG,EDDENF02Q                                               
         BZ    NO                  MUST HAVE A FORWARDED BIT ON                 
         B     FILT49                                                           
*                                                                               
FILT41H  CLC   =C'SDEL',STATFILT   DOESN'T FOLLOW TABLE RULES                   
         BNE   FILT42                                                           
         CLI   EDFMETH,EDIENCOQ                                                 
         BNE   NO                  MUST BE ENCODA                               
         TM    EDFSTAT,EDFSTRCV                                                 
         BZ    NO                  MUST BE DELIVERED                            
         B     FILT49                                                           
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
         CLI   EDFMETH,EDIBDEQ     BDE-EMAIL TRANSACTION?                       
         BE    *+12                                                             
         CLI   EDFMETH,EDIBDFQ     BDE-FTP TRANSACTION?                         
         BNE   FILT48                                                           
         OC    EDFBDRTM,EDFBDRTM                                                
         BNZ   NO                  MUST NOT HAVE A SAVED RECEIVED TIME          
*                                                                               
         CLI   EDFMETH,EDIENCOQ    ENCODA TRANSACTION?                          
         BNE   FILT48                                                           
         TM    EDFENFLG,EDDENF02Q                                               
         BO    NO                  MUST NOT HAVE A FORWARDED BIT ON             
*                                                                               
FILT48   CLC   STATFILT,=C'CANX'                                                
         BNE   FILT49                                                           
         TM    EDFSTAT,EDFSTRCV    NOT CANX IF DLVD TOO                         
         BO    NO                                                               
*                                                                               
FILT49   MVC   PRTSTAT,STATFILT                                                 
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
         ZIC   R4,ORDFLTLN                                                      
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   ORDERFLT(0),0(R1)                                                
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
         BZ    FILT150                                                          
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
*        REFENCE NUMBER FILTER - USES DISK ADDRESS FORM                         
*----------------------------------------------------------------------         
*                                                                               
FILT150  OC    DAFILTER,DAFILTER                                                
         BZ    FILT160                                                          
         MVC   EDCTFDSK+3(1),RECNUM     UPDATE RECORD NUMBER                    
         CLC   DAFILTER,EDCTFDSK                                                
         BNE   NO                                                               
*----------------------------------------------------------------------         
*        ETRAN FILTER                                                           
*----------------------------------------------------------------------         
*                                                                               
FILT160  OC    ETRNFILT,ETRNFILT   ETRAN?                                       
         BZ    FILT170                                                          
         CLI   EDFSYS,EDFSREPQ     MUST BE REP SYSTEM                           
         BNE   NO                                                               
         CLI   EDFTYPE,EDFTDTRQ    MUST BE TYPE D                               
         BNE   NO                                                               
         ZIC   R1,ETRNLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EDIRDDTS(0),ETRNFILT                                             
         BNE   NO                                                               
*----------------------------------------------------------------------         
*        EC NUMBER FILTER                                                       
*----------------------------------------------------------------------         
*                                                                               
FILT170  OC    ECNFLT,ECNFLT                                                    
         BNZ   FILT171                                                          
         CLI   ECNDISPF,C'Y'                                                    
         BNE   FILT180                                                          
*                                                                               
FILT171  LA    R4,ECNTABLE         TABLE OF WHERE TO FIND EC#                   
FILT172  CLC   EDFSYS,0(R4)        MATCH SYSTEM                                 
         BNE   FILT174                                                          
         CLC   EDFTYPE,1(R4)       MATCH TYPE                                   
         BE    FILT176                                                          
FILT174  LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILT172                                                          
         B     NO                                                               
FILT176  ZICM  R1,2(R4),2          DISP INTO EDICT REC                          
         AR    R1,R7               R7 = A(EDICT REC)                            
*                                                                               
         CLI   ECNDISPF,C'Y'                                                    
         BNE   FILT178                                                          
         MVC   ECNDISPV,0(R1)                                                   
*                                                                               
FILT178  OC    ECNFLT,ECNFLT                                                    
         BZ    FILT180                                                          
         ZIC   R4,ECNFLEN                                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   ECNFLT(0),0(R1)                                                  
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BDE SENDER ID FILTER                                                   
*----------------------------------------------------------------------         
*                                                                               
FILT180  OC    BSIDFILT,BSIDFILT                                                
         BZ    FILT190                                                          
         CLC   EDFBDESC,BSIDFILT                                                
         BNE   NO                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BDE DOC# ID FILTER (Y/N)                                               
*----------------------------------------------------------------------         
*                                                                               
FILT190  CLI   DOC#FILT,C'N'                                                    
         BNE   FILT200                                                          
         CLC   EDFBDDOC,SPACES                                                  
         BH    NO                                                               
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
ECNTABLE DC    AL1(EDFSREPQ,EDFTCNEQ),AL2(EDIRCNHN-EDFILD)                      
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
         DC    AL1(EDFSNETQ,EDFTNINQ),AL2(NEEDNET-EDFILD)                       
         DC    AL1(EDFSNETQ,EDFTNDLQ),AL2(EDIRTVST-EDFILD)                      
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
         DC    AL1(EDFSNETQ,EDFTNDLQ),AL2(EDINTVCL-EDFILD)                      
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
SUMMARY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    RECSUSED,RECSUSED   CLEAR OUT NUM OF RECS USED FOR DAY           
         LA    R0,TOTALSQ          CLEAR TOTAL ACCUMULATORS                     
         LA    R1,TOTALS                                                        
         ZAP   0(L'TOTALS,R1),=P'0'                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,FIRSTRAC       READ EDICT FILE                              
SUMM10   STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
         STCM  R2,3,LASTRAC                                                     
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
         CLI   EDFSYS,EDFBIASQ     SKIP THESE                                   
         BE    SUMMNX                                                           
         CLI   DDSVERS,C'Y'        DDS VERSION                                  
         BE    SUMM50                                                           
         CLC   EDFPQUID,USERNUM    DEFAULT FILTER ON SENT=USERID                
         BNE   SUMMNX                                                           
         EJECT                                                                  
***********************************************************************         
*        ADD TO TOTALS                                                          
***********************************************************************         
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
         TM    TOTSTAT,EDFSTCAN    IF ON CANX ENTRY IN TABLE                    
         BNO   *+12                                                             
         TM    EDFSTAT,EDFSTRCV    MAKE SURE ITEM NOT DLVD TOO                  
         BO    SUMM70                                                           
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
         SR    R2,R2                                                            
         ICM   R2,3,LASTRAC                                                     
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
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
SUMM120  LA    R6,NXTHEAD+8(R6)                                                 
SUMM122  CLC   PRNAME(6),=C'TOTALS'   TOTALS LINE                               
         BNE   SUMM123                                                          
         MVC   SUMWTNG(L'SUMWTNG+1),=C'======'                                  
         MVC   SUMDLVD(L'SUMDLVD+1),=C'======'                                  
         MVC   SUMSENT(L'SUMSENT+1),=C'======'                                  
         MVC   SUMCANX(L'SUMCANX+1),=C'======'                                  
         MVC   SUMERR(L'SUMERR+1),=C'======'                                    
         MVC   SUMTOT(L'SUMTOT),=C'======'                                      
         LA    R6,NXTHEAD+8(R6)                                                 
         B     SUMM127                                                          
*                                                                               
SUMM123  CLC   PRNAME(10),=C'DARE ORDER'                                        
         BNE   SUMM125                                                          
         CLI   DDSVERS,C'Y'        DDS VERSION                                  
         BNE   SUMM130                                                          
*        LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
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
         CLI   DDSVERS,C'Y'        IF DDS VERSION                               
         BNE   SUMM128             NO                                           
         CLI   PRMETHC,C' '        ANYTHING TO DISPLAY?                         
         BNH   SUMM128             NO                                           
         MVI   SUMMETHC-1,C'('     YES - DISPLAY 1 CHAR METHOD                  
         MVC   SUMMETHC,PRMETHC                                                 
         MVI   SUMMETHC+1,C')'                                                  
*                                                                               
SUMM128  LA    R4,TOTALS                                                        
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
*        LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
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
         CH    R1,=H'90'                                                        
         BH    SUMMX10             MORE THAN 90%!!!!!!!!                        
*                                                                               
         CHI   R1,70                                                            
         BL    SUMMXX              IF LESS THAN 70%, NO PROBLEM                 
*                                  70% <  X < 90%                               
         TIME  DEC                                                              
         C     R0,=XL4'12000000'   AFTER 6PM, OKAY                              
         BH    SUMMXX                                                           
*                                                                               
SUMMX10  MVC   EDSMSG(52),=C'EDICT FILE GETTING DANGEROUSLY FULL: NOTIF+        
               Y AHYD GRP'                                                      
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
*        LA    R6,NXTHEAD+8(R6)    SKIP A COUPLE OF LINES                       
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
         DC    C'M',AL1(EDFSTJNK)                      E-MAIL                   
         DC    AL2(MERR-TOTALS,MTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'M',AL1(EDFSTCAN)                                               
         DC    AL2(MCANX-TOTALS,MTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'M',AL1(EDFSTRCV)                                               
         DC    AL2(MDLVD-TOTALS,MTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'M',AL1(EDFSTSNT)                                               
         DC    AL2(MSENT-TOTALS,MTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'M',AL1(EDFSTWTG)                                               
         DC    AL2(MWTNG-TOTALS,MTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'T',AL1(EDFSTJNK)                      BDE E-MAIL               
         DC    AL2(TERR-TOTALS,TTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'T',AL1(EDFSTCAN)                                               
         DC    AL2(TCANX-TOTALS,TTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'T',AL1(EDFSTRCV)                                               
         DC    AL2(TDLVD-TOTALS,TTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'T',AL1(EDFSTSNT)                                               
         DC    AL2(TSENT-TOTALS,TTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'T',AL1(EDFSTWTG)                                               
         DC    AL2(TWTNG-TOTALS,TTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'P',AL1(EDFSTJNK)                      BDE FTP                  
         DC    AL2(PERR-TOTALS,PTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'P',AL1(EDFSTCAN)                                               
         DC    AL2(PCANX-TOTALS,PTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'P',AL1(EDFSTRCV)                                               
         DC    AL2(PDLVD-TOTALS,PTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'P',AL1(EDFSTSNT)                                               
         DC    AL2(PSENT-TOTALS,PTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'P',AL1(EDFSTWTG)                                               
         DC    AL2(PWTNG-TOTALS,PTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         EJECT                                                                  
*&&DO                                                                           
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
*&&                                                                             
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
         DC    C'B',AL1(EDFSTJNK)                      BXF                      
         DC    AL2(BERR-TOTALS,BTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'B',AL1(EDFSTCAN)                                               
         DC    AL2(BCANX-TOTALS,BTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'B',AL1(EDFSTRCV)                                               
         DC    AL2(BDLVD-TOTALS,BTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'B',AL1(EDFSTSNT)                                               
         DC    AL2(BSENT-TOTALS,BTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'B',AL1(EDFSTWTG)                                               
         DC    AL2(BWTNG-TOTALS,BTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         EJECT                                                                  
*                                                                               
         DC    C'O',AL1(EDFSTJNK)                      PDF                      
         DC    AL2(OERR-TOTALS,OTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'O',AL1(EDFSTCAN)                                               
         DC    AL2(OCANX-TOTALS,OTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'O',AL1(EDFSTRCV)                                               
         DC    AL2(ODLVD-TOTALS,OTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'O',AL1(EDFSTSNT)                                               
         DC    AL2(OSENT-TOTALS,OTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'O',AL1(EDFSTWTG)                                               
         DC    AL2(OWTNG-TOTALS,OTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         EJECT                                                                  
*AH3                                                                            
*&&DO                                                                           
         DC    C'Z',AL1(EDFSTJNK)                      ENCODA EC                
         DC    AL2(ZERR-TOTALS,ZTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'Z',AL1(EDFSTCAN)                                               
         DC    AL2(ZCANX-TOTALS,ZTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'Z',AL1(EDFSTRCV)                                               
         DC    AL2(ZDLVD-TOTALS,ZTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'Z',AL1(EDFSTSNT)                                               
         DC    AL2(ZSENT-TOTALS,ZTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'Z',AL1(EDFSTWTG)                                               
         DC    AL2(ZWTNG-TOTALS,ZTOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*&&                                                                             
**AW                                                                            
*        DC    C'K',AL1(EDFSTJNK)                      EDI                      
*        DC    AL2(KERR-TOTALS,KTOT-TOTALS)                                     
*        DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
*        DC    C'K',AL1(EDFSTCAN)                                               
*        DC    AL2(KCANX-TOTALS,KTOT-TOTALS)                                    
*        DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
*        DC    C'K',AL1(EDFSTRCV)                                               
*        DC    AL2(KDLVD-TOTALS,KTOT-TOTALS)                                    
*        DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
*        DC    C'K',AL1(EDFSTSNT)                                               
*        DC    AL2(KSENT-TOTALS,KTOT-TOTALS)                                    
*        DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
*        DC    C'K',AL1(EDFSTWTG)                                               
*        DC    AL2(KWTNG-TOTALS,KTOT-TOTALS)                                    
*        DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'I',AL1(EDFSTJNK)                      ECN                      
         DC    AL2(IERR-TOTALS,ITOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'I',AL1(EDFSTCAN)                                               
         DC    AL2(ICANX-TOTALS,ITOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'I',AL1(EDFSTRCV)                                               
         DC    AL2(IDLVD-TOTALS,ITOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'I',AL1(EDFSTSNT)                                               
         DC    AL2(ISENT-TOTALS,ITOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'I',AL1(EDFSTWTG)                                               
         DC    AL2(IWTNG-TOTALS,ITOT-TOTALS)                                    
         DC    AL2(WTNGTOT-TOTALS,TOTTOT-TOTALS)                                
*                                                                               
         DC    C'R',AL1(EDFSTJNK)                      FAXGATE                  
         DC    AL2(RERR-TOTALS,XTOT-TOTALS)                                     
         DC    AL2(ERRTOT-TOTALS,TOTTOT-TOTALS)                                 
         DC    C'R',AL1(EDFSTCAN)                                               
         DC    AL2(RCANX-TOTALS,RTOT-TOTALS)                                    
         DC    AL2(CANXTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'R',AL1(EDFSTRCV)                                               
         DC    AL2(RDLVD-TOTALS,RTOT-TOTALS)                                    
         DC    AL2(DLVDTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'R',AL1(EDFSTSNT)                                               
         DC    AL2(RSENT-TOTALS,RTOT-TOTALS)                                    
         DC    AL2(SENTTOT-TOTALS,TOTTOT-TOTALS)                                
         DC    C'R',AL1(EDFSTWTG)                                               
         DC    AL2(RWTNG-TOTALS,RTOT-TOTALS)                                    
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
PRNTOT   DS    0H                                                               
         DC    CL12'EASYLINK',C'E'                                              
         DC    AL2(EWTNG-TOTALS,EDLVD-TOTALS,ESENT-TOTALS)                      
         DC    AL2(ECANX-TOTALS,EERR-TOTALS,ETOT-TOTALS)                        
*                                                                               
         DC    CL12'FTP',C'F'                                                   
         DC    AL2(FWTNG-TOTALS,FDLVD-TOTALS,FSENT-TOTALS)                      
         DC    AL2(FCANX-TOTALS,FERR-TOTALS,FTOT-TOTALS)                        
*                                                                               
         DC    CL12'E-MAIL',C'M'                                                
         DC    AL2(MWTNG-TOTALS,MDLVD-TOTALS,MSENT-TOTALS)                      
         DC    AL2(MCANX-TOTALS,MERR-TOTALS,MTOT-TOTALS)                        
*                                                                               
         DC    CL12'DDS INBOX',C'T'                                             
         DC    AL2(TWTNG-TOTALS,TDLVD-TOTALS,TSENT-TOTALS)                      
         DC    AL2(TCANX-TOTALS,TERR-TOTALS,TTOT-TOTALS)                        
*                                                                               
         DC    CL12'BDE FTP',C'P'                                               
         DC    AL2(PWTNG-TOTALS,PDLVD-TOTALS,PSENT-TOTALS)                      
         DC    AL2(PCANX-TOTALS,PERR-TOTALS,PTOT-TOTALS)                        
*AH3                                                                            
*        DC    CL12'ENCODA EC',C'Z'                                             
*        DC    AL2(ZWTNG-TOTALS,ZDLVD-TOTALS,ZSENT-TOTALS)                      
*        DC    AL2(ZCANX-TOTALS,ZERR-TOTALS,ZTOT-TOTALS)                        
**AW                                                                            
*        DC    CL12'EDI      ',C'K'                                             
*        DC    AL2(KWTNG-TOTALS,KDLVD-TOTALS,KSENT-TOTALS)                      
*        DC    AL2(KCANX-TOTALS,KERR-TOTALS,KTOT-TOTALS)                        
*                                                                               
         DC    CL12'ECN',C'I'                                                   
         DC    AL2(IWTNG-TOTALS,IDLVD-TOTALS,ISENT-TOTALS)                      
         DC    AL2(ICANX-TOTALS,IERR-TOTALS,ITOT-TOTALS)                        
*                                                                               
         DC    CL12'EXTREME RCH',C'R'                                           
         DC    AL2(RWTNG-TOTALS,RDLVD-TOTALS,RSENT-TOTALS)                      
         DC    AL2(RCANX-TOTALS,RERR-TOTALS,RTOT-TOTALS)                        
*                                                                               
*        DC    CL12'FAXGATE',C'X'                                               
*        DC    AL2(XWTNG-TOTALS,XDLVD-TOTALS,XSENT-TOTALS)                      
*        DC    AL2(XCANX-TOTALS,XERR-TOTALS,XTOT-TOTALS)                        
*                                                                               
*        DC    CL12'COLUMBINE',C'C'                                             
*        DC    AL2(CWTNG-TOTALS,CDLVD-TOTALS,CSENT-TOTALS)                      
*        DC    AL2(CCANX-TOTALS,CERR-TOTALS,CTOT-TOTALS)                        
*                                                                               
*        DC    CL12'ESS'                                                        
*        DC    AL2(SWTNG-TOTALS,SDLVD-TOTALS,SSENT-TOTALS)                      
*        DC    AL2(SCANX-TOTALS,SERR-TOTALS,STOT-TOTALS)                        
*                                                                               
         DC    CL12'DARE',C'D'                                                  
         DC    AL2(DWTNG-TOTALS,DDLVD-TOTALS,DSENT-TOTALS)                      
         DC    AL2(DCANX-TOTALS,DERR-TOTALS,DTOT-TOTALS)                        
*AH3                                                                            
         DC    CL12'BXF',C'B'                                                   
         DC    AL2(BWTNG-TOTALS,BDLVD-TOTALS,BSENT-TOTALS)                      
         DC    AL2(BCANX-TOTALS,BERR-TOTALS,BTOT-TOTALS)                        
*                                                                               
*        DC    CL12'BIAS',C'B'                                                  
*        DC    AL2(BWTNG-TOTALS,BDLVD-TOTALS,BSENT-TOTALS)                      
*        DC    AL2(BCANX-TOTALS,BERR-TOTALS,BTOT-TOTALS)                        
*                                                                               
         DC    CL12'PDF',C'O'                                                   
         DC    AL2(OWTNG-TOTALS,ODLVD-TOTALS,OSENT-TOTALS)                      
         DC    AL2(OCANX-TOTALS,OERR-TOTALS,OTOT-TOTALS)                        
*                                                                               
         DC    CL12'UNSENDABLE',C' '                                            
         DC    AL2(UWTNG-TOTALS,UDLVD-TOTALS,USENT-TOTALS)                      
         DC    AL2(UCANX-TOTALS,UERR-TOTALS,UTOT-TOTALS)                        
*                                                                               
         DC    CL12'TOTALS',C' '                                                
         DC    AL2(WTNGTOT-TOTALS,DLVDTOT-TOTALS,SENTTOT-TOTALS)                
         DC    AL2(CANXTOT-TOTALS,ERRTOT-TOTALS,TOTTOT-TOTALS)                  
*                                                                               
         DC    CL12'DARE ORDERS',C' '                                           
         DC    AL2(DOWTNG-TOTALS,DODLVD-TOTALS,DOSENT-TOTALS)                   
         DC    AL2(DOCANX-TOTALS,DOERR-TOTALS,DOTOT-TOTALS)                     
*                                                                               
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
                                                                                
***********************************************************************         
*        OPTTABLE DSECT                                                         
***********************************************************************         
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                 OPTION KEYWORD                               
OPTROUT  DS    XL4                 ROUTINE TO VALIDATE OPTION                   
OPTTLNQ  EQU   *-OPTTABD                                                        
                                                                                
***********************************************************************         
*        TOTTAB DSECT                                                           
***********************************************************************         
TOTTABD  DSECT                                                                  
TOTMETH  DS    CL1                 METHOD                                       
TOTSTAT  DS    XL1                 STATUS                                       
TOTMSTOT DS    XL2                 DISP TO METHOD/STATUS TOTAL                  
TOTMTOT  DS    XL2                 DISP TO METHOD TOTAL                         
TOTSTOT  DS    XL2                 DISP TO STATUS TOTAL                         
TOTOTAL  DS    XL2                 DISP TO TOTAL TOTAL                          
TOTLNQ   EQU   *-TOTTABD                                                        
                                                                                
***********************************************************************         
*        PRNTOT DSECT                                                           
***********************************************************************         
PRNTOTD  DSECT                                                                  
PRNAME   DS    CL12                NAME OF METHOD                               
PRMETHC  DS    CL1                 METHOD SINGLE CHAR                           
PRWTNG   DS    XL2                 DISP TO WTNG TOTAL                           
PRDLVD   DS    XL2                 DISP TO DLVD TOTAL                           
PRSENT   DS    XL2                 DISP TO SENT TOTAL                           
PRCANX   DS    XL2                 DISP TO CANX TOTAL                           
PRERR    DS    XL2                 DISP TO ERR  TOTAL                           
PRTOT    DS    XL2                 DISP TO TOT FOR MTHOD                        
PRNTLNQ  EQU   *-PRNTOTD                                                        
                                                                                
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BASERD   DS    A                                                                
DMCB     DS    8F                                                               
*                                                                               
EDICTFL  DS    A                   A(EDICT FILE)                                
AFACIDT  DS    A                   A(FACIDTAB) TABLE                            
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
TRNSEN   DS    XL1                 TRANSACTION SESSION NUMBER                   
TRNDATEB DS    XL3                 TRANSACTION DATE IN YMD BINARY               
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
MTOT     DS    PL4                 E-MAIL                                       
MWTNG    DS    PL4                                                              
MDLVD    DS    PL4                                                              
MSENT    DS    PL4                                                              
MCANX    DS    PL4                                                              
MERR     DS    PL4                                                              
*                                                                               
TTOT     DS    PL4                 BDE EMAIL                                    
TWTNG    DS    PL4                                                              
TDLVD    DS    PL4                                                              
TSENT    DS    PL4                                                              
TCANX    DS    PL4                                                              
TERR     DS    PL4                                                              
*                                                                               
PTOT     DS    PL4                 BDE EMAIL                                    
PWTNG    DS    PL4                                                              
PDLVD    DS    PL4                                                              
PSENT    DS    PL4                                                              
PCANX    DS    PL4                                                              
PERR     DS    PL4                                                              
*                                                                               
ZTOT     DS    PL4                 ENCODA EC                                    
ZWTNG    DS    PL4                                                              
ZDLVD    DS    PL4                                                              
ZSENT    DS    PL4                                                              
ZCANX    DS    PL4                                                              
ZERR     DS    PL4                                                              
*                                                                               
KTOT     DS    PL4                 EDI                                          
KWTNG    DS    PL4                                                              
KDLVD    DS    PL4                                                              
KSENT    DS    PL4                                                              
KCANX    DS    PL4                                                              
KERR     DS    PL4                                                              
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
BTOT     DS    PL4                 BXF                                          
BWTNG    DS    PL4                                                              
BDLVD    DS    PL4                                                              
BSENT    DS    PL4                                                              
BCANX    DS    PL4                                                              
BERR     DS    PL4                                                              
*                                                                               
OTOT     DS    PL4                 PDF                                          
OWTNG    DS    PL4                                                              
ODLVD    DS    PL4                                                              
OSENT    DS    PL4                                                              
OCANX    DS    PL4                                                              
OERR     DS    PL4                                                              
*                                                                               
ITOT     DS    PL4                 ECN                                          
IWTNG    DS    PL4                                                              
IDLVD    DS    PL4                                                              
ISENT    DS    PL4                                                              
ICANX    DS    PL4                                                              
IERR     DS    PL4                                                              
*                                                                               
RTOT     DS    PL4                 EXTREME REACH                                
RWTNG    DS    PL4                                                              
RDLVD    DS    PL4                                                              
RSENT    DS    PL4                                                              
RCANX    DS    PL4                                                              
RERR     DS    PL4                                                              
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
                                                                                
***********************************************************************         
*        SAVED STORAGE (IN TWAB)                                                
***********************************************************************         
SAVEDSTR DS    0F                  SAVED STORAGE VARIABLES                      
ISITME   DS    CL4                 $EDI INDENTIFIER                             
LASTTBR  DS    F                   LAST TTBR OF THE LAST SCREEN                 
FRSTTBR  DS    F                   FIRST TTBR OF THE LAST SCREEN                
LASTTRN  DS    H                   LAST TRANSACTION NUMBER                      
*                                                                               
PARMBLK  DS    0C                                                               
PQREFILT DS    H                   PRINT QUEUE REFERENCE NUMBER FILTER          
DAFILT   DS    F                   DISK ADR FILTER                              
DAFILTER DS    F                   DISK ADR FILTER FROM REF NUMBER              
REFFILT  DS    CL8                 REF # = DISK ADR FILTER                      
DISPALL  DS    CL1                 DDS DISPLAY ALL ON $EDI                      
PQSUBFLT DS    CL3                 PRINT QUEUE SUB ID FILTER                    
METHFILT DS    CL1                 METHOD OF TRANSMISSION FILTER                
REPFILT  DS    CL1                 DISPLAY EDICTR FILE FILTER                   
TYPEFILT DS    CL1                 TYPE FILTER                                  
TIMEFILT DS    CL5                 TIME FILTER                                  
STATFILT DS    XL4                 STATUS FILTER                                
REQFILT  DS    CL3                 REQUESTOR FILTER                             
DAYFILT  DS    XL3                 DAY FILTER                                   
ORDERFLT DS    CL8                 DARE ORDER NUMBER FILTER                     
ORDFLTLN DS    XL1                 DARE ORDER NUMBER FILTER LENGTH              
DIDFILT  DS    CL6                 DARE ID FILTER                               
STATNFLT DS    CL8                 STATION FILTER                               
STATNLEN DS    XL1                 STATION FLLTER LENGTH                        
ECNFLT   DS    CL8                 EC# FILTER                                   
ECNFLEN  DS    XL1                 EC# FLLTER LENGTH                            
ECNDISPF DS    CL1                 EC# FILTER DISP FLAG                         
ECNDISPV DS    CL8                 EC# RETURN VALUE FOR DISP                    
SENDFILT DS    XL8                 SENDER FILTER                                
SENDLEN  DS    XL1                 SENDER FILTER LENGTH                         
AGYFILT  DS    XL10                AGENCY FILTER                                
AGYFNUM  DS    XL2                 AGENCY FILTER AS NUMBER                      
AGYLEN   DS    XL1                 AGENCY FILTER LENGTH                         
EZLED    DS    CL1                 DISPLAY EASYLINK LEDGER #                    
ELNDIS   DS    CL1                 DISPLAY EASYLINK MAILBOX#                    
DESTFILT DS    CL16                DESTINATION FILTER                           
DESTLEN  DS    CL1                 DESTINATION FILTER LENGTH                    
ETRNFILT DS    CL12                ETRAN FILTER                                 
ETRNLEN  DS    CL1                 ETRAN FILTER LENGTH                          
CLTFILT  DS    CL3                 CLIENT FILTER                                
BSIDFILT DS    CL1                 BDE SENDER ID FILTER                         
BCRFILT  DS    CL1                 SHOW BDE CUSTOMER REF# FILTER                
BCNFILT  DS    CL1                 SHOW BDE COMMON NAME FILTER                  
DOC#FILT DS    CL1                 SHOW BDE ENTRIES W/O DOC#                    
*                                                                               
FLAGS    DS    XL1                                                              
FLODARE  EQU   X'80'               OUTGOING DARE FILTER GIVEN                   
FLIDARE  EQU   X'40'               INCOMING DARE FILTER GIVEN                   
FLDEST   EQU   X'20'               DEST FILTER GIVEN                            
FLSENT   EQU   X'10'               SENT FILTER GIVEN                            
FLBIAS   EQU   X'08'               INCOMING BIAS RECS ONLY                      
FLSUMM   EQU   X'02'               SUMMARY OPTION GIVEN                         
FLNEWSYS EQU   X'01'               CHANGE IN SYSTEM                             
*                                                                               
PBLKLNQ  EQU   *-PARMBLK                                                        
*                                                                               
OLDSYST  DS    CL1                 PREVIOUS SYSTEM                              
FIRST    DS    CL1                 FIRST TIME THROUGH (Y/N)                     
DAYPWOS  DS    CL3                 DATE IN PWOS FORM                            
OLDFILT  DS    CL50                PREVIOUS STRING OF OPTIONAL FILTERS          
*                                                                               
LASTSEN  DS    XL1                 LAST TRANSACTION SESSION NUMBER              
LASTDATB DS    XL3                 LAST TRANSACTION DATE IN YMB BINARY          
*                                                                               
SAVEDL   EQU   *-SAVEDSTR                                                       
                                                                                
***********************************************************************         
*        MORE WORKING STORAGE                                                   
**********************************************************************          
QHDR     DS    A                                                                
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAGE DS    XL1                                                              
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
                                                                                
***********************************************************************         
*        DSECT FOR DISPLAY LINE DATA FOR USERS                                  
***********************************************************************         
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
NVMED    DS    CL1                 LEN=48                                       
         DS    CL1                                                              
NVCLT    DS    CL3                                                              
         DS    CL1                                                              
NVPRD    DS    CL11                                                             
NVEST    DS    CL8                                                              
NVSTA    DS    CL5                                                              
         DS    CL1                                                              
NVDATES  DS    CL17                                                             
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
         ORG   EDUMCPP                                                          
         DS    CL14                                                             
AWREPORT DS    CL13                                                             
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
         ORG   EDUMCPP                                                          
RDSTN    DS    CL5                                                              
         DS    CL1                                                              
RDETRN   DS    CL12                                                             
         DS    CL1                                                              
RDSTART  DS    CL8                                                              
         DS    CL1                                                              
RDEND    DS    CL8                                                              
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
***********************************************************************         
*        DSECT FOR 2ND DISPLAY LINE DATA FOR USERS                              
***********************************************************************         
*                                                                               
         ORG   EDUST                                                            
         DS    CL15                                                             
EDULREF  DS    CL8                 CUST REF NUMB                                
         DS    CL2                                                              
EDULEZJ  DS    CL8                 EZ REF# OR FAXGATE JOB                       
         DS    CL2                                                              
EDULEFTP DS    CL8                 EASYLINK MAILBOX OR FTP #                    
         DS    CL2                                                              
EDULREAS DS    CL24                REASON FOR ERROR                             
         ORG   EDULREAS                                                         
EDULRLFN DS    CL24                DELIVERED FAX NUMBER                         
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
         ORG   DDSLDEST                                                         
DDSLBCN  DS    CL25                BDE COMMON NAME                              
         ORG                                                                    
DDSLFTP  DS    CL5                                                              
         ORG   DDSLTMS                                                          
DDSLDARE DS    CL56                                                             
         DS    CL1                                                              
DDSLDA   DS    CL8                                                              
         DS    CL1                                                              
DDSLERR  DS    CL3                                                              
         DS    CL2                                                              
         ORG   DDSLTMS                                                          
DDSLDR2  DS    CL58                SECOND DARE LINE TOTAL =114                  
*                                                                               
         ORG   DDSLSTAT                                                         
         DS    CL2                 SECOND DDS DISPLAY LINE                      
DDSLQTYP DS    CL2                 PQ REPORT TYPE                               
         DS    CL1                                                              
DDSLBDES DS    CL1                 BDE SENDER LETTER                            
         DS    CL3                                                              
DDSLBCR  DS    CL27                BDE CUSTOMER REF#                            
         ORG   DDSLBCR                                                          
DDSLREAS DS    CL24                REASON FOR ERROR                             
         ORG   DDSLREAS                                                         
DDSLRLFN DS    CL24                DELIVERED FAX NUMBER                         
         DS    CL8                                                              
DDSLDSN  DS    CL6                 DESTINATION SEQUENCE NUMBER                  
         DS    CL1                                                              
DDSLREF  DS    CL8                 CUST REF NUMB                                
         DS    CL1                                                              
DDSLACK2 DS    CL1                 'F' - ENCODA ACK2(FWRD) RECEIVED             
DDSLEZJ  DS    CL8                 EZ REF# OR FAXGATE JOB                       
         DS    CL2                                                              
DDSLEFTP DS    CL8                 EASYLINK MAILBOX OR FTP #                    
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR SUMMARY LINE DATA                                            
***********************************************************************         
*                                                                               
EDSUMMD  DSECT                                                                  
SUMROW   DS    CL12                NAME OF ROW                                  
         DS    CL1                 C'('                                         
SUMMETHC DS    CL1                 METH OF TRANSMISSION                         
         DS    CL1                 C')'                                         
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
       ++INCLUDE CTGENEDICT                                                     
         PRINT OFF                                                              
       ++INCLUDE SPDARDARED                                                     
       ++INCLUDE FACIDTABD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SRETI00   08/29/19'                                      
         END                                                                    
