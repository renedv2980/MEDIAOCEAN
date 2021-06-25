*          DATA SET DMWRKFM    AT LEVEL 012 AS OF 10/07/15                      
*PHASE WRKFMA                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETRET                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE TIMBER                                                                 
*                                                                               
*&&      SET   NOP=N                                                            
*                                                                               
         TITLE 'WKMAINT - WRKFIL FILE MAINTENANCE'                              
         PRINT NOGEN                                                            
WKMAINT  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,WKMAINT,RA,R9,R8,WORK=A(WKWORK)                                
*                                                                               
WKMA1    ST    R1,ACOMRG           SAVE MVS SUPV INFO                           
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    WKMA1C                                                           
         CH    R2,=H'8'                                                         
         BNH   *+8                                                              
         LA    R2,8                                                             
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
WKMA1A   CLI   0(R1),C'0'                                                       
         BE    WKMA1B                                                           
         CLI   0(R1),C'1'                                                       
         BNE   WKMA1C                                                           
         OC    UPSIVAL,0(RF)                                                    
WKMA1B   LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,WKMA1A                                                        
WKMA1C   MVC   UPSIINP,UPSIVAL     SAVE UPSI                                    
         XC    ALOADPT,ALOADPT     CLEAR EXTERNAL LOAD POINT ADDR               
         B     WKMA1X                                                           
UPSITAB  DC    X'8040201008040201'                                              
WKMA1X   EQU   *                                                                
*                                                                               
WKMA2    GOTO1 =V(DATCON),DMCB,(5,DUB),(10,DATEIPL)                             
         GOTO1 (RF),(R1),(4,DATEIPL),(0,DATEYMD)                                
         GOTO1 (RF),(R1),(0,DATEYMD),(2,DATECPR)                                
*                                                                               
         TBIN  SECS                R1 IS TIME IN SECS                           
         SR    R0,R0                                                            
         D     R0,=F'60'           R1 IS TIME IN MINS                           
         SR    R0,R0                                                            
         D     R0,=F'10'           R1 IS TIME IN 10 MIN INCREMENTS              
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,TIMEINC                                                       
*                                                                               
WKMAI3   L     RC,=V(CPRINT)       RC IS PRINTER CONTROL REGISTER               
         USING DPRINT,RC                                                        
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         MVI   CHRULT,C' '                                                      
         L     RF,=A(INFO1)        WORKER FILE MAINTENANCE                      
         MVC   TITLE(28),0(RF)                                                  
         L     RE,=V(BOXAREA)                                                   
         USING BOXD,RE                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         TM    UPSIVAL,X'80'       TEST IF WANT 132 CHR PRINT LINES             
         BZ    WKMAI4                                                           
         MVC   COLSMAX(5),=AL1(132,002,020,035,100)                             
         MVC   BOXWIDTH,=F'132'                                                 
*                                                                               
WKMAI4   L     R1,=A(CIREC)        INITIALISE WRKF FILE BUFFERS                 
         ST    R1,ACIREC                                                        
         L     R1,=A(CXREC)                                                     
         ST    R1,ACXREC                                                        
         XC    CIDATA,CIDATA                                                    
         LA    RE,L'W_INDEX                                                     
         STH   RE,CINDXLN                                                       
         MVC   WRKFID,WRKFIL                                                    
                                                                                
***********************************************************************         
* READ A SET OF INPUT PARAMETER CARDS                                           
***********************************************************************         
GETPARM  CLI   FRSTTIME,C'X'       WAS LAST SET TERMINATED WITH /* CARD         
         BE    EOJ                 YES EOJ                                      
         BH    *+16                                                             
         ZAP   LINE,=P'99'                                                      
         MVC   TITLE+29(16),SPACES                                              
         L     RF,=A(INFO2)        PARAMETER CARDS                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
*                                                                               
GP0      LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(VALPARM)                                                   
         BASR  RE,RF                                                            
         CLI   ERRNUM,0                                                         
         BNE   GPERR               ERROR FOUND IN CARD                          
*                                                                               
         LA    R4,PARMTBL          CHECK FOR REQUIRED & OPTIONAL PARMS          
GP2      TM    2(R4),X'80'                                                      
         BZ    GP2A                                                             
         CLI   0(R4),0             WAS REQUIRED PARM INPUT                      
         BNE   GP2B                YES                                          
         LA    R1,4(R4)            NO- ERROR                                    
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,1                                                         
         B     GPERR                                                            
GP2A     CLI   0(R4),0             WAS OPTIONAL PARM INPUT                      
         BNE   GP2B                YES                                          
         MVC   0(1,R4),1(R4)       NO- SET DEFAULT VALUE                        
GP2B     LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   GP2                                                              
*                                                                               
GP3      CLI   WRKFMAX,1           SET DEFAULT WKID IF ONLY ONE FILE            
         BNE   GP4                                                              
         MVI   WKID,1              SET DEFAULT INTERNAL WRKF FILE NUM           
         MVI   WRKFINP,1           SET ONE WRKF FILE INPUT                      
         OI    WRKFINP+1,X'01'     SET REFERENCED BY WKID=U PARM                
*                                                                               
GP4      CLI   FILE,0              TEST IF ANY FILE RENAMES VIA FILE=           
         BE    GP4X                                                             
         CLC   FILE(1),WRKFINP     NUM RENAMED >= NUM REFERENCED                
         BNL   GP4B                                                             
GP4A     LA    R1,FILE+4           INVALID FILE=                                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP4B     LA    R1,WRKFINP+1        POINT TO LIST OF INPUT FILES REF             
GP4C     TM    0(R1),X'0F'         TEST IF REFERENCED                           
         BZ    *+12                NO                                           
         TM    0(R1),X'10'         YES MUST BE RENAMED VIA FILE=                
         BZ    GP4A                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GP4C                                                             
GP4X     EQU   *                                                                
*                                                                               
GP5      CLI   OUTPUT,0            OUTPUT ONLY FOR MODE=COPY                    
         BE    GP5B                                                             
         CLI   MODE,4                                                           
         BE    GP5B                                                             
GP5A     LA    R1,OUTPUT+4         INVALID OUTPUT=                              
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP5B     TM    INPUT,DISK          CANT HAVE INPUT=OUTPUT=DISK                  
         BZ    *+12                                                             
         TM    OUTPUT,DISK                                                      
         BO    GP5A                                                             
*                                                                               
GPM1     CLI   MODE,1              INIT - MUST DEFINE WRKF ID                   
         BNE   GPM2                                                             
         CLI   WRKFINP,1           MUST BE SINGLE FILE ONLY                     
         BE    GETPARMX                                                         
         MVI   ERRNUM,5                                                         
         B     GPERR                                                            
*                                                                               
GPM2     CLI   MODE,2              PRNT - INPUT DEFAULTS TO DISK                
         BNE   GPM3                                                             
         CLI   INPUT,0                                                          
         BNE   *+8                                                              
         MVI   INPUT,DISK                                                       
         CLI   USER,0              MUST HAVE USER=                              
         BNE   GPM2B                                                            
GPM2A    LA    R1,USER+4           MISSING USER=                                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,1                                                         
         B     GPERR                                                            
GPM2B    B     GETPARMX                                                         
*                                                                               
GPM3     CLI   MODE,3              REPT - REPORT ON WRKF FILE                   
         BNE   GPM4                                                             
         CLI   WRKFINP,1           MUST BE SINGLE FILE ONLY                     
         BE    *+12                                                             
         MVI   ERRNUM,5                                                         
         B     GPERR                                                            
         TM    INPUT,TAPE          MUST BE DISK INPUT                           
         BZ    GPM3B                                                            
GPM3A    LA    R1,INPUT+4          INVALID INPUT=                               
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPM3B    CLI   USER,0              DEFAULT USER TO ALL                          
         BNE   GPM3C                                                            
         MVI   USER,X'80'                                                       
         L     RF,=A(REPTAB)                                                    
         ST    RF,AREPALL                                                       
         LA    RF,L'REPTAB(RF)                                                  
         MVC   0(2,RF),FFS                                                      
GPM3C    B     GETPARMX                                                         
*                                                                               
GPM4     CLI   MODE,4              COPY - MUST HAVE INPUT=                      
         BNE   GPM5                                                             
         CLI   INPUT,0                                                          
         BNE   GPM4B                                                            
GPM4A    LA    R1,INPUT+4          MISSING INPUT=                               
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,1                                                         
         B     GPERR                                                            
GPM4B    CLI   OUTPUT,0                                                         
         BNE   GPM4C                                                            
         TM    INPUT,TAPE                                                       
         BZ    *+12                                                             
         MVI   OUTPUT,DISK                                                      
         B     *+8                                                              
         MVI   OUTPUT,TAPE                                                      
GPM4C    CLI   USER,0              MUST HAVE USER=                              
         BE    GPM2A                                                            
         B     GETPARMX                                                         
*                                                                               
GPM5     DC    H'0'                                                             
*                                                                               
GPERR    L     R1,ERRNUM           POINT TO ERROR INFO WORD                     
*                                                                               
GPERR1   CLI   ERRNUM,1            HIGH ORDER BYTE HAS ERR NUM                  
         BNE   GPERR2                                                           
         L     RF,=A(ERRMSG1)      MISSING PARAM                                
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+26(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR2   CLI   ERRNUM,2                                                         
         BNE   GPERR3                                                           
         L     RF,=A(ERRMSG2)      INVALID SYNTAX                               
         MVC   WORK(60),0(RF)                                                   
         B     GPERRA                                                           
*                                                                               
GPERR3   CLI   ERRNUM,3                                                         
         BNE   GPERR4                                                           
         L     RF,=A(ERRMSG3)      INVALID PARAM                                
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+26(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR4   CLI   ERRNUM,4                                                         
         BNE   GPERR5                                                           
         L     RF,=A(ERRMSG4)      INVALID VALUE FOR PARAM                      
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+36(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR5   CLI   ERRNUM,5                                                         
         BNE   GPERR6                                                           
         L     RF,=A(ERRMSG5)      MUST SPECIFY A SINGLE WRKF ONLY              
         MVC   WORK(60),0(RF)                                                   
         B     GPERRA                                                           
*                                                                               
GPERR6   DC    H'0'                DIE IF UNKNOWN ERROR                         
*                                                                               
GPERRA   GOTO1 =V(PRINTER)         DISPLAY AND PRINT ERROR MESSAGE              
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PUTMSGP                                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   FRSTTIME,C'X'       FLUSH AND PRINT REMAINING CARDS              
         BE    GPERRX                                                           
*                                                                               
GPERRB   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    GPERRX                                                           
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         B     GPERRB                                                           
*                                                                               
GPERRX   SR    RF,RF               EXIT WITH ERRNUM AS RETURN CODE              
         IC    RF,ERRNUM                                                        
         XBASE RC=(RF)                                                          
*                                                                               
GETPARMX B     GPXTRN                                                           
*                                                                               
EOJ      XBASE                                                                  
                                                                                
***********************************************************************         
* LOAD IN EXTERNAL ROUTINE AND PASS CONTROL FOR PRE-PROCESSING                  
***********************************************************************         
GPXTRN   TM    LOAD,YES            WAS AN EXTERNAL ROUTINE SPECIFIED            
         BZ    GPXX                NO                                           
         L     RF,ALOADPT                                                       
         GOTO1 =V(LOADER),DUB,LOADNAME,(RF)                                     
         L     RF,4(R1)                                                         
         LA    RF,0(RF)                                                         
         LTR   RF,RF                                                            
         BNZ   GPX1                                                             
         LA    R1,LOAD+4           SET ERRNUM FOR LOAD PARAMETER                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
*                                                                               
GPX1     ST    RF,ALOADPT          SAVE EXTERNAL MODULE LOAD ADDRESS            
         LA    R1,PLXTRN           PASS CONTROL TO EXTERNAL ROUTINE             
         MVI   0(R1),0             SET FIRST TIME SWITCH                        
         BASR  RE,RF                                                            
*                                                                               
GPXX     DS    0H                                                               
                                                                                
***********************************************************************         
* INITIALISE AND OPEN WRKF FILES AND TAPES FOR THIS MODE                        
***********************************************************************         
OPEN     LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         L     RF,=A(OPNWK)                                                     
         BASR  RE,RF               OPEN ALL WRKF FILES REFERENCED               
*                                                                               
OPEN2    TM    INPUT,TAPE          OPEN INPUT TAPE IF SPECIFIED                 
         BZ    OPEN3                                                            
         BAS   RE,OPNIN                                                         
*                                                                               
OPEN3    TM    OUTPUT,TAPE         OPEN OUTPUT TAPE IF SPECIFIED                
         BZ    OPEN4                                                            
         BAS   RE,OPNOUT                                                        
*                                                                               
OPEN4    GOTO1 =V(PRINTER)                                                      
         L     RF,=A(INFO3)        ACTION MESSAGES                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
         MVC   DUB1,WRKFIL         SET WRKF FILE IN TITLE                       
         MVC   DUB2,DUB1                                                        
         CLI   WRKFINP,1           TEST ONLY ONE WRKF FILE REFERENCED           
         BNE   *+16                                                             
         MVC   DUB1(7),FILEIX                                                   
         MVC   DUB2(7),FILEID                                                   
         MVC   TITLE+29(5),DUB1                                                 
         CLC   DUB1(7),DUB2                                                     
         BE    *+14                                                             
         MVI   TITLE+34,C'='                                                    
         MVC   TITLE+35(7),DUB2                                                 
*                                                                               
OPEN5    CLI   MODE,1              GO TO ROUTINE FOR MODE                       
         BE    INIWK                                                            
*NOP     CLI   MODE,2                                                           
*        BE    PRTWK                                                            
*        CLI   MODE,3                                                           
*        BE    RPTWK                                                            
*        CLI   MODE,4                                                           
*        BE    CPYWK                                                            
         DC    H'0'                                                             
                                                                                
**********************************************************************          
* INITIALISE WRKFIL                                                  *          
**********************************************************************          
INIWK    LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         L     RF,=A(INITWK)                                                    
         BASR  RE,RF               GO TO INITIALISE ROUTINE                     
*                                                                               
INIWK1   CLI   ERRNUM,1            TEST FOR GOOD INITIALISE                     
         BL    INIWKX              YES                                          
         BH    *+12                                                             
         L     RF,=A(ERRMSGA)      ERROR END OF FILE                            
         B     *+8                                                              
         L     RF,=A(ERRMSGB)      ERROR DISK WRITE                             
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+6(5),WRKFID                                                 
         BAS   RE,PUTMSGP                                                       
         DC    H'0'                DIE IF FAIL TO INITIALISE                    
*                                                                               
INIWKX   L     RF,=A(INFO4)        INITIALISED WRKFX                            
         MVC   12(5,RF),FILEIX                                                  
         CLC   FILEIX(7),FILEID    TEST IF WRKF FILE WAS RENAMED                
         BE    *+14                                                             
         MVI   17(RF),C'='                                                      
         MVC   18(7,RF),FILEID                                                  
         BAS   RE,PINFO                                                         
         GOTO1 =V(PRINTER)                                                      
         LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(WKOUT)                                                     
         BASR  RE,RF               PRINT ATTRIBUTES OF NEW WRKFIL               
         B     GETPARM                                                          
         EJECT                                                                  
**********************************************************************          
* COPY FILES FROM DISK/TAPE TO DISK/TAPE                             *          
**********************************************************************          
*&&NOP                                                                          
         SPACE 1                                                                
CPYWK    EQU   *                                                                
         XC    COPYCTRS,COPYCTRS   CLEAR COPY COUNTERS                          
         XC    INDEX,INDEX         AND INDEX                                    
*                                                                               
         LA    RE,WRKFINP          SET NEXT LOCN IN WRKF INPUT LIST             
         ST    RE,AWRKFINP                                                      
         L     R2,AQH              R2=A(RECORD LENGTH HEADER)                   
         L     R3,AQ                                                            
         USING WLHDRD,R3           R3=A(REPORT HEADER PRINT LINE)               
         L     R5,ACIREC                                                        
         USING W_RECD,R5           R5=A(CIREC FOR DISK HDR & DATA)              
*                                                                               
CPYWKFST TM    INPUT,DISK          BUMP TO NEXT INPUT WRKF FILE                 
         BZ    CPYWKNXT                                                         
*                                                                               
         L     RE,AWRKFINP         INPUT=DISK BUMP WRKFINP                      
         LA    RE,1(RE)                                                         
         ST    RE,AWRKFINP                                                      
         CLI   0(RE),X'FF'         TEST IF END OF INPUT LIST                    
         BE    CPYWKA                                                           
*                                                                               
         TM    0(RE),X'0F'         TEST IF WRKF FILE REFERENCED                 
         BZ    CPYWKFST                                                         
*                                                                               
         LA    R0,WRKFINP                                                       
         SR    RE,R0               RE=INTERNAL WRKF FILE NUM                    
         N     RE,=X'0000000F'     REMOVE HIGH BITS                             
         SLL   RE,3                                                             
         L     RF,AWRKFLST         INDEX INTO WRKF FILE LIST                    
         AR    RF,RE                                                            
         MVC   WRKFID,WRKFIL                                                    
         MVC   WRKFID+4(1),1(RF)   SET WRKF FILE ID FOR DATAMGR                 
         MVC   WRKFINT,0(RF)                                                    
         MVC   WRKFEXT,4(RF)                                                    
         MVC   WRKFDTF+1(3),5(RF)                                               
         LA    RF,FILEIX(RE)       SET ORIGINAL DTF NAME                        
         MVC   FILEIX(8),0(RF)                                                  
         LA    RF,FILEID(RE)       SET OVERRIDE DTF NAME                        
         MVC   FILEID(8),0(RF)                                                  
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR WRKF FILE            
         ST    RF,COPYINDX                                                      
         BAS   RE,WKLOCK           ENQUEUE WRKFIL IF COPY FROM DISK             
         XC    INDEX,INDEX                                                      
*                                                                               
CPYWKNXT BAS   RE,GETREPT          GET NEXT REPORT HEADER (USING CXREC)         
         BNZ   CPYWK0B             TEST FOR EOF                                 
*                                                                               
         TM    INPUT,TAPE          END OF INPUT TAPE                            
         BO    CPYWKA                                                           
         BAS   RE,WKUNLK           DEQUEUE AT END OF INPUT WRKF FILE            
         B     CPYWKFST                                                         
*                                                                               
CPYWK0B  L     RF,TOTLREAD         BUMP TOTAL FILES READ                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTLREAD                                                      
         TM    INPUT,TAPE                                                       
         BO    CPYWK0C                                                          
*                                                                               
         L     RE,COPYINDX         BUMP THIS WRKF FILE FILES READ               
         L     RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(RE)                                                         
         EJECT                                                                  
*****************************************                                       
*        TEST FILTERS                   *                                       
*****************************************                                       
         SPACE 1                                                                
CPYWK0C  EQU   *                                                                
         MVC   DUB(2),WLUSRID                                                   
         BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   CPYWKNXT                                                         
         MVC   DUB(1),WLCLASS                                                   
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   CPYWKNXT                                                         
         MVC   DUB(1),WLSTAT                                                    
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   CPYWKNXT                                                         
         MVC   DUB(6),WLSYSPRG                                                  
         BAS   RE,TSTSYSP          FILTER ON FILEID                             
         BNE   CPYWKNXT                                                         
*                                                                               
         CLI   REPFLAG,0           TEST IF REPORT DATE/TIME/NUM INPUT           
         BE    CPYWK0X                                                          
*                                                                               
         CLI   REPFLAG,X'80'       TEST IF CREATE DATE ONLY INPUT               
         BNE   CPYWK0D                                                          
*                                                                               
         CLC   WLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   CPYWKNXT                                                         
         B     CPYWK0X                                                          
*                                                                               
CPYWK0D  TM    REPFLAG,X'01'       TEST IF INDIVIDUAL REPORT                    
         BZ    CPYWK0E                                                          
         CLC   WLFILENO,REPFILN    TEST FILENO MATCHES                          
         BE    CPYWK0X                                                          
         B     CPYWKNXT            NEXT FILE                                    
*                                                                               
CPYWK0E  TM    REPFLAG,X'04'       TEST IF TIME1-TIME2 INPUT                    
         BNO   CPYWK0F                                                          
         CLC   WLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   CPYWKNXT                                                         
         CLC   WLAGELT,REPTIMS+0   TIME MUST BE IN RANGE                        
         BL    CPYWKNXT                                                         
         CLC   WLAGELT,REPTIMS+2                                                
         BH    CPYWKNXT                                                         
         B     CPYWK0X                                                          
*                                                                               
CPYWK0F  TM    REPFLAG,X'02'       TEST IF ONLY ONE TIME                        
         BZ    CPYWK0G                                                          
         CLC   WLAGELD,REPCDATE                                                 
         BH    CPYWKNXT            IGNORE IF CREATED ON LATER DATE              
         BL    CPYWK0X             OK IF CREATED ON PREVIOUS DATE               
         CLC   WLAGELT,REPTIMS+2                                                
         BH    CPYWKNXT                                                         
         B     CPYWK0X                                                          
*                                                                               
CPYWK0G  DC    H'0'                DIE IF UNKNOWN SITUATION                     
*                                                                               
CPYWK0X  MVC   DUB+0(2),WLAGELD                                                 
*NOP*    MVC   DUB+2(2),WLAGEDD                                                 
         BAS   RE,TSTRANGE         FILTER ON CREATED/DEAD DATES                 
         BNE   CPYWKNXT                                                         
         SPACE 1                                                                
CPYWK1   TM    INPUT,TAPE          GET FULL EXTENDED REPORT HEADER              
         BZ    CPYWK1A                                                          
         MVC   0(128,R5),WLINDEX   TAPE ALREADY HAS IT - MOVE TO BUFF           
         B     CPYWK2                                                           
*                                                                               
CPYWK1A  MVC   SAVE(32),WLINDEX                                                 
         LR    RF,R5               COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,=H'14336'                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NXFLAG,X'C0'                                                     
         GOTO1 VDATAMGR,DMCB,SEQ,WRKFID,INDEX,AQ,(R5)                           
         CLI   8(R1),0                                                          
         BNE   CPYWK9                                                           
         CLC   5(3,R3),=C'ERR'     TEST IF ERROR ON 1ST CI READ                 
         BE    CPYWK9                                                           
*                                                                               
CPYWK2   MVC   SAVE(128),WLINDEX   SAVE REPORT DATA                             
         NOP   CPYWKNXT            FILTER ON NON INDEX DATA HERE                
         B     CPYWK3                                                           
*                                                                               
CPYWK3   TM    OUTPUT,TAPE         WRITE REPORT HEADER TO TAPE/DISK             
         BZ    *+12                                                             
         BAS   RE,WOTAPE                                                        
         B     CPYWK4                                                           
*                                                                               
         OI    WLFLAG,WLFLRSET                                                  
         MVC   INDEX,WLINDEX                                                    
         GOTO1 VDATAMGR,DMCB1,=C'OPEN',WRKFIL,INDEX,(R3),(R5)                   
         CLI   8(R1),0                                                          
         BNE   CPYWK9                                                           
         SR    RE,RE                                                            
         IC    RE,WLREPRNO         GET WRKF FILE INTERNAL NUM                   
         SLL   RE,3                                                             
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR WRKF FILE            
         ST    RF,COPYINDX                                                      
*                                                                               
CPYWK4   TM    INPUT,TAPE          GET NEXT INPUT PRINT LINE                    
         BZ    CPYWK4B                                                          
         BAS   RE,RITAPE                                                        
         OC    0(4,R2),0(R2)       TEST EOF                                     
         BNZ   CPYWK5                                                           
CPYWK4A  MVC   0(2,R2),LENEOF      SEQ ERR ON INPUT                             
         MVC   WLSOFLAB,EOFLAB                                                  
         MVC   5(3,R3),=C'ERR'                                                  
         MVC   10(10,R3),0(R3)                                                  
         B     CPYWK5                                                           
*                                                                               
CPYWK4B  OI    NXFLAG,X'80'                                                     
         GOTO1 VDATAMGR,DMCB,SEQ,WRKFID,INDEX,(R3),(R5)                         
         CLI   8(R1),0                                                          
         BNE   CPYWK4A                                                          
         CLC   WLSOFLAB,SOFLAB                                                  
         BE    CPYWK4A                                                          
*                                                                               
CPYWK5   SR    R7,R7               SET R7 NONZERO IF EOF LABEL                  
         CLC   WLSOFLAB,EOFLAB                                                  
         BNE   CPYWK6                                                           
         LA    R7,=C'CLOSE  '      SET NORMAL EOF                               
         CLC   4(3,R3),1(R3)                                                    
         BE    *+8                                                              
         LA    R7,=C'CLO/ERR'      SET ERROR EOF                                
*                                                                               
CPYWK6   EQU   *                                                                
         TM    OUTPUT,TAPE         PUT NEXT OUTPUT PRINT LINE                   
         BZ    CPYWK6A                                                          
         BAS   RE,WOTAPE                                                        
         LTR   R7,R7                                                            
         BNZ   CPYWK8                                                           
         B     CPYWK4                                                           
CPYWK6A  LTR   R7,R7                                                            
         BNZ   CPYWK6B                                                          
         GOTO1 VDATAMGR,DMCB1,ADD,WRKFIL,INDEX,AQ,(R5)                          
         CLI   8(R1),0                                                          
         BE    CPYWK4                                                           
         B     CPYWK9                                                           
CPYWK6B  GOTO1 VDATAMGR,DMCB1,(R7),WRKFIL,INDEX,AQ,(R5)                         
         CLI   8(R1),0                                                          
         BNE   CPYWK9                                                           
*                                                                               
CPYWK8   LA    RF,=C'CLOSE  '      END OF FILE RECORD                           
         CR    R7,RF                                                            
         BNE   CPYWK9                                                           
         L     RF,TOTLCOPY         BUMP TOTAL OF FILES COPIED                   
         LA    RF,1(RF)                                                         
         ST    RF,TOTLCOPY                                                      
         ICM   RE,15,COPYINDX      BUMP THIS WRKF FILE FILES COPIED             
         BZ    CPYWKNXT                                                         
         L     RF,4(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,4(RE)                                                         
         B     CPYWKNXT                                                         
*                                                                               
CPYWK9   DC    H'0'                                                             
         TM    8(R1),X'80'         TEST FOR WRKF FULL                           
         BNO   CPYWK9A                                                          
         DC    H'0'                                                             
         L     RF,=A(ERRMSGA)      ERROR END OF FILE                            
         B     CPYWKX                                                           
*                                                                               
CPYWK9A  L     RF,=A(ERRMSGE)      ERROR IN COPY INDEX                          
         MVC   P(32),0(RF)                                                      
         MVC   P+6(5),WRKFID                                                    
         GOTO1 =V(HEXOUT),P1,SAVE,P+32,20,=C'TOG'                               
         LA    R6,P+76                                                          
         MVC   0(06,R6),=C' DMCB='                                              
         LA    R6,6(R6)                                                         
         L     RE,DMCB                                                          
         MVC   0(7,R6),0(RE)                                                    
         LA    R6,7(R6)                                                         
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
         GOTO1 =V(HEXOUT),P1,DMCB+8,(R6),1                                      
         LA    R6,2(R6)                                                         
         GOTO1 =V(PRINTER)                                                      
         TM    INPUT,TAPE          BACK FOR NEXT REPORT                         
         BZ    CPYWKNXT                                                         
         CLC   0(10,R3),10(R3)     UNLESS EOF ON TAPE CAUSED SEQ ERROR          
         BNE   CPYWKNXT                                                         
*                                                                               
CPYWKA   OC    TOTLCOPY,TOTLCOPY   TEST COPY COUNTER AT EOF                     
         BNZ   CPYWKB                                                           
         L     RF,=A(INFO5)        REPORT NOT FOUND                             
         BAS   RE,PINFO                                                         
         B     CPYWKX                                                           
*                                                                               
CPYWKB   L     RF,=A(INFO6)        TOTAL NNNNN FILES COPIED FROM ..             
         TM    INPUT,TAPE                                                       
         BO    *+8                                                              
         L     RF,=A(INFO7)                                                     
         MVC   WORK(45),0(RF)                                                   
         L     RF,=A(INFO8)        OUT OF NNNNN FILES READ                      
         MVC   WORK+45(35),0(RF)                                                
         MVC   P(80),WORK                                                       
         L     R0,TOTLCOPY         TOTAL FILES COPIED COUNT                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+06(5),DUB                                                      
         L     R0,TOTLREAD         TOTAL FILES READ COUNT                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+52(5),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CPYWKC   TM    INPUT,TAPE          REPEAT COUNTS FOR EACH WRKF FILE             
         BZ    *+10                                                             
         MVC   WORK+45(35),SPACES                                               
         LA    R2,FILEIX+8         R2=A(NEXT WRKF FILE ID)                      
         LA    R3,COPYCTRS+8       R3=A(NEXT READ/COPY COUNTER PAIR)            
         SR    R1,R1                                                            
         IC    R1,WRKFMAX          R1=MAX NUM OF WRKF FILES                     
*                                                                               
CPYWKD   OC    0(8,R3),0(R3)       TEST READ/COPY COUNTERS                      
         BZ    CPYWKD2                                                          
         MVC   P(80),WORK                                                       
         MVC   P(5),0(R2)          SET WRKF FILE ID                             
         L     R0,4(R3)            GET WRKF COPY COUNT                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+06(5),DUB                                                      
         TM    INPUT,TAPE                                                       
         BO    CPYWKD1                                                          
         L     R0,0(R3)            GET WRKF READ COUNT                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+52(5),DUB                                                      
CPYWKD1  GOTO1 =V(PRINTER)                                                      
CPYWKD2  LA    R2,8(R2)            BUMP TO NEXT WRKF FILE ID                    
         LA    R3,8(R3)            BUMP TO NEXT COUNTERS                        
         BCT   R1,CPYWKD                                                        
*                                                                               
CPYWKX   TM    OUTPUT,TAPE         CLOSE OUTPUT TAPE FILE                       
         BZ    *+8                                                              
         BAS   RE,CLSOUT                                                        
         B     GETPARM                                                          
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*        PRINT A WORKER FILE                                         *          
**********************************************************************          
         SPACE 1                                                                
PRTWK    EQU   *                                                                
         XC    COPYCTRS,COPYCTRS   CLEAR COPY COUNTERS                          
         XC    INDEX,INDEX                                                      
*                                                                               
         LA    RE,WRKFINP          SET NEXT LOCN IN WRKF INPUT LIST             
         ST    RE,AWRKFINP                                                      
         L     R2,AQH              R2=A(RECORD LENGTH HEADER)                   
         L     R3,AQ                                                            
         USING WLHDRD,R3           R3=A(REPORT HEADER PRINT LINE)               
         L     R5,ACIREC                                                        
         USING W_RECD,R5           R5=A(CIREC FOR DISK HDR & DATA)              
*                                                                               
PRTWKFST TM    INPUT,DISK          BUMP TO NEXT INPUT WRKF FILE                 
         BZ    PRTWKNXT                                                         
*                                                                               
         L     RE,AWRKFINP                                                      
         LA    RE,1(RE)                                                         
         ST    RE,AWRKFINP                                                      
         CLI   0(RE),X'FF'         TEST IF END OF INPUT LIST                    
         BE    PRTWKA                                                           
*                                                                               
         TM    0(RE),X'0F'         TEST IF WRKF FILE REFERENCED                 
         BZ    PRTWKFST                                                         
*                                                                               
         LA    R0,WRKFINP                                                       
         SR    RE,R0               RE=INTERNAL WRKF FILE NUM                    
         N     RE,=X'0000000F'                                                  
         SLL   RE,3                                                             
         L     RF,AWRKFLST         INDEX INTO WRKF FILE LIST                    
         AR    RF,RE                                                            
         MVC   WRKFID,WRKFIL                                                    
         MVC   WRKFID+4(1),1(RF)   SET WRKF FILE ID FOR DATAMGR                 
         MVC   WRKFINT,0(RF)                                                    
         MVC   WRKFEXT,4(RF)                                                    
         MVC   WRKFDTF+1(3),5(RF)                                               
         LA    RF,FILEIX(RE)       SET ORIGINAL DTF NAME                        
         MVC   FILEIX(8),0(RF)                                                  
         LA    RF,FILEID(RE)       SET OVERRIDE DTF NAME                        
         MVC   FILEID(8),0(RF)                                                  
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR WRKF FILE            
         ST    RF,COPYINDX                                                      
         BAS   RE,WKLOCK           ENQUEUE WRKFIL IF COPY FROM DISK             
         XC    INDEX,INDEX                                                      
*                                                                               
PRTWKNXT BAS   RE,GETREPT          GET NEXT REPORT HEADER (USING CXREC)         
         BNZ   PRTWK0B                                                          
*                                                                               
         TM    INPUT,TAPE          END OF INPUT TAPE                            
         BO    PRTWKA                                                           
         BAS   RE,WKUNLK           DEQUEUE AT END OF INPUT WRKF FILE            
         B     PRTWKFST                                                         
*                                                                               
PRTWK0B  L     RF,TOTLREAD         BUMP TOTAL FILES READ                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTLREAD                                                      
         TM    INPUT,TAPE                                                       
         BO    PRTWK0C                                                          
*                                                                               
         L     RE,COPYINDX         BUMP THIS WRKF FILE FILES READ               
         L     RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(RE)                                                         
         EJECT                                                                  
*****************************************                                       
*        TEST FILTERS                   *                                       
*****************************************                                       
         SPACE 1                                                                
PRTWK0C  EQU   *                                                                
         MVC   DUB(2),WLUSRID                                                   
         BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   PRTWKNXT                                                         
         MVC   DUB(1),WLCLASS                                                   
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   PRTWKNXT                                                         
         MVC   DUB(1),WLSTAT                                                    
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   PRTWKNXT                                                         
*                                                                               
         MVC   DUB(6),WLSYSPRG     FILTER ON FILEID                             
         BAS   RE,TSTSYSP                                                       
         BNE   PRTWKNXT                                                         
*                                                                               
         CLI   REPFLAG,0           TEST IF REPORT DATE/TIME/NUM INPUT           
         BE    PRTWK0X                                                          
         CLI   REPFLAG,X'80'       TEST IF CREATE DATE ONLY INPUT               
         BNE   PRTWK0D                                                          
         CLC   WLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   PRTWKNXT                                                         
         B     PRTWK0X                                                          
PRTWK0D  TM    REPFLAG,X'01'       TEST IF INDIVIDUAL REPORT                    
         BZ    PRTWK0E                                                          
         CLC   WLFILENO,REPFILN                                                 
         BNE   PRTWKNXT                                                         
         B     PRTWK0X                                                          
PRTWK0E  TM    REPFLAG,X'04'       TEST IF TIME1-TIME2 INPUT                    
         BNO   PRTWK0F                                                          
         CLC   WLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   PRTWKNXT                                                         
         CLC   WLAGELT,REPTIMS+0   TIME MUST BE IN RANGE                        
         BL    PRTWKNXT                                                         
         CLC   WLAGELT,REPTIMS+2                                                
         BH    PRTWKNXT                                                         
         B     PRTWK0X                                                          
PRTWK0F  TM    REPFLAG,X'02'       TEST IF ONLY ONE TIME                        
         BZ    PRTWK0G                                                          
         CLC   WLAGELD,REPCDATE                                                 
         BH    PRTWKNXT            IGNORE IF CREATED ON LATER DATE              
         BL    PRTWK0X             OK IF CREATED ON PREVIOUS DATE               
         CLC   WLAGELT,REPTIMS+2                                                
         BH    PRTWKNXT                                                         
         B     PRTWK0X                                                          
PRTWK0G  DC    H'0'                DIE IF UNKNOWN SITUATION                     
PRTWK0X  MVC   DUB+0(2),WLAGELD                                                 
*NOP*    MVC   DUB+2(2),WLAGEDD                                                 
         BAS   RE,TSTRANGE         FILTER ON CREATED/DEAD DATES                 
         BNE   PRTWKNXT                                                         
*                                                                               
PRTWK1   TM    INPUT,TAPE          GET FULL EXTENDED FILE HEADER                
         BZ    PRTWK1A                                                          
         XC    0(256,R5),0(R5)     CLEAR START OF DISK BUFFER                   
         MVC   W_INDEX(20),WLINDEX MAKE BUFFER LOOK LIKE DISK RECORD            
         MVC   W_AGELT,WLAGELT                                                  
         MVC   W_BATTR,WLBATTR                                                  
         B     PRTWK2                                                           
PRTWK1A  MVC   SAVE(32),WLINDEX    SAVE INDEX ENTRY                             
         LR    RF,R5               COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,=H'14336'                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NXFLAG,X'C0'                                                     
         GOTO1 VDATAMGR,DMCB,SEQ,WRKFID,INDEX,(R3),(R5)                         
         CLI   8(R1),0                                                          
         BE    *+6                 NOW HAVE IT FOR DISK                         
         DC    H'0'                                                             
*                                                                               
PRTWK2   MVC   SAVE(128),WLINDEX   SAVE FILE DATA                               
         NOP   PRTWKNXT            FILTER ON NON INDEX DATA HERE                
PRTWK2A  OC    TOTLCOPY,TOTLCOPY   SET ACTION MSG IF FIRST TIME                 
         BNZ   PRTWK3                                                           
         L     RF,=A(INFO9)        FILE DESCRIPTION AND DATA ....               
         BAS   RE,PINFO                                                         
*                                                                               
PRTWK3   ZAP   LINE,=P'99'         PRINT FILE ATTRIBUTES                        
         MVC   REPFILN,W_FILENO                                                 
         MVC   SAVE(10),REPUSER                                                 
         L     R7,=A(REPOUTA)      POINT TO LIST OF ALPHA NAMES                 
*                                                                               
PRTWK3A  MVC   P(16),0(R7)         FILE ID                                      
         MVC   REPUSER(7),W_KEY                                                 
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         FILE CLASS                                   
         MVC   P+19(1),W_CLASS                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         FILE STATUS                                  
         MVC   CISTAT,W_STAT                                                    
         BAS   RE,STATOUT                                                       
         MVC   P+19(3),REPSTATA                                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         FILE NAME                                    
         MVC   P+19(11),W_DESC                                                  
         OC    P+19(11),SPACES                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         FILE MAKER                                   
         MVC   P+19(5),W_MAKER                                                  
         OC    P+19(5),SPACES                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTWK3B  GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         LOCN CREATED                                 
         XC    REPSYSP(5),REPSYSP                                               
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DATE CREATED                                 
         GOTO1 =V(DATCON),DMCB,(2,W_DATEL),(5,P+19)                             
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         TIME CREATED                                 
         MVC   DUB(2),W_TIMEL                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         LIVE RETAIN HOURS                            
         EDIT  (B2,W_RETNL),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DEAD RETAIN HOURS                            
         EDIT  (B2,W_RETND),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DATE RETAINED                                
         GOTO1 =V(DATCON),DMCB,(2,W_AGERD),(5,P+19)                             
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         TIME RETAINED                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,W_AGERT                                                       
         MH    R1,=H'10'                                                        
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTWK3C  GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF RECORDS                               
         MVC   P(16),0(R7)                                                      
         EDIT  (B4,W_RECS),(6,P+19),ALIGN=LEFT                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           AVG RECORD LEN                               
         MVC   P(16),0(R7)                                                      
         EDIT  (B2,W_AVGRL),(3,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF CIS = REGULAR+EXTENSIONS              
         MVC   P(16),0(R7)                                                      
         SR    R0,R0                                                            
         IC    R0,W_NCI                                                         
         SR    R1,R1                                                            
         IC    R1,W_NCIX                                                        
         AR    R0,R1                                                            
         EDIT  (R0),(3,P+19),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTWK3D  GOTO1 =V(PRINTER)                                                      
         OC    W_PRSYM,W_PRSYM     TEST IF FILE EVER PRTD/SENT                  
         BZ    PRTWK3F                                                          
         LA    R7,16(R7)           COUNT PRINTED                                
         MVC   P(16),0(R7)                                                      
         EDIT  (B1,W_PRCNT),(3,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DEVICE PRINTED                               
         MVC   P(16),0(R7)                                                      
         MVC   P+19(8),W_PRSYM                                                  
PRTWK3E  GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DATE PRINTED                                 
         MVC   P(16),0(R7)                                                      
         GOTO1 =V(DATCON),DMCB,(2,W_DATED),(5,P+19)                             
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           TIME PRINTED                                 
         MVC   P(16),0(R7)                                                      
         MVC   DUB(2),W_TIMED                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
PRTWK3F  EQU   *                                                                
*                                                                               
PRTWK3X  MVC   REPUSER(10),SAVE                                                 
         GOTO1 ,DMCB,SEQ,WRKFID,INDEX,(R3),(R5)                                 
*                                                                               
PRTWK4   OC    REPRECS,REPRECS     SET NUM OF PAGES TO PRINT                    
         BNZ   *+10                                                             
         MVC   REPRECS,=X'7FFFFFFF'                                             
         XC    CPAGE,CPAGE                                                      
*                                                                               
PRTWK6   TM    INPUT,TAPE          GET NEXT PRINT LINE                          
         BZ    PRTWK6A                                                          
         BAS   RE,RITAPE                                                        
         OC    0(4,R2),0(R2)       TEST END OF FILE ON TAPE                     
         BNZ   PRTWK6B                                                          
         DC    H'0'                SEQ ERR ON INPUT TAPE                        
PRTWK6A  GOTO1 VDATAMGR,DMCB,SEQ,WRKFID,INDEX,(R3),(R5)                         
         CLI   8(R1),0                                                          
         BNE   PRTWKA                                                           
PRTWK6B  CLC   WLSOFLAB,EOFLAB     TEST FOR EOF LABEL                           
         BE    PRTWKB                                                           
*                                                                               
PRTWK7   MVI   FLAG,0              SET NORMAL PRINT FLAG                        
*                                                                               
PRTWK8   BAS   RE,PLINEQ           PRINT LINE DATA                              
         CLI   FLAG,0                                                           
         BE    PRTWK6                                                           
         B     PRTWKB                                                           
*                                                                               
PRTWKA   TM    8(R1),X'80'         TEST END OF FILE                             
         BO    PRTWKB                                                           
         TM    8(R1),X'40'         TEST DISK ERROR                              
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R2),X'09'                                                      
         MVC   1(12,R2),=C'*DISK*ERROR*'                                        
         LA    R0,9                                                             
         LA    R1,1(R2)                                                         
         MVC   12(12,R1),0(R1)                                                  
         LA    R1,12(R1)                                                        
         BCT   R0,*-10                                                          
         MVI   FLAG,1                                                           
         B     PRTWK8                                                           
*                                                                               
PRTWKB   L     R1,TOTLCOPY         BUMP NUMBER OF FILES PRINTED                 
         LA    R1,1(R1)                                                         
         ST    R1,TOTLCOPY                                                      
         B     PRTWKNXT                                                         
*                                                                               
PRTWKC   OC    TOTLCOPY,TOTLCOPY   TEST FILES PRINTED AT EOF                    
         BNZ   PRTWKX                                                           
         L     RF,=A(INFO5)        FILE NOT FOUND                               
         BAS   RE,PINFO                                                         
         B     GETPARM                                                          
*                                                                               
PRTWKX   ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
**********************************************************************          
*        REPORT ON WORKER STATUS FOR ALL OR A SPECIFIC USER          *          
**********************************************************************          
         SPACE 1                                                                
RPTWK    EQU   *                                                                
         CLI   KDAYS,0             DEFAULT KEEP DAYS                            
         BNE   *+10                                                             
         MVC   REPKDAYS,DEFKDAYS                                                
         CLI   LDAYS,0             DEFAULT LIVE DAYS                            
         BNE   *+10                                                             
         MVC   REPLDAYS,DEFLDAYS                                                
         CLI   DDAYS,0             DEFAULT DEAD DAYS                            
         BNE   *+10                                                             
         MVC   REPDDAYS,DEFDDAYS                                                
*                                                                               
         MVC   DUB1,DATEIPL                                                     
         MVC   TODAY4,DUB1                                                      
         GOTO1 =V(DATCON),DMCB,(4,DUB1),(0,DUB)                                 
         MVC   TODAY0,DUB                                                       
         L     R5,REPKDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPKDATE)                            
         L     R5,REPLDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPLDATE)                            
         L     R5,REPDDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPDDATE)                            
*                                                                               
         CLI   SORT,1              ALPHABETIC (FILE SUBID)                      
         BNE   *+12                                                             
         LA    R5,SORTC1A                                                       
         B     RPTWK1                                                           
         CLI   SORT,2              NUMERIC (FILE SEQ NUM)                       
         BNE   *+12                                                             
         LA    R5,SORTC1N                                                       
         B     RPTWK1                                                           
         CLI   SORT,3              TIME (DATE/TIME CREATED)                     
         BNE   *+12                                                             
         LA    R5,SORTC1T                                                       
         B     RPTWK1                                                           
         DC    H'0'                                                             
RPTWK1   GOTO1 =V(SORTER),DMCB,(R5),SORTC2,0                                    
         B     RPTWK2                                                           
*                                                                               
SORTC1A  DC    CL80'SORT FIELDS=(31,10,A,01,10,A),FORMAT=BI,WORK=1'             
SORTC1N  DC    CL80'SORT FIELDS=(31,10,A,01,02,A,06,02,A,03,04,A,07,04,X        
               A),FORMAT=BI,WORK=1'                                             
SORTC1T  DC    CL80'SORT FIELDS=(31,10,A,01,02,A,19,04,A,03,04,A,07,04,X        
               A),FORMAT=BI,WORK=1'                                             
SORTC2   DC    CL80'RECORD TYPE=F,LENGTH=40'                                    
*                                                                               
RPTWK2   L     R5,ACIREC           R5=A(CI REC)                                 
         USING W_RECD,R5                                                        
         LA    R6,SRTREC           R6=A(SORT REC)                               
         USING SRTRECD,R6                                                       
         ZAP   CINCI,=P'0'                                                      
         SR    R7,R7                                                            
         ICM   R7,3,CICITOT        R7=NUMBER OF DATA CI'S                       
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT                                                     
         AR    R7,R1                                                            
         LH    R1,CICINDX                                                       
         SR    R7,R1                                                            
         MH    R1,CITRKS                                                        
         LA    R1,1(R1)                                                         
         SLL   R1,16                                                            
         ST    R1,CIADDR           SET DISK ADDR OF FIRST DATA CI               
         MVI   CIADDR+2,1                                                       
*                                                                               
         CLI   REORG,YES                                                        
         BNE   RPTWK4                                                           
         CLI   WRITE,YES                                                        
         BNE   RPTWK4                                                           
         BAS   RE,WKLOCK           LOCK WORKER FILE                             
*                                                                               
RPTWK4   XC    SRTREC,SRTREC       INIT SORT REC AND READ CI REC                
         MVC   SRTADDR,CIADDR                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BE    RPTWK6                                                           
         MVC   SRTUSER,=X'FFFF'    ERRORS HAVE HIGH KEY                         
         MVI   SRTFILID,1                                                       
         B     RPTWKA              ALL DISK ERRORS ARE LISTED                   
*                                                                               
RPTWK6   CLI   W_STAT,W_STPU       PURGED HAVE HIGH KEY                         
         BNE   *+14                                                             
         MVC   SRTUSER,=X'FFFF'                                                 
         B     RPTWK8                                                           
         TM    W_STAT,X'01'        TEMP CI'S HAVE HIGH KEY                      
         BZ    *+18                                                             
         MVC   SRTUSER,=X'FFFF'                                                 
         MVI   SRTFILID,3                                                       
         B     RPTWK8                                                           
*                                                                               
RPTWK6A  OC    W_USRID,W_USRID     ALL CI'S MUST HAVE VALID KEY                 
         BZ    RPTWK6P                                                          
         CLC   W_USRID,=X'FFFF'                                                 
         BE    RPTWK6P                                                          
         OC    W_SYSPRG,W_SYSPRG                                                
         BZ    RPTWK6P                                                          
         CLC   W_SYSPRG,SPACES                                                  
         BE    RPTWK6P                                                          
         OC    W_FILENO,W_FILENO                                                
         BZ    RPTWK6P                                                          
         CLC   W_FILENO,MAXSEQ+2                                                
         BH    RPTWK6P                                                          
         GOTO1 DATECHK,W_AGELD     MUST HAVE VALID INDEX CREATION DATE          
         BNE   RPTWK6P                                                          
         GOTO1 DATECHK,W_AGERD     MUST HAVE VALID INDEX RETAIN DATE            
         BH    RPTWK6P                                                          
RPTWK6B  CLI   W_SEQ,1             TEST IF HIGH ORDER CI                        
         BH    RPTWK7              YES                                          
*                                                                               
PRTWK6C  OC    W_RECS,W_RECS       ALL LOW CI'S MUST HAVE VALID DATA            
         BZ    RPTWK6P                                                          
         OC    W_AVGRL,W_AVGRL                                                  
         BZ    RPTWK6P                                                          
         GOTO1 DATECHK,W_DATEL                                                  
         BNE   RPTWK6P                                                          
         GOTO1 TIMECHK,W_TIMEL                                                  
         BH    RPTWK6P                                                          
         GOTO1 DATECHK,W_DATED                                                  
         BH    RPTWK6P                                                          
         GOTO1 TIMECHK,W_TIMED                                                  
         BH    RPTWK6P                                                          
         B     RPTWK7                                                           
RPTWK6P  MVC   SRTUSER,=X'FFFF'    INVALID CI'S HAVE HIGH KEY                   
         MVI   SRTFILID,2                                                       
         B     RPTWK8                                                           
*                                                                               
RPTWK7   MVC   DUB(2),W_USRID      FILTER ON USER ID FOR GOOD CI'S              
         BAS   RE,TSTUSR                                                        
         BNE   RPTWKB                                                           
         MVC   DUB(1),W_CLASS      FILTER ON CLASS                              
         BAS   RE,TSTCLASS                                                      
         BNE   RPTWKB                                                           
         MVC   DUB(1),W_STAT       FILTER ON STATUS                             
         BAS   RE,TSTSTAT                                                       
         BNE   RPTWKB                                                           
         CLI   REPFLAG,X'80'       FILTER ON CREATE DATE                        
         BNE   *+14                                                             
         CLC   W_AGELD,REPCDATE    CREATED DATE MUST MATCH CDATE=VALUE          
         BNE   RPTWKB                                                           
         MVC   DUB+0(2),W_DATEL    FILTER ON CREATED/DEAD DATES                 
         MVC   DUB+2(2),W_DATED                                                 
         BAS   RE,TSTRANGE                                                      
         BNE   RPTWKB                                                           
         MVC   SRTUSER,W_USRID                                                  
         MVC   SRTFILID,W_SYSPRG                                                
         MVC   SRTREPNO,W_FILENO                                                
*NOP     MVC   SRTCLASS,W_CLASS                                                 
         SR    R0,R0                                                            
         IC    R0,W_SEQ                                                         
         CLI   W_SEQ,2             EXTENSION CI'S START WITH SEQNUM=2           
         BL    RPTWK7A                                                          
         TM    W_ATTB,W_ATXCI      TEST IF EXTENSION CI                         
         BZ    *+8                                                              
         AH    R0,=H'254'          ADJUST SO FIRST XTNSN SEQNUM=256             
RPTWK7A  STCM  R0,3,SRTSEQ                                                      
         MVC   SRTSTAT,W_STAT                                                   
         MVC   SRTNEXT,W_CINEXT                                                 
         MVC   SRTDATEC,W_AGELD                                                 
         MVC   SRTTIMEC,W_AGELT                                                 
         B     RPTWKA                                                           
*                                                                               
RPTWK8   MVC   DUB(2),W_USRID      FILTER ON USER FOR OTHER CI'S                
         BAS   RE,TSTUSR                                                        
         BNE   RPTWKB                                                           
         MVC   SRTBAD(8),W_KEY     SAVE KEY OF BAD/TEMP/PRGD CI'S               
*                                                                               
RPTWKA   MVC   SRTKEY(2),SRTUSER   GET ALPHA USER ID AND PUT TO SORT            
         CLC   SRTKEY(2),=X'FFFF'                                               
         BE    RPTWKA2                                                          
         MVC   USERN,SRTUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   SRTKEY,USERA                                                     
RPTWKA2  GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         AP    CINCI,=P'1'                                                      
*                                                                               
RPTWKB   SR    R1,R1               BUMP TO NEXT CI                              
         ICM   R1,3,CIADDR                                                      
         CLM   R1,3,CJSTTRK                                                     
         BL    *+12                                                             
         AH    R1,CJTRKS           PART2  CI                                    
         B     *+8                                                              
         AH    R1,CITRKS           PART1  CI                                    
         STCM  R1,3,CIADDR                                                      
         BCT   R7,RPTWK4                                                        
*                                                                               
RPTWKB1  CP    CINCI,=P'0'         TEST IF EMPTY WORKER FILE                    
         BNE   RPTWKB2                                                          
         L     RF,=A(INFO6)        NO FILES FOUND                               
         BAS   RE,PINFO                                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         GOTO1 (RF),(R1),=C'GET'                                                
         GOTO1 (RF),(R1),=C'GET'   FORCE SORTER COMPLETION                      
         B     GETPARM                                                          
*                                                                               
RPTWKB2  L     RF,=A(INFOA)        WRKFL FILE FOLLOWS                           
         MVC   0(5,RF),FILEIX                                                   
         BAS   RE,PINFO                                                         
         ZAP   LINE,=P'99'                                                      
*                                                                               
RPTWKC   L     RF,=A(HLINE1)       SET UP HEAD/MID LINES FOR FILE               
         MVC   MID1(165),000(RF)                                                
         MVC   MID2(165),165(RF)                                                
         MVC   MID3(165),330(RF)                                                
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         MVI   CHRULT,C' '                                                      
*                                                                               
         L     RE,=V(BOXAREA)      SET UP BOX INFO FOR FILE                     
         USING BOXD,RE                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         LA    RF,BOXROWS-1        POINT TO ROWS-1 TO USE 1 BASE NUMS           
         MVI   03(RF),C'T'                                                      
         MVI   07(RF),C'M'                                                      
         MVI   61(RF),C'B'                                                      
*                                                                               
         LA    R7,BOXCOLS          HANG REPORT LINE DESECT ON COLS              
         USING REPTLD,R7                                                        
         MVI   RLFRSTC,C'L'                                                     
         MVI   RLID-1,C'C'                                                      
         MVI   RLSTAT-1,C'C'                                                    
         MVI   RLRECS-1,C'C'                                                    
         MVI   RLNCI-1,C'C'                                                     
         MVI   RLCRTD-1,C'C'                                                    
         MVI   RLRETD-1,C'C'                                                    
         MVI   RLHOURS-1,C'C'                                                   
         MVI   RLCPL-1,C'C'                                                     
         MVI   RLPCT-1,C'C'                                                     
         MVI   RLDESC-1,C'C'                                                    
         MVI   RLSENT-1,C'C'                                                    
         MVI   RLPSWD-1,C'C'                                                    
         MVI   RLATTB-1,C'C'                                                    
         MVI   RLMAKER-1,C'C'                                                   
         MVI   RLOSTAT-1,C'C'                                                   
         MVI   RLLASTC,C'R'                                                     
*                                                                               
         LA    R7,P                R7=A(REPORT LINE)                            
         USING REPTLD,R7                                                        
         XC    SRTREC,SRTREC                                                    
         ZAP   CINCI,=P'0'                                                      
         L     RE,=A(SUMBUF)       SET NEXT LOCN IN SUMMARY BUFFER              
         ST    RE,ASUMNEXT                                                      
         BAS   RE,CLRFILE          SET FILE REPORT COUNTERS                     
         BAS   RE,CLRUSER          SET USER REPORT COUNTERS                     
*                                                                               
RPTWKD   LA    R6,SRTREC           GET NEXT CI RECORD FROM SORT                 
         MVC   SRTRECL,SRTREC      ----------------------------                 
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+14                                                             
         MVC   SRTREC,0(RF)                                                     
         B     *+14                                                             
         MVC   SRTUSER,=X'FFFF'    SET END OF FILE                              
         MVI   SRTFILID,X'FF'                                                   
         CLC   SRTUSER,=X'FFFF'                                                 
         BE    RPTWKF                                                           
         CLC   SRTREC(8),SRTRECL   TEST FOR NEW REPORT                          
         BNE   RPTWKF                                                           
         SPACE 2                                                                
RPTWKE   AP    CINCI,=P'1'         SAME FILE - BUMP NUM OF CI'S                 
         L     RF,AQBNEXT          ------------------------------               
         MVC   0(L'SRTREC,RF),SRTREC                                            
         LA    RF,L'SRTREC(RF)                                                  
         XC    0(L'SRTREC,RF),0(RF)                                             
         ST    RF,AQBNEXT          BUMP TO NEXT SLOT                            
         B     RPTWKD                                                           
         SPACE 2                                                                
RPTWKF   OC    SRTRECL(2),SRTRECL  NEW FILE                                     
         BZ    RPTWKN              ----------                                   
         L     R6,=A(QBUFF)                                                     
         CLC   0(2,R6),FFS         TEST LAST TIME                               
         BE    RPTWKO              WAS REPTWKP                                  
         MVC   CIADDR(2),SRTADDR   READ FIRST CI REC                            
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACXREC           SAVE FIRST CI DATA IN CXREC                  
         MVC   0(128,R1),0(R5)                                                  
         MVI   CIOK,0              SET FILE OK                                  
         MVC   CISTAT,W_STAT       SAVE OLD STATUS                              
         MVC   CIREPNO,W_FILENO    SAVE OLD SEQUENCE NUMBER                     
         SPACE 2                                                                
RPTWKG   TM    W_STAT,X'01'        ERROR IF FIRST CI IN CREATE STATUS           
         BO    RPTWKGP                                                          
RPTWKG2  ZAP   DUB,CINCI           GET NUMBER OF CIS                            
         CVB   R0,DUB                                                           
         STH   R0,DUB                                                           
         CLI   W_SEQ,0             ERROR IF MORE THAN ONE CI IF SINGLE          
         BNE   RPTWKG3                                                          
         CLC   DUB(2),=H'1'                                                     
         BE    RPTWKG4                                                          
         B     RPTWKGP                                                          
RPTWKG3  CLI   W_SEQ,1             ERROR IF FIRST CI SEQ NUM NOT 1              
         BNE   RPTWKGP                                                          
         L     R6,AQBNEXT          ERROR IF LAST CI SEQ NUM NOT TOTAL           
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
         CLC   SRTSEQ,DUB                                                       
         BNE   RPTWKGP                                                          
RPTWKG4  L     R6,AQBNEXT          ERROR IF LAST CI LINK ADDR NOT ZERO          
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
         OC    SRTNEXT,SRTNEXT                                                  
         BZ    RPTWKGX                                                          
RPTWKGP  MVI   CIOK,1              SET FILE IS IN ERROR                         
         CLI   REORG,YES                                                        
         BNE   RPTWKGX                                                          
         MVI   CISTAT,W_STPU       SET NEW STATUS                               
         MVC   CIREPNO,=X'FFFF'    SET PURGED DUE TO ERROR                      
         B     RPTWKI                                                           
RPTWKGX  EQU   *                                                                
*                                                                               
RPTWKH   CLI   REORG,YES           REORGANISE MODE                              
         BNE   RPTWKJ              ---------------                              
         TM    W_STAT,W_STKE       PROCESS KEEP FILES                           
         BZ    RPTWKH1                                                          
         CLC   W_DESC(7),=C'*LINEUP'                                            
         BE    RPTWKH3                                                          
         CLC   W_DATEL,REPKDATE                                                 
         BNL   RPTWKH3                                                          
         CLC   W_DATED,REPKDATE                                                 
         BNL   RPTWKH3                                                          
         NI    CISTAT,255-W_STKE   SET UNKEEP STATUS                            
*                                                                               
RPTWKH1  TM    W_STAT,W_STLIVE     PROCESS LIVE FILES                           
         BZ    RPTWKH2                                                          
         CLC   W_AGERD,=X'C79F'    TEST PERMANENT RETAIN                        
         BNL   RPTWKH3                                                          
RPTWKH1A CLC   W_DATEL,REPLDATE                                                 
         BNL   RPTWKH3                                                          
         MVI   CISTAT,W_STPU       SET NEW STATUS                               
         MVC   CIREPNO,=X'FFFE'    SET PURGED DUE TO ACTIVE DATE                
         B     RPTWKI                                                           
*                                                                               
RPTWKH2  TM    W_STAT,W_STDEAD     PROCESS DEAD FILES                           
         BZ    RPTWKH3                                                          
         CLC   W_AGERD,=X'C79F'    TEST PERMANENT RETAIN                        
         BNL   RPTWKH3                                                          
         CLC   W_DATED,REPDDATE                                                 
         BNL   RPTWKH3                                                          
         OC    W_DATED,W_DATED     TREAT UNKNOWN PRTD DATE AS ACTV              
         BZ    RPTWKH1A                                                         
         MVI   CISTAT,W_STPU       SET NEW STATUS                               
         MVC   CIREPNO,=X'FFFD'    SET PURGED DUE TO PRINTED DATE               
         B     RPTWKI                                                           
*                                                                               
RPTWKH3  SR    R1,R1               SET NEW SEQUENCE NUMBER                      
         ICM   R1,3,USERSEQ                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,USERSEQ                                                     
*                                                                               
RPTWKI   BAS   RE,REPTUPDT         UPDATE DATA CONTROL INTERVALS                
         SPACE 2                                                                
RPTWKJ   L     R1,ACXREC           RESTORE OLD FIRST CI DATA                    
         MVC   0(128,R5),0(R1)                                                  
         BAS   RE,REPTOUT          PRINT FILE DETAILS                           
         SPACE 2                                                                
RPTWKK   SP    CINCI,=P'1'         UPDATE FILE AND USER TOTALS                  
         CLI   CIOK,0                                                           
         BE    RPTWK0                                                           
         AP    CIERROR,=P'1'       BUMP ERROR COUNTERS                          
         AP    CJERROR,CINCI                                                    
         AP    UIERROR,=P'1'                                                    
         AP    UJERROR,CINCI                                                    
RPTWK0   CLI   CISTAT,W_STPU       DONT INCLUDE NOW PURGED FILES                
         BE    RPTWKKX                                                          
*                                                                               
RPTWKK1  TM    CISTAT,W_STLIVE     TEST LIVE/DEAD STATUS                        
         BZ    RPTWKK2                                                          
         AP    CILIVE,=P'1'        BUMP LIVE COUNTERS                           
         AP    CJLIVE,CINCI                                                     
         AP    UILIVE,=P'1'                                                     
         AP    UJLIVE,CINCI                                                     
         B     RPTWKK3                                                          
RPTWKK2  AP    CIDEAD,=P'1'        BUMP DEAD COUNTERS                           
         AP    CJDEAD,CINCI                                                     
         AP    UIDEAD,=P'1'                                                     
         AP    UJDEAD,CINCI                                                     
*                                                                               
RPTWKK3  CLC   W_AGERD(3),DATECPR  TEST EXPIRED/AVAIL RETAIN DATE               
         BH    RPTWKK4                                                          
         AP    CIXPRD,=P'1'        BUMP EXPIRED COUNTERS                        
         AP    CJXPRD,CINCI                                                     
         AP    UIXPRD,=P'1'                                                     
         AP    UJXPRD,CINCI                                                     
         B     RPTWKK6                                                          
*                                                                               
RPTWKK4  TM    CISTAT,W_STDEAD     TEST NON EXPIRED FOR DEAD/KEEP               
         BZ    RPTWKK5                                                          
         TM    CISTAT,W_STKE                                                    
         BO    RPTWKK5                                                          
         AP    CISEMI,=P'1'        BUMP SEMI INUSE COUNTERS                     
         AP    CJSEMI,CINCI                                                     
         AP    UISEMI,=P'1'                                                     
         AP    UJSEMI,CINCI                                                     
         B     RPTWKK6                                                          
*                                                                               
RPTWKK5  AP    CIRETN,=P'1'        BUMP RETAINED COUNTERS                       
         AP    CJRETN,CINCI                                                     
         AP    UIRETN,=P'1'                                                     
         AP    UJRETN,CINCI                                                     
*                                                                               
RPTWKK6  SR    R0,R0               BUMP RECS                                    
         ICM   R0,15,W_RECS                                                     
         CVD   R0,DUB                                                           
         AP    CRRECS,DUB                                                       
         AP    URRECS,DUB                                                       
*                                                                               
RPTWKKX  ZAP   CINCI,=P'0'         RESET FOR NEXT FILE                          
         L     RF,=A(QBUFF)                                                     
         ST    RF,AQBNEXT                                                       
         CLC   SRTREC(2),SRTRECL   TEST IF LAST FILE FOR USER                   
         BE    RPTWKE              NO                                           
         SPACE 2                                                                
RPTWKL   L     RF,ASUMNEXT         LAST FILE FOR USER                           
         CLC   100(6,RF),FFS       --------------------                         
         BE    RPTWKL1                                                          
         MVC   0(6,RF),REPIDASV    SAVE USER ID NAME                            
         MVC   6(L'CUSER,RF),CUSER SAVE USER COUNTERS                           
         LA    RF,100(RF)                                                       
         ST    RF,ASUMNEXT         BUMP TO NEXT LOCATION                        
         XC    0(6,RF),0(RF)       SET END OF TABLE                             
*                                                                               
RPTWKL1  BAS   RE,OUTUSER          PRINT USER COUNTS                            
         BAS   RE,CLRUSER          CLEAR USER COUNTS                            
         CLC   SRTREC(2),=X'FFFF'                                               
         BE    RPTWKO              LAST FILE FOR LAST USER                      
*                                                                               
RPTWKL2  CLI   COMPACT,YES         COMPACT FORMAT                               
         BE    *+14                YES                                          
         ZAP   LINE,=P'99'         NO NEW USER ON NEW PAGE                      
         B     RPTWKLX                                                          
         ZAP   PACKED,MAXLINE      FIND HOW NEAR END OF PAGE                    
         SP    PACKED,LINE                                                      
         CP    PACKED,=P'7'        ENOUGH ROOM TO START NEW USER                
         BL    RPTWKL3             NO                                           
         MVI   PCTLBOX,C'B'        YES SET TO DRAW BOX BEFORE NEXT              
         B     RPTWKLX                                                          
*                                                                               
RPTWKL3  ZAP   LINE,=P'99'         FORCE NEXT USER TO NEW PAGE                  
*                                                                               
RPTWKLX  EQU   *                                                                
         SPACE 2                                                                
RPTWKN   MVC   SAVE(10),REPUSER    NEW USER ID - GET NEW ID NAME                
         XC    REPUSER(10),REPUSER -----------------------------                
         MVC   REPUSER,SRTREC                                                   
         BAS   RE,IDOUT                                                         
         MVC   REPUSER(10),SAVE                                                 
         MVC   REPIDASV,REPIDA     SAVE NEW USER ID NAME AT REPIDASV            
         ZAP   CINCI,=P'0'         INIT FOR NEW USER                            
         L     RF,=A(QBUFF)                                                     
         ST    RF,AQBNEXT                                                       
         XC    USERSEQ,USERSEQ                                                  
         B     RPTWKE                                                           
         SPACE 2                                                                
RPTWKO   MVI   CHRULT,C' '         END OF FILE                                  
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         L     RE,=V(BOXAREA)                                                   
         USING BOXD,RE                                                          
         MVI   BOXOFF,C'Y'         TURN OFF BOXES                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
         SPACE 2                                                                
RPTWKP   LA    R6,SRTREC           POINT TO SORT RECORD                         
         CLI   SRTFILID,X'FF'                                                   
         BE    RPTWKQ              SORT RECORD IS EOF RECORD                    
         L     RF,=A(QBUFF)                                                     
         MVC   0(L'SRTREC,RF),SRTREC                                            
         XC    L'SRTREC(L'SRTREC,RF),L'SRTREC(RF)                               
         MVI   CISTAT,W_STPU                                                    
         MVC   CIREPNO,SRTBAD+5                                                 
         MVC   FULL(2),SRTADDR                                                  
         MVC   FULL+2(2),=X'0100'                                               
         GOTO1 =V(HEXOUT),DMCB,FULL,DUB1,4,=C'MIX'                              
*                                                                               
RPTWKP0  CLI   SRTFILID,0          TEST IF PURGED                               
         BNE   RPTWKP1                                                          
         OC    CIREPNO,CIREPNO     IGNORE ZERO NUMBERED CI'S                    
         BZ    RPTWKD                                                           
         BAS   RE,REPTUPDT                                                      
         B     RPTWKD                                                           
*                                                                               
RPTWKP1  CLI   SRTFILID,1          TEST DISK ERROR                              
         BNE   RPTWKP2                                                          
         L     RF,=A(ERRMSGC)      DISK READ ERROR                              
         MVC   P(27),0(RF)                                                      
         MVC   P+6(5),WRKFID                                                    
         MVC   P+29(8),DUB1                                                     
         B     RPTWKPA                                                          
*                                                                               
RPTWKP2  CLI   SRTFILID,2          TEST INVALID CI DATA                         
         BNE   RPTWKP3                                                          
         L     RF,=A(ERRMSGD)      INVALID CI DATA                              
         MVC   P(27),0(RF)                                                      
         MVC   P+6(5),WRKFID       SET WRKF FILE ID                             
         MVC   P+29(8),DUB1                                                     
         MVC   CIREPNO,=X'FFFF'                                                 
         BAS   RE,REPTUPDT                                                      
         GOTO1 =V(HEXOUT),DMCB,SRTBAD,P+40,8,=C'MIX'                            
         B     RPTWKPA                                                          
*                                                                               
RPTWKP3  CLI   SRTFILID,3          TEST TEMP CI                                 
         BNE   RPTWKP4                                                          
         BAS   RE,REPTUPDT                                                      
         B     RPTWKD                                                           
*                                                                               
RPTWKP4  DC    H'0'                                                             
*                                                                               
RPTWKPA  GOTO1 =V(PRINTER)                                                      
         B     RPTWKD                                                           
         SPACE 2                                                                
RPTWKQ   CLI   REORG,YES           REBUILD INDEX IF REORG MODE                  
         BNE   RPTWKR                                                           
         CLI   WRITE,YES                                                        
         BNE   RPTWKR                                                           
         BAS   RE,REPTNDX                                                       
         BAS   RE,WKUNLK           UNLOCK WORKER FILE                           
         SPACE 2                                                                
RPTWKR   EQU   *                                                                
         L     RF,=A(INFOB)        WRKFL FILE SUMMARY FOLLOWS                   
         MVC   0(5,RF),FILEIX                                                   
         BAS   RE,PINFO                                                         
         ZAP   LINE,=P'99'                                                      
         L     RF,=A(SLINE1)       SET UP HEAD/MID LINES FOR SUMMARY            
         MVC   MID1(165),000(RF)                                                
         MVC   MID2(165),165(RF)                                                
         MVC   MID3(165),330(RF)                                                
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         MVI   CHRULT,C' '                                                      
*                                                                               
         L     RE,=V(BOXAREA)      SET UP BOX INFO FOR SUMMARY                  
         USING BOXD,RE                                                          
         XC    BOXYORN(7),BOXYORN  RESET FOR NEW SET OF BOXES                   
         MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         LA    RF,BOXROWS-1        POINT TO ROWS-1 TO USE 1 BASE NUMS           
         MVI   03(RF),C'T'                                                      
         MVI   07(RF),C'M'                                                      
         MVI   61(RF),C'B'                                                      
*                                                                               
         LA    R7,BOXCOLS          HANG REPORT LINE DESECT ON COLS              
         USING SMRYLD,R7                                                        
         MVI   SLFRSTC,C'L'                                                     
         MVI   SLRTOTL-1,C'C'                                                   
         MVI   SLRECS-1,C'C'                                                    
         MVI   SLNCI-1,C'C'                                                     
         MVI   SLMIDL1,C'C'       SEPERATOR FOR LIVE/DEAD/ERROR                 
         MVI   SLRLIVE-1,C'C'                                                   
         MVI   SLRDEAD-1,C'C'                                                   
         MVI   SLRERROR-1,C'C'                                                  
         MVI   SLMIDL2,C'C'       SEPERATOR FOR XPRD/SEMI/RETN                  
         MVI   SLRXPRD-1,C'C'                                                   
         MVI   SLRSEMI-1,C'C'                                                   
         MVI   SLRRETN-1,C'C'                                                   
         MVI   SLLASTC,C'R'                                                     
         SPACE 2                                                                
RPTQS    LA    R7,P                R7=A(REPORT SUMMARY LINE)                    
         USING SMRYLD,R7                                                        
         L     RE,=A(SUMBUF)       SET NEXT LOCN IN SUMMARY BUFFER              
         ST    RE,ASUMNEXT                                                      
WRKFS1   MVC   REPIDASV,0(RE)                                                   
         OC    0(6,RE),0(RE)       TEST END OF SUMMARY BUFFER                   
         BZ    WRKFS2                                                           
         MVC   CUSER,6(RE)         RESTORE USER COUNTS                          
         BAS   RE,SMRYOUT          PRINT USER COUNTS                            
         L     RE,ASUMNEXT                                                      
         LA    RE,100(RE)          BUMP TO NEXT USER                            
         ST    RE,ASUMNEXT                                                      
         B     WRKFS1                                                           
WRKFS2   BAS   RE,SMRYOUT          PRINT GRAND TOTALS                           
         SPACE 2                                                                
RPTWKX   MVI   CHRULT,C' '         END OF FILE                                  
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         L     RE,=V(BOXAREA)                                                   
         USING BOXD,RE                                                          
         MVI   BOXOFF,C'Y'         TURN OFF BOXES                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
         LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(WKOUT)                                                     
         BASR  RE,RF               PRINT ATTRIBUTES OF WRKF FILE                
         ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
         EJECT                                                                  
REPTOUT  NTR1                      PRINT FILE DATA ON ONE LINE                  
         USING REPTLD,R7                                                        
         ZAP   FULL,MAXLINE        FIND HOW NEAR END OF PAGE                    
         SP    FULL,LINE                                                        
         CP    FULL,=P'3'          ENOUGH ROOM FOR SUBTOTAL                     
         BH    *+10                YES                                          
         ZAP   LINE,=P'99'         NO SET TO PRINT ON NEXT PAGE                 
*                                                                               
ROUTUS   MVC   RLUSER,REPIDASV     USER ID NAME                                 
*                                                                               
ROUTID   MVC   SAVE(10),REPUSER    FILE ID AND SEQUENCE NUMBER                  
         XC    REPUSER(10),REPUSER                                              
         MVC   REPSYSP,W_SYSPRG                                                 
         MVC   REPSUBP,W_SUBPRG                                                 
         MVC   REPDAY,W_DAY                                                     
         MVC   REPCLAS,W_CLASS                                                  
         MVC   REPFILN,CIREPNO                                                  
         CLC   CIREPNO,=X'FFF0'    TEST IF PURGED FILE                          
         BL    *+10                                                             
         XC    REPFILN,REPFILN     YES WE SHOW NO SEQUENCE HERE                 
         CLI   CIOK,0                                                           
         BE    *+10                                                             
         MVC   REPFILN,W_FILENO    SET OLD REP NUM IF ERROR                     
         BAS   RE,IDOUT                                                         
         MVC   RLID,REPIDA                                                      
         CLI   CIOK,0                                                           
         BE    *+8                                                              
         MVI   RLID+3,C'*'         SET EYECATCHER FOR ERROR                     
         MVC   REPUSER(10),SAVE                                                 
*                                                                               
         BAS   RE,STATOUT                                                       
         MVC   RLSTAT,REPSTATA                                                  
*                                                                               
         EDIT  (P4,CINCI),(5,RLNCI)                                             
*                                                                               
ROUTDLI  MVC   DUB,SPACES          LIVE DATE/TIME                               
         LA    R2,RLCRTD                                                        
         GOTO1 =V(DATCON),DMCB,(2,W_AGELD),(8,DUB)                              
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
         XC    DUB(2),DUB                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,W_AGELT                                                     
         SLL   R1,2                                                             
         D     R0,=F'3'            CONVERT CREATE TIME TO SECONDS               
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT CREATE TIME TO MINUTES               
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT CREATE TIME TO HOURS                 
         STC   R0,DUB+1                                                         
         STC   R1,DUB                                                           
ROUTDLIX BAS   RE,TIMEOUT                                                       
         MVC   06(2,R2),DUB+3                                                   
         MVC   08(2,R2),DUB+6                                                   
*                                                                               
ROUTDRE  MVC   DUB,SPACES          RETAIN DATE/TIME                             
         LA    R2,RLRETD                                                        
         GOTO1 =V(DATCON),DMCB,(2,W_AGERD),(8,DUB)                              
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
         XC    DUB(2),DUB                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,W_AGERT                                                       
         MH    R1,=H'10'           CONVERT 10MINS TO HHMM                       
         CH    R1,=H'1440'                                                      
         BH    ROUTDRE1                                                         
         D     R0,=F'60'                                                        
         STC   R0,DUB+1                                                         
         STC   R1,DUB                                                           
ROUTDRE1 BAS   RE,TIMEOUT                                                       
         MVC   06(2,R2),DUB+3                                                   
         MVC   08(2,R2),DUB+6                                                   
*                                                                               
ROUTLNS  EDIT  (B4,W_RECS),(7,RLRECS)                                           
*                                                                               
ROUTHR   LA    R2,RLHOURS          LIVE AND DEAD RETAIN HOURS                   
         CLC   W_RETNL,=X'FFFF'                                                 
         BL    *+14                                                             
         MVC   0(4,R2),=C'PERM'                                                 
         B     ROUTHR1                                                          
         EDIT  (B2,W_RETNL),(4,0(R2))                                           
ROUTHR1  CLC   W_RETND,=X'FFFF'                                                 
         BL    *+14                                                             
         MVC   5(4,R2),=C'PERM'                                                 
         B     ROUTHRX                                                          
         EDIT  (B2,W_RETND),(4,5(R2))                                           
ROUTHRX  EQU   *                                                                
*                                                                               
ROUTPCT  ZAP   DUB,CINCI           COMPUTE PERCENTAGE                           
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         MH    RF,CJTRKS                                                        
         AH    RF,CITRKS                                                        
         MH    RF,CIHIREC                                                       
         LH    RE,CIBLKLN                                                       
         SH    RE,=H'49'                                                        
         MR    RE,RE               RE&RF=MAX CAPACITY                           
         LH    R1,W_AVGRL                                                       
         LA    R1,2(R1)                                                         
         SR    R0,R0                                                            
         ICM   R0,15,W_RECS                                                     
         MH    R0,=H'100'                                                       
         MR    R0,R0               R0&R1=ACT CAPACITY * 100                     
         DR    R0,RF                                                            
         SLDL  R0,32                                                            
         CH    R0,=H'100'          ADJUST FOR APPROX                            
         BNH   *+8                                                              
         LH    R0,=H'100'                                                       
*                                                                               
         EDIT  (R0),(3,RLPCT),ZERO=NOBLANK                                      
         EDIT  (B2,W_AVGRL),(6,RLCPL)                                           
         MVC   RLDESC,W_DESC                                                    
*                                                                               
ROUTDDE  MVC   DUB,SPACES          DEAD DATE/TIME/LOCATION/COPYS                
         LA    R2,RLSENT                                                        
         MVC   00(5,R2),DOTS       SET DEFAULT IMAGE IF NOT DEAD                
         MVC   06(4,R2),DOTS                                                    
         MVC   11(8,R2),DOTS                                                    
         MVC   20(1,R2),DOTS                                                    
         OC    W_DATED,W_DATED     NO DATA IF NEVER BEEN MADE DEAD              
         BZ    ROUTDDEX                                                         
ROUTDDE1 GOTO1 =V(DATCON),DMCB,(2,W_DATED),(8,DUB)                              
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
         MVC   DUB(2),W_TIMED                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   06(2,R2),DUB+3                                                   
         MVC   08(2,R2),DUB+6                                                   
ROUTDDE2 CLI   W_PRCNT,0           NO DATA IF NEVER BEEN PRINTED                
         BE    ROUTDDEX                                                         
         MVC   11(8,R2),W_PRSYM                                                 
ROUTDDE4 MVC   20(1,R2),W_PRCNT                                                 
         OI    20(R2),C'0'                                                      
ROUTDDEX EQU   *                                                                
*                                                                               
         MVC   RLPSWD,W_PSWD                                                    
*                                                                               
ROUTAT   LA    R2,RLATTB           FILE ATTRIBUTES                              
         TM    W_ATTB,W_ATERR                                                   
         BZ    *+8                                                              
         MVI   0(R2),C'E'                                                       
         LA    R2,1(R2)                                                         
         LA    R2,1(R2)                                                         
         LA    R2,1(R2)                                                         
         MVC   RLMAKER,W_MAKER                                                  
         OC    RLMAKER,SPACES                                                   
*                                                                               
ROUTOLD  CLI   REORG,YES           OLD STATUS IF REORG                          
         BNE   ROUTOLDX                                                         
         MVC   FLAG,CISTAT         SAVE NEW STATUS                              
         MVC   CISTAT,W_STAT       SET OLD STATUS                               
         BAS   RE,STATOUT                                                       
         CLI   CIOK,0                                                           
         BNE   *+14                                                             
         MVC   RLOSTAT,REPSTATA                                                 
         B     ROUTOLD1                                                         
         MVC   RLOSTAT-1(4),=C'*ER*'                                            
ROUTOLD1 MVC   CISTAT,FLAG         RESTORE NEW STATUS                           
ROUTOLDX EQU   *                                                                
*                                                                               
         GOTO1 =V(PRINTER)                                                      
REPTOUTX XIT1                                                                   
         SPACE 2                                                                
REPTEDIT CVB   R0,DUB                                                           
         EDIT  (R0),(5,P+19),ALIGN=LEFT                                         
         OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         BR    R7                                                               
         EJECT                                                                  
OUTUSER  NTR1                      PRINT USER FILE COUNTS                       
*                                                                               
         MVC   P,SPACES            MINI BOX LINE FOR USER COUNTS                
         MVC   RLUSER-1(1),SPACES                                               
         MVC   RLUSER,SPACES                                                    
         MVI   RLID-1,LEFTT                                                     
         MVC   RLID,USCORES                                                     
         MVI   RLSTAT-1,CROSS                                                   
         MVC   RLSTAT,USCORES                                                   
         MVI   RLRECS-1,CROSS                                                   
         MVC   RLRECS,USCORES                                                   
         MVI   RLNCI-1,CROSS                                                    
         MVC   RLNCI,USCORES                                                    
         MVI   RLCRTD-1,RIGHTT                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   RLUSER,SPACES       SET USER ID NAME TO SPACES                   
         MVC   RLID(4),=C'TOT='    SET TOTAL NUM OF FILES                       
         ZAP   PACKED,UILIVE                                                    
         AP    PACKED,UIDEAD                                                    
         EDIT  (P6,PACKED),(5,RLID+4),ALIGN=LEFT                                
*                                                                               
         ZAP   PACKED,URRECS                                                    
         EDIT  (P6,PACKED),(7,RLRECS)                                           
*                                                                               
         ZAP   PACKED,UILIVE       SET TOTAL NUM OF CI'S                        
         AP    PACKED,UIDEAD                                                    
         AP    PACKED,UJLIVE                                                    
         AP    PACKED,UJDEAD                                                    
         EDIT  (P6,PACKED),(5,RLNCI)                                            
*                                                                               
OUTUSERX GOTO1 =V(PRINTER)         PRINT USER TOTALS                            
         XIT1                                                                   
         SPACE 2                                                                
CLRUSER  NTR1                      CLEAR USER FILE COUNTER                      
         LA    RF,CUSER                                                         
         LA    R0,14                                                            
         B     SETCTRS                                                          
CLRFILE  NTR1                      CLEAR FILE FILE COUNTERS                     
         LA    RF,CFILE                                                         
         LA    R0,14                                                            
         B     SETCTRS                                                          
SETCTRS  ZAP   0(6,RF),=P'0'                                                    
         LA    RF,6(RF)                                                         
         BCT   R0,SETCTRS                                                       
         XIT1                                                                   
         EJECT                                                                  
SMRYOUT  NTR1                      PRINT USER SUMMARY LINE                      
         USING SMRYLD,R7                                                        
         ZAP   FULL,MAXLINE        FIND HOW NEAR END OF PAGE                    
         SP    FULL,LINE                                                        
         CP    FULL,=P'3'          ENOUGH ROOM FOR GRAND TOTAL                  
         BH    *+10                YES                                          
         ZAP   LINE,=P'99'         NO SET TO PRINT ON NEXT PAGE                 
*                                                                               
SOUT1    OC    REPIDASV,REPIDASV   TEST IF CALL FOR GRAND TOTAL                 
         BZ    SOUT2               YES                                          
         MVC   SLUSER,REPIDASV     NO SET USER ID                               
         B     SOUT3                                                            
*                                                                               
SOUT2    MVI   PCTLBOX,C'B'        PRINT GRAND TOTAL                            
         MVC   SLUSER(5),=C'TOTAL'                                              
         MVC   CUSER,CFILE         SET USER COUNTS TO GRAND TOTAL               
*                                                                               
SOUT3    ZAP   DUB1,UILIVE         TOTAL NUMBER OF FILES                        
         AP    DUB1,UIDEAD                                                      
         AP    DUB1,UIERROR                                                     
         ZAP   DUB2,CILIVE                                                      
         AP    DUB2,CIDEAD                                                      
         AP    DUB2,CIERROR                                                     
         ZAP   CITOTAL,DUB2        SAVE TOTAL NUMBER OF FILES                   
         OC    REPIDASV,REPIDASV                                                
         BNZ   SOUT4                                                            
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         CVD   R0,DUB2             SET GRAND TOTAL PCT VALUE                    
SOUT4    LA    R2,SLRTOTL                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,URRECS         TOTAL NUMBER OF RECS                         
         ZAP   DUB2,CRRECS                                                      
         LA    R2,SLRECS                                                        
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UJLIVE         TOTAL PART2 CIS                              
         AP    DUB1,UJDEAD                                                      
         AP    DUB1,UJERROR                                                     
         ZAP   DUB2,CJLIVE                                                      
         AP    DUB2,CJDEAD                                                      
         AP    DUB2,CJERROR                                                     
         ZAP   CJTOTAL,DUB2        SAVE TOTAL NUMBER OF PART2 CIS               
         OC    REPIDASV,REPIDASV                                                
         BNZ   SOUT5                                                            
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         CVD   R0,DUB2             SET GRAND TOTAL PCT VALUE                    
SOUT5    LA    R2,SLNCI                                                         
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UILIVE         NUMBER OF LIVE FILES                         
         ZAP   DUB2,CILIVE                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRLIVE                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIDEAD         NUMBER OF DEAD FILES                         
         ZAP   DUB2,CIDEAD                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRDEAD                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIERROR        NUMBER OF ERROR FILES                        
         ZAP   DUB2,CIERROR                                                     
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRERROR                                                      
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIXPRD         NUMBER OF EXPIRED FILES                      
         ZAP   DUB2,CIXPRD                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRXPRD                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UISEMI         NUMBER OF SEMI EXPIRED FILES                 
         ZAP   DUB2,CISEMI                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRSEMI                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIRETN         NUMBER OF RETAINED FILES                     
         ZAP   DUB2,CIRETN                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRRETN                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
SOUTX    GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         SPACE 2                                                                
SOUTEDIT CP    DUB1,=P'0'          EDIT DUB1 AND DUB1/DUB2 AS PCT               
         BNE   SOUTED1                                                          
         MVI   06(R2),C'0'         SET ZERO/ZERO DEFAULTS                       
         MVC   08(4,R2),=C'  .0'                                                
         B     SOUTEDX                                                          
SOUTED1  EDIT  (P8,DUB1),(7,00(R2))                                             
         CP    DUB2,=P'0'                                                       
         BNE   SOUTED2                                                          
         MVC   08(4,R2),=C'  .0'   SET NNNN/ZERO DEFAULT                        
         B     SOUTEDX                                                          
SOUTED2  CP    DUB1,DUB2           TEST IF DUB1 SAME AS DUB2                    
         BL    SOUTED3                                                          
         MVC   08(4,R2),=C' ...'   SET 100 PCT DEFAULT VALUE                    
         B     SOUTEDX                                                          
SOUTED3  XC    DUB,DUB             CLEAR INFRONT OF DUB1                        
         MP    DUB(16),=P'1000'                                                 
         DP    DUB(16),DUB2                                                     
         AP    DUB1,DUB1           DOUBLE REMAINDER                             
         CP    DUB1,DUB2                                                        
         BL    *+10                                                             
         AP    DUB,=P'1'           ROUND UP ANSWER IN DUB                       
         MVC   DUB1,DUB                                                         
         EDIT  (P8,DUB1),(4,08(R2)),1                                           
SOUTEDX  BR    R3                                                               
         EJECT                                                                  
* GET/PUT MESSAGE TO OPERATOR AND OPTIONALLY TO PRINTER                         
* MESSAGE IS IN WORK AND REPLY IS RETURNED IN OPERANS                           
*                                                                               
PUTMSG   MVI   DMCB+16,X'01'       SET PUT FLAG                                 
         B     PAGM1                                                            
PUTMSGP  MVI   DMCB+16,X'81'       SET PUT AND PRINT FLAG                       
         B     PAGM1                                                            
GETMSG   MVI   DMCB+16,X'02'       SET GET FLAG                                 
         B     PAGM1                                                            
PAGMSG   MVI   DMCB+16,X'03'       SET PUT AND GET FLAGS                        
         B     PAGM1                                                            
*                                                                               
PAGM1    ST    RE,PGSAVRE                                                       
         TM    DMCB+16,X'01'       PUT MSG IN OPERMSG TO CONSOLE                
         BZ    PAGM2                                                            
         GOTO1 =V(LOGIO),DMCB,1,(60,WORK)                                       
         TM    DMCB+16,X'80'       AND SEE IF PRINTER ALSO                      
         BZ    PAGM2                                                            
         MVC   P(60),WORK                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PAGM2    TM    DMCB+16,X'02'       GET REPLY INTO OPERANS                       
         BZ    PAGMX                                                            
         MVC   OPERANS,SPACES                                                   
         GOTO1 =V(LOGIO),DMCB,0,(8,OPERANS)                                     
PAGMX    L     RE,PGSAVRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
PINFO    ST    RE,PGSAVRE          PRINT INFO MSG AT RF                         
         MVC   P(60),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         L     RE,PGSAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
DATECHK  OC    0(2,R1),0(R1)       VALIDATE CMPRSD DATE FIELD AT R1             
         BNZ   *+12                                                             
         LH    RF,=H'-1'           CC=LOW IF NO DATE                            
         B     DCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID DATE                      
         SR    R0,R0               UNCOMPRESS DATE INTO DUB                     
         ICM   R0,3,0(R1)                                                       
         SRDL  R0,5                                                             
         SRL   R1,27                                                            
         STC   R1,DUB+2                                                         
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         STC   R1,DUB+1            R1=MONTH                                     
         STC   R0,DUB+0                                                         
         CLI   DUB+0,84            RANGE CHECK YEAR                             
         BL    DCHKX                                                            
         CLI   DUB+0,99                                                         
         BH    DCHKX                                                            
         CLI   DUB+1,01            RANGE CHECK MONTH                            
         BL    DCHKX                                                            
         CLI   DUB+1,12                                                         
         BH    DCHKX                                                            
         CLI   DUB+2,01            RANGE CHECK DAY                              
         BL    DCHKX                                                            
         IC    R1,DCHKT-1(R1)      GET NUMBER OF DAYS IN MONTH                  
         CLI   DUB+1,02                                                         
         BNE   *+16                                                             
         TM    DUB+0,X'03'         TEST FEB IN LEAP YEAR                        
         BNZ   *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,DUB+3                                                         
         CLC   DUB+2(1),DUB+3                                                   
         BH    DCHKX                                                            
         SR    RF,RF               CC=EQUAL IF DATE OK                          
DCHKX    LTR   RF,RF                                                            
         BR    RE                                                               
DCHKT    DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
         SPACE 2                                                                
TIMECHK  OC    0(2,R1),0(R1)       VALIDATE TIME FIELD AT R1                    
         BNZ   *+12                                                             
         LH    RF,=H'-1'           CC=LOW IF NO TIME                            
         B     TCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID TIME                      
         CLI   0(R1),23                                                         
         BH    TCHKX                                                            
         CLI   1(R1),59                                                         
         BH    TCHKX                                                            
         SR    RF,RF               CC=EQUAL IF OK                               
TCHKX    LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
WKLOCK   LA    R0,C'E'             LOCK/UNLOCK WRKFIL FILE                      
         B     *+8                                                              
WKUNLK   LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,((R0),WRKFID)                                          
         L     RE,CISAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
TSTUSR   ST    RE,DUB+4            TEST IF USERID AT DUB(2) IS WANTED           
         L     RE,=A(REPTAB)                                                    
*                                                                               
TSTUSR2  CLC   0(2,RE),=X'FFFF'    SEARCH REPTAB FOR USER                       
         BE    TSTUSR4                                                          
         MVC   DUB+2(2),0(RE)                                                   
         NI    DUB+2,X'7F'                                                      
         CLC   DUB(2),DUB+2                                                     
         BE    TSTUSR6                                                          
         LA    RE,L'REPTAB(RE)                                                  
         B     TSTUSR2                                                          
*                                                                               
TSTUSR4  TM    USER,X'80'          USERID NOT IN TABLE                          
         BZ    TSTUSRN                                                          
         L     RE,AREPALL          USE ALL VALUE IF USER=ALL INPUT              
         B     TSTUSRY                                                          
*                                                                               
TSTUSR6  TM    0(RE),X'80'         USERID IS IN TABLE                           
         BZ    TSTUSRY                                                          
*                                                                               
TSTUSRN  LA    RE,1                CC=NEQ IF USERID NOT WANTED                  
         B     TSTUSRX                                                          
TSTUSRY  MVC   REPUSER(10),0(RE)   CC=EQL IF USERID IS WANTED                   
         OC    REPSYSP,REPSYSP                                                  
         BNZ   TSTUSRY1                                                         
         MVC   REPSYSP(6),=C'******'                                            
TSTUSRY1 MVC   REPCDATE,10(RE)     SET FILE CREATE DATE                         
         OC    REPCDATE,REPCDATE                                                
         BNZ   *+10                                                             
         MVC   REPCDATE,DATECPR                                                 
         MVC   CLASSL,17(RE)                                                    
         MVI   CLASSL+11,0                                                      
         MVC   REPSTAT,28(RE)                                                   
         MVC   REPRECS,29(RE)                                                   
         ST    RE,AREPTAB                                                       
         SR    RE,RE                                                            
*                                                                               
TSTUSRX  LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
TSTCLASS ST    RE,DUB+4            TEST IF CLASS AT DUB(1) IS WANTED            
         CLI   CLASSL,0                                                         
         BE    TSTCLY              NO CLASS SPECIFIED                           
         LA    RE,CLASSL+1                                                      
*                                                                               
TSTCL2   CLI   0(RE),0             SEARCH CLASS LIST                            
         BE    TSTCL4                                                           
         CLC   DUB(1),0(RE)                                                     
         BE    TSTCL6                                                           
         LA    RE,1(RE)                                                         
         B     TSTCL2                                                           
*                                                                               
TSTCL4   CLI   CLASSL,C'+'         CLASS NOT IN LIST                            
         BE    TSTCLN                                                           
         B     TSTCLY                                                           
TSTCL6   CLI   CLASSL,C'+'         CLASS IN LIST                                
         BE    TSTCLY                                                           
         B     TSTCLN                                                           
*                                                                               
TSTCLN   LA    RE,1                CC=NEQ IF CLASS NOT WANTED                   
         B     *+6                                                              
TSTCLY   SR    RE,RE               CC=EQL IF CLASS IS WANTED                    
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
TSTSTAT  ST    RE,DUB+4            TEST IF STATUS AT DUB(1) IS WANTED           
         TM    DUB,X'01'                                                        
         BZ    *+12                                                             
         TM    DUB,W_STAC                                                       
         BZ    TSTSTN                                                           
         CLI   REPSTAT,0                                                        
         BE    TSTSTY                                                           
*                                                                               
TSTST2   MVC   DUB+1(1),REPSTAT                                                 
         TM    DUB+1,W_STKE                                                     
         BZ    *+12                                                             
         TM    DUB,W_STKE                                                       
         BZ    TSTSTN                                                           
         NI    DUB+1,255-W_STKE                                                 
         BZ    TSTSTY                                                           
         NC    DUB+1(1),DUB                                                     
         BZ    TSTSTN                                                           
*                                                                               
TSTSTY   SR    RE,RE               CC=EQL IF STATUS IS WANTED                   
         B     *+8                                                              
TSTSTN   LA    RE,1                CC=NEQ IF STATUS IS NOT WANTED               
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
TSTSYSP  EQU   *                                                                
         OC    REPSYSP,REPSYSP                                                  
         BZ    TSTOK                                                            
         LA    R1,REPSYSP          TEST FILEID                                  
         LA    RF,DUB                                                           
         LA    R0,6                                                             
TST01    CLI   0(R1),C'*'          CHECK FOR WILDCARDS                          
         BE    TST02                                                            
         CLC   0(1,R1),0(RF)       OR A MATCH                                   
         BNE   TST03                                                            
TST02    LA    R1,1(R1)            NEXT CHR                                     
         LA    RF,1(RF)                                                         
         BCT   R0,TST01            UNTIL ALL 6 DONE                             
*                                                                               
TSTOK    CR    RB,RB               CC EQU MEANS OK                              
         BR    RE                                                               
TST03    LTR   RB,RB               CC NEQ MEANS NOT OK                          
         BR    RE                                                               
         EJECT                                                                  
TSTRANGE ST    RE,DUB1             TEST IF DATES AT DUB(4) ARE WANTED           
*                                                                               
TSTRA1   CLI   CRANGE,0            TEST IF CREATE DATE FILTER                   
         BE    TSTRA2                                                           
         CLC   DUB+0(2),REPCSDAT   DUB+0(2) HAS CREATED DATE                    
         BL    *+14                                                             
         CLC   DUB+0(2),REPCEDAT                                                
         BNH   TSTRAY                                                           
         CLI   DRANGE,0            FAILED ON CREATE                             
         BE    TSTRAN                                                           
*                                                                               
TSTRA2   CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BE    TSTRAY                                                           
         CLC   DUB+2(2),REPDSDAT   DUB+2(2) HAS DEAD DATE                       
         BL    TSTRAN                                                           
         CLC   DUB+2(2),REPDEDAT                                                
         BH    TSTRAN                                                           
*                                                                               
TSTRAY   SR    RE,RE               CC=EQL IF IN DATE RANGE                      
         B     *+8                                                              
TSTRAN   LA    RE,1                CC=NEQ IF NOT IN DATE RANGE                  
         LTR   RE,RE                                                            
         L     RE,DUB1                                                          
         BR    RE                                                               
         EJECT                                                                  
REPTUPDT NTR1                      UPDATE FILE'S CI'S                           
         CLI   REORG,YES                                                        
         BNE   REPTUPX                                                          
         CLI   WRITE,YES                                                        
         BNE   REPTUPX                                                          
         L     R6,=A(QBUFF)                                                     
         USING SRTRECD,R6                                                       
         L     R5,ACIREC                                                        
         USING W_RECD,R5                                                        
*                                                                               
REPTUP2  OC    SRTUSER,SRTUSER                                                  
         BZ    REPTUPX             END OF CI LIST                               
         MVC   CIADDR(2),SRTADDR                                                
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BNE   REPTUPE                                                          
         CLI   CISTAT,0            PURGE CI                                     
         BNE   *+14                                                             
         XC    W_INDEX,W_INDEX                                                  
         B     REPTUP4                                                          
         MVC   W_STAT,CISTAT       SET NEW STATUS                               
         LA    R6,L'SRTREC(R6)                                                  
         MVC   W_CINEXT,SRTADDR                                                 
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
*                                                                               
REPTUP4  GOTO1 (RF),(R1),DMWRT                                                  
         CLI   8(R1),0                                                          
         BNE   REPTUPE                                                          
         LA    R6,L'SRTREC(R6)                                                  
         B     REPTUP2                                                          
*                                                                               
REPTUPE  MVI   P+119,C'E'          INDICATE UPDATE ERROR                        
         OI    CIOK,X'80'                                                       
*                                                                               
REPTUPX  XIT1                                                                   
         EJECT                                                                  
REPTNDX  NTR1                      UPDATE WRKFIL INDEX FROM DATA CI'S           
         MVC   CXADDR,=X'00010100'                                              
         LH    R1,CICINDX                                                       
         MH    R1,CITRKS                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,CIADDR                                                      
         MVC   CIADDR+2(2),=X'0100'                                             
         L     R5,ACIREC           R5=A(DATA REC)                               
         L     R6,ACXREC           R6=A(INDEX REC)                              
         LH    R7,CICINDX          R7=A(INDEX ENTRY)                            
         STH   R7,CXENTRY                                                       
         MH    R7,CINDXLN                                                       
         AR    R7,R6                                                            
         MVI   FLAG1,1             SET PART 1 INDEX                             
*                                                                               
REPTNDX2 GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CXADDR,(R6)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ INDEX REC                   
*                                                                               
REPTNDX4 CLC   0(L'W_INDEX,R7),=32X'FF'                                         
         BNE   *+6                                                              
         DC    H'0'                DIE IF INDEX CLOBBERED                       
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BNE   *+10                                                             
         MVC   0(L'W_INDEX,R7),0(R5)    COPY DATA TO INDEX IF NO ERRORS         
*                                                                               
REPTNDX6 AH    R7,CINDXLN          BUMP TO NEXT INDEX ENTRY                     
         LH    RF,CXENTRY                                                       
         LA    RF,1(RF)                                                         
         STH   RF,CXENTRY                                                       
         CH    RF,CIENTRYS                                                      
         BNL   REPTNDXA            END OF INDEX PAGE                            
         CLC   0(L'W_INDEX,R7),=32X'FF'                                         
         BNE   REPTNDX8                                                         
         OC    CJCITOT,CJCITOT                                                  
         BZ    REPTNDXA                                                         
         CLI   FLAG1,1             END OF PART 1 INDEX                          
         BNE   REPTNDXA            NO EXIT                                      
         MVI   FLAG1,2             YES SET PART 2 INDEX                         
         B     REPTNDX6            BYPASS END OF PART 1 INDEX ENTRY             
*                                                                               
REPTNDX8 LA    RF,REPTNDX4         BUMP TO NEXT DATA CI                         
         SR    RE,RE                                                            
         ICM   RE,3,CIADDR                                                      
         CLM   RE,3,CJSTTRK                                                     
         BL    *+12                                                             
         AH    RE,CJTRKS                                                        
         B     *+8                                                              
         AH    RE,CITRKS                                                        
         STCM  RE,3,CIADDR                                                      
         BR    RF                                                               
*                                                                               
REPTNDXA GOTO1 VDATAMGR,DMCB,DMWRT,WRKFID,CXADDR,(R6)                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT WRITE INDEX REC                  
         CLC   CXENTRY,CIENTRYS                                                 
         BL    REPTNDXX                                                         
         LR    R7,R6               RESET TO START OF NEW INDEX REC              
         XC    CXENTRY,CXENTRY                                                  
         SR    RE,RE               BUMP TO NEXT INDEX REC                       
         IC    RE,CXADDR+2                                                      
         LA    RE,1(RE)                                                         
         STC   RE,CXADDR+2                                                      
         CH    RE,CIHIREC                                                       
         BNH   REPTNDXC                                                         
         LH    RE,CXADDR           BUMP TO NEXT INDEX TRACK                     
         LA    RE,1(RE)                                                         
         STH   RE,CXADDR                                                        
         MVI   CXADDR+2,1                                                       
*                                                                               
REPTNDXC LA    RF,REPTNDX2         BUMP TO NEXT DATA CI                         
         B     REPTNDX8+4                                                       
*                                                                               
REPTNDXX XIT1                                                                   
         EJECT                                                                  
STATOUT  NTR1                      EXPAND FILE STATUS TO REPSTATA               
         MVC   REPSTATA,SPACES                                                  
         LA    RE,REPSTATA                                                      
         CLI   CISTAT,0            TEST PURGED STATUS                           
         BNE   *+14                                                             
         MVC   0(2,RE),=C'..'                                                   
         B     STATOUTX                                                         
         MVI   0(RE),C'?'                                                       
*                                                                               
STATOUT1 TM    CISTAT,W_STAC       MAIN STATUS = ACTV/HOLD/PRTD/SENT            
         BZ    *+12                                                             
         MVI   0(RE),C'A'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,W_STHO                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'H'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,W_STSE                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'S'                                                       
         B     STATOUT2                                                         
*                                                                               
STATOUT2 TM    CISTAT,W_STKE       SUB STATUS-1 = KEEP                          
         BZ    STATOUTX                                                         
         MVI   1(RE),C'K'                                                       
*                                                                               
STATOUTX XIT1                                                                   
         EJECT                                                                  
TIMEOUT  MVI   DUB+2,0             EXPAND BINARY TIME IN DUB(2)                 
         SR    R0,R0                                                            
*                                                                               
         IC    R0,DUB              GET BINARY HOUR BYTE                         
         CH    R0,=H'23'                                                        
         BNH   *+10                                                             
         MVI   DUB+2,1             SET INVALID TIME FLAG                        
         SR    R0,R0                                                            
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+3(2),DUB1+6(2)                                               
         MVI   DUB+5,C'.'                                                       
*                                                                               
         IC    R0,DUB+1            GET BINARY MINUTE BYTE                       
         CH    R0,=H'59'                                                        
         BNH   *+10                                                             
         MVI   DUB+2,1             SET INVALID TIME FLAG                        
         SR    R0,R0                                                            
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+6(2),DUB1+6(2)                                               
*                                                                               
         CLI   DUB+2,0             EXIT WITH CC=EQL IF VALID TIME               
         BR    RE                                                               
         EJECT                                                                  
IDOUT    NTR1                      EXPAND FILE ID TO REPIDA                     
         LA    R4,REPIDA                                                        
         MVC   REPIDA,SPACES                                                    
*                                                                               
IDOUT2   OC    REPUSER,REPUSER     FILE USER ID                                 
         BZ    IDOUT6                                                           
         MVC   USERN,REPUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   0(10,R4),USERA                                                   
         LA    R4,11(R4)                                                        
*                                                                               
IDOUT6   OC    REPSYSP,REPSYSP     FILE ID                                      
         BZ    IDOUT9                                                           
         MVC   0(4,R4),REPSYSP                                                  
         LA    R4,4(R4)                                                         
         CLI   REPDAY,X'C1'                                                     
         BNL   IDOUT7                                                           
*                                                                               
         IC    R1,REPDAY           PWO DAY                                      
         MVC   1(1,R4),REPDAY                                                   
         OC    1(1,R4),=X'F0'                                                   
         SRL   R1,4                                                             
         STC   R1,0(R4)                                                         
         OC    0(1,R4),=X'F0'                                                   
         LA    R4,2(R4)                                                         
         B     IDOUT8                                                           
*                                                                               
IDOUT7   MVC   0(1,R4),REPDAY      CHR DAY                                      
         LA    R4,1(R4)                                                         
*                                                                               
IDOUT8   MVC   0(1,R4),REPCLAS     CLASS                                        
         LA    R4,1(R4)                                                         
*                                                                               
IDOUT9   OC    REPFILN,REPFILN     FILE SEQ NUM                                 
         BZ    IDOUTA                                                           
         EDIT  (B2,REPFILN),(5,(R4))                                            
         LA    R4,6(R4)                                                         
*                                                                               
IDOUTA   OC    REPIDA,SPACES                                                    
         LA    R0,L'REPIDA                                                      
         GOTO1 =V(SQUASHER),DMCB,REPIDA,(C',',(R0))                             
*                                                                               
IDOUTX   XIT1                                                                   
         EJECT                                                                  
GETUSER  NTR1                      GET USER ALPHA FROM USER NUMBER              
         MVI   DMCB1+8,0                                                        
         MVC   USERA,SPACES                                                     
         L     R5,=A(CTBUF)                                                     
*                                                                               
GUSER1   OC    0(2,R5),0(R5)       SEARCH USER ID TABLE                         
         BZ    GUSER2                                                           
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    GUSER2              END OF TABLE                                 
         CLC   0(2,R5),USERN                                                    
         BE    *+12                                                             
         LA    R5,12(R5)                                                        
         B     GUSER1                                                           
         MVC   USERA,2(R5)         MOVE OUT ALPHA FROM TABLE                    
         B     GUSERX                                                           
*                                                                               
GUSER2   LR    R0,R5               SAVE A(NEXT AVAIL TABLE ENTRY)               
         L     R5,=A(CTREC)                                                     
         XC    0(25,R5),0(R5)      READ CONTROL FILE FOR NUMBER                 
         MVI   0(R5),C'I'                                                       
         MVC   23(2,R5),USERN                                                   
         GOTO1 VDATAMGR,DMCB1,DMREAD,=C'CTFILE',(R5),(R5)                       
         CLI   8(R1),0                                                          
         BNE   GUSER4                                                           
         LA    R5,28(R5)           SEARCH CONTROL RECORD FOR ALPHA              
         SR    R6,R6                                                            
GUSER3   CLI   0(R5),0                                                          
         BNE   *+12                                                             
         MVI   DMCB1+8,1                                                        
         B     GUSER4                                                           
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     GUSER3                                                           
         MVC   USERA,2(R5)         EXTRACT ALPHA                                
         B     GUSER5                                                           
*                                                                               
GUSER4   LH    R6,USERN            ERROR IN FINDING USER ID NUM                 
         CVD   R6,DUB                                                           
         UNPK  USERA(6),DUB                                                     
         OI    USERA+5,X'F0'                                                    
*                                                                               
GUSER5   LR    R5,R0               SAVE NUM/ALPHA IN TABLE                      
         OC    0(2,R5),0(R5)                                                    
         BNZ   GUSERX              NO MORE ROOM IN TABLE                        
         MVC   0(2,R5),USERN                                                    
         MVC   2(10,R5),USERA                                                   
*                                                                               
GUSERX   CLI   DMCB1+8,0           SET CC=EQL IF USER FOUND OK                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TCOUNT   DC    F'0'                                                             
* GET NEXT FILE FROM DISK OR TAPE                                               
*                                                                               
GETREPT  NTR1                                                                   
         L     R2,AQH              R2=TAPE HEADER                               
         L     R3,AQ                                                            
         USING WLHDRD,R3           R3=A(FILE HEADER PRINT LINE)                 
         TM    INPUT,TAPE                                                       
         BZ    GETR4                                                            
*                                                                               
GETR2    BAS   RE,RITAPE           READ TAPE UNTIL FILE HEADER                  
         OC    0(4,R2),0(R2)                                                    
         BZ    GETRX               EXIT IF EOF                                  
*                                                                               
         CLC   0(2,R2),LENSOF1     LOOK FOR NEW HEADER LEN                      
         BNE   GETR2A                                                           
         CLC   WLSOFLAB,SOFLAB     LOOK FOR NEW HEADER                          
         BE    GETR3                                                            
*                                                                               
GETR2A   CLC   0(2,R2),LENSOF      LOOK FOR OLD HEADER LEN                      
         BNE   GETR2                                                            
         CLC   8(8,R2),SOFLAB      LOOK FOR OLD HEADER                          
         BNE   GETR2                                                            
*                                                                               
GETR3    CLI   WRKFINP,0           TEST IF ANY WRKF FILES NAMED                 
         BE    GETRX               NO                                           
         XC    INDEX,INDEX         YES FIND TARGET WRKF FILE FOR FILE           
         MVC   NXSRCID(2),WLUSRID                                               
         GOTO1 VDATAMGR,DMCB,=C'GFILE',WRKFIL,INDEX,,(R5)                       
         SR    R1,R1                                                            
         IC    R1,NXINFO           GET INTERNAL WRKF FILE NUMBER                
         LA    RF,WRKFINP(R1)                                                   
         TM    0(RF),X'0F'         TEST IF THIS WRKF FILE NAMED                 
         BZ    GETR2               NO THEN IGNORE THIS FILE                     
         B     GETRX                                                            
*                                                                               
GETR4    L     R0,ACXREC           GET NEXT INDEX ENTRY USING CXREC             
         MVI   NXFLAG,0                                                         
         GOTO1 VDATAMGR,DMCB,SEQ,WRKFID,INDEX,(R3),(R0)                         
         CLI   8(R1),0                                                          
         BE    GETRX                                                            
         TM    8(R1),X'80'         TEST AND SET END OF FILE                     
         BZ    *+14                                                             
         XC    0(4,R2),0(R2)                                                    
         B     GETRX                                                            
         DC    H'0'                DISK ERROR ON INDEX                          
*                                                                               
GETRX    OC    0(4,R2),0(R2)       EXIT WITH CC=EQL IF EOF                      
         XIT1                                                                   
         EJECT                                                                  
* READ INPUT TAPE                                                               
*                                                                               
RITAPE   ST    RE,RWSAVRE          READ INPUT TAPE INTO QH/Q                    
         L     RE,AQH                                                           
         XC    0(4,RE),0(RE)       CLEAR HEADER TO ZEROS                        
         L     RE,AQ                                                            
         LA    R0,8                                                             
         MVC   0(128,RE),SPACES    CLEAR DATA TO SPACES                         
         LA    RE,128(RE)                                                       
         BCT   R0,*-10                                                          
         L     R0,AQH                                                           
         L     R1,=A(TAPEIN)                                                    
         GET   (1),(0)                                                          
         CLI   INPUT,WKTAPE        TEST FOR OLD WK FORMAT                       
         BNE   RITAPEXX                                                         
         BAS   RE,TAPECONV         CONVERT IT                                   
         B     RITAPEXX                                                         
*                                                                               
RITAPEX  TM    MSG,YES             TEST IF MESSAGES SUPPRESSED                  
         BZ    RITAPEX2                                                         
RITAPEX1 L     RF,=A(QUEST1)       ANY MORE INPUT TAPES                         
         MVC   WORK(60),0(RF)                                                   
         BAS   RE,PAGMSG                                                        
         CLI   OPERANS,C'N'                                                     
         BE    RITAPEX2                                                         
         CLI   OPERANS,C'Y'                                                     
         BNE   RITAPEX1                                                         
         BAS   RE,CLSIN            END OF INPUT VOLUME                          
         BAS   RE,OPNIN                                                         
         B     RITAPE+4                                                         
*                                                                               
RITAPEX2 BAS   RE,CLSIN            END OF INPUT FILE                            
         L     RE,AQH                                                           
         XC    0(4,RE),0(RE)       SET EOF IN RECORD HDR                        
         B     *+8                                                              
*                                                                               
RITAPEX4 ST    RE,RWSAVRE          CLOSE OUTPUT TAPE IF SPECIFIED               
         TM    OUTPUT,TAPE                                                      
         BZ    RITAPEXX                                                         
         BAS   RE,CLSOUT                                                        
*                                                                               
RITAPEXX L     RE,RWSAVRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* WRITE OUTPUT TAPE                                                             
*                                                                               
WOTAPE   ST    RE,RWSAVRE                                                       
*                                                                               
         L     R0,AQH              PUT PRINT LINE FROM Q                        
         L     R1,=A(TAPEOUT)                                                   
         LR    RE,R0                                                            
         CLC   0(2,RE),82(R1)      COMPARE REC LEN TO LRECL                     
         BH    WOTAPEX                                                          
         PUT   (1),(0)                                                          
*                                                                               
         TM    TCOPY,TAPE          TEST IF EXTRA OUTPUT TAPE COPY               
         BZ    WOTAPEX                                                          
         L     R0,AQH              PUT PRINT LINE FROM Q AGAIN                  
         L     R1,=A(TAPECPY)                                                   
         PUT   (1),(0)                                                          
*                                                                               
WOTAPEX  L     RE,RWSAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
OPNIN    STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(TAPEIN)                                                    
         OPEN  ((2),INPUT)                                                      
         LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
*                                                                               
CLSIN    STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(TAPEIN)                                                    
         CLOSE ((2))                                                            
         LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
*                                                                               
OPNOUT   STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(TAPEOUT)                                                   
         OPEN  ((2),OUTPUT)                                                     
         TM    TCOPY,TAPE                                                       
         BZ    OPNOUTX                                                          
         L     R2,=A(TAPECPY)                                                   
         OPEN  ((2),OUTPUT)                                                     
OPNOUTX  LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
*                                                                               
CLSOUT   STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(TAPEOUT)                                                   
         CLOSE ((2))                                                            
         TM    TCOPY,TAPE                                                       
         BZ    CLSOUTX                                                          
         L     R2,=A(TAPECPY)                                                   
         CLOSE ((2))                                                            
CLSOUTX  LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
         EJECT                                                                  
* CONVERT OLD WORKER FORMAT TO NEW                                              
*                                                                               
TAPECONV NTR1                                                                   
         L     R5,AQH              R5=A(4 BYTE HEADER)                          
         SR    RF,RF                                                            
         ICM   RF,3,0(R5)          GET RECLEN INTO RF                           
         LR    R1,RF                                                            
         L     RE,=A(CTREC)        MOVE RECORD TO CTREC                         
         LR    R0,R5                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLC   8(8,R5),SOFLAB      TEST FOR HEADER                              
         BE    TCONV011                                                         
         CLC   8(8,R5),EOFLAB      TEST FOR FOOTER                              
         BNE   TCONV010                                                         
         MVC   0(4,R5),=X'000C0000'                                             
         MVC   4(8,R5),EOFLAB                                                   
         B     TCONVEX                                                          
*                                                                               
TCONV010 SR    RF,RF               SET NEW LEN                                  
         ICM   RF,3,0(R5)                                                       
         LA    RF,2(RF)                                                         
         STCM  RF,3,0(R5)                                                       
         SH    RF,=H'4'                                                         
         SLL   RF,16                                                            
         STCM  RF,15,4(R5)                                                      
         SRL   RF,16                                                            
         LR    R1,RF                                                            
         L     RE,=A(CTREC)        MOVE RECORD BACK                             
         LA    RE,6(RE)                                                         
         LA    R0,8(R5)                                                         
         MVCL  R0,RE                                                            
         B     TCONVEX                                                          
*                                                                               
TCONV011 XC    0(28,R5),0(R5)      SET FOR NEW HEADER                           
         MVC   0(4,R5),=X'008C0000'                                             
         LA    R3,4(R5)                                                         
         USING WLHDRD,R3                                                        
         MVC   WLSOFLAB,SOFLAB                                                  
*                                                                               
         L     R2,=A(CTREC)                                                     
         LA    R2,32(R2)                                                        
         USING WKINDEX,R2                                                       
*                                                                               
         MVC   WLKEY,WKKEY                                                      
         MVC   WLFILENO,WKFILNO                                                 
         MVI   WLTYPE,0                                                         
         MVI   WLATTB,0                                                         
         MVC   WLSTAT,WKSTAT                                                    
         TM    WLSTAT,X'20'        DELETED                                      
         BNO   *+12                                                             
         NI    WLSTAT,X'FF'-X'20'                                               
         OI    WLSTAT,X'04'        SET X'04' FOR NWK                            
*                                                                               
         MVC   WLAGES,WKAGES                                                    
*                                                                               
         GOTO1 =V(DATCON),DMCB,(1,WKAGED),(2,WLAGELD)                           
         MVC   WLAGERD,=X'C79F'                                                 
*                                                                               
         MVC   WLRECS,WKRECS                                                    
         MVC   WLAVGRL,WKRECL                                                   
         MVC   WLMAXRL,=AL2(1024)                                               
*        MVC   WLTTBLL,WKTTBL                                                   
         MVC   WLNCI,WKAGES                                                     
         GOTO1 =V(DATCON),DMCB,(1,WKAGED),(2,WLDATEL)                           
         MVC   WLTIMEL,WKTIMEC                                                  
         MVC   WLRETND,=X'0018'                                                 
         MVC   WLRETNL,=X'0012'                                                 
         MVC   WLDESC,WKCOMNT                                                   
         MVC   WLDESC+4(6),=C'Cnvrtd'                                           
         L     R3,AQH                                                           
*                                                                               
TCONVEX  XIT1                                                                   
         EJECT                                                                  
* PRINT LINE DATA IN Q - R3 POINTS TO DATA                                      
*                                                                               
PLINEQ   ST    RE,CISAVRE                                                       
         LA    RE,CTLCHR           POINT TO TABLE OF VALID CC CHRS              
         LR    RF,R3               POINT TO CC CHR IN RECORD                    
PLINEQ1  CLC   0(1,RF),0(RE)       CONVERT TO V(PRINT) DEFINITION               
         BE    PLINEQ2                                                          
         LA    RE,5(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   PLINEQ1                                                          
PLINEQ2  MVC   PLINECC,1(RE)       SET C'BLXX' PARM                             
         ST    RF,PLINEPL          SET A(DATA-1) PARM                           
         GOTO1 =V(PRINT),PLINEPL                                                
PLINEQX  L     RE,CISAVRE                                                       
         BR    RE                                                               
*                                                                               
PLINEPL  DC    A(0),A(PLINECC)     PARM LIST FOR V(PRINT)                       
PLINECC  DC    F'0'                                                             
*                                                                               
CTLCHR   DS    0CL5                                                             
         DC    X'09',C'BL01'                                                    
         DC    X'11',C'BL02'                                                    
         DC    X'19',C'BL03'                                                    
         DC    X'89',C'BC01'                                                    
*                                                                               
         DC    X'01',C'BL00'                                                    
         DC    X'0B',C'BL01'                                                    
         DC    X'13',C'BL02'                                                    
         DC    X'1B',C'BL03'                                                    
         DC    X'8B',C'BC01'                                                    
*                                                                               
CTLCHRX  DC    X'00',C'BL01'                                                    
         DS    0H                                                               
LEFTT    EQU   X'EB'               BOX CHR LEFT HAND T JUNCTION                 
CROSS    EQU   X'8F'               BOX CHR INTERSECTION                         
RIGHTT   EQU   X'EC'               BOX CHR RIGHT T JUNCTIONTION                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* PARAMETER TABLE - LIST OF KEYWORDS AND VALUES                                 
*                                                                               
* XL1    PARM VALUE                                                             
* XL1    PARM DEFAULT VALUE                                                     
* XL1    PARM FLAGS X'80'=REQUIRED,X'40'=LIST,X'20'=ROUT,X'01'=SINGLE           
* XL1    PARM MIN LEN                                                           
* CL8    PARM KEYWORD NAME                                                      
* AL4    PARM VALUE LIST                                                        
*                                                                               
         DS    0F                                                               
PARMTBL  DS    0CL16                                                            
MODE     DC    X'0000C004',C'MODE    ',A(MODEL)                                 
INPUT    DC    X'00004001',C'INPUT   ',A(INPUTL)                                
OUTPUT   DC    X'00004001',C'OUTPUT  ',A(OUTPUTL)                               
TCOPY    DC    X'00004001',C'COPY    ',A(TCOPYL)                                
MSG      DC    X'00024001',C'MSG     ',A(MSGL)                                  
WARN     DC    X'00024001',C'WARNINGS',A(WARNL)                                 
LOAD     DC    X'00000004',C'LOAD    ',A(LOADNAME)                              
WKID     DC    X'00002001',C'WKID    ',A(VWKID)                                 
USER     DC    X'00002003',C'USER    ',A(VUSER)                                 
REPORT   DC    X'00002002',C'FILE    ',A(VREPT)                                 
CLASS    DC    X'00002001',C'CLASS   ',A(VCLASS)                                
STATUS   DC    X'00002001',C'STATUS  ',A(VSTAT)                                 
KDAYS    DC    X'00002001',C'KDAYS   ',A(VKDAY)                                 
LDAYS    DC    X'00002001',C'LDAYS   ',A(VLDAY)                                 
DDAYS    DC    X'00002001',C'DDAYS   ',A(VDDAY)                                 
CDATE    DC    X'00002001',C'CDATE   ',A(VCDATE)                                
CRANGE   DC    X'00002001',C'CRANGE  ',A(VCRANGE)                               
DRANGE   DC    X'00002001',C'DRANGE  ',A(VDRANGE)                               
REORG    DC    X'00014001',C'REORG   ',A(REORGL)                                
WRITE    DC    X'02022001',C'WRITE   ',A(VWRITE)                                
         DC    X'00022001',C'WRSRV   ',A(VWRITE)                                
COMPACT  DC    X'00014001',C'COMPACT ',A(COMPACTL)                              
SORT     DC    X'00034001',C'SORT    ',A(SORTL)                                 
LOCK     DC    X'00024001',C'LOCK    ',A(LOCKL)                                 
ENTRYS   DC    X'00002001',C'ENTRYS  ',A(VNCIS)                                 
CISIZE   DC    X'00002001',C'CISIZE  ',A(VTRKS)                                 
BLKSIZE  DC    X'00002004',C'BLKSIZE ',A(VBLKS)                                 
OENTRYS  DC    X'00002001',C'OENTRYS ',A(VNCIS)                                 
OCISIZE  DC    X'00002001',C'OCISIZE ',A(VTRKS)                                 
DSPNDX   DC    X'00014001',C'DSPNDX  ',A(DSPNDXL)                               
FILE     DC    X'00002005',C'FILEID  ',A(VFILE)                                 
         DC    X'00002005',C'DAFILE  ',A(VFILE)                                 
EXTID    DC    X'00004001',C'EXTID   ',A(EXTIDL)                                
PARMTBLX DC    X'FFFF'                                                          
*                                                                               
LOADNAME DC    CL8' '                                                           
CLASSL   DC    XL12'00'            MAX OF 10 CLASSES                            
*                                                                               
FILEID   DC    CL144' ',X'FF'      MAX 0F 08 WRKF FILES                         
FILEIX   DC    CL144' ',X'FF'                                                   
WRKFINP  DC    XL17'00',X'FF'                                                   
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
PACKED   DS    PL6                                                              
*                                                                               
ERRNUM   DS    F                                                                
ACOMRG   DS    A                                                                
ALOADPT  DS    A                                                                
AQBNEXT  DS    A                                                                
ASUMNEXT DS    A                                                                
PGSAVRE  DS    A                                                                
RWSAVRE  DS    A                                                                
OCSAVREG DS    5F                                                               
FIRST    DS    F                                                                
LAST     DS    F                                                                
*                                                                               
COPYINDX DS    A                                                                
COPYCTRS DS    0XL72                                                            
TOTLREAD DS    F                                                                
TOTLCOPY DS    F                                                                
         DS    32F                 READ/COPY FOR 16 WRKF FILES                  
*                                                                               
MAXSEQ   DC    F'65000'            MAXIMUM SEQUENCE NUM FOR USER ID             
FRSTTIME DC    C'Y'                                                             
UPSIVAL  DC    X'00'                                                            
UPSIINP  DC    X'00'                                                            
FLAG     DC    X'00'                                                            
FLAG1    DC    X'00'                                                            
         DC    XL3'00'                                                          
*                                                                               
TODAY4   DS    0CL8                                                             
DATEIPL  DC    CL8' '              FORMAT-4 C'DD/MM/YY'                         
TODAY0   DS    0CL6                                                             
DATEYMD  DC    CL8' '              FORMAT-0 C'YYMMDD'                           
TODAY2   DS    0XL2                                                             
DATECPR  DC    XL2'00'             FORMAT-2 B'YYYYYYYMMMMDDDDD'                 
TIMEINC  DC    XL1'00'                                                          
         DC    XL1'00'                                                          
*                                                                               
P0       DS    F                                                                
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
PARAM0   DS    F                                                                
PARAM1   DS    F                                                                
PARAM2   DS    F                                                                
PARAM3   DS    F                                                                
PARAM4   DS    F                                                                
PARAM5   DS    F                                                                
PARAM6   DS    F                                                                
*                                                                               
PARM     DS    6F                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
USERN    DS    H                                                                
USERA    DS    CL10                                                             
*                                                                               
         DS    0F                                                               
INDEX    DS    0XL66               INDEX ENTRY                                  
NXSRCID  DS    XL2                                                              
NXSYSPG  DS    CL3                                                              
NXSUBPG  DS    CL1                                                              
NXDAY    DS    CL1                                                              
NXCLASS  DS    CL1                                                              
NXFILENO DS    XL2                                                              
NXTYPE   DS    XL1                                                              
NXATTB   DS    XL1                                                              
NXSTAT   DS    XL1                                                              
NXSEQ    DS    XL1                                                              
NXAGES   DS    XL1                                                              
NXAGELD  DS    XL2                                                              
NXUDATA  DS    XL2                                                              
NXAGERD  DS    XL2                                                              
NXAGERT  DS    XL1                                                              
NXAGELT  DS    XL2                                                              
*                                                                               
         DS    XL2                                                              
NXINFO   DS    XL2                                                              
NXFILNOX DS    XL2                                                              
NXCIADDR DS    XL2                                                              
NXFLAG   DS    X                                                                
         DS    X                                                                
NXUSRINF DS    CL8                                                              
         DS    CL24                                                             
*                                                                               
SAVE     DS    CL256                                                            
WORK     DS    CL256                                                            
OPERANS  DS    CL8                                                              
C        DS    CL80                                                             
*                                                                               
RDID     EQU   01                                                               
WTCKD    EQU   05                                                               
WTERASE  EQU   08                                                               
DACLOSE  EQU   15                                                               
DARPT    EQU   16                                                               
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
DADDS    DC    C'DADDS   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMPRINT  DC    C'DMPRINT '                                                      
DMWRT    DC    C'DMWRT   '                                                      
WRKFIL   DC    C'WRKFIL  '                                                      
SEQ      DC    C'SEQ     '                                                      
ADD      DC    C'ADD     '                                                      
*                                                                               
AWRKFXPE DC    A(0)                                                             
AWRKFLST DC    A(0)                                                             
WRKFMAX  DC    AL1(0)                                                           
         DC    AL1(0)                                                           
WRKFINT  DC    AL1(0)                                                           
WRKFEXT  DC    AL1(0)                                                           
WRKFID   DC    CL8' '                                                           
WRKFDTF  DC    A(0)                                                             
*                                                                               
FFS      DC    8X'FF'                                                           
USCORES  DC    16X'BF'                                                          
*                                                                               
AQH      DC    A(QH)                                                            
AQ       DC    A(Q)                                                             
*                                                                               
ACIREC   DS    A                                                                
ACXREC   DS    A                                                                
AREPTAB  DS    A                                                                
AREPALL  DS    A                                                                
AWRKFINP DS    A                                                                
*                                                                               
CINDXMIN DC    H'2'                                                             
CPAGE    DS    H                                                                
         SPACE 2                                                                
*DMWRKFW                                                                        
       ++INCLUDE DMWRKFW                                                        
*                                                                               
CISEQ    DS    XL1                                                              
CISTAT   DS    XL1                                                              
CIREPNO  DS    XL2                                                              
CINCI    DS    PL4                                                              
CIOK     DS    X                                                                
         DS    X                                                                
USERSEQ  DS    H                                                                
CITOTAL  DS    PL6                 TOTAL NUM OF PART1 CIS                       
CJTOTAL  DS    PL6                 TOTAL NUM OF PART2 CIS                       
*                                                                               
CFILE    DS    0CL84               COUNTERS FOR WHOLE FILE                      
CILIVE   DS    PL6                                                              
CIDEAD   DS    PL6                                                              
CIERROR  DS    PL6                                                              
CIXPRD   DS    PL6                                                              
CIRETN   DS    PL6                                                              
CISEMI   DS    PL6                                                              
CJLIVE   DS    PL6                                                              
CJDEAD   DS    PL6                                                              
CJERROR  DS    PL6                                                              
CJXPRD   DS    PL6                                                              
CJRETN   DS    PL6                                                              
CJSEMI   DS    PL6                                                              
CRRECS   DS    PL6                                                              
*                                                                               
CUSER    DS    0CL84               COUNTERS FOR INDIVIDUAL USER ID              
UILIVE   DS    PL6                                                              
UIDEAD   DS    PL6                                                              
UIERROR  DS    PL6                                                              
UIXPRD   DS    PL6                                                              
UIRETN   DS    PL6                                                              
UISEMI   DS    PL6                                                              
UJLIVE   DS    PL6                                                              
UJDEAD   DS    PL6                                                              
UJERROR  DS    PL6                                                              
UJXPRD   DS    PL6                                                              
UJRETN   DS    PL6                                                              
UJSEMI   DS    PL6                                                              
URRECS   DS    PL6                                                              
*                                                                               
SRTREC   DS    CL40                                                             
SRTRECL  DS    CL40                                                             
*                                                                               
REPUSERA DS    CL10                                                             
*                                                                               
REPDEFN  DS    0XL14                                                            
REPUSER  DS    XL2                                                              
REPSYSP  DS    CL3                                                              
REPSUBP  DS    CL1                                                              
REPDAY   DS    XL1                                                              
REPCLAS  DS    XL1                                                              
REPFILN  DS    XL2                                                              
REPTIMS  DS    XL4                                                              
REPFLAG  DS    XL1                                                              
*                                                                               
REPCDATE DS    XL2                                                              
REPRECS  DS    XL4                                                              
REPSTAT  DS    XL1                                                              
REPSTATA DS    CL3                                                              
REPIDA   DS    CL24                                                             
REPIDASV DS    CL10                                                             
*                                                                               
REPKDATE DS    XL2                                                              
REPLDATE DS    XL2                                                              
REPDDATE DS    XL2                                                              
REPCSDAT DS    XL2                                                              
REPCEDAT DS    XL2                                                              
REPDSDAT DS    XL2                                                              
REPDEDAT DS    XL2                                                              
         DS    XL2                                                              
REPKDAYS DS    F                                                                
REPLDAYS DS    F                                                                
REPDDAYS DS    F                                                                
REPCSDAY DS    F                                                                
REPCEDAY DS    F                                                                
REPDSDAY DS    F                                                                
REPDEDAY DS    F                                                                
         EJECT                                                                  
*&&UK                                                                           
DEFKDAYS DC    F'-14'              DEFAULT KEEP DAYS                            
DEFLDAYS DC    F'-3'               DEFAULT LIVE DAYS                            
DEFDDAYS DC    F'-1'               DEFAULT DEAD DAYS                            
*&&                                                                             
*&&US                                                                           
DEFKDAYS DC    F'-2'               DEFAULT KEEP DAYS                            
DEFLDAYS DC    F'-4'               DEFAULT LIVE DAYS                            
DEFDDAYS DC    F'-2'               DEFAULT DEAD DAYS                            
*&&                                                                             
LENSOF   DC    H'0156'             TOT LEN OF TAPE STR-OF-FILE REC OLD          
LENSOF1  DC    H'0140'             TOT LEN OF TAPE STR-OF-FILE REC NEW          
LENEOF   DC    H'0012'             TOT LEN OF TAPE END-OF-FILE REC              
*                                                                               
SOFLAB   DS    0CL8                                                             
         DC    C'*SOFSOF*'                                                      
EOFLAB   DS    0CL8                                                             
         DC    C'*EOFEOF*'                                                      
*                                                                               
INHOUSE  DC    CL24'DDS,INHOUSE'                                                
DOTS     DC    16C'.'                                                           
ZEROS    DC    16C'0'                                                           
*                                                                               
PLXTRN   DC    A(CIREC),A(0),A(0),A(0)                                          
         DC    V(PRINTER),V(CPRINT)                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    8X'FF',C'*REPTAB*',8X'FF',C'*REPTAB*'                            
REPTAB   DS    50XL32                                                           
         DC    8X'FF'                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'QHQHQHQH'                                                      
QH       DS    4C                                                               
Q        DS    1024C                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'CICICICI'                                                      
CIREC    DS    14336C                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'CXCXCXCX'                                                      
CXREC    DS    14336C                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'QBQBQBQB'                                                      
QBUFF    DS    21000C                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'WKWKWKWK'                                                      
WKWORK   DS    4000D                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'UIDSUIDS'                                                      
CTBUF    DC    1000XL12'00'                                                     
         DC    2X'FF'                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'SUMBSUMB'                                                      
SUMBUF   DC    1001XL100'00'                                                    
         DC    6X'FF'                                                           
         SPACE 2                                                                
CTREC    DS    4096C                                                            
PARMCARD DS    CL80                                                             
         EJECT                                                                  
REPOUTA  DC    CL16'FILE ID'                                                    
         DC    CL16'FILE CLASS'                                                 
         DC    CL16'FILE STATUS'                                                
         DC    CL16'FILE NAME'                                                  
         DC    CL16'FILE SYS/PRG'                                               
         DC    CL16'LOCN CREATED'                                               
         DC    CL16'DATE CREATED'                                               
         DC    CL16'TIME CREATED'                                               
         DC    CL16'LIVE RETAIN HRS'                                            
         DC    CL16'DEAD RETAIN HRS'                                            
         DC    CL16'DATE RETAINED'                                              
         DC    CL16'TIME RETAINED'                                              
         DC    CL16'RECS PER PAGE'                                              
         DC    CL16'NUM OF RECS'                                                
         DC    CL16'CHRS PER LINE'                                              
         DC    CL16'NUM OF CIS'                                                 
         DC    CL16'LOCN PRINTED'                                               
         DC    CL16'COUNT PRINTED'                                              
         DC    CL16'DEVICE PRINTED'                                             
         DC    CL16'DATE PRINTED'                                               
         DC    CL16'TIME PRINTED'                                               
         SPACE 2                                                                
INFO0    DC    CL60'---------------'                                            
INFO1    DC    CL60'WORKER FILE MAINTENANCE'                                    
INFO2    DC    CL60'PARAMETER CARDS'                                            
INFO3    DC    CL60'ACTION MESSAGES'                                            
INFO4    DC    CL60'INITIALISED XXXXX'                                          
INFO5    DC    CL60'ERROR FILE(S) NOT FOUND'                                    
INFO6    DC    CL60'TOTAL NNNNN FILES COPIED FROM TAPE TO DISK '                
INFO7    DC    CL60'TOTAL NNNNN FILES COPIED FROM DISK TO TAPE '                
INFO8    DC    CL60'OUT OF NNNNN FILES READ'                                    
INFO9    DC    CL60'FILE DESCRIPTION AND DATA FOLLOWS'                          
INFOA    DC    CL60'XXXXX FILE FOLLOWS'                                         
INFOB    DC    CL60'XXXXX FILE SUMMARY FOLLOWS'                                 
         SPACE 2                                                                
QUEST1   DC    CL60'ANY MORE INPUT TAPES ?'                                     
         SPACE 2                                                                
ERRMSG1  DC    CL60'ERROR MISSING PARAMETER - '                                 
ERRMSG2  DC    CL60'ERROR INVALID PARAMETER CARD SYNTAX'                        
ERRMSG3  DC    CL60'ERROR INVALID PARAMETER - '                                 
ERRMSG4  DC    CL60'ERROR INVALID VALUE FOR PARAMETER - '                       
ERRMSG5  DC    CL60'ERROR MUST SPECIFY A SINGLE WRKF ONLY'                      
*                                                                               
ERRMSGA  DC    CL60'ERROR WRKFL DISK END OF FILE'                               
ERRMSGB  DC    CL60'ERROR WRKFL DISK WRITE ERROR'                               
ERRMSGC  DC    CL60'ERROR WRKFL DISK READ ERROR'                                
ERRMSGD  DC    CL60'ERROR WRKFL INVALID CI DATA'                                
ERRMSGE  DC    CL60'ERROR WRKFL ERROR IN COPY INDEX='                           
         EJECT                                                                  
HLINE1   DC    CL50'                        S                  Create '         
         DC    CL50'    Expiry    Retain                              '         
         DC    CL50'                                                  '         
         DC    CL15'               '                                            
*                                                                               
HLINE2   DC    CL50'  User     File id      T  Records  Nci           '         
         DC    CL50'                                  Description     '         
         DC    CL50'                       Passwd                 Make'         
         DC    CL15'r              '                                            
*                                                                               
HLINE3   DC    CL50'                        A                Date  Tim'         
         DC    CL50'e Date  Time Live Dead  Avg   USE                 '         
         DC    CL50' Date  Time  Device  C                            '         
         DC    CL15'                '                                           
         EJECT                                                                  
SLINE1   DC    CL165' '                                                         
         ORG   SLINE1                                                           
         DC    C' '                                                             
S1USER   DC    CL06' '                                                          
         DC    C' '                                                             
S1RTOTL  DC    CL12'   FILES'                                                   
         DC    C' '                                                             
S1RECS   DC    CL12'    RECS'                                                   
         DC    C' '                                                             
S1NCI    DC    CL12' PART-2 NCI'                                                
         DC    C' '                                                             
         DC    C' '                                                             
S1RLIVE  DC    CL12'LIVE FILES'                                                 
         DC    C' '                                                             
S1RDEAD  DC    CL12'DEAD FILES'                                                 
         DC    C' '                                                             
S1RERR   DC    CL12'ERROR FILE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
S1RXPRD  DC    CL12'  EXPIRED'                                                  
         DC    C' '                                                             
S1RSEMI  DC    CL12' VULNERABLE'                                                
         DC    C' '                                                             
S1RRETN  DC    CL12'  RETAINED'                                                 
         DC    C' '                                                             
         ORG                                                                    
         EJECT                                                                  
SLINE2   DC    CL165' '                                                         
         ORG   SLINE2                                                           
         DC    C' '                                                             
S2USER   DC    CL06' USER'                                                      
         DC    C' '                                                             
S2RTOTL  DC    CL12' '                                                          
         DC    C' '                                                             
S2RECS   DC    CL12' '                                                          
         DC    C' '                                                             
S2NCI    DC    CL12' '                                                          
         DC    C' '                                                             
         DC    C' '                                                             
S2RLIVE  DC    CL12' '                                                          
         DC    C' '                                                             
S2RDEAD  DC    CL12' '                                                          
         DC    C' '                                                             
S2RERR   DC    CL12' '                                                          
         DC    C' '                                                             
         DC    C' '                                                             
S2RXPRD  DC    CL12' '                                                          
         DC    C' '                                                             
S2RSEMI  DC    CL12' '                                                          
         DC    C' '                                                             
S2RRETN  DC    CL12' '                                                          
         DC    C' '                                                             
         ORG                                                                    
         EJECT                                                                  
SLINE3   DC    CL165' '                                                         
         ORG   SLINE3                                                           
         DC    C' '                                                             
S3USER   DC    CL06' '                                                          
         DC    C' '                                                             
S3RTOTL  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3RECS   DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3NCI    DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
         DC    C' '                                                             
S3RLIVE  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3RDEAD  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3RERR   DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
         DC    C' '                                                             
S3RXPRD  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3RSEMI  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3RRETN  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
         ORG                                                                    
         EJECT                                                                  
* LISTS OF PARAMETER VALUES AND EQUATES                                         
*                                                                               
YES      EQU   X'02'                                                            
NO       EQU   X'01'                                                            
DISK     EQU   X'01'                                                            
TAPE     EQU   X'02'                                                            
WKTAPE   EQU   X'82'                                                            
*                                                                               
MODEL    DC    X'01',CL7'INIT'                                                  
         DC    X'02',CL7'PRINT'                                                 
         DC    X'03',CL7'REPORT'                                                
         DC    X'04',CL7'COPY'                                                  
MODELX   DC    X'FF'                                                            
*                                                                               
INPUTL   DC    X'01',CL7'DISK'                                                  
         DC    X'02',CL7'TAPE'                                                  
         DC    X'82',CL7'WKTAPE'                                                
INPUTLX  DC    X'FF'                                                            
*                                                                               
OUTPUTL  DC    X'01',CL7'DISK'                                                  
         DC    X'02',CL7'TAPE'                                                  
OUTPUTLX DC    X'FF'                                                            
*                                                                               
TCOPYL   DC    X'02',CL7'YES'                                                   
         DC    X'02',CL7'TAPE'                                                  
TCOPYLX  DC    X'FF'                                                            
*                                                                               
MSGL     DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
MSGLX    DC    X'FF'                                                            
*                                                                               
WARNL    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
WARNLX   DC    X'FF'                                                            
*                                                                               
EXTRNLL  DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
EXTRNLLX DC    X'FF'                                                            
*                                                                               
REORGL   DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
REORGLX  DC    X'FF'                                                            
*                                                                               
COMPACTL DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
COMPACTX DC    X'FF'                                                            
*                                                                               
SORTL    DC    X'01',CL7'ALPHA'                                                 
         DC    X'02',CL7'NUMERIC'                                               
         DC    X'03',CL7'TIME'                                                  
SORTLX   DC    X'FF'                                                            
*                                                                               
LOCKL    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
LOCKLX   DC    X'FF'                                                            
*                                                                               
EXTIDL   DC    C'A',CL7'ADV'                                                    
         DC    C'C',CL7'CSC'                                                    
         DC    C'Q',CL7'FQA'                                                    
*&&US*&& DC    C'R',CL7'REP'                                                    
         DC    C'T',CL7'TST'                                                    
EXTIDLX  DC    X'FF'                                                            
*                                                                               
DSPNDXL  DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
DSPNDXLX DC    X'FF'                                                            
         EJECT                                                                  
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=RITAPEX,        X        
               RECFM=VB,BLKSIZE=0,LRECL=4004,BUFNO=2                            
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,BLKSIZE=0,LRECL=4004,BUFNO=2                            
*                                                                               
TAPECPY  DCB   DDNAME=TAPECPY,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,BLKSIZE=0,LRECL=4004,BUFNO=2                            
*                                                                               
         DS    0F                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',1024X'00'                          
UTL      DC    F'0',X'01',XL3'00',XL252'00'                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ/PRINT/VALIDATE A SET OF PARAMETER CARDS                                  
***********************************************************************         
VALPARM  CSECT                                                                  
         NMOD1 0,**VALP**                                                       
         DROP  R8,R9,RA            DROP WKMAINT CSECT'S EXTRA BASE REGS         
         LR    RA,R1                                                            
         USING PARMTBL,RA          RA=A(WKMAINT CSECT'S STORAGE)                
         LR    RC,R0                                                            
         USING DPRINT,RC           RC=A(WKMAINT CSECT'S PRINTER)                
*                                                                               
         LA    R1,MODE+4           CLEAR ERRNUM AND SET A(PARMTBL NTRY)         
         ST    R1,ERRNUM                                                        
         CLI   FRSTTIME,C'Y'       READ FIRST CARD                              
         BNE   VPARM2                                                           
         MVI   FRSTTIME,C'N'                                                    
*                                                                               
VPARM1   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         MVC   P(80),C                                                          
         CLI   C,C'*'              IGNORE COMMENT CARDS                         
         BE    VPARM1P                                                          
*                                                                               
VPARM1A  CLC   C(5),=CL8'DATE='    DATE=DD/MM/YY TO SET SYSTEM DATE             
         BNE   VPARM1B                                                          
         GOTO1 =V(DATVAL),DMCB,(0,C+5),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   VPARM1A1                                                         
         LA    R1,=CL8'DATE='                                                   
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     VPARMX                                                           
VPARM1A1 MVC   DATEIPL,C+5         OVERRIDE SYSTEM IPL DATE                     
         GOTO1 =V(DATCON),DMCB,(4,DATEIPL),(0,DATEYMD)                          
         GOTO1 (RF),(R1),(0,DATEYMD),(2,DATECPR)                                
         L     RF,=A(SSB)                                                       
         MVI   5(RF),X'80'         SET OFFLINE PASS OF DATE IN V(SSB)           
         MVC   8(6,RF),DATEYMD     PASS DATE AS C'YYMMDD'                       
         B     VPARM1P                                                          
*                                                                               
VPARM1B  CLC   C(6),=CL8'DDSIO='   DDSIO=XXXXXXXX TO SET WHICH DMDMGR           
         BNE   VPARM1C                                                          
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),C+6                                                      
         B     VPARM1P                                                          
*                                                                               
VPARM1C  CLC   C(7),=CL8'DSPACE='  DSPACE=X TO SET THE DATA SPACE               
         BNE   VPARM1X                                                          
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),C+7                                        
         B     VPARM1P                                                          
*                                                                               
VPARM1P  GOTO1 =V(PRINTER)                                                      
         B     VPARM1                                                           
*                                                                               
VPARM1X  GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                     
         L     RF,ACIREC                                                        
         XC    NXUSRINF,NXUSRINF                                                
         GOTO1 VDATAMGR,DMCB,=C'GLIST',WRKFIL,INDEX,,(RF)                       
         ICM   RE,15,NXUSRINF                                                   
         ICM   RF,15,NXUSRINF+4                                                 
         ST    RE,AWRKFLST         SAVE A(WRKF FILE LIST)                       
         ST    RF,AWRKFXPE         SAVE A(WRKF INDEX PAGE/ENTRY LIST)           
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         XC    0(2,RF),0(RF)       SET TO START AT PAGE ONE                     
         MVC   WRKFMAX,0(RE)       SAVE NUMBER OF WRKF FILES IN LIST            
         CLI   WRKFMAX,16                                                       
         BNH   *+6                                                              
         DC    H'0'                MAX OF 16 FILES THIS VERSION                 
         LA    RE,8(RE)                                                         
         LA    RF,FILEIX+8                                                      
         SR    R1,R1                                                            
VPARM1X1 CLI   0(RE),0             TEST END OF WRKF LIST                        
         BE    VPARM1X2                                                         
         ICM   R1,7,5(RE)          GET A(DTF)                                   
         MVC   0(7,RF),22(R1)      SAVE ORIGIONAL DTF FILE ID                   
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         B     VPARM1X1                                                         
VPARM1X2 L     RE,=V(DMENQDEQ)     SAVE A(DDS ENQ/DEQ ROUTINE)                  
         ST    RE,CIENQDEQ                                                      
*                                                                               
VPARM2   CLC   C(2),MODE+4         FIRST CARD OF SET MUST BE MODE CARD          
         BE    *+12                                                             
         MVI   ERRNUM,1                                                         
         B     VPARMX                                                           
         LA    R1,PARMTBL          INITIALIZE VALUES                            
         CLI   0(R1),X'FF'                                                      
         BE    VPARM4                                                           
         MVI   0(R1),0                                                          
         LA    R1,L'PARMTBL(R1)                                                 
         B     *-16                                                             
*                                                                               
VPARM4   MVC   FILEIX(8),=CL8' '   RESET FILE DTF NAMES                         
         MVC   FILEID,FILEIX                                                    
         XC    WRKFINP,WRKFINP     RESET FILE INPUT LIST                        
         LA    R1,REPTAB                                                        
         CLC   0(4,R1),=32X'FF'                                                 
         BE    *+18                                                             
         XC    0(L'REPTAB,R1),0(R1)                                             
         LA    R1,L'REPTAB(R1)                                                  
         B     *-20                                                             
         LA    R1,REPTAB                                                        
         LA    R0,L'REPTAB                                                      
         SR    R1,R0                                                            
         ST    R1,AREPTAB                                                       
         B     VPARM8                                                           
*                                                                               
VPARM6   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLI   C,C'*'                                                           
         BE    VPARMP              IGNORE COMMENT CARDS                         
         CLC   C(2),=C'/*'                                                      
         BNE   *+12                                                             
         MVI   FRSTTIME,C'X'                                                    
         B     VPARMX                                                           
         CLC   C(2),MODE+4                                                      
         BE    VPARMX                                                           
*                                                                               
VPARM8   MVC   P(80),C                                                          
         MVC   C+72(8),SPACES                                                   
         L     R2,ACIREC                                                        
         GOTO1 =V(SCANNER),DMCB,(C'C',C),(R2)                                   
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   ERRNUM,2            INVALID SYNTAX                               
         B     VPARMX                                                           
         SR    R0,R0                                                            
         IC    R0,4(R1)            R0=NUM OF PARMS ON CARD                      
*                                                                               
VPARMA   LA    R1,12(R2)           POINT TO KEYWORD NAME                        
         ST    R1,ERRNUM                                                        
         CLI   0(R2),2             MUST BE 2 TO 8 CHRS LONG                     
         BNL   *+12                                                             
VPARMB   MVI   ERRNUM,3                                                         
         B     VPARMX                                                           
         CLI   0(R2),8                                                          
         BH    VPARMB                                                           
         LA    R4,PARMTBL          SEARCH FOR KEYWORD IN TABLE                  
         SR    RF,RF                                                            
         IC    RF,0(R2)            MATCH ON 2 THRU N CHRS                       
         BCTR  RF,0                                                             
VPARMC   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),4(R4)                                                   
         BE    VPARMD                                                           
         LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BE    VPARMB                                                           
         B     VPARMC                                                           
*                                                                               
VPARMD   CLC   1(1,R2),3(R4)       KEYWORD VALUE MUST BE N THRU 8 CHRS          
         BNL   *+12                                                             
VPARME   MVI   ERRNUM,4                                                         
         B     VPARMX                                                           
         CLI   1(R2),8                                                          
         BH    VPARME                                                           
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         L     RF,12(R4)                                                        
         TM    2(R4),X'40'         KEYWORD VALUE IN LIST                        
         BO    VPARMF              YES                                          
         TM    2(R4),X'20'         KEYWORD VALUE BY ROUTINE                     
         BO    VPARMG              YES                                          
         MVC   0(8,RF),22(R2)      NO- SAVE VALUE                               
         MVI   0(R4),YES                                                        
         B     VPARMN                                                           
*                                                                               
VPARMF   CLI   0(RF),X'FF'         SEARCH VALUE LIST                            
         BE    VPARME                                                           
         EX    RE,*+8              MATCH ON N THRU 7 CHRS                       
         B     *+10                                                             
         CLC   1(0,RF),22(R2)                                                   
         BE    *+12                                                             
         LA    RF,8(RF)                                                         
         B     VPARMF                                                           
         MVC   0(1,R4),0(RF)       SAVE VALUE IN PARMTBL                        
         B     VPARMN                                                           
*                                                                               
VPARMG   BASR  RE,RF               GO TO ROUTINE WITH R2=A(ENTRY)               
         CLI   ERRNUM,0                                                         
         BNE   VPARMX                                                           
*                                                                               
VPARMN   LA    R2,32(R2)           BUMP TO NEXT PARM                            
         BCT   R0,VPARMA                                                        
*                                                                               
VPARMP   GOTO1 =V(PRINTER)         PRINT CARD                                   
         B     VPARM6              GO GET NEXT PARM CARD                        
*                                                                               
VPARMX   XIT1                                                                   
                                                                                
*----------------------------------------------------------------------         
* WRITE= AND WRSRV= CARD                                                        
*----------------------------------------------------------------------         
VWRITE   CLI   22(R2),C'Y'         SERVICE FILE WRITE=YES                       
         BE    VWRITX                                                           
         CLI   22(R2),C'N'         SERVICE FILE WRITE=NO                        
         BNE   VWRITERR                                                         
         MVI   WRITE,NO                                                         
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND-SSOOFF(RF),SSOWSRN FORCE SERVICE TO WRITE=NO            
         B     VWRITX                                                           
VWRITERR MVI   ERRNUM,4                                                         
VWRITX   BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* NUMBER OF CONTROL INTERVALS                                                   
*----------------------------------------------------------------------         
VNCIS    MVI   0(R4),1                                                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VNCISERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'65000'        MAXIMUM VALUE                                
         BH    VNCISERR                                                         
         C     RF,=F'10'           MINIMUM VALUE                                
         BL    VNCISERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STCM  RF,3,CICITOT                                                     
         B     *+8                                                              
         STCM  RF,3,CJCITOT                                                     
         B     *+8                                                              
VNCISERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* TRACKS PER CONTROL INTERVAL                                                   
*----------------------------------------------------------------------         
VTRKS    MVI   0(R4),1                                                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VTRKSERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'60'           MAXIMUM VALUE                                
         BH    VTRKSERR                                                         
         C     RF,=F'1'            MINIMUM VALUE                                
         BL    VTRKSERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STH   RF,CITRKS                                                        
         B     *+8                                                              
         STH   RF,CJTRKS                                                        
         B     *+8                                                              
VTRKSERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* BLOCK SIZE IN BYTES                                                           
*----------------------------------------------------------------------         
VBLKS    MVI   BLKSIZE,1                                                        
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VBLKSERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'6000'         OLD VALUE                                    
         BE    VBLKS1                                                           
         C     RF,=F'13680'        NEW VALUE                                    
         BE    VBLKS1                                                           
         B     VBLKSERR                                                         
VBLKS1   STH   RF,CIBLKLN                                                       
         B     *+8                                                              
VBLKSERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* KEEP NUMBER OF DAYS                                                           
*----------------------------------------------------------------------         
VKDAY    MVI   KDAYS,1                                                          
         CLI   MODE,3                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VKDAYERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BNP   VKDAYERR                                                         
         CH    RF,=H'1000'                                                      
         BH    VKDAYERR                                                         
         LNR   RF,RF                                                            
         ST    RF,REPKDAYS                                                      
         B     *+8                                                              
VKDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* LIVE NUMBER OF DAYS                                                           
*----------------------------------------------------------------------         
VLDAY    MVI   LDAYS,1                                                          
         CLI   MODE,3                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VLDAYERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BNP   VLDAYERR                                                         
         CH    RF,=H'1000'                                                      
         BH    VLDAYERR                                                         
         LNR   RF,RF                                                            
         ST    RF,REPLDAYS                                                      
         B     *+8                                                              
VLDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* DEAD NUMBER OF DAYS                                                           
*----------------------------------------------------------------------         
VDDAY    MVI   DDAYS,1                                                          
         CLI   MODE,3                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VDDAYERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BNP   VDDAYERR                                                         
         CH    RF,=H'1000'                                                      
         BH    VDDAYERR                                                         
         LNR   RF,RF                                                            
         ST    RF,REPDDAYS                                                      
         B     *+8                                                              
VDDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*----------------------------------------------------------------------         
* CREATE DATE                                                                   
*----------------------------------------------------------------------         
VCDATE   NTR1                                                                   
         GOTO1 =V(DATVAL),DMCB,(0,22(R2)),DUB                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    VCDERR                                                           
         GOTO1 =V(DATCON),DMCB,(0,DUB),(2,DUB1)                                 
         L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VCDERR                                                           
         OI    14(RF),X'80'        SET CDATE INPUT                              
         MVC   10(2,RF),DUB1                                                    
         B     *+8                                                              
VCDERR   MVI   ERRNUM,4                                                         
VCDX     XIT1                                                                   
                                                                                
*----------------------------------------------------------------------         
* CREATED RANGE DAYS                                                            
*----------------------------------------------------------------------         
VCRANGE  NTR1                                                                   
         MVI   CRANGE,1                                                         
         CLI   1(R2),1                                                          
         BL    VCRERR                                                           
         CLI   22(R2),C'0'         START DAY 0-9                                
         BL    VCRERR                                                           
         CLI   22(R2),C'9'                                                      
         BH    VCRERR                                                           
         IC    RF,22(R2)                                                        
         SLL   RF,28                                                            
         SRL   RF,28               ZERO MEANS TODAY                             
         LNR   RF,RF                                                            
         ST    RF,REPCSDAY         SAVE NEGATIVE START VALUE                    
         CLI   1(R2),1                                                          
         BNE   VCR1                                                             
         ST    RF,REPCEDAY         END VALUE DEFAULTS TO START                  
         B     VCR2                                                             
VCR1     CLI   1(R2),3                                                          
         BNE   VCRERR                                                           
         CLI   24(R2),C'0'         END DAY 0-9                                  
         BL    VCRERR                                                           
         CLI   24(R2),C'9'                                                      
         BH    VCRERR                                                           
         IC    RF,24(R2)                                                        
         SLL   RF,28                                                            
         SRL   RF,28               ZERO MEANS TODAY                             
         LNR   RF,RF                                                            
         ST    RF,REPCEDAY         SAVE NEGATIVE END VALUE                      
         CLC   22(1,R2),24(R2)                                                  
         BNL   VCR2                CHECK START VERSUS END                       
         MVC   REPCEDAY,REPCSDAY                                                
         ST    RF,REPCSDAY                                                      
VCR2     MVC   DUB1,DATEIPL                                                     
         GOTO1 =V(DATCON),DMCB,(4,DUB1),(0,DUB)                                 
         L     RF,REPCSDAY                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(RF)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPCSDAT)                            
         L     RF,REPCEDAY                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(RF)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPCEDAT)                            
         B     *+8                                                              
VCRERR   MVI   ERRNUM,4                                                         
VCRX     XIT1                                                                   
                                                                                
*----------------------------------------------------------------------         
* DEAD RANGE DAYS - NO LONGER SUPPORTED                                         
*----------------------------------------------------------------------         
VDRANGE  NTR1                                                                   
         B     VDRERR              REMOVED W_AGEDD FROM KEY                     
*&&DO                                                                           
         MVI   DRANGE,1                                                         
         CLI   1(R2),1                                                          
         BL    VDRERR                                                           
         CLI   22(R2),C'0'         START DAY 0-9                                
         BL    VDRERR                                                           
         CLI   22(R2),C'9'                                                      
         BH    VDRERR                                                           
         IC    RF,22(R2)                                                        
         SLL   RF,28                                                            
         SRL   RF,28               ZERO MEANS TODAY                             
         LNR   RF,RF                                                            
         ST    RF,REPDSDAY         SAVE NEGATIVE START VALUE                    
         CLI   1(R2),1                                                          
         BNE   VDR1                                                             
         ST    RF,REPDEDAY         END VALUE DEFAULTS TO START                  
         B     VDR2                                                             
VDR1     CLI   1(R2),3                                                          
         BNE   VDRERR                                                           
         CLI   24(R2),C'0'         END DAY 0-9                                  
         BL    VDRERR                                                           
         CLI   24(R2),C'9'                                                      
         BH    VDRERR                                                           
         IC    RF,24(R2)                                                        
         SLL   RF,28                                                            
         SRL   RF,28               ZERO MEANS TODAY                             
         LNR   RF,RF                                                            
         ST    RF,REPDEDAY         SAVE NEGATIVE END VALUE                      
         CLC   22(1,R2),24(R2)                                                  
         BNL   VDR2                CHECK START VERSUS END                       
         MVC   REPDEDAY,REPDSDAY                                                
         ST    RF,REPDSDAY                                                      
VDR2     MVC   DUB1,DATEIPL                                                     
         GOTO1 =V(DATCON),DMCB,(4,DUB1),(0,DUB)                                 
         L     RF,REPDSDAY                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(RF)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPDSDAT)                            
         L     RF,REPDEDAY                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(RF)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(2,REPDEDAT)                            
         B     *+8                                                              
*&&                                                                             
VDRERR   MVI   ERRNUM,4                                                         
VDRX     XIT1                                                                   
                                                                                
*----------------------------------------------------------------------         
* FILE CLASS                                                                    
*----------------------------------------------------------------------         
VCLASS   NTR1                                                                   
         MVI   CLASS,1                                                          
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VCLASSX                                                          
         XC    CLASSL,CLASSL                                                    
         CLC   22(4,R2),=C'ALL '                                                
         BE    VCLASSX                                                          
VCLASS2  SR    R0,R0               R0=NUM OF CLASS INPUT CHRS                   
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    VCLASSX                                                          
         LA    R5,22(R2)           R5=A(INPUT CLASS CHR)                        
         LA    R6,CLASSL+1         R6=A(CLASS LIST CHR)                         
*                                                                               
         MVI   CLASSL,C'+'         FIRST CHR CAN BE + OR -                      
         CLI   0(R5),C'+'                                                       
         BE    *+16                                                             
         CLI   0(R5),C'-'                                                       
         BNE   VCLASS4                                                          
         MVI   CLASSL,C'-'                                                      
         SH    R0,=H'1'                                                         
         BNP   VCLASSE                                                          
         LA    R5,1(R5)                                                         
*                                                                               
VCLASS4  CLI   0(R5),C'*'          EACH INPUT CHR MUST BE VALID                 
         BE    *+12                                                             
         CLI   0(R5),C'A'                                                       
         BL    VCLASSE                                                          
         MVC   0(1,R6),0(R5)                                                    
         LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,VCLASS4                                                       
         L     RF,AREPTAB          SAVE IN REPTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VCLASSE                                                          
         MVC   17(11,RF),CLASSL                                                 
         B     *+8                                                              
*                                                                               
VCLASSE  MVI   ERRNUM,4                                                         
VCLASSX  XIT1                                                                   
                                                                                
*----------------------------------------------------------------------         
* FILE STATUS                                                                   
*----------------------------------------------------------------------         
VSTAT    NTR1                                                                   
         MVI   STATUS,1                                                         
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VSTATX                                                           
         MVI   REPSTAT,0                                                        
         CLI   1(R2),4                                                          
         BH    VSTATERR                                                         
         CLC   22(4,R2),=C'ALL '                                                
         BE    VSTATX                                                           
VSTAT2   SR    R0,R0               R3=NUM OF STATUS CHRS                        
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    VSTATX                                                           
         LA    R5,22(R2)           R5=A(STATUS CHR)                             
*                                                                               
VSTAT4   MVC   BYTE,REPSTAT                                                     
         CLI   0(R5),C'A'                                                       
         BNE   *+8                                                              
         OI    REPSTAT,W_STAC                                                   
         CLI   0(R5),C'H'                                                       
         BNE   *+8                                                              
         OI    REPSTAT,W_STHO                                                   
         CLI   0(R5),C'L'                                                       
         BNE   *+8                                                              
         OI    REPSTAT,W_STLIVE                                                 
         CLI   0(R5),C'K'                                                       
         BNE   *+8                                                              
         OI    REPSTAT,W_STKE                                                   
         CLI   0(R5),C'S'                                                       
         BNE   *+8                                                              
         OI    REPSTAT,W_STSE                                                   
         CLI   0(R5),C'D'                                                       
         BNE   *+8                                                              
         OI    REPSTAT,W_STDEAD                                                 
         CLC   REPSTAT,BYTE        MUST HAVE CHANGED SOMETHING                  
         BE    VSTATERR                                                         
*                                                                               
VSTAT6   LA    R5,1(R5)                                                         
         BCT   R0,VSTAT4                                                        
         L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VSTATERR                                                         
         MVC   28(1,RF),REPSTAT                                                 
         B     *+8                                                              
VSTATERR MVI   ERRNUM,4                                                         
VSTATX   XIT1                                                                   
         SPACE 2                                                                
VFILE    NTR1                      WRKF FILE DTF/DD OVERRIDE NAME               
         CLI   1(R2),5                                                          
         BL    VFILEERR                                                         
         CLI   1(R2),7                                                          
         BH    VFILEERR                                                         
         IC    RF,FILE             BUMP NUMBER OF FILE NAMES INPUT              
         LA    RF,1(RF)                                                         
         STC   RF,FILE                                                          
         MVC   DUB,22(R2)                                                       
         L     RF,AWRKFLST         POINT TO WRKF FILE LIST                      
         CLI   WRKFMAX,1                                                        
         BNE   VFILE1                                                           
         LA    RF,8(RF)            NO RULES IF ONLY ONE FILE                    
         B     VFILE3                                                           
VFILE1   LA    RF,8(RF)                                                         
         CLI   0(RF),0             TEST END OF TABLE                            
         BE    VFILEERR                                                         
         CLC   1(1,RF),DUB+4       FIFTH CHR MUST MATCH FILE ID CHR             
         BNE   VFILE1                                                           
VFILE3   SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
VFILE4   LA    RF,WRKFINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'10'                                                      
         BO    VFILEERR                                                         
         OI    0(RF),X'10'         SET REFERENCED VIA FILE=XXXXX                
         SLL   R1,3                                                             
         LA    RF,FILEID(R1)                                                    
         MVC   0(7,RF),DUB         SAVE INPUT OVERRIDE IN FILEID LIST           
         B     *+8                                                              
VFILEERR MVI   ERRNUM,4                                                         
         XIT1                                                                   
         SPACE 2                                                                
VWKID    NTR1                                                                   
         SR    R0,R0               WRKF FILE ID = LIST OF WRKF ID CHRS          
         ICM   R0,1,1(R2)                                                       
         BZ    VWKIDERR            R0=NUMBER OF FILES                           
         LA    R5,22(R2)           R5=A(NEXT WRKF FILE CHR)                     
VWKID1   L     RF,AWRKFLST         POINT TO LIST OF WRKF FILES                  
         LA    RF,8(RF)                                                         
VWKID2   CLI   0(RF),0             TEST END OF TABLE                            
         BE    VWKIDERR                                                         
         CLC   1(1,RF),0(R5)       TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VWKID3                                                           
         LA    RF,8(RF)                                                         
         B     VWKID2                                                           
VWKID3   MVC   WKID(1),0(RF)       SET WKID TO INTERNAL WRKF FILE NUM           
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RF,WRKFINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VWKID4                                                           
         IC    R1,WRKFINP          BUMP NUM OF WRKF FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,WRKFINP                                                       
VWKID4   TM    0(RF),X'01'         TEST DUPLICATE                               
         BO    VWKIDERR                                                         
         OI    0(RF),X'01'         SET REFERENCED BY WKID INPUT                 
         LA    R5,1(R5)                                                         
         BCT   R0,VWKID1           BACK FOR NEXT WRKF FILE ID CHR               
         B     *+8                                                              
VWKIDERR MVI   ERRNUM,4                                                         
         XIT1                                                                   
         SPACE 2                                                                
VUSER    NTR1                      USERID = ALL OR XXX...                       
         CLI   MODE,2                                                           
         BNL   *+12                                                             
VUSER0   MVI   ERRNUM,3                                                         
         B     VUSERX                                                           
         MVC   REPUSERA,SPACES                                                  
         CLI   22(R2),C'-'                                                      
         BNE   *+14                                                             
         MVC   REPUSERA(9),23(R2)                                               
         B     *+10                                                             
         MVC   REPUSERA,22(R2)                                                  
         XC    DUB(2),DUB                                                       
         CLC   REPUSERA(3),=C'ALL '                                             
         BNE   VUSER1                                                           
         CLI   REPUSERA+4,C' '     ALLOW USERIDS LIKE ALLIED                    
         BNE   VUSER1                                                           
         CLI   REPUSERA+3,C' '                                                  
         BE    VUSER3                                                           
         L     RF,AWRKFLST         POINT TO LIST OF WRKF FILES                  
         LA    RF,8(RF)                                                         
VUSER0A  CLI   0(RF),0             TEST END OF TABLE                            
         BE    VUSERERR                                                         
         CLC   1(1,RF),REPUSERA+3  TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VUSER0B                                                          
         LA    RF,8(RF)                                                         
         B     VUSER0A                                                          
VUSER0B  SR    R1,R1               ALLX MATCHES TO WRKF FILE ID X               
         IC    R1,0(RF)                                                         
         LA    RF,WRKFINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VUSER0C                                                          
         IC    R1,WRKFINP          BUMP NUM OF WRKF FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,WRKFINP                                                       
VUSER0C  TM    0(RF),X'02'         TEST DUPLICATE USER=ALLX                     
         BO    VUSERERR                                                         
         OI    0(RF),X'02'         SET REFERENCED BY USER=ALLX INPUT            
         B     VUSER3                                                           
*                                                                               
VUSER1   L     R5,ACXREC           READ USER ID RECORD                          
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'I'                                                       
         MVC   15(10,R5),REPUSERA                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'CTFILE',(R5),(R5)                        
         CLI   8(R1),0                                                          
         BNE   VUSERERR                                                         
         LA    R5,28(R5)                                                        
         SR    RE,RE                                                            
VUSER2   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     VUSER2                                                           
         MVC   DUB(2),2(R5)        DUB=USER ID NUM                              
*                                                                               
VUSER3   OC    DUB(2),DUB          ONLY ONE USER=ALL ALLOWED                    
         BNZ   VUSER4                                                           
         TM    USER,X'80'                                                       
         BZ    *+12                                                             
         CLI   REPUSERA+3,C' '                                                  
         BE    VUSERERR                                                         
         OI    USER,X'80'          SET USER=ALL INPUT                           
         B     VUSER6                                                           
*                                                                               
VUSER4   CLI   22(R2),C'-'         USER=-XXX ONLY AFTER USER=ALL                
         BNE   VUSER5                                                           
         TM    USER,X'80'                                                       
         BZ    VUSERERR                                                         
         OI    USER,X'01'          SET USER=-XXX INPUT                          
         OI    DUB,X'80'                                                        
         B     VUSER6                                                           
*                                                                               
VUSER5   TM    USER,X'81'          USER=XXX MUST BE FIRST                       
         BNZ   VUSERERR                                                         
         OI    USER,X'02'          SET USER=XXX INPUT                           
         B     VUSER6                                                           
*                                                                               
VUSER6   L     RF,AREPTAB          BUMP TO NEXT REPTAB ENTRY                    
         LA    RF,L'REPTAB(RF)                                                  
         CLC   0(4,RF),=32X'FF'                                                 
         BE    VUSERERR            REPTAB IS FULL                               
         ST    RF,AREPTAB                                                       
         MVC   0(2,RF),DUB                                                      
         OC    DUB(2),DUB                                                       
         BNZ   *+12                                                             
         ST    RF,AREPALL          SAVE A(ALL ENTRY)                            
         B     VUSER8                                                           
*                                                                               
VUSER7   XC    INDEX,INDEX         FIND WHICH WK FOR USER ID                    
         MVC   NXSRCID(2),DUB                                                   
         GOTO1 VDATAMGR,DMCB,=C'GFILE',WRKFIL,INDEX,,(R5)                       
         SR    R1,R1                                                            
         IC    R1,NXINFO           GET INTERNAL WRKF FILE NUMBER                
         LA    RF,WRKFINP(R1)                                                   
         TM    0(RF),X'0F'         TEST IF FIRST REF TO WRKF FILE               
         BNZ   VUSER7A                                                          
         IC    R1,WRKFINP          BUMP NUM OF WRKF FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,WRKFINP                                                       
VUSER7A  OI    0(RF),X'04'         SET FLAG TO SHOW REF VIA USERID              
*                                                                               
VUSER8   L     RF,AREPTAB          SET END OF TABLE                             
         LA    RF,L'REPTAB(RF)                                                  
         MVC   0(2,RF),=X'FFFF'                                                 
         B     *+8                                                              
VUSERERR MVI   ERRNUM,4                                                         
VUSERX   XIT1                                                                   
                                                                                
***********************************************************************         
* FILEID = ALL OR SPPSDC OR SPPSNNC                                             
***********************************************************************         
VREPT    NTR1                      FILEID = ALL OR SPPSDC OR SPPSNNC            
         ST    R2,FULL                                                          
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3            CANNOT SPECIFY FOR INIT                      
         B     VUSERX                                                           
*                                                                               
         MVI   REPORT,1            SET REPORT TO 1                              
         XC    REPSYSP(13),REPSYSP CLEAR FILEID AREA                            
*                                                                               
         CLI   1(R2),3             3 CHR FILE ID                                
         BNE   VREPT1                                                           
         CLC   22(3,R2),=C'ALL'      TEST FOR ALL                               
         BNE   VREPTERR                                                         
         MVC   REPSYSP(6),=C'******' SET ALL WILDCARDS                          
         B     VREPT2                                                           
*                                                                               
VREPT1   MVC   REPSYSP(4),22(R2)   SET SYSPRG FROM SCANNER BLOCK                
         CLI   1(R2),6             6CHR=SHORT FORM                              
         BNE   VREPT1A                                                          
         MVC   REPDAY,26(R2)       SET DAY AND CLASS                            
         MVC   REPCLAS,27(R2)                                                   
         B     VREPT2                                                           
*                                                                               
VREPT1A  TM    26(R2),X'F0'        PACK AND STORE DAY NUMBER                    
         BNO   VREPTERR                                                         
         TM    27(R2),X'F0'                                                     
         BNO   VREPTERR                                                         
         PACK  FULL,26(2,R2)                                                    
         L     R1,FULL                                                          
         SRL   R1,4                                                             
         STC   R1,REPDAY                                                        
         MVC   REPCLAS,28(R2)      STORE CLASS                                  
*                                                                               
VREPT2   LA    R2,32(R2)           BUMP SCANNER BLOCK                           
         BCT   R0,*+8                                                           
         B     VREPT7                                                           
         CLI   1(R2),0             EXIT IF END OR KEYWORD                       
         BNE   VREPT7                                                           
         TM    2(R2),X'80'         CHECK FILE REF NUMBER                        
         BZ    VREPT4                                                           
         CLC   4(4,R2),=F'1'                                                    
         BL    VREPTERR                                                         
         CLC   4(4,R2),MAXSEQ                                                   
         BH    VREPTERR                                                         
         MVC   REPFILN,6(R2)                                                    
         B     VREPT8                                                           
*                                                                               
VREPT4   SR    RF,RF               SEE IF VALID TIME EXPRESSION                 
         IC    RF,0(R2)                                                         
         GOTO1 =V(TIMBER),DMCB,(X'80',(RF)),(X'02',DUB),12(R2)                  
         CLI   0(R1),0                                                          
         BNE   VREPT5                                                           
         MVC   REPTIMS+0(4),DUB    SET LOW HIGH TIME                            
         OI    REPFLAG,X'04'       SET LOW AND HIGH TIME INPUT                  
         B     VREPT6                                                           
VREPT5   OI    0(R1),X'40'         SET TO VALIDATE A SINGLE TIME                
         GOTO1 (RF),(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   VREPTERR                                                         
         MVC   REPTIMS+2(2),DUB    SET HIGH TIME                                
         OI    REPFLAG,X'02'       SET HIGH TIME INPUT                          
VREPT6   SR    RF,RF                                                            
         ICM   RF,3,REPTIMS+0                                                   
         MH    RF,=H'180'                                                       
         SRL   RF,2                LOWTIME=SECS*3/4                             
         STCM  RF,3,REPTIMS+0                                                   
         SR    RF,RF                                                            
         ICM   RF,3,REPTIMS+2                                                   
         MH    RF,=H'180'                                                       
         SRL   RF,2                HIGHTIME=SECS*3/4                            
         STCM  RF,3,REPTIMS+2                                                   
         B     VREPT8                                                           
*                                                                               
VREPT7   SH    R2,=H'32'           DECR SCANNER BLOCK                           
         AH    R0,=H'1'                                                         
*                                                                               
VREPT8   L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VREPTERR                                                         
         MVC   2(13,RF),REPSYSP                                                 
         B     VREPTX                                                           
VREPTERR EQU   *                                                                
         L     R2,FULL                                                          
         MVI   ERRNUM,4                                                         
VREPTX   XIT1  REGS=(R0,R2)                                                     
         LTORG                                                                  
                                                                                
***********************************************************************         
* OPEN WRKF FILE(S) REFERENCED BY INPUT PARAMS                                  
***********************************************************************         
OPNWK    CSECT                                                                  
         NMOD1 0,*OPENWK*                                                       
         LR    RA,R1                                                            
         USING PARMTBL,RA          RA=A(WKMAINT CSECT'S STORAGE)                
         LA    R1,WRKFINP          POINT TO LIST OF INPUT WRKF IDS              
         ST    R1,AWRKFINP                                                      
*                                                                               
OPNWK1   L     R1,AWRKFINP         BUMP TO NEXT FILE IN LIST                    
         LA    R1,1(R1)                                                         
         ST    R1,AWRKFINP                                                      
         CLI   0(R1),X'FF'         TEST END OF LIST                             
         BE    OPNWK6                                                           
         TM    0(R1),X'0F'         TEST IF FILE REFERENCED                      
         BZ    OPNWK1                                                           
         LA    R0,WRKFINP                                                       
         SR    R1,R0               R1=INTERNAL WRKF FILE NUM                    
         SLL   R1,3                                                             
         L     RF,AWRKFLST         INDEX INRO WRKF FILE LIST                    
         AR    RF,R1                                                            
         MVC   WRKFID,WRKFIL                                                    
         MVC   WRKFID+4(1),1(RF)   SET WRKF FILE ID FOR DATAMGR                 
         MVC   WRKFINT,0(RF)       SET WRKF FILE INTERNAL NUM                   
         MVC   WRKFEXT,4(RF)       SET WRKF FILE EXTERNAL NUM                   
         MVC   WRKFDTF+1(3),5(RF)  SET WRKF FILE A(DTF)                         
         LA    RF,FILEIX(R1)                                                    
         MVC   FILEIX(8),0(RF)     SET ORIGINAL DTF NAME                        
         LA    RF,FILEID(R1)                                                    
         MVC   FILEID(8),0(RF)     SET OVERRIDE DTF NAME                        
*                                                                               
OPNWK2   L     R2,WRKFDTF          R2=A(WRKFIL DTF)                             
         USING DTFPHD,R2                                                        
         MVI   DUB,C'N'                                                         
         MVC   DUB+1(7),FILEID                                                  
         MVI   DUB+8,C'X'                                                       
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P2,ACXREC                                                        
         MVC   P4,WRKFDTF                                                       
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6,=X'00010100'                                                  
*                                                                               
         TM    DTFOPEN,X'20'       TEST IF ALREADY OPEN                         
         BZ    OPNWK3              NO                                           
         CLC   DTFFID,FILEID       TEST IF SAME FILE ID                         
         BE    OPNWK4              YES                                          
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 VDATAMGR,P0,DADDS                                                
*                                                                               
OPNWK3   MVC   DTFFID,FILEID                                                    
         GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',DUB                               
*                                                                               
OPNWK4   MVC   DNEXT,=X'00010000'                                               
         MVC   P1,=A(RDID)                                                      
         GOTO1 VDATAMGR,P0,DADDS   READ FIRST RECORD                            
         DROP  R2                                                               
*                                                                               
         CLI   MODE,1              EXIT IF MODE IS INITIALISE                   
         BE    OPNWKX                                                           
*                                                                               
OPNWK5   L     R2,ACXREC           READ FIRST INDEX RECORD                      
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CXADDR,(R2)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,WRKFDTF          SET CIDATA STORED INFRONT OF DTF             
         SH    RE,=H'40'                                                        
         MVC   0(20,RE),0(R2)      EXTRACT PART 1 INDEX DATA                    
         TM    0(RE),X'80'                                                      
         BZ    *+14                                                             
         MVC   20(20,RE),L'W_INDEX(R2) EXTRACT PART 2 INDEX DATA                
         NI    0(RE),X'7F'                                                      
         MVC   14(1,RE),WRKFINT    SET INTERNAL WRKF FILE NUM                   
         MVC   15(5,RE),WRKFID     SET ALPHA WRKF FILE NAME                     
         MVC   CIDATA,0(RE)                                                     
         L     RE,WRKFDTF          SET F/L REC LEN IN WRKFIL DTF                
         LA    RE,52(RE)                                                        
         MVC   0(2,RE),CIBLKLN                                                  
         OI    0(RE),X'80'                                                      
         B     OPNWK1              BACK FOR NEXT WRKF FILE                      
*                                                                               
OPNWK6   XC    CXPAGE,CXPAGE       SET FIRST INDEX ENTRY                        
         LH    R5,CICINDX                                                       
         BCTR  R5,0                                                             
         STH   R5,CXENTRY                                                       
*                                                                               
OPNWKX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* INITIALISE A WRKF FILE                                                        
*                                                                               
INITWK   CSECT                                                                  
         NMOD1 0,*INITWK*                                                       
         LR    RA,R1                                                            
         USING PARMTBL,RA          RA=A(WKMAINT CSECT'S STORAGE)                
         MVI   ERRNUM,0                                                         
*                                                                               
IWK      CLI   ENTRYS,0            SET DEFAULT VALUES IF NOT INPUT              
         BNE   *+10                                                             
         MVC   CICITOT,=H'100'     DEFAULT NUM OF ENTRYS                        
         CLI   CISIZE,0                                                         
         BNE   *+10                                                             
         MVC   CITRKS,=H'1'        DEFAULT TRACKS PER CI                        
         CLI   BLKSIZE,0                                                        
         BNE   *+10                                                             
         MVC   CIBLKLN,=H'13680'   DEFAULT BLOCK SIZE                           
         CLI   OENTRYS,0                                                        
         BNE   *+16                                                             
         MVC   CJCITOT(14),=X'000000007FFF7FFF000000010100'                     
         XC    CJCITOT+14(6),CJCITOT+14                                         
         MVI   CIRSNF,1            SET RSN IS REL POSN IN INDEX                 
         TM    DSPNDX,X'02'                                                     
         BZ    *+8                                                              
         OI    CIRSNF,X'02'        SET INDEX RECS IN DATA SPACE                 
*                                                                               
         BAS   RE,IWKLOCK          LOCK WORKER FILE                             
*                                                                               
         SR    RE,RE               SET BLOCK SIZE MOD ENTRY SIZE                
         LH    RF,CIBLKLN                                                       
         LA    R0,L'W_INDEX                                                     
         DR    RE,R0                                                            
         MR    RE,R0                                                            
         STH   RF,CIBLKLN                                                       
*                                                                               
IWK2     XC    P1(24),P1           CALC BLOCKS PER TRACK                        
         MVC   P1,=A(DARPT)                                                     
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,WRKFDTF                                                       
         GOTO1 VDATAMGR,P0,DADDS                                                
         LH    RF,P3+2                                                          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF BLOCK TOO BIG FOR TRACK               
         STH   RF,CIHIREC                                                       
         MH    RF,CITRKS                                                        
         ST    RF,FULL             FULL=RECORDS PER CI                          
*                                                                               
         SR    RE,RE               CALC ENTRYS PER RECORD                       
         LH    RF,CIBLKLN                                                       
         LA    R0,L'W_INDEX                                                     
         DR    RE,R0                                                            
         STH   RF,CIENTRYS                                                      
*                                                                               
         LH    R0,CIENTRYS         OPTIMISE PART 1 NUMBER OF CI'S               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         DR    RE,R0                                                            
         CH    RE,=H'10'                                                        
         BNL   *+12                                                             
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         STCM  RF,3,CICITOT                                                     
         SR    RE,RE               OPTIMISE PART 2 NUMBER OF CI'S               
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         OC    CJCITOT,CJCITOT                                                  
         BZ    IWK5                                                             
         LA    RF,1(RF)                                                         
         AH    RF,CJCITOT                                                       
         DR    RE,R0                                                            
         CH    RE,=H'10'                                                        
         BNL   INWK3                                                            
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         ICM   RE,3,CICITOT                                                     
         SR    RF,RE                                                            
         STH   RF,CJCITOT                                                       
*                                                                               
INWK3    SR    RE,RE               CALC PAGE/ENTRY OF PART 2 INDEX              
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         LA    RF,1(RF)                                                         
         DR    RE,R0                                                            
         STH   RF,CJPAGE                                                        
         STH   RE,CJENTRY                                                       
         SR    RE,RE               CALC DISK ADDR OF PART 2 INDEX               
         LH    R0,CIHIREC                                                       
         DR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         STH   RF,CJNDXADR                                                      
         STC   RE,CJNDXADR+2                                                    
         MVI   CJNDXADR+3,0                                                     
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CICITOT        CALC DISK ADDR OF PART 2 CI                  
         MH    RE,CITRKS                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,3,CJSTTRK                                                     
*                                                                               
IWK5     LH    R0,CIENTRYS         CALC NUM OF INDEX PAGES                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LA    RF,1(RF)                                                         
         AH    RF,CJCITOT                                                       
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STH   RF,CIPAGES                                                       
*                                                                               
         SR    RE,RE               CALC NUM OF CI'S TO HOLD INDEX               
         L     R0,FULL                                                          
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         CH    RF,CINDXMIN         CHECK FOR MINIMUM                            
         BNL   *+8                                                              
         LH    RF,CINDXMIN                                                      
         STH   RF,CICINDX                                                       
         MH    RF,CITRKS                                                        
         LA    RF,1(RF)                                                         
         STH   RF,CIFDTRK          SET TRACK NUM OF FIRST DATA CI               
         MVC   CFWFXID,EXTID       SET WRKF EXTERNAL ID (T/A)                   
*                                                                               
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P1,=A(WTCKD)                                                     
         MVC   P2,ACXREC                                                        
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,WRKFDTF                                                       
         LA    R0,P6                                                            
         ST    R0,P5                                                            
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,CICITOT        R3=NUM OF ACTIVE INDEX ENTRYS                
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LA    R3,1(R3)                                                         
         AH    R3,CJCITOT                                                       
         LH    R4,CICINDX                                                       
         MH    R4,CITRKS                                                        
         MH    R4,CIHIREC          R4=NUM OF INDEX RECS                         
         XC    FULL,FULL           SET DUB TO PAGE/ENTRY OF END PART 1          
         MVC   DUB(2),=X'7FFF'                                                  
         OC    CJCITOT,CJCITOT                                                  
         BZ    IWK6                                                             
         MVC   DUB(2),CJPAGE                                                    
         LH    RF,CJENTRY                                                       
         SH    RF,=H'1'                                                         
         STH   RF,DUB+2                                                         
         BNM   IWK6                                                             
         LH    RF,CJPAGE                                                        
         BCTR  RF,0                                                             
         STH   RF,DUB                                                           
         LH    RF,CIENTRYS                                                      
         BCTR  RF,0                                                             
         STH   RF,DUB+2                                                         
*                                                                               
IWK6     L     R5,ACXREC           WRITE INDEX RECORDS LOOP                     
         LH    R6,CIENTRYS         R6=NUM OF 00 ENTRYS                          
         SR    R7,R7               R7=NUM OF FF ENTRYS                          
         CR    R3,R6                                                            
         BL    *+10                                                             
         SR    R3,R6               FULL INDEX PAGE                              
         B     IWK6A                                                            
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         SR    R6,R6               EMPTY INDEX PAGE                             
         LH    R7,CIENTRYS                                                      
         B     IWK6A                                                            
         LR    R6,R3               PARTIAL INDEX PAGE                           
         LH    R7,CIENTRYS                                                      
         SR    R7,R3                                                            
         SR    R3,R3                                                            
*                                                                               
IWK6A    LTR   R6,R6                                                            
         BZ    IWK6B                                                            
         XC    0(L'W_INDEX,R5),0(R5)                                            
         LA    R5,L'W_INDEX(R5)                                                 
         BCT   R6,*-10                                                          
IWK6B    LTR   R7,R7                                                            
         BZ    IWK6C                                                            
         MVC   0(L'W_INDEX,R5),=32X'FF'                                         
         LA    R5,L'W_INDEX(R5)                                                 
         BCT   R7,*-10                                                          
*                                                                               
IWK6C    OC    FULL(2),FULL        SET FIRST PAGE DATA                          
         BNZ   IWK6C1                                                           
         L     R5,ACXREC                                                        
         MVC   0(20,R5),CIDATA                                                  
         MVC   L'W_INDEX(20,R5),CIDATA+20                                       
         OC    CJCITOT,CJCITOT     SET PART 2 INDEX PRESENT                     
         BZ    IWK6D                                                            
         OI    0(R5),X'80'                                                      
IWK6C1   CLC   FULL(2),DUB         SET END OF PART 1 INDEX                      
         BNE   IWK6D                                                            
         LH    R5,DUB+2                                                         
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         MVC   0(L'W_INDEX,R5),=32X'FF'                                         
*                                                                               
IWK6D    GOTO1 VDATAMGR,P0,DADDS                                                
         OC    P3(2),P3                                                         
         BNZ   IWKC                                                             
         LH    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STH   RE,FULL                                                          
         BCT   R4,IWK6                                                          
*                                                                               
IWK8     L     R5,ACIREC           POINT TO CI DATA RECORD                      
         USING W_RECD,R5                                                        
         ST    R5,P2                                                            
         LR    RE,R5                                                            
         LH    RF,CIBLKLN                                                       
         XCEF                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT        CALC NUM OF TRACKS OF DATA CI'S              
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         SR    R4,R4                                                            
         ICM   R4,3,CJCITOT                                                     
         MH    R4,CJTRKS                                                        
         AR    R4,R0               R4=NUM OF TRACKS IN PARTS 1 AND 2            
*                                                                               
IWKA     LH    R0,CIHIREC          WRITE DATA CI RECORDS LOOP                   
         GOTO1 VDATAMGR,P0,DADDS                                                
         OC    P3(2),P3            TEST FOR ERRORS ON WRITE                     
         BNZ   IWKC                                                             
         BCT   R0,IWKA+4                                                        
         BCT   R4,IWKA                                                          
*                                                                               
IWKB     MVC   P1,=A(WTERASE)      ERASE TO END OF EXTENT                       
         XC    P2(8),P2                                                         
         GOTO1 VDATAMGR,P0,DADDS                                                
         B     IWKX                                                             
*                                                                               
IWKC     MVI   ERRNUM,1            SET END OF FILE                              
         TM    P3+1,X'04'                                                       
         BO    *+8                                                              
         MVI   ERRNUM,2            SET DISK ERROR                               
*                                                                               
IWKX     BAS   RE,IWKUNLK          UNLOCK WORKER FILE                           
         XIT1                                                                   
         SPACE 2                                                                
IWKLOCK  LA    R0,C'E'             LOCK/UNLOCK WRKFIL FILE                      
         B     *+8                                                              
IWKUNLK  LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,((R0),WRKFID)                                          
         L     RE,CISAVRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* ROUTINE TO PRINT CONTROL INTERVAL DATA FOR WRKF FILE                          
*                                                                               
WKOUT    CSECT                                                                  
         NMOD1 0,**WKOUT*                                                       
         LR    RA,R1                                                            
         USING PARMTBL,RA          RA=A(WKMAINT CSECT'S STORAGE)                
         LR    RC,R0                                                            
         USING DPRINT,RC           RC=A(WKMAINT CSECT'S PRINTER)                
         L     R7,=A(WKOUTA)                                                    
*                                                                               
         LH    R0,CIBLKLN                                                       
         BAS   R5,WKOUT2+4         RECORD LENGTH                                
         LH    R0,CIHIREC                                                       
         BAS   R5,WKOUT2           RECS PER TRACK                               
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR INDEX                               
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR PART1                               
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         MH    R0,CJTRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR PART2                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS/INDEX CI                                
         LH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS/PART1 CI                                
         LH    R0,CJTRKS                                                        
         BAS   R5,WKOUT2           TRKS/PART2 CI                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CICINDX                                                       
         BAS   R5,WKOUT2           NUM OF INDEX CIS                             
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         BAS   R5,WKOUT2           NUM OF PART1 CIS                             
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         BAS   R5,WKOUT2           NUM OF PART2 CIS                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CIENTRYS                                                      
         BAS   R5,WKOUT2           INDEX ENTRYS/REC                             
         LH    R0,CIPAGES                                                       
         BAS   R5,WKOUT2           INDEX TOTAL RECS                             
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+22                                                             
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
         BAS   R5,WKOUT2           INDEX PART1 RECS                             
         SR    R0,R0                                                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LH    R0,CIPAGES                                                       
         SH    R0,CJPAGE                                                        
         BAS   R5,WKOUT2           INDEX PART2 RECS                             
         B     WKOUTX                                                           
*                                                                               
WKOUT2   LA    R7,16(R7)           BUMP TO NEXT ALPHA                           
         MVC   P(16),0(R7)                                                      
         EDIT  (R0),(5,P+19),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         BR    R5                                                               
*                                                                               
WKOUTX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WKOUTA   DC    CL16'RECORD LENGTH'                                              
         DC    CL16'RECS PER TRACK'                                             
         DC    CL16'TRKS FOR INDEX'                                             
         DC    CL16'TRKS FOR PART1'                                             
         DC    CL16'TRKS FOR PART2'                                             
         DC    CL16'TRKS/INDEX CI'                                              
         DC    CL16'TRKS/PART1 CI'                                              
         DC    CL16'TRKS/PART2 CI'                                              
         DC    CL16'NUM OF INDEX CIS'                                           
         DC    CL16'NUM OF PART1 CIS'                                           
         DC    CL16'NUM OF PART2 CIS'                                           
         DC    CL16'INDEX ENTRYS/REC'                                           
         DC    CL16'INDEX TOTAL RECS'                                           
         DC    CL16'INDEX PART1 RECS'                                           
         DC    CL16'INDEX PART2 RECS'                                           
         EJECT                                                                  
SRTRECD  DSECT                     SORT RECORD                                  
SRTDATA  DS    0CL40                                                            
SRTUSER  DS    XL2                 01                                           
SRTFILID DS    CL4                 03                                           
SRTREPNO DS    XL2                 07                                           
SRTSEQ   DS    XL2                 09                                           
         DS    XL3                 11                                           
SRTSTAT  DS    XL1                 14                                           
SRTADDR  DS    XL2                 15                                           
SRTNEXT  DS    XL2                 17                                           
SRTDATEC DS    XL2                 19                                           
SRTTIMEC DS    XL2                 21                                           
SRTBAD   DS    XL8                 23                                           
SRTKEY   DS    CL10                31                                           
         SPACE 2                                                                
REPTLD   DSECT                     REPORT LINE                                  
*                                                                               
REPTL    DS    0CL165                                                           
RLFRSTC  DS    CL1                 FIRST COLUMN FOR BOX                         
RLUSER   DS    CL08                                                             
         DS    CL1                                                              
RLID     DS    CL13                                                             
         DS    CL1                                                              
RLSTAT   DS    CL02                                                             
         DS    CL1                                                              
RLRECS   DS    CL07                                                             
         DS    CL1                                                              
RLNCI    DS    CL05                                                             
         DS    CL1                                                              
RLCRTD   DS    CL10                                                             
         DS    CL1                                                              
RLRETD   DS    CL10                                                             
         DS    CL1                                                              
RLHOURS  DS    CL09                                                             
         DS    CL1                                                              
RLCPL    DS    CL06                                                             
         DS    CL1                                                              
RLPCT    DS    CL03                                                             
         DS    CL1                                                              
RLDESC   DS    CL16                                                             
         DS    CL1                                                              
RLSENT   DS    CL21                                                             
         DS    CL1                                                              
RLPSWD   DS    CL06                                                             
         DS    CL1                                                              
RLATTB   DS    CL15                                                             
         DS    CL1                                                              
RLMAKER  DS    CL05                                                             
         DS    CL1                                                              
RLOSTAT  DS    CL2                                                              
RLLASTC  DS    CL1                 LAST COLUMN FOR BOX                          
         SPACE 2                                                                
SMRYLD   DSECT                     SUMMARY REPORT LINE                          
*                                                                               
SMRYL    DS    0CL165                                                           
SLFRSTC  DS    CL1                 FIRST COLUMN FOR BOX                         
SLUSER   DS    CL06                                                             
         DS    CL1                                                              
SLRTOTL  DS    CL12                                                             
         DS    CL1                                                              
SLRECS   DS    CL12                                                             
         DS    CL1                                                              
SLNCI    DS    CL12                                                             
SLMIDL1  DS    CL1                                                              
         DS    CL1                                                              
SLRLIVE  DS    CL12                                                             
         DS    CL1                                                              
SLRDEAD  DS    CL12                                                             
         DS    CL1                                                              
SLRERROR DS    CL12                                                             
SLMIDL2  DS    CL1                                                              
         DS    CL1                                                              
SLRXPRD  DS    CL12                                                             
         DS    CL1                                                              
SLRSEMI  DS    CL12                                                             
         DS    CL1                                                              
SLRRETN  DS    CL12                                                             
SLLASTC  DS    CL1                 LAST COLUMN FOR BOX                          
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMWRKFD                                                                        
       ++INCLUDE DMWRKFD                                                        
         EJECT                                                                  
*FATABSDEQU                                                                     
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
*DMSPACED                                                                       
       ++INCLUDE DMSPACED                                                       
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*DMWRKRK                                                                        
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
*DMWRKFL                                                                        
       ++INCLUDE DMWRKFL                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
*DDBIGBOX                                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DMWRKFM   10/07/15'                                      
         END                                                                    
