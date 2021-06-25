*          DATA SET DMPRTQM    AT LEVEL 006 AS OF 11/18/20                      
*PHASE PRTQMA                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE TIMBER                                                                 
         TITLE 'PQMAINT - PRTQUE FILE MAINTENANCE'                              
         PRINT NOGEN                                                            
PQMAINT  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,PQMAINT,RA,R9,R8,WORK=A(PQWORK)                                
         ST    R1,ACOMRG           SAVE MVS SUPV INFO                           
         L     RC,=V(CPRINT)       RC IS PRINTER CONTROL REGISTER               
         USING DPRINT,RC                                                        
         LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(PGMAINI)      INITIALISE SOME STUFF                        
         BASR  RE,RF                                                            
         EJECT                                                                  
* READ A SET OF INPUT PARAMETER CARDS                                           
*                                                                               
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
GP1      LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
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
GP3      CLI   PRTQMAX,1           SET DEFAULT PQID IF ONLY ONE FILE            
         BNE   GP4                                                              
         MVI   PQID,1              SET DEFAULT INTERNAL PRTQ FILE NUM           
         MVI   PRTQINP,1           SET ONE PRTQ FILE INPUT                      
         OI    PRTQINP+1,X'01'     SET REFERENCED BY PQID=U PARM                
*                                                                               
GP4      CLI   FILE,0              TEST IF ANY FILE RENAMES VIA FILE=           
         BE    GP4X                                                             
         CLC   FILE(1),PRTQINP     NUM RENAMED >= NUM REFERENCED                
         BNL   GP4B                                                             
GP4A     LA    R1,FILE+4           INVALID FILE=                                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP4B     LA    R1,PRTQINP+1        POINT TO LIST OF INPUT FILES REF             
GP4C     TM    0(R1),X'0F'         TEST IF REFERENCED                           
         BZ    *+12                NO                                           
         TM    0(R1),X'10'         YES MUST BE RENAMED VIA FILE=                
         BZ    GP4A                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GP4C                                                             
GP4X     EQU   *                                                                
*                                                                               
GP5      CLI   OUTPUT,0            OUTPUT ONLY FOR MODE=COPY OR REPLACE         
         BE    GP5B                                                             
         CLI   MODE,4                                                           
         BE    GP5B                                                             
         CLI   MODE,5                                                           
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
GPM1     CLI   MODE,1              INIT - MUST DEFINE PRTQ ID                   
         BNE   GPM2                                                             
         CLI   PRTQINP,1           MUST BE SINGLE FILE ONLY                     
         BE    GETPARMX                                                         
         MVI   ERRNUM,5                                                         
         B     GPERR                                                            
*                                                                               
GPM2     CLI   MODE,2              PRNT - INPUT DEFAULTS TO DISK                
         BE    GPM2A                                                            
         CLI   MODE,6              ARCHIVE - INPUT DEFAULTS TO DISK             
         BNE   GPM3                                                             
         MVI   MODE,2              SET PRNT MODE WITH ARCHIVING                 
         MVI   ARCMODE,1                                                        
GPM2A    CLI   INPUT,0                                                          
         BNE   *+8                                                              
         MVI   INPUT,DISK                                                       
         CLI   USER,0              MUST HAVE USER=                              
         BNE   GPM2B                                                            
         MVI   USER,X'80'                                                       
         L     RF,=A(REPTAB)                                                    
         ST    RF,AREPALL                                                       
         LA    RF,L'REPTAB(RF)                                                  
         MVC   0(2,RF),FFS                                                      
*                                                                               
GPM2B    B     GETPARMX                                                         
*                                                                               
GPM3     CLI   MODE,3              REPT - REPORT ON PRTQ FILE                   
         BNE   GPM4                                                             
         CLI   PRTQINP,1           MUST BE SINGLE FILE ONLY                     
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
         MVI   REPLMODE,0                                                       
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
GPM4C    B     GETPARMX                                                         
*                                                                               
GPM5     CLI   MODE,5              REPLACE - INPUT=TAPE,OUTPUT=DISK             
         BNE   GPM6                                                             
         CLI   INPUT,0             INPUT DEFAULTS TO TAPE                       
         BNE   *+8                                                              
         MVI   INPUT,TAPE                                                       
         CLI   OUTPUT,0            OUTPUT DEFAULTS TO DISK                      
         BNE   *+8                                                              
         MVI   OUTPUT,DISK                                                      
GPM5A    TM    INPUT,TAPE          INPUT MUST BE TAPE                           
         BNZ   GPM5B                                                            
         LA    R1,INPUT+4          INVALID INPUT=                               
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPM5B    TM    OUTPUT,DISK         OUTPUT MUST BE DISK                          
         BNZ   GPM5C                                                            
         LA    R1,OUTPUT+4         INVALID OUTPUT=                              
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPM5C    MVI   MODE,4              SET MODE=COPY WITH REPLACE OPTION            
         MVI   REPLMODE,1                                                       
         B     GETPARMX                                                         
*                                                                               
GPM6     DC    H'0'                                                             
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
         L     RF,=A(ERRMSG5)      MUST SPECIFY A SINGLE PRTQ ONLY              
         MVC   WORK(60),0(RF)                                                   
         B     GPERRA                                                           
*                                                                               
GPERR6   CLI   ERRNUM,6                                                         
         BNE   GPERR6                                                           
         L     RF,=A(ERRMSG6)      MUST SPECIFY A SINGLE PRTQ ONLY              
         MVC   WORK(60),0(RF)                                                   
         B     GPERRA                                                           
*                                                                               
GPERR7   DC    H'0'                DIE IF UNKNOWN ERROR                         
*                                                                               
GPERRA   GOTO1 =V(PRINTER)         DISPLAY AND PRINT ERROR MESSAGE              
         BASR  RE,RF                                                            
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
         EJECT                                                                  
***********************************************************************         
* LOAD IN EXTERNAL ROUTINE AND PASS CONTROL FOR PRE-PROCESSING        *         
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
         L     R1,=A(PLXTRN)       PASS CONTROL TO EXTERNAL ROUTINE             
         MVI   0(R1),0             SET FIRST TIME SWITCH                        
         BASR  RE,RF                                                            
*                                                                               
GPXX     DS    0H                                                               
         EJECT                                                                  
* INITIALISE AND OPEN PRTQ FILES AND TAPES FOR THIS MODE                        
*                                                                               
OPEN     LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         L     RF,=A(OPNPQ)                                                     
         BASR  RE,RF               OPEN ALL PRTQ FILES REFERENCED               
*                                                                               
OPEN2    TM    INPUT,TAPE          OPEN INPUT TAPE IF SPECIFIED                 
         BZ    OPEN3                                                            
         BAS   RE,OPNIN                                                         
*                                                                               
OPEN3    TM    OUTPUT,TAPE         OPEN OUTPUT TAPE IF SPECIFIED                
         BZ    OPEN4                                                            
         BAS   RE,OPNOUT                                                        
*                                                                               
OPEN4    EQU   *                   GOTO1 =V(PRINTER)                            
         L     RF,=A(INFO3)        ACTION MESSAGES                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
         MVC   DUB1,PRTQUE         SET PRTQ FILE IN TITLE                       
         MVC   DUB2,DUB1                                                        
         CLI   PRTQINP,1           TEST ONLY ONE PRTQ FILE REFERENCED           
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
         BE    INIPQ                                                            
         CLI   MODE,2                                                           
         BE    PRTPQ                                                            
         CLI   MODE,3                                                           
         BE    RPTPQ                                                            
         CLI   MODE,4                                                           
         BE    CPYPQ                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRTQUE                                                   *         
***********************************************************************         
INIPQ    LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         L     RF,=A(INITPQ)                                                    
         BASR  RE,RF               GO TO INITIALISE ROUTINE                     
*                                                                               
INIPQ1   CLI   ERRNUM,1            TEST FOR GOOD INITIALISE                     
         BL    INIPQX              YES                                          
         BH    *+12                                                             
         L     RF,=A(ERRMSGA)      ERROR END OF FILE                            
         B     *+8                                                              
         L     RF,=A(ERRMSGB)      ERROR DISK WRITE                             
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+6(5),PRTQID                                                 
         BAS   RE,PUTMSGP                                                       
         DC    H'0'                DIE IF FAIL TO INITIALISE                    
*                                                                               
INIPQX   L     RF,=A(INFO4)        INITIALISED PRTQX                            
         MVC   12(5,RF),FILEIX                                                  
         CLC   FILEIX(7),FILEID    TEST IF PRTQ FILE WAS RENAMED                
         BE    *+14                                                             
         MVI   17(RF),C'='                                                      
         MVC   18(7,RF),FILEID                                                  
         BAS   RE,PINFO                                                         
         GOTO1 =V(PRINTER)                                                      
INIPQX1  LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(PQOUT)                                                     
         BASR  RE,RF               PRINT ATTRIBUTES OF NEW PRTQUE               
         B     GETPARM                                                          
                                                                                
***********************************************************************         
* COPY REPORTS FROM DISK/TAPE TO DISK/TAPE                            *         
* CALLS TO DMPRTQUE VIA DATAMGR SET X'20' ON IN DMCB1(1) TO SHOW THAT *         
* DMPRTQM IS THE CALLER. THIS SHOWS FILES ARE BEING COPIED/RESTORED   *         
* RATHER THAN BEING READ/ADDED BY NORMAL APPLICATIONS                 *         
***********************************************************************         
CPYPQ    XC    COPYCTRS,COPYCTRS   CLEAR COPY COUNTERS                          
         XC    INDEX,INDEX                                                      
         LA    RE,PRTQINP          SET NEXT LOCN IN PRTQ INPUT LIST             
         ST    RE,APRTQINP                                                      
         L     R2,AQH              R2=A(RECORD LENGTH HEADER)                   
         L     R3,AQ                                                            
         USING PQPLD,R3            R3=A(REPORT HEADER PRINT LINE)               
         L     R5,ACIREC                                                        
         USING PQRECD,R5           R5=A(CIREC FOR DISK HDR & DATA)              
*                                                                               
CPYPQ0   TM    INPUT,DISK          BUMP TO NEXT INPUT PRTQ FILE                 
         BZ    CPYPQ0A                                                          
         L     RE,APRTQINP                                                      
         LA    RE,1(RE)                                                         
         ST    RE,APRTQINP                                                      
         CLI   0(RE),X'FF'         TEST IF END OF INPUT LIST                    
         BE    CPYPQA                                                           
         TM    0(RE),X'0F'         TEST IF PRTQ FILE REFERENCED                 
         BZ    CPYPQ0                                                           
         LA    R0,PRTQINP                                                       
         SR    RE,R0               RE=INTERNAL PRTQ FILE NUM                    
         SLL   RE,3                                                             
         L     RF,APRTQLST         INDEX INTO PRTQ FILE LIST                    
         AR    RF,RE                                                            
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),1(RF)   SET PRTQ FILE ID FOR DATAMGR                 
         MVC   PRTQINT,0(RF)                                                    
         MVC   PRTQEXT,4(RF)                                                    
         MVC   PRTQDTF+1(3),5(RF)                                               
         LA    RF,FILEIX(RE)       SET ORIGINAL DTF NAME                        
         MVC   FILEIX(8),0(RF)                                                  
         LA    RF,FILEID(RE)       SET OVERRIDE DTF NAME                        
         MVC   FILEID(8),0(RF)                                                  
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR PRTQ FILE            
         ST    RF,COPYINDX                                                      
         BAS   RE,PQLOCK           ENQUEUE PRTQUE IF COPY FROM DISK             
         XC    INDEX,INDEX                                                      
*                                                                               
CPYPQ0A  BAS   RE,GETREPT          GET NEXT REPORT HEADER (USING CXREC)         
         BNZ   CPYPQ0B                                                          
         TM    INPUT,TAPE          END OF INPUT TAPE                            
         BO    CPYPQA                                                           
         BAS   RE,PQUNLK           DEQUEUE AT END OF INPUT PRTQ FILE            
         B     CPYPQ0                                                           
*                                                                               
CPYPQ0B  L     RF,TOTLREAD         BUMP TOTAL REPORTS READ                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTLREAD                                                      
         TM    INPUT,TAPE                                                       
         BO    CPYPQ0B1                                                         
         L     RE,COPYINDX         BUMP THIS PRTQ FILE REPORTS READ             
         L     RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(RE)                                                         
*                                                                               
CPYPQ0B1 TM    QLATTB,QLATERR      IGNORE ERROR REPORTS                         
         BO    CPYPQ0A                                                          
         CLC   QLAGERD(3),DATECPR  TEST EXPIRED REPORT                          
         BH    CPYPQ0C                                                          
         TM    EXPIRED,YES         YES TEST IF WANT EXPIRED REPORTS             
         BZ    CPYPQ0A                                                          
*                                                                               
CPYPQ0C  MVC   DUB(2),QLSRCID                                                   
         BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),QLCLASS                                                   
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),QLREPTY                                                   
         BAS   RE,TSTRTYPE         FILTER ON RTYPE                              
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),QLSTAT                                                    
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   CPYPQ0A                                                          
         MVC   DUB+0(1),QLTYPE                                                  
         MVC   DUB+1(1),QLTYP1                                                  
         BAS   RE,TSTTYPE          FILTER ON TYPE                               
         BNE   CPYPQ0A                                                          
         CLC   REPSUBID,=C'ALL'    FILTER ON SUBID                              
         BE    *+14                                                             
         CLC   REPSUBID,QLSUBID                                                 
         BNE   CPYPQ0A                                                          
         CLI   REPFLAG,0           TEST IF REPORT DATE/TIME/NUM INPUT           
         BE    CPYPQ0X                                                          
         CLI   REPFLAG,X'80'       TEST IF CREATE DATE ONLY INPUT               
         BNE   CPYPQ0D                                                          
         MVC   HALF1,QLAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE      CREATED DATE MUST MATCH INPUT DATE           
         BNE   CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0D  TM    REPFLAG,X'01'       TEST IF INDIVIDUAL REPORT                    
         BZ    CPYPQ0E                                                          
         CLC   QLREPNO,REPSEQL                                                  
         BNE   CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0E  TM    REPFLAG,X'04'       TEST IF TIME1-TIME2 INPUT                    
         BNO   CPYPQ0F                                                          
         MVC   HALF1,QLAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE      CREATED DATE MUST MATCH INPUT DATE           
         BNE   CPYPQ0A                                                          
         CLC   QLAGELT,REPSEQL     TIME MUST BE IN RANGE                        
         BL    CPYPQ0A                                                          
         CLC   QLAGELT,REPSEQH                                                  
         BH    CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0F  TM    REPFLAG,X'02'       TEST IF ONLY ONE TIME                        
         BZ    CPYPQ0G                                                          
         MVC   HALF1,QLAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE                                                   
         BH    CPYPQ0A             IGNORE IF CREATED ON LATER DATE              
         BL    CPYPQ0X             OK IF CREATED ON PREVIOUS DATE               
         CLC   QLAGELT,REPSEQH                                                  
         BH    CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0G  DC    H'0'                DIE IF UNKNOWN SITUATION                     
*                                                                               
CPYPQ0X  CLI   CRANGE,0            TEST IF CREATE DATE FILTER                   
         BNE   CPYPQ0X1            YES                                          
         CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BNE   CPYPQ0X2            YES                                          
         B     CPYPQ1              NO FILTERS: SKIP TESTING                     
CPYPQ0X1 MVC   HALF1,QLAGELD       LIVE DATE                                    
         BRAS  RE,TESTNCD          CONVERT TO NEW CMPRSD DATE                   
         MVC   DUB+0(2),HALF1                                                   
         CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BE    CPYPQ0X3            NO: DON'T NEED DEAD DATE                     
CPYPQ0X2 MVC   HALF1,QLAGEDD       DEAD DATE                                    
         BRAS  RE,TESTNCD          CONVERT TO NEW CMPRSD DATE                   
         MVC   DUB+2(2),HALF1                                                   
CPYPQ0X3 BAS   RE,TSTRANGE         FILTER ON CREATED/DEAD DATES                 
         BNE   CPYPQ0A                                                          
*                                                                               
CPYPQ1   TM    INPUT,TAPE          GET FULL EXTENDED REPORT HEADER              
         BZ    CPYPQ1A                                                          
         CLI   NEWUSER,0           TEST IF NEWUSER=XXX INPUT                    
         BE    *+10                                                             
         MVC   QLSRCID,REPNEWUN                                                 
         MVC   0(128,R5),QLINDEX   TAPE ALREADY HAS IT - MOVE TO BUFF           
         B     CPYPQ2                                                           
CPYPQ1A  MVC   SAVE(32),QLINDEX                                                 
         LR    RF,R5               COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,=H'14336'                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NXFLAG,X'C0'                                                     
         GOTO1 VDATAMGR,DMCB,(X'20',=C'SEQ'),PRTQID,INDEX,(R3),(R5)             
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
         CLC   5(3,R3),=C'ERR'     TEST IF ERROR ON 1ST CI READ                 
         BE    CPYPQ9                                                           
*                                                                               
CPYPQ2   MVC   SAVE(128),QLINDEX   SAVE REPORT DATA                             
         NOP   CPYPQ0A             FILTER ON NON INDEX DATA HERE                
         B     CPYPQ3                                                           
*                                                                               
CPYPQ3   TM    OUTPUT,TAPE         WRITE REPORT HEADER TO TAPE/DISK             
         BZ    *+12                                                             
         BAS   RE,WOTAPE                                                        
         B     CPYPQ3B                                                          
         OI    QLFLAG,X'40'        SET STAT FROM RECORD                         
         OI    QLFLAG,X'20'        SET DATE FROM RECORD                         
         CLI   USER,X'80'                                                       
         BNE   *+8                 SET RETN FROM RECORD IF USER=ALL             
         OI    QLFLAG,X'10'                                                     
         CLI   REPLMODE,0                                                       
         BE    CPYPQ3A                                                          
         OI    QLFLAG,QLFLKEY                                                   
CPYPQ3A  GOTO1 VDATAMGR,DMCB1,(X'20',=C'OPEN'),PRTQUE,INDEX,(R3),(R5)           
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
         SR    RE,RE                                                            
         IC    RE,QLREPINT         GET PRTQ FILE INTERNAL NUM                   
         SLL   RE,3                                                             
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR PRTQ FILE            
         ST    RF,COPYINDX                                                      
         B     CPYPQ4                                                           
CPYPQ3B  TM    INPUT,DISK          TEST IF HIDDEN <ARC=CARD...>                 
         BZ    CPYPQ4                                                           
         LA    RF,PQDATA1-PQRECD(R5)                                            
         LARL  R1,ARCLINE                                                       
         CLC   0(8,RF),0(R1)       IS FIRST LINE <ARC=AG,.....>                 
         BNE   CPYPQ4              NO                                           
         SR    R1,R1               BUILD A TAPE RECORD                          
         ICM   R1,3,0(RF)                                                       
         LA    R1,2(R1)                                                         
         STH   R1,0(R2)                                                         
         MVC   0(ARCLINEL-2,R3),2(RF)                                           
         BAS   RE,WOTAPE                                                        
*                                                                               
CPYPQ4   TM    INPUT,TAPE          GET NEXT INPUT PRINT LINE                    
         BZ    CPYPQ4B                                                          
         BAS   RE,RITAPE                                                        
         OC    0(4,R2),0(R2)       TEST EOF                                     
         BNZ   CPYPQ5                                                           
CPYPQ4A  MVC   0(2,R2),LENEOF      SEQ ERR ON INPUT                             
         MVC   0(10,R3),EOFLAB                                                  
         MVC   5(3,R3),=C'ERR'                                                  
         MVC   10(10,R3),0(R3)                                                  
         B     CPYPQ5                                                           
CPYPQ4B  GOTO1 VDATAMGR,DMCB,(X'20',=C'SEQ'),PRTQID,INDEX,(R3),(R5)             
         CLI   8(R1),0                                                          
         BNE   CPYPQ4A                                                          
         CLC   0(5,R3),SOFLAB                                                   
         BE    CPYPQ4A                                                          
*                                                                               
CPYPQ5   SR    R7,R7               SET R7 NONZERO IF EOF LABEL                  
         CLC   0(5,R3),EOFLAB                                                   
         BNE   CPYPQ6                                                           
         LA    R7,=C'CLOSE  '      SET NORMAL EOF                               
         CLC   5(3,R3),2(R3)                                                    
         BE    *+8                                                              
         LA    R7,=C'CLO/ERR'      SET ERROR EOF                                
*                                                                               
CPYPQ6   TM    OUTPUT,TAPE         PUT NEXT OUTPUT PRINT LINE                   
         BZ    CPYPQ6A                                                          
         BAS   RE,WOTAPE                                                        
         LTR   R7,R7                                                            
         BNZ   CPYPQ8                                                           
         B     CPYPQ4                                                           
CPYPQ6A  LTR   R7,R7                                                            
         BNZ   CPYPQ6B                                                          
         GOTO1 VDATAMGR,DMCB1,(X'20',=C'ADD'),PRTQUE,INDEX,(R3),(R5)            
         CLI   8(R1),0                                                          
         BE    CPYPQ4                                                           
         B     CPYPQ9                                                           
CPYPQ6B  GOTO1 VDATAMGR,DMCB1,(X'20',(R7)),PRTQUE,INDEX,(R3),(R5)               
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
*                                                                               
CPYPQ8   LA    RF,=C'CLOSE  '      END OF FILE RECORD                           
         CR    R7,RF                                                            
         BNE   CPYPQ9                                                           
         L     RF,TOTLCOPY         BUMP TOTAL OF REPORTS COPIED                 
         LA    RF,1(RF)                                                         
         ST    RF,TOTLCOPY                                                      
         L     RE,COPYINDX         BUMP THIS PRTQ FILE REPORTS COPIED           
         L     RF,4(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,4(RE)                                                         
         B     CPYPQ0A                                                          
*                                                                               
CPYPQ9   L     RF,=A(ERRMSGE)      ERROR IN COPY INDEX                          
         MVC   P(32),0(RF)                                                      
         MVC   P+6(5),PRTQID                                                    
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
         BZ    CPYPQ0A                                                          
         CLC   0(10,R3),10(R3)     UNLESS EOF ON TAPE CAUSED SEQ ERROR          
         BNE   CPYPQ0A                                                          
*                                                                               
CPYPQA   OC    TOTLCOPY,TOTLCOPY   TEST COPY COUNTER AT EOF                     
         BNZ   CPYPQB                                                           
         L     RF,=A(INFO5)        REPORT NOT FOUND                             
         BAS   RE,PINFO                                                         
         B     CPYPQX                                                           
*                                                                               
CPYPQB   L     RF,=A(INFO6)        TOTAL NNNNN REPORTS COPIED FROM ..           
         TM    INPUT,TAPE                                                       
         BO    *+8                                                              
         L     RF,=A(INFO7)                                                     
         MVC   WORK(45),0(RF)                                                   
         L     RF,=A(INFO8)        OUT OF NNNNN REPORTS READ                    
         MVC   WORK+45(35),0(RF)                                                
         MVC   P(80),WORK                                                       
         L     R0,TOTLCOPY         TOTAL REPORTS COPIED COUNT                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+06(5),DUB                                                      
         L     R0,TOTLREAD         TOTAL REPORTS READ COUNT                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+52(5),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CPYPQC   TM    INPUT,TAPE          REPEAT COUNTS FOR EACH PRTQ FILE             
         BZ    *+10                                                             
         MVC   WORK+45(35),SPACES                                               
         LA    R2,FILEIX+8         R2=A(NEXT PRTQ FILE ID)                      
         LA    R3,COPYCTRS+8       R3=A(NEXT READ/COPY COUNTER PAIR)            
         SR    R1,R1                                                            
         IC    R1,PRTQMAX          R1=MAX NUM OF PRTQ FILES                     
*                                                                               
CPYPQD   OC    0(8,R3),0(R3)       TEST READ/COPY COUNTERS                      
         BZ    CPYPQD2                                                          
         MVC   P(80),WORK                                                       
         MVC   P(5),0(R2)          SET PRTQ FILE ID                             
         L     R0,4(R3)            GET PRTQ COPY COUNT                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+06(5),DUB                                                      
         TM    INPUT,TAPE                                                       
         BO    CPYPQD1                                                          
         L     R0,0(R3)            GET PRTQ READ COUNT                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+52(5),DUB                                                      
CPYPQD1  GOTO1 =V(PRINTER)                                                      
CPYPQD2  LA    R2,8(R2)            BUMP TO NEXT PRTQ FILE ID                    
         LA    R3,8(R3)            BUMP TO NEXT COUNTERS                        
         BCT   R1,CPYPQD                                                        
*                                                                               
CPYPQX   TM    OUTPUT,TAPE         CLOSE OUTPUT TAPE FILE                       
         BZ    *+8                                                              
         BAS   RE,CLSOUT                                                        
         B     GETPARM                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A REPORT IN PRINT QUEUE                                       *         
***********************************************************************         
PRTPQ    XC    COPYCTRS,COPYCTRS   CLEAR PRINT COUNTERS                         
         XC    INDEX,INDEX                                                      
         LA    RE,PRTQINP          SET NEXT LOCN IN PRTQ INPUT LIST             
         ST    RE,APRTQINP                                                      
         L     R2,AQH              R2=A(RECORD LENGTH AREA)                     
         L     R3,AQ                                                            
         USING PQPLD,R3            R3=A(REPORT HEADER PRINT LINE)               
         L     R5,ACIREC                                                        
         USING PQRECD,R5           R5=A(CIREC FOR DISK HDR & DATA)              
*                                                                               
PRTPQ0   TM    INPUT,DISK          BUMP TO NEXT INPUT PRTQ FILE                 
         BZ    PRTPQ0A                                                          
         L     RE,APRTQINP                                                      
         LA    RE,1(RE)                                                         
         ST    RE,APRTQINP                                                      
         CLI   0(RE),X'FF'         TEST IF END OF INPUT LIST                    
         BE    PRTPQQ                                                           
         TM    0(RE),X'0F'         TEST IF PRTQ FILE REFERENCED                 
         BZ    PRTPQ0                                                           
         LA    R0,PRTQINP                                                       
         SR    RE,R0               RE=INTERNAL PRTQ FILE NUM                    
         SLL   RE,3                                                             
         L     RF,APRTQLST         INDEX INTO PRTQ FILE LIST                    
         AR    RF,RE                                                            
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),1(RF)   SET PRTQ FILE ID FOR DATAMGR                 
         MVC   PRTQINT,0(RF)                                                    
         MVC   PRTQEXT,4(RF)                                                    
         MVC   PRTQDTF+1(3),5(RF)                                               
         LA    RF,FILEIX(RE)       SET ORIGINAL DTF NAME                        
         MVC   FILEIX(8),0(RF)                                                  
         LA    RF,FILEID(RE)       SET OVERRIDE DTF NAME                        
         MVC   FILEID(8),0(RF)                                                  
         XC    INDEX,INDEX                                                      
*                                                                               
PRTPQ0A  BAS   RE,GETREPT          GET NEXT REPORT HEADER                       
         BNZ   PRTPQ0B             NO MORE REPORTS                              
         TM    INPUT,TAPE                                                       
         BO    PRTPQQ                                                           
         B     PRTPQ0                                                           
PRTPQ0B  L     RF,TOTLREAD                                                      
         LA    RF,1(RF)            BUMP REPORTS READ                            
         ST    RF,TOTLREAD                                                      
*                                                                               
         MVC   DUB(2),QLSRCID                                                   
         BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   PRTPQ0A                                                          
         MVC   DUB(1),QLCLASS                                                   
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   PRTPQ0A                                                          
         MVC   DUB(1),QLREPTY                                                   
         BAS   RE,TSTRTYPE         FILTER ON RTYPE                              
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),QLSTAT                                                    
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   PRTPQ0A                                                          
         MVC   DUB+0(1),QLTYPE                                                  
         MVC   DUB+1(1),QLTYP1                                                  
         BAS   RE,TSTTYPE          FILTER ON TYPE                               
         BNE   PRTPQ0A                                                          
         CLI   ARCMODE,0           IGNORE DOWN/SQL IF ARCHIVING                 
         BE    *+12                                                             
         TM    QLTYPE,PQTYDL+PQTYSQL                                            
         BNZ   PRTPQ0A                                                          
         CLC   REPSUBID,=C'ALL'    FILTER ON SUB ID                             
         BE    *+14                                                             
         CLC   REPSUBID,QLSUBID                                                 
         BNE   PRTPQ0A                                                          
         CLI   REPFLAG,0           TEST IF REPORT DATE/TIME/NUM INPUT           
         BE    PRTPQ0X                                                          
         CLI   REPFLAG,X'80'       TEST IF CREATE DATE ONLY INPUT               
         BNE   PRTPQ0D                                                          
         MVC   HALF1,QLAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE      CREATED DATE MUST MATCH INPUT DATE           
         BNE   PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0D  TM    REPFLAG,X'01'       TEST IF INDIVIDUAL REPORT                    
         BZ    PRTPQ0E                                                          
         CLC   QLREPNO,REPSEQL                                                  
         BNE   PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0E  TM    REPFLAG,X'04'       TEST IF TIME1-TIME2 INPUT                    
         BNO   PRTPQ0F                                                          
         MVC   HALF1,QLAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE      CREATED DATE MUST MATCH INPUT DATE           
         BNE   PRTPQ0A                                                          
         CLC   QLAGELT,REPSEQL     TIME MUST BE IN RANGE                        
         BL    PRTPQ0A                                                          
         CLC   QLAGELT,REPSEQH                                                  
         BH    PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0F  TM    REPFLAG,X'02'       TEST IF ONLY ONE TIME                        
         BZ    PRTPQ0G                                                          
         MVC   HALF1,QLAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE                                                   
         BH    PRTPQ0A             IGNORE IF CREATED ON LATER DATE              
         BL    PRTPQ0X             OK IF CREATED ON PREVIOUS DATE               
         CLC   QLAGELT,REPSEQH                                                  
         BH    PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0G  DC    H'0'                DIE IF UNKNOWN SITUATION                     
*                                                                               
PRTPQ0X  CLI   CRANGE,0            TEST IF CREATE DATE FILTER                   
         BNE   PRTPQ0X1            YES                                          
         CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BNE   PRTPQ0X2            YES                                          
         B     PRTPQ1              NO FILTERS: SKIP TESTING                     
PRTPQ0X1 MVC   HALF1,QLAGELD       LIVE DATE                                    
         BRAS  RE,TESTNCD          CONVERT TO NEW CMPRSD DATE                   
         MVC   DUB+0(2),HALF1                                                   
         CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BE    PRTPQ0X3            NO: DON'T NEED DEAD DATE                     
PRTPQ0X2 MVC   HALF1,QLAGEDD       DEAD DATE                                    
         BRAS  RE,TESTNCD          CONVERT TO NEW CMPRSD DATE                   
         MVC   DUB+2(2),HALF1                                                   
PRTPQ0X3 BAS   RE,TSTRANGE         FILTER ON CREATED/DEAD DATES                 
         BNE   PRTPQ0A                                                          
*                                                                               
PRTPQ1   TM    INPUT,TAPE          GET FULL EXTENDED REPORT HEADER              
         BZ    PRTPQ1A                                                          
         XC    0(256,R5),0(R5)     CLEAR START OF DISK BUFFER                   
         MVC   PQINDEX(20),QLINDEX MAKE BUFFER LOOK LIKE DISK RECORD            
         MVC   PQAGELT(4),QLAGELT                                               
         MVC   PQBATTR,QLBATTR                                                  
         MVC   PQFATTR(L'QLFATTR),QLFATTR                                       
         B     PRTPQ2                                                           
PRTPQ1A  MVC   SAVE(32),QLINDEX    SAVE INDEX ENTRY                             
         LR    RF,R5               COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,=H'14336'                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NXFLAG,X'C0'                                                     
         GOTO1 VDATAMGR,DMCB,(X'20',=C'SEQ'),PRTQID,INDEX,(R3),(R5)             
         CLI   8(R1),0                                                          
         BE    *+6                 NOW HAVE IT FOR DISK                         
         DC    H'0'                                                             
*                                                                               
PRTPQ2   MVC   SAVE(128),QLINDEX   SAVE REPORT DATA                             
         NOP   PRTPQ0A             FILTER ON NON INDEX DATA HERE                
PRTPQ2A  OC    TOTLCOPY,TOTLCOPY   SET ACTION MSG IF FIRST TIME                 
         BNZ   PRTPQ3                                                           
         L     RF,=A(INFO9)        REPORT DESCRIPTION AND DATA ....             
         BAS   RE,PINFO                                                         
*                                                                               
PRTPQ3   CLI   ARCMODE,0           ARCHIVING REPORT                             
         BNE   PRTPQ4                                                           
         TM    INFO,YES            TEST IF INFO SUPPRESSED                      
         BZ    PRTPQ4                                                           
         ZAP   LINE,=P'99'         PRINT REPORT ATTRIBUTES                      
         MVC   REPSEQL,PQREPNO                                                  
         MVC   SAVE(10),REPUSER                                                 
         L     R7,=A(REPOUTA)      POINT TO LIST OF ALPHA NAMES                 
         MVC   P,SPACES                                                         
*                                                                               
PRTPQ3A  MVC   P(16),0(R7)         REPORT ID                                    
         MVC   REPUSER(7),PQKEY                                                 
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT CLASS                                 
         MVC   P+19(1),PQCLASS                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT STATUS                                
         MVC   CISTAT,PQSTAT                                                    
         BRAS  RE,STATOUT                                                       
         MVC   P+19(3),REPSTATA                                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT NAME                                  
         MVC   P+19(11),PQDESC                                                  
         OC    P+19(11),SPACES                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT MAKER                                 
         MVC   P+19(5),PQMAKER                                                  
         OC    P+19(5),SPACES                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTPQ3B  GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         LOCN CREATED                                 
         XC    REPSUBID(5),REPSUBID                                             
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DATE CREATED                                 
         MVC   HALF1,PQDATEL                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(5,P+19)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         TIME CREATED                                 
         MVC   DUB(2),PQTIMEL                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         LIVE RETAIN HOURS                            
         EDIT  (B2,PQRETNL),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DEAD RETAIN HOURS                            
         EDIT  (B2,PQRETND),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DATE RETAINED                                
         MVC   HALF1,PQAGERD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(5,P+19)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         TIME RETAINED                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,PQAGERT                                                       
         MH    R1,=H'10'                                                        
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTPQ3C  GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF PAGES                                 
         MVC   P(16),0(R7)                                                      
         EDIT  (B2,PQPAGES),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           LINES PER PAGE                               
         MVC   P(16),0(R7)                                                      
         EDIT  (B1,PQLPP),(2,P+19),ALIGN=LEFT                                   
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF LINES                                 
         MVC   P(16),0(R7)                                                      
         EDIT  (B3,PQLINES),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           CHRS PER LINE                                
         MVC   P(16),0(R7)                                                      
         EDIT  (B2,PQAVCPL),(3,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF CIS = REGULAR+EXTENSIONS              
         MVC   P(16),0(R7)                                                      
         SR    R0,R0                                                            
         IC    R0,PQNCI                                                         
         SR    R1,R1                                                            
         IC    R1,PQNCIX                                                        
         AR    R0,R1                                                            
         EDIT  (R0),(3,P+19),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTPQ3D  GOTO1 =V(PRINTER)                                                      
         OC    PQPRLOC,PQPRLOC     TEST IF REPORT EVER PRTD/SENT                
         BZ    PRTPQ3F                                                          
         LA    R7,16(R7)           LOCN PRINTED                                 
         MVC   P(16),0(R7)                                                      
         CLC   PQPRLOC,FFS                                                      
         BNE   *+14                                                             
         MVC   REPIDA,DOTS                                                      
         B     *+20                                                             
         MVC   REPUSER,PQPRLOC                                                  
         MVC   REPSEQL+1(1),PQPRNUM                                             
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           COUNT PRINTED                                
         MVC   P(16),0(R7)                                                      
         EDIT  (B1,PQPRCNT),(3,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DEVICE PRINTED                               
         MVC   P(16),0(R7)                                                      
         MVC   P+19(8),PQPRSYM                                                  
PRTPQ3E  GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DATE PRINTED                                 
         MVC   P(16),0(R7)                                                      
         MVC   HALF1,PQDATED                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(5,P+19)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           TIME PRINTED                                 
         MVC   P(16),0(R7)                                                      
         MVC   DUB(2),PQTIMED                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
PRTPQ3F  EQU   *                                                                
*                                                                               
PRTPQ4   CLI   ARCMODE,0           ARCHIVING REPORT                             
         BE    PRTPQ5                                                           
         MVC   USERN,PQSRCID       GET USERID DETAILS                           
         BAS   RE,GETUSER                                                       
         LA    RE,PQDATA1-PQRECD(R5)                                            
         LARL  R1,ARCLINE                                                       
         CLC   0(8,RE),0(R1)       IS FIRST LINE <ARC=AG,.....>                 
         BNE   PRTPQ4A                                                          
         MVC   WORK(L'ARCCARD),4(RE)                                            
         CLC   WORK+11(4),=C'ONLI' TEST ONLINE/OFFLINE DEFAULTS                 
         BE    *+14                                                             
         CLC   WORK+11(4),=C'OFFL'                                              
         BNE   PRTPQ4C                                                          
         MVC   WORK+04(2),USERAGY  SET AGENCY FROM USER ID                      
         MVC   WORK+11(4),USERPOW  SET POWER CODE FROM USER ID                  
         BNE   PRTPQ4C             EMBEDDED ARC=CARD SAVED IN WORK              
*                                                                               
PRTPQ4A  LARL  R1,ARCCARD          USE MODEL ARC=CARD                           
         MVC   WORK(L'ARCCARD),0(R1)                                            
         MVC   WORK+04(2),USERAGY  AGENCY                                       
         MVC   WORK+07(3),PQMAKER  SYSTEM/PROGRAM                               
         MVC   WORK+11(4),USERPOW  POWER CODE                                   
         SR    R0,R0               DEST AND ORIGIN USER ID NUMS                 
         ICM   R0,3,PQSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+18(5),DUB                                                   
         UNPK  WORK+24(5),DUB                                                   
*                                                                               
PRTPQ4C  MVC   0(L'SPACES,R3),SPACES                                            
         MVI   0(R3),X'89'                                                      
         MVI   FLAG,0                                                           
         BAS   RE,PLINEQ                                                        
         MVI   0(R3),X'09'                                                      
         MVC   1(13,R3),=C'CONTROL CARDS'                                       
         MVI   FLAG,0                                                           
         BAS   RE,PLINEQ                                                        
*                                                                               
PRTPQ4D  MVC   0(L'SPACES,R3),SPACES                                            
         MVI   0(R3),X'09'                                                      
         MVC   1(13,R3),=C'-------------'                                       
         MVI   FLAG,0                                                           
         BAS   RE,PLINEQ                                                        
*                                                                               
PRTPQ4E  MVC   0(L'SPACES,R3),SPACES                                            
         MVI   0(R3),X'09'                                                      
         MVC   1(05,R3),=C'DATE='                                               
         MVC   HALF1,PQAGELD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(10,6(R3))                            
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVI   FLAG,0                                                           
         BAS   RE,PLINEQ           PRINT DATE=CARD                              
*                                                                               
PRTPQ4F  MVC   0(L'SPACES,R3),SPACES                                            
         MVI   0(R3),X'09'                                                      
         MVC   01(5,R3),=C'LOGO='                                               
         MVC   06(7,R3),USERA                                                   
         MVC   13(4,R3),WORK+11                                                 
         MVC   20(4,R3),USERPOW                                                 
         MVC   24(3,R3),PQMAKER                                                 
         MVI   FLAG,0                                                           
         BAS   RE,PLINEQ           PRINT LOGO=CARD                              
*                                                                               
PRTPQ4G  MVC   0(L'SPACES,R3),SPACES                                            
         MVI   0(R3),X'09'                                                      
         MVC   1(L'ARCCARD,R3),WORK                                             
         MVI   FLAG,0                                                           
         BAS   RE,PLINEQ           PRINT ARC=CARD                               
*                                                                               
PRTPQ5   MVC   REPUSER(10),SAVE                                                 
         GOTO1 ,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R5)                             
         OC    REPPAGES,REPPAGES   SET NUM OF PAGES TO PRINT                    
         BNZ   *+10                                                             
         MVC   REPPAGES,=X'7FFF'                                                
         XC    CPAGE,CPAGE                                                      
*                                                                               
PRTPQ6   TM    INPUT,TAPE          GET NEXT PRINT LINE                          
         BZ    PRTPQ6A                                                          
         BAS   RE,RITAPE                                                        
         OC    0(4,R2),0(R2)       TEST END OF FILE ON TAPE                     
         BNZ   PRTPQ6B                                                          
         DC    H'0'                SEQ ERR ON INPUT TAPE                        
PRTPQ6A  GOTO1 VDATAMGR,DMCB,(X'20',=C'SEQ'),PRTQID,INDEX,(R3),(R5)             
         CLI   8(R1),0                                                          
         BNE   PRTPQA                                                           
PRTPQ6B  CLC   0(5,R3),EOFLAB      TEST FOR EOF LABEL                           
         BNE   PRTPQ7                                                           
         TM    INPUT,DISK                                                       
         BO    PRTPQB                                                           
         B     PRTPQP                                                           
*                                                                               
PRTPQ7   MVI   FLAG,0              SET NORMAL PRINT FLAG                        
         TM    PQLINET,PQLTCC      TEST IF REPORT HAS CC CHR                    
         BZ    PRTPQ8              NO                                           
         LA    RE,PLCC             YES POINT RE TO CC CHR                       
         TM    PQLINET,PQLTFL                                                   
         BO    *+8                                                              
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'89'         TEST FOR NEW PAGE CC                         
         BL    PRTPQ8                                                           
         LH    R1,CPAGE            BUMP PAGE NUMBER COUNTER                     
         LA    R1,1(R1)                                                         
         STH   R1,CPAGE                                                         
         CH    R1,REPPAGES                                                      
         BNH   PRTPQ8                                                           
         MVI   FLAG,1              SET LAST PRINT FLAG                          
*                                                                               
PRTPQ8   BAS   RE,PLINEQ           PRINT LINE DATA                              
         CLI   FLAG,0                                                           
         BE    PRTPQ6                                                           
         B     PRTPQP                                                           
*                                                                               
PRTPQA   TM    8(R1),X'80'         TEST END OF FILE                             
         BO    PRTPQB                                                           
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
         B     PRTPQ8                                                           
*                                                                               
PRTPQB   CLI   ARCMODE,0           SET REPORT ARCHIVED                          
         BE    PRTPQP                                                           
         GOTO1 VDATAMGR,DMCB,(X'20',=C'ADSET'),PRTQID,INDEX,(R3),(R5)           
         CLI   8(R1),0                                                          
         BE    PRTPQP                                                           
         DC    H'0'                                                             
*                                                                               
PRTPQP   L     R1,TOTLCOPY         BUMP NUMBER OF REPORTS PRINTED               
         LA    R1,1(R1)                                                         
         ST    R1,TOTLCOPY                                                      
         B     PRTPQ0A                                                          
*                                                                               
PRTPQQ   OC    TOTLCOPY,TOTLCOPY   TEST REPORTS PRINTED AT EOF                  
         BNZ   PRTPQX                                                           
         L     RF,=A(INFO5)        REPORT NOT FOUND                             
         BAS   RE,PINFO                                                         
         B     GETPARM                                                          
*                                                                               
PRTPQX   ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT ON PRINT QUEUE STATUS FOR ALL OR A SPECIFIC USER             *         
***********************************************************************         
RPTPQ    CLI   KDAYS,0             DEFAULT KEEP DAYS                            
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
         GOTO1 =V(ADDAY),(R1),DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),(R1),(0,DUB1),(30,REPKDATE)                           
         L     R5,REPLDAYS                                                      
         GOTO1 =V(ADDAY),(R1),DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),(R1),(0,DUB1),(30,REPLDATE)                           
         L     R5,REPDDAYS                                                      
         GOTO1 =V(ADDAY),(R1),DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),(R1),(0,DUB1),(30,REPDDATE)                           
*                                                                               
         CLI   SORT,1              ALPHABETIC (REPORT SUBID)                    
         BNE   *+12                                                             
         L     R5,=A(SORTC1A)                                                   
         B     RPTPQ1                                                           
         CLI   SORT,2              NUMERIC (REPORT SEQ NUM)                     
         BNE   *+12                                                             
         L     R5,=A(SORTC1N)                                                   
         B     RPTPQ1                                                           
         CLI   SORT,3              TIME (DATE/TIME CREATED)                     
         BNE   *+12                                                             
         L     R5,=A(SORTC1T)                                                   
         B     RPTPQ1                                                           
         DC    H'0'                                                             
RPTPQ1   GOTO1 =V(SORTER),DMCB,(R5),SORTC2,0                                    
         B     RPTPQ2                                                           
*                                                                               
SORTC2   DC    CL24'RECORD TYPE=F,LENGTH=40 '                                   
*                                                                               
RPTPQ2   L     R5,ACIREC           R5=A(CI REC)                                 
         USING PQRECD,R5                                                        
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
         BNE   RPTPQ4                                                           
         CLI   WRITE,YES                                                        
         BNE   RPTPQ4                                                           
         BAS   RE,PQLOCK           LOCK PRINT QUEUE                             
*                                                                               
RPTPQ4   XC    SRTREC,SRTREC       INIT SORT REC AND READ CI REC                
         MVC   SRTADDR,CIADDR                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BE    RPTPQ6                                                           
         MVC   SRTUSER,=X'FFFF'    ERRORS HAVE HIGH KEY                         
         MVI   SRTSUBID,1                                                       
         B     RPTPQA              ALL DISK ERRORS ARE LISTED                   
*                                                                               
RPTPQ6   CLI   PQSTAT,PQSTPU       PURGED HAVE HIGH KEY                         
         BNE   *+14                                                             
         MVC   SRTUSER,=X'FFFF'                                                 
         B     RPTPQ8                                                           
         TM    PQSTAT,PQSTTE       TEMP CI'S HAVE HIGH KEY                      
         BZ    *+18                                                             
         MVC   SRTUSER,=X'FFFF'                                                 
         MVI   SRTSUBID,3                                                       
         B     RPTPQ8                                                           
*                                                                               
RPTPQ6A  OC    PQSRCID,PQSRCID     ALL CI'S MUST HAVE VALID KEY                 
         BZ    RPTPQ6P                                                          
         CLC   PQSRCID,=X'FFFF'                                                 
         BE    RPTPQ6P                                                          
         OC    PQSUBID,PQSUBID                                                  
         BZ    RPTPQ6P                                                          
         CLC   PQSUBID,SPACES                                                   
         BE    RPTPQ6P                                                          
         OC    PQREPNO,PQREPNO                                                  
         BZ    RPTPQ6P                                                          
         CLC   PQREPNO,MAXSEQ+2                                                 
         BH    RPTPQ6P                                                          
         MVC   HALF1,PQAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         LA    R1,HALF1                                                         
         BRAS  RE,DATECHK          MUST HAVE VALID INDEX CREATION DATE          
         BNE   RPTPQ6P                                                          
         MVC   HALF1,PQAGERD                                                    
         BRAS  RE,TESTNCD                                                       
         LA    R1,HALF1                                                         
         BRAS  RE,DATECHK          MUST HAVE VALID INDEX RETAIN DATE            
         BH    RPTPQ6P                                                          
         OC    PQAGEDD,PQAGEDD                                                  
         BZ    RPTPQ6B                                                          
         MVC   HALF1,PQAGEDD                                                    
         BRAS  RE,TESTNCD                                                       
         LA    R1,HALF1                                                         
         BRAS  RE,DATECHK          MUST HAVE VALID INDEX PRINTED DATE           
         BNE   RPTPQ6P                                                          
RPTPQ6B  CLI   PQSEQ,1             TEST IF HIGH ORDER CI                        
         BH    RPTPQ7              YES                                          
*                                                                               
PRTPQ6C  TM    PQATTB,PQATERR      DONT CHECK THIS FOR ERR REPORTS              
         BO    RPTPQ7                                                           
         OC    PQLINES,PQLINES     ALL LOW CI'S MUST HAVE VALID DATA            
         BZ    RPTPQ6P                                                          
         OC    PQPAGES,PQPAGES                                                  
         BNZ   *+12                                                             
         TM    PQLINET,PQLTCC                                                   
         BO    RPTPQ6P                                                          
         OC    PQAVCPL,PQAVCPL                                                  
         BZ    RPTPQ6P                                                          
         CLI   PQLPP,0                                                          
         BE    RPTPQ6P                                                          
         MVC   HALF1,PQDATEL                                                    
         BRAS  RE,TESTNCD                                                       
         LA    R1,HALF1                                                         
         BRAS  RE,DATECHK                                                       
         BNE   RPTPQ6P                                                          
         LA    R1,PQTIMEL                                                       
         BRAS  RE,TIMECHK                                                       
         BH    RPTPQ6P                                                          
         MVC   HALF1,PQDATED                                                    
         BRAS  RE,TESTNCD                                                       
         LA    R1,HALF1                                                         
         BRAS  RE,DATECHK                                                       
         BH    RPTPQ6P                                                          
         LA    R1,PQTIMED                                                       
         BRAS  RE,TIMECHK                                                       
         BH    RPTPQ6P                                                          
         B     RPTPQ7                                                           
RPTPQ6P  MVC   SRTUSER,=X'FFFF'    INVALID CI'S HAVE HIGH KEY                   
         MVI   SRTSUBID,2                                                       
         B     RPTPQ8                                                           
*                                                                               
RPTPQ7   MVC   DUB(2),PQSRCID      FILTER ON USER ID FOR GOOD CI'S              
         BAS   RE,TSTUSR                                                        
         BNE   RPTPQB                                                           
         MVC   DUB(1),PQCLASS      FILTER ON CLASS                              
         BAS   RE,TSTCLASS                                                      
         BNE   RPTPQB                                                           
         MVC   DUB(1),PQREPTY      FILTER ON RTYPE                              
         BAS   RE,TSTRTYPE                                                      
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),PQSTAT       FILTER ON STATUS                             
         BAS   RE,TSTSTAT                                                       
         BNE   RPTPQB                                                           
         MVC   DUB+0(1),PQTYPE     FILTER ON TYPE                               
         MVC   DUB+1(1),PQTYP1                                                  
         BAS   RE,TSTTYPE                                                       
         BNE   RPTPQB                                                           
         CLI   REPFLAG,X'80'       FILTER ON CREATE DATE                        
         BNE   RPTPQ7A                                                          
         MVC   HALF1,PQAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPCDATE      CREATED DATE MUST MATCH CDATE=VALUE          
         BNE   RPTPQB                                                           
*                                                                               
RPTPQ7A  CLI   CRANGE,0            TEST IF CREATE DATE FILTER                   
         BNE   RPTPQ7A1            YES                                          
         CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BNE   RPTPQ7A2            YES                                          
         B     RPTPQ7AX            NO FILTERS: SKIP TESTING                     
RPTPQ7A1 MVC   HALF1,PQAGELD       TAKE LIVE DATE                               
         BRAS  RE,TESTNCD          CONVERT TO NEW CMPRSD DATE                   
         MVC   DUB+0(2),HALF1                                                   
         CLI   DRANGE,0            TEST IF DEAD DATE FILTER                     
         BE    RPTPQ7A3            NO: DON'T NEED DEAD DATE                     
RPTPQ7A2 MVC   HALF1,PQAGEDD       DEAD DATE                                    
         BRAS  RE,TESTNCD          CONVERT TO NEW CMPRSD DATE                   
         MVC   DUB+2(2),HALF1                                                   
RPTPQ7A3 BAS   RE,TSTRANGE         FILTER ON CREATED/DEAD DATES                 
         BNE   RPTPQB                                                           
*                                                                               
RPTPQ7AX MVC   SRTUSER,PQSRCID                                                  
         MVC   SRTSUBID,PQSUBID                                                 
         MVC   SRTREPNO,PQREPNO                                                 
         MVC   SRTCLASS,PQCLASS                                                 
         SR    R0,R0                                                            
         IC    R0,PQSEQ                                                         
         CLI   PQSEQ,2             EXTENSION CI'S START WITH SEQNUM=2           
         BL    RPTPQ7B                                                          
         TM    PQTYPE,PQTYXCI      TEST IF EXTENSION CI                         
         BZ    *+8                                                              
         AH    R0,=H'254'          ADJUST SO FIRST XTNSN SEQNUM=256             
RPTPQ7B  STCM  R0,3,SRTSEQ                                                      
         MVC   SRTSTAT,PQSTAT                                                   
         MVC   SRTNEXT,PQCINEXT                                                 
         MVC   HALF1,PQAGELD                                                    
         BRAS  RE,TESTNCD                                                       
         MVC   SRTDATEC,HALF1                                                   
         MVC   SRTTIMEC,PQAGELT                                                 
         B     RPTPQA                                                           
*                                                                               
RPTPQ8   MVC   DUB(2),PQSRCID      FILTER ON USER FOR OTHER CI'S                
         BAS   RE,TSTUSR                                                        
         BNE   RPTPQB                                                           
         MVC   SRTBAD(8),PQKEY     SAVE KEY OF BAD/TEMP/PRGD CI'S               
*                                                                               
RPTPQA   MVC   SRTKEY(2),SRTUSER   GET ALPHA USER ID AND PUT TO SORT            
         CLC   SRTKEY(2),=X'FFFF'                                               
         BE    RPTPQA2                                                          
         MVC   USERN,SRTUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   SRTKEY,USERA                                                     
*                                                                               
         CLC   AGYALPH,SPACES      ARE WE LOOKING FOR A SPECIFIC ALPHA?         
         BNH   RPTPQA2             NO: CONTINUE                                 
         CLC   USERAGY,AGYALPH     YES: MAKE SURE IT'S THE ONE WE WANT          
         BNE   RPTPQB              NO: SKIP IT                                  
*                                                                               
RPTPQA2  GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         AP    CINCI,=P'1'                                                      
*                                                                               
RPTPQB   SR    R1,R1               BUMP TO NEXT CI                              
         ICM   R1,3,CIADDR                                                      
         CLM   R1,3,CJSTTRK                                                     
         BL    *+12                                                             
         AH    R1,CJTRKS           PART2  CI                                    
         B     *+8                                                              
         AH    R1,CITRKS           PART1  CI                                    
         STH   R1,CIADDR                                                        
         BCT   R7,RPTPQ4                                                        
*                                                                               
RPTPQB1  CP    CINCI,=P'0'         TEST IF EMPTY PRINT QUEUE                    
         BNE   RPTPQB2                                                          
         L     RF,=A(INFO6)        NO REPORTS FOUND                             
         BAS   RE,PINFO                                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         GOTO1 (RF),(R1),=C'GET'                                                
         GOTO1 (RF),(R1),=C'GET'   FORCE SORTER COMPLETION                      
         B     GETPARM                                                          
*                                                                               
RPTPQB2  L     RF,=A(INFOA)        PRTQU REPORT FOLLOWS                         
         MVC   0(5,RF),FILEIX                                                   
         BAS   RE,PINFO                                                         
         ZAP   LINE,=P'99'                                                      
*                                                                               
RPTPQC   L     RF,=A(HLINE1)       SET UP HEAD/MID LINES FOR REPORT             
         MVC   MID1(165),000(RF)                                                
         MVC   MID2(165),165(RF)                                                
         MVC   MID3(165),330(RF)                                                
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         MVI   CHRULT,C' '                                                      
*                                                                               
         L     RE,=V(BOXAREA)      SET UP BOX INFO FOR REPORT                   
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
         MVI   RLCLASS-1,C'C'                                                   
         MVI   RLSTAT-1,C'C'                                                    
         MVI   RLARC-1,C'C'                                                     
         MVI   RLMAKER-1,C'C'                                                   
         MVI   RLPAGES-1,C'C'                                                   
         MVI   RLLINES-1,C'C'                                                   
         MVI   RLNCI-1,C'C'                                                     
         MVI   RLCRTD-1,C'C'                                                    
         MVI   RLRETD-1,C'C'                                                    
         MVI   RLHOURS-1,C'C'                                                   
         MVI   RLCPL-1,C'C'                                                     
         MVI   RLPCT-1,C'C'                                                     
         MVI   RLDESC-1,C'C'                                                    
         MVI   RLPRTD-1,C'C'                                                    
         MVI   RLFORMS-1,C'C'                                                   
         MVI   RLPSWD-1,C'C'                                                    
         MVI   RLATTB-1,C'C'                                                    
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
RPTPQD   LA    R6,SRTREC           GET NEXT CI RECORD FROM SORT                 
         MVC   SRTRECL,SRTREC      ----------------------------                 
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+14                                                             
         MVC   SRTREC,0(RF)                                                     
         B     *+14                                                             
         MVC   SRTUSER,=X'FFFF'    SET END OF FILE                              
         MVI   SRTSUBID,X'FF'                                                   
         CLC   SRTUSER,=X'FFFF'                                                 
         BE    RPTPQF                                                           
         CLC   SRTREC(8),SRTRECL   TEST FOR NEW REPORT                          
         BNE   RPTPQF                                                           
*                                                                               
RPTPQE   AP    CINCI,=P'1'         SAME REPORT - BUMP NUM OF CI'S               
         L     RF,AQBNEXT          ------------------------------               
         MVC   0(L'SRTREC,RF),SRTREC                                            
         LA    RF,L'SRTREC(RF)                                                  
         XC    0(L'SRTREC,RF),0(RF)                                             
         ST    RF,AQBNEXT          BUMP TO NEXT SLOT                            
         B     RPTPQD                                                           
**NOP**  CP    CINCI,=P'255'                                                    
**NOP**  BNH   RPTPQD                                                           
**NOP**  CLI   REORG,YES           NUMBER OF CI'S EXCEEDS MAXIMUN               
**NOP**  BE    RPTPQGX                                                          
*                                                                               
RPTPQF   OC    SRTRECL(2),SRTRECL  NEW REPORT                                   
         BZ    RPTPQN              ----------                                   
         L     R6,=A(QBUFF)                                                     
         CLC   0(2,R6),FFS         TEST LAST TIME                               
         BE    RPTPQP                                                           
         MVC   CIADDR(2),SRTADDR   READ FIRST CI REC                            
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACXREC           SAVE FIRST CI DATA IN CXREC                  
         MVC   0(128,R1),0(R5)                                                  
         MVI   CIOK,0              SET REPORT OK                                
         MVC   CISTAT,PQSTAT       SAVE OLD STATUS                              
         MVC   CIREPNO,PQREPNO     SAVE OLD SEQUENCE NUMBER                     
*                                                                               
RPTPQG   TM    PQSTAT,PQSTTE       ERROR IF FIRST CI IN CREATE STATUS           
         BO    RPTPQGP                                                          
RPTPQG2  ZAP   DUB,CINCI           GET NUMBER OF CIS                            
         CVB   R0,DUB                                                           
         STH   R0,DUB                                                           
         CLI   PQSEQ,0             ERROR IF MORE THAN ONE CI IF SINGLE          
         BNE   RPTPQG3                                                          
         CLC   DUB(2),=H'1'                                                     
         BE    RPTPQG4                                                          
         B     RPTPQGP                                                          
RPTPQG3  CLI   PQSEQ,1             ERROR IF FIRST CI SEQ NUM NOT 1              
         BNE   RPTPQGP                                                          
         L     R6,AQBNEXT          ERROR IF LAST CI SEQ NUM NOT TOTAL           
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
         CLC   SRTSEQ,DUB                                                       
         BNE   RPTPQGP                                                          
RPTPQG4  L     R6,AQBNEXT          ERROR IF LAST CI LINK ADDR NOT ZERO          
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
         OC    SRTNEXT,SRTNEXT                                                  
         BZ    RPTPQGX                                                          
RPTPQGP  MVI   CIOK,1              SET REPORT IS IN ERROR                       
         CLI   REORG,YES                                                        
         BNE   RPTPQGX                                                          
         MVI   CISTAT,PQSTPU       SET NEW STATUS                               
         MVC   CIREPNO,=X'FFFF'    SET PURGED DUE TO ERROR                      
         B     RPTPQI                                                           
RPTPQGX  EQU   *                                                                
*                                                                               
RPTPQH   CLI   REORG,YES           REORGANISE MODE                              
         BNE   RPTPQJ              ---------------                              
         TM    PQSTAT,PQSTKE       PROCESS KEEP REPORTS                         
         BZ    RPTPQH1                                                          
         CLC   PQDESC(7),=C'*LINEUP'                                            
         BE    RPTPQH3                                                          
         MVC   HALF1,PQDATEL                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPKDATE                                                   
         BNL   RPTPQH3                                                          
         MVC   HALF1,PQDATED                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPKDATE                                                   
         BNL   RPTPQH3                                                          
         NI    CISTAT,255-PQSTKE   SET UNKEEP STATUS                            
*                                                                               
RPTPQH1  TM    PQSTAT,PQSTLIVE     PROCESS LIVE REPORTS                         
         BZ    RPTPQH2                                                          
         MVC   HALF,PERMNEW        31DEC2059                                    
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+10                                                             
         MVC   HALF,PERMOLD        31DEC2027                                    
         CLC   PQAGERD,HALF        TEST PERMANENT RETAIN                        
         BE    RPTPQH3                                                          
RPTPQH1A MVC   HALF1,PQDATEL                                                    
         BRAS  RE,TESTNCD          SET HALF1 TO NEW CMPRSD DATE                 
         CLC   HALF1,REPLDATE                                                   
         BNL   RPTPQH3                                                          
         MVI   CISTAT,PQSTPU       SET NEW STATUS                               
         MVC   CIREPNO,=X'FFFE'    SET PURGED DUE TO ACTIVE DATE                
         B     RPTPQI                                                           
*                                                                               
RPTPQH2  TM    PQSTAT,PQSTDEAD     PROCESS DEAD REPORTS                         
         BZ    RPTPQH3                                                          
         MVC   HALF,PERMNEW        31DEC2059                                    
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+10                                                             
         MVC   HALF,PERMOLD        31DEC2027                                    
         CLC   PQAGERD,HALF        TEST PERMANENT RETAIN                        
         BE    RPTPQH3                                                          
         MVC   HALF1,PQDATED                                                    
         BRAS  RE,TESTNCD                                                       
         CLC   HALF1,REPDDATE                                                   
         BNL   RPTPQH3                                                          
         OC    PQDATED,PQDATED     TREAT UNKNOWN PRTD DATE AS ACTV              
         BZ    RPTPQH1A                                                         
         MVI   CISTAT,PQSTPU       SET NEW STATUS                               
         MVC   CIREPNO,=X'FFFD'    SET PURGED DUE TO PRINTED DATE               
         B     RPTPQI                                                           
*                                                                               
RPTPQH3  SR    R1,R1               SET NEW SEQUENCE NUMBER                      
         ICM   R1,3,USERSEQ                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,USERSEQ                                                     
*                                                                               
RPTPQI   BRAS  RE,REPTUPDT         UPDATE DATA CONTROL INTERVALS                
*                                                                               
RPTPQJ   L     R1,ACXREC           RESTORE OLD FIRST CI DATA                    
         MVC   0(128,R5),0(R1)                                                  
         BAS   RE,REPTOUT          PRINT REPORT DETAILS                         
*                                                                               
RPTPQK   SP    CINCI,=P'1'         UPDATE FILE AND USER TOTALS                  
         CLI   CIOK,0                                                           
         BE    RPTPQ0                                                           
         AP    CIERROR,=P'1'       BUMP ERROR COUNTERS                          
         AP    CJERROR,CINCI                                                    
         AP    UIERROR,=P'1'                                                    
         AP    UJERROR,CINCI                                                    
RPTPQ0   CLI   CISTAT,PQSTPU       DONT INCLUDE NOW PURGED REPORTS              
         BE    RPTPQKX                                                          
*                                                                               
RPTPQK1  TM    CISTAT,PQSTLIVE     TEST LIVE/DEAD STATUS                        
         BZ    RPTPQK2                                                          
         AP    CILIVE,=P'1'        BUMP LIVE COUNTERS                           
         AP    CJLIVE,CINCI                                                     
         AP    UILIVE,=P'1'                                                     
         AP    UJLIVE,CINCI                                                     
         B     RPTPQK3                                                          
RPTPQK2  AP    CIDEAD,=P'1'        BUMP DEAD COUNTERS                           
         AP    CJDEAD,CINCI                                                     
         AP    UIDEAD,=P'1'                                                     
         AP    UJDEAD,CINCI                                                     
*                                                                               
RPTPQK3  CLC   PQAGERD(3),DATECPR  TEST EXPIRED/AVAIL RETAIN DATE               
         BH    RPTPQK4                                                          
         AP    CIXPRD,=P'1'        BUMP EXPIRED COUNTERS                        
         AP    CJXPRD,CINCI                                                     
         AP    UIXPRD,=P'1'                                                     
         AP    UJXPRD,CINCI                                                     
         B     RPTPQK6                                                          
*                                                                               
RPTPQK4  TM    CISTAT,PQSTDEAD     TEST NON EXPIRED FOR DEAD/KEEP               
         BZ    RPTPQK5                                                          
         TM    CISTAT,PQSTKE                                                    
         BO    RPTPQK5                                                          
         AP    CISEMI,=P'1'        BUMP SEMI INUSE COUNTERS                     
         AP    CJSEMI,CINCI                                                     
         AP    UISEMI,=P'1'                                                     
         AP    UJSEMI,CINCI                                                     
         B     RPTPQK6                                                          
*                                                                               
RPTPQK5  AP    CIRETN,=P'1'        BUMP RETAINED COUNTERS                       
         AP    CJRETN,CINCI                                                     
         AP    UIRETN,=P'1'                                                     
         AP    UJRETN,CINCI                                                     
*                                                                               
RPTPQK6  SR    R0,R0               BUMP LINES/PAGES                             
         ICM   R0,3,PQPAGES                                                     
         CVD   R0,DUB                                                           
         AP    CRPAGES,DUB                                                      
         AP    URPAGES,DUB                                                      
         ICM   R0,7,PQLINES                                                     
         CVD   R0,DUB                                                           
         AP    CRLINES,DUB                                                      
         AP    URLINES,DUB                                                      
*                                                                               
RPTPQKX  ZAP   CINCI,=P'0'         RESET FOR NEXT REPORT                        
         L     RF,=A(QBUFF)                                                     
         ST    RF,AQBNEXT                                                       
         CLC   SRTREC(2),SRTRECL   TEST IF LAST REPORT FOR USER                 
         BE    RPTPQE              NO                                           
*                                                                               
RPTPQL   L     RF,ASUMNEXT         LAST REPORT FOR USER                         
         CLC   100(6,RF),FFS       --------------------                         
         BE    RPTPQL1                                                          
         MVC   0(8,RF),REPIDASV    SAVE USER ID NAME                            
         MVC   8(L'CUSER,RF),CUSER SAVE USER COUNTERS                           
         LA    RF,100(RF)                                                       
         ST    RF,ASUMNEXT         BUMP TO NEXT LOCATION                        
         XC    0(6,RF),0(RF)       SET END OF TABLE                             
*                                                                               
RPTPQL1  BRAS  RE,OUTUSER          PRINT USER COUNTS                            
         BAS   RE,CLRUSER          CLEAR USER COUNTS                            
         CLC   SRTREC(2),=X'FFFF'                                               
         BE    RPTPQO              LAST REPORT FOR LAST USER                    
*                                                                               
RPTPQL2  CLI   COMPACT,YES         COMPACT FORMAT                               
         BE    *+14                YES                                          
         ZAP   LINE,=P'99'         NO NEW USER ON NEW PAGE                      
         B     RPTPQLX                                                          
         ZAP   PACKED,MAXLINE      FIND HOW NEAR END OF PAGE                    
         SP    PACKED,LINE                                                      
         CP    PACKED,=P'7'        ENOUGH ROOM TO START NEW USER                
         BL    RPTPQL3             NO                                           
         MVI   PCTLBOX,C'B'        YES SET TO DRAW BOX BEFORE NEXT              
         B     RPTPQLX                                                          
*                                                                               
RPTPQL3  ZAP   LINE,=P'99'         FORCE NEXT USER TO NEW PAGE                  
*                                                                               
RPTPQLX  EQU   *                                                                
*                                                                               
RPTPQN   MVC   SAVE(10),REPUSER    NEW USER ID - GET NEW ID NAME                
         XC    REPUSER(10),REPUSER -----------------------------                
         MVC   REPUSER,SRTREC                                                   
         BAS   RE,IDOUT                                                         
         MVC   REPUSER(10),SAVE                                                 
         MVC   REPIDASV,REPIDA     SAVE NEW USER ID NAME AT REPIDASV            
         ZAP   CINCI,=P'0'         INIT FOR NEW USER                            
         L     RF,=A(QBUFF)                                                     
         ST    RF,AQBNEXT                                                       
         XC    USERSEQ,USERSEQ                                                  
         B     RPTPQE                                                           
*                                                                               
RPTPQO   MVI   CHRULT,C' '         END OF REPORT                                
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         L     RE,=V(BOXAREA)                                                   
         USING BOXD,RE                                                          
         MVI   BOXOFF,C'Y'         TURN OFF BOXES                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
*                                                                               
RPTPQP   LA    R6,SRTREC           POINT TO SORT RECORD                         
         CLI   SRTSUBID,X'FF'                                                   
         BE    RPTPQQ              SORT RECORD IS EOF RECORD                    
         L     RF,=A(QBUFF)                                                     
         MVC   0(L'SRTREC,RF),SRTREC                                            
         XC    L'SRTREC(L'SRTREC,RF),L'SRTREC(RF)                               
         MVI   CISTAT,PQSTPU                                                    
         MVC   CIREPNO,SRTBAD+5                                                 
         MVC   FULL(2),SRTADDR                                                  
         MVC   FULL+2(2),=X'0100'                                               
         GOTO1 =V(HEXOUT),DMCB,FULL,DUB1,4,=C'MIX'                              
*                                                                               
RPTPQP0  CLI   SRTSUBID,0          TEST IF PURGED                               
         BNE   RPTPQP1                                                          
         OC    CIREPNO,CIREPNO     IGNORE ZERO NUMBERED CI'S                    
         BZ    RPTPQD                                                           
         BRAS  RE,REPTUPDT                                                      
         B     RPTPQD                                                           
*                                                                               
RPTPQP1  CLI   SRTSUBID,1          TEST DISK ERROR                              
         BNE   RPTPQP2                                                          
         L     RF,=A(ERRMSGC)      DISK READ ERROR                              
         MVC   P(27),0(RF)                                                      
         MVC   P+6(5),PRTQID                                                    
         MVC   P+29(8),DUB1                                                     
         B     RPTPQPA                                                          
*                                                                               
RPTPQP2  CLI   SRTSUBID,2          TEST INVALID CI DATA                         
         BNE   RPTPQP3                                                          
         L     RF,=A(ERRMSGD)      INVALID CI DATA                              
         MVC   P(27),0(RF)                                                      
         MVC   P+6(5),PRTQID       SET PRTQ FILE ID                             
         MVC   P+29(8),DUB1                                                     
         MVC   CIREPNO,=X'FFFF'                                                 
         BRAS  RE,REPTUPDT                                                      
         GOTO1 =V(HEXOUT),DMCB,SRTBAD,P+40,8,=C'MIX'                            
         B     RPTPQPA                                                          
*                                                                               
RPTPQP3  CLI   SRTSUBID,3          TEST TEMP CI                                 
         BNE   RPTPQP4                                                          
         BRAS  RE,REPTUPDT                                                      
         B     RPTPQD                                                           
*                                                                               
RPTPQP4  DC    H'0'                                                             
*                                                                               
RPTPQPA  GOTO1 =V(PRINTER)                                                      
         B     RPTPQD                                                           
*                                                                               
RPTPQQ   CLI   REORG,YES           REBUILD INDEX IF REORG MODE                  
         BNE   RPTPQR                                                           
         CLI   WRITE,YES                                                        
         BNE   RPTPQR                                                           
         BRAS  RE,REPTNDX                                                       
         BAS   RE,PQUNLK           UNLOCK PRINT QUEUE                           
*                                                                               
RPTPQR   L     RF,=A(INFOB)        PRTQU REPORT SUMMARY FOLLOWS                 
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
         MVI   SLPAGES-1,C'C'                                                   
         MVI   SLLINES-1,C'C'                                                   
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
*                                                                               
RPTQS    LA    R7,P                R7=A(REPORT SUMMARY LINE)                    
         USING SMRYLD,R7                                                        
         L     RE,=A(SUMBUF)       SET NEXT LOCN IN SUMMARY BUFFER              
         ST    RE,ASUMNEXT                                                      
PRTQS1   MVC   REPIDASV,0(RE)                                                   
         OC    0(6,RE),0(RE)       TEST END OF SUMMARY BUFFER                   
         BZ    PRTQS2                                                           
         MVC   CUSER,8(RE)         RESTORE USER COUNTS                          
         BRAS  RE,SMRYOUT          PRINT USER COUNTS                            
         L     RE,ASUMNEXT                                                      
         LA    RE,100(RE)          BUMP TO NEXT USER                            
         ST    RE,ASUMNEXT                                                      
         B     PRTQS1                                                           
PRTQS2   BRAS  RE,SMRYOUT          PRINT GRAND TOTALS                           
*                                                                               
RPTPQX   MVI   CHRULT,C' '         END OF REPORT                                
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
         L     RF,=A(PQOUT)                                                     
         BASR  RE,RF               PRINT ATTRIBUTES OF PRTQ FILE                
         ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
         EJECT                                                                  
REPTOUT  NTR1                      PRINT REPORT DATA ON ONE LINE                
         USING REPTLD,R7                                                        
         ZAP   FULL,MAXLINE        FIND HOW NEAR END OF PAGE                    
         SP    FULL,LINE                                                        
         CP    FULL,=P'3'          ENOUGH ROOM FOR SUBTOTAL                     
         BH    *+10                YES                                          
         ZAP   LINE,=P'99'         NO SET TO PRINT ON NEXT PAGE                 
*                                                                               
ROUTUS   MVC   RLUSER,REPIDASV     USER ID NAME (ALLOW EXTRA CHR)               
         CLI   REPIDASV+8,C' '                                                  
         BNH   *+10                                                             
         MVC   RLUSER+8(1),REPIDASV+8                                           
*                                                                               
ROUTID   MVC   SAVE(10),REPUSER    REPORT ID AND SEQUENCE NUMBER                
         XC    REPUSER(10),REPUSER                                              
         MVC   REPSUBID,PQSUBID                                                 
         MVC   REPSEQL,CIREPNO                                                  
         CLC   CIREPNO,=X'FFF0'    TEST IF PURGED REPORT                        
         BL    *+10                                                             
         XC    REPSEQL,REPSEQL     YES WE SHOW NO SEQUENCE HERE                 
         CLI   CIOK,0                                                           
         BE    *+10                                                             
         MVC   REPSEQL,PQREPNO     SET OLD REP NUM IF ERROR                     
         BAS   RE,IDOUT                                                         
         MVC   RLID,REPIDA                                                      
         CLI   CIOK,0                                                           
         BE    *+8                                                              
         MVI   RLID+3,C'*'         SET EYECATCHER FOR ERROR                     
         MVC   REPUSER(10),SAVE                                                 
*                                                                               
         MVC   RLCLASS,PQCLASS     CLASS/STATUS                                 
         BRAS  RE,STATOUT                                                       
         MVC   RLSTAT,REPSTATA                                                  
*                                                                               
ROUTARC  LA    R2,RLARC            REPORT ARCHIVE STATUS                        
         MVC   0(1,R2),PQARC                                                    
         OI    0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         TM    PQTYP1,PQTYAE                                                    
         BZ    *+8                                                              
         MVI   0(R2),C'E'                                                       
         TM    PQTYP1,PQTYAR                                                    
         BZ    *+8                                                              
         MVI   0(R2),C'A'                                                       
         TM    PQTYP1,PQTYAD                                                    
         BZ    *+8                                                              
         MVI   0(R2),C'D'                                                       
*                                                                               
         MVC   RLMAKER,PQMAKER                                                  
         OC    RLMAKER,SPACES                                                   
*                                                                               
         EDIT  (P4,CINCI),(4,RLNCI)                                             
ROUTDLI  MVC   DUB,SPACES          LIVE DATE/TIME                               
         LA    R2,RLCRTD                                                        
         MVC   HALF1,PQAGELD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(8,DUB)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
         XC    DUB(2),DUB                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,PQAGELT                                                     
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
         MVC   HALF1,PQAGERD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(8,DUB)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
ROUTDRE1 TM    PQTYP1,PQTYNCD                                                   
         BZ    ROUTDRE2                                                         
         MVI   5(R2),C':'          FLAG NEW CMPRSD DATE                         
         CLC   PQAGERD,PERMNEW                                                  
         BNE   ROUTDRE3                                                         
         MVC   0(5,R2),=C'PERM '                                                
         B     ROUTDRE3                                                         
ROUTDRE2 CLC   PQAGERD,PERMOLD                                                  
         BNE   ROUTDRE3                                                         
         MVC   0(5,R2),=C'PERM '                                                
         B     ROUTDRE3                                                         
ROUTDRE3 XC    DUB(2),DUB                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,PQAGERT                                                       
         MH    R1,=H'10'           CONVERT 10MINS TO HHMM                       
         CH    R1,=H'1440'                                                      
         BH    ROUTDRE4                                                         
         D     R0,=F'60'                                                        
         STC   R0,DUB+1                                                         
         STC   R1,DUB                                                           
ROUTDRE4 BAS   RE,TIMEOUT                                                       
         MVC   06(2,R2),DUB+3                                                   
         MVC   08(2,R2),DUB+6                                                   
*                                                                               
         CLI   CIOK,0              NO CRAP FOR ERROR REPORTS                    
         BE    ROUTPGS                                                          
         MVC   RLPAGES(4),=C'*ER*'                                              
         CLI   PQSEQ,1             TEST IF PART 1 CI                            
         BH    ROUTOLD             NO REST OF DATA DONT EXIST IN RECORD         
*                                                                               
ROUTPGS  EDIT  (B2,PQPAGES),(6,RLPAGES)                                         
         CLI   CIOK,0                                                           
         BE    ROUTLNS                                                          
         MVC   RLPAGES(3),=C'*ER*' FLAG ERR REPORT                              
ROUTLNS  EDIT  (B3,PQLINES),(7,RLLINES)                                         
*                                                                               
ROUTHR   LA    R2,RLHOURS          LIVE AND DEAD RETAIN HOURS                   
         CLC   PQRETNL,=X'FFFF'                                                 
         BL    *+14                                                             
         MVC   0(3,R2),=C'PERM'                                                 
         B     ROUTHR1                                                          
         EDIT  (B2,PQRETNL),(3,0(R2))                                           
ROUTHR1  CLC   PQRETND,=X'FFFF'                                                 
         BL    *+14                                                             
         MVC   4(3,R2),=C'PERM'                                                 
         B     ROUTHRX                                                          
         EDIT  (B2,PQRETND),(3,4(R2))                                           
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
         LH    R1,PQAVCPL                                                       
         LA    R1,2(R1)                                                         
         SR    R0,R0                                                            
         ICM   R0,7,PQLINES                                                     
         MH    R0,=H'100'                                                       
         MR    R0,R0               R0&R1=ACT CAPACITY * 100                     
         DR    R0,RF                                                            
         SLDL  R0,32                                                            
         CH    R0,=H'100'          ADJUST FOR APPROX                            
         BNH   *+8                                                              
         LH    R0,=H'100'                                                       
*                                                                               
         EDIT  (R0),(3,RLPCT),ZERO=NOBLANK                                      
         EDIT  (B2,PQAVCPL),(3,RLCPL)                                           
         MVC   RLDESC,PQDESC                                                    
*                                                                               
ROUTDDE  MVC   DUB,SPACES          DEAD DATE/TIME/LOCATION/COPYS                
         LA    R2,RLPRTD                                                        
         MVC   00(5,R2),DOTS       SET DEFAULT IMAGE IF NOT DEAD                
         MVC   06(4,R2),DOTS                                                    
         MVC   11(8,R2),DOTS                                                    
         MVC   20(1,R2),DOTS                                                    
         OC    PQDATEA,PQDATEA     TEST IF ARCHIVED DATE DEFINED                
         BNZ   ROUTDDE5                                                         
         OC    PQDATED,PQDATED     TEST IF DEAD DATE DEFINED                    
         BNZ   ROUTDDE1                                                         
         B     ROUTDDEX                                                         
*                                                                               
ROUTDDE1 MVC   HALF1,PQDATED                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(8,DUB)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
         MVC   DUB(2),PQTIMED                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   06(2,R2),DUB+3                                                   
         MVC   08(2,R2),DUB+6                                                   
ROUTDDE2 CLI   PQPRCNT,0           NO DATA IF NEVER BEEN PRINTED                
         BE    ROUTDDEX                                                         
         MVC   11(8,R2),PQPRSYM                                                 
ROUTDDE4 MVC   20(1,R2),PQPRCNT                                                 
         OI    20(R2),C'0'                                                      
         B     ROUTDDEX                                                         
*                                                                               
ROUTDDE5 MVC   HALF1,PQDATEA                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF1),(8,DUB)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   00(5,R2),DUB                                                     
*&&UK*&& OI    00(R2),C'0'                                                      
         MVC   DUB(2),PQTIMEA                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   06(2,R2),DUB+3                                                   
         MVC   08(2,R2),DUB+6                                                   
         MVC   11(8,R2),=C'ARCHIVED'                                            
         MVC   20(1,R2),PQPRCNT    SHOW PRINTED COUNT AS WELL                   
         OI    20(R2),C'0'                                                      
ROUTDDEX EQU   *                                                                
*                                                                               
         MVC   RLFORMS,PQFORMS                                                  
         MVC   RLCHARS,PQCHARS                                                  
         EDIT  (B1,PQCOPIES),(2,RLCOPIES)                                       
*                                                                               
ROUTSEC  TM    PQATTB,PQATPW       SECURE REPORT                                
         BZ    ROUTSECX                                                         
         TM    PQSECF1,PQSINONO    TEST IF VALID SECURITY FLAG                  
         BZ    ROUTSECA            YES                                          
         CLI   PQPSWD,C' '         NO IS THERE A PASSWORD/PIN                   
         BNH   ROUTSECX                                                         
         MVC   RLPSWD(4),=C'****'  PIN ATTACHED TO REPORT                       
         B     ROUTSECX                                                         
ROUTSECA TM    PQSECF1,PQSIPIN     TEST IF PIN ATTACHED TO REPORT               
         BZ    ROUTSECB                                                         
         MVC   RLPSWD(4),=C'****'  PIN ATTACHED TO REPORT                       
         B     ROUTSECC                                                         
ROUTSECB TM    PQSECF1,PQSIPID     TEST IF PID ATTACHED TO REPORT               
         BZ    ROUTSECC                                                         
         MVC   RLPSWD(4),=C'PID '                                               
ROUTSECC TM    PQSECF1,PQSIBNK     B=BANK                                       
         BZ    ROUTSECD                                                         
         MVI   RLPSWD+5,C'B'                                                    
         B     ROUTSECX                                                         
ROUTSECD TM    PQSECF1,PQSIPAY     P=PAYROLL                                    
         BZ    ROUTSECE                                                         
         MVI   RLPSWD+5,C'P'                                                    
         B     ROUTSECX                                                         
ROUTSECE TM    PQSECF1,PQSISSN     S=SOCIAL SECURITY NUMBER                     
         BZ    ROUTSECF                                                         
         MVI   RLPSWD+5,C'S'                                                    
         B     ROUTSECX                                                         
ROUTSECF TM    PQSECF1,PQSISEC     C=CONTROL FILE SECURITY                      
         BZ    ROUTSECG                                                         
         MVI   RLPSWD+5,C'C'                                                    
         B     ROUTSECX                                                         
ROUTSECG EQU   *                                                                
ROUTSECX EQU   *                                                                
*                                                                               
ROUTAT   LA    R2,RLATTB           REPORT ATTRIBUTES                            
         TM    PQATTB,PQATERR                                                   
         BZ    *+8                                                              
         MVI   0(R2),C'E'                                                       
         LA    R2,1(R2)                                                         
         TM    PQATTB,PQATJOBO                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'J'                                                       
         LA    R2,1(R2)                                                         
         TM    PQATTB,PQATWIDE                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'W'                                                       
*                                                                               
ROUTOLD  CLI   REORG,YES           OLD STATUS IF REORG                          
         BNE   ROUTOLDX                                                         
         MVC   FLAG,CISTAT         SAVE NEW STATUS                              
         MVC   CISTAT,PQSTAT       SET OLD STATUS                               
         BRAS  RE,STATOUT                                                       
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
*                                                                               
REPTEDIT CVB   R0,DUB                                                           
         EDIT  (R0),(5,P+19),ALIGN=LEFT                                         
         OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         BR    R7                                                               
*                                                                               
TESTNCD  TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BOR   RE                                                               
         CLC   HALF1,PERMOLD       NO NEED TO CONVERT PERMANENT                 
         BHR   RE                                                               
         ST    RE,FULL2                                                         
         GOTO1 =V(DATCON),DMCB,(2,HALF1),(30,HALF1)                             
         L     RE,FULL2                                                         
         BR    RE                                                               
*                                                                               
CLRUSER  NTR1                      CLEAR USER REPORT COUNTER                    
         LA    RF,CUSER                                                         
         LA    R0,14                                                            
         B     SETCTRS                                                          
CLRFILE  NTR1                      CLEAR FILE REPORT COUNTERS                   
         LA    RF,CFILE                                                         
         LA    R0,14                                                            
         B     SETCTRS                                                          
SETCTRS  ZAP   0(6,RF),=P'0'                                                    
         LA    RF,6(RF)                                                         
         BCT   R0,SETCTRS                                                       
         XIT1                                                                   
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
*                                                                               
PINFO    ST    RE,PGSAVRE          PRINT INFO MSG AT RF                         
         TM    UPSIVAL,X'08'                                                    
         BOR   RE                                                               
         MVC   P(60),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         ICM   RE,15,PGSAVRE                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* LOCK/UNLOCK PRINT QUEUE FILE                                        *         
***********************************************************************         
PQLOCK   LA    R0,C'E'                                                          
         B     *+8                                                              
PQUNLK   LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,(R0),PRTQID,X'FF000000',0     FILE EXCLUSIVE           
         L     RE,CISAVRE                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* TEST IF USERID IS WANTED                                            *         
***********************************************************************         
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
         OC    REPSUBID,REPSUBID                                                
         BNZ   TSTUSRY1                                                         
         MVC   REPSUBID,=C'ALL'                                                 
TSTUSRY1 MVC   REPCDATE,10(RE)     SET REPORT CREATE DATE                       
         OC    REPCDATE,REPCDATE                                                
         BNZ   *+10                                                             
         MVC   REPCDATE,DATECPR                                                 
         MVC   CLASSL,18(RE)                                                    
         MVI   CLASSL+11,0                                                      
         MVC   REPSTAT,29(RE)                                                   
         MVC   REPPAGES,30(RE)                                                  
         MVC   REPTYPE,32(RE)                                                   
         MVC   REPTYP1,33(RE)                                                   
         MVC   RTYPEL,41(RE)                                                    
         MVI   RTYPEL+11,0                                                      
         ST    RE,AREPTAB                                                       
         SR    RE,RE                                                            
*                                                                               
TSTUSRX  LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* TEST IF CLASS AT DUB(1) IS WANTED                                   *         
***********************************************************************         
TSTCLASS ST    RE,DUB+4                                                         
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
                                                                                
***********************************************************************         
* TEST IF RTYPE AT DUB(1) IS WANTED                                   *         
***********************************************************************         
TSTRTYPE ST    RE,DUB+4                                                         
         CLI   RTYPEL,0                                                         
         BE    TSTRTY              NO RTYPE SPECIFIED                           
         LA    RE,RTYPEL+1                                                      
*                                                                               
TSTRT2   CLI   0(RE),0             SEARCH RTYPE LIST                            
         BE    TSTRT4                                                           
         CLC   DUB(1),0(RE)                                                     
         BE    TSTRT6                                                           
         LA    RE,1(RE)                                                         
         B     TSTRT2                                                           
*                                                                               
TSTRT4   CLI   RTYPEL,C'+'         RTYPE NOT IN LIST                            
         BE    TSTRTN                                                           
         B     TSTRTY                                                           
TSTRT6   CLI   RTYPEL,C'+'         RTYPE IN LIST                                
         BE    TSTRTY                                                           
         B     TSTRTN                                                           
*                                                                               
TSTRTN   LA    RE,1                CC=NEQ IF RTYPE NOT WANTED                   
         B     *+6                                                              
TSTRTY   SR    RE,RE               CC=EQL IF RTYPE IS WANTED                    
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
TSTSTAT  ST    RE,DUB+4            TEST IF STATUS AT DUB(1) IS WANTED           
         TM    DUB,PQSTTE                                                       
         BZ    *+12                                                             
         TM    DUB,PQSTAC                                                       
         BZ    TSTSTN                                                           
         CLI   REPSTAT,0                                                        
         BE    TSTSTY                                                           
*                                                                               
TSTST2   MVC   DUB+1(1),REPSTAT                                                 
         TM    DUB+1,PQSTKE                                                     
         BZ    *+12                                                             
         TM    DUB,PQSTKE                                                       
         BZ    TSTSTN                                                           
         NI    DUB+1,255-PQSTKE                                                 
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
TSTTYPE  ST    RE,DUB+4            TEST IF TYPES AT DUB(2) ARE WANTED           
         CLI   REPTYPE,0                                                        
         BNE   *+12                                                             
         CLI   REPTYP1,0                                                        
         BE    TSTTYY                                                           
         MVC   DUB+2(1),REPTYPE                                                 
         MVC   DUB+3(1),REPTYP1                                                 
*                                                                               
TSTTY1   TM    DUB+3,PQTYAR        TEST IF WANT ARCHIVABLE REPORT               
         BZ    TSTTY2                                                           
         TM    DUB+3,PQTYAD                                                     
         BO    TSTTY2                                                           
         TM    DUB+1,PQTYAR                                                     
         BZ    TSTTYN                                                           
         TM    DUB+1,PQTYAD        SKIP IF ALREADY BEEN ARCHIVED                
         BO    TSTTYN                                                           
         B     TSTTYY              ARCHIVABLE BUT NOT ARCHIVED                  
*                                                                               
TSTTY2   TM    DUB+3,PQTYAD        TEST IF WANT ARCHIVED REPORT                 
         BZ    TSTTY3                                                           
         TM    DUB+1,PQTYAD                                                     
         BO    TSTTYY                                                           
         TM    DUB+3,PQTYAR        TEST IF ARCHIVABLE AS WELL                   
         BZ    TSTTYN                                                           
         TM    DUB+1,PQTYAR                                                     
         BO    TSTTYY                                                           
         B     TSTTYN                                                           
*                                                                               
TSTTY3   TM    DUB+3,PQTYAE        TEST IF WANT ARCHIVE ELIGIBLE                
         BZ    TSTTY4                                                           
         TM    DUB+1,PQTYAE                                                     
         BO    TSTTYY                                                           
         TM    DUB+3,PQTYAR        TEST IF ARCHIVABLE AS WELL                   
         BZ    TSTTYN                                                           
         TM    DUB+1,PQTYAR                                                     
         BO    TSTTYY                                                           
         B     TSTTYN                                                           
*                                                                               
TSTTY4   EQU   *                   MORE TEST TYPES HERE IF NECESSARY            
*                                                                               
TSTTYY   SR    RE,RE               CC=EQL IF TYPE IS WANTED                     
         B     *+8                                                              
TSTTYN   LA    RE,1                CC=NEQ IF TYPE IS NOT WANTED                 
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
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
IDOUT    NTR1                      EXPAND REPORT ID TO REPIDA                   
         LA    R4,REPIDA                                                        
         MVC   REPIDA,SPACES                                                    
*                                                                               
IDOUT2   OC    REPUSER,REPUSER     REPORT USER ID                               
         BZ    IDOUT6                                                           
         MVC   USERN,REPUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   0(10,R4),USERA                                                   
         LA    R4,11(R4)                                                        
*                                                                               
IDOUT6   OC    REPSUBID,REPSUBID   REPORT SUB ID                                
         BZ    IDOUT8                                                           
         MVC   0(3,R4),REPSUBID                                                 
         LA    R4,4(R4)                                                         
*                                                                               
IDOUT8   OC    REPSEQL,REPSEQL     REPORT SEQ NUM                               
         BZ    IDOUTA                                                           
         EDIT  (B2,REPSEQL),(5,(R4))                                            
         LA    R4,6(R4)                                                         
*                                                                               
IDOUTA   OC    REPIDA,SPACES                                                    
         LA    R0,L'REPIDA                                                      
         GOTO1 =V(SQUASHER),DMCB,REPIDA,(C',',(R0))                             
*                                                                               
IDOUTX   XIT1                                                                   
                                                                                
***********************************************************************         
* GET USER ALPHA FROM USER NUMBER                                     *         
***********************************************************************         
GETUSER  NTR1                                                                   
*                                                                               
         MVI   DMCB1+8,0                                                        
         MVC   USERA,SPACES                                                     
         MVC   USERPOW,SPACES                                                   
         MVC   USERAGY,SPACES                                                   
         L     R5,=A(CTBUF)                                                     
*                                                                               
GUSER1   OC    0(2,R5),0(R5)       SEARCH USER ID TABLE                         
         BZ    GUSER2                                                           
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    GUSER2              END OF TABLE                                 
         CLC   0(2,R5),USERN                                                    
         BE    *+12                                                             
         LA    R5,18(R5)                                                        
         B     GUSER1                                                           
         MVC   USERA,2(R5)         MOVE OUT ALPHA FROM TABLE                    
         MVC   USERPOW,12(R5)      MOVE OUT POWER CODE FROM TABLE               
         MVC   USERAGY,16(R5)      MOVE OUT AGENCY FROM TABLE                   
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
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    GUSER3B                                                          
         CLI   0(R5),X'06'         AGENCY ALPHA ID ELEMENT                      
         BE    GUSER3C                                                          
         CLI   0(R5),X'30'         DESTINATION DETAIL ELEMENT                   
         BE    GUSER3D                                                          
GUSER3A  IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     GUSER3                                                           
GUSER3B  MVC   USERA,2(R5)         EXTRACT ALPHA                                
         B     GUSER3A                                                          
GUSER3C  MVC   USERAGY,2(R5)       EXTRACT AGENCY                               
         B     GUSER3A                                                          
GUSER3D  MVC   USERPOW,82(R5)      EXTRACT POWER CODE                           
         B     GUSER5                                                           
*                                                                               
GUSER4   LLH   R6,USERN            ERROR IN FINDING USER ID NUM                 
         CVD   R6,DUB                                                           
         UNPK  USERA(6),DUB        SET ALPHA TO 6 DIGIT USER ID NUM             
         OI    USERA+5,X'F0'                                                    
         MVC   USERA+6(4),SPACES                                                
         MVC   USERPOW,=C'XXXX'                                                 
*&&UK*&& MVC   USERAGY,=C'D1'                                                   
*&&US*&& MVC   USERAGY,=C'SJ'                                                   
*                                                                               
GUSER5   LR    R5,R0               SAVE NUM/ALPHA IN TABLE                      
         OC    0(2,R5),0(R5)                                                    
         BNZ   GUSERX              NO MORE ROOM IN TABLE                        
         MVC   0(2,R5),USERN                                                    
         MVC   2(10,R5),USERA                                                   
         MVC   12(4,R5),USERPOW                                                 
         MVC   16(2,R5),USERAGY                                                 
*                                                                               
GUSERX   CLI   DMCB1+8,0           SET CC=EQL IF USER FOUND OK                  
         XIT1                                                                   
                                                                                
***********************************************************************         
* GET NEXT REPORT FROM DISK OR TAPE                                   *         
***********************************************************************         
GETREPT  NTR1                                                                   
*                                                                               
         L     R2,AQH                                                           
         L     R3,AQ                                                            
         USING PQPLD,R3            R3=A(REPORT HEADER PRINT LINE)               
         TM    INPUT,TAPE                                                       
         BZ    GETR4                                                            
*                                                                               
GETR2    BAS   RE,RITAPE           READ TAPE UNTIL REPORT HEADER                
         OC    0(4,R2),0(R2)                                                    
         BZ    GETRX               EXIT IF EOF                                  
         CLC   0(2,R2),LENSOF      TEST START OF FILE RECORD                    
         BE    *+14                                                             
         CLC   0(2,R2),LENSOF1                                                  
         BNE   GETR2                                                            
         CLC   0(5,R3),SOFLAB                                                   
         BNE   GETR2                                                            
         CLI   PRTQINP,0           TEST IF ANY PRTQ FILES NAMED                 
         BE    GETRX               NO                                           
         XC    INDEX,INDEX         YES FIND TARGET PRTQ FILE FOR REPORT         
         MVC   NXNDX(2),QLSRCID                                                 
         GOTO1 VDATAMGR,DMCB,(X'20',=C'GFILE'),PRTQUE,INDEX,,(R5)               
         SR    R1,R1                                                            
         IC    R1,NXINFO           GET INTERNAL PRTQ FILE NUMBER                
         LA    RF,PRTQINP(R1)                                                   
         TM    0(RF),X'0F'         TEST IF THIS PRTQ FILE NAMED                 
         BZ    GETR2               NO THEN IGNORE THIS REPORT                   
         B     GETRX                                                            
*                                                                               
GETR4    L     R0,ACXREC           GET NEXT INDEX ENTRY USING CXREC             
         MVI   NXFLAG,0                                                         
         GOTO1 VDATAMGR,DMCB,(X'20',=C'SEQ'),PRTQID,INDEX,(R3),(R0)             
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
*                                                                               
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
* PRINT LINE DATA IN Q - R3 POINTS TO DATA                                      
*                                                                               
PLINEQ   ST    RE,CISAVRE                                                       
         LA    RE,CTLCHR           POINT TO TABLE OF VALID CC CHRS              
         LR    RF,R3               POINT TO CC CHR IN RECORD                    
         TM    PQLINET,PQLTFL                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         TM    PQLINET,PQLTCC      TEST IF LINE HAS CC CHR                      
         BO    PLINEQ1             YES                                          
         BCTR  RF,0                NO POINT TO WHERE IT WOULD BE                
         B     PLINEQ2                                                          
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
*ERMNEW  DC    X'BF9F'             CMPRSD NEW 31DEC2059                         
PERMNEW  DC    X'FF9F'             SAME                                         
PERMOLD  DC    X'FF9F'             CMPRSD OLD 31DEC2027                         
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
MSG      DC    X'00014001',C'MSG     ',A(MSGL)                                  
INFO     DC    X'00024001',C'INFO    ',A(INFOL)                                 
WARN     DC    X'00024001',C'WARNINGS',A(WARNL)                                 
LOAD     DC    X'00000004',C'LOAD    ',A(LOADNAME)                              
PQID     DC    X'00002001',C'PQID    ',A(VPQID)                                 
USER     DC    X'00002003',C'USER    ',A(VUSER)                                 
AGENCY   DC    X'00002002',C'AGENCY  ',A(VAGY)                                  
REPORT   DC    X'00002002',C'REPORT  ',A(VREPT)                                 
CLASS    DC    X'00002001',C'CLASS   ',A(VCLASS)                                
RTYPE    DC    X'00002001',C'RTYPE   ',A(VRTYPE)                                
STATUS   DC    X'00002001',C'STATUS  ',A(VSTAT)                                 
TYPE     DC    X'00002001',C'TYPE    ',A(VTYPE)                                 
KDAYS    DC    X'00002001',C'KDAYS   ',A(VKDAY)                                 
LDAYS    DC    X'00002001',C'LDAYS   ',A(VLDAY)                                 
DDAYS    DC    X'00002001',C'DDAYS   ',A(VDDAY)                                 
CDATE    DC    X'00002001',C'CDATE   ',A(VCDATE)                                
CRANGE   DC    X'00002001',C'CRANGE  ',A(VCRANGE)                               
DRANGE   DC    X'00002001',C'DRANGE  ',A(VDRANGE)                               
REORG    DC    X'00014001',C'REORG   ',A(REORGL)                                
BIG20    DC    X'00014001',C'BIG20   ',A(BIG20L)                                
WRITE    DC    X'02022001',C'WRITE   ',A(VWRITE)                                
         DC    X'00022001',C'WRSRV   ',A(VWRITE)                                
COMPACT  DC    X'00014001',C'COMPACT ',A(COMPACTL)                              
SORT     DC    X'00034001',C'SORT    ',A(SORTL)                                 
PAGES    DC    X'00002001',C'PAGES   ',A(VPAGE)                                 
LOCK     DC    X'00024001',C'LOCK    ',A(LOCKL)                                 
ENTRYS   DC    X'00002001',C'ENTRYS  ',A(VNCIS)                                 
CISIZE   DC    X'00002001',C'CISIZE  ',A(VTRKS)                                 
BLKSIZE  DC    X'00002004',C'BLKSIZE ',A(VBLKS)                                 
OENTRYS  DC    X'00002001',C'OENTRYS ',A(VNCIS)                                 
OCISIZE  DC    X'00002001',C'OCISIZE ',A(VTRKS)                                 
DSPNDX   DC    X'00014001',C'DSPNDX  ',A(DSPNDXL)                               
FILE     DC    X'00002005',C'FILEID  ',A(VFILE)                                 
         DC    X'00002005',C'DAFILE  ',A(VFILE)                                 
NEWUSER  DC    X'00002003',C'NEWUSER ',A(VNEWUSER)                              
EXTID    DC    X'00004001',C'EXTID   ',A(EXTIDL)                                
EXPIRED  DC    X'00024001',C'EXPIRED ',A(EXPIREDL)                              
OCIBLKS  DC    X'00002001',C'OCIBLKS ',A(VOCIBLKS)                              
PARMTBLX DC    X'FFFF'                                                          
*                                                                               
AGYALPH  DC    CL2' '                                                           
LOADNAME DC    CL8' '                                                           
CLASSL   DC    XL12'00'            MAX OF 10 CLASSES                            
RTYPEL   DC    XL12'00'            MAX OF 10 REPORT TYPES                       
REPLMODE DC    X'00'               REPLACE MODE                                 
ARCMODE  DC    X'00'               ARCHIVE MODE                                 
*                                                                               
FILEID   DC    CL144' ',X'FF'      MAX 0F 16 PRTQ FILES                         
FILEIX   DC    CL144' ',X'FF'                                                   
PRTQINP  DC    XL17'00',X'FF'                                                   
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
HALF1    DS    H                                                                
HALF2    DS    H                                                                
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
COPYCTRS DS    0XL136                                                           
TOTLREAD DS    F                                                                
TOTLCOPY DS    F                                                                
         DS    16XL8               READ/COPY FOR 16 PRTQ FILES                  
*                                                                               
MAXSEQ   DC    F'65500'            MAXIMUM SEQUENCE NUM FOR USER ID             
FRSTTIME DC    C'Y'                                                             
OPENCTL  DC    C'N'                                                             
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
DATECPRO DS    XL2                 CMPRSD DATE OLD FORMAT                       
DATECPRN DS    XL2                 CMPRSD DATE NEW FORMAT (BASE 1964)           
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
USERPOW  DS    CL4                                                              
USERAGY  DS    CL2                                                              
         DS    CL2                                                              
*                                                                               
         DS    0F                                                               
INDEX    DS    0CL40                                                            
NXNDX    DS    CL24                PRTQ INDEX ENTRY                             
NXINFO   DS    XL2                 INFO PASSING FIELD                           
NXREPNOX DS    XL2                 UPPER LIMIT                                  
NXCIADDR DS    XL2                 TTTT OF FIRST CI                             
NXFLAG   DS    XL1                 FLAG VALUES                                  
         DS    XL1                 N/D                                          
NXUSRINF DS    XL8                 USER INFO                                    
         DS    XL24                                                             
*                                                                               
SAVE     DS    CL256                                                            
WORK     DS    CL256                                                            
OPERANS  DS    CL8                                                              
C        DS    CL80                                                             
*                                                                               
RDID     EQU   01                                                               
WTCKD    EQU   05                                                               
WTERASE  EQU   08                                                               
DAOPEN   EQU   14                                                               
DACLOSE  EQU   15                                                               
DARPT    EQU   16                                                               
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
DADDS    DC    C'DADDS   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMPRINT  DC    C'DMPRINT '                                                      
DMWRT    DC    C'DMWRT   '                                                      
PRTQUE   DC    C'PRTQU   '                                                      
*                                                                               
APRTQXPE DC    A(0)                                                             
APRTQLST DC    A(0)                                                             
PRTQMAX  DC    AL1(0)                                                           
         DC    AL1(0)                                                           
PRTQINT  DC    AL1(0)                                                           
PRTQEXT  DC    AL1(0)                                                           
PRTQID   DC    CL8' '                                                           
PRTQDTF  DC    A(0)                                                             
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
APRTQINP DS    A                                                                
*                                                                               
CINDXMIN DC    H'2'                                                             
CPAGE    DS    H                                                                
*                                                                               
*DMPRTQW                                                                        
       ++INCLUDE DMPRTQW                                                        
CJPAGE   DS    H                                                                
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
CRPAGES  DS    PL6                                                              
CRLINES  DS    PL6                                                              
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
URPAGES  DS    PL6                                                              
URLINES  DS    PL6                                                              
*                                                                               
SRTREC   DS    CL40                                                             
SRTRECL  DS    CL40                                                             
*                                                                               
REPUSERA DS    CL10                                                             
REPNEWUA DS    CL10                                                             
REPNEWUN DS    XL2                                                              
*                                                                               
REPDEFN  DS    0XL10                                                            
REPUSER  DS    XL2                                                              
REPSUBID DS    CL3                                                              
REPSEQL  DS    XL2                                                              
REPFLAG  DS    XL1                 DEFINES CONTENTS OF SEQL AND SEQH            
REPSEQH  DS    XL2                                                              
*                                                                               
REPCDATE DS    XL2                                                              
REPPAGES DS    XL2                                                              
REPSTAT  DS    XL1                                                              
REPSTATA DS    CL3                                                              
REPIDA   DS    CL24                                                             
REPIDASV DS    CL10                                                             
REPTYPE  DS    XL1                                                              
REPTYP1  DS    XL1                                                              
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
LENSOF   DC    H'0132'             TOT LEN OF TAPE STR-OF-FILE REC OLD          
LENSOF1  DC    H'0137'             TOT LEN OF TAPE STR-OF-FILE REC NEW          
LENEOF   DC    H'0014'             TOT LEN OF TAPE END-OF-FILE REC              
*                                                                               
SOFLAB   DS    0CL10                                                            
         DC    X'0000',C'SOFSOF',X'0000'                                        
EOFLAB   DS    0CL10                                                            
         DC    X'FFFF',C'EOFEOF',X'FFFF'                                        
*                                                                               
DOTS     DC    24C'.'                                                           
*                                                                               
ARCLINE  DC    X'004D09'           <ARC=AA,SPP,FFFF  ,UUUUU,UUUUU,...>          
         DC    C'<'                                                             
ARCCARD  DC    CL72'ARC=AA,SPP,FFFF  ,NNNNN,NNNNN, ,A'                          
         DC    C'>'                                                             
ARCLINEL EQU   *-ARCLINE                                                        
*                                                                               
PLXTRN   DC    A(CIREC),A(0),A(0),A(0)                                          
         DC    V(PRINTER),V(CPRINT)                                             
         EJECT                                                                  
* LAST POINT OF ADDRESSABLE STORAGE VIA RB,RA,R9,R8                             
* ALL MINI SUBROUTINES TO GO HERE TO GET ADDRESSABILITY VIA BRAS                
*                                                                               
STATOUT  NTR1  BASE=*,LABEL=*      EXPAND REPORT STATUS TO REPSTATA             
         MVC   REPSTATA,SPACES                                                  
         LA    RE,REPSTATA                                                      
         CLI   CISTAT,0            TEST PURGED STATUS                           
         BNE   *+14                                                             
         MVC   0(2,RE),=C'..'                                                   
         B     STATOUTX                                                         
         MVI   0(RE),C'?'                                                       
*                                                                               
STATOUT1 TM    CISTAT,PQSTAC       MAIN STATUS = ACTV/HOLD/PRTD/SENT            
         BZ    *+12                                                             
         MVI   0(RE),C'A'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,PQSTHO                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'H'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,PQSTPR                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'P'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,PQSTSE                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'S'                                                       
         B     STATOUT2                                                         
*                                                                               
STATOUT2 TM    CISTAT,PQSTKE       SUB STATUS-1 = KEEP                          
         BZ    STATOUTX                                                         
         MVI   1(RE),C'K'                                                       
*                                                                               
STATOUTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
OUTUSER  NTR1  BASE=*,LABEL=*      PRINT USER REPORT COUNTS                     
*                                                                               
         MVC   P,SPACES            MINI BOX LINE FOR USER COUNTS                
         MVC   RLUSER-1(1),SPACES                                               
         MVC   RLUSER,SPACES                                                    
         MVI   RLID-1,LEFTT                                                     
         MVC   RLID,USCORES                                                     
         MVI   RLCLASS-1,RIGHTT                                                 
*                                                                               
         MVI   RLPAGES-1,LEFTT                                                  
         MVC   RLPAGES,USCORES                                                  
         MVI   RLLINES-1,CROSS                                                  
         MVC   RLLINES,USCORES                                                  
         MVI   RLNCI-1,CROSS                                                    
         MVC   RLNCI,USCORES                                                    
         MVI   RLCRTD-1,RIGHTT                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   RLUSER,SPACES       SET USER ID NAME TO SPACES                   
         MVC   RLID(4),=C'TOT='    SET TOTAL NUM OF REPORTS                     
         ZAP   PACKED,UILIVE                                                    
         AP    PACKED,UIDEAD                                                    
         EDIT  (P6,PACKED),(5,RLID+4),ALIGN=LEFT                                
*                                                                               
         ZAP   PACKED,URPAGES                                                   
         EDIT  (P6,PACKED),(7,RLPAGES-1)                                        
         ZAP   PACKED,URLINES                                                   
         EDIT  (P6,PACKED),(8,RLLINES-1)                                        
*                                                                               
         ZAP   PACKED,UILIVE       SET TOTAL NUM OF CI'S                        
         AP    PACKED,UIDEAD                                                    
         AP    PACKED,UJLIVE                                                    
         AP    PACKED,UJDEAD                                                    
         EDIT  (P6,PACKED),(5,RLNCI-1)                                          
*                                                                               
OUTUSERX GOTO1 =V(PRINTER)         PRINT USER TOTALS                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPTUPDT NTR1  BASE=*,LABEL=*      UPDATE REPORT'S CI'S                         
         CLI   REORG,YES                                                        
         BNE   REPTUPX                                                          
         CLI   WRITE,YES                                                        
         BNE   REPTUPX                                                          
         L     R6,=A(QBUFF)                                                     
         USING SRTRECD,R6                                                       
         L     R5,ACIREC                                                        
         USING PQRECD,R5                                                        
*                                                                               
REPTUP2  OC    SRTUSER,SRTUSER                                                  
         BZ    REPTUPX             END OF CI LIST                               
         MVC   CIADDR(2),SRTADDR                                                
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BNE   REPTUPE                                                          
         CLI   CISTAT,0            PURGE CI                                     
         BNE   *+14                                                             
         XC    PQINDEX,PQINDEX                                                  
         B     REPTUP4                                                          
         MVC   PQSTAT,CISTAT       SET NEW STATUS                               
         LA    R6,L'SRTREC(R6)                                                  
         MVC   PQCINEXT,SRTADDR                                                 
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
         LTORG                                                                  
         EJECT                                                                  
REPTNDX  NTR1  BASE=*,LABEL=*      UPDATE PRTQUE INDEX FROM DATA CI'S           
         MVC   CXADDR,=X'00010100'                                              
         LH    R1,CICINDX                                                       
         MH    R1,CITRKS                                                        
         LA    R1,1(R1)                                                         
         STH   R1,CIADDR                                                        
         MVC   CIADDR+2(2),=X'0100'                                             
         L     R5,ACIREC           R5=A(DATA REC)                               
         L     R6,ACXREC           R6=A(INDEX REC)                              
         LH    R7,CICINDX          R7=A(INDEX ENTRY)                            
         STH   R7,CXENTRY                                                       
         MH    R7,CINDXLN                                                       
         AR    R7,R6                                                            
         MVI   FLAG1,1             SET PART 1 INDEX                             
*                                                                               
REPTNDX2 GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CXADDR,(R6)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ INDEX REC                   
*                                                                               
REPTNDX4 CLC   0(L'PQINDEX,R7),=32X'FF'                                         
         BNE   *+6                                                              
         DC    H'0'                DIE IF INDEX CLOBBERED                       
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CIADDR,(R5)                          
         CLI   8(R1),0                                                          
         BNE   *+10                                                             
         MVC   0(L'PQINDEX,R7),0(R5)    COPY DATA TO INDEX IF NO ERRORS         
*                                                                               
REPTNDX6 AH    R7,CINDXLN          BUMP TO NEXT INDEX ENTRY                     
         LH    RF,CXENTRY                                                       
         LA    RF,1(RF)                                                         
         STH   RF,CXENTRY                                                       
         CH    RF,CIENTRYS                                                      
         BNL   REPTNDXA            END OF INDEX PAGE                            
         CLC   0(L'PQINDEX,R7),=32X'FF'                                         
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
         STH   RE,CIADDR                                                        
         BR    RF                                                               
*                                                                               
REPTNDXA GOTO1 VDATAMGR,DMCB,DMWRT,PRTQID,CXADDR,(R6)                           
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
         LTORG                                                                  
         EJECT                                                                  
SMRYOUT  NTR1  BASE=*,LABEL=*      PRINT USER SUMMARY LINE                      
         USING SMRYLD,R7                                                        
         ZAP   FULL,MAXLINE        FIND HOW NEAR END OF PAGE                    
         SP    FULL,LINE                                                        
         CP    FULL,=P'3'          ENOUGH ROOM FOR GRAND TOTAL                  
         BH    *+10                YES                                          
         ZAP   LINE,=P'99'         NO SET TO PRINT ON NEXT PAGE                 
*                                                                               
SOUT1    OC    REPIDASV,REPIDASV   TEST IF CALL FOR GRAND TOTAL                 
         BZ    SOUT2               YES                                          
         MVC   SLUSER,REPIDASV     SET USER ID (ALLOW EXTRA CHR)                
         CLI   REPIDASV+8,C' '                                                  
         BNH   *+10                                                             
         MVC   SLUSER+8(1),REPIDASV+8                                           
         B     SOUT3                                                            
*                                                                               
SOUT2    MVI   PCTLBOX,C'B'        PRINT GRAND TOTAL                            
         MVC   SLUSER(5),=C'TOTAL'                                              
         MVC   CUSER,CFILE         SET USER COUNTS TO GRAND TOTAL               
*                                                                               
SOUT3    ZAP   DUB1,UILIVE         TOTAL NUMBER OF REPORTS                      
         AP    DUB1,UIDEAD                                                      
         AP    DUB1,UIERROR                                                     
         ZAP   DUB2,CILIVE                                                      
         AP    DUB2,CIDEAD                                                      
         AP    DUB2,CIERROR                                                     
         ZAP   CITOTAL,DUB2        SAVE TOTAL NUMBER OF REPORTS                 
         OC    REPIDASV,REPIDASV                                                
         BNZ   SOUT4                                                            
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         CVD   R0,DUB2             SET GRAND TOTAL PCT VALUE                    
SOUT4    LA    R2,SLRTOTL                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,URPAGES        TOTAL NUMBER OF PAGES                        
         ZAP   DUB2,CRPAGES                                                     
         LA    R2,SLPAGES                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,URLINES        TOTAL NUMBER OF LINES                        
         ZAP   DUB2,CRLINES                                                     
         LA    R2,SLLINES                                                       
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
         ZAP   DUB1,UILIVE         NUMBER OF LIVE REPORTS                       
         ZAP   DUB2,CILIVE                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRLIVE                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIDEAD         NUMBER OF DEAD REPORTS                       
         ZAP   DUB2,CIDEAD                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRDEAD                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIERROR        NUMBER OF ERROR REPORTS                      
         ZAP   DUB2,CIERROR                                                     
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRERROR                                                      
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIXPRD         NUMBER OF EXPIRED REPORTS                    
         ZAP   DUB2,CIXPRD                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRXPRD                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UISEMI         NUMBER OF SEMI EXPIRED REPORTS               
         ZAP   DUB2,CISEMI                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRSEMI                                                       
         BAS   R3,SOUTEDIT                                                      
*                                                                               
         ZAP   DUB1,UIRETN         NUMBER OF RETAINED REPORTS                   
         ZAP   DUB2,CIRETN                                                      
         OC    REPIDASV,REPIDASV                                                
         BNZ   *+10                                                             
         ZAP   DUB2,CITOTAL        SET GRAND TOTAL PCT VALUE                    
         LA    R2,SLRRETN                                                       
         BAS   R3,SOUTEDIT                                                      
         B     SOUTX                                                            
*                                                                               
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
*                                                                               
SOUTX    GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
DATECHK  NTR1  BASE=*,LABEL=*      VALIDATE CMPRSD DATE FIELD AT R1             
*                                                                               
         OC    0(2,R1),0(R1)                                                    
         BNZ   *+12                                                             
         LHI   RF,-1               CC=LOW IF NO DATE                            
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
         AHI   R0,-64              BASE YEAR FOR NEW CMPRSD IS 1964             
         STC   R0,DUB+0            R0=YEAR                                      
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
         XIT1                                                                   
         LTORG                                                                  
DCHKT    DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
                                                                                
TIMECHK  NTR1  BASE=*,LABEL=*      VALIDATE TIME FIELD AT R1                    
*                                                                               
         OC    0(2,R1),0(R1)                                                    
         BNZ   *+12                                                             
         LHI   RF,-1               CC=LOW IF NO TIME                            
         B     TCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID TIME                      
         CLI   0(R1),23                                                         
         BH    TCHKX                                                            
         CLI   1(R1),59                                                         
         BH    TCHKX                                                            
         SR    RF,RF               CC=EQUAL IF OK                               
TCHKX    LTR   RF,RF                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*00(2)   USER ID NUMBER                                                         
*02(8)   REPORT SUB ID                                                          
*10(2)   CREATION DATE                                                          
*12(6)   N/D                                                                    
*18(11)  CLASS LIST                                                             
*29(1)   STATUS FILTER                                                          
*30(2)   REPORT PAGES                                                           
*32(1)   TYPE FILTER                                                            
*33(1)   TYP1 FILTER                                                            
*34(8)   USER ID ALPHA (NOT USED YET)                                           
*42(11)  RTYPE LIST                                                             
*53(11)  N/D                                                                    
         DS    0D                                                               
         DC    8X'FF',C'*REPTAB*',8X'FF',C'*REPTAB*',8X'FF'                     
REPTAB   DS    50XL64                                                           
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'QHQHQHQH'                                                      
QH       DS    4C                                                               
Q        DS    1024C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'CICICICI'                                                      
CIREC    DS    14336C                                                           
*                                                                               
         DS    0D                                                               
         DC    C'CXCXCXCX'                                                      
CXREC    DS    14336C                                                           
*                                                                               
         DS    0D                                                               
         DC    C'QBQBQBQB'                                                      
QBUFF    DS    21000C                                                           
*                                                                               
         DS    0D                                                               
         DC    C'WKWKWKWK'                                                      
PQWORK   DS    8000D                                                            
*                                                                               
         DS    0D                                                               
         DC    C'UIDSUIDS'                                                      
CTBUF    DC    1000XL18'00'                                                     
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'SUMBSUMB'                                                      
SUMBUF   DC    1001XL100'00'                                                    
         DC    6X'FF'                                                           
*                                                                               
CTREC    DS    1024C                                                            
PARMCARD DS    CL80                                                             
         EJECT                                                                  
REPOUTA  DC    CL16'REPORT ID'                                                  
         DC    CL16'REPORT CLASS'                                               
         DC    CL16'REPORT STATUS'                                              
         DC    CL16'REPORT NAME'                                                
         DC    CL16'REPORT SYS/PRG'                                             
         DC    CL16'LOCN CREATED'                                               
         DC    CL16'DATE CREATED'                                               
         DC    CL16'TIME CREATED'                                               
         DC    CL16'LIVE RETAIN HRS'                                            
         DC    CL16'DEAD RETAIN HRS'                                            
         DC    CL16'DATE RETAINED'                                              
         DC    CL16'TIME RETAINED'                                              
         DC    CL16'NUM OF PAGES'                                               
         DC    CL16'LINES PER PAGE'                                             
         DC    CL16'NUM OF LINES'                                               
         DC    CL16'CHRS PER LINE'                                              
         DC    CL16'NUM OF CIS'                                                 
         DC    CL16'LOCN PRINTED'                                               
         DC    CL16'COUNT PRINTED'                                              
         DC    CL16'DEVICE PRINTED'                                             
         DC    CL16'DATE PRINTED'                                               
         DC    CL16'TIME PRINTED'                                               
*                                                                               
INFO0    DC    CL60'---------------'                                            
INFO1    DC    CL60'PRINT QUEUE FILE MAINTENANCE'                               
INFO2    DC    CL60'PARAMETER CARDS'                                            
INFO3    DC    CL60'ACTION MESSAGES'                                            
INFO4    DC    CL60'INITIALISED XXXXX'                                          
INFO5    DC    CL60'ERROR REPORT(S) NOT FOUND'                                  
INFO6    DC    CL60'TOTAL NNNNN REPORTS COPIED FROM TAPE TO DISK '              
INFO7    DC    CL60'TOTAL NNNNN REPORTS COPIED FROM DISK TO TAPE '              
INFO8    DC    CL60'OUT OF NNNNN REPORTS READ'                                  
INFO9    DC    CL60'REPORT DESCRIPTION AND DATA FOLLOWS'                        
INFOA    DC    CL60'XXXXX REPORT FOLLOWS'                                       
INFOB    DC    CL60'XXXXX REPORT SUMMARY FOLLOWS'                               
*                                                                               
QUEST1   DC    CL60'ANY MORE INPUT TAPES ?'                                     
*                                                                               
ERRMSG1  DC    CL60'ERROR MISSING PARAMETER - '                                 
ERRMSG2  DC    CL60'ERROR INVALID PARAMETER CARD SYNTAX'                        
ERRMSG3  DC    CL60'ERROR INVALID PARAMETER - '                                 
ERRMSG4  DC    CL60'ERROR INVALID VALUE FOR PARAMETER - '                       
ERRMSG5  DC    CL60'ERROR MUST SPECIFY A SINGLE PRTQ ONLY'                      
ERRMSG6  DC    CL60'ERROR 20 BIT PQS MUST HAVE 1 TRK PART 2S'                   
*                                                                               
ERRMSGA  DC    CL60'ERROR PRTQU DISK END OF FILE'                               
ERRMSGB  DC    CL60'ERROR PRTQU DISK WRITE ERROR'                               
ERRMSGC  DC    CL60'ERROR PRTQU DISK READ ERROR'                                
ERRMSGD  DC    CL60'ERROR PRTQU INVALID CI DATA'                                
ERRMSGE  DC    CL60'ERROR PRTQU ERROR IN COPY INDEX='                           
         EJECT                                                                  
SORTC1A  DC    CL80'SORT FIELDS=(31,10,A,01,10,A),FORMAT=BI,WORK=1'             
SORTC1N  DC    CL80'SORT FIELDS=(31,10,A,01,02,A,06,02,A,03,03,A,08,03,X        
               A),FORMAT=BI,WORK=1'                                             
SORTC1T  DC    CL80'SORT FIELDS=(31,10,A,01,02,A,19,04,A,03,05,A,08,03,X        
               A),FORMAT=BI,WORK=1'                                             
         EJECT                                                                  
HLINE1   DC    CL165' '                                                         
         ORG   HLINE1                                                           
         DC    C' '                                                             
H1USER   DC    CL08' '                                                          
         DC    C' '                                                             
H1ID     DC    CL09' '                                                          
         DC    C' '                                                             
H1CLASS  DC    CL01'C'                                                          
         DC    C' '                                                             
H1STAT   DC    CL02'S '                                                         
         DC    C' '                                                             
H1ARC    DC    CL02'A'                                                          
         DC    C' '                                                             
H1MAKER  DC    CL03' '                                                          
         DC    C' '                                                             
H1PAGES  DC    CL06' '                                                          
         DC    C' '                                                             
H1LINES  DC    CL07' '                                                          
         DC    C' '                                                             
H1NCI    DC    CL04' '                                                          
         DC    C' '                                                             
H1CRTD   DC    CL10'  CREATE'                                                   
         DC    C' '                                                             
H1RETD   DC    CL10'  EXPIRY'                                                   
         DC    C' '                                                             
H1HOURS  DC    CL07'HRS HRS'                                                    
         DC    C' '                                                             
H1CPL    DC    CL03'AVG'                                                        
         DC    C' '                                                             
H1PCT    DC    CL03'PCT'                                                        
         DC    C' '                                                             
H1DESC   DC    CL11' '                                                          
         DC    C' '                                                             
H1PRTD   DC    CL21' LATEST PRINTED/SENT'                                       
         DC    C' '                                                             
H1FORMS  DC    CL04' '                                                          
         DC    C' '                                                             
H1CHARS  DC    CL04' '                                                          
         DC    C' '                                                             
H1COPIES DC    CL02' '                                                          
         DC    C' '                                                             
H1PSWD   DC    CL06' '                                                          
         DC    C' '                                                             
H1ATTB   DC    CL12' '                                                          
         DC    C' '                                                             
H1OLD    DC    CL02'O '                                                         
         DC    C' '                                                             
         ORG                                                                    
         EJECT                                                                  
HLINE2   DC    CL165' '                                                         
         ORG   HLINE2                                                           
         DC    C' '                                                             
H2USER   DC    CL08'USERID'                                                     
         DC    C' '                                                             
H2ID     DC    CL09'REPORT'                                                     
         DC    C' '                                                             
H2CLASS  DC    CL01'L'                                                          
         DC    C' '                                                             
H2STAT   DC    CL02'T '                                                         
         DC    C' '                                                             
H2ARC    DC    CL02'R'                                                          
         DC    C' '                                                             
H2MAKER  DC    CL03'REF'                                                        
         DC    C' '                                                             
H2PAGES  DC    CL06' PAGES'                                                     
         DC    C' '                                                             
H2LINES  DC    CL07'  LINES'                                                    
         DC    C' '                                                             
H2NCI    DC    CL04' NCI'                                                       
         DC    C' '                                                             
H2CRTD   DC    CL10' '                                                          
         DC    C' '                                                             
H2RETD   DC    CL10' '                                                          
         DC    C' '                                                             
H2HOURS  DC    CL07'       '                                                    
         DC    C' '                                                             
H2CPL    DC    CL03' '                                                          
         DC    C' '                                                             
H2PCT    DC    CL03' '                                                          
         DC    C' '                                                             
H2DESC   DC    CL11'DESCRIPTION'                                                
         DC    C' '                                                             
H2PRTD   DC    CL21' '                                                          
         DC    C' '                                                             
H2FORMS  DC    CL04'FORM'                                                       
         DC    C' '                                                             
H2CHARS  DC    CL04'CHARS'                                                      
         DC    C' '                                                             
H2COPIES DC    CL02'CY'                                                         
         DC    C' '                                                             
H2PSWD   DC    CL06'SECURE'                                                     
         DC    C' '                                                             
H2ATTB   DC    CL12'ATTRIBUTES'                                                 
         DC    C' '                                                             
H2OLD    DC    CL02'S '                                                         
         DC    C' '                                                             
         ORG                                                                    
         EJECT                                                                  
HLINE3   DC    CL165' '                                                         
         ORG   HLINE3                                                           
         DC    C' '                                                             
H3USER   DC    CL08' '                                                          
         DC    C' '                                                             
H3ID     DC    CL09' '                                                          
         DC    C' '                                                             
H3CLASS  DC    CL01'S'                                                          
         DC    C' '                                                             
H3STAT   DC    CL02'A '                                                         
         DC    C' '                                                             
H3ARC    DC    CL02'C'                                                          
         DC    C' '                                                             
H3MAKER  DC    CL3' '                                                           
         DC    C' '                                                             
H3PAGES  DC    CL06' '                                                          
         DC    C' '                                                             
H3LINES  DC    CL07' '                                                          
         DC    C' '                                                             
H3NCI    DC    CL04' '                                                          
         DC    C' '                                                             
H3CRTD   DC    CL10'DATE  TIME'                                                 
         DC    C' '                                                             
H3RETD   DC    CL10'DATE  TIME'                                                 
         DC    C' '                                                             
H3HOURS  DC    CL07'LVE DED'                                                    
         DC    C' '                                                             
H3CPL    DC    CL03'CPL'                                                        
         DC    C' '                                                             
H3PCT    DC    CL03'USE'                                                        
         DC    C' '                                                             
H3DESC   DC    CL11' '                                                          
         DC    C' '                                                             
H3PRTD   DC    CL21'DATE  TIME  DEVICE  C'                                      
         DC    C' '                                                             
H3FORMS  DC    CL04' '                                                          
         DC    C' '                                                             
H3CHARS  DC    CL04' '                                                          
         DC    C' '                                                             
H3COPIES DC    CL02' '                                                          
         DC    C' '                                                             
H3PSWD   DC    CL06' '                                                          
         DC    C' '                                                             
H3ATTB   DC    CL12' '                                                          
         DC    C' '                                                             
H3OLD    DC    CL02'T '                                                         
         DC    C' '                                                             
         ORG                                                                    
         EJECT                                                                  
SLINE1   DC    CL165' '                                                         
         ORG   SLINE1                                                           
         DC    C' '                                                             
S1USER   DC    CL08' '                                                          
         DC    C' '                                                             
S1RTOTL  DC    CL12'   REPORTS'                                                 
         DC    C' '                                                             
S1PAGES  DC    CL12'    PAGES'                                                  
         DC    C' '                                                             
S1LINES  DC    CL12'    LINES'                                                  
         DC    C' '                                                             
S1NCI    DC    CL12' PART-2 NCI'                                                
         DC    C' '                                                             
         DC    C' '                                                             
S1RLIVE  DC    CL12'LIVE REPORTS'                                               
         DC    C' '                                                             
S1RDEAD  DC    CL12'DEAD REPORTS'                                               
         DC    C' '                                                             
S1RERR   DC    CL12'ERROR REPORT'                                               
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
S2USER   DC    CL08'USERID'                                                     
         DC    C' '                                                             
S2RTOTL  DC    CL12' '                                                          
         DC    C' '                                                             
S2PAGES  DC    CL12' '                                                          
         DC    C' '                                                             
S2LINES  DC    CL12' '                                                          
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
S3USER   DC    CL08' '                                                          
         DC    C' '                                                             
S3RTOTL  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3PAGES  DC    CL12'  COUNT  PCT'                                               
         DC    C' '                                                             
S3LINES  DC    CL12'  COUNT  PCT'                                               
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
ALLQ     EQU   X'03'                                                            
CARDQ    EQU   X'02'                                                            
INFOQ    EQU   X'01'                                                            
NONEQ    EQU   X'00'                                                            
*                                                                               
MODEL    DC    X'01',CL7'INIT'                                                  
         DC    X'02',CL7'PRINT'                                                 
         DC    X'03',CL7'REPORT'                                                
         DC    X'04',CL7'COPY'                                                  
         DC    X'05',CL7'REPLACE'                                               
         DC    X'06',CL7'ARCHIVE'                                               
MODELX   DC    X'FF'                                                            
*                                                                               
INPUTL   DC    X'01',CL7'DISK'                                                  
         DC    X'02',CL7'TAPE'                                                  
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
INFOL    DC    X'01',CL7'NO '                                                   
         DC    X'02',CL7'YES'                                                   
INFOLX   DC    X'FF'                                                            
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
BIG20L   DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
BIG20LX  DC    X'FF'                                                            
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
DSPNDXL  DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
DSPNDXLX DC    X'FF'                                                            
*                                                                               
EXTIDL   DC    C'A',CL7'ADV'                                                    
         DC    C'T',CL7'TST'                                                    
*&&US*&& DC    C'R',CL7'REP'                                                    
*&&US*&& DC    C'D',CL7'DARE'                                                   
EXTIDLX  DC    X'FF'                                                            
*                                                                               
EXPIREDL DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
EXPIREDX DC    X'FF'                                                            
*                                                                               
ARCHIVEL DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
ARCHIVEX DC    X'FF'                                                            
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
         DS    0D                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(MASTC),204X'00'          
UTL      DC    F'0',X'01',XL3'00',XL56'00'                                      
MASTC    DC    1932X'00'                                                        
MCARC    DC    CL72' '                                                          
         DC    44X'00'                                                          
         EJECT                                                                  
         DROP  R8,R9,RA            DROP PQMAINT CSECT'S EXTRA BASE REGS         
***********************************************************************         
* INITIALISE PROGRAM VARAIABLES                                       *         
***********************************************************************         
PGMAINI  CSECT                                                                  
         NMOD1 0,**PGIN**                                                       
         LR    R9,R1                                                            
         USING PARMTBL,R9          R9=A(PQMAINT CSECT'S STORAGE)                
         LR    RC,R0                                                            
         USING DPRINT,RC           RC=A(PQMAINT CSECT'S PRINTER)                
*                                                                               
PQMA1    L     R1,ACOMRG           EXTRACT AND SAVE PARM=... DATA               
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    PQMA1C                                                           
         CH    R2,=H'8'                                                         
         BNH   *+8                                                              
         LA    R2,8                                                             
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
PQMA1A   CLI   0(R1),C'0'                                                       
         BE    PQMA1B                                                           
         CLI   0(R1),C'1'                                                       
         BNE   PQMA1C                                                           
         OC    UPSIVAL,0(RF)                                                    
PQMA1B   LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,PQMA1A                                                        
PQMA1C   MVC   UPSIINP,UPSIVAL     SAVE UPSI                                    
         XC    ALOADPT,ALOADPT     CLEAR EXTERNAL LOAD POINT ADDR               
         B     PQMA1X                                                           
UPSITAB  DC    X'8040201008040201'                                              
PQMA1X   EQU   *                                                                
*                                                                               
PQMA2    GOTO1 =V(DATCON),DMCB,(5,DUB),(10,DATEIPL)                             
         GOTO1 (RF),(R1),(4,DATEIPL),(0,DATEYMD)                                
*                                                                               
         GOTO1 (RF),(R1),(0,DATEYMD),(14,DATECPRN)                              
         MVC   DATECPRO,DATECPRN                                                
         NI    DATECPRO,X'7F'                                                   
         MVC   DATECPR,DATECPRN    SET CMPRSD DATE TO NEW FORMAT                
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
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         MVI   CHRULT,C' '                                                      
         L     RF,=A(INFO1)        PRINT QUEUE FILE MAINTENANCE                 
         MVC   TITLE(28),0(RF)                                                  
         L     RE,=V(BOXAREA)                                                   
         USING BOXD,RE                                                          
         MVC   BOXWIDTH,=F'165'                                                 
PQMAI3A  TM    UPSIVAL,X'80'       TEST IF WANT 132 CHR PRINT LINES             
         BZ    PQMAI3B                                                          
         MVC   COLSMAX(5),=AL1(132,002,020,035,100)                             
         MVC   BOXWIDTH,=F'132'                                                 
PQMAI3B  TM    UPSIVAL,X'40'       TEST IF WANT 165 CHR PRINT LINES             
         BZ    PQMAI3C                                                          
         MVC   COLSMAX(5),=AL1(165,002,025,069,141)                             
         MVC   BOXWIDTH,=F'165'                                                 
PQMAI3C  TM    UPSIVAL,X'20'       TEST IF WANT 198 CHR PRINT LINES             
         BZ    PQMAI4                                                           
         MVC   COLSMAX(5),=AL1(198,002,030,089,174)                             
         MVC   BOXWIDTH,=F'198'                                                 
*                                                                               
PQMAI4   L     R1,=A(CIREC)        INITIALISE PRTQ FILE BUFFERS                 
         ST    R1,ACIREC                                                        
         L     R1,=A(CXREC)                                                     
         ST    R1,ACXREC                                                        
         XC    CIDATA,CIDATA                                                    
         LA    RE,L'PQINDEX                                                     
         STH   RE,CINDXLN                                                       
         MVC   PRTQID,PRTQUE                                                    
PQMAIX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ/PRINT/VALIDATE A SET OF PARAMETER CARDS                        *         
***********************************************************************         
VALPARM  CSECT                                                                  
         NMOD1 0,**VALP**,RA       NEED EXTRA BASE IN THIS ROUTINE              
         LR    R9,R1                                                            
         USING PARMTBL,R9          R9=A(PQMAINT CSECT'S STORAGE)                
         LR    RC,R0                                                            
         USING DPRINT,RC           RC=A(PQMAINT CSECT'S PRINTER)                
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
*                                                                               
         GOTO1 (RF),(R1),(0,DATEYMD),(14,DATECPRN)                              
         MVC   DATECPRO,DATECPRN                                                
         NI    DATECPRO,X'7F'                                                   
         MVC   DATECPR,DATECPRN    SET CMPRSD DATE TO NEW FORMAT                
*                                                                               
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
VPARM1P  TM    UPSIVAL,X'08'       TEST SUPPRESS PRINTING                       
         BO    VPARM1                                                           
         GOTO1 =V(PRINTER)                                                      
         B     VPARM1                                                           
*                                                                               
VPARM1X  CLI   OPENCTL,C'Y'        OPEN CTFILE FIRST TIME THROUGH               
         BE    VPARM2                                                           
         MVI   OPENCTL,C'Y'                                                     
         GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                     
         L     RF,ACIREC                                                        
         XC    NXUSRINF,NXUSRINF                                                
         GOTO1 VDATAMGR,DMCB,(X'20',=C'GLIST'),PRTQUE,INDEX,,(RF)               
         L     RE,NXUSRINF                                                      
         L     RF,NXUSRINF+4                                                    
         ST    RE,APRTQLST         SAVE A(PRTQ FILE LIST)                       
         ST    RF,APRTQXPE         SAVE A(PRTQ INDEX PAGE/ENTRY LIST)           
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         XC    0(2,RF),0(RF)       SET TO START AT PAGE ONE                     
         MVC   PRTQMAX,0(RE)       SAVE NUMBER OF PRTQ FILES IN LIST            
         CLI   PRTQMAX,16                                                       
         BNH   *+6                                                              
         DC    H'0'                MAX OF 16 FILES THIS VERSION                 
         LA    RE,8(RE)                                                         
         LA    RF,FILEIX+8                                                      
         SR    R1,R1                                                            
VPARM1X1 CLI   0(RE),0             TEST END OF PRTQ LIST                        
         BE    VPARM1X2                                                         
         ICM   R1,7,5(RE)          GET A(DTF)                                   
         MVC   0(7,RF),22(R1)      SAVE ORIGIONAL DTF FILE ID                   
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         B     VPARM1X1                                                         
VPARM1X2 L     RE,=V(DMISGENQ)     SAVE A(ISGENQ INTERFACE)                     
         ST    RE,CIENQDEQ                                                      
*                                                                               
VPARM2   CLC   C(2),MODE+4         FIRST CARD OF SET MUST BE MODE CARD          
         BE    *+12                                                             
         MVI   ERRNUM,1                                                         
         B     VPARMX                                                           
         LA    R1,PARMTBL          INITIALIZE VALUES                            
VPARM2A  CLI   0(R1),X'FF'                                                      
         BE    VPARM2B                                                          
         MVI   0(R1),0                                                          
         LA    R1,L'PARMTBL(R1)                                                 
         B     VPARM2A                                                          
VPARM2B  MVI   REPLMODE,0          SET NORMAL MODE                              
         MVI   ARCMODE,0                                                        
*                                                                               
VPARM4   MVC   FILEIX(8),=CL8' '   RESET FILE DTF NAMES                         
         MVC   FILEID,FILEIX                                                    
         XC    PRTQINP,PRTQINP     RESET FILE INPUT LIST                        
         L     R1,=A(MCARC)                                                     
         MVC   0(72,R1),SPACES     RESET ARC=CARD                               
         L     R1,=A(REPTAB)                                                    
         CLC   0(4,R1),=32X'FF'                                                 
         BE    *+18                                                             
         XC    0(L'REPTAB,R1),0(R1)                                             
         LA    R1,L'REPTAB(R1)                                                  
         B     *-20                                                             
         L     R1,=A(REPTAB)                                                    
         LA    R0,L'REPTAB                                                      
         SR    R1,R0                                                            
         ST    R1,AREPTAB                                                       
         B     VPARM8                                                           
*                                                                               
VPARM6   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLI   C,C'*'                                                           
         BE    VPARMP              IGNORE COMMENT CARDS                         
         CLC   C(4),=C'ARC='                                                    
         BNE   *+18                                                             
         L     R1,=A(MCARC)                                                     
         MVC   0(72,R1),C          SAVE ARC=CARD IN MCARC                       
         B     VPARMP                                                           
         CLC   C(2),=C'/*'                                                      
         BNE   *+12                                                             
         MVI   FRSTTIME,C'X'       SET EOF ON INPUT                             
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
VPARMP   TM    UPSIVAL,X'08'       TEST TO SUPPRESS PRINTING                    
         BO    VPARM6                                                           
         GOTO1 =V(PRINTER)         PRINT CARD                                   
         B     VPARM6              GO GET NEXT PARM CARD                        
*                                                                               
VPARMX   XIT1                                                                   
                                                                                
***********************************************************************         
* WRITE= AND WRSRV= CARDS                                             *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* NUMBER OF CONTROL INTERVALS                                         *         
***********************************************************************         
VNCIS    MVI   0(R4),1             NUMBER OF CONTROL INTERVALS                  
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VNCISERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'65500'        MAXIMUM VALUE                                
         BH    VNCISERR                                                         
         C     RF,=F'10'           MINIMUM VALUE                                
         BL    VNCISERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STH   RF,CICITOT                                                       
         B     *+8                                                              
         STH   RF,CJCITOT                                                       
         B     *+8                                                              
VNCISERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* TRACKS PER CONTROL INTERVAL                                         *         
***********************************************************************         
VTRKS    MVI   0(R4),1             TRACKS PER CONTROL INTERVAL                  
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
                                                                                
***********************************************************************         
* BLOCK SIZE IN BYTES                                                 *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* NUMBER OF PAGES                                                     *         
***********************************************************************         
VPAGE    MVI   PAGES,1             NUMBER OF PAGES                              
         CLI   MODE,2                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VPAGEERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BZ    VPAGEERR                                                         
         STH   RF,REPPAGES                                                      
         L     RF,AREPTAB          SAVE IN REPTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VPAGEERR                                                         
         MVC   30(2,RF),REPPAGES                                                
         B     *+8                                                              
VPAGEERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* NUMBER OF BLOCKS PER PART2                                          *         
***********************************************************************         
VOCIBLKS MVI   OCIBLKS,0           NUMBER OF BLOCKS PER PART 2                  
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
*                                                                               
         TM    3(R2),X'80'                                                      
         BZ    VBLKSER2                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BZ    VBLKSER2                                                         
         STH   RF,OCIBLKS                                                       
         BR    RE                                                               
*                                                                               
VBLKSER2 MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* KEEP NUMBER OF DAYS                                                 *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* LIVE NUMBER OF DAYS                                                 *         
***********************************************************************         
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
*                                                                               
VDDAY    MVI   DDAYS,1             DEAD NUMBER OF DAYS                          
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
*                                                                               
VCDATE   NTR1                      CREATE DATE                                  
         GOTO1 =V(DATVAL),DMCB,(0,22(R2)),DUB                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    VCDERR                                                           
         GOTO1 =V(DATCON),DMCB,(0,DUB),(30,DUB1)                                
         L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VCDERR                                                           
         OI    7(RF),X'80'         SET CDATE INPUT                              
         MVC   10(2,RF),DUB1                                                    
         B     *+8                                                              
VCDERR   MVI   ERRNUM,4                                                         
VCDX     XIT1                                                                   
*                                                                               
VCRANGE  NTR1                      CREATED RANGE DAYS                           
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
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(30,REPCSDAT)                           
         L     RF,REPCEDAY                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(RF)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(30,REPCEDAT)                           
         B     *+8                                                              
VCRERR   MVI   ERRNUM,4                                                         
VCRX     XIT1                                                                   
*                                                                               
VDRANGE  NTR1                      DEAD RANGE DAYS                              
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
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(30,REPDSDAT)                           
         L     RF,REPDEDAY                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(RF)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(30,REPDEDAT)                           
         B     *+8                                                              
VDRERR   MVI   ERRNUM,4                                                         
VDRX     XIT1                                                                   
*                                                                               
VCLASS   NTR1                      REPORT CLASS                                 
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
         BE    VCLASS5                                                          
         CLI   0(R5),C'$'                                                       
         BE    VCLASS5                                                          
         CLI   0(R5),C'A'                                                       
         BL    VCLASSE                                                          
VCLASS5  MVC   0(1,R6),0(R5)                                                    
         LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,VCLASS4                                                       
         L     RF,AREPTAB          SAVE IN REPTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VCLASSE                                                          
         MVC   18(11,RF),CLASSL                                                 
         B     *+8                                                              
*                                                                               
VCLASSE  MVI   ERRNUM,4                                                         
VCLASSX  XIT1                                                                   
*                                                                               
VRTYPE   NTR1                      REPORT TYPE                                  
         MVI   RTYPE,1                                                          
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VRTYPEX                                                          
         XC    RTYPEL,RTYPEL                                                    
         CLC   22(4,R2),=C'ALL '                                                
         BE    VRTYPEX                                                          
VRTYPE2  SR    R0,R0               R0=NUM OF RTYPE INPUT CHRS                   
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    VRTYPEX                                                          
         LA    R5,22(R2)           R5=A(INPUT RTYPE CHR)                        
         LA    R6,RTYPEL+1         R6=A(RTYPE LIST CHR)                         
*                                                                               
         MVI   RTYPEL,C'+'         FIRST CHR CAN BE + OR -                      
         CLI   0(R5),C'+'                                                       
         BE    *+16                                                             
         CLI   0(R5),C'-'                                                       
         BNE   VRTYPE4                                                          
         MVI   RTYPEL,C'-'                                                      
         SH    R0,=H'1'                                                         
         BNP   VRTYPEE                                                          
         LA    R5,1(R5)                                                         
*                                                                               
VRTYPE4  CLI   0(R5),C'*'          EACH INPUT CHR MUST BE VALID                 
         BE    VRTYPE5                                                          
         CLI   0(R5),C'$'                                                       
         BE    VRTYPE5                                                          
         CLI   0(R5),C'A'                                                       
         BL    VRTYPEE                                                          
VRTYPE5  MVC   0(1,R6),0(R5)                                                    
         LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,VRTYPE4                                                       
         L     RF,AREPTAB          SAVE IN REPTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VRTYPEE                                                          
         MVC   42(11,RF),RTYPEL                                                 
         B     *+8                                                              
*                                                                               
VRTYPEE  MVI   ERRNUM,4                                                         
VRTYPEX  XIT1                                                                   
*                                                                               
VSTAT    NTR1                      REPORT STATUS                                
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
VSTAT4   CLI   0(R5),C'A'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTAC                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'H'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTHO                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'L'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTLIVE                                                 
         B     VSTAT6                                                           
         CLI   0(R5),C'K'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTKE                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'P'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTPR                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'S'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTSE                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'D'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTDEAD                                                 
         B     VSTAT6                                                           
         B     VSTATERR                                                         
*                                                                               
VSTAT6   LA    R5,1(R5)                                                         
         BCT   R0,VSTAT4                                                        
         L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VSTATERR                                                         
         MVC   29(1,RF),REPSTAT                                                 
         B     *+8                                                              
VSTATERR MVI   ERRNUM,4                                                         
VSTATX   XIT1                                                                   
*                                                                               
VTYPE    NTR1                      REPORT TYPE                                  
         MVI   TYPE,1                                                           
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VTYPEX                                                           
         MVI   REPTYPE,0                                                        
         MVI   REPTYP1,0                                                        
         CLI   1(R2),4                                                          
         BH    VTYPEERR                                                         
         CLC   22(4,R2),=C'ALL '                                                
         BE    VTYPEX                                                           
VTYPE2   SR    R0,R0               R3=NUM OF TYPE CHRS                          
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    VTYPEX                                                           
         LA    R5,22(R2)           R5=A(TYPE CHR)                               
*                                                                               
VTYPE4   CLI   1(R2),4             TEST FOR 4 CHR STRING MATCH FIRST            
         BNE   VTYPE5                                                           
         CLC   0(4,R5),=C'ARCE'    ARCHIVE ELIGIBLE                             
         BNE   *+12                                                             
         OI    REPTYP1,PQTYAE                                                   
         B     VTYPE7                                                           
         CLC   0(4,R5),=C'ARCA'    ARCHIVABLE                                   
         BNE   *+12                                                             
         OI    REPTYP1,PQTYAR                                                   
         B     VTYPE7                                                           
         CLC   0(4,R5),=C'ARCD'    ARCHIVED                                     
         BNE   *+12                                                             
         OI    REPTYP1,PQTYAD                                                   
         B     VTYPE7                                                           
         CLC   0(4,R5),=C'ARCI'    ARCHIVE (ALL)                                
         BNE   *+12                                                             
         OI    REPTYPE,PQTYAE+PQTYAR+PQTYAD                                     
         B     VTYPE7                                                           
*                                                                               
VTYPE5   CLI   0(R5),C'A'          ARCHIVABLE                                   
         BNE   *+12                                                             
         OI    REPTYP1,PQTYAR                                                   
         B     VTYPE6                                                           
         CLI   0(R5),C'D'          ARCHIVED                                     
         BNE   *+12                                                             
         OI    REPTYP1,PQTYAD                                                   
         B     VTYPE6                                                           
         CLI   0(R5),C'E'          ARCHIVE ELIGIBLE                             
         BNE   *+12                                                             
         OI    REPTYP1,PQTYAD                                                   
         B     VTYPE6                                                           
         B     VTYPEERR                                                         
*                                                                               
VTYPE6   LA    R5,1(R5)            BUMP TO NEXT TYPE CHR                        
         BCT   R0,VTYPE5                                                        
*                                                                               
VTYPE7   L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VTYPEERR                                                         
         MVC   32(1,RF),REPTYPE                                                 
         MVC   33(1,RF),REPTYP1                                                 
         B     *+8                                                              
VTYPEERR MVI   ERRNUM,4                                                         
VTYPEX   XIT1                                                                   
*                                                                               
VFILE    NTR1                      PRTQ FILE DTF/DD OVERRIDE NAME               
         CLI   1(R2),5                                                          
         BL    VFILEERR                                                         
         CLI   1(R2),7                                                          
         BH    VFILEERR                                                         
         IC    RF,FILE             BUMP NUMBER OF FILE NAMES INPUT              
         LA    RF,1(RF)                                                         
         STC   RF,FILE                                                          
         MVC   DUB,22(R2)                                                       
         L     RF,APRTQLST         POINT TO PRTQ FILE LIST                      
         CLI   PRTQMAX,1                                                        
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
VFILE4   LA    RF,PRTQINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'10'                                                      
         BO    VFILEERR                                                         
         OI    0(RF),X'10'         SET REFERENCED VIA FILE=XXXXX                
         SLL   R1,3                                                             
         LA    RF,FILEID(R1)                                                    
         MVC   0(7,RF),DUB         SAVE INPUT OVERRIDE IN FILEID LIST           
         B     *+8                                                              
VFILEERR MVI   ERRNUM,4                                                         
         XIT1                                                                   
*                                                                               
VPQID    NTR1                                                                   
         SR    R0,R0               PRTQ FILE ID = LIST OF PRTQ ID CHRS          
         ICM   R0,1,1(R2)                                                       
         BZ    VPQIDERR            R0=NUMBER OF FILES                           
         LA    R5,22(R2)           R5=A(NEXT PRTQ FILE CHR)                     
VPQID1   L     RF,APRTQLST         POINT TO LIST OF PRTQ FILES                  
         LA    RF,8(RF)                                                         
VPQID2   CLI   0(RF),0             TEST END OF TABLE                            
         BE    VPQIDERR                                                         
         CLC   1(1,RF),0(R5)       TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VPQID3                                                           
         LA    RF,8(RF)                                                         
         B     VPQID2                                                           
VPQID3   MVC   PQID(1),0(RF)       SET PQID TO INTERNAL PRTQ FILE NUM           
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RF,PRTQINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VPQID4                                                           
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
VPQID4   TM    0(RF),X'01'         TEST DUPLICATE                               
         BO    VPQIDERR                                                         
         OI    0(RF),X'01'         SET REFERENCED BY PQID INPUT                 
         LA    R5,1(R5)                                                         
         BCT   R0,VPQID1           BACK FOR NEXT PRTQ FILE ID CHR               
         B     *+8                                                              
VPQIDERR MVI   ERRNUM,4                                                         
         XIT1                                                                   
                                                                                
***********************************************************************         
* VALIDATE USER ID - USER = ALL OR XXX...                             *         
***********************************************************************         
VUSER    NTR1                                                                   
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
         L     RF,APRTQLST         POINT TO LIST OF PRTQ FILES                  
         LA    RF,8(RF)                                                         
VUSER0A  CLI   0(RF),0             TEST END OF TABLE                            
         BE    VUSERERR                                                         
         CLC   1(1,RF),REPUSERA+3  TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VUSER0B                                                          
         LA    RF,8(RF)                                                         
         B     VUSER0A                                                          
VUSER0B  SR    R1,R1               ALLX MATCHES TO PRTQ FILE ID X               
         IC    R1,0(RF)                                                         
         LA    RF,PRTQINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VUSER0C                                                          
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
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
VUSER7   XC    INDEX,INDEX         FIND WHICH PQ FOR USER ID                    
         MVC   NXNDX(2),DUB                                                     
         GOTO1 VDATAMGR,DMCB,(X'20',=C'GFILE'),PRTQUE,INDEX,,(R5)               
         SR    R1,R1                                                            
         IC    R1,NXINFO           GET INTERNAL PRTQ FILE NUMBER                
         LA    RF,PRTQINP(R1)                                                   
         TM    0(RF),X'0F'         TEST IF FIRST REF TO PRTQ FILE               
         BNZ   VUSER7A                                                          
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
VUSER7A  OI    0(RF),X'04'         SET FLAG TO SHOW REF VIA USERID              
*                                                                               
VUSER8   L     RF,AREPTAB          SET END OF TABLE                             
         LA    RF,L'REPTAB(RF)                                                  
         MVC   0(2,RF),=X'FFFF'                                                 
         B     *+8                                                              
VUSERERR MVI   ERRNUM,4                                                         
VUSERX   XIT1                                                                   
                                                                                
***********************************************************************         
* VALIDATE AGENCY ALPHA - AGENCY = AA                                 *         
***********************************************************************         
VAGY     NTR1                                                                   
*                                                                               
         CLI   MODE,3              ONLY ON REPORT MODE                          
         BNE   VAGYERR4                                                         
*                                                                               
         CLC   AGYALPH,SPACES                                                   
         BH    VAGYERR5            ONLY ONE AGENCY CARD ALLOWED                 
*                                                                               
         MVC   AGYALPH,22(R2)                                                   
         CLI   24(R2),C' '                                                      
         BH    VAGYERR4            INVALID AGENCY CARD                          
*                                                                               
         L     R5,ACXREC           READ USER ID RECORD                          
         USING CT5REC,R5                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALPH                                                 
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'CTFILE',(R5),(R5)                        
         CLI   8(R1),0                                                          
         BE    VAGYX                                                            
*                                                                               
VAGYERR4 MVI   ERRNUM,4                                                         
         B     VAGYX                                                            
VAGYERR5 MVI   ERRNUM,5                                                         
VAGYX    XIT1                                                                   
                                                                                
***********************************************************************         
* REPORTID = ALL OR XXX                                               *         
***********************************************************************         
VREPT    NTR1  ,                   REPORTID = ALL OR XXX                        
         ST    R2,FULL             OR X'01' = XXX,NNNNN                         
         CLI   MODE,2              OR X'02' = XXX,HH:MM                         
         BNL   *+12                OR X'04' = XXX,HH:MM-HH:MM                   
         MVI   ERRNUM,3                                                         
         B     VUSERX                                                           
         MVI   REPORT,1                                                         
         XC    REPSUBID(8),REPSUBID                                             
         CLI   1(R2),3             MUST HAVE 3 CHR REPORT SUB ID                
         BNE   VREPTERR                                                         
         MVC   REPSUBID,22(R2)     SET SUBID FROM FIRST SCANNER BLOCK           
*                                                                               
VREPT2   LA    R2,32(R2)           BUMP SCANNER BLOCK                           
         BCT   R0,*+8                                                           
         B     VREPT7                                                           
         CLI   1(R2),0             EXIT IF END OR KEYWORD                       
         BNE   VREPT7                                                           
         TM    2(R2),X'80'         CHECK SEQUENCE NUMBER                        
         BZ    VREPT4                                                           
         CLC   4(4,R2),=F'1'                                                    
         BL    VREPTERR                                                         
         CLC   4(4,R2),MAXSEQ                                                   
         BH    VREPTERR                                                         
         MVC   REPSEQL,6(R2)                                                    
         OI    REPFLAG,X'01'       SET SINGLE REPORT FLAG                       
         B     VREPT8                                                           
VREPT4   SR    RF,RF               SEE IF VALID TIME EXPRESSION                 
         IC    RF,0(R2)                                                         
         GOTO1 =V(TIMBER),DMCB,(X'80',(RF)),(X'02',DUB),12(R2)                  
         CLI   0(R1),0                                                          
         BNE   VREPT5                                                           
         MVC   REPSEQL,DUB         SET LOW TIME                                 
         MVC   REPSEQH,DUB+2       SET HIGH TIME                                
         OI    REPFLAG,X'04'       SET LOW AND HIGH TIME INPUT                  
         B     VREPT6                                                           
VREPT5   OI    0(R1),X'40'         SET TO VALIDATE A SINGLE TIME                
         GOTO1 (RF),(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   VREPTERR                                                         
         MVC   REPSEQH,DUB         SET HIGH TIME                                
         OI    REPFLAG,X'02'       SET HIGH TIME INPUT                          
VREPT6   SR    RF,RF                                                            
         ICM   RF,3,REPSEQL                                                     
         MH    RF,=H'180'                                                       
         SRL   RF,2                LOWTIME=SECS*3/4                             
         STCM  RF,3,REPSEQL                                                     
         SR    RF,RF                                                            
         ICM   RF,3,REPSEQH                                                     
         MH    RF,=H'180'                                                       
         SRL   RF,2                HIGHTIME=SECS*3/4                            
         STCM  RF,3,REPSEQH                                                     
         B     VREPT8                                                           
*                                                                               
VREPT7   SH    R2,=H'32'           DECR SCANNER BLOCK                           
         AH    R0,=H'1'                                                         
*                                                                               
VREPT8   L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VREPTERR                                                         
         MVC   2(8,RF),REPSUBID                                                 
         B     VREPTX                                                           
VREPTERR L     R2,FULL                                                          
         MVI   ERRNUM,4                                                         
VREPTX   XIT1  REGS=(R0,R2)                                                     
                                                                                
***********************************************************************         
* NEWUSER = XX..                                                      *         
***********************************************************************         
VNEWUSER NTR1                                                                   
         CLI   MODE,4              ONLY VALID FOR COPY                          
         BE    *+12                                                             
VNEWU0   MVI   ERRNUM,3                                                         
         B     VNEWUX                                                           
         TM    INPUT,TAPE          ONLY VALID FOR INPUT=TAPE                    
         BZ    VNEWU0                                                           
         CLI   NEWUSER,0           ONLY ONE NEW USER ALLOWED                    
         BNE   VNEWU0                                                           
         MVC   REPNEWUA,22(R2)                                                  
*                                                                               
VNEWU1   L     R5,ACXREC           READ USER ID RECORD                          
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'I'                                                       
         MVC   15(10,R5),REPNEWUA                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'CTFILE',(R5),(R5)                        
         CLI   8(R1),0                                                          
         BNE   VNEWUERR                                                         
         LA    R5,28(R5)                                                        
         SR    RE,RE                                                            
VNEWU2   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     VNEWU2                                                           
         MVC   REPNEWUN,2(R5)      SAVE NEW USER ID NUM                         
         OI    NEWUSER,X'02'       SET NEWUSER=XXX INPUT                        
*                                                                               
VNEWU7   XC    INDEX,INDEX         FIND WHICH PQ FOR NEW USER ID                
         MVC   NXNDX(2),REPNEWUN                                                
         GOTO1 VDATAMGR,DMCB,(X'20',=C'GFILE'),PRTQUE,INDEX,,(R5)               
         SR    R1,R1                                                            
         IC    R1,NXINFO           GET INTERNAL PRTQ FILE NUMBER                
         LA    RF,PRTQINP(R1)                                                   
         TM    0(RF),X'0F'         TEST IF FIRST REF TO PRTQ FILE               
         BNZ   VNEWU7A                                                          
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
VNEWU7A  OI    0(RF),X'04'         SET FLAG TO SHOW REF VIA USERID              
         B     VNEWUX                                                           
VNEWUERR MVI   ERRNUM,4                                                         
VNEWUX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* OPEN PRTQ FILE(S) REFERENCED BY INPUT PARAMS                        *         
***********************************************************************         
OPNPQ    CSECT                                                                  
         NMOD1 0,*OPENPQ*                                                       
         LR    R9,R1                                                            
         USING PARMTBL,R9          R9=A(PQMAINT CSECT'S STORAGE)                
         LA    R1,PRTQINP          POINT TO LIST OF INPUT PRTQ IDS              
         ST    R1,APRTQINP                                                      
*                                                                               
OPNPQ1   L     R1,APRTQINP         BUMP TO NEXT FILE IN LIST                    
         LA    R1,1(R1)                                                         
         ST    R1,APRTQINP                                                      
         CLI   0(R1),X'FF'         TEST END OF LIST                             
         BE    OPNPQ6                                                           
         TM    0(R1),X'0F'         TEST IF FILE REFERENCED                      
         BZ    OPNPQ1                                                           
         LA    R0,PRTQINP                                                       
         SR    R1,R0               R1=INTERNAL PRTQ FILE NUM                    
         SLL   R1,3                                                             
         L     RF,APRTQLST         INDEX INRO PRTQ FILE LIST                    
         AR    RF,R1                                                            
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),1(RF)   SET PRTQ FILE ID FOR DATAMGR                 
         MVC   PRTQINT,0(RF)       SET PRTQ FILE INTERNAL NUM                   
         MVC   PRTQEXT,4(RF)       SET PRTQ FILE EXTERNAL NUM                   
         MVC   PRTQDTF+1(3),5(RF)  SET PRTQ FILE A(DTF)                         
         LA    RF,FILEIX(R1)                                                    
         MVC   FILEIX(8),0(RF)     SET ORIGINAL DTF NAME                        
         LA    RF,FILEID(R1)                                                    
         MVC   FILEID(8),0(RF)     SET OVERRIDE DTF NAME                        
*                                                                               
OPNPQ2   L     R2,PRTQDTF          R2=A(PRTQUE DTF)                             
         USING DTFPHD,R2                                                        
         MVI   DUB,C'N'            SET OPEN FILE LIST                           
         MVC   DUB+1(7),FILEID                                                  
         MVI   DUB+8,C'X'                                                       
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P2,ACXREC                                                        
         MVC   P4,PRTQDTF                                                       
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6,=X'00010100'                                                  
*                                                                               
         TM    DTFOPEN,X'20'       TEST IF ALREADY OPEN                         
         BZ    OPNPQ3              NO                                           
         CLC   DTFFID,FILEID       TEST IF SAME FILE ID                         
         BE    OPNPQ3              YES                                          
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 VDATAMGR,P0,DADDS                                                
*                                                                               
OPNPQ3   MVC   DTFFID,FILEID       SET FILE DNAME                               
*                                                                               
OPNPQ3A  CLI   MODE,1              TEST MODE=INIT                               
         BNE   OPNPQ3B                                                          
         L     RF,=A(SSB)          TURN OFF DYNALLOC                            
         OI    SSOFLAG2-SSOOFF(RF),SSO2DYNO                                     
         B     OPNPQ3W                                                          
*                                                                               
OPNPQ3B  CLI   MODE,2              TEST MODE=PRINT                              
         BNE   OPNPQ3C                                                          
         CLI   ARCMODE,0           TEST ARCHIVING REPORT                        
         BE    OPNPQ3R                                                          
         B     OPNPQ3W                                                          
*                                                                               
OPNPQ3C  CLI   MODE,3              TEST MODE=REPORT                             
         BNE   OPNPQ3D                                                          
         CLI   REORG,YES                                                        
         BNE   OPNPQ3R                                                          
         CLI   WRITE,NO                                                         
         BE    OPNPQ3R                                                          
         B     OPNPQ3W                                                          
*                                                                               
OPNPQ3D  CLI   MODE,4              TEST MODE=COPY                               
         BNE   OPNPQ3R                                                          
         TM    OUTPUT,DISK         SET READ-ONLY IF DUMPING TO TAPE             
         BZ    OPNPQ3R                                                          
         CLI   WRITE,NO                                                         
         BE    OPNPQ3R                                                          
         B     OPNPQ3W                                                          
*                                                                               
OPNPQ3R  TM    DTFOPEN,X'20'       OPEN FILE READ-ONLY                          
         BZ    OPNPQ3R1                                                         
         TM    DTFOPEN,X'80'                                                    
         BO    OPNPQ4              ALREADY OPEN READ-ONLY                       
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 VDATAMGR,P0,DADDS                                                
OPNPQ3R1 OI    DTFOPEN,X'80'       SET FILE READ-ONLY                           
         MVI   DUB,C'N'                                                         
         B     OPNPQ3X                                                          
*                                                                               
OPNPQ3W  TM    DTFOPEN,X'20'       OPEN FILE UPDATIVE                           
         BZ    OPNPQ3W1                                                         
         TM    DTFOPEN,X'80'                                                    
         BZ    OPNPQ4              ALREADY OPEN FOR UPDATE                      
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 VDATAMGR,P0,DADDS                                                
OPNPQ3W1 NI    DTFOPEN,X'7F'       SET FILE UPDATIVE                            
         MVI   DUB,C'U'                                                         
*                                                                               
OPNPQ3X  MVC   P1,=A(DAOPEN)                                                    
         GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',DUB                               
*                                                                               
OPNPQ4   MVC   DNEXT,=X'00010000'  SET 16 BIT INITIAL TRACK                     
         CLI   BIG20,YES                                                        
         JNE   *+10                                                             
         MVC   DNEXT,=X'00001000'  SET 20 BIT INITIAL TRACK                     
*                                                                               
         MVC   P1,=A(RDID)                                                      
         GOTO1 VDATAMGR,P0,DADDS   READ FIRST RECORD                            
         DROP  R2                                                               
*                                                                               
         CLI   MODE,1              EXIT IF MODE IS INITIALISE                   
         BE    OPNPQX                                                           
*                                                                               
OPNPQ5   L     R2,ACXREC           READ FIRST INDEX RECORD                      
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CXADDR,(R2)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,PRTQDTF          SET CIDATA STORED INFRONT OF DTF             
         SH    RE,=H'40'                                                        
         MVC   0(20,RE),0(R2)      EXTRACT PART 1 INDEX DATA                    
         TM    0(RE),X'80'                                                      
         BZ    *+14                                                             
         MVC   20(20,RE),L'PQINDEX(R2) EXTRACT PART 2 INDEX DATA                
         NI    0(RE),X'7F'                                                      
*                                                                               
         L     RF,PRTQDTF                                                       
         TM    DTFTYPE-DTFPHD(RF),DTFTBIGF  IS THIS A 20 BIT PQ                 
         JO    *+10                                                             
         MVC   CJHIREC-CIDATA(2,RE),CIHIREC-CIDATA(RE)                          
*                                                                               
         MVC   14(1,RE),PRTQINT    SET INTERNAL PRTQ FILE NUM                   
         MVC   15(5,RE),PRTQID     SET ALPHA PRTQ FILE NAME                     
         MVC   CIDATA,0(RE)                                                     
*                                                                               
         L     RE,PRTQDTF          SET F/L REC LEN IN PRTQUE DTF                
         LA    RE,52(RE)                                                        
         MVC   0(2,RE),CIBLKLN                                                  
         OI    0(RE),X'80'                                                      
         B     OPNPQ1              BACK FOR NEXT PRTQ FILE                      
*                                                                               
OPNPQ6   XC    CXPAGE,CXPAGE       SET FIRST INDEX ENTRY                        
         LH    R5,CICINDX                                                       
         BCTR  R5,0                                                             
         STH   R5,CXENTRY                                                       
*                                                                               
OPNPQX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIATIALISE A PRINT QUEUE FILE                                    *         
***********************************************************************         
INITPQ   CSECT                                                                  
         NMOD1 0,*INITPQ*                                                       
         LR    R9,R1                                                            
         USING PARMTBL,R9          R9=A(PQMAINT CSECT'S STORAGE)                
         MVI   ERRNUM,0                                                         
         XC    SAVE,SAVE                                                        
*                                                                               
IPQ      CLI   ENTRYS,0            SET DEFAULT VALUES IF NOT INPUT              
         BNE   *+10                                                             
         MVC   CICITOT,=H'100'     DEFAULT NUM OF PART#1 ENTRIES                
         CLI   CISIZE,0                                                         
         BNE   *+10                                                             
         MVC   CITRKS,=H'1'        DEFAULT PART#1 TRACKS PER CI                 
         CLI   OENTRYS,0                                                        
         BNE   *+10                                                             
         MVC   CJCITOT,=H'100'     DEFAULT NUM OF PART#2 ENTRIES                
         CLI   OCISIZE,0                                                        
         BNE   *+10                                                             
         MVC   CJTRKS,=H'1'        DEFAULT PART#2 TRACKS PER CI                 
*                                                                               
         CLI   BIG20,YES           20 BIT FILE                                  
         JNE   IPQ1                                                             
         CLC   CJTRKS,=H'1'        1 TRACK PART 2S                              
         JE    IPQ1                                                             
         MVI   ERRNUM,6            MUST BE FOR 20 BIT                           
         J     IPQX                                                             
*                                                                               
IPQ1     CLI   BLKSIZE,0                                                        
         BNE   *+10                                                             
         MVC   CIBLKLN,=H'13680'   DEFAULT BLOCK SIZE                           
         MVI   CIRSNF,1            SET RSN IS REL POSN IN INDEX                 
         TM    DSPNDX,X'02'                                                     
         BZ    *+8                                                              
         OI    CIRSNF,X'02'        SET INDEX RECS IN DATA SPACE                 
*                                                                               
         BAS   RE,IPQLOCK          LOCK PRINT QUEUE                             
*                                                                               
         SR    RE,RE               SET BLOCK SIZE MOD ENTRY SIZE                
         LH    RF,CIBLKLN                                                       
         LA    R0,L'PQINDEX                                                     
         DR    RE,R0                                                            
         MR    RE,R0                                                            
         STH   RF,CIBLKLN                                                       
*                                                                               
IPQ2     XC    P1(24),P1           CALC BLOCKS PER TRACK                        
         MVC   P1,=A(DARPT)                                                     
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,PRTQDTF                                                       
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
         LA    R0,L'PQINDEX                                                     
         DR    RE,R0                                                            
         STH   RF,CIENTRYS                                                      
*                                                                               
IPQ2A    LH    R0,CIENTRYS         OPTIMIZE PART#1 NUMBER OF CI'S               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         AHI   RF,1                ADJUST FOR END OF INDEX                      
         DR    RE,R0                                                            
IPQ2A1   CH    RE,IPQMIN           TEST IF JUST OVER OPTIMUM                    
         BH    IPQ2A2                                                           
         MR    RE,R0                                                            
         AHI   RF,-1                                                            
         STH   RF,CICITOT                                                       
         B     IPQ2B                                                            
IPQ2A2   CH    RE,IPQMAX           TEST IF JUST UNDER OPTIMUM                   
         BL    IPQ2B                                                            
         LR    R1,R0                                                            
         SR    R1,RE               SAVE NUMBER WE ARE GOING TO ADD              
         AHI   RF,1                                                             
         MR    RE,R0                                                            
         AHI   RF,-1                                                            
         STH   RF,CICITOT                                                       
         SR    RE,RE               COMPUTE NUM OF PART#2S TO LOSE               
         LR    RF,R1                                                            
         MH    RF,CITRKS           RF=NUM OF PART#1 TRACKS ADDED                
         LH    R1,CJTRKS                                                        
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AHI   RF,1                RF=NUM OF PART#2 CIS TO LOSE                 
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT                                                     
         SR    R1,RF                                                            
         STCM  R1,3,CJCITOT                                                     
*                                                                               
IPQ2B    SR    RE,RE               OPTIMIZE PART 2 NUMBER OF CI'S               
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT                                                     
         AHI   RF,2                ADJUST FOR END OF INDEXES                    
         AR    RF,R1                                                            
         DR    RE,R0                                                            
IPQ2B1   CH    RE,IPQMIN           TEST IF JUST OVER OPTIMUM                    
         BH    IPQ2B2                                                           
         MR    RE,R0                                                            
         AHI   RF,-2                                                            
         SR    RE,RE                                                            
         ICM   RE,3,CICITOT                                                     
         SR    RF,RE                                                            
         STH   RF,CJCITOT                                                       
         B     IPQ3                                                             
IPQ2B2   EQU   *                   DONT CARE IF ITS UNDER OPTIMUM               
*                                                                               
IPQ3     SR    RE,RE               CALC PAGE/ENTRY OF PART 2 INDEX              
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
         SR    RE,RE               CALC DISK ADDR OF PART 2 CI                  
         ICM   RE,3,CICITOT                                                     
         MH    RE,CITRKS                                                        
         LA    RE,1(RE)                                                         
         STH   RE,CJSTTRK                                                       
*                                                                               
         MVC   CJHIREC+1(1),OCIBLKS+1 SET BLOCKS PER PART 2                     
*                                                                               
         CLI   BIG20,YES           20 BIT                                       
         JE    *+10                                                             
         MVC   CJHIREC,CIHIREC     NON 20 BIT CJHIREC IS CIHIREC                
*                                                                               
IPQ5     LH    R0,CIENTRYS         CALC NUM OF INDEX PAGES                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT                                                     
         BZ    IPQ5A                                                            
         LA    RF,1(RF)                                                         
         AR    RF,R1                                                            
IPQ5A    DR    RE,R0                                                            
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
         STH   RF,CIFDTRK          SET TRACK NUMBER OF FIRST DATA CI            
         MVC   CFPQXID,EXTID       SET PRTQUE EXTERNAL ID (T/A/R/D)             
*                                                                               
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P1,=A(WTCKD)                                                     
         MVC   P2,ACXREC                                                        
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,PRTQDTF                                                       
         LA    R0,P6                                                            
         ST    R0,P5                                                            
*                                                                               
         SR    R3,R3               R3=NUM OF ACTIVE INDEX ENTRYS                
         ICM   R3,3,CICITOT                                                     
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT                                                     
         BZ    IPQ5B                                                            
         LA    R3,1(R3)                                                         
         AR    R3,R1                                                            
IPQ5B    LH    R4,CICINDX                                                       
         MH    R4,CITRKS                                                        
         MH    R4,CIHIREC          R4=NUM OF INDEX RECS                         
         XC    FULL,FULL           SET DUB TO PAGE/ENTRY OF END PART 1          
         MVC   DUB(2),=X'7FFF'                                                  
         OC    CJCITOT,CJCITOT                                                  
         BZ    IPQ6                                                             
         MVC   DUB(2),CJPAGE                                                    
         LH    RF,CJENTRY                                                       
         SH    RF,=H'1'                                                         
         STH   RF,DUB+2                                                         
         BNM   IPQ6                                                             
         LH    RF,CJPAGE                                                        
         BCTR  RF,0                                                             
         STH   RF,DUB                                                           
         LH    RF,CIENTRYS                                                      
         BCTR  RF,0                                                             
         STH   RF,DUB+2                                                         
*                                                                               
IPQ6     L     R5,ACXREC           WRITE INDEX RECORDS LOOP                     
         LH    R6,CIENTRYS         R6=NUM OF 00 ENTRYS                          
         SR    R7,R7               R7=NUM OF FF ENTRYS                          
         CR    R3,R6                                                            
         BL    *+10                                                             
         SR    R3,R6               FULL INDEX PAGE                              
         B     IPQ6A                                                            
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         SR    R6,R6               EMPTY INDEX PAGE                             
         LH    R7,CIENTRYS                                                      
         B     IPQ6A                                                            
         LR    R6,R3               PARTIAL INDEX PAGE                           
         LH    R7,CIENTRYS                                                      
         SR    R7,R3                                                            
         SR    R3,R3                                                            
*                                                                               
IPQ6A    LTR   R6,R6                                                            
         BZ    IPQ6B                                                            
         XC    0(L'PQINDEX,R5),0(R5)                                            
         LA    R5,L'PQINDEX(R5)                                                 
         BCT   R6,*-10                                                          
IPQ6B    LTR   R7,R7                                                            
         BZ    IPQ6C                                                            
         MVC   0(L'PQINDEX,R5),=32X'FF'                                         
         LA    R5,L'PQINDEX(R5)                                                 
         BCT   R7,*-10                                                          
*                                                                               
IPQ6C    OC    FULL(2),FULL        SET FIRST PAGE DATA                          
         BNZ   IPQ6C1                                                           
         L     R5,ACXREC                                                        
         MVC   0(20,R5),CIDATA                                                  
         MVC   L'PQINDEX(20,R5),CIDATA+20                                       
         MVC   SAVE(48),0(R5)      SAVE FIRST TWO INDEX ENTRIES                 
         OC    CJCITOT,CJCITOT     SET PART 2 INDEX PRESENT                     
         BZ    IPQ6D                                                            
         OI    0(R5),X'80'                                                      
         MVC   SAVE(48),0(R5)                                                   
IPQ6C1   CLC   FULL(2),DUB         SET END OF PART 1 INDEX                      
         BNE   IPQ6D                                                            
         LH    R5,DUB+2                                                         
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         MVC   0(L'PQINDEX,R5),=32X'FF'                                         
*                                                                               
IPQ6D    TM    WRITE,NO            TEST IF WRITE=NO                             
         BO    IPQ6D1                                                           
         GOTO1 VDATAMGR,P0,DADDS                                                
         OC    P3(2),P3                                                         
         BNZ   IPQC                                                             
IPQ6D1   LH    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STH   RE,FULL                                                          
         BCT   R4,IPQ6                                                          
*                                                                               
IPQ8     L     R5,ACIREC           POINT TO CI DATA RECORD                      
         USING PQRECD,R5                                                        
         ST    R5,P2                                                            
         LR    RE,R5                                                            
         LH    RF,CIBLKLN                                                       
         XCEF                                                                   
         SR    R0,R0               CALC NUM OF TRACKS OF DATA CI'S              
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         SR    R4,R4                                                            
         ICM   R4,3,CJCITOT                                                     
*                                                                               
         CLI   BIG20,YES           20 BIT PQ                                    
         JNE   IPQ8A                                                            
         XR    R1,R1               DO CALCULATION TO DETERMINE                  
         XR    RE,RE               HOW MANY ACTUAL TRACKS PER PART 2            
         XR    RF,RF               BASED ON CJHIREC BLOCKS PER PART 2           
         ICM   R1,3,CIHIREC        DIVIDED BY CIHIREC ACTUAL BLOCKS/TRK         
         ICM   RF,3,CJHIREC                                                     
         DR    RE,R1                                                            
         ST    RF,FULL                                                          
         MH    R4,FULL+2           MULTIPLY TOTAL PART 2S BY TRKS/PART2         
         J     IPQ8B                                                            
*                                                                               
IPQ8A    MH    R4,CJTRKS           MULTIPLY TOTAL PART 2S BY TRKS/PART2         
IPQ8B    AR    R4,R0               R4=NUM OF TRACKS IN PARTS 1 AND 2            
*                                                                               
IPQA     LH    R0,CIHIREC          WRITE DATA CI RECORDS LOOP                   
IPQA1    TM    WRITE,NO            TEST IF WRITE=NO                             
         BO    IPQA2                                                            
         GOTO1 VDATAMGR,P0,DADDS                                                
         OC    P3(2),P3            TEST FOR ERRORS ON WRITE                     
         JNZ   *+2                                                              
         BNZ   IPQC                                                             
IPQA2    BCT   R0,IPQA1                                                         
         BCT   R4,IPQA                                                          
*                                                                               
IPQB     TM    WRITE,NO            TEST IF WRITE=NO                             
         BO    IPQX                                                             
         MVC   P1,=A(WTERASE)      ERASE TO END OF EXTENT                       
         XC    P2(8),P2                                                         
         GOTO1 VDATAMGR,P0,DADDS                                                
         B     IPQX                                                             
*                                                                               
IPQC     MVI   ERRNUM,1            SET END OF FILE                              
         TM    P3+1,X'04'                                                       
         BO    *+8                                                              
         MVI   ERRNUM,2            SET DISK ERROR                               
*                                                                               
IPQX     BAS   RE,IPQUNLK          UNLOCK PRINT QUEUE                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LOCK/UNLOCK PRINT QUEUE FILE                                        *         
***********************************************************************         
IPQLOCK  LA    R0,C'E'                                                          
         B     *+8                                                              
IPQUNLK  LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         TM    WRITE,NO            TEST IF WRITE=NO                             
         BOR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,(R0),PRTQID,X'FF000000',0                              
         L     RE,CISAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
IPQMIN   DC    H'10'               MINIMUM ENTRIES SPARE                        
IPQMAX   DC    H'560'              MAXIMUM ENTRIES SPARE                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT CONTROL INTERVAL DATA FOR PRTQ FILE                *         
***********************************************************************         
PQOUT    CSECT                                                                  
         NMOD1 0,**PQOUT*                                                       
         LR    R9,R1                                                            
         USING PARMTBL,R9          R9=A(PQMAINT CSECT'S STORAGE)                
         LR    RC,R0                                                            
         USING DPRINT,RC           RC=A(PQMAINT CSECT'S PRINTER)                
*                                                                               
         CLI   MODE,1              HEXOUT INDEX IF MODE=INIT                    
         BNE   PQOUT1                                                           
         MVC   P(16),=CL16'1ST INDEX ENTRY'                                     
         GOTO1 =V(HEXOUT),PARAM1,SAVE,P+19,24,=C'TOG'                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(16),=CL16'2ND INDEX ENTRY'                                     
         GOTO1 =V(HEXOUT),PARAM1,SAVE+24,P+19,24,=C'TOG'                        
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PQOUT1   L     R7,=A(PQOUTA)                                                    
         LH    R0,CIBLKLN                                                       
         BAS   R5,PQOUT2+4         RECORD LENGTH                                
         LH    R0,CIHIREC                                                       
         BAS   R5,PQOUT2           RECS PER TRACK                               
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,PQOUT2           TRKS FOR INDEX                               
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,PQOUT2           TRKS FOR PART1                               
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         MH    R0,CJTRKS                                                        
         BAS   R5,PQOUT2           TRKS FOR PART2                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CITRKS                                                        
         BAS   R5,PQOUT2           TRKS/INDEX CI                                
         LH    R0,CITRKS                                                        
         BAS   R5,PQOUT2           TRKS/PART1 CI                                
         LH    R0,CJTRKS                                                        
         BAS   R5,PQOUT2           TRKS/PART2 CI                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CICINDX                                                       
         BAS   R5,PQOUT2           NUM OF INDEX CIS                             
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         BAS   R5,PQOUT2           NUM OF PART1 CIS                             
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         BAS   R5,PQOUT2           NUM OF PART2 CIS                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CIENTRYS                                                      
         BAS   R5,PQOUT2           INDEX ENTRYS/REC                             
         LH    R0,CIPAGES                                                       
         BAS   R5,PQOUT2           INDEX TOTAL RECS                             
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+22                                                             
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
         BAS   R5,PQOUT2           INDEX PART1 RECS                             
         SR    R0,R0                                                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LH    R0,CIPAGES                                                       
         SH    R0,CJPAGE                                                        
         BAS   R5,PQOUT2           INDEX PART2 RECS                             
         B     PQOUTX                                                           
*                                                                               
PQOUT2   LA    R7,16(R7)           BUMP TO NEXT ALPHA                           
         MVC   P(16),0(R7)                                                      
         EDIT  (R0),(5,P+19),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         BR    R5                                                               
*                                                                               
PQOUTX   XIT1                                                                   
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
PQOUTA   DC    CL16'RECORD LENGTH'                                              
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
SRTSUBID DS    CL3                 03                                           
SRTREPNO DS    XL2                 06                                           
SRTCLASS DS    CL1                 08                                           
SRTSEQ   DS    XL2                 09                                           
         DS    XL3                 11                                           
SRTSTAT  DS    XL1                 14                                           
SRTADDR  DS    XL2                 15                                           
SRTNEXT  DS    XL2                 17                                           
SRTDATEC DS    XL2                 19                                           
SRTTIMEC DS    XL2                 21                                           
SRTBAD   DS    XL8                 23                                           
SRTKEY   DS    CL10                31                                           
*                                                                               
REPTLD   DSECT                     REPORT LINE                                  
REPTL    DS    0CL165                                                           
RLFRSTC  DS    CL1                 FIRST COLUMN FOR BOX                         
RLUSER   DS    CL08                                                             
         DS    CL1                                                              
RLID     DS    CL09                                                             
         DS    CL1                                                              
RLCLASS  DS    CL01                                                             
         DS    CL1                                                              
RLSTAT   DS    CL02                                                             
         DS    CL1                                                              
RLARC    DS    CL2                                                              
         DS    CL1                                                              
RLMAKER  DS    CL03                                                             
         DS    CL1                                                              
RLPAGES  DS    CL06                                                             
         DS    CL1                                                              
RLLINES  DS    CL07                                                             
         DS    CL1                                                              
RLNCI    DS    CL04                                                             
         DS    CL1                                                              
RLCRTD   DS    CL10                                                             
         DS    CL1                                                              
RLRETD   DS    CL10                                                             
         DS    CL1                                                              
RLHOURS  DS    CL07                                                             
         DS    CL1                                                              
RLCPL    DS    CL03                                                             
         DS    CL1                                                              
RLPCT    DS    CL03                                                             
         DS    CL1                                                              
RLDESC   DS    CL11                                                             
         DS    CL1                                                              
RLPRTD   DS    CL21                                                             
         DS    CL1                                                              
RLFORMS  DS    CL04                                                             
         DS    CL1                                                              
RLCHARS  DS    CL04                                                             
         DS    CL1                                                              
RLCOPIES DS    CL02                                                             
         DS    CL1                                                              
RLPSWD   DS    CL06                                                             
         DS    CL1                                                              
RLATTB   DS    CL12                                                             
         DS    CL1                                                              
RLOSTAT  DS    CL2                                                              
RLLASTC  DS    CL1                 LAST COLUMN FOR BOX                          
*                                                                               
SMRYLD   DSECT                     SUMMARY REPORT LINE                          
SMRYL    DS    0CL165                                                           
SLFRSTC  DS    CL1                 FIRST COLUMN FOR BOX                         
SLUSER   DS    CL08                                                             
         DS    CL1                                                              
SLRTOTL  DS    CL12                                                             
         DS    CL1                                                              
SLPAGES  DS    CL12                                                             
         DS    CL1                                                              
SLLINES  DS    CL12                                                             
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
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMPRTQD                                                                        
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
*DDBIGBOX                                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DMPRTQM   11/18/20'                                      
         END                                                                    
