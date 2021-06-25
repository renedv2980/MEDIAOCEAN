*          DATA SET DMWRKZM    AT LEVEL 003 AS OF 03/03/16                      
*PHASE WRKZMA                                                                   
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
         TITLE 'WKMAINT - WRKZ FILE MAINTENANCE'                                
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
WKMAI4   L     R1,=A(CIREC)        INITIALISE WRKZ FILE BUFFERS                 
         ST    R1,ACIREC                                                        
         L     R1,=A(CXREC)                                                     
         ST    R1,ACXREC                                                        
         XC    CIDATA,CIDATA                                                    
         LA    RE,L'W_INDEX                                                     
         STH   RE,CINDXLN                                                       
         MVC   WRKZID,WRKFIL                                                    
                                                                                
*----------------------------------------------------------------------         
* READ A SET OF INPUT PARAMETER CARDS                                           
*----------------------------------------------------------------------         
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
GP0      BRAS  RE,VALPARM                                                       
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
GP3      CLI   WRKZMAX,1           SET DEFAULT WKID IF ONLY ONE FILE            
         BNE   GP4                                                              
         MVI   WKID,1              SET DEFAULT INTERNAL WRKZ FILE NUM           
         MVI   WRKZINP,1           SET ONE WRKZ FILE INPUT                      
         OI    WRKZINP+1,X'01'     SET REFERENCED BY WKID=U PARM                
*                                                                               
GP4      CLI   FILE,0              TEST IF ANY FILE RENAMES VIA FILE=           
         BE    GP4X                                                             
         CLC   FILE(1),WRKZINP     NUM RENAMED >= NUM REFERENCED                
         BNL   GP4B                                                             
GP4A     LA    R1,FILE+4           INVALID FILE=                                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP4B     LA    R1,WRKZINP+1        POINT TO LIST OF INPUT FILES REF             
GP4C     TM    0(R1),X'0F'         TEST IF REFERENCED                           
         BZ    *+12                NO                                           
         TM    0(R1),X'10'         YES MUST BE RENAMED VIA FILE=                
         BZ    GP4A                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GP4C                                                             
GP4X     EQU   *                                                                
*                                                                               
GPM1     CLI   MODE,1              INIT - MUST DEFINE WRKZ ID                   
         BNE   GPM5                                                             
         CLI   WRKZINP,1           MUST BE SINGLE FILE ONLY                     
         BE    GETPARMX                                                         
         MVI   ERRNUM,5                                                         
         B     GPERR                                                            
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
         L     RF,=A(ERRMSG5)      MUST SPECIFY A SINGLE WRKZ ONLY              
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
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* LOAD IN EXTERNAL ROUTINE AND PASS CONTROL FOR PRE-PROCESSING                  
*----------------------------------------------------------------------         
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
GPXX     B     OPEN                                                             
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* INITIALISE AND OPEN WRKZ FILES AND TAPES FOR THIS MODE                        
*----------------------------------------------------------------------         
OPEN     BRAS  RE,OPNWK            OPEN ALL WRKZ FILES REFERENCED               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         L     RF,=A(INFO3)        ACTION MESSAGES                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
         MVC   DUB1,WRKFIL         SET WRKZ FILE IN TITLE                       
         MVC   DUB2,DUB1                                                        
         CLI   WRKZINP,1           TEST ONLY ONE WRKZ FILE REFERENCED           
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
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
* INITIALISE WRKFIL                                                             
**********************************************************************          
INIWK    BRAS  RE,INITWK           GO TO INITIALISE ROUTINE                     
*                                                                               
INIWK1   CLI   ERRNUM,1            TEST FOR GOOD INITIALISE                     
         BL    INIWKX              YES                                          
         BH    *+12                                                             
         L     RF,=A(ERRMSGA)      ERROR END OF FILE                            
         B     *+8                                                              
         L     RF,=A(ERRMSGB)      ERROR DISK WRITE                             
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+6(5),WRKZID                                                 
         BAS   RE,PUTMSGP                                                       
         DC    H'0'                DIE IF FAIL TO INITIALISE                    
*                                                                               
INIWKX   L     RF,=A(INFO4)        INITIALISED WRKZX                            
         MVC   12(5,RF),FILEIX                                                  
         CLC   FILEIX(7),FILEID    TEST IF WRKZ FILE WAS RENAMED                
         BE    *+14                                                             
         MVI   17(RF),C'='                                                      
         MVC   18(7,RF),FILEID                                                  
         BAS   RE,PINFO                                                         
         GOTO1 =V(PRINTER)                                                      
         BRAS  RE,WKOUT            PRINT ATTRIBUTES OF NEW WRKFIL               
         B     GETPARM                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET/PUT MESSAGE TO OPERATOR AND OPTIONALLY TO PRINTER                         
* MESSAGE IS IN WORK AND REPLY IS RETURNED IN OPERANS                           
***********************************************************************         
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
         MVC   P(60),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         L     RE,PGSAVRE                                                       
         BR    RE                                                               
*                                                                               
WKLOCK   LA    R0,C'E'             LOCK/UNLOCK WRKFIL FILE                      
         B     *+8                                                              
WKUNLK   LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,((R0),WRKZID)                                          
         L     RE,CISAVRE                                                       
         BR    RE                                                               
*                                                                               
         DS    0H                                                               
LEFTT    EQU   X'EB'               BOX CHR LEFT HAND T JUNCTION                 
CROSS    EQU   X'8F'               BOX CHR INTERSECTION                         
RIGHTT   EQU   X'EC'               BOX CHR RIGHT T JUNCTIONTION                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* PARAMETER TABLE - LIST OF KEYWORDS AND VALUES                                 
*                                                                               
* XL1    PARM VALUE                                                             
* XL1    PARM DEFAULT VALUE                                                     
* XL1    PARM FLAGS X'80'=REQUIRED,X'40'=LIST,X'20'=ROUT,X'01'=SINGLE           
* XL1    PARM MIN LEN                                                           
* CL8    PARM KEYWORD NAME                                                      
* AL4    PARM VALUE LIST                                                        
*                                                                               
***********************************************************************         
         DS    0F                                                               
PARMTBL  DS    0CL16                                                            
MODE     DC    X'0000C004',C'MODE    ',A(MODEL)                                 
MSG      DC    X'00024001',C'MSG     ',A(MSGL)                                  
WARN     DC    X'00024001',C'WARNINGS',A(WARNL)                                 
LOAD     DC    X'00000004',C'LOAD    ',A(LOADNAME)                              
WKID     DC    X'00002001',C'WKID    ',A(VWKID)                                 
CLASS    DC    X'00002001',C'CLASS   ',A(VCLASS)                                
STATUS   DC    X'00002001',C'STATUS  ',A(VSTAT)                                 
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
FILEID   DC    CL144' ',X'FF'      MAX 0F 08 WRKZ FILES                         
FILEIX   DC    CL144' ',X'FF'                                                   
WRKZINP  DC    XL17'00',X'FF'                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
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
         DS    32F                 READ/COPY FOR 16 WRKZ FILES                  
*                                                                               
MAXSEQ   DC    F'200000'           MAXIMUM SEQUENCE NUM FOR USER ID             
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
INDEX    DS    XL64                INDEX ENTRY                                  
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
DACLOSE  EQU   15                                                               
DARPT    EQU   16                                                               
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
DADDS    DC    C'DADDS   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMPRINT  DC    C'DMPRINT '                                                      
DMWRT    DC    C'DMWRT   '                                                      
WRKFIL   DC    C'WRKZIL  '                                                      
SEQ      DC    C'SEQ     '                                                      
ADD      DC    C'ADD     '                                                      
*                                                                               
AWRKZXPE DC    A(0)                                                             
AWRKZLST DC    A(0)                                                             
WRKZMAX  DC    AL1(0)                                                           
         DC    AL1(0)                                                           
WRKZINT  DC    AL1(0)                                                           
WRKZEXT  DC    AL1(0)                                                           
WRKZID   DC    CL8' '                                                           
WRKZDTF  DC    A(0)                                                             
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
AWRKZINP DS    A                                                                
*                                                                               
CINDXMIN DC    H'2'                                                             
CPAGE    DS    H                                                                
*                                                                               
*DMWRKZW                                                                        
       ++INCLUDE DMWRKZW                                                        
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
*                                                                               
         DS    0D                                                               
         DC    CL8'WRKZLST '                                                    
WRKZLST  DS    0XL8                                                             
         DC    AL1(0),AL1(0),AL1(0),XL5'00'                                     
*                                                                               
         DC    AL1(1),C'I',XL2'00',X'F5',VL3(WRKZ)                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00'                                              
         DC    AL1(0),C' ',XL6'00' SPACE FOR ONE EXTRA FILE                     
*                                                                               
         DC    AL1(0),C' ',XL6'00'                                              
WRKZLSTX EQU   *                                                                
*                                                                               
SOFLAB   DS    0CL8                                                             
         DC    C'*SOFSOF*'                                                      
EOFLAB   DS    0CL8                                                             
         DC    C'*EOFEOF*'                                                      
*                                                                               
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
*                                                                               
         DS    0D                                                               
         DC    C'QHQHQHQH'                                                      
QH       DS    4C                                                               
Q        DS    1024C                                                            
PARMCARD DS    CL80                                                             
         EJECT                                                                  
*                                                                               
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
*                                                                               
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
*                                                                               
QUEST1   DC    CL60'ANY MORE INPUT TAPES ?'                                     
*                                                                               
ERRMSG1  DC    CL60'ERROR MISSING PARAMETER - '                                 
ERRMSG2  DC    CL60'ERROR INVALID PARAMETER CARD SYNTAX'                        
ERRMSG3  DC    CL60'ERROR INVALID PARAMETER - '                                 
ERRMSG4  DC    CL60'ERROR INVALID VALUE FOR PARAMETER - '                       
ERRMSG5  DC    CL60'ERROR MUST SPECIFY A SINGLE WRKZ ONLY'                      
*                                                                               
ERRMSGA  DC    CL60'ERROR WRKZL DISK END OF FILE'                               
ERRMSGB  DC    CL60'ERROR WRKZL DISK WRITE ERROR'                               
ERRMSGC  DC    CL60'ERROR WRKZL DISK READ ERROR'                                
ERRMSGD  DC    CL60'ERROR WRKZL INVALID CI DATA'                                
ERRMSGE  DC    CL60'ERROR WRKZL ERROR IN COPY INDEX='                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* LISTS OF PARAMETER VALUES AND EQUATES                                         
***********************************************************************         
YES      EQU   X'02'                                                            
NO       EQU   X'01'                                                            
DISK     EQU   X'01'                                                            
TAPE     EQU   X'02'                                                            
WKTAPE   EQU   X'82'                                                            
*                                                                               
MODEL    DC    X'01',CL7'INIT'                                                  
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
*                                                                               
         DS    0F                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',1024X'00'                          
UTL      DC    F'0',X'01',XL3'00',XL252'00'                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ/PRINT/VALIDATE A SET OF PARAMETER CARDS                                  
***********************************************************************         
VALPARM  NTR1                                                                   
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
*                                                                               
         L     RF,ACIREC                                                        
NX       USING UKRECD,INDEX                                                     
         XC    NX.UKUSRINF,NX.UKUSRINF                                          
         GOTO1 VDATAMGR,DMCB,=C'GLIST',WRKFIL,INDEX,,(RF)                       
         ICM   RE,15,NX.UKUSRINF                                                
         ICM   RF,15,NX.UKUSRINF+4                                              
         DROP  NX                                                               
*                                                                               
         ST    RE,AWRKZLST         SAVE A(WRKZ FILE LIST)                       
         ST    RF,AWRKZXPE         SAVE A(WRKZ INDEX PAGE/ENTRY LIST)           
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         XC    0(2,RF),0(RF)       SET TO START AT PAGE ONE                     
         MVC   WRKZMAX,0(RE)       SAVE NUMBER OF WRKZ FILES IN LIST            
         CLI   WRKZMAX,16                                                       
         BNH   *+6                                                              
         DC    H'0'                MAX OF 16 FILES THIS VERSION                 
         LA    RE,8(RE)                                                         
         LA    RF,FILEIX+8                                                      
         SR    R1,R1                                                            
VPARM1X1 CLI   0(RE),0             TEST END OF WRKZ LIST                        
         BE    VPARM1X2                                                         
         ICM   R1,7,5(RE)          GET A(DTF)                                   
         MVC   0(7,RF),22(R1)      SAVE ORIGIONAL DTF FILE ID                   
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         B     VPARM1X1                                                         
*                                                                               
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
         XC    WRKZINP,WRKZINP     RESET FILE INPUT LIST                        
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
* WRITE= AND WRSRV= CARDS                                                       
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
         C     RF,MAXSEQ           MAXIMUM VALUE                                
         BH    VNCISERR                                                         
         C     RF,=F'10'           MINIMUM VALUE                                
         BL    VNCISERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STCM  RF,15,CICITOT                                                    
         B     *+8                                                              
         STCM  RF,15,CJCITOT                                                    
         B     *+8                                                              
VNCISERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
*                                                                               
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
*                                                                               
VBLKS    MVI   BLKSIZE,1           BLOCK SIZE IN BYTES                          
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
*                                                                               
VCLASS   NTR1                      FILE CLASS                                   
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
VCLASSE  MVI   ERRNUM,4                                                         
VCLASSX  XIT1                                                                   
*                                                                               
VSTAT    NTR1                      FILE STATUS                                  
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
*                                                                               
VFILE    NTR1                      WRKZ FILE DTF/DD OVERRIDE NAME               
         CLI   1(R2),5                                                          
         BL    VFILEERR                                                         
         CLI   1(R2),7                                                          
         BH    VFILEERR                                                         
         IC    RF,FILE             BUMP NUMBER OF FILE NAMES INPUT              
         LA    RF,1(RF)                                                         
         STC   RF,FILE                                                          
         MVC   DUB,22(R2)                                                       
         L     RF,AWRKZLST         POINT TO WRKZ FILE LIST                      
         CLI   WRKZMAX,1                                                        
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
VFILE4   LA    RF,WRKZINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'10'                                                      
         BO    VFILEERR                                                         
         OI    0(RF),X'10'         SET REFERENCED VIA FILE=XXXXX                
         SLL   R1,3                                                             
         LA    RF,FILEID(R1)                                                    
         MVC   0(7,RF),DUB         SAVE INPUT OVERRIDE IN FILEID LIST           
         B     *+8                                                              
VFILEERR MVI   ERRNUM,4                                                         
         XIT1                                                                   
                                                                                
***********************************************************************         
* WRKZ FILE ID = LIST OF WRKZ ID CHRS                                           
***********************************************************************         
VWKID    NTR1                                                                   
         SR    R0,R0               WRKZ FILE ID = LIST OF WRKZ ID CHRS          
         ICM   R0,1,1(R2)                                                       
         BZ    VWKIDERR            R0=NUMBER OF FILES                           
         LA    R5,22(R2)           R5=A(NEXT WRKZ FILE CHR)                     
VWKID1   L     RF,AWRKZLST         POINT TO LIST OF WRKZ FILES                  
         LA    RF,8(RF)                                                         
VWKID2   CLI   0(RF),0             TEST END OF TABLE                            
         BE    VWKIDERR                                                         
         CLC   1(1,RF),0(R5)       TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VWKID3                                                           
         LA    RF,8(RF)                                                         
         B     VWKID2                                                           
VWKID3   MVC   WKID(1),0(RF)       SET WKID TO INTERNAL WRKZ FILE NUM           
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RF,WRKZINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VWKID4                                                           
         IC    R1,WRKZINP          BUMP NUM OF WRKZ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,WRKZINP                                                       
VWKID4   TM    0(RF),X'01'         TEST DUPLICATE                               
         BO    VWKIDERR                                                         
         OI    0(RF),X'01'         SET REFERENCED BY WKID INPUT                 
         LA    R5,1(R5)                                                         
         BCT   R0,VWKID1           BACK FOR NEXT WRKZ FILE ID CHR               
         B     *+8                                                              
VWKIDERR MVI   ERRNUM,4                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* OPEN WRKZ FILE(S) REFERENCED BY INPUT PARAMS                                  
***********************************************************************         
OPNWK    NTR1                                                                   
*                                                                               
         LA    R1,WRKZINP          POINT TO LIST OF INPUT WRKZ IDS              
         ST    R1,AWRKZINP                                                      
*                                                                               
OPNWK1   L     R1,AWRKZINP         BUMP TO NEXT FILE IN LIST                    
         LA    R1,1(R1)                                                         
         ST    R1,AWRKZINP                                                      
         CLI   0(R1),X'FF'         TEST END OF LIST                             
         BE    OPNWK6                                                           
         TM    0(R1),X'0F'         TEST IF FILE REFERENCED                      
         BZ    OPNWK1                                                           
         LA    R0,WRKZINP                                                       
         SR    R1,R0               R1=INTERNAL WRKZ FILE NUM                    
         SLL   R1,3                                                             
         L     RF,AWRKZLST         INDEX INRO WRKZ FILE LIST                    
         AR    RF,R1                                                            
         MVC   WRKZID,WRKFIL                                                    
         MVC   WRKZID+4(1),1(RF)   SET WRKZ FILE ID FOR DATAMGR                 
         MVC   WRKZINT,0(RF)       SET WRKZ FILE INTERNAL NUM                   
         MVC   WRKZEXT,4(RF)       SET WRKZ FILE EXTERNAL NUM                   
         MVC   WRKZDTF+1(3),5(RF)  SET WRKZ FILE A(DTF)                         
         LA    RF,FILEIX(R1)                                                    
         MVC   FILEIX(8),0(RF)     SET ORIGINAL DTF NAME                        
         LA    RF,FILEID(R1)                                                    
         MVC   FILEID(8),0(RF)     SET OVERRIDE DTF NAME                        
*                                                                               
         USING DTFPHD,R2                                                        
OPNWK2   L     R2,WRKZDTF          R2=A(WRKFIL DTF)                             
         MVI   DUB,C'N'                                                         
         MVC   DUB+1(7),FILEID                                                  
         MVI   DUB+8,C'X'                                                       
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P2,ACXREC                                                        
         MVC   P4,WRKZDTF                                                       
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6,=X'00001001'                                                  
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
OPNWK4   MVC   DNEXT,=X'00001000'                                               
         MVC   P1,=A(RDID)                                                      
         GOTO1 VDATAMGR,P0,DADDS   READ FIRST RECORD                            
         DROP  R2                                                               
*                                                                               
         CLI   MODE,1              EXIT IF MODE IS INITIALISE                   
         BE    OPNWKX                                                           
         DC    H'0'                                                             
*                                                                               
OPNWK6   XC    CXPAGE,CXPAGE       SET FIRST INDEX ENTRY                        
         LH    R5,CICINDX                                                       
         BCTR  R5,0                                                             
         STH   R5,CXENTRY                                                       
*                                                                               
OPNWKX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALISE A WRKZ FILE                                                        
***********************************************************************         
INITWK   NTR1                                                                   
*                                                                               
         MVI   ERRNUM,0                                                         
*                                                                               
IWK      CLI   ENTRYS,0            SET DEFAULT VALUES IF NOT INPUT              
         BNE   IWK010                                                           
         MVC   CICITOT,=F'100'     DEFAULT NUM OF ENTRYS                        
*                                                                               
IWK010   CLI   CISIZE,0                                                         
         BNE   IWK020                                                           
         MVC   CITRKS,=H'1'        DEFAULT TRACKS PER CI                        
*                                                                               
IWK020   CLI   BLKSIZE,0                                                        
         BNE   IWK030                                                           
         MVC   CIBLKLN,=H'13680'   DEFAULT BLOCK SIZE                           
*                                                                               
IWK030   CLI   OENTRYS,0                                                        
         BNE   IWK040                                                           
         MVC   CJCITOT,=X'00000000'                                             
         MVC   CJTRKS,=X'0000'                                                  
         MVC   CJSTTRK,=X'7FFFFFFF'                                             
         MVC   CJPAGE,=X'7FFF'                                                  
         MVC   CJENTRY,=X'0000'                                                 
         MVC   CJNDXADR,=X'00001001'                                            
*                                                                               
IWK040   MVI   CIRSNF,1            SET RSN IS REL POSN IN INDEX                 
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
         MVC   P4,WRKZDTF                                                       
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
         ICM   RF,15,CICITOT                                                    
         DR    RE,R0                                                            
         CH    RE,=H'10'                                                        
         BNL   IWK060                                                           
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         STCM  RF,15,CICITOT                                                    
*                                                                               
IWK060   SR    RE,RE               OPTIMISE PART 2 NUMBER OF CI'S               
         ICM   RF,15,CICITOT                                                    
         OC    CJCITOT,CJCITOT                                                  
         BZ    IWK080                                                           
         LA    RF,1(RF)                                                         
         A     RF,CJCITOT                                                       
         DR    RE,R0                                                            
         CH    RE,=H'10'                                                        
         BNL   IWK070                                                           
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         ICM   RE,15,CICITOT                                                    
         SR    RF,RE                                                            
         STCM  RF,15,CJCITOT                                                    
*                                                                               
IWK070   SR    RE,RE               CALC PAGE/ENTRY OF PART 2 INDEX              
         ICM   RF,15,CICITOT                                                    
         LA    RF,1(RF)                                                         
         DR    RE,R0                                                            
         STH   RF,CJPAGE                                                        
         STH   RE,CJENTRY                                                       
         SR    RE,RE               CALC DISK ADDR OF PART 2 INDEX               
         LH    R0,CIHIREC                                                       
         DR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         SLL   RF,12                                                            
         STCM  RF,15,CJNDXADR                                                   
         STC   RE,CJNDXADR+3                                                    
*                                                                               
         ICM   RE,15,CICITOT       CALC DISK ADDR OF PART 2 CI                  
         MH    RE,CITRKS                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,15,CJSTTRK                                                    
*                                                                               
IWK080   LH    R0,CIENTRYS         CALC NUM OF INDEX PAGES                      
         SR    RE,RE                                                            
         ICM   RF,15,CICITOT                                                    
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LA    RF,1(RF)                                                         
         A     RF,CJCITOT                                                       
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
         ST    RF,CIFDTRK          SET TRACK NUM OF FIRST DATA CI               
         MVC   CFWFXID,EXTID       SET WRKZ EXTERNAL ID (T/A)                   
*                                                                               
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P1,=A(WTCKD)                                                     
         MVC   P2,ACXREC                                                        
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,WRKZDTF                                                       
         LA    R0,P6                                                            
         ST    R0,P5                                                            
*                                                                               
         ICM   R3,15,CICITOT       R3=NUM OF ACTIVE INDEX ENTRYS                
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LA    R3,1(R3)                                                         
         A     R3,CJCITOT                                                       
         LH    R4,CICINDX                                                       
         MH    R4,CITRKS                                                        
         MH    R4,CIHIREC          R4=NUM OF INDEX RECS                         
         XC    FULL,FULL           SET DUB TO PAGE/ENTRY OF END PART 1          
         MVC   DUB(2),=X'7FFF'                                                  
         OC    CJCITOT,CJCITOT                                                  
         BZ    IWK090                                                           
         MVC   DUB(2),CJPAGE                                                    
         LH    RF,CJENTRY                                                       
         AHI   RF,-1                                                            
         STH   RF,DUB+2                                                         
         BNM   IWK090                                                           
         LH    RF,CJPAGE                                                        
         BCTR  RF,0                                                             
         STH   RF,DUB                                                           
         LH    RF,CIENTRYS                                                      
         BCTR  RF,0                                                             
         STH   RF,DUB+2                                                         
*                                                                               
IWK090   L     R5,ACXREC           WRITE INDEX RECORDS LOOP                     
         LH    R6,CIENTRYS         R6=NUM OF 00 ENTRYS                          
         SR    R7,R7               R7=NUM OF FF ENTRYS                          
         CR    R3,R6                                                            
         BL    *+10                                                             
         SR    R3,R6               FULL INDEX PAGE                              
         B     IWK100                                                           
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         SR    R6,R6               EMPTY INDEX PAGE                             
         LH    R7,CIENTRYS                                                      
         B     IWK100                                                           
         LR    R6,R3               PARTIAL INDEX PAGE                           
         LH    R7,CIENTRYS                                                      
         SR    R7,R3                                                            
         SR    R3,R3                                                            
*                                                                               
IWK100   LTR   R6,R6                                                            
         BZ    IWK110                                                           
         XC    0(L'W_INDEX,R5),0(R5)                                            
         LA    R5,L'W_INDEX(R5)                                                 
         BCT   R6,*-10                                                          
IWK110   LTR   R7,R7                                                            
         BZ    IWK120                                                           
         MVC   0(L'W_INDEX,R5),=64X'FF'                                         
         LA    R5,L'W_INDEX(R5)                                                 
         BCT   R7,*-10                                                          
*                                                                               
IWK120   OC    FULL(2),FULL        SET FIRST PAGE DATA                          
         BNZ   IWK130                                                           
*                                                                               
         L     R5,ACXREC                                                        
         MVC   0(L'CI1DATA,R5),CI1DATA                                          
         MVC   L'W_INDEX(L'CI2DATA,R5),CI2DATA                                  
         OC    CJCITOT,CJCITOT     SET PART 2 INDEX PRESENT                     
         BZ    IWK140                                                           
         OI    0(R5),X'80'                                                      
IWK130   CLC   FULL(2),DUB         SET END OF PART 1 INDEX                      
         BNE   IWK140                                                           
         LH    R5,DUB+2                                                         
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         MVC   0(L'W_INDEX,R5),=64X'FF'                                         
*                                                                               
IWK140   GOTO1 VDATAMGR,P0,DADDS                                                
         OC    P3(2),P3                                                         
         BNZ   IWKC                                                             
         LH    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STH   RE,FULL                                                          
         BCT   R4,IWK090                                                        
*                                                                               
IWK8     L     R5,ACIREC           POINT TO CI DATA RECORD                      
         USING W_RECD,R5                                                        
         ST    R5,P2                                                            
         LR    RE,R5                                                            
         LH    RF,CIBLKLN                                                       
         XCEF                                                                   
         ICM   R0,15,CICITOT       CALC NUM OF TRACKS OF DATA CI'S              
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         SR    R4,R4                                                            
         ICM   R4,15,CJCITOT                                                    
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
*                                                                               
IWKLOCK  LA    R0,C'E'             LOCK/UNLOCK WRKZ FILE                        
         B     *+8                                                              
IWKUNLK  LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,((R0),WRKZID)                                          
         L     RE,CISAVRE                                                       
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO PRINT CONTROL INTERVAL DATA FOR WRKZ FILE                          
***********************************************************************         
WKOUT    NTR1                                                                   
*                                                                               
         LARL  R7,WKOUTA                                                        
*                                                                               
         LH    R0,CIBLKLN                                                       
         BAS   R5,WKOUT2+4         RECORD LENGTH                                
         LH    R0,CIHIREC                                                       
         BAS   R5,WKOUT2           RECS PER TRACK                               
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR INDEX                               
         ICM   R0,15,CICITOT                                                    
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR PART1                               
         ICM   R0,15,CJCITOT                                                    
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
         ICM   R0,15,CICITOT                                                    
         SH    R0,CICINDX                                                       
         BAS   R5,WKOUT2           NUM OF PART1 CIS                             
         ICM   R0,15,CJCITOT                                                    
         BAS   R5,WKOUT2           NUM OF PART2 CIS                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CIENTRYS                                                      
         BAS   R5,WKOUT2           INDEX ENTRYS/REC                             
         LH    R0,CIPAGES                                                       
         BAS   R5,WKOUT2           INDEX TOTAL RECS                             
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    WKOUT1                                                           
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
WKOUT1   BAS   R5,WKOUT2           INDEX PART1 RECS                             
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
         EDIT  (R0),(6,P+19),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         BR    R5                                                               
*                                                                               
WKOUTX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
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
WKWORK   DS    4000D                                                            
*                                                                               
         DS    0D                                                               
         DC    C'UIDSUIDS'                                                      
CTBUF    DC    1000XL12'00'                                                     
         DC    2X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'SUMBSUMB'                                                      
SUMBUF   DC    1001XL100'00'                                                    
         DC    6X'FF'                                                           
*                                                                               
CTREC    DS    4096C                                                            
                                                                                
***********************************************************************         
* INCLUDES                                                                      
***********************************************************************         
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMWRKZD                                                                        
       ++INCLUDE DMWRKZD                                                        
         EJECT                                                                  
*DMSYSFD                                                                        
       ++INCLUDE DMSYSFD                                                        
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
*DMWRKZK                                                                        
       ++INCLUDE DMWRKZK                                                        
         EJECT                                                                  
*DMWRKZL                                                                        
       ++INCLUDE DMWRKZL                                                        
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
**PAN#1  DC    CL21'003DMWRKZM   03/03/16'                                      
         END                                                                    
